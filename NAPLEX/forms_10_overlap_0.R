start_time <- Sys.time()

# Load packages and data ---------------------------------------------------
library(readxl)
library(lpSolveAPI)
library(dplyr)
library(tidyr)

collection_path <- "JAN_2023_NAPLEX.xlsx"

Bank_Collection <- read_excel(collection_path, col_types = "text")
Bank_Collection <- rename(Bank_Collection, "ID" = "QuestionId")


enemies <- read_excel(collection_path, col_types = "text") %>%
  rename("ID" = "QuestionId") %>%
  subset(., select = c(ID, Enemies)) %>%
  separate_rows(Enemies)



drugs <- read_excel(collection_path, col_types = "text") %>%
  rename("ID" = "QuestionId") %>%
  subset(., select = c(ID, `Drug Class`)) %>%
  separate_rows(`Drug Class`, sep = "\\|") %>%
  mutate(Drug = gsub("\\s*\\([^\\)]+\\)", "", `Drug Class`) )




medcond <- read_excel(collection_path, col_types = "text") %>%
  rename("ID" = "QuestionId") %>%
  subset(., select = c(ID, `Medical Conditions`)) %>%
  separate_rows(`Medical Conditions`, sep = "\\|") %>%
  mutate(MedCond = gsub("\\s*\\([^\\)]+\\)", "", `Medical Conditions`) )




# Create New Variables ----------------------------------------------------

# Create a numeric variable for item difficulty
Bank_Collection$ID <- as.numeric(Bank_Collection$ID)

# Create a numeric variable for content categories
Bank_Collection$Content <- as.numeric(substr(Bank_Collection$Competency, 1, 1))

# Create a numeric variable for item difficulty
Bank_Collection$B <- as.numeric(Bank_Collection$IrtB)

# Create a numeric variable for cognitive domain
#Bank_Collection$domain <- as.numeric(substr(Bank_Collection$CognitiveDomain, 1, 1))
Bank_Collection$domain <- NA
Bank_Collection$domain[Bank_Collection$`Cognitive Domain` == "Remember/Recall"] <- 1
Bank_Collection$domain[Bank_Collection$`Cognitive Domain` == "Apply/Understand"] <- 2
Bank_Collection$domain[Bank_Collection$`Cognitive Domain` == "Analysis/Evaluate"] <- 3
Bank_Collection$domain[Bank_Collection$`Cognitive Domain` == "Not Coded"] <- 4



# Create a numeric variable for item type
Bank_Collection$type <- NA
Bank_Collection$type[Bank_Collection$Type == "Multiple Choice"] <- 1
Bank_Collection$type[Bank_Collection$Type == "Multiple Response"] <- 2
Bank_Collection$type[Bank_Collection$Type == "Fill in the Blank"] <- 3
Bank_Collection$type[Bank_Collection$Type == "Ordered Response"] <- 4
Bank_Collection$type[Bank_Collection$Type == "Hotspot (HTML5)"] <- 5


# Identify recently used items 
Scored_03_2022 <- read_excel("Scored_Items_03_2022.xlsx")
Bank_Collection$recent <- ifelse(Bank_Collection$ID %in% Scored_03_2022$ITEM, 2, 1) 



# Operational Items -------------------------------------------------------


# subset for only Operational Items
OP_items <- subset(Bank_Collection, Status == "Scored")


# !!!!!!!!!!!!!!!!!!!!!!!!! remove items from area 6 until July 2022 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OP_items <- subset(OP_items, Content < 6 & type < 4 & domain < 4)


# create sequence variable for consistent ordering
OP_items$seq <- seq(1:nrow(OP_items))

# create scenario id numbers
OP_items   <- OP_items %>%
  group_by(Cases) %>%
  arrange(Cases) %>%
  mutate(scenario_id = cur_group_id()) %>%
  arrange(seq) %>%
  ungroup()


Scenario_list <- subset(OP_items, 
                        #scenario_id != 1, 
                        select = c("ID", "seq", "scenario_id" ))



attach(OP_items)


# Define Variables --------------------------------------------------------

n_items <- nrow(OP_items) #Number of items in pool (I)

n_forms <- 10             #Number of test forms (F)

tif_points <- 1           #Number of theta points at which information if controlled (J)

# make 200 when we have enough Area 6 items otherwise, 186
form_length <- 186        #Length of both test forms (N)

n_scenarios <- length(unique(na.omit(Cases)))

# Not enough items in Area 6 (14 items), so we need to build without it. 
n_content <- c(36,28,70,28,22) #Numbers of items required from k=1:8 content categories (Nc)

# Specify the percent from each cognitive domain, currently using a 30/30/10 % split with 20% free 
n_domain <- c(50, 50, 20)

# Specify the number of items from each item type
n_types <- c(150, 10, 1)

# Cap the number of items from a single med condition or drug class
n_drugs <- c(10)

n_medcond <- c(10)

# Allow for 25% overlap from the previous forms -- for no area 6=(136, 50)
n_recent <- c(136, 50)



# Set TIF parameters ------------------------------------------------------

theta <- c(-0.33) #Define I? points at which information is controlled
Info <- array(0,c(n_items,tif_points)) #Define empty matrix with item information function values


#Fill matrix with item information function values (Rasch model)

for (j in 1:tif_points) {
  P <- exp(theta[j] - B) / (1 + exp(theta[j] - B))
  Q <- 1 - P
  Info[,j] <- P*Q
}






# Content Categories ------------------------------------------------------

Vc <- list()

for (k in 1:length(n_content)) {
  Vc[[k]] <- c(1:n_items)[Content == k]
}


# Enemy Items -------------------------------------------------------------


enemies$itemA.index <- match(enemies$ID, OP_items$ID)
enemies$itemB.index <- match(enemies$Enemies, OP_items$ID)

enemies <- rename(enemies, c(ItemA = ID, 
                             ItemB = Enemies))

Ve <- list()

library(purrr)
Ve <- enemies[,3:4] %>% purrr::transpose() 
Ve <- lapply(Ve, unlist, use.names = FALSE)
Ve <- unique(lapply(Ve, sort))
Ve <- Filter(function(x) length(x) > 1, Ve)





# Scenario Items ----------------------------------------------------------

# stand-alone items are assigned to the last spot in the list.

Vs <- list()

for (s in 1:n_scenarios) {
  Vs[[s]] <- c(1:n_items)[Scenario_list$scenario_id == s]
}



# Drug Classification -----------------------------------------------------



#drugs$itemA.index<-match(drugs$Item, OP_items$ID)
#drugs$itemB.index<-match(drugs$Item, OP_items$ID)
#
#drugs <- rename(drugs, c(ItemA=Item, ItemB=Drug))
#
#Vr <- list()
#
#library(purrr)
#Vr<-drugs[,3:4] %>% purrr::transpose() 
#Vr<-lapply(Vr, unlist, use.names=FALSE)
#Vr<-unique(lapply(Vr, sort))
#Vr<- Filter(function(x) length(x)>1, Vr)


# --- old file format below

drugs$item.index<-match(drugs$ID, OP_items$ID)


drugs <- drugs %>%
  group_by(Drug) %>%
  arrange(Drug)%>%
  mutate(drug_id = cur_group_id()) %>%
  arrange(item.index)


Vr <- list()
for(r in 1:length(unique(drugs$Drug))){
  Vr[[r]] <- drugs$item.index[drugs$drug_id==r]
}

Vr<- Filter(function(x) length(x)>1, Vr)
Vr <- lapply(Vr, function(x) x[!is.na(x)])

# Creates a vector matching the nubmer of drugs to the max number in any category
drug.vector <- NULL  
for (i in 1:length(Vr)) drug.vector[i]=10;  
 


# Medical Condition -------------------------------------------------------


# Removes the text inside the parentheses
#medcond <- medcond %>% mutate(d = gsub("\\s*\\([^\\)]+\\)", "", MedCond) )

#medcond %>% mutate(d = gsub("(\\s*\\(\\w+\\))", "", MedCond, perl=TRUE)) %>%  print(n=100)

#medcond %>% mutate(d = gsub("\\s*\\([^\\)]+\\)\\s*$", "", MedCond, perl=TRUE)) %>%  print(n=100)



# Change bar-sep to comma-sep
#medcond$dx <- sub("\\|", ", " , medcond$MedCond)

#medcond<-medcond %>%
#  mutate(MedCond = strsplit(gsub("[][\"]", "", MedCond), "\\|")) %>%
#  unnest(MedCond) 


#medcond$MedCond <- as.numeric(trimws(medcond$MedCond))
#medcond <- subset(medcond, select = -c(d, dx))


medcond$item.index<-match(medcond$ID, OP_items$ID)

medcond <- medcond %>%
  group_by(MedCond) %>%
  arrange(MedCond)%>%
  mutate(medcond_id = cur_group_id()) %>%
  arrange(item.index)


Vm <- list()
for(m in 1:length(unique(medcond$MedCond))){
  Vm[[m]] <- medcond$item.index[medcond$medcond_id==m]
}

Vm<- Filter(function(x) length(x)>1, Vm)
Vm <- lapply(Vm, function(x) x[!is.na(x)])

med.vector <- NULL  
for (i in 1:length(Vm)) med.vector[i]=10;  


# Item Types --------------------------------------------------------------

Vi <- list()

for(i in 1:length(n_types)){
  Vi[[i]] <- c(1:n_items)[type==i]
}

# Cognitive Domain --------------------------------------------------------


Vd <- list()

for(d in 1:length(n_domain)){
  Vd[[d]] <- c(1:n_items)[domain==d]
}




# Recent Publication Overlap ----------------------------------------------


Vp <- list()

for(p in 1:length(n_recent)){
  Vp[[p]] <- c(1:n_items)[recent==p]
}



# Set Linear Programming Parameters ---------------------------------------

# Number of decision variables number of items by the number of forms
n_dec_vars <- n_items*n_forms 
# Create empty model
lprec <- make.lp(0,n_dec_vars)
# Objective function
set.objfn(lprec,rep(Info,n_forms))




# Set Constraints ---------------------------------------------------------


# Constraints in Eq. 5 (Ensures no overlap in tests)
for (k in 1:n_items){
  add.constraint(lprec,rep(1,10),"<=",1,indices=c(k,
                                                 n_items+k, 
                                                 2*n_items+k,
                                                 3*n_items+k,
                                                 4*n_items+k,
                                                 5*n_items+k,
                                                 6*n_items+k,
                                                 7*n_items+k,
                                                 8*n_items+k,
                                                 9*n_items+k)
  )
}




# Constraints in Eq. 6 (Set content categories Form 1)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 2)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 3)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=2*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 4)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=3*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 5)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=4*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 6)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=5*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 7)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=6*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 8)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=7*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 9)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=8*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 10)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=9*n_items+Vc[[k]])
}


# Constraints for enemy items


# Set enemies  Form 1
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=Ve[[e]])
}
# Set enemies  Form 2
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=n_items+Ve[[e]])
}
# Set enemies  Form 3
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=2*n_items+Ve[[e]])
}
# Set enemies  Form 4
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=3*n_items+Ve[[e]])
}
# Set enemies  Form 5
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=4*n_items+Ve[[e]])
}
# Set enemies  Form 6
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=5*n_items+Ve[[e]])
}
# Set enemies  Form 7
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=6*n_items+Ve[[e]])
}
# Set enemies  Form 8
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=7*n_items+Ve[[e]])
}
# Set enemies  Form 9
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=8*n_items+Ve[[e]])
}
# Set enemies  Form 10
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=9*n_items+Ve[[e]])
}


# Constraints in Eq. 7 (Sets the number of items in each form)
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = 1:n_items)
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (n_items+1):(2*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (2*n_items+1):(3*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (3*n_items+1):(4*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (4*n_items+1):(5*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (5*n_items+1):(6*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (6*n_items+1):(7*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (7*n_items+1):(8*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (8*n_items+1):(9*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (9*n_items+1):(10*n_items))



# Constraints for Scenario sets

### treats them like enemy items...only 1 item from any set.

# Set Scenarios  Form 1
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=Vs[[s]])
}
# Set Scenarios  Form 2
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=n_items+Vs[[s]])
}
# Set Scenarios  Form 3
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=2*n_items+Vs[[s]])
}
# Set Scenarios  Form 4
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=3*n_items+Vs[[s]])
}
# Set Scenarios  Form 5
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=4*n_items+Vs[[s]])
}
# Set Scenarios  Form 6
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=5*n_items+Vs[[s]])
}
# Set Scenarios  Form 7
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=6*n_items+Vs[[s]])
}
# Set Scenarios  Form 8
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=7*n_items+Vs[[s]])
}
# Set Scenarios  Form 9
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=8*n_items+Vs[[s]])
}
# Set Scenarios  Form 10
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=9*n_items+Vs[[s]])
}




# Constraint for Item Types

# Form 1
for (i in 1:length(n_types)){
  add.constraint(lprec,rep(1,length(Vi[[i]])),">=",n_types[i],indices=Vi[[i]])
}
# Form 2
for (i in 1:length(n_types)){
  add.constraint(lprec,rep(1,length(Vi[[i]])),">=",n_types[i],indices=n_items+Vi[[i]])
}
# Form 3
for (i in 1:length(n_types)){
  add.constraint(lprec,rep(1,length(Vi[[i]])),">=",n_types[i],indices=2*n_items+Vi[[i]])
}
# Form 4
for (i in 1:length(n_types)){
  add.constraint(lprec,rep(1,length(Vi[[i]])),">=",n_types[i],indices=3*n_items+Vi[[i]])
}
# Form 5
for (i in 1:length(n_types)){
  add.constraint(lprec,rep(1,length(Vi[[i]])),">=",n_types[i],indices=4*n_items+Vi[[i]])
}
# Form 6
for (i in 1:length(n_types)){
  add.constraint(lprec,rep(1,length(Vi[[i]])),">=",n_types[i],indices=5*n_items+Vi[[i]])
}
# Form 7)
for (i in 1:length(n_types)){
  add.constraint(lprec,rep(1,length(Vi[[i]])),">=",n_types[i],indices=6*n_items+Vi[[i]])
}
# Form 8
for (i in 1:length(n_types)){
  add.constraint(lprec,rep(1,length(Vi[[i]])),">=",n_types[i],indices=7*n_items+Vi[[i]])
}
# Form 9
for (i in 1:length(n_types)){
  add.constraint(lprec,rep(1,length(Vi[[i]])),">=",n_types[i],indices=8*n_items+Vi[[i]])
}
# Form 10 
for (i in 1:length(n_types)){
  add.constraint(lprec,rep(1,length(Vi[[i]])),">=",n_types[i],indices=9*n_items+Vi[[i]])
}


## Constraint for Drug Classification
#
#
## Form 1
#for (r in 1:length(drug.vector)){
#  add.constraint(lprec,rep(1,length(Vr[[r]])),"<=",drug.vector[r],indices=Vr[[r]])
#}
## Form 2
#for (r in 1:length(drug.vector)){
#  add.constraint(lprec,rep(1,length(Vr[[r]])),"<=",drug.vector[r],indices=n_items+Vr[[r]])
#}
## Form 3
#for (r in 1:length(drug.vector)){
#  add.constraint(lprec,rep(1,length(Vr[[r]])),"<=",drug.vector[r],indices=2*n_items+Vr[[r]])
#}
## Form 4
#for (r in 1:length(drug.vector)){
#  add.constraint(lprec,rep(1,length(Vr[[r]])),"<=",drug.vector[r],indices=3*n_items+Vr[[r]])
#}
## Form 5
#for (r in 1:length(drug.vector)){
#  add.constraint(lprec,rep(1,length(Vr[[r]])),"<=",drug.vector[r],indices=4*n_items+Vr[[r]])
#}
## Form 6
#for (r in 1:length(Vr)){
#  add.constraint(lprec,rep(1,length(Vr[[r]])),"<=",drug.vector[r],indices=5*n_items+Vr[[r]])
#}
## Form 7)
#for (r in 1:length(Vr)){
#  add.constraint(lprec,rep(1,length(Vr[[r]])),"<=",drug.vector[r],indices=6*n_items+Vr[[r]])
#}
## Form 8
#for (r in 1:length(Vr)){
#  add.constraint(lprec,rep(1,length(Vr[[r]])),"<=",drug.vector[r],indices=7*n_items+Vr[[r]])
#}
## Form 9
#for (r in 1:length(Vr)){
#  add.constraint(lprec,rep(1,length(Vr[[r]])),"<=",drug.vector[r],indices=8*n_items+Vr[[r]])
#}
## Form 10 
#for (r in 1:length(Vr)){
#  add.constraint(lprec,rep(1,length(Vr[[r]])),"<=",drug.vector[r],indices=9*n_items+Vr[[r]])
#}
#





## Constraint for Cognitive Domain
#
#
## Form 1
#for (d in 1:length(n_domain)){
#  add.constraint(lprec,rep(1,length(Vd[[d]])),">=",n_domain[d],indices=Vd[[d]])
#}
## Form 2
#for (d in 1:length(n_domain)){
#  add.constraint(lprec,rep(1,length(Vd[[d]])),">=",n_domain[d],indices=n_items+Vd[[d]])
#}
## Form 3
#for (d in 1:length(n_domain)){
#  add.constraint(lprec,rep(1,length(Vd[[d]])),">=",n_domain[d],indices=2*n_items+Vd[[d]])
#}
## Form 4
#for (d in 1:length(n_domain)){
#  add.constraint(lprec,rep(1,length(Vd[[d]])),">=",n_domain[d],indices=3*n_items+Vd[[d]])
#}
## Form 5
#for (d in 1:length(n_domain)){
#  add.constraint(lprec,rep(1,length(Vd[[d]])),">=",n_domain[d],indices=4*n_items+Vd[[d]])
#}
## Form 6
#for (d in 1:length(n_domain)){
#  add.constraint(lprec,rep(1,length(Vd[[d]])),">=",n_domain[d],indices=5*n_items+Vd[[d]])
#}
## Form 7)
#for (d in 1:length(n_domain)){
#  add.constraint(lprec,rep(1,length(Vd[[d]])),">=",n_domain[d],indices=6*n_items+Vd[[d]])
#}
## Form 8
#for (d in 1:length(n_domain)){
#  add.constraint(lprec,rep(1,length(Vd[[d]])),">=",n_domain[d],indices=7*n_items+Vd[[d]])
#}
## Form 9
#for (d in 1:length(n_domain)){
#  add.constraint(lprec,rep(1,length(Vd[[d]])),">=",n_domain[d],indices=8*n_items+Vd[[d]])
#}
## Form 10 
#for (d in 1:length(n_domain)){
#  add.constraint(lprec,rep(1,length(Vd[[d]])),">=",n_domain[d],indices=9*n_items+Vd[[d]])
#}







## Constraint for Medical Condition
#
#
## Form 1
#for (m in 1:length(med.vector)){
#  add.constraint(lprec,rep(1,length(Vm[[m]])),"<=",med.vector[m],indices=Vm[[m]])
#}
## Form 2
#for (m in 1:length(med.vector)){
#  add.constraint(lprec,rep(1,length(Vm[[m]])),"<=",med.vector[m],indices=n_items+Vm[[m]])
#}
## Form 3
#for (m in 1:length(med.vector)){
#  add.constraint(lprec,rep(1,length(Vm[[m]])),"<=",med.vector[m],indices=2*n_items+Vm[[m]])
#}
## Form 4
#for (m in 1:length(med.vector)){
#  add.constraint(lprec,rep(1,length(Vm[[m]])),"<=",med.vector[m],indices=3*n_items+Vm[[m]])
#}
## Form 5
#for (m in 1:length(med.vector)){
#  add.constraint(lprec,rep(1,length(Vm[[m]])),"<=",med.vector[m],indices=4*n_items+Vm[[m]])
#}
## Form 6
#for (m in 1:length(n_medcond)){
#  add.constraint(lprec,rep(1,length(Vm[[m]])),"<=",med.vector[m],indices=5*n_items+Vm[[m]])
#}
## Form 7)
#for (m in 1:length(n_medcond)){
#  add.constraint(lprec,rep(1,length(Vm[[m]])),"<=",med.vector[m],indices=6*n_items+Vm[[m]])
#}
## Form 8
#for (m in 1:length(n_medcond)){
#  add.constraint(lprec,rep(1,length(Vm[[m]])),"<=",med.vector[m],indices=7*n_items+Vm[[m]])
#}
## Form 9
#for (m in 1:length(n_medcond)){
#  add.constraint(lprec,rep(1,length(Vm[[m]])),"<=",med.vector[m],indices=8*n_items+Vm[[m]])
#}
## Form 10 
#for (m in 1:length(n_medcond)){
#  add.constraint(lprec,rep(1,length(Vm[[m]])),"<=",med.vector[m],indices=9*n_items+Vm[[m]])
#}
#









# Constraint for Recent Publication Overlap



# Form 1
for (p in 1:length(n_recent)){
  add.constraint(lprec,rep(1,length(Vp[[p]])),"<=",n_recent[d],indices=Vp[[p]])
}
# Form 2
for (p in 1:length(n_recent)){
  add.constraint(lprec,rep(1,length(Vp[[p]])),"<=",n_recent[d],indices=n_items+Vp[[p]])
}
# Form 3
for (p in 1:length(n_recent)){
  add.constraint(lprec,rep(1,length(Vp[[p]])),"<=",n_recent[d],indices=2*n_items+Vp[[p]])
}
# Form 4
for (p in 1:length(n_recent)){
  add.constraint(lprec,rep(1,length(Vp[[p]])),"<=",n_recent[d],indices=3*n_items+Vp[[p]])
}
# Form 5
for (p in 1:length(n_recent)){
  add.constraint(lprec,rep(1,length(Vp[[p]])),"<=",n_recent[d],indices=4*n_items+Vp[[p]])
}
# Form 6
for (p in 1:length(n_recent)){
  add.constraint(lprec,rep(1,length(Vp[[p]])),"<=",n_recent[d],indices=5*n_items+Vp[[p]])
}
# Form 7)
for (p in 1:length(n_recent)){
  add.constraint(lprec,rep(1,length(Vp[[p]])),"<=",n_recent[d],indices=6*n_items+Vp[[p]])
}
# Form 8
for (p in 1:length(n_recent)){
  add.constraint(lprec,rep(1,length(Vp[[p]])),"<=",n_recent[d],indices=7*n_items+Vp[[p]])
}
# Form 9
for (p in 1:length(n_recent)){
  add.constraint(lprec,rep(1,length(Vp[[p]])),"<=",n_recent[d],indices=8*n_items+Vp[[p]])
}
# Form 10 
for (p in 1:length(n_recent)){
  add.constraint(lprec,rep(1,length(Vp[[p]])),"<=",n_recent[d],indices=9*n_items+Vp[[p]])
}



# Constraints in Eqs. 8-9 
set.type(lprec,columns=c(1:(n_forms*n_items)),type="binary")
set.type(lprec,columns=n_dec_vars,type="real")
set.bounds(lprec,lower=rep(0,n_dec_vars),upper=rep(1,n_dec_vars))



# SOLVE  ------------------------------------------------------------------

# set to maximize the objective function at the desired point
lp.control(lprec,sense="max");

#Solve model
solve(lprec)

# Get the value of the objective function
get.objective(lprec)

# Get the values of the decision variables
get.variables(lprec)

# Get realizations of the constraints
get.constraints(lprec)



# Plot Test Information Functions -----------------------------------------

decvar <- get.variables(lprec)  #Insert values for decision variables in new vector decvar
x <- seq(-3,3, .01)   #Define theta axis
ItemInfo <- array(0,c(n_items, length(x))) #Define empty matrix with item information function values along theta axis for all items

for (j in 1:length(x)){
  P <-exp(x[j]-B)/(1+exp(x[j]-B))
  Q <- 1-P
  ItemInfo[,j] <- P*Q
} #Calculate item information function values along theta axis for all items 




y = matrix(0,length(x),10) #Define empty matrix with item information function values along theta axis for all items

for (j in 1:length(x)){
  #Calculate test information function values along theta axis for Form 1
  y[j,1]=sum(decvar[1:n_items]*ItemInfo[,j]) 
  
  #Calculate test information function values along theta axis for Form 2
  y[j,2]=sum(decvar[(n_items+1):(2*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 3
  y[j,3]=sum(decvar[(2*n_items+1):(3*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 4
  y[j,4]=sum(decvar[(3*n_items+1):(4*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 5
  y[j,5]=sum(decvar[(4*n_items+1):(5*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 6
  y[j,6]=sum(decvar[(5*n_items+1):(6*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 7
  y[j,7]=sum(decvar[(6*n_items+1):(7*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 8
  y[j,8]=sum(decvar[(7*n_items+1):(8*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 9
  y[j,9]=sum(decvar[(8*n_items+1):(9*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 10
  y[j,10]=sum(decvar[(9*n_items+1):(10*n_items)]*ItemInfo[,j])
 
  
} 

#Plot information functions

#jpeg("TIF.jpg")
plot(x,y[,1], type="l", lty=1, xlab='Theta',ylab='Information', col="gray1")
lines(x,y[,2], type="l", lty=1, xlab='Theta',ylab='Information', col="gray2")
lines(x,y[,3], type="l", lty=1, xlab='Theta',ylab='Information', col="gray3")
lines(x,y[,4], type="l", lty=1, xlab='Theta',ylab='Information', col="gray4")
lines(x,y[,5], type="l", lty=1, xlab='Theta',ylab='Information', col="gray5")
lines(x,y[,6], type="l", lty=1, xlab='Theta',ylab='Information', col="gray6")
lines(x,y[,7], type="l", lty=1, xlab='Theta',ylab='Information', col="gray7")
lines(x,y[,8], type="l", lty=1, xlab='Theta',ylab='Information', col="gray8")
lines(x,y[,9], type="l", lty=1, xlab='Theta',ylab='Information', col="gray9")
lines(x,y[,10], type="l", lty=1, xlab='Theta',ylab='Information', col="gray10")


#title(i)
#dev.off()

# Assign Items to Forms ---------------------------------------------------



OP_items$FORM_1 <-decvar[1:n_items]
OP_items$FORM_2 <-decvar[(n_items+1):(2*n_items)]
OP_items$FORM_3 <-decvar[(2*n_items+1):(3*n_items)]
OP_items$FORM_4 <-decvar[(3*n_items+1):(4*n_items)]
OP_items$FORM_5 <-decvar[(4*n_items+1):(5*n_items)]
OP_items$FORM_6 <-decvar[(5*n_items+1):(6*n_items)]
OP_items$FORM_7 <-decvar[(6*n_items+1):(7*n_items)]
OP_items$FORM_8 <-decvar[(7*n_items+1):(8*n_items)]
OP_items$FORM_9 <-decvar[(8*n_items+1):(9*n_items)]
OP_items$FORM_10 <-decvar[(9*n_items+1):(10*n_items)]



# Create output tables ----------------------------------------------------

table.blueprint<-OP_items %>%
  group_by(Content) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2), 
            f3=sum(FORM_3),
            f4=sum(FORM_4),
            f5=sum(FORM_5),
            f6=sum(FORM_6),
            f7=sum(FORM_7),
            f8=sum(FORM_8),
            f9=sum(FORM_9),
            f10=sum(FORM_10)
  )


table.competency <- OP_items %>%
  group_by(Competency) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2), 
            f3=sum(FORM_3),
            f4=sum(FORM_4),
            f5=sum(FORM_5),
            f6=sum(FORM_6),
            f7=sum(FORM_7),
            f8=sum(FORM_8),
            f9=sum(FORM_9),
            f10=sum(FORM_10)
  )


table.scenario <- OP_items %>%
  group_by(scenario_id) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2), 
            f3=sum(FORM_3),
            f4=sum(FORM_4),
            f5=sum(FORM_5),
            f6=sum(FORM_6),
            f7=sum(FORM_7),
            f8=sum(FORM_8),
            f9=sum(FORM_9),
            f10=sum(FORM_10)
  )



table.cognitive.domain <- OP_items %>%
  group_by(`Cognitive Domain`) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2), 
            f3=sum(FORM_3),
            f4=sum(FORM_4),
            f5=sum(FORM_5),
            f6=sum(FORM_6),
            f7=sum(FORM_7),
            f8=sum(FORM_8),
            f9=sum(FORM_9),
            f10=sum(FORM_10)
  )



table.item.type <- OP_items %>%
  group_by(Type) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2), 
            f3=sum(FORM_3),
            f4=sum(FORM_4),
            f5=sum(FORM_5),
            f6=sum(FORM_6),
            f7=sum(FORM_7),
            f8=sum(FORM_8),
            f9=sum(FORM_9),
            f10=sum(FORM_10)
  )




#Number of scenarios per form
table.scenario %>%
  summarize(f1=length(unique(scenario_id[table.scenario$f1>0])),
            f2=length(unique(scenario_id[table.scenario$f2>0])),
            f3=length(unique(scenario_id[table.scenario$f3>0])),
            f4=length(unique(scenario_id[table.scenario$f4>0])),
            f5=length(unique(scenario_id[table.scenario$f5>0])),
            f6=length(unique(scenario_id[table.scenario$f6>0])),
            f7=length(unique(scenario_id[table.scenario$f7>0])),
            f8=length(unique(scenario_id[table.scenario$f8>0])),
            f9=length(unique(scenario_id[table.scenario$f9>0])),
            f10=length(unique(scenario_id[table.scenario$f10>0]))
            
  )

# Create variables for item time
OP_items$f1_time <- OP_items$FORM_1*as.numeric(OP_items$Time)
OP_items$f2_time <- OP_items$FORM_2*as.numeric(OP_items$Time)
OP_items$f3_time <- OP_items$FORM_3*as.numeric(OP_items$Time)
OP_items$f4_time <- OP_items$FORM_4*as.numeric(OP_items$Time)
OP_items$f5_time <- OP_items$FORM_5*as.numeric(OP_items$Time)
OP_items$f6_time <- OP_items$FORM_6*as.numeric(OP_items$Time)
OP_items$f7_time <- OP_items$FORM_7*as.numeric(OP_items$Time)
OP_items$f8_time <- OP_items$FORM_8*as.numeric(OP_items$Time)
OP_items$f9_time <- OP_items$FORM_9*as.numeric(OP_items$Time)
OP_items$f10_time <- OP_items$FORM_10*as.numeric(OP_items$Time)


# Table for total exam time in hours
table.time <- OP_items %>%
  summarize(f1= sum(OP_items$f1_time, na.rm=TRUE) / 3600,
            f2=sum(OP_items$f2_time, na.rm=TRUE) / 3600,
            f3=sum(OP_items$f3_time, na.rm=TRUE) / 3600,
            f4=sum(OP_items$f4_time, na.rm=TRUE) / 3600,
            f5=sum(OP_items$f5_time, na.rm=TRUE) / 3600,
            f6=sum(OP_items$f6_time, na.rm=TRUE) / 3600,
            f7=sum(OP_items$f7_time, na.rm=TRUE) / 3600,
            f8=sum(OP_items$f8_time, na.rm=TRUE) / 3600,
            f9=sum(OP_items$f9_time, na.rm=TRUE) / 3600,
            f10=sum(OP_items$f10_time, na.rm=TRUE) / 3600)



# make tables for drugs and med cond.


form_1_array<-OP_items$ID[OP_items$FORM_1==1]
form_2_array<-OP_items$ID[OP_items$FORM_2==1]
form_3_array<-OP_items$ID[OP_items$FORM_3==1]
form_4_array<-OP_items$ID[OP_items$FORM_4==1]
form_5_array<-OP_items$ID[OP_items$FORM_5==1]
form_6_array<-OP_items$ID[OP_items$FORM_6==1]
form_7_array<-OP_items$ID[OP_items$FORM_7==1]
form_8_array<-OP_items$ID[OP_items$FORM_8==1]
form_9_array<-OP_items$ID[OP_items$FORM_9==1]
form_10_array<-OP_items$ID[OP_items$FORM_10==1]





# tables for drug classes

drugs<-drugs %>%
  mutate (f1 = ifelse(ID %in% form_1_array, 1, 0),
          f2 = ifelse(ID %in% form_2_array, 1, 0),
          f3 = ifelse(ID %in% form_3_array, 1, 0),
          f4 = ifelse(ID %in% form_4_array, 1, 0),
          f5 = ifelse(ID %in% form_5_array, 1, 0),
          f6 = ifelse(ID %in% form_6_array, 1, 0),
          f7 = ifelse(ID %in% form_7_array, 1, 0),
          f8 = ifelse(ID %in% form_8_array, 1, 0),
          f9 = ifelse(ID %in% form_9_array, 1, 0),
          f10 = ifelse(ID %in% form_10_array, 1, 0)     )



table.drugs <- drugs %>%
  group_by(Drug) %>%
  summarize(f1 = sum(f1),
            f2 = sum(f2),
            f3 = sum(f3),
            f4 = sum(f4),
            f5 = sum(f5),
            f6 = sum(f6),
            f7 = sum(f7),
            f8 = sum(f8),
            f9 = sum(f9),
            f10 = sum(f10)
  )



#  tables for enemy items

enemies<-enemies %>%
  mutate (f1 = ifelse(ItemA %in% form_1_array & ItemB %in% form_1_array, 1, 0),
          f2 = ifelse(ItemA %in% form_2_array & ItemB %in% form_2_array, 1, 0),
          f3 = ifelse(ItemA %in% form_3_array & ItemB %in% form_3_array, 1, 0),
          f4 = ifelse(ItemA %in% form_4_array & ItemB %in% form_4_array, 1, 0),
          f5 = ifelse(ItemA %in% form_5_array & ItemB %in% form_5_array, 1, 0),
          f6 = ifelse(ItemA %in% form_6_array & ItemB %in% form_6_array, 1, 0),
          f7 = ifelse(ItemA %in% form_7_array & ItemB %in% form_7_array, 1, 0),
          f8 = ifelse(ItemA %in% form_8_array & ItemB %in% form_8_array, 1, 0),
          f9 = ifelse(ItemA %in% form_9_array & ItemB %in% form_9_array, 1, 0),
          f10 = ifelse(ItemA %in% form_10_array & ItemB %in% form_10_array, 1, 0) )


# Table produces a 0/1 result if any enemy pairs are found in the form
table.enemies <- enemies %>%
  summarize(f1 = max(f1),
            f2 = max(f2),
            f3 = max(f3),
            f4 = max(f4),
            f5 = max(f5),
            f6 = max(f6),
            f7 = max(f7),
            f8 = max(f8),
            f9 = max(f9),
            f10 = max(f10)
  )






# tables for medical conditions
medcond<-medcond %>%
  mutate (f1 = ifelse(ID %in% form_1_array, 1, 0),
          f2 = ifelse(ID %in% form_2_array, 1, 0),
          f3 = ifelse(ID %in% form_3_array, 1, 0),
          f4 = ifelse(ID %in% form_4_array, 1, 0),
          f5 = ifelse(ID %in% form_5_array, 1, 0),
          f6 = ifelse(ID %in% form_6_array, 1, 0),
          f7 = ifelse(ID %in% form_7_array, 1, 0),
          f8 = ifelse(ID %in% form_8_array, 1, 0),
          f9 = ifelse(ID %in% form_9_array, 1, 0),
          f10 = ifelse(ID %in% form_10_array, 1, 0)     )

table.medcond <- medcond %>%
  group_by(MedCond) %>%
  summarize(f1 = sum(f1),
            f2 = sum(f2),
            f3 = sum(f3),
            f4 = sum(f4),
            f5 = sum(f5),
            f6 = sum(f6),
            f7 = sum(f7),
            f8 = sum(f8),
            f9 = sum(f9),
            f10 = sum(f10)
  )



table.recent<-OP_items %>%
  group_by( recent) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2), 
            f3=sum(FORM_3),
            f4=sum(FORM_4),
            f5=sum(FORM_5),
            f6=sum(FORM_6),
            f7=sum(FORM_7),
            f8=sum(FORM_8),
            f9=sum(FORM_9),
            f10=sum(FORM_10)
  )




# Checking normality of the item difficulty distributions

di1 <- subset(OP_items, FORM_1==1, select = B)
hist(di1$B)
di2 <- subset(OP_items, FORM_2==1, select = B)
hist(di2$B)
di3 <- subset(OP_items, FORM_3==1, select = B)
hist(di3$B)
di4 <- subset(OP_items, FORM_4==1, select = B)
hist(di4$B)
di5 <- subset(OP_items, FORM_5==1, select = B)
hist(di5$B)
di6 <- subset(OP_items, FORM_6==1, select = B)
hist(di6$B)
di7 <- subset(OP_items, FORM_7==1, select = B)
hist(di7$B)
di8 <- subset(OP_items, FORM_8==1, select = B)
hist(di8$B)
di9 <- subset(OP_items, FORM_9==1, select = B)
hist(di9$B)
di10 <- subset(OP_items, FORM_10==1, select = B)
hist(di10$B)



###########################################################################

# Pretest Items -----------------------------------------------------------

###########################################################################




# Make a vector of all the items in the operational collection
OP_PT_enemies <- OP_items
OP_PT_enemies$selected <- apply(OP_items[,18:27], 1, sum)
OP_PT_enemies <- subset(OP_PT_enemies, selected ==1)
OP_PT_enemy_ID<-as.vector(as.numeric(OP_PT_enemies$ID))

OP_PT_enemy_list <- subset(enemies, enemies$ItemA %in% OP_PT_enemy_ID  & enemies$ItemB %in%  OP_PT_enemy_ID)

                                             
# subset for only Operational Items
PT_items <- subset(Bank_Collection, 
                   Status == "Approved Not Used" | 
                     Status == "CA Review 2" |
                     Status == "Live Pretest")

# check to see if any pretest items are enemies with operational items
PT_sample <- subset(PT_items, PT_items$ID %in% OP_PT_enemy_list$ItemA & PT_items$ID %in% OP_PT_enemy_list$ItemB)

# we currently pretest all items for a scenario at the same time, but need to check in the future!!!!


# Create variables

PT_items$B <- -0.33

# remove any items in category 6 & 7 
PT_items <- subset(PT_items, Content < 7 )

# create sequence variable for consistent ordering
PT_items$seq <- seq(1:nrow(PT_items))


# create scenario id numbers
PT_items   <- PT_items %>%
  group_by(Cases) %>%
  arrange(Cases)%>%
  mutate(scenario_id = cur_group_id()) %>%
  arrange(seq) %>%
  ungroup()


Scenario_list <- subset(PT_items, 
                        #scenario_id != 1, 
                        select = c("ID", "seq", "scenario_id" ))



attach(PT_items)





# Define Variables --------------------------------------------------------

n_items <- nrow(PT_items) #Number of items in pool (I)
n_forms <- 10             #Number of test forms (F)
tif_points <- 1           #Number of theta points at which information if controlled (J)
# make 200 when we have enough Area 6 items.
form_length <- 25        #Length of both test forms (N)
n_scenarios <- length(unique(na.omit(Cases)))

n_content <- c(4, 4, 4, 3, 2, 8) #Numbers of items required from k=1:8 content categories (Nc)




# Set TIF parameters ------------------------------------------------------

theta <- c(-0.33) #Define I? points at which information is controlled
Info <- array(0,c(n_items,tif_points)) #Define empty matrix with item information function values


#Fill matrix with item information function values (Rasch model)

for(j in 1:tif_points){
  P <-exp(theta[j]-B)/(1+exp(theta[j]-B))
  Q <- 1-P
  Info[,j] <- P*Q
}






# Content Categories ------------------------------------------------------

Vc <- list()

for(k in 1:length(n_content)){
  Vc[[k]] <- c(1:n_items)[Content==k]
}


# Enemy Items -------------------------------------------------------------

enemies$itemA.index<-match(enemies$ItemA, PT_items$ID)
enemies$itemB.index<-match(enemies$ItemB, PT_items$ID)


Ve <- list()

library(purrr)
Ve<-enemies[,3:4] %>% purrr::transpose() 
Ve<-lapply(Ve, unlist, use.names=FALSE)
Ve<-unique(lapply(Ve, sort))
Ve<- Filter(function(x) length(x)>1, Ve)




# Scenario Items ----------------------------------------------------------

# stand-alone items are assigned to the last spot in the list.

Vs <- list()

for(s in 1:n_scenarios){
  Vs[[s]] <- c(1:n_items)[Scenario_list$scenario_id==s]
}





# Set Linear Programming Parameters ---------------------------------------

# Number of decision variables number of items by the number of forms
n_dec_vars <- n_items*n_forms 
# Create empty model
lprec <- make.lp(0,n_dec_vars)
# Objective function
set.objfn(lprec,rep(Info,n_forms))




# Set Constraints ---------------------------------------------------------


# Constraints in Eq. 5 (Ensures no overlap in tests)
for (k in 1:n_items){
  add.constraint(lprec,rep(1,10),"<=",1,indices=c(k,
                                                  n_items+k, 
                                                  2*n_items+k,
                                                  3*n_items+k,
                                                  4*n_items+k,
                                                  5*n_items+k,
                                                  6*n_items+k,
                                                  7*n_items+k,
                                                  8*n_items+k,
                                                  9*n_items+k)
  )
}




# Constraints in Eq. 6 (Set content categories Form 1)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 2)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 3)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=2*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 4)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=3*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 5)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=4*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 6)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=5*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 7)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=6*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 8)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=7*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 9)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=8*n_items+Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 10)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=9*n_items+Vc[[k]])
}


# Constraints for enemy items


# Set enemies  Form 1
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=Ve[[e]])
}
# Set enemies  Form 2
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=n_items+Ve[[e]])
}
# Set enemies  Form 3
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=2*n_items+Ve[[e]])
}
# Set enemies  Form 4
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=3*n_items+Ve[[e]])
}
# Set enemies  Form 5
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=4*n_items+Ve[[e]])
}
# Set enemies  Form 6
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=5*n_items+Ve[[e]])
}
# Set enemies  Form 7
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=6*n_items+Ve[[e]])
}
# Set enemies  Form 8
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=7*n_items+Ve[[e]])
}
# Set enemies  Form 9
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=8*n_items+Ve[[e]])
}
# Set enemies  Form 10
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=9*n_items+Ve[[e]])
}


# Constraints in Eq. 7 (Sets the number of items in each form)
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = 1:n_items)
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (n_items+1):(2*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (2*n_items+1):(3*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (3*n_items+1):(4*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (4*n_items+1):(5*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (5*n_items+1):(6*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (6*n_items+1):(7*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (7*n_items+1):(8*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (8*n_items+1):(9*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (9*n_items+1):(10*n_items))



# Constraints for Scenario sets

### treats them like enemy items...only 1 item from any set.

# Set Scenarios  Form 1
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=Vs[[s]])
}
# Set Scenarios  Form 2
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=n_items+Vs[[s]])
}
# Set Scenarios  Form 3
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=2*n_items+Vs[[s]])
}
# Set Scenarios  Form 4
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=3*n_items+Vs[[s]])
}
# Set Scenarios  Form 5
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=4*n_items+Vs[[s]])
}
# Set Scenarios  Form 6
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=5*n_items+Vs[[s]])
}
# Set Scenarios  Form 7
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=6*n_items+Vs[[s]])
}
# Set Scenarios  Form 8
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=7*n_items+Vs[[s]])
}
# Set Scenarios  Form 9
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=8*n_items+Vs[[s]])
}
# Set Scenarios  Form 10
for (s in 1:length(Vs)){
  add.constraint(lprec,rep(1,length(Vs[[s]])),"<=",1,indices=9*n_items+Vs[[s]])
}





# Constraints in Eqs. 8-9 
set.type(lprec,columns=c(1:(n_forms*n_items)),type="binary")
set.type(lprec,columns=n_dec_vars,type="real")
set.bounds(lprec,lower=rep(0,n_dec_vars),upper=rep(1,n_dec_vars))



# SOLVE  ------------------------------------------------------------------

# set to maximize the objective function at the desired point
lp.control(lprec,sense="max");

#Solve model
solve(lprec)

# Get the value of the objective function
get.objective(lprec)

# Get the values of the decision variables
get.variables(lprec)

# Get realizations of the constraints
get.constraints(lprec)



# Plot Test Information Functions -----------------------------------------

decvar <- get.variables(lprec)  #Insert values for decision variables in new vector decvar
x <- seq(-3,3, .01)   #Define theta axis
ItemInfo <- array(0,c(n_items, length(x))) #Define empty matrix with item information function values along theta axis for all items

for (j in 1:length(x)){
  P <-exp(x[j]-B)/(1+exp(x[j]-B))
  Q <- 1-P
  ItemInfo[,j] <- P*Q
} #Calculate item information function values along theta axis for all items 




y = matrix(0,length(x),10) #Define empty matrix with item information function values along theta axis for all items

for (j in 1:length(x)){
  #Calculate test information function values along theta axis for Form 1
  y[j,1]=sum(decvar[1:n_items]*ItemInfo[,j]) 
  
  #Calculate test information function values along theta axis for Form 2
  y[j,2]=sum(decvar[(n_items+1):(2*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 3
  y[j,3]=sum(decvar[(2*n_items+1):(3*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 4
  y[j,4]=sum(decvar[(3*n_items+1):(4*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 5
  y[j,5]=sum(decvar[(4*n_items+1):(5*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 6
  y[j,6]=sum(decvar[(5*n_items+1):(6*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 7
  y[j,7]=sum(decvar[(6*n_items+1):(7*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 8
  y[j,8]=sum(decvar[(7*n_items+1):(8*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 9
  y[j,9]=sum(decvar[(8*n_items+1):(9*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 10
  y[j,10]=sum(decvar[(9*n_items+1):(10*n_items)]*ItemInfo[,j])
  
  
} 

#Plot information functions

#jpeg("TIF.jpg")
plot(x,y[,1], type="l", lty=1, xlab='Theta',ylab='Information', col="gray1")
lines(x,y[,2], type="l", lty=1, xlab='Theta',ylab='Information', col="gray2")
lines(x,y[,3], type="l", lty=1, xlab='Theta',ylab='Information', col="gray3")
lines(x,y[,4], type="l", lty=1, xlab='Theta',ylab='Information', col="gray4")
lines(x,y[,5], type="l", lty=1, xlab='Theta',ylab='Information', col="gray5")
lines(x,y[,6], type="l", lty=1, xlab='Theta',ylab='Information', col="gray6")
lines(x,y[,7], type="l", lty=1, xlab='Theta',ylab='Information', col="gray7")
lines(x,y[,8], type="l", lty=1, xlab='Theta',ylab='Information', col="gray8")
lines(x,y[,9], type="l", lty=1, xlab='Theta',ylab='Information', col="gray9")
lines(x,y[,10], type="l", lty=1, xlab='Theta',ylab='Information', col="gray10")


#title(i)
#dev.off()

# Assign Items to Forms ---------------------------------------------------



PT_items$FORM_1 <-decvar[1:n_items]
PT_items$FORM_2 <-decvar[(n_items+1):(2*n_items)]
PT_items$FORM_3 <-decvar[(2*n_items+1):(3*n_items)]
PT_items$FORM_4 <-decvar[(3*n_items+1):(4*n_items)]
PT_items$FORM_5 <-decvar[(4*n_items+1):(5*n_items)]
PT_items$FORM_6 <-decvar[(5*n_items+1):(6*n_items)]
PT_items$FORM_7 <-decvar[(6*n_items+1):(7*n_items)]
PT_items$FORM_8 <-decvar[(7*n_items+1):(8*n_items)]
PT_items$FORM_9 <-decvar[(8*n_items+1):(9*n_items)]
PT_items$FORM_10 <-decvar[(9*n_items+1):(10*n_items)]




pt.table.blueprint<-PT_items %>%
  group_by(Content) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2), 
            f3=sum(FORM_3),
            f4=sum(FORM_4),
            f5=sum(FORM_5),
            f6=sum(FORM_6),
            f7=sum(FORM_7),
            f8=sum(FORM_8),
            f9=sum(FORM_9),
            f10=sum(FORM_10)
  )






# Write output file -------------------------------------------------------


OP_items$selected <- apply(OP_items[,18:27], 1, sum)
OP_output <- subset(OP_items, selected ==1 , select=c(ID, Type, Competency,
                                         Cases, Status, `Cognitive Domain`, Enemies, IrtB,# Time,
                                         FORM_1, FORM_2, FORM_3, FORM_4, FORM_5,
                                         FORM_6, FORM_7, FORM_8, FORM_9, FORM_10))

PT_items$ selected <- apply(PT_items[,18:27], 1, sum)
PT_output <- subset(PT_items, selected ==1 , select=c(ID, Type, Competency,
                                         Cases, Status, `Cognitive Domain`, Enemies, IrtB, #Time,
                                         FORM_1, FORM_2, FORM_3, FORM_4, FORM_5,
                                         FORM_6, FORM_7, FORM_8, FORM_9, FORM_10))

output <- rbind(OP_output, PT_output)




#output$Status <- recode(output$Status, 
#                             "Scored" = "Scored",
#                             "Approved Not Used" = "Pretest")

# used this ifelse because we has multiple pretest statuses
output$Status <- ifelse(output$Status == "Scored", "Scored", "Pretest")

FORM_1 <- subset(output, FORM_1==1, select = c(ID, Status))
FORM_2 <- subset(output, FORM_2==1, select = c(ID, Status))
FORM_3 <- subset(output, FORM_3==1, select = c(ID, Status))
FORM_4 <- subset(output, FORM_4==1, select = c(ID, Status))
FORM_5 <- subset(output, FORM_5==1, select = c(ID, Status))
FORM_6 <- subset(output, FORM_6==1, select = c(ID, Status))
FORM_7 <- subset(output, FORM_7==1, select = c(ID, Status))
FORM_8 <- subset(output, FORM_8==1, select = c(ID, Status))
FORM_9 <- subset(output, FORM_9==1, select = c(ID, Status))
FORM_10 <- subset(output, FORM_10==1, select = c(ID, Status))



list_of_datasets <- list("Master" = output,
                         "FORM_1" = FORM_1,
                         "FORM_2" = FORM_2,
                         "FORM_3" = FORM_3,
                         "FORM_4" = FORM_4,
                         "FORM_5" = FORM_5,
                         "FORM_6" = FORM_6,
                         "FORM_7" = FORM_7,
                         "FORM_8" = FORM_8,
                         "FORM_9" = FORM_9,
                         "FORM_10" = FORM_10)


openxlsx::write.xlsx(list_of_datasets, file = "NAPLEX_forms_01_2023.xlsx")



# Output scoring file for SMART -------------------------------------------


#  Don't use this until we can autogenerate Area 6 !!!!!!!!!!!!!!!!!
SMART <- subset(OP_items, selected ==1 , select=c(IRTb, ID, Content))
SMART <- SMART %>%
  rename(
    CALIBRATION = IRTb,
    ITEM = ID,
    DOMAIN = Content
    )

# ExamVersion is the month and year of the publication
# e.g., 42021 is July 2021 and 12022 is January 2022

SMART$ExamVersion <- "42021"

openxlsx::write.xlsx(SMART, file = "SMART_scoring_42021.xlsx")


                                                 

# RUN TIME
end_time <- Sys.time()

end_time - start_time
