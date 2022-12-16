start_time <- Sys.time()

# Load packages and data ---------------------------------------------------
library(readxl)
library(lpSolveAPI)
library(dplyr)
library(tidyr)

Bank_Collection <- read.csv("RadGridExport_PreNAPLEX_FEB_3_2022.csv", fileEncoding = "UTF-8-BOM")

# Not all calibrations in Exam Studio.  Need to add.
rescale <- read_excel("PreNAPLEX rescaling.xlsx")
old_forms <- read_excel('Forms_2021.xlsx') %>%
  select(ID) %>%
  mutate(ID = as.numeric(ID))


Bank_Collection <- merge(Bank_Collection, rescale, by = "QuestionId", all.x = TRUE) %>%
  filter(!(QuestionId %in% old_forms$ID))


enemies <- subset(Bank_Collection, select = c(QuestionId, Enemies)) %>%
  filter(Enemies != "") %>%
  separate_rows(Enemies, sep = ",") %>%
  mutate(Enemies = as.numeric(Enemies))

drug_class <- subset(Bank_Collection, select = c(QuestionId, Drug.Class)) %>%
  filter(Drug.Class != "") %>%
  separate_rows(Drug.Class, sep = "\\|")

med_conditions <- subset(Bank_Collection, select = c(QuestionId, Medical.Conditions)) %>%
  filter(Medical.Conditions != "") %>%
  separate_rows(Medical.Conditions, sep = "\\|")
  



# Create New Variables ----------------------------------------------------

# Create a numeric variable for content categories
Bank_Collection$Content <- as.numeric(substr(Bank_Collection$Competency, 1, 1))
Bank_Collection$domain <- sub(" .*", "", Bank_Collection$Competency)



# Create a numeric variable for item difficulty
Bank_Collection$B <- as.numeric(Bank_Collection$IRTb)

# Create a numeric variable for item type
Bank_Collection$type <- NA
Bank_Collection$type[Bank_Collection$ItemType=="Multiple Choice"] <- 1
Bank_Collection$type[Bank_Collection$ItemType=="Free Response"] <- 2
Bank_Collection$type[Bank_Collection$ItemType=="Ordered"] <- 3
Bank_Collection$type[Bank_Collection$ItemType=="Hot Spot"] <- 4




# Operational Items -------------------------------------------------------


# subset for only Operational Items
OP_items <- subset(Bank_Collection, Status == "Pre-Exam")

# create sequence variable for consistent ordering
OP_items$seq <- seq(1:nrow(OP_items))

# create scenario id numbers
OP_items   <- OP_items %>%
  group_by(Cases) %>%
  arrange(Cases)%>%
  mutate(scenario_id = cur_group_id()) %>%
  arrange(seq) %>%
  ungroup()


Scenario_list <- subset(OP_items, 
                        #scenario_id != 1, 
                        select = c("QuestionId", "seq", "scenario_id" ))




# !!!!!!!!!!!!!!!!!!!!!!!!! remove items from area 6 until July 2021 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
OP_items <- subset(OP_items, Content != 6)
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



attach(OP_items)


# Define Variables --------------------------------------------------------

n_items <- nrow(OP_items) #Number of items in pool (I)
n_forms <- 2             #Number of test forms (F)
tif_points <- 1           #Number of theta points at which information if controlled (J)
# make 200 when we have enough Area 6 items.
form_length <- 100        #Length of both test forms (N)
n_scenarios <- length(unique(na.omit(Cases)))

# Not enough items in Area 6 (14 items), so we need to build without it. 
n_content <- c(18,14,35,14,11) #Numbers of items required from k=1:8 content categories (Nc)

# Need to specify the number of items from each cognitive domain
# currently using a 30/30/10 % split with 20% free 

n_types <- c(80, 10, 0, 0)




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

enemies$itemA.index<-match(enemies$QuestionId, OP_items$QuestionId)
enemies$itemB.index<-match(enemies$Enemies, OP_items$QuestionId)

enemies <- rename(enemies, c(ItemA=QuestionId, ItemB=Enemies))

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


# Item Types --------------------------------------------------------------

Vi <- list()

for(i in 1:length(n_types)){
  Vi[[i]] <- c(1:n_items)[type==i]
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
  add.constraint(lprec,rep(1,2),"<=",1,indices=c(k,
                                                 n_items+k))}




# Constraints in Eq. 6 (Set content categories Form 1)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=Vc[[k]])
}
# Constraints in Eq. 6 (Set content categories Form 2)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=n_items+Vc[[k]])
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



# Constraints in Eq. 7 (Sets the number of items in each form)
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = 1:n_items)
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (n_items+1):(2*n_items))




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




# Constraint for Item Types

# Form 1
for (i in 1:length(n_types)){
  add.constraint(lprec,rep(1,length(Vi[[i]])),">=",n_types[i],indices=Vi[[i]])
}
# Form 2
for (i in 1:length(n_types)){
  add.constraint(lprec,rep(1,length(Vi[[i]])),">=",n_types[i],indices=n_items+Vi[[i]])
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
} 

#Plot information functions

#jpeg("TIF.jpg")
plot(x,y[,1], type="l", lty=1, xlab='Theta',ylab='Information', col="red")
lines(x,y[,2], type="l", lty=1, xlab='Theta',ylab='Information', col="blue")



#title(i)
#dev.off()

# Assign Items to Forms ---------------------------------------------------


OP_items$FORM_1 <-decvar[1:n_items]
OP_items$FORM_2 <-decvar[(n_items+1):(2*n_items)]




# Create output tables ----------------------------------------------------

table.blueprint<-OP_items %>%
  group_by(Content) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2))


table.competency <- OP_items %>%
  group_by(Competency) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2)  )


table.scenario <- OP_items %>%
  group_by(scenario_id) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2)  )



table.item.type <- OP_items %>%
  group_by(ItemType) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2)  )




#Number of scenarios per form
table.scenario %>%
  summarize(f1=length(unique(scenario_id[table.scenario$f1>0])),
            f2=length(unique(scenario_id[table.scenario$f2>0])))


form_1_array<-OP_items$QuestionId[OP_items$FORM_1==1]
form_2_array<-OP_items$QuestionId[OP_items$FORM_2==1]


#  tables for enemy items

enemies<-enemies %>%
  mutate (f1 = ifelse(ItemA %in% form_1_array & ItemB %in% form_1_array, 1, 0),
          f2 = ifelse(ItemA %in% form_2_array & ItemB %in% form_2_array, 1, 0) )


# Table produces a 0/1 result if any enemy pairs are found in the form
table.enemies <- enemies %>%
  summarize(f1 = max(f1),
            f2 = max(f2)  )







# Write output file -------------------------------------------------------


OP_items$ selected <- apply(OP_items[,19:20], 1, sum)
OP_output <- subset(OP_items, selected ==1 , select=c(QuestionId, Type, Competency,
                                         Cases, Status, IRTb, FORM_1, FORM_2))


FORM_1 <- subset(OP_output, FORM_1==1, select = c(QuestionId))
FORM_2 <- subset(OP_output, FORM_2==1, select = c(QuestionId))


list_of_datasets <- list("All Items" = OP_items,
                         "Selected" = OP_output,
                         "FORM_1" = FORM_1,
                         "FORM_2" = FORM_2)


openxlsx::write.xlsx(list_of_datasets, file = "Return_Collection_2022.xlsx")



# RUN TIME
end_time <- Sys.time()

end_time - start_time
