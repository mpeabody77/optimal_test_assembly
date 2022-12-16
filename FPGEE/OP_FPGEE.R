start_time <- Sys.time()

# Load packages and data ---------------------------------------------------
library(readxl)
library(lpSolveAPI)
library(dplyr)



# current available items
Bank_Collection <- read_excel("PCOA_FPGEE_Grid_Export_7_13_2022.xlsx")
Bank_Collection <- subset(Bank_Collection, `F Exam` == "FPGEE")



# remove items from recent forms using previous forms

Fall_2018_OP <- read_excel("./old forms/Fall 2018.xlsx", sheet = "FPGEE Fall 2018 OP", col_types = "text")
Fall_2018_PT <- read_excel("./old forms/Fall 2018.xlsx", sheet = "FPGEE Fall 2018 PT", col_types = "text")

Spring_2018_OP <- read_excel("./old forms/Spring 2018.xlsx", sheet = "FPGEE Spring 2018 OP", col_types = "text")
Spring_2018_PT <- read_excel("./old forms/Spring 2018.xlsx", sheet = "FPGEE Spring 2018 PT", col_types = "text")


Fall_2019_OP <- read_excel("./old forms/Fall 2019.xlsx", sheet = "FPGEE Fall 2019 OP", col_types = "text")
Fall_2019_PT <- read_excel("./old forms/Fall 2019.xlsx", sheet = "FPGEE Fall 2019 PT", col_types = "text")

Spring_2019_OP <- read_excel("./old forms/Spring 2019.xlsx", sheet = "FPGEE Spring 2019 OP", col_types = "text")
Spring_2019_PT <- read_excel("./old forms/Spring 2019.xlsx", sheet = "FPGEE Spring 2019 PT", col_types = "text")

Fall_2020_OP <- read_excel("./old forms/Fall 2020.xlsx", sheet = "FPGEE Fall 2020 OP", col_types = "text")

Fall_2021_OP <- read_excel("./old forms/Fall 2021.xlsx", sheet = "FPGEE Fall 2021 OP", col_types = "text")


last_forms <- plyr::rbind.fill(Fall_2018_OP, Fall_2018_PT, 
                          Spring_2018_OP, Spring_2018_PT,
                          Fall_2019_OP, Fall_2019_PT,
                          Spring_2019_OP, Spring_2019_PT,
                          Fall_2020_OP,
                          Fall_2021_OP )
                          
remove <- unique(as.integer(last_forms$Item_ID))

Bank_Collection <- subset(Bank_Collection, !(Bank_Collection$QuestionId %in% remove))







# Create New Variables ----------------------------------------------------

# Create a numeric variable for content categories
#Bank_Collection$Content <- as.character(substr(Bank_Collection$Competency, 1, 3))
#
library(magrittr)
Bank_Collection$Content <- strsplit(Bank_Collection$Competency, split="\\.(?=[^.]+$)", perl=TRUE) %>% sapply(extract2, 1)

Bank_Collection$Content[Bank_Collection$Content=="1.1"] <- 1
Bank_Collection$Content[Bank_Collection$Content=="1.2"] <- 2
Bank_Collection$Content[Bank_Collection$Content=="1.3"] <- 3
Bank_Collection$Content[Bank_Collection$Content=="1.4"] <- 4
Bank_Collection$Content[Bank_Collection$Content=="2.1"] <- 5
Bank_Collection$Content[Bank_Collection$Content=="2.2"] <- 6
Bank_Collection$Content[Bank_Collection$Content=="2.3"] <- 7
Bank_Collection$Content[Bank_Collection$Content=="2.4"] <- 8
Bank_Collection$Content[Bank_Collection$Content=="2.5"] <- 9
Bank_Collection$Content[Bank_Collection$Content=="2.6"] <- 10
Bank_Collection$Content[Bank_Collection$Content=="2.7"] <- 11
Bank_Collection$Content[Bank_Collection$Content=="3.1"] <- 12
Bank_Collection$Content[Bank_Collection$Content=="3.2"] <- 13
Bank_Collection$Content[Bank_Collection$Content=="3.3"] <- 14
Bank_Collection$Content[Bank_Collection$Content=="3.4"] <- 15
Bank_Collection$Content[Bank_Collection$Content=="3.5"] <- 16
Bank_Collection$Content[Bank_Collection$Content=="3.6"] <- 17
Bank_Collection$Content[Bank_Collection$Content=="3.7"] <- 18
Bank_Collection$Content[Bank_Collection$Content=="3.8"] <- 19
Bank_Collection$Content[Bank_Collection$Content=="3.9"] <- 20
Bank_Collection$Content[Bank_Collection$Content=="3.10"] <- 21
Bank_Collection$Content[Bank_Collection$Content=="4.1"] <- 22
Bank_Collection$Content[Bank_Collection$Content=="4.2"] <- 23
Bank_Collection$Content[Bank_Collection$Content=="4.3"] <- 24
Bank_Collection$Content[Bank_Collection$Content=="4.4"] <- 25
Bank_Collection$Content[Bank_Collection$Content=="4.5"] <- 26
Bank_Collection$Content[Bank_Collection$Content=="4.6"] <- 27
Bank_Collection$Content[Bank_Collection$Content=="4.7"] <- 28

Bank_Collection$Content <- as.numeric(Bank_Collection$Content)






# Create a numeric variable for item difficulty
Bank_Collection$B <- as.numeric(Bank_Collection$IrtB)
Bank_Collection$ID <- as.numeric(Bank_Collection$QuestionId)

# This randomizes the rows so that it gets a better distribution of the 3rd levels
Bank_Collection <- Bank_Collection[sample(nrow(Bank_Collection)),]

# Operational Items -------------------------------------------------------


# subset for only Operational Items
OP_items <- subset(Bank_Collection, Status == "Scored" & `F Exam` == "FPGEE")

# create sequence variable for consistent ordering
OP_items$seq <- seq(1:nrow(OP_items))

attach(OP_items)




# Define Variables --------------------------------------------------------

n_items <- nrow(OP_items) #Number of items in pool (I)
n_forms <- 1             #Number of test forms (F)
tif_points <- 1           #Number of theta points at which information if controlled (J)
form_length <- 200        #Length of both test forms (N)
n_content <- c(6, 8, 2, 4, 15, 17, 2, 12, 8 ,6 ,6 ,6 ,3 ,2 ,6 ,5 ,3 ,3 ,7 ,3 ,6 , 10, 8 ,5 ,2 ,4 ,9 ,32) 






# Set TIF parameters ------------------------------------------------------

#theta <- c(-2, -1, 0, 1, 2)

theta <- (-0.16779)
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

enemies<- subset(OP_items, select = c(QuestionId, Enemies))

library(dplyr)
library(tidyr)

enemies<-enemies %>%
  mutate(Enemies = strsplit(gsub("[][\"]", "", Enemies), ",")) %>%
  unnest(Enemies)

enemies$Enemies <- as.integer(trimws(enemies$Enemies))

enemies$itemA.index<-match(enemies$QuestionId, OP_items$ID)
enemies$itemB.index<-match(enemies$Enemies, OP_items$ID)

enemies <- rename(enemies, c(ItemA=QuestionId, ItemB=Enemies))

Ve <- list()

library(purrr)
Ve<-enemies[,3:4] %>% purrr::transpose() 
Ve<-lapply(Ve, unlist, use.names=FALSE)
Ve<-unique(lapply(Ve, sort))
Ve<- Filter(function(x) length(x)>1, Ve)



# Set Linear Programming Parameters ---------------------------------------

# Number of decision variables number of items by the number of forms + 1 for the TIF posts
n_dec_vars <- n_items*n_forms+1 

# Create empty model
lprec <- make.lp(0,n_dec_vars)

# Objective function
set.objfn(lprec, 1, indices=n_dec_vars)




# Set Constraints ---------------------------------------------------------

# Constraints in Eq. 5 (Ensures no overlap in tests)
#for (k in 1:n_items){
#  add.constraint(lprec,rep(1,2),"<=",1,indices=c(k,
#                                                 n_items+k) )
#}
# 

# Constraints in Eq. 6 (Set content categories Form 1)

for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=Vc[[k]])
}

# Constraints in Eq. 6 (Set content categories Form 2)
#for (k in 1:length(n_content)){
#  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=n_items+Vc[[k]])
#}


# Constraints for enemy items


# Set enemies  Form 1
for (e in 1:length(Ve)){
  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=Ve[[e]])
}

# Set enemies  Form 2
#for (e in 1:length(Ve)){
#  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=n_items+Ve[[e]])
#}




# Constraints in Eq. 7 (Sets the number of items in each form)
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = 1:n_items)
#add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (n_items+1):(2*n_items))



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
x <- seq(-3, 3, .01)   #Define theta axis

#Define empty matrix with item information function values along theta axis for all items
ItemInfo <- array(0,c(n_items, length(x))) 

for (j in 1:length(x)){
  P <-exp(x[j]-B)/(1+exp(x[j]-B))
  Q <- 1-P
  ItemInfo[,j] <- P*Q
} #Calculate item information function values along theta axis for all items 




y = matrix(0,length(x),1) #Define empty matrix with item information function values along theta axis for all items

for (j in 1:length(x)){
  #Calculate test information function values along theta axis for Form 1
  y[j,1]=sum(decvar[1:n_items]*ItemInfo[,j]) 
  
  #Calculate test information function values along theta axis for Form 2
  #y[j,2]=sum(decvar[(n_items+1):(2*n_items)]*ItemInfo[,j])
} 

#Plot information functions

#jpeg("TIF.jpg")
plot(x,y[,1], type="l", lty=1, xlab='Theta',ylab='Information', col="gray1", ylim=c(0,50))
#lines(x,y[,2], type="l", lty=1, xlab='Theta',ylab='Information', col="gray2")
abline(v=theta)
#title(i)
#dev.off()

end_time <- Sys.time()

end_time - start_time

# Assign Items to Forms ---------------------------------------------------



OP_items$FORM_1 <-decvar[1:n_items]
#OP_items$FORM_2 <-decvar[(n_items+1):(2*n_items)]



# Create output tables ----------------------------------------------------

table.blueprint<-OP_items %>%
  group_by(Content) %>%
  summarize(f1=sum(FORM_1)#,
            #f2=sum(FORM_2),
            )


table.competency <- OP_items %>%
  group_by(Competency) %>%
  summarize(f1=sum(FORM_1)#,
            #f2=sum(FORM_2), 
            )


table.item.type <- OP_items %>%
  group_by(Type) %>%
  summarize(f1=sum(FORM_1)#,
            #f2=sum(FORM_2), 
            )



form_1_array<-OP_items$ID[OP_items$FORM_1==1]
#form_2_array<-OP_items$ID[OP_items$FORM_2==1]


#  tables for enemy items

enemies<-enemies %>%
  mutate (f1 = ifelse(ItemA %in% form_1_array & ItemB %in% form_1_array, 1, 0)#,
          #f2 = ifelse(ItemA %in% form_2_array & ItemB %in% form_2_array, 1, 0),
          )


# Table produces a 0/1 result if any enemy pairs are found in the form
table.enemies <- enemies %>%
  summarize(f1 = max(f1)#,
            #f2 = max(f2),
            )





# Checking normality of the item difficulty distributions

di1 <- subset(OP_items, FORM_1==1, select = B)
hist(di1$B)
#di2 <- subset(OP_items, FORM_2==1, select = B)



# Write output file -------------------------------------------------------

#OP_items$selected <- apply(OP_items[,18], 1, sum)
# guess you can't apply a single column
OP_items$selected <- OP_items$FORM_1

OP_output <- subset(OP_items, selected ==1 , select=c(QuestionId, Type, Competency, Status, IrtB, FORM_1 ) )

OP_swaps <- subset(OP_items, selected < 1, select=c(QuestionId, Type, Competency, Status, IrtB, FORM_1 ) )


output <- rbind(OP_output, OP_swaps)


#output$Status <- recode(output$Status, "Scored Items (Sco)" = "Scored"  )



FORM_1 <- subset(output, FORM_1==1, select = c(QuestionId, Status))

# Add a column in the OP & PT swaps that list the enemy items.
## seems to work, but only spot check...

issues<-enemies %>%
  mutate (f1 = ifelse(ItemA %in% form_1_array & ItemB %in% form_1_array, 1, 0) ) %>%
  filter(f1 == 1) %>%
  select(ItemA, ItemB) %>%
  filter(!duplicated(paste0(pmax(ItemA, ItemB), pmin(ItemA, ItemB))))
      
enemies2<-enemies %>%
  select(ItemA, ItemB)

list_of_datasets <- list("Master" = output,
                         "FORM_1" = FORM_1,
                         "OP_swaps" = OP_swaps,
                         "enemies"= enemies2,
                         "issues" = issues)


openxlsx::write.xlsx(list_of_datasets, file = "FPGEE Fall 2022 Return_Collection (7.13.2022).xlsx")



# RUN TIME
end_time <- Sys.time()

end_time - start_time
