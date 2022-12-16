start_time <- Sys.time()

# Load packages and data ---------------------------------------------------
library(readxl)
library(lpSolveAPI)
library(dplyr)

# Start by selecting the items from Exam Studio and exporting as XLSX.
# Can't do a csv because the forms are comma separated within the field.

# Select the following fields from the Exam Studio grid.
# -- QuestionId	
# -- Type	
# -- Status	
# -- Competency	
# -- Forms 
# -- Enemies	
# -- IrtB
# -- F Exam


# current available items
Bank_Collection <- read_excel("Item_Pool_11_3_2022.xlsx")


# remove items from recent forms using previous forms

Form_19 <- read_excel("./old forms/Form_19.xlsx", sheet = "Form_19", col_types = "text")
Form_20 <- read_excel("./old forms/Form_20.xlsx", sheet = "Form_20", col_types = "text")
Form_21 <- read_excel("./old forms/Form_21.xlsx", sheet = "Form_21", col_types = "text")
Form_22 <- read_excel("./old forms/Form_22.xlsx", sheet = "Form_22", col_types = "text")


last_forms <- plyr::rbind.fill(Form_19, Form_20, Form_21, Form_22)

remove <- unique(as.integer(last_forms$ITEM))

Bank_Collection <- subset(Bank_Collection, !(Bank_Collection$QuestionId %in% remove))



# Create New Variables ----------------------------------------------------

# Create a numeric variable for content categories
#Bank_Collection$Content <- substr(Bank_Collection$Competency, 4, 9)


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
Bank_Collection$Content[Bank_Collection$Content=="3.10"]<- 21
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
OP_items <- subset(Bank_Collection, Status == "Scored"  & `F Exam` == "PCOA")

# create sequence variable for consistent ordering
OP_items$seq <- seq(1:nrow(OP_items))

attach(OP_items)




# Define Variables --------------------------------------------------------

n_items <- nrow(OP_items) #Number of items in pool (I)
n_forms <- 2             #Number of test forms (F)
tif_points <- 5           #Number of theta points at which information if controlled (J)
form_length <- 200        #Length of both test forms (N)

# Area 3.10 is not covered and does not appear in this list.  
n_content <- c(6, 8, 2, 4, 15, 17, 2, 12, 8 ,6 ,6 ,6 ,3 ,2 ,6 ,5 ,3 ,3 ,7 ,3 ,6 , 10, 8 ,5 ,2 ,4 ,9 ,30) #Numbers of items required from k=1:8 content categories (Nc)
# changed the last category to 30 to give a little play




# Set TIF parameters ------------------------------------------------------

theta <- c(-2, -1, 0, 1, 2)
#theta <- c(-1.5,0,1.5)  #Define I? points at which information is controlled
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

# old way with AuthorWise file
#enemies$itemA.index<-match(enemies$ItemA, OP_items$ID)
#enemies$itemB.index<-match(enemies$ItemB, OP_items$ID)


#Ve <- list()

#library(purrr)
#Ve<-enemies[,3:4] %>% purrr::transpose() 
#Ve<-lapply(Ve, unlist, use.names=FALSE)
#Ve<-unique(lapply(Ve, sort))
#Ve<- Filter(function(x) length(x)>1, Ve)


# new way with Exam Studio file
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


d_theta <- c(27, 27, 27, 27, 27) #Target values for test information
for(k in 1:5){
  add.constraint(lprec,c(Info[,k],-1),"<=",d_theta[k],indices=c(1:n_items,n_dec_vars)) #Form 1
  add.constraint(lprec,c(Info[,k],-1),"<=",d_theta[k],indices=c((n_items+1):(2*n_items),n_dec_vars)) #Form 2
  add.constraint(lprec,c(Info[,k],1),">=",d_theta[k],indices=c(1:n_items,n_dec_vars)) #Form 1
  add.constraint(lprec,c(Info[,k],1),">=",d_theta[k],indices=c((n_items+1):(2*n_items),n_dec_vars)) #Form 2
}





# Constraints in Eq. 5 (Ensures no overlap in tests)
for (k in 1:n_items){
  add.constraint(lprec,rep(1,2),"<=",1,indices=c(k,
                                                 n_items+k) )
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
#for (k in 1:length(n_content)){
#  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=2*n_items+Vc[[k]])
#}
# Constraints in Eq. 6 (Set content categories Form 4)
#for (k in 1:length(n_content)){
#  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=3*n_items+Vc[[k]])
#}
# Constraints in Eq. 6 (Set content categories Form 5)
#for (k in 1:length(n_content)){
#  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=4*n_items+Vc[[k]])
#}



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
#for (e in 1:length(Ve)){
#  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=2*n_items+Ve[[e]])
#}
# Set enemies  Form 4
#for (e in 1:length(Ve)){
#  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=3*n_items+Ve[[e]])
#}
# Set enemies  Form 5
#for (e in 1:length(Ve)){
#  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=4*n_items+Ve[[e]])
#}



# Constraints in Eq. 7 (Sets the number of items in each form)
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = 1:n_items)
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (n_items+1):(2*n_items))
#add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (2*n_items+1):(3*n_items))
#add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (3*n_items+1):(4*n_items))
#add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (4*n_items+1):(5*n_items))




# Constraints in Eqs. 8-9 
set.type(lprec,columns=c(1:(n_forms*n_items)),type="binary")
set.type(lprec,columns=n_dec_vars,type="real")
set.bounds(lprec,lower=rep(0,n_dec_vars),upper=rep(1,n_dec_vars))



# SOLVE  ------------------------------------------------------------------

# set to maximize the objective function at the desired point
#lp.control(lprec,sense="max");
lp.control(lprec,sense="min",epsint = 0.1, mip.gap=c(0.1,0.05));

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
  #y[j,3]=sum(decvar[(2*n_items+1):(3*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 4
  #y[j,4]=sum(decvar[(3*n_items+1):(4*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 5
  #y[j,5]=sum(decvar[(4*n_items+1):(5*n_items)]*ItemInfo[,j])
  
 } 

#Plot information functions

#jpeg("TIF.jpg")
plot(x,y[,1], type="l", lty=1, xlab='Theta',ylab='Information', col="gray1", ylim=c(0,30))
lines(x,y[,2], type="l", lty=1, xlab='Theta',ylab='Information', col="gray2")
#lines(x,y[,3], type="l", lty=1, xlab='Theta',ylab='Information', col="gray3")
#lines(x,y[,4], type="l", lty=1, xlab='Theta',ylab='Information', col="gray4")
#lines(x,y[,5], type="l", lty=1, xlab='Theta',ylab='Information', col="gray5")
abline(v=theta, h=d_theta)



#title(i)
#dev.off()

end_time <- Sys.time()

end_time - start_time

# Assign Items to Forms ---------------------------------------------------



OP_items$FORM_23 <-decvar[1:n_items]
OP_items$FORM_24 <-decvar[(n_items+1):(2*n_items)]
#OP_items$FORM_3 <-decvar[(2*n_items+1):(3*n_items)]
#OP_items$FORM_4 <-decvar[(3*n_items+1):(4*n_items)]
#OP_items$FORM_5 <-decvar[(4*n_items+1):(5*n_items)]




# Create output tables ----------------------------------------------------

table.blueprint<-OP_items %>%
  group_by(Content) %>%
  summarize(f1=sum(FORM_23),
            f2=sum(FORM_24)
  )


table.competency <- OP_items %>%
  group_by(Competency) %>%
  summarize(f1=sum(FORM_23),
            f2=sum(FORM_24)
  )




#table.item.type <- OP_items %>%
#  group_by(ItemType) %>%
#  summarize(f1=sum(FORM_1)#,
#            #f2=sum(FORM_2), 
#            #f3=sum(FORM_3),
#            #f4=sum(FORM_4),
#            #f5=sum(FORM_5),
#            #f6=sum(FORM_6),
#            #f7=sum(FORM_7),
#            #f8=sum(FORM_8),
#            #f9=sum(FORM_9),
#            #f10=sum(FORM_10)
#  )



form_1_array<-OP_items$ID[OP_items$FORM_23==1]
form_2_array<-OP_items$ID[OP_items$FORM_24==1]
#form_3_array<-OP_items$ID[OP_items$FORM_3==1]
#form_4_array<-OP_items$ID[OP_items$FORM_4==1]
#form_5_array<-OP_items$ID[OP_items$FORM_5==1]
#form_6_array<-OP_items$ID[OP_items$FORM_6==1]
#form_7_array<-OP_items$ID[OP_items$FORM_7==1]
#form_8_array<-OP_items$ID[OP_items$FORM_8==1]
#form_9_array<-OP_items$ID[OP_items$FORM_9==1]
#form_10_array<-OP_items$ID[OP_items$FORM_10==1]


#  tables for enemy items

enemies<-enemies %>%
  mutate (f1 = ifelse(ItemA %in% form_1_array & ItemB %in% form_1_array, 1, 0),
          f2 = ifelse(ItemA %in% form_2_array & ItemB %in% form_2_array, 1, 0),
          #f3 = ifelse(ItemA %in% form_3_array & ItemB %in% form_3_array, 1, 0),
          #f4 = ifelse(ItemA %in% form_4_array & ItemB %in% form_4_array, 1, 0),
          #f5 = ifelse(ItemA %in% form_5_array & ItemB %in% form_5_array, 1, 0),
          #f6 = ifelse(ItemA %in% form_6_array & ItemB %in% form_6_array, 1, 0),
          #f7 = ifelse(ItemA %in% form_7_array & ItemB %in% form_7_array, 1, 0),
          #f8 = ifelse(ItemA %in% form_8_array & ItemB %in% form_8_array, 1, 0),
          #f9 = ifelse(ItemA %in% form_9_array & ItemB %in% form_9_array, 1, 0),
          #f10 = ifelse(ItemA %in% form_10_array & ItemB %in% form_10_array, 1, 0) 
          )


# Table produces a 0/1 result if any enemy pairs are found in the form
table.enemies <- enemies %>%
  summarize(f1 = max(f1),
            f2 = max(f2),
            #f3 = max(f3),
            #f4 = max(f4),
            #f5 = max(f5),
            #f6 = max(f6),
            #f7 = max(f7),
            #f8 = max(f8),
            #f9 = max(f9),
            #f10 = max(f10)
  )





# Checking normality of the item difficulty distributions

di1 <- subset(OP_items, FORM_23==1, select = B)
hist(di1$B)
di2 <- subset(OP_items, FORM_24==1, select = B)
hist(di2$B)
#di3 <- subset(OP_items, FORM_3==1, select = B)
#hist(di3$B)
#di4 <- subset(OP_items, FORM_4==1, select = B)
#hist(di4$B)
#di5 <- subset(OP_items, FORM_5==1, select = B)
#hist(di5$B)
#di6 <- subset(OP_items, FORM_6==1, select = B)
#hist(di6$B)
#di7 <- subset(OP_items, FORM_7==1, select = B)
#hist(di7$B)
#di8 <- subset(OP_items, FORM_8==1, select = B)
#hist(di8$B)
#di9 <- subset(OP_items, FORM_9==1, select = B)
#hist(di9$B)
#di10 <- subset(OP_items, FORM_10==1, select = B)
#hist(di10$B)



############################################################################################################
############################################################################################################
############################################################################################################
############################################################################################################


# Pretest Items -----------------------------------------------------------

PT_items <- subset(Bank_Collection, Status == "Approved Not Used")
#PT_items$B <- -0.33
# create sequence variable for consistent ordering
PT_items$seq <- seq(1:nrow(PT_items))




# Sets the B values for each subset as a normal distribution rather than assigning all the same value.  
# Then the algorithm pulls pulls a few items from cat1 before the optimization is better by using cat2, and so on.
PT_items<-PT_items %>% 
  group_by(Content) %>%
  mutate(B = rnorm(n(), -.33, .01))

#ggplot(PT_items, aes(B))+
#  geom_density(aes(color=as.factor(Content)))

attach(PT_items)

# Define Variables --------------------------------------------------------

n_items <- nrow(PT_items) #Number of items in pool (I)
n_forms <- 5             #Number of test forms (F)
tif_points <- 1           #Number of theta points at which information if controlled (J)
# make 200 when we have enough Area 6 items.
form_length <- 25        #Length of both test forms (N)


# Ensures the pretest form matches the item pool
#
#PT_pool <- PT_items %>%
#  group_by(Content) %>%
#  summarize(Pool = length(ID)) %>%
#  mutate(PCT = Pool / sum(Pool), 
#         Form = floor(form_length * PCT) 
#         )
#n_content <- as.vector(PT_pool$Form)




#Hard coded to the 1st level blueprint
#n_content <- c(2, 1, 8, 13) #Numbers of items required from k=1:8 content categories (Nc)

#Hard coded for the 2nd level, but there are more categories than items.
n_content <- c(1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1) #Numbers of items required from k=1:8 content categories (Nc)


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

#enemies$itemA.index<-match(enemies$ItemA, PT_items$ID)
#enemies$itemB.index<-match(enemies$ItemB, PT_items$ID)


#Ve <- list()

#library(purrr)
#Ve<-enemies[,3:4] %>% purrr::transpose() 
#Ve<-lapply(Ve, unlist, use.names=FALSE)
#Ve<-unique(lapply(Ve, sort))
#Ve<- Filter(function(x) length(x)>1, Ve)


#enemies$itemA.index<-match(enemies$ItemA, Bank_Collection$ID)
#enemies$itemB.index<-match(enemies$ItemB, Bank_Collection$ID)
#
#
#Ve <- list()
#
#library(purrr)
#Ve<-enemies[,3:4] %>% purrr::transpose() 
#Ve<-lapply(Ve, unlist, use.names=FALSE)
#Ve<-unique(lapply(Ve, sort))
#Ve<- Filter(function(x) length(x)>1, Ve)


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

# Number of decision variables number of items by the number of forms
n_dec_vars <- n_items*n_forms 
# Create empty model
lprec <- make.lp(0,n_dec_vars)
# Objective function
set.objfn(lprec,rep(Info,n_forms))




# Set Constraints ---------------------------------------------------------


# Constraints in Eq. 5 (Ensures no overlap in tests)
for (k in 1:n_items){
  add.constraint(lprec,rep(1,5),"<=",1,indices=c(k,
                                                  n_items+k, 
                                                  2*n_items+k,
                                                  3*n_items+k,
                                                  4*n_items+k#,
                                                  #5*n_items+k,
                                                  #6*n_items+k,
                                                  #7*n_items+k,
                                                  #8*n_items+k,
                                                  #9*n_items+k
                                                 )
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
#for (k in 1:length(n_content)){
#  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=5*n_items+Vc[[k]])
#}
# Constraints in Eq. 6 (Set content categories Form 7)
#for (k in 1:length(n_content)){
#  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=6*n_items+Vc[[k]])
#}
# Constraints in Eq. 6 (Set content categories Form 8)
#for (k in 1:length(n_content)){
#  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=7*n_items+Vc[[k]])
#}
# Constraints in Eq. 6 (Set content categories Form 9)
#for (k in 1:length(n_content)){
#  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=8*n_items+Vc[[k]])
#}
# Constraints in Eq. 6 (Set content categories Form 10)
#for (k in 1:length(n_content)){
#  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=9*n_items+Vc[[k]])
#}


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
#for (e in 1:length(Ve)){
#  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=5*n_items+Ve[[e]])
#}
# Set enemies  Form 7
#for (e in 1:length(Ve)){
#  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=6*n_items+Ve[[e]])
#}
# Set enemies  Form 8
#for (e in 1:length(Ve)){
#  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=7*n_items+Ve[[e]])
#}
# Set enemies  Form 9
#for (e in 1:length(Ve)){
#  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=8*n_items+Ve[[e]])
#}
# Set enemies  Form 10
#for (e in 1:length(Ve)){
#  add.constraint(lprec,rep(1,length(Ve[[e]])),"<=",1,indices=9*n_items+Ve[[e]])
#}


# Constraints in Eq. 7 (Sets the number of items in each form)
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = 1:n_items)
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (n_items+1):(2*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (2*n_items+1):(3*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (3*n_items+1):(4*n_items))
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (4*n_items+1):(5*n_items))
#add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (5*n_items+1):(6*n_items))
#add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (6*n_items+1):(7*n_items))
#add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (7*n_items+1):(8*n_items))
#add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (8*n_items+1):(9*n_items))
#add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (9*n_items+1):(10*n_items))



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




y = matrix(0,length(x),n_forms) #Define empty matrix with item information function values along theta axis for all items

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
  #y[j,6]=sum(decvar[(5*n_items+1):(6*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 7
  #y[j,7]=sum(decvar[(6*n_items+1):(7*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 8
  #y[j,8]=sum(decvar[(7*n_items+1):(8*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 9
  #y[j,9]=sum(decvar[(8*n_items+1):(9*n_items)]*ItemInfo[,j])
  
  #Calculate test information function values along theta axis for Form 10
  #y[j,10]=sum(decvar[(9*n_items+1):(10*n_items)]*ItemInfo[,j])
  
} 

#Plot information functions

#jpeg("TIF.jpg")
plot(x,y[,1], type="l", lty=1, xlab='Theta',ylab='Information', col="red")
lines(x,y[,2], type="l", lty=1, xlab='Theta',ylab='Information', col="blue")
lines(x,y[,3], type="l", lty=1, xlab='Theta',ylab='Information', col="green")
lines(x,y[,4], type="l", lty=1, xlab='Theta',ylab='Information', col="orange")
lines(x,y[,5], type="l", lty=1, xlab='Theta',ylab='Information', col="black")
#lines(x,y[,6], type="l", lty=1, xlab='Theta',ylab='Information', col="gray6")
#lines(x,y[,7], type="l", lty=1, xlab='Theta',ylab='Information', col="gray7")
#lines(x,y[,8], type="l", lty=1, xlab='Theta',ylab='Information', col="gray8")
#lines(x,y[,9], type="l", lty=1, xlab='Theta',ylab='Information', col="gray9")
#lines(x,y[,10], type="l", lty=1, xlab='Theta',ylab='Information', col="gray10")


#title(i)
#dev.off()

# Assign Items to Forms ---------------------------------------------------



PT_items$FORM_1 <-decvar[1:n_items]
PT_items$FORM_2 <-decvar[(n_items+1):(2*n_items)]
PT_items$FORM_3 <-decvar[(2*n_items+1):(3*n_items)]
PT_items$FORM_4 <-decvar[(3*n_items+1):(4*n_items)]
PT_items$FORM_5 <-decvar[(4*n_items+1):(5*n_items)]
#PT_items$FORM_6 <-decvar[(5*n_items+1):(6*n_items)]
#PT_items$FORM_7 <-decvar[(6*n_items+1):(7*n_items)]
#PT_items$FORM_8 <-decvar[(7*n_items+1):(8*n_items)]
#PT_items$FORM_9 <-decvar[(8*n_items+1):(9*n_items)]
#PT_items$FORM_10 <-decvar[(9*n_items+1):(10*n_items)]




pt.table.blueprint<-PT_items %>%
  group_by(Content) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2), 
            f3=sum(FORM_3),
            f4=sum(FORM_4),
            f5=sum(FORM_5)#,
            #f6=sum(FORM_6),
            #f7=sum(FORM_7),
            #f8=sum(FORM_8),
            #f9=sum(FORM_9),
            #f10=sum(FORM_10)
  )




pt.table.competency<-PT_items %>%
  group_by(Competency) %>%
  summarize(f1=sum(FORM_1),
            f2=sum(FORM_2), 
            f3=sum(FORM_3),
            f4=sum(FORM_4),
            f5=sum(FORM_5)#,
            #f6=sum(FORM_6),
            #f7=sum(FORM_7),
            #f8=sum(FORM_8),
            #f9=sum(FORM_9),
            #f10=sum(FORM_10)
  )





# Write output file -------------------------------------------------------

OP_items$selected <- apply(OP_items[,13:14], 1, sum)
PT_items$selected <- apply(PT_items[,13:17], 1, sum)


# For the operational forms, forms 1-5 are the same and forms 6-10 are the same.
# This allows us to spread 10 pretest blocks across two operational forms.
#OP_items <- rename(OP_items, FORM_6 = FORM_2)
#OP_items <- OP_items %>% 
#  mutate(FORM_2 = FORM_1,
#         FORM_3 = FORM_1,
#         FORM_4 = FORM_1,
#         FORM_5 = FORM_1,
#         FORM_7 = FORM_6,
#         FORM_8 = FORM_6,
#         FORM_9 = FORM_6,
#         FORM_10 = FORM_6)
#OP_items <- OP_items %>% relocate(FORM_6, .after = FORM_5)
#OP_items <- OP_items %>% relocate(selected, .after = FORM_10)


#OP_items$selected <- OP_items$FORM_1
#OP_items$FORM_2 <- OP_items$FORM_1
#OP_items$FORM_3 <- OP_items$FORM_1
#OP_items$FORM_4 <- OP_items$FORM_1
#OP_items$FORM_5 <- OP_items$FORM_1
###

OP_output <- subset(OP_items, selected == 1 , select=c(QuestionId, Type, Competency,
                                         Status, IrtB, FORM_23, FORM_24))

                            

PT_output <- subset(PT_items, selected == 1 , select=c(QuestionId, Type, Competency,
                                         Status, IrtB                ))

OP_swaps <- subset(OP_items, selected < 1, 
                   select=c(QuestionId, Type, Competency, Status, IrtB, 
                            FORM_23, FORM_24
                            ))


PT_swaps <- subset(PT_items, selected < 1, 
                   select=c(QuestionId, Type, Competency, Status, IrtB, 
                            FORM_1, FORM_2, FORM_3, FORM_4, FORM_5#,
                            #FORM_6, FORM_7, FORM_8, FORM_9, FORM_10
                            ))

output <- rbind(OP_output, OP_swaps
                #PT_output, 
                
                #PT_swaps
                )


#output$Status <- recode(output$Status, 
#                            "Scored Items (Sco)" = "Scored",
#                            "Approved Items (Apv)" = "Pretest")

FORM_23 <- subset(output, FORM_23 == 1, select = c(QuestionId, Status))
FORM_24 <- subset(output, FORM_24 == 1, select = c(QuestionId, Status))

#FORM_3 <- subset(output, FORM_3==1, select = c(QuestionId, Status))
#FORM_4 <- subset(output, FORM_4==1, select = c(QuestionId, Status))
#FORM_5 <- subset(output, FORM_5==1, select = c(QuestionId, Status))
#FORM_6 <- subset(output, FORM_6==1, select = c(ID, Status))
#FORM_7 <- subset(output, FORM_7==1, select = c(ID, Status))
#FORM_8 <- subset(output, FORM_8==1, select = c(ID, Status))
#FORM_9 <- subset(output, FORM_9==1, select = c(ID, Status))
#FORM_10 <- subset(output, FORM_10==1, select = c(ID, Status))





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
                         "FORM_23" = FORM_23,
                         "FORM_24" = FORM_24,
                         #"FORM_3" = FORM_3,
                         #"FORM_4" = FORM_4,
                         #"FORM_5" = FORM_5,
                         #"FORM_6" = FORM_6,
                         #"FORM_7" = FORM_7,
                         #"FORM_8" = FORM_8,
                         #"FORM_9" = FORM_9,
                         #"FORM_10" = FORM_10,
                         "OP_swaps" = OP_swaps,
                         #"PT_swaps" = PT_swaps, 
                         "enemies"= enemies2,
                         "issues" = issues)


openxlsx::write.xlsx(list_of_datasets, file = "PCOA_Initial_Build_Forms_23_24 (11_7_2022)_v2.xlsx")



# RUN TIME
end_time <- Sys.time()

end_time - start_time
