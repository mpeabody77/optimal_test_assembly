

# Load packages and data ---------------------------------------------------
library(readxl)
library(lpSolveAPI)
library(dplyr)
library(tidyr)

# !!! ONLY NEED THIS WHEN PULLING FROM EXAM STUDIO

#Bank_Collection <- read_excel("MPJE bank.xlsx", col_types = "text") %>%
#  separate_rows(Jurisdiction) %>%
#  mutate(Competency = gsub( " .*$", "", Competency ) ,
#         Content = as.numeric(substr(Competency, 1, 3)) ,
#         ID = as.character(QuestionId),
#         B = as.numeric(IrtB)) %>%
#  filter(Jurisdiction == "MI") %>%
#  # This randomizes the rows so that it gets a better distribution of the items
#  slice(., sample(1:n()))


# !!! THIS IS PULLED FROM THE TEXT FILES SENT TO PEARSON
Bank_Collection <- read.delim('Mi_1601.txt', header = FALSE) %>%
  rename("ID" = "V1",
         "Competency" = "V2",
         "B" = "V3") %>%
  # ensure these items are masked
  filter(ID != "60016337" |
           ID != "60014222" |
           ID != "23017220" |
           ID != "60014014" |
           ID != "24011010" ) %>%
  mutate(Competency = stringr::str_trim(Competency, side = c("both")),
         Status = "Scored")

  




# !!! THIS IS PULLED FROM THE TEXT FILES SENT TO PEARSON
MI_pretest <- read.delim('Mi_40.txt', header = FALSE, colClasses=c("character")) %>%
  rename("ID" = "V1") %>%
  filter(ID != "60016337") %>%
  filter(ID != "60014222") %>%
  filter(ID != "23017220") %>%
  filter(ID != "60014014") %>% 
  filter(ID != "24011010" )
           
  

bank <- read_excel("MPJE bank.xlsx")

XP_items <- merge(MI_pretest, bank, by.x = "ID", by.y = "QuestionId", all.x = TRUE) %>%
  mutate(Competency = gsub( " .*$", "", Competency )) %>%
  filter(!is.na(Status)) %>%
  mutate(Status = "Pretest") %>%
  slice(., sample(20)) %>%
  select(ID, Status)


# Create operational test forms for each state ----------------------------

# Create 3rd level competency categories                                      # N items per comp.
Bank_Collection$Content[Bank_Collection$Competency=="1.1.1"] <- 1         #   4
Bank_Collection$Content[Bank_Collection$Competency=="1.1.2"] <- 2         #   2
Bank_Collection$Content[Bank_Collection$Competency=="1.2.1"] <- 3         #   5
Bank_Collection$Content[Bank_Collection$Competency=="1.2.2"] <- 4         #   3
Bank_Collection$Content[Bank_Collection$Competency=="1.3.1"] <- 5         #   4
Bank_Collection$Content[Bank_Collection$Competency=="1.3.2"] <- 6         #   4
Bank_Collection$Content[Bank_Collection$Competency=="1.3.3"] <- 7         #   1
Bank_Collection$Content[Bank_Collection$Competency=="1.3.4"] <- 8         #   3
Bank_Collection$Content[Bank_Collection$Competency=="1.3.5"] <- 9         #   5
Bank_Collection$Content[Bank_Collection$Competency=="1.3.6"] <- 10        #   3
Bank_Collection$Content[Bank_Collection$Competency=="1.4.1"] <- 11        #   4
Bank_Collection$Content[Bank_Collection$Competency=="1.4.2"] <- 12        #   4
Bank_Collection$Content[Bank_Collection$Competency=="1.4.3"] <- 13        #   2
Bank_Collection$Content[Bank_Collection$Competency=="1.4.4"] <- 14        #   4
Bank_Collection$Content[Bank_Collection$Competency=="1.4.5"] <- 15        #   3
Bank_Collection$Content[Bank_Collection$Competency=="1.4.6"] <- 16        #   4
Bank_Collection$Content[Bank_Collection$Competency=="1.4.7"] <- 17        #   2
Bank_Collection$Content[Bank_Collection$Competency=="1.4.8"] <- 18        #   5
Bank_Collection$Content[Bank_Collection$Competency=="1.4.9"] <- 19        #   3
Bank_Collection$Content[Bank_Collection$Competency=="1.4.10"] <- 20       #   2
Bank_Collection$Content[Bank_Collection$Competency=="1.4.11"] <- 21       #   1
Bank_Collection$Content[Bank_Collection$Competency=="1.4.12"] <- 22       #   2
Bank_Collection$Content[Bank_Collection$Competency=="1.4.13"] <- 22       #
Bank_Collection$Content[Bank_Collection$Competency=="1.4.14"] <- 22       #
Bank_Collection$Content[Bank_Collection$Competency=="1.4.15"] <- 23       #   0
Bank_Collection$Content[Bank_Collection$Competency=="1.5.1"] <- 24        #   4
Bank_Collection$Content[Bank_Collection$Competency=="1.5.2"] <- 24        #
Bank_Collection$Content[Bank_Collection$Competency=="1.6.1"] <- 25        #   1
Bank_Collection$Content[Bank_Collection$Competency=="1.6.2"] <- 26        #   1
Bank_Collection$Content[Bank_Collection$Competency=="1.6.3"] <- 27        #   2
Bank_Collection$Content[Bank_Collection$Competency=="1.7.1"] <- 28        #   2
Bank_Collection$Content[Bank_Collection$Competency=="1.7.2"] <- 29        #   1
Bank_Collection$Content[Bank_Collection$Competency=="1.7.3"] <- 30        #   2
Bank_Collection$Content[Bank_Collection$Competency=="1.8.1"] <- 31        #   0
Bank_Collection$Content[Bank_Collection$Competency=="1.8.2"] <- 32        #   0
Bank_Collection$Content[Bank_Collection$Competency=="1.8.3"] <- 33        #   0
Bank_Collection$Content[Bank_Collection$Competency=="1.8.4"] <- 34        #   0
Bank_Collection$Content[Bank_Collection$Competency=="2.1.1"] <- 35        #   1
Bank_Collection$Content[Bank_Collection$Competency=="2.1.2"] <- 36        #   2
Bank_Collection$Content[Bank_Collection$Competency=="2.1.3"] <- 37        #   4
Bank_Collection$Content[Bank_Collection$Competency=="2.1.4"] <- 37        #
Bank_Collection$Content[Bank_Collection$Competency=="2.2.1"] <- 38        #   1
Bank_Collection$Content[Bank_Collection$Competency=="2.2.2"] <- 39        #   1
Bank_Collection$Content[Bank_Collection$Competency=="2.2.3"] <- 40        #   2
Bank_Collection$Content[Bank_Collection$Competency=="2.2.4"] <- 40        #   
Bank_Collection$Content[Bank_Collection$Competency=="2.3.1"] <- 41        #   1
Bank_Collection$Content[Bank_Collection$Competency=="2.3.2"] <- 42        #   2
Bank_Collection$Content[Bank_Collection$Competency=="2.3.3"] <- 43        #   1
Bank_Collection$Content[Bank_Collection$Competency=="3.1.1"] <- 44        #   2

OP_items <- subset(Bank_Collection, Status == "Scored")
#XP_items <- subset(Bank_Collection, Status == "Approved Not Used")





# Create operational test forms for each state ----------------------------


  
attach(OP_items)


# Define Variables --------------------------------------------------------

## For this example I'm using two tests of 50 items with 8 content categories.  
## n_content is the variable for the minimum number of items in each content category
## n_content sums to 48, giving the model a little wiggle room.

n_items <- nrow(OP_items) #Number of items in pool (I)
n_forms <- 1 #Number of test forms (F)
tif_points <- 1 #Number of theta points at which information if controlled (J)
form_length <- 100 #Length of both test forms (N)


##############
# the specifications for content categories need to be predefined for each state in a dataframe.
# example uses VA test specs where 100 items is exactly specified...very bad.


n_content <- c(
  4,
  2,
  5,
  3,
  4,
  4,
  1,
  3,
  5,
  3,
  4,
  4,
  2,
  4,
  3,
  4,
  2,
  5,
  3,
  2,
  1,
  2,
  0,
  4,
  1,
  1,
  2,
  2,
  1,
  2,
  0,
  0,
  0,
  0,
  1,
  2,
  4,
  1,
  1,
  2,
  1,
  2,
  1,
  2
)


# Content Categories ------------------------------------------------------

# Make list of sets of item indices for the k=1:8 content categories
# Observe double brackets around index of Vc
# Content==k gives logical selection of numbers in c(1:n_items) for which Content is k



Vc <- list()
for(k in 1:length(n_content)){
  Vc[[k]] <- c(1:n_items)[Content==k]
}


# Set TIF parameters ------------------------------------------------------


#theta <- c(-1.33, -0.33, 0.67) #Define θ points at which information is controlled
theta <- c(1.06461) #Define θ points at which information is controlled

Info <- array(0,c(n_items,tif_points)) #Define empty matrix with item information function values


#Fill matrix with item information function values (Rasch model)

for(j in 1:tif_points){
  P <-exp(theta[j]-B)/(1+exp(theta[j]-B))
  Q <- 1-P
  Info[,j] <- P*Q
}



# Set Linear Programming Parameters ---------------------------------------

# Number of decision variables number of items by the number of forms
# +1 is adding in the decision variable y, which is the left hand side of the equation
# M in Wim's paper

#n_dec_vars <- n_items*n_forms+1 # Number of decision variables (Two forms; y)
n_dec_vars <- n_items*n_forms # Number of decision variables (Two forms; y)


# Create empty model
lprec <- make.lp(0,n_dec_vars)


# Objective function in Eq. 2
#set.objfn(lprec,1,indices=n_dec_vars) #Argument: name of model, coefficient, variable
set.objfn(lprec,rep(Info,n_forms))

# Set Constraints ---------------------------------------------------------


# Constraints in Eqs. 3-4 (with y variable on the left side)
#d_theta <- c(6, 10, 6) #Target values for test information at 50 items
#d_theta <- c(16, 20, 16) #Target values for test information at 100 items


#for(k in 1:3){
#  add.constraint(lprec,c(Info[,k],-1),"<=",d_theta[k],indices=c(1:n_items,n_dec_vars)) #Form 1
#  add.constraint(lprec,c(Info[,k],-1),"<=",d_theta[k],indices=c((n_items+1):(2*n_items),n_dec_vars)) #Form 2
#  add.constraint(lprec,c(Info[,k],1),">=",d_theta[k],indices=c(1:n_items,n_dec_vars)) #Form 1
#  add.constraint(lprec,c(Info[,k],1),">=",d_theta[k],indices=c((n_items+1):(2*n_items),n_dec_vars)) #Form 2
#}





# Constraints in Eq. 6 (Set content categories Form 1)
for (k in 1:length(n_content)){
  add.constraint(lprec,rep(1,length(Vc[[k]])),">=",n_content[k],indices=Vc[[k]])
}



# Constraints in Eq. 7 (Sets the number of items in each form)
add.constraint(lprec, rep(1,n_items), "=", form_length, indices = 1:n_items)


# Constraints in Eqs. 8-9 
set.type(lprec,columns=c(1:(n_forms*n_items)),type="binary")
set.type(lprec,columns=n_dec_vars,type="real")
set.bounds(lprec,lower=rep(0,n_dec_vars),upper=rep(1,n_dec_vars))



# SOLVE  ------------------------------------------------------------------

# Set control parameters: minimization problem; integer tolerance is set to 0.1;
# absolute MIP gap is set to 0.1; relative MIP gap is set to 0.05
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


#Test information functions for Form 1 and 2

y = matrix(0,length(x),1) #Define empty matrix with item information function values along theta axis for all items

for (j in 1:length(x)){
  #Calculate test information function values along theta axis for Form 1
  y[j,1]=sum(decvar[1:n_items]*ItemInfo[,j]) 
  
} 

#Plot information functions

#jpeg(paste(i,"TIF.jpg",sep = "_"))
plot(x,y[,1], type="l", lty=1, xlab='Theta',ylab='Information', col="black")

#dev.off()







# Assign Items to Forms ---------------------------------------------------

OP_items$FORM_1 <-decvar[1:n_items]


# Create output tables ----------------------------------------------------




table.blueprint<-OP_items %>%
  group_by(Content) %>%
  summarize(A=sum(FORM_1)
  )


table.competency <- OP_items %>%
  group_by(Competency) %>%
  summarize(A=sum(FORM_1)
  )



form_1_array<-OP_items$ID[OP_items$FORM_1==1]






# Checking normality of the item difficulty distributions

require(ggplot2)
require(tidyr)
OP_items %>%
  select(ID, B, FORM_1) %>%
  pivot_longer(cols = starts_with("FORM"), names_to = "FORM", values_to = "SELECTED") %>%
  filter(SELECTED == 1) %>%
  ggplot(., aes(B)) +
  geom_density() +
  facet_wrap(~ FORM)


# check the overlap between forms

OP_items %>%
  select(ID, B, FORM_1) %>%
  pivot_longer(cols = starts_with("FORM"), names_to = "FORM", values_to = "SELECTED") %>%
  filter(SELECTED == 1) %>%
  group_by(ID) %>%
  summarize(N = n()) %>%
  group_by(N) %>%
  summarize(N_forms = n())















# Write output file -------------------------------------------------------



FORM_1 <- OP_items %>%
  pivot_longer(cols = starts_with("FORM"), names_to = "FORM", values_to = "SELECTED") %>%
  filter(FORM == "FORM_1" & SELECTED == 1) %>%
  select(ID, Status) %>% 
  rbind(., XP_items) %>%
  slice(., sample(1:n()))



list_of_datasets <- list("Master" = OP_items,
                         "FORM_1" = FORM_1
                         )


openxlsx::write.xlsx(list_of_datasets, file = "MI_paper_pencil_MPJE_9_13_2022.xlsx")
  
  