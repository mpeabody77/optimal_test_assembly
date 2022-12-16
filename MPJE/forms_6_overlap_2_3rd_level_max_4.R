

##  Diao, Q., & van der Linden, W.J. (2011). Automated Test Assembly Using lp_Solve Version 5.5 in R.
##       Applied Psychological Measurement, 35(5), 398-409.
##
##  Supplemented by 2015 NCME Workshop on Optimal Test Design by Wim van der Linden.
##  Example R Code for Assembly of Two Parallel Test Forms


start_time <- Sys.time()

# Load packages and data ---------------------------------------------------
require(readxl)
library(lpSolveAPI)
require(dplyr)

Bank_Collection <- read_excel("2020 Jan MPJE Metadata with States - Updated.xlsx", sheet = "Jan 2020 MPJE Metadata", skip=1, 
                              col_names = c("Item.ID",	"Competency",	"Status",	"Key",	"Calibration"))

state_bank <- read_excel("2020 Jan MPJE Metadata with States - Updated.xlsx", sheet = "States", 
                         col_names = c("Item.ID", "State", "ID_State"))


# Create operational test forms for each state ----------------------------


#Bank_Collection <- read.csv("MPJE Item Collection.csv", stringsAsFactors = TRUE)
Bank_Collection$Content <- substr(Bank_Collection$Competency, 1, 3)
Bank_Collection$B <- as.numeric(Bank_Collection$Calibration)

Bank_Collection$Content[Bank_Collection$Content=="1.1"] <- 1
Bank_Collection$Content[Bank_Collection$Content=="1.2"] <- 2
Bank_Collection$Content[Bank_Collection$Content=="1.3"] <- 3
Bank_Collection$Content[Bank_Collection$Content=="1.4"] <- 4
Bank_Collection$Content[Bank_Collection$Content=="1.5"] <- 5
Bank_Collection$Content[Bank_Collection$Content=="1.6"] <- 6
Bank_Collection$Content[Bank_Collection$Content=="1.7"] <- 7
Bank_Collection$Content[Bank_Collection$Content=="1.8"] <- 8
Bank_Collection$Content[Bank_Collection$Content=="2.1"] <- 9
Bank_Collection$Content[Bank_Collection$Content=="2.2"] <- 10
Bank_Collection$Content[Bank_Collection$Content=="2.3"] <- 11
Bank_Collection$Content[Bank_Collection$Content=="3.1"] <- 12
Bank_Collection$Content <- as.numeric(Bank_Collection$Content)

# Create 3rd level competency categories
Bank_Collection$third.level[Bank_Collection$Competency=="1.1.1"] <- 1
Bank_Collection$third.level[Bank_Collection$Competency=="1.1.2"] <- 2
Bank_Collection$third.level[Bank_Collection$Competency=="1.2.1"] <- 3
Bank_Collection$third.level[Bank_Collection$Competency=="1.2.2"] <- 4
Bank_Collection$third.level[Bank_Collection$Competency=="1.3.1"] <- 5
Bank_Collection$third.level[Bank_Collection$Competency=="1.3.2"] <- 6
Bank_Collection$third.level[Bank_Collection$Competency=="1.3.3"] <- 7
Bank_Collection$third.level[Bank_Collection$Competency=="1.3.4"] <- 8
Bank_Collection$third.level[Bank_Collection$Competency=="1.3.5"] <- 9
Bank_Collection$third.level[Bank_Collection$Competency=="1.3.6"] <- 10
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.1"] <- 11
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.10"] <- 12
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.11"] <- 13
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.12"] <- 14
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.13"] <- 15
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.14"] <- 16
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.15"] <- 17
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.2"] <- 18
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.3"] <- 19
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.4"] <- 20
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.5"] <- 21
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.6"] <- 22
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.7"] <- 23
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.8"] <- 24
Bank_Collection$third.level[Bank_Collection$Competency=="1.4.9"] <- 25
Bank_Collection$third.level[Bank_Collection$Competency=="1.5.1"] <- 26
Bank_Collection$third.level[Bank_Collection$Competency=="1.5.2"] <- 27
Bank_Collection$third.level[Bank_Collection$Competency=="1.6.1"] <- 28
Bank_Collection$third.level[Bank_Collection$Competency=="1.6.2"] <- 29
Bank_Collection$third.level[Bank_Collection$Competency=="1.6.3"] <- 30
Bank_Collection$third.level[Bank_Collection$Competency=="1.7.1"] <- 31
Bank_Collection$third.level[Bank_Collection$Competency=="1.7.2"] <- 32
Bank_Collection$third.level[Bank_Collection$Competency=="1.7.3"] <- 33
Bank_Collection$third.level[Bank_Collection$Competency=="1.8.1"] <- 34
Bank_Collection$third.level[Bank_Collection$Competency=="1.8.2"] <- 35
Bank_Collection$third.level[Bank_Collection$Competency=="1.8.3"] <- 36
Bank_Collection$third.level[Bank_Collection$Competency=="1.8.4"] <- 37
Bank_Collection$third.level[Bank_Collection$Competency=="2.1.1"] <- 38
Bank_Collection$third.level[Bank_Collection$Competency=="2.1.2"] <- 39
Bank_Collection$third.level[Bank_Collection$Competency=="2.1.3"] <- 40
Bank_Collection$third.level[Bank_Collection$Competency=="2.1.4"] <- 41
Bank_Collection$third.level[Bank_Collection$Competency=="2.2.1"] <- 42
Bank_Collection$third.level[Bank_Collection$Competency=="2.2.2"] <- 43
Bank_Collection$third.level[Bank_Collection$Competency=="2.2.3"] <- 44
Bank_Collection$third.level[Bank_Collection$Competency=="2.2.4"] <- 45
Bank_Collection$third.level[Bank_Collection$Competency=="2.3.1"] <- 46
Bank_Collection$third.level[Bank_Collection$Competency=="2.3.2"] <- 47
Bank_Collection$third.level[Bank_Collection$Competency=="2.3.3"] <- 48
Bank_Collection$third.level[Bank_Collection$Competency=="3.1.1"] <- 49

OP_items <- subset(Bank_Collection, Status == "S")
XP_items <- subset(Bank_Collection, Status == "P")

states <- unique(state_bank$State)



# Create operational test forms for each state ----------------------------


# This loop runs the test assembly for each state
for(i in states){
  aux <-state_bank[state_bank$State %in% i, ]
  aux <- merge(aux, OP_items, by = "Item.ID")
  assign(paste(i, "bank", sep="_"), aux)
  
  
  attach(aux)
  
  
  # Define Variables --------------------------------------------------------
  
  ## For this example I'm using two tests of 50 items with 8 content categories.  
  ## n_content is the variable for the minimum number of items in each content category
  ## n_content sums to 48, giving the model a little wiggle room.
  
  n_items <- nrow(aux) #Number of items in pool (I)
  n_forms <- 6 #Number of test forms (F)
  tif_points <- 1 #Number of theta points at which information if controlled (J)
  form_length <- 100 #Length of both test forms (N)
  
  
  ##############
  # the specifications for content categories need to be predefined for each state in a dataframe.
  # example uses VA test specs where 100 items is exactly specified...very bad.
  
  n_content <- c(6,8,20,36,4,3,5,0,7,4,4,2) #Numbers of items required from k=1:8 content categories (Nc)
  n_third_level <- c(4,4,4,4,4,4,4,4,4,4,
                     4,4,4,4,4,4,4,4,4,4,
                     4,4,4,4,4,4,4,4,4,4,
                     4,4,4,0,0,0,0,4,4,4,
                     4,4,4,4,4,4,4,4,4 )
  ##############
  
  
  # Content Categories ------------------------------------------------------
  
  # Make list of sets of item indices for the k=1:8 content categories
  # Observe double brackets around index of Vc
  # Content==k gives logical selection of numbers in c(1:n_items) for which Content is k
  
  
  
  Vc <- list()
  for(k in 1:length(n_content)){
    Vc[[k]] <- c(1:n_items)[Content==k]
  }
  
  
  # make a list of 3rd level competencies
  Vc2 <- list()
  for(k2 in 1:length(n_third_level)){
    Vc2[[k2]] <- c(1:n_items)[third.level==k2]
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
  
  
  
  
  
  
  
  # Constraints in Eq. 5 (Sets the number of forms an item can be selected to be less than or equal to a number)
  for (k in 1:n_items){
    add.constraint(lprec,rep(1,6),"<=",2,indices=c(k,
                                                   n_items+k, 
                                                   2*n_items+k,
                                                   3*n_items+k,
                                                   4*n_items+k,
                                                   5*n_items+k)
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
  
  
  
  # 3rd level constraints for each form------
  
  # Constraints in Eq. 6 (Set content categories Form 1)
  for (k2 in 1:length(n_third_level)){
    add.constraint(lprec,rep(1,length(Vc2[[k2]])),"<=",n_third_level[k2],indices=Vc2[[k2]])
  }
  # Constraints in Eq. 6 (Set content categories Form 2)
  for (k2 in 1:length(n_third_level)){
    add.constraint(lprec,rep(1,length(Vc2[[k2]])),"<=",n_third_level[k2],indices=n_items+Vc2[[k2]])
  }
  # Constraints in Eq. 6 (Set content categories Form 3)
  for (k2 in 1:length(n_third_level)){
    add.constraint(lprec,rep(1,length(Vc2[[k2]])),"<=",n_third_level[k2],indices=2*n_items+Vc2[[k2]])
  }
  # Constraints in Eq. 6 (Set content categories Form 4)
  for (k2 in 1:length(n_third_level)){
    add.constraint(lprec,rep(1,length(Vc2[[k2]])),"<=",n_third_level[k2],indices=3*n_items+Vc2[[k2]])
  }
  # Constraints in Eq. 6 (Set content categories Form 5)
  for (k2 in 1:length(n_third_level)){
    add.constraint(lprec,rep(1,length(Vc2[[k2]])),"<=",n_third_level[k2],indices=4*n_items+Vc2[[k2]])
  }
  # Constraints in Eq. 6 (Set content categories Form 6)
  for (k2 in 1:length(n_third_level)){
    add.constraint(lprec,rep(1,length(Vc2[[k2]])),"<=",n_third_level[k2],indices=5*n_items+Vc2[[k2]])
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Constraints in Eq. 7 (Sets the number of items in each form)
  add.constraint(lprec, rep(1,n_items), "=", form_length, indices = 1:n_items)
  add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (n_items+1):(2*n_items))
  add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (2*n_items+1):(3*n_items))
  add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (3*n_items+1):(4*n_items))
  add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (4*n_items+1):(5*n_items))
  add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (5*n_items+1):(6*n_items))
  
  
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
  
  y = matrix(0,length(x),6) #Define empty matrix with item information function values along theta axis for all items
  
  for (j in 1:length(x)){
    #Calculate test information function values along theta axis for Form 1
    y[j,1]=sum(decvar[1:n_items]*ItemInfo[,j]) 
    
    #Calculate test information function values along theta axis for Form 2
    y[j,2]=sum(decvar[(n_items+1):(2*n_items)]*ItemInfo[,j])
    
    #Calculate test information function values along theta axis for Form 3
    y[j,3]=sum(decvar[(2*n_items+1):(3*n_items)]*ItemInfo[,j])
    
    #Calculate test information function values along theta axis for Form 4
    y[j,4]=sum(decvar[(3*n_items+1):(4*n_items)]*ItemInfo[,j])
    
    #Calculate test information function values along theta axis for Form 4
    y[j,5]=sum(decvar[(4*n_items+1):(5*n_items)]*ItemInfo[,j])
    
    #Calculate test information function values along theta axis for Form 4
    y[j,6]=sum(decvar[(5*n_items+1):(6*n_items)]*ItemInfo[,j])
    
  } 
  
  #Plot information functions
  
  #jpeg(paste(i,"TIF.jpg",sep = "_"))
  plot(x,y[,1], type="l", lty=1, xlab='Theta',ylab='Information', col="black")
  lines(x,y[,2], type="l", lty=1, xlab='Theta',ylab='Information', col="blue")
  lines(x,y[,3], type="l", lty=1, xlab='Theta',ylab='Information', col="red")
  lines(x,y[,4], type="l", lty=1, xlab='Theta',ylab='Information', col="green")
  lines(x,y[,5], type="l", lty=1, xlab='Theta',ylab='Information', col="orange")
  lines(x,y[,6], type="l", lty=1, xlab='Theta',ylab='Information', col="brown")
  
  title(i)
  #dev.off()
  
  # Assign Items to Forms ---------------------------------------------------
  
  
  
  aux$FORM_A <-decvar[1:n_items]
  aux$FORM_B <-decvar[(n_items+1):(2*n_items)]
  aux$FORM_C <-decvar[(2*n_items+1):(3*n_items)]
  aux$FORM_D <-decvar[(3*n_items+1):(4*n_items)]
  aux$FORM_E <-decvar[(4*n_items+1):(5*n_items)]
  aux$FORM_F <-decvar[(5*n_items+1):(6*n_items)]
  
  
  second_level_table<-aux %>%
    group_by(Content) %>%
    summarize(A=sum(FORM_A),
              B=sum(FORM_B), 
              C=sum(FORM_C),
              D=sum(FORM_D),
              E=sum(FORM_E),
              F=sum(FORM_F)
    )%>%
    mutate(State = i)
  
  
  third_level_table <- aux %>%
    group_by(Competency) %>%
    summarize(A=sum(FORM_A),
              B=sum(FORM_B), 
              C=sum(FORM_C),
              D=sum(FORM_D),
              E=sum(FORM_E),
              F=sum(FORM_F)
    )%>%
    mutate(State = i)
  
  
  form.a<-subset(aux, FORM_A==1)
  form.b<-subset(aux, FORM_B==1)
  form.c<-subset(aux, FORM_C==1)
  form.d<-subset(aux, FORM_D==1)
  form.e<-subset(aux, FORM_E==1)
  form.f<-subset(aux, FORM_F==1)
  
 
  #plot(density(form.a$B),  xlab="Theta", ylab = "Density", main=NA)
  #lines(density(form.b$B),lty=2)
  #abline(v=theta, col="blue")
  #title(i)
  
  assign(paste(i, "forms", sep="_"), aux)
  assign(paste(i, "second_level", sep="_"), second_level_table)
  assign(paste(i, "third_level", sep="_"), third_level_table)
  
  # write a file for each state showing the item selection 0 or 1
  # write.csv(aux, paste(i, "forms.csv", sep="_"), row.names=FALSE)
  
  
}

# write a single file showing the blueprint category totals for each state and form
# this doesn't exactly work...
list_second_level_tables <- lapply(ls(pattern="??_second_level"), function(x) get(x))
second_level_by_form_and_state<-do.call(rbind, list_second_level_tables)
#write.table(second_level_by_form_and_state, "second_level_by_form_and_state.txt", sep=",", row.names=FALSE)

list_third_level_tables <- lapply(ls(pattern="??_third_level"), function(x) get(x))
third_level_by_form_and_state<-do.call(rbind, list_third_level_tables)
#write.table(third_level_by_form_and_state, "third_level_by_form_and_state.txt", sep=",",row.names=FALSE)


# single stacked file with forms for all states
all_forms_list <- lapply(ls(pattern="??_forms"), function(x) get(x))
all_state_forms<-do.call(rbind, all_forms_list)
write.table(all_state_forms, "all_state_forms.txt", sep=",",row.names=FALSE)




# Create XP test forms for each state -------------------------------------
# still need to do


for(i in states){
  aux <-state_bank[state_bank$State %in% i, ]
  aux <- merge(aux, XP_items, by = "Item.ID")
  assign(paste(i, "pretest_bank", sep="_"), aux)
}


# RUN TIME
end_time <- Sys.time()

end_time - start_time