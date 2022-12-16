

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

Bank_Collection <- read_excel("2020 Jan MPJE Metadata with States.xlsx", sheet = "Jan 2020 MPJE Metadata")
state_bank <- read_excel("2020 Jan MPJE Metadata with States.xlsx", sheet = "States")


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

#state_bank <- read.csv("State Banks.csv", stringsAsFactors = TRUE)

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
  n_forms <- 4 #Number of test forms (F)
  tif_points <- 1 #Number of theta points at which information if controlled (J)
  form_length <- 100 #Length of both test forms (N)
  
  
  ##############
  # the specifications for content categories need to be predefined for each state in a dataframe.
  # example uses VA test specs where 100 items is exactly specified...very bad.
  
  n_content <- c(6,8,20,36,4,3,5,0,7,4,4,2) #Numbers of items required from k=1:8 content categories (Nc)
  
  ##############
  
  
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
  
  
  
  
  
  
  
  # Constraints in Eq. 5 (Ensures no overlap in tests)
  for (k in 1:n_items){
    add.constraint(lprec,rep(1,4),"<=",3,indices=c(k,
                                                     n_items+k, 
                                                     2*n_items+k,
                                                     3*n_items+k)
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
  
  
  
  # Constraints in Eq. 7 (Sets the number of items in each form)
  add.constraint(lprec, rep(1,n_items), "=", form_length, indices = 1:n_items)
  add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (n_items+1):(2*n_items))
  add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (2*n_items+1):(3*n_items))
  add.constraint(lprec, rep(1,n_items), "=", form_length, indices = (3*n_items+1):(4*n_items))
  
  
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
  
  y = matrix(0,length(x),4) #Define empty matrix with item information function values along theta axis for all items
  
  for (j in 1:length(x)){
    #Calculate test information function values along theta axis for Form 1
    y[j,1]=sum(decvar[1:n_items]*ItemInfo[,j]) 
    
    #Calculate test information function values along theta axis for Form 2
    y[j,2]=sum(decvar[(n_items+1):(2*n_items)]*ItemInfo[,j])
    
    #Calculate test information function values along theta axis for Form 3
    y[j,3]=sum(decvar[(2*n_items+1):(3*n_items)]*ItemInfo[,j])
    
    #Calculate test information function values along theta axis for Form 4
    y[j,4]=sum(decvar[(3*n_items+1):(4*n_items)]*ItemInfo[,j])
    
  } 
  
  #Plot information functions
  
  #jpeg(paste(i,"TIF.jpg",sep = ' '))
  plot(x,y[,1], type="l", lty=1, xlab='Theta',ylab='Information', col="black")
  lines(x,y[,2], type="l", lty=1, xlab='Theta',ylab='Information', col="blue")
  lines(x,y[,3], type="l", lty=1, xlab='Theta',ylab='Information', col="red")
  lines(x,y[,4], type="l", lty=1, xlab='Theta',ylab='Information', col="green")
  
  title(i)
  #dev.off()
  
  # Assign Items to Forms ---------------------------------------------------
  
  
  
  aux$FORM_A <-decvar[1:n_items]
  aux$FORM_B <-decvar[(n_items+1):(2*n_items)]
  aux$FORM_C <-decvar[(2*n_items+1):(3*n_items)]
  aux$FORM_D <-decvar[(3*n_items+1):(4*n_items)]
  
  
  
  blueprint.table<-aux %>%
    group_by(Content) %>%
    summarize(A=sum(FORM_A),
              B=sum(FORM_B), 
              C=sum(FORM_C),
              D=sum(FORM_D)
    )%>%
    mutate(State = i)
  
  form.a<-subset(aux, FORM_A==1)
  form.b<-subset(aux, FORM_B==1)
  form.c<-subset(aux, FORM_C==1)
  form.d<-subset(aux, FORM_D==1)
  
  #plot(density(form.a$B),  xlab="Theta", ylab = "Density", main=NA)
  #lines(density(form.b$B),lty=2)
  #abline(v=theta, col="blue")
  #title(i)
  
  assign(paste(i, "forms", sep="_"), aux)
  assign(paste(i, "table", sep="_"), blueprint.table)
  
}


list_tables <- lapply(ls(pattern="??_table"), function(x) get(x))
state_tables<-do.call(rbind, list_tables)
write.csv(state_tables, "State Blueprints.csv", row.names=FALSE)




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