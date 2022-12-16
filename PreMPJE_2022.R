

start_time <- Sys.time()

# Load packages and data ---------------------------------------------------
require(readxl)
library(lpSolveAPI)
require(dplyr)
require(tidyr)

Bank_Collection <- read_excel("RadGridExport.xlsx")

Bank_Collection <- Bank_Collection %>%
  filter(Status == "Pre-Exam" & Jurisdiction != "")

# This regex takes everything before the first space in the string
Bank_Collection$domain <- sub(" .*", "", Bank_Collection$Competency)

state_bank <- separate_rows(Bank_Collection, Jurisdiction, sep = "\\|")
state_bank <- subset(state_bank, select = c(QuestionId, Jurisdiction))



last_year <- read.csv("PreMPJE_2021_Forms.csv", fileEncoding = 'UTF-8-BOM')
last_year <- separate_rows(last_year, Forms, sep = ", ")
last_year$Jurisdiction <- substr(last_year$Forms, 9, 10)
last_year$previous <- 1
last_year <- subset(last_year, select = c(QuestionId, Jurisdiction, previous))

state_bank<-merge(state_bank, last_year, by=c("QuestionId", "Jurisdiction"), all.x=TRUE)
state_bank$previous[is.na(state_bank$previous)] <- 2



# Create operational test forms for each state ----------------------------


# This regex takes everythign befor the second period in the string
Bank_Collection$Content<-gsub("^([^.]*.[^.]*)..*$", "\\1", Bank_Collection$Competency)

Bank_Collection$B <- as.numeric(Bank_Collection$IrtB)

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



OP_items <- subset(Bank_Collection, select = c(QuestionId, IrtB, domain, Content, B))

states <- unique(state_bank$Jurisdiction)


# Create operational test forms for each Jurisdiction ----------------------------


# This loop runs the test assembly for each Jurisdiction
for(i in states){
  aux <-state_bank[state_bank$Jurisdiction %in% i, ]
  aux <- merge(aux, OP_items, by = "QuestionId")
  assign(paste(i, "bank", sep="_"), aux)
  
  
  
  attach(aux)
  
  
  # Define Variables --------------------------------------------------------
  
  ## For this example I'm using two tests of 50 items with 8 content categories.  
  ## n_content is the variable for the minimum number of items in each content category
  ## n_content sums to 48, giving the model a little wiggle room.
  
  n_items <- nrow(aux) #Number of items in pool (I)
  n_forms <- 1 #Number of test forms (F)
  tif_points <- 1 #Number of theta points at which information if controlled (J)
  form_length <- 40 #Length of both test forms (N)
  
  # Allow for 25% overlap from the previous forms (1 = previously used, 2 = unused)
  #n_recent <- c(10, 30)
  
  # custom build forms for rogue states
  
  
  
 # ifelse(i == "CO",   n_content <-c(2, 3, 8, 14, 2, 1, 2, 0, 3, 2, 2, 1),
 #        ifelse(i == "CT",   n_content <-c(2, 3, 8, 14, 2, 1, 2, 0, 3, 1, 2, 1), 
 #               ifelse(i == "GA",   n_content <-c(2, 1, 8, 14, 1, 0, 2, 0, 3, 2, 2, 1), 
 #                      ifelse(i == "ID",   n_content <-c(2, 3, 8, 14, 2, 1, 2, 0, 1, 2, 2, 1), 
 #                             ifelse(i == "KS",   n_content <-c(2, 3, 8, 14, 2, 0, 2, 0, 3, 2, 2, 1), 
 #                                    ifelse(i == "KY",   n_content <-c(2, 1, 4, 14, 2, 1, 2, 0, 3, 2, 1, 1), 
 #                                           ifelse(i == "MA",   n_content <-c(2, 2, 8, 14, 0, 1, 2, 0, 2, 1, 2, 1), 
 #                                                  ifelse(i == "MD",   n_content <-c(2, 3, 8, 14, 1, 1, 2, 0, 3, 2, 2, 1), 
 #                                                         ifelse(i == "MO",   n_content <-c(2, 1, 8, 14, 2, 1, 2, 0, 1, 2, 2, 1), 
 #                                                                ifelse(i == "MS",   n_content <-c(2, 3, 8, 14, 1, 0, 2, 0, 3, 2, 2, 1), 
 #                                                                       ifelse(i == "NY",   n_content <-c(1, 3, 8, 14, 2, 1, 2, 0, 1, 2, 2, 1), 
 #                                                                              ifelse(i == "OH",   n_content <-c(2, 3, 8, 14, 2, 1, 2, 0, 1, 2, 2, 1), 
 #                                                                                     ifelse(i == "VA",   n_content <-c(2, 3, 8, 14, 1, 1, 2, 0, 2, 2, 2, 1), 
 #                                                                                            ifelse(i == "WI",   n_content <-c(2, 1, 8, 14, 2, 1, 1, 0, 3, 2, 2, 1),
 #                                                                                                   n_content <-c(2, 3, 8, 14, 2, 1, 2, 0, 3, 2, 2, 1)))))))))))))))
 #                             
 #             
                
 if (i == "CO") {
   n_content <-c(2, 3, 8, 14, 1, 0, 2, 0, 3, 2, 2, 1)
   }   else if (i == "CT") {
   n_content <-c(2, 3, 8, 14, 2, 1, 2, 0, 3, 1, 2, 1)
   }   else if (i == "GA") {
   n_content <-c(2, 1, 8, 14, 1, 0, 2, 0, 3, 2, 2, 1)
   }   else if (i == "ID") {
   n_content <-c(2, 3, 8, 14, 2, 1, 2, 0, 1, 2, 2, 1)
   }   else if (i == "KS") {
   n_content <-c(2, 3, 8, 14, 2, 0, 2, 0, 3, 2, 2, 1)
   }   else if (i == "KY") {
   n_content <-c(2, 1, 4, 14, 2, 1, 2, 0, 3, 2, 1, 1)
   }  else if (i == "MA")  {
   n_content <-c(2, 2, 8, 14, 0, 1, 2, 0, 0, 1, 2, 1)
   }   else if (i == "MD") {
   n_content <-c(2, 3, 8, 14, 1, 1, 2, 0, 3, 2, 2, 1)
   }   else if (i == "MO") {
   n_content <-c(2, 1, 8, 14, 2, 1, 2, 0, 1, 2, 2, 1)
   }   else if (i == "MS") {
   n_content <-c(2, 3, 8, 14, 1, 0, 2, 0, 3, 2, 2, 1)
   }   else if (i == "NJ") {
   n_content <-c(1, 1, 4, 14, 0, 0, 1, 0, 3, 1, 2, 1)
   }   else if (i == "NY") {
   n_content <-c(1, 3, 8, 14, 2, 1, 2, 0, 1, 2, 2, 1)
   }   else if (i == "OH") {
   n_content <-c(2, 3, 8, 14, 2, 1, 2, 0, 1, 2, 2, 1)
   }   else if (i == "VA") {
   n_content <-c(2, 3, 7, 14, 1, 1, 2, 0, 2, 2, 2, 1)
   }   else if (i == "WI") {
   n_content <-c(1, 1, 8, 14, 0, 1, 1, 0, 3, 1, 2, 1)
   }   else                {
   n_content <-c(2, 3, 8, 14, 2, 1, 2, 0, 3, 2, 2, 1)}

  

  if (i == "ID") {
    n_recent <-c(0, 40)
  }   else if (i == "KY") {
    n_recent <-c(30, 10)
  }   else if (i == "MO") {
    n_recent <-c(15, 25)
  }   else if (i == "TN") {
    n_recent <-c(15, 25)
  }   else if (i == "VA") {
    n_recent <-c(30, 10)
  }   else if (i == "WA") {
    n_recent <-c(15, 25)
  }  else                {
    n_recent <-c(10, 30)}
  
  
  
  
  # Content Categories ------------------------------------------------------
  
  Vc <- list()
  for(k in 1:length(n_content)){
    Vc[[k]] <- c(1:n_items)[Content==k]
  }

  
  
  
  # Recent Publication Overlap ----------------------------------------------
  
  
  Vp <- list()
  
  for(p in 1:length(n_recent)){
    Vp[[p]] <- c(1:n_items)[previous==p]
  }
  
  
  

  # Set TIF parameters ------------------------------------------------------
  
  
  theta <- c(1.06461) #Define θ points at which information is controlled
  
  Info <- array(0,c(n_items,tif_points)) #Define empty matrix with item information function values
  
  
  #Fill matrix with item information function values (Rasch model)
  
  for(j in 1:tif_points){
    P <-exp(theta[j]-B)/(1+exp(theta[j]-B))
    Q <- 1-P
    Info[,j] <- P*Q
  }
  
  
  
  # Set Linear Programming Parameters ---------------------------------------
  
 
  n_dec_vars <- n_items # Number of decision variables (Two forms; y)
  
  
  # Create empty model
  lprec <- make.lp(0,n_dec_vars)
  
  
  # Objective function in Eq. 2
  set.objfn(lprec,Info)
  
 
  
  # Set Constraints ---------------------------------------------------------
  
 
  # Content Categories
  for (k in 1:length(n_content)){
    add.constraint(lprec, rep(1,length(Vc[[k]])),">=",n_content[k],indices=Vc[[k]])
  }
  
  # Recent Publication overlap
  for (p in 1:length(n_recent)){
    add.constraint(lprec,rep(1,length(Vp[[p]])),"<=",n_recent[p],indices=Vp[[p]])
  }

  # Set the number of items in each form
  add.constraint(lprec, rep(1,n_items), "=", form_length, indices = 1:n_items)
  
  
  # Set the type of constraints
  set.type(lprec,columns=c(1:n_items),type="binary")
  set.type(lprec,columns=n_dec_vars,type="binary")
  set.bounds(lprec,lower=rep(0,n_dec_vars),upper=rep(1,n_dec_vars))
  
  
  
  # SOLVE  ------------------------------------------------------------------
  
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
    #y[j,2]=sum(decvar[(n_items+1):(2*n_items)]*ItemInfo[,j])
    
  } 
  
  #Plot information functions
  
  #jpeg(paste(i,"TIF.jpg",sep = "_"))
  plot(x,y[,1], type="l", lty=1, xlab='Theta',ylab='Information', col="black")
  #lines(x,y[,2], type="l", lty=1, xlab='Theta',ylab='Information', col="blue")
  
  title(i)
  #dev.off()
  
  # Assign Items to Forms ---------------------------------------------------
  
  
  aux$FORM_A <-decvar[1:n_items]
  assign(paste(i, "pool", sep="_"), aux)
  
  
  test<-subset(aux, select = c(QuestionId), FORM_A== 1)
  names(test)[1] <- paste(i)

  assign(paste(i, "test", sep="_"), test)

  
  
}


# make a table to check the content areas

list_of_forms <- lapply(ls(pattern="??_pool"), function(x) get(x))
all_forms<-do.call(rbind, list_of_forms)
all_forms <- subset(all_forms, FORM_A == 1)

table<-all_forms %>% 
  group_by(Jurisdiction) %>%
  summarize("1.1" = length(which(Content == "1")),
            "1.2" = length(which(Content == "2")),
            "1.3" = length(which(Content == "3")),
            "1.4" = length(which(Content == "4")),
            "1.5" = length(which(Content == "5")),
            "1.6" = length(which(Content == "6")),
            "1.7" = length(which(Content == "7")),
            "1.8" = length(which(Content == "8")),
            "2.1" = length(which(Content == "9")),
            "2.2" = length(which(Content == "10")),
            "2.3" = length(which(Content == "11")),
            "3.1" = length(which(Content == "12")))

table$tot <- apply(table[,2:13], 1, sum)







# Write output file -------------------------------------------------------


list_of_tests <- lapply(ls(pattern="??_test"), function(x) get(x))
all_tests<-do.call(cbind, list_of_tests)

output <- all_forms
#output <- subset(all_forms, select=c(QuestionId, JurisdCompetency, B,  FORM_A))


### Tables to show the overlap
new<-state_bank %>% 
  filter(previous == 2) %>%
  left_join(Bank_Collection, by = "QuestionId") %>%
  group_by(Jurisdiction.x, Content) %>%
  summarize(n = n()) %>%
  arrange(Content) %>%
  pivot_wider(names_from = Content,
              values_from = n,
              values_fill = 0)

new$total <- apply(new[, 2:13], 1, sum, na.rm = TRUE)



previous_year <- all_forms %>%
  group_by(Jurisdiction, previous) %>%
  summarize(n = n()) %>%
  pivot_wider(names_from = previous,
              values_from = n) %>%
  rename("Previous" = `1`, "New" = `2`)


### Push the final file

list_of_datasets <- list("Master" = output,
                         "states" = all_tests,
                         "Bank without last year" = new,
                         "Current form overlap" = previous_year)


openxlsx::write.xlsx(list_of_datasets, file = "Return_Collection_2022.xlsx")


# RUN TIME
end_time <- Sys.time()

end_time - start_time





