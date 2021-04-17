rm(list = ls())
setwd("~/Desktop/University/Third Year/Clubs/SEA/SEA Code")
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
Mentees <- read_csv("Game_Day-Mentees.csv")
Mentors <- read_csv("Game_Day-Mentors.csv")
Num_mentees4mentors <-read_csv("Number of Mentees for Mentors.csv")
#This function works best if we order the excel sheet by how important it is to
#a mentee that they get a specific mentor field

#Calculates the pairing score for one mentee and each of the mentors (should 
#output a (1 x number of mentors) dataframe)
mentee_score <- function(Mentee,y){
  scores <- c()
  for (j in 1:nrow(y)){
    Mentor <- y[j,]
    s = 0
    # If a mentor is leader we divide the importance of the mentee to have a
    # mentor in a leadership position by 2 and add it to their score
    if (Mentee$Area == Mentor$Area){
      s = s + Mentee$`Area Importance`*2
    }
    if (Mentee$`Secondary Area` == Mentor$Area){
      s = s + (6 - Mentee$`Area Importance`)/3
    }
    s = s + Mentee$Leadership*Mentor$Leadership/2
    if (Mentor$Career == Mentee$Career){
      #If a mentor has the desired career a mentee desired the importance score
      #is added to the score
      s = s + Mentee$`Career Importance`
    }
    if (Mentor$`Age Preference` == "3rd/4th year"){
      if (Mentee$Year == 3 | 4){
        s = s + 1
      }
      else if (Mentee$Year == 1 | 2){
        s = s - 10
      }
    }
    if (Mentor$`Age Preference` == "1st/2nd year"){
      if (Mentee$Year == 1 | 2){
        s = s + 1
      }
      else if (Mentee$Year == 3 | 4){
        s = s - 4
      }
    }
    if (Mentor$Hobby == Mentee$Hobby){
      s = s + 2.5
    }
    if (Mentee$Mentor != "No Preference"){
      if (Mentor$Name==Mentee$Mentor){
        s = s + 30
      }
    }
    # Add mentor choice + 40
    if (Mentor$Mentee != "No Preference"){
      if (Mentee$Name==Mentor$Mentee){
        s = s + 30
      }
    }
    if (y$Frequency[j]==Mentee$Frequency){
      s = s + 2
    }
    scores <- c(scores,s)
  }
  return(scores)
}

z <- mentee_score(Mentees[1,],Mentors)


#Iterates the mentee_score function to calculate scores for all mentees and 
#mentors and stores them in a data frame (num mentees x num mentors)
score_matrix <- function(x,y){
  num_mentees <- nrow(x)
  num_mentors <- nrow(y)
  score_sheet <- data.frame(matrix(ncol = num_mentors, nrow = num_mentees))
  rownames(score_sheet) <- x$Name
  colnames(score_sheet)<- y$Name
  for (i in 1:num_mentees){
    score_sheet[i,] <- mentee_score(x[i,],y)
  }
  return(score_sheet)
}
mat <- score_matrix(Mentees,Mentors)

change_num <-function(x){
  if(x == 1){
    return(2)
  }
  else if(x==2){
    return(1)
  }
}


#pairs the mentee and mentor pairing that has the highest score and removes that 
#mentee and mentor for the score sheet
single_pair <- function(z){
  pair <- c()
  numrow <- nrow(z)
  num <- which(z==max(z))
  num <- min(num)
  col <- floor(num/numrow)
  row <- num - col*numrow
  if (numrow > ncol(z)){
    if(numrow == 2 && ncol(z)){
      bool = T
    }else{
      bool = F
    }
  }
  if (floor(num/numrow) != num/numrow){
    col <- col + 1
  } else{
      row <- numrow
    }
  pair <- c(pair,rownames(z)[row])
  pair <- c(pair,colnames(z)[col])
    z <- z[-row,]
    z <- z[,-col]
  

  return(list(z,pair))
}

y <- single_pair(mat)

new_pair <- function(x,y){
 n1 <- nrow(x)
 n2 <- nrow(y)
 n <- c(n1,n2)
 num <- which(n==max(n))
 iterations <- min(n) - 1
 mat <- score_matrix(x,y)
 mentee_paired <- data.frame(matrix(ncol = 2, nrow = 0))
 name <- c("Mentee","Mentor")
 colnames(mentee_paired) <- name
 for (i in 1:iterations){
   info <- single_pair(mat)
   mat <- info[[1]]
   mentee_paired[i,] <- info[[2]]
 }
 return(list(mentee_paired,mat))
}

final_pair <- function(x,p){
  if(ncol(x)!=nrow(x)){
    stop("No need to run this function because num(col) != num(row)")
  }
  row_names <- rownames(x)
  col_names <- colnames(x)
  print(p)
  for (i in 1:nrow(p)){
    t <- match(p[i,1],row_names)
    row_names = row_names[-t]
    y <- match(p[i,2],col_names)
    col_names = col_names[-y]
  }
  return(c(row_names,col_names))
}


p <- new_pair(Mentees,Mentors)
pairs <- p[[1]]


x <- final_pair(mat,pairs)
pairs <- rbind(pairs,x)

find_unpaired <- function(paired,original){
  original <- original$Name
  if (length(original)==length(paired)){
    stop("No unpaired")
  }
  count = 1
  for (i in 1:length(paired)){
    exist <- match(paired[count],original)
    if (is.na(exist) == F){
      paired = paired[-count]
      original = original[-exist]
    } else {
      count = count + 1
    }
  }
  return(original)
}

unpaired_mentors <- find_unpaired(pairs[,2],Mentors)

unpaired_mentees <- find_unpaired(pairs[,1],Mentees)

email_lookup <- function(paired_list,Me,Mo){
  numrows = nrow(paired_list)
  complete_list <- data.frame(matrix(ncol = 4, nrow = numrows))
  name <- c("Mentee","Mentee Email","Mentor","Mentor Email")
  colnames(complete_list) <- name
  Me = Me[1:2]
  Mo = Mo[1:2]
  print(Mo)
  print(paired_list)
  for (i in 1:nrow(paired_list)){
    index <- match(paired_list$Mentee[i],Me$Name)
    complete_list$`Mentee Email`[i] <- Me$Email[index] 
    complete_list$`Mentee`[i] <- Me$Name[index] 
    index <- match(paired_list$Mentor[i],Mo$Name)
    complete_list$`Mentor Email`[i] <- Mo$Email[index]
    complete_list$`Mentor`[i] <- Mo$Name[index] 
    
  }
  return(complete_list)
}

get_mentees_data <- function(full,leftover){
  completed <- c()
  for(i in 1:length(leftover)){
    print(leftover)
    new_ <- full %>%
      filter(`Name`==leftover[i])
    completed <- rbind(completed,new_)
  }
  return(completed)
}

make_final_pair <- mat[unpaired_mentees,]
print(make_final_pair)
print(unpaired_mentors)
View(make_final_pair)
last <- readline(prompt="Enter name: ")
#/////////


pairs <- rbind(pairs,c(last,unpaired_mentors))
unpaired_mentees <- find_unpaired(pairs[,1],Mentees)

#///////////// Stop here


get_mentees_data <- function(full,leftover){
  completed <- c()
  for(i in 1:length(leftover)){
    new_ <- full %>%
      filter(`Name`==leftover[i])
    completed <- rbind(completed,new_)
  }
  return(completed)
}


unpaired_mentees <- get_mentees_data(Mentees, unpaired_mentees)
two_mentees_mentors <- Mentors %>%
  filter(`Number of Mentees` > 1)

p <- new_pair(unpaired_mentees,two_mentees_mentors)
pairs2 <- p[[1]]
x <- final_pair(mat,pairs2)
pairs2 <- rbind(pairs2,x)

pairs <- rbind(pairs,pairs2)

unpaired_mentors <- find_unpaired(pairs2[,2],two_mentees_mentors)

unpaired_mentees <- find_unpaired(pairs[,1],unpaired_mentees)
make_final_pair <- mat[unpaired_mentees,]
print(make_final_pair)
print(unpaired_mentors)
View(make_final_pair)
last <- readline(prompt="Enter name: ")

#////////


pairs <- rbind(pairs,c(last,unpaired_mentors))
unpaired_mentees <- find_unpaired(pairs[,1],Mentees)

pairs_w_email <- email_lookup(pairs,Mentees,Mentors)
print(pairs_w_email)

write.csv(pairs_w_email,"Pairing Results")




unpaired_mentees <- get_mentees_data(Mentees, unpaired_mentees)
three_mentees_mentors <- Mentors %>%
  filter(`Number of Mentees` > 2)

p <- new_pair(unpaired_mentees,three_mentees_mentors)
pairs3 <- p[[1]]
x <- final_pair(mat,pairs3)
pairs3 <- rbind(pairs3,x)
pairs <- rbind(pairs,pairs3)

unpaired_mentors <- find_unpaired(pairs3[,2],three_mentees_mentors)

unpaired_mentees <- find_unpaired(pairs[,1],unpaired_mentees)
make_final_pair <- mat[unpaired_mentees,]

make_final_pair <- mat[unpaired_mentees,]
print(make_final_pair)
print(unpaired_mentors)
View(make_final_pair)
last <- readline(prompt="Enter name: ")
#////////


pairs <- rbind(pairs,c(last,unpaired_mentors))
unpaired_mentees <- find_unpaired(pairs[,1],Mentees)

pairs_w_email <- email_lookup(pairs,Mentees,Mentors)
print(pairs_w_email)






unpaired_mentees <- get_mentees_data(Mentees, unpaired_mentees)
four_mentees_mentors <- Mentors %>%
  filter(`Number of Mentees` > 3)

p <- new_pair(unpaired_mentees,four_mentees_mentors)
pairs4 <- p[[1]]
x <- final_pair(mat,pairs4)
pairs4 <- rbind(pairs4,x)
pairs <- rbind(pairs,pairs4)

unpaired_mentors <- find_unpaired(pairs4[,2],four_mentees_mentors)

unpaired_mentees <- find_unpaired(pairs[,1],unpaired_mentees)
make_final_pair <- mat[unpaired_mentees,]

make_final_pair <- mat[unpaired_mentees,]
print(make_final_pair)
print(unpaired_mentors)
View(make_final_pair)
last <- readline(prompt="Enter name: ")
#////////


pairs <- rbind(pairs,c(last,unpaired_mentors))
unpaired_mentees <- find_unpaired(pairs[,1],Mentees)

pairs_w_email <- email_lookup(pairs,Mentees,Mentors)
print(pairs_w_email)
