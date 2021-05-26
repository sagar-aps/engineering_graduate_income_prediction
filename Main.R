#Step 0 : Load libraries

library(dplyr)
library(ggplot2) #for graphics
library(stringr) #for data cleaning
library(tidyr)
library(caret) #for CreateDataPartition
library(ggthemes) #for ggplot theme economist
library(lubridate) #for dealing with dates


#Step 1 :Load data from kaggle into R


data<-read.csv2("./Engineering_graduate_salary.csv",sep = ",")

#Step 2 : Examine data

glimpse(data)


# Step 3: reload data with correct datatypes

data<-read.csv2("./Engineering_graduate_salary.csv",
                  sep = ",",dec = ".",colClasses = c(DOB= "Date",
                                                    CollegeID="factor",
                                                    CollegeCityID="factor",
                                                    CollegeCityTier="factor")
                ,stringsAsFactors = TRUE)

glimpse(data)

#Number of years to graduate

data <- data %>% mutate(time_to_grad = GraduationYear - year(DOB),
                        time_to_X12 = X12graduation -year(DOB))


data %>% select(time_to_grad,time_to_X12) %>%
  gather()%>%
  ggplot() +
  geom_histogram(aes(value, fill=key)) +
  xlim(13,29)


dat <- data.frame(xx = c(runif(100,20,50),runif(100,40,80),runif(100,0,30)),yy = rep(letters[1:3],each = 100))
rm(dat)
#Boards


data %>% group_by(X10board) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  mutate(perc = n/sum(n)*100) %>%
  filter(X10board != "cbse" & X10board != "icse")

#central, cbse, dav , "delhi public school" , "jawahar navodaya vidyalaya ","all india board " , "cbsc"=> cbse
#"certificate", "icse", "cisce", "isc", "anglo" => icse

cbse_alt_names <- c("central","cbse","delhi public school", "jawahar navodaya vidyalaya ","all india board " ,"cbsc","aissce","aisse")
icse_alt_names <- c("certificate", "icse", "cisce", "isc", "anglo")

data %>% mutate(
  X10board = as.character(X10board),
  X12board = as.character(X12board),
  X10board=ifelse(str_detect(X10board,paste(cbse_alt_names,collapse = "|")),"cbse",X10board),
  X10board=ifelse(str_detect(X10board,paste(icse_alt_names,collapse = "|")),"icse",X10board),
  X12board=ifelse(str_detect(X12board,paste(cbse_alt_names,collapse = "|")),"cbse",X12board),
  X12board=ifelse(str_detect(X12board,paste(icse_alt_names,collapse = "|")),"icse",X12board),
  X10board=ifelse(str_detect(X10board,paste(c("cbse","icse","0"),collapse = "|")),X10board,"state board"),
  X12board=ifelse(str_detect(X12board,paste(c("cbse","icse","0"),collapse = "|")),X12board,"state board"),
  X10board=ifelse(str_detect(X10board,"0"),NA,X10board),
  X12board=ifelse(str_detect(X12board,"0"),NA,X12board),
  X10board = as.factor(X10board),
  X12board = as.factor(X12board)
  ) %>%
  group_by(X10board,X12board) %>%
  summarise(n=n())

#The above code is a bit cumbersome. We can simplify it slightly by writing the following function.

replace_by <- function(target, pattern, replaceby = deparse(substitute(pattern)), ifnotfound=target) {
  ifelse(str_detect(target, paste(pattern,collapse = "|")),replaceby,ifnotfound)
}

noncbseicse0 <- paste(c("cbse","icse","0"))

 data %>% mutate(
  X10board_orig= X10board, #make backup copy of variable
  X12board_orig = X12board,
  
  X10board = as.character(X10board), #convert to char to facilitate str_replace
  X12board = as.character(X12board),
  
  X10board= replace_by(X10board,cbse_alt_names,"cbse"), #replace cbse and icse synonyms
  X10board= replace_by(X10board,icse_alt_names,"icse"),
  X12board= replace_by(X12board,cbse_alt_names,"cbse"),
  X12board= replace_by(X12board,icse_alt_names,"icse"),
  
  X10board= replace_by(X10board, noncbseicse0, X10board ,"state board"), #replace non cbse & icse
  X12board= replace_by(X12board,noncbseicse0, X12board, "state board"), #and non 0 by state board
  
   X10board = as.factor(X10board), #reconvert to factor
   X12board = as.factor(X12board)) %>%
   group_by(X10board,X12board) %>%
    summarise(n=n()) %>%
   arrange(-n) 


temp<-data %>%
  group_by(Specialization) %>%
  summarise(n=n()) %>%
  arrange(n) 
#%>%
#  ggplot(aes(n))+
 # geom_histogram(bins = 10)
temp



deparse(substitute(mechanical))

mechanical <- c("industrial & management engineering","mechanical & production engineering",
                "industrial engineering","automobile/automotive engineering","mechanical and automation",
                "industrial & production engineering","mechanical engineering")
computer <-c("computer and communication engineering" ,"computer networking", 
             "computer science and technology","computer engineering",
              "computer science & engineering")
eintc <-c("electronics & instrumentation","control and instrumentation engineering","instrumentation engineering",
         "mechatronics","applied electronics and instrumentation","electronics","embedded systems technology" ,
         "electronics and computer engineering","telecommunication engineering","electronics engineering",
         "instrumentation and control engineering","electronics & instrumentation eng" ,"electronics and communication engineering",
         "electronics & telecommunications")
electrical <- c("electrical and power engineering","electrical engineering","electrical")
IT <- c("information & communication technology", "information science",	
                            "information science engineering", "computer application",
                            "information technology")
other <- c("ceramic engineering","biomedical engineering" ,"metallurgical engineering","aeronautical engineering","chemical engineering","other","biotechnology","electronics and electrical engineering")










data<-data %>% mutate(
  spec_orig = Specialization,
  Specialization = replace_by(Specialization,mechanical),
  Specialization = replace_by(Specialization,computer),
  Specialization = replace_by(Specialization,eintc),
  Specialization = replace_by(Specialization,electrical),
  Specialization = replace_by(Specialization,IT),
  Specialization = replace_by(Specialization,other))




data <- data %>% mutate(
  X10board_orig= X10board,
  X12board_orig = X12board,
  X10board=ifelse(str_detect(X10board,paste(cbse_alt_names,collapse = "|")),"cbse",X10board),
  X10board=ifelse(str_detect(X10board,paste(icse_alt_names,collapse = "|")),"icse",X10board),
  X12board=ifelse(str_detect(X12board,paste(cbse_alt_names,collapse = "|")),"cbse",X12board),
  X12board=ifelse(str_detect(X12board,paste(icse_alt_names,collapse = "|")),"icse",X12board),
  X10board=ifelse(str_detect(X10board,paste(c("cbse","icse","0"),collapse = "|")),X10board,"state board"),
  X12board=ifelse(str_detect(X12board,paste(c("cbse","icse","0"),collapse = "|")),X12board,"state board"))



#Many columns of our data are actually categorical data represented as numbers. We need to use MCA and not PCA on this data

glimpse(data)




data %>% group_by(CollegeCityTier) %>%
  summarise(n=n())

data %>% group_by(CollegeID) %>%
  summarise(n=n()) %>%
  arrange(-n)

#ID Exclude
#DOB exclude
#X10 perc as double
#X12 perc as double
#CollegeID as fact
#collegetier as fact


install.packages(c("FactoMineR", "factoextra"))
install.packages("missMDA")

library("FactoMineR")
library("factoextra")
library("missMDA")



FAMD()

#PCA MCA



# 
# 
# 
# 
 
# 
#



# Mixed Regression




# Descision Tree

# Random Forest






