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
                                                    CollegeTier="factor",
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


# dat <- data.frame(xx = c(runif(100,20,50),runif(100,40,80),runif(100,0,30)),yy = rep(letters[1:3],each = 100))
# rm(dat)
#Boards


data %>% group_by(X10board,X12board) %>%
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
  
  X10board= replace_by(X10board, "0",NA), # Convert 0 values to NA
  X12board= replace_by(X12board, "0",NA),
  
   X10board = as.factor(X10board), #reconvert to factor
   X12board = as.factor(X12board)) %>%
   group_by(X10board,X12board) %>%
    summarise(n=n()) %>%
   arrange(-n) 


temp<-data %>%
  group_by(Specialization) %>%
  summarise(n=n()) %>%
  arrange(n) 


data %>%
  group_by(Specialization) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  filter(n<150) %>%
  pull(n) %>%
  sum()



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
biotech <- c("biomedical engineering" ,"biotechnology")
other <- c("ceramic engineering","metallurgical engineering","aeronautical engineering","chemical engineering","other","electronics and electrical engineering")


#check mutation spec

 data %>% mutate(
  
  Specialization = as.character(Specialization),
  spec_orig = Specialization,
  
  Specialization = replace_by(Specialization,mechanical),
  Specialization = replace_by(Specialization,computer),
  Specialization = replace_by(Specialization,eintc),
  Specialization = replace_by(Specialization,electrical),
  Specialization = replace_by(Specialization,IT),
  Specialization = replace_by(Specialization,biotech),
  Specialization = replace_by(Specialization,other),
  Specialization = as.factor(Specialization)) %>%
  group_by(Specialization, spec_orig) %>%
  summarise(n=n()) %>%
  arrange(Specialization) 


spec %>% ggplot(aes(Specialization)) +
  stat_count()


#final mutate Specialization

data<-data %>% mutate(
  
  Specialization = as.character(Specialization), #convert to char for string functions
  spec_orig = Specialization,   #backup of original
  
  
  Specialization = replace_by(Specialization,mechanical),
  Specialization = replace_by(Specialization,computer),
  Specialization = replace_by(Specialization,eintc),
  Specialization = replace_by(Specialization,electrical),
  Specialization = replace_by(Specialization,IT),
  Specialization = replace_by(Specialization,biotech),
  Specialization = replace_by(Specialization,other),
  
  Specialization = as.factor(Specialization) #reconvert to factor
  )


#final mutate Moards

data <- data %>% mutate(
  X10board_orig= X10board, #Save original variables
  X12board_orig = X12board,
  
  X10board = as.character(X10board), #convert to char to facilitate string functions
  X12board = as.character(X12board),
  
  X10board= replace_by(X10board,cbse_alt_names,"cbse"), #replace cbse and icse synonyms
  X10board= replace_by(X10board,icse_alt_names,"icse"),
  X12board= replace_by(X12board,cbse_alt_names,"cbse"),
  X12board= replace_by(X12board,icse_alt_names,"icse"),
  
  X10board= replace_by(X10board, noncbseicse0, X10board ,"state board"), #replace non cbse & icse
  X12board= replace_by(X12board,noncbseicse0, X12board, "state board"), #and non 0 by state board
  
  X10board= replace_by(X10board, "0",NA), # Convert 0 values to NA
  X12board= replace_by(X12board, "0",NA),
  
  X10board = as.factor(X10board), #reconvert to factor
  X12board = as.factor(X12board))
  

  
  # X10board=ifelse(str_detect(X10board,paste(cbse_alt_names,collapse = "|")),"cbse",X10board),
  # X10board=ifelse(str_detect(X10board,paste(icse_alt_names,collapse = "|")),"icse",X10board),
  # X12board=ifelse(str_detect(X12board,paste(cbse_alt_names,collapse = "|")),"cbse",X12board),
  # X12board=ifelse(str_detect(X12board,paste(icse_alt_names,collapse = "|")),"icse",X12board),
  # X10board=ifelse(str_detect(X10board,paste(c("cbse","icse","0"),collapse = "|")),X10board,"state board"),
  # X12board=ifelse(str_detect(X12board,paste(c("cbse","icse","0"),collapse = "|")),X12board,"state board"))












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



#count.na by column


sapply(data, function(x) sum(is.na(x))) %>%
  as.data.frame() %>%
  filter(.>0)

#reorder columns

data %>% 
  # dplyr::relocate(disp) %>% ## simply make disp the first column
  relocate(starts_with("time"), .after = CollegeState)  


data<- data %>% 
  # dplyr::relocate(disp) %>% ## simply make disp the first column
  relocate(c("ID","DOB"), .after = Salary)  


replace_minus_one <- function(x){
  ifelse(x==-1,NA,x)
}

data <- data%>%
  mutate_at(.vars = vars(ComputerProgramming:CivilEngg),replace_minus_one)

cleaned_data <- data %>% select(Gender:Salary)


countexams <- function(x){
  7 - sum(is.na(x))
}

data %>% select(ComputerProgramming:CivilEngg) %>%
  apply(MARGIN=1, countexams) %>%
  hist()


##Dimensions

#gender ~ Salary

data %>% 
  select(Gender,Salary) %>%
  group_by(Gender) %>%
  summarise(n = n(), avg = mean(Salary), se = sd(Salary)/sqrt(n)) %>%
  ggplot(aes(x = Gender, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


data %>% 
  select(Gender,Salary) %>%
  ggplot(aes(Gender,Salary)) +
  geom_boxplot()+
  ylim(0,1000000)
  
#It doesn't appear that girls get paid less than boys at median. The spread 
# is highter for boys though and there is more variation in the income
# compared to girls.



#Board ~ Salary



data %>% 
  select(X10board,X12board, Salary) %>%
  mutate(
    Board_Concat = paste(X10board,"-",X12board)
  ) %>%
  #select(Board_Concat,Salary) %>%
  #filter(is.na(Non_State_Board)==FALSE ) %>%
  ggplot(aes(Board_Concat,Salary)) +
  geom_boxplot()+
  ylim(0,1000000)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_abline(slope = 0, intercept = 500*80*12, alpha=0.2) 

#Examining concatednated boards, we percieve ICSE and CBSE students to 
#have higher salaries. We can distill the result into exposure to any of 
#these boards.

# icse/cbse (Exposure to central board)  vs salary

data %>% 
  select(X10board,X12board, Salary) %>%
  mutate(
    Non_State_Board = ifelse(X10board=="icse" | X12board =="icse","ICSE",
                              ifelse(X10board =="cbse" | X12board =="cbse","CBSE",
                              ifelse(is.na (X10board) & is.na(X12board), NA ,"State")
                              ))
  ) %>%
  select(Non_State_Board,Salary) %>%
  filter(is.na(Non_State_Board)==FALSE ) %>%
  ggplot(aes(Non_State_Board,Salary)) +
  geom_boxplot()+
  ylim(0,1000000)

#The median income for students with exposure to central boards is higher

  
  
data %>% mutate(
  Non_State_Board = ifelse(X10board=="icse" | X12board =="icse","ICSE",
                           ifelse(X10board =="cbse" | X12board =="cbse","CBSE",
                                  ifelse(is.na (X10board) & is.na(X12board), NA ,"State")
                           ))
) %>% ggplot(aes(shape=Non_State_Board))+
  geom_point(aes(X10percentage,Salary),color='blue', alpha=0.3)+
  geom_point(aes(X12percentage,Salary),color='red', alpha=0.3)+
  xlab("X10 in blue, X12 in red")+
  ylim(0,1e+06)

#Above plot is of X10 and X12 scores against Salary. Since the right bottom
#is the most heavily populated region, we can infer that high marks in 
#10th or 12th isn't a great indicator of the first Salary



#Discipline vs Salary
require(scales)

data %>% 
  select(Specialization, Salary) %>%
  ggplot(aes(Specialization,Salary)) +
  geom_boxplot()+
  ylim(0,1000000)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_abline(slope = 0, intercept = 600*80*12, alpha=0.2) 


#Quantitative variables : X10perc, X12perc, 

cor(cleaned_data$English,cleaned_data$Salary)
cor(cleaned_data$Quant,cleaned_data$Salary)
cor(cleaned_data$Logical,cleaned_data$Salary)


install.packages(c("FactoMineR", "factoextra"))
install.packages("missMDA")

library("FactoMineR")
library("factoextra")
library("missMDA")

res.ncp <- estim_ncpFAMD(cleaned_data)

cleaned_imputed <- imputeFAMD(cleaned_data , ncp = 2)

dat.famd <- FAMD(cleaned_data, tab.disj = cleaned_imputed$tab.disj)



#plot.PCA(dat.famd, axes=c(1, 2), choix="var")


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






