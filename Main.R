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

#Number of years to graduate

data <- data %>% mutate(time_to_grad = GraduationYear - year(DOB))

data %>% ggplot(aes(time_to_grad)) +
  geom_histogram() +
  xlim(17,30)

#Boards


data %>% group_by(X10board) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  mutate(perc = n/sum(n)*100) %>%
  filter(X10board != "cbse")

#central, cbse, dav , "delhi public school" , "jawahar navodaya vidyalaya ","all india board " , "cbsc"=> cbse
#certificate, icse, cisce, isc, anglo => icse















