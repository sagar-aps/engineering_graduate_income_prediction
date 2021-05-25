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
#"certificate", "icse", "cisce", "isc", "anglo" => icse

cbse_alt_names <- c("central","cbse","delhi public school", "jawahar navodaya vidyalaya ","all india board " ,"cbsc")
icse_alt_names <- c("certificate", "icse", "cisce", "isc", "anglo")

data %>% mutate(
  X10board=ifelse(str_detect(X10board,paste(cbse_alt_names,collapse = "|")),"cbse",X10board),
  X10board=ifelse(str_detect(X10board,paste(icse_alt_names,collapse = "|")),"icse",X10board),
  X12board=ifelse(str_detect(X12board,paste(cbse_alt_names,collapse = "|")),"cbse",X12board),
  X12board=ifelse(str_detect(X12board,paste(icse_alt_names,collapse = "|")),"icse",X12board),
  X10board=ifelse(str_detect(X10board,paste(c("cbse","icse"),collapse = "|")),X10board,"state board"),
  X12board=ifelse(str_detect(X12board,paste(c("cbse","icse"),collapse = "|")),X12board,"state board")) %>%
  group_by(X10board,X12board) %>%
  summarise(n=n())








temp<-data %>%
  group_by(Specialization) %>%
  summarise(n=n()) %>%
  arrange(n) 
#%>%
#  ggplot(aes(n))+
 # geom_histogram(bins = 10)
temp

replace_by <- function(target, pattern, replaceby = deparse(substitute(pattern)), ifnotfound=target) {
  ifelse(str_detect(target, paste(pattern,collapse = "|")),replaceby,ifnotfound)
}

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




temp2<-data %>% mutate(
  spec2 = Specialization,
  spec2 = replace_by(spec2,mechanical),
  spec2 = replace_by(spec2,computer),
  spec2 = replace_by(spec2,eintc),
  spec2 = replace_by(spec2,electrical),
  spec2 = replace_by(spec2,IT),
  spec2 = replace_by(spec2,other))%>% 
  select(Specialization,spec2) %>%
  group_by(spec2,Specialization)%>%
  summarise(n=n()) %>%
  arrange(Specialization)



# 
# 
# 
# 
 
# 
# civil engineering




