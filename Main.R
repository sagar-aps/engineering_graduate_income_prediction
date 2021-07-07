#Step 0 : Load libraries

library(dplyr)
library(ggplot2) #for graphics
library(stringr) #for data cleaning
library(tidyr)
library(caret) #for CreateDataPartition
library(ggthemes) #for ggplot theme economist
library(lubridate) #for dealing with dates
library(tidyverse)
library("FactoMineR")
library("factoextra")
library("missMDA")
library(reshape2)
library(neuralnet)
library(nnet)

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
                        time_to_X12 = X12graduation -year(DOB))%>%
                



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

#from the above result, we can see that the majority of values in these columns are NAs.

#We can't exlude rows with NA as each row has at lresultseast one NA.

#We can replace the NA by encoding it with a -1 but this will have unpredictable results.

#We can average the scores on each test taken but the tests could have had varying difficulty 

#To account for this, we could scale each of the variables and take the average of each non NA score

#We can also add a variable to show the number of tests taken

#We don't lose too much because we already have the specialization name from another field.





data %>% pull(MechanicalEngg)  %>% scale()

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


#State vs Salary
data %>% 
  ggplot(aes(CollegeState, Salary)) +
  geom_boxplot()+
  ylim(0,1000000)+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_abline(slope = 0, intercept = 600*80*12, alpha=0.2) 


hist(data$Salary,breaks = 30)
  


# NA treatment

#Are all tests equally difficult ?
cleaned_data %>% select(ComputerProgramming:CivilEngg) %>%
  gather("Discipline","Score")%>%
  na.omit()%>%
  ggplot(aes(Discipline,Score)) +
  geom_boxplot()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# install.packages("purrrlyr")
# library(purrrlyr)
#testing if scale worked

backup_cleaned_data<- cleaned_data 

claned_NA_treated_data <- cleaned_data %>% select(ComputerProgramming:CivilEngg) %>%
  mutate_all(funs(scale)) %>%
 mutate(MaxScaledScore = apply(X=., MARGIN=1, 
                               FUN = function(this_row)
                                 {max(this_row,na.rm = TRUE)}
                               )
        ) %>%
  mutate(MeanScale = rowMeans(.,na.rm = TRUE),
         TestsTaken = rowSums(!is.na(.))-1)%>%
  select(MaxScaledScore:TestsTaken)%>%
   cbind(cleaned_data)  
 # relocate(c("tests_taken","MaxScaledScore"), .after = Domain)%>%
 # relocate(ComputerProgramming:CivilEngg, .after = Salary)
#%>%select(Gender:Salary)

rm(claned_NA_treated_data)

cleaned_NA_treated_data<- cleaned_data %>% select(ComputerProgramming:CivilEngg) %>%
  mutate_all(funs(scale)) %>%
  mutate(MaxScaledScore = apply(X=., MARGIN=1,FUN = max, na.rm=TRUE)) %>%
  mutate(MeanScale = rowMeans(.,na.rm = TRUE),
         TestsTaken = rowSums(!is.na(.))-1)%>%
          select(MaxScaledScore:TestsTaken) %>%
  mutate_all(.,.funs = function(x){ifelse(is.infinite(x),NA,x)})%>%
  cbind(cleaned_data %>% select(-c(ComputerProgramming:CivilEngg))) %>%
  relocate(1:3, .after = Domain)%>%
  select(-DOB)%>%
  select(-CollegeID)%>%
  select(-X12graduation) %>%
  mutate(GraduationYear=as.factor(GraduationYear))%>%
  #        X12graduation= as.factor(X12graduation)) %>%
  select(-CollegeCityID)%>%
  select(-CollegeState)%>%
  mutate(Salary_Cat = cut(Salary,c(seq(0,1000000,200000),10000000)))
  

c(seq(0,200,50),10000000)

#Cleaned_Imputed_Data



sapply(cleaned_NA_treated_data, function(x) sum(is.na(x))) %>%
  as.data.frame() %>%
  filter(.>0)

removed_outliers<- removed_outliers%>%
  select(-DOB)

#PCA MCA


#res.ncp <- estim_ncpFAMD(cleaned_NA_treated_data)

cl_NA_tr_imputed <- imputeFAMD(cleaned_NA_treated_data, ncp = 5)
dat.famd <- FAMD(cleaned_NA_treated_data, tab.disj = cl_NA_tr_imputed$tab.disj, ncp = 100)

temp<-data%>%
  group_by(CollegeState) %>%
  summarize(n=n())

summary(dat.famd)

get_eigenvalue(dat.famd)

temp<-dimdesc(dat.famd)

dimdesc(dat.famd)

#dimdesc(dat.famd, proba = 0.2)

temp$Dim.1

fviz_famd_var(dat.famd)

get_famd(dat.famd)$cos2

get_famd(dat.famd)$contrib

facto_summarize(dat.famd, element = "var") %>%
  arrange(-contrib)

fviz_screeplot(dat.famd,
               ncp=30,
               addlabels=TRUE
               )



fviz_famd_ind(dat.famd,
              label = "none,",
              habillage = cleaned_NA_treated_data$Salary_Cat,
              addEllipses = TRUE,
              alpha.ind = 0.2
              )





imputed_data<-cl_NA_tr_imputed$completeObs





#PCA after removing Outliers

removed_outliers%>% ggplot(aes(Salary_Cat))+
  geom_histogram(stat="count")

quantile(cleaned_NA_treated_data$Salary, 0.95)

removed_outliers <- removed_outliers%>%
  filter(Salary < quantile(Salary,0.95))

cl_NA_tr_imputed_out <- imputeFAMD(removed_outliers, ncp = 5)
dat.rm.out.famd <- FAMD(removed_outliers, tab.disj = cl_NA_tr_imputed_out$tab.disj, ncp = 100)

temp<-data%>%
  group_by(CollegeState) %>%
  summarize(n=n())

summary(dat.rm.out.famd)

get_eigenvalue(dat.rm.out.famd)

temp<-dimdesc(dat.rm.out.famd)

dimdesc(dat.rm.out.famd)

#dimdesc(dat.rm.out.famd, proba = 0.2)

temp$Dim.1

fviz_famd_var(dat.rm.out.famd)

get_famd(dat.rm.out.famd)$cos2

get_famd(dat.rm.out.famd)$contrib

facto_summarize(dat.rm.out.famd, element = "var") %>%
  arrange(-contrib)

fviz_screeplot(dat.rm.out.famd,
               ncp=30,
               addlabels=TRUE
)



fviz_famd_ind(dat.rm.out.famd,
              label = "none,",
              habillage = cleaned_NA_treated_data$Salary_Cat,
              addEllipses = TRUE,
              alpha.ind = 0.2
)


predict.FAMD()

#Quantitative variables : X10perc, X12perc, 

cor(cleaned_data$English,cleaned_data$Salary)
cor(cleaned_data$Quant,cleaned_data$Salary)
cor(cleaned_data$Logical,cleaned_data$Salary)

# 
# install.packages(c("FactoMineR", "factoextra"))
# install.packages("missMDA")


#res.ncp <- estim_ncpFAMD(cleaned_data)
#Takes too much time





full_impute <- imputeFAMD(cleaned_data , ncp = 2)

dat.famd <- FAMD(cleaned_data, tab.disj = cleaned_imputed$tab.disj)



#plot.PCA(dat.famd, axes=c(1, 2), choix="var")


FAMD()





# Correlation Matrix

cormat<-imputed_data %>%
  select(where(is.numeric))%>%
  cor()%>%
  round(digits = 2)

head(cormat)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
  
upper_tri <- get_lower_tri(cormat)

upper_tri%>%
  melt(na.rm = TRUE) %>%
  ggplot(aes(Var1,Var2, fill=value))+
  geom_tile()+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
  

cormat <- round(cor(imputed_data),2)
head(cormat)



#Training Set generation

total_imputed_set %>% group_by(CollegeID)%>%
  summarise(n=n()) %>% 
  arrange(-n)%>%
  ggplot(aes(n))+
  geom_histogram(bins=30)+
  xlim(0,30)

subset(total_imputed_set, ave(CollegeID,CollegeID, FUN=length)>10,)

CollegeID = names(table(total_imputed_set$CollegeID)>10)

names(which(table(total_imputed_set$CollegeID)>10))
  
total_imputed_set[total_imputed_set$CollegeID %in% names(which(table(total_imputed_set$CollegeID)>10)),] %>%
  nrow()


table(data$CollegeState)

table(data$Salary)

upper <- mean(data$Salary)+3*sd(data$Salary)
lower <- mean(data$Salary)-3*sd(data$Salary)

data%>% 
  filter(Salary>upper | Salary<lower)%>%
  nrow()

total_imputed_set <- imputed_data %>%
  cbind(CollegeID=cleaned_data$CollegeID)%>%
  select(-TestsTaken)%>%
  select(-MaxScaledScore)

glimpse(total_imputed_set)

set.seed(755, sample.kind = "Rounding")

test_index <- createDataPartition(y = total_imputed_set$Salary, times = 1, p = 0.3, 
                                  list = FALSE)
train_set <- total_imputed_set[-test_index,]
test_set <- total_imputed_set[test_index,]

factor_names <- total_imputed_set %>%
  select(-where(is.numeric))%>%
  colnames()

factor_names

removed <- test_set %>% 
  anti_join(train_set, by = factor_names) 

test_set <- test_set %>% 
  semi_join(train_set, by = factor_names) 

train_set <- train_set %>%
  rbind(removed)

nrow(test_set)+nrow(train_set)

train_set %>%
  filter(`cleaned_data$CollegeID`=="10209")

test_set %>%
  filter(`cleaned_data$CollegeID`=="10209")


# Descision Tree

# Random Forest

#Pre-Final LM
total_imputed_set_lm <- total_imputed_set %>%  
  select(-CollegeID)

set.seed(755, sample.kind = "Rounding")

#We use p=0.5 in the next line because we are going to remove a lot of rows with no matching factor values and wee want to end up with a ~20% representative test set.
test_index <- createDataPartition(y = total_imputed_set_lm$Salary, times = 1, p = 0.5, 
                                  list = FALSE)

train_set <- total_imputed_set_lm[-test_index,]
test_set <- total_imputed_set_lm[test_index,]

factor_names <- total_imputed_set_lm %>%
  select(-where(is.numeric))%>%
  colnames()

removed <- test_set %>% 
  anti_join(train_set, by = factor_names) 

test_set <- test_set %>% 
  semi_join(train_set, by = factor_names) 

train_set <- train_set %>%
  rbind(removed)

nrow(test_set)/nrow(total_imputed_set_lm)


lm_obj <- lm(Salary~. , data=train_set)

predicted_sal_lm = predict.lm(lm_obj , test_set)
predicted_sal_lm

RMSE(predicted_sal_lm,test_set$Salary)/sd(data$Salary)


summary(lm_model)



total_imputed_set <- imputed_data %>%
  cbind(CollegeID = cleaned_data$CollegeID)%>%
  cbind(CollegeState=data$CollegeState)%>%
  select(-TestsTaken)%>%
  select(-MaxScaledScore)%>%
  select(-Salary_Cat)

#GLM

glm_model <- glm(Salary~., train_set, family = "inverse.gaussian")

predicted_sal_glm <- predict.glm(glm_model, test_set)

#summary(glm_model)

RMSE_glm <- RMSE(predicted_sal_glm,test_set$Salary)

RMSE_glm 



#Redo Liner Regression with tuning parameter as number of instances of factor levels
#loop in differnt file
#Fully Final LM



min_coll = 3
min_state =9

res <- data.frame()

for(min_coll in 2:25)
{
  
# college_ID_99999 <- total_imputed_set[total_imputed_set$CollegeID %in% names(
#   which(table(total_imputed_set$CollegeID)<min_coll)),"CollegeID"] 

    for(min_state in 9:20)  
              
          {  
              college_ID_99999 <-  as.character(names(
              which(table(total_imputed_set$CollegeID)<min_coll)))
            
            NorthEasternStates <- c("Sikkim","Meghalaya","Assam")
            UnionTerritory <- c("Union Territory","Jammu and Kashmir")
            
            total_imputed_set_2 <- total_imputed_set %>%
              mutate(CollegeState = as.character(CollegeState),
                     CollegeState = replace_by(CollegeState,c("Maharashtra","Goa"),"Maharashtra & Goa"),
                     CollegeState = replace_by(CollegeState,NorthEasternStates),
                     CollegeState = replace_by(CollegeState,UnionTerritory),
                     CollegeState = as.factor(CollegeState),
                     GraduationYear = as.character(GraduationYear),
                     GraduationYear = replace_by(GraduationYear,"GraduationYear_0","GraduationYear_2013"),
                     GraduationYear = as.factor(GraduationYear)
              )
            
            
            lm_set <- total_imputed_set_2 %>%
              mutate(
                CollegeID = as.character(CollegeID),
                CollegeID2 = replace_by(CollegeID,college_ID_99999,replaceby = "9999"),
                CollegeID2 = as.factor(CollegeID2),
                Degree = as.character(Degree),
                Degree = ifelse(Degree=="M.Sc. (Tech.)","M.Tech./M.E.",Degree),
                Degree = as.factor(Degree)
              )%>%
              select(-CollegeID) 
            
            # lm_set %>% group_by(CollegeState)%>%
            #   summarize(n=n())%>%
            #   arrange(-n)%>%
            #   view()
            
            state_other <-  as.character(names(
              which(table(lm_set$CollegeState)<min_state)))
             
            lm_set <- lm_set %>%
              mutate(
                CollegeState = as.character(CollegeState),
                CollegeState2 = replace_by(CollegeState,state_other),
                CollegeState2 = as.factor(CollegeState2)
              )%>%
              select(-CollegeState) 
            
          # lm_set %>% group_by(Degree)%>%
            #   summarize(n=n())%>%
            #   arrange(-n)%>%
            #   view()
            
            
            # lm_set %>% group_by(CollegeState2)%>%
            #    summarize(n=n())%>%
            #    arrange(-n)%>%
            #      view()
              
            set.seed(755, sample.kind = "Rounding")
            
            test_index <- createDataPartition(lm_set$Salary, times = 1, p = 0.3, 
                                              list = FALSE)
            train_set <- lm_set[-test_index,]
            test_set <- lm_set[test_index,]
            nrow(test_set)/(nrow(train_set)+nrow(test_set))
            
            factor_names <- lm_set %>%
              select(-where(is.numeric))%>%
              colnames()
            
            removed <- test_set %>% 
              anti_join(train_set, by = factor_names) 
            test_set <- test_set %>% 
              semi_join(train_set, by = factor_names) 
            train_set <- train_set %>%
              rbind(removed)
            nrow(test_set)/(nrow(train_set)+nrow(test_set))
            
            lm_obj <- train(Salary~. , 
                            data=train_set,
                            method="lm",
                            trControl = trainControl(method = "repeatedcv", 
                                                     number = 10, 
                                                     repeats = 10, 
                                                     allowParallel = TRUE,
                                                     savePredictions = TRUE, 
                                                     verboseIter = FALSE)
            )
            predicted_sal_lm = predict.lm(lm_obj , test_set)
            RMSE_lm=  RMSE(predicted_sal_lm,test_set$Salary)
            RMSE_lm
            res <- rbind(res,data.frame(MinColl= min_coll,MinState=min_state, Partition = nrow(test_set)/(nrow(train_set)+nrow(test_set)), RMSE=  RMSE(predicted_sal_lm,test_set$Salary)))
      }
}

res%>%
  view

which.min(res$RMSE)

rm(row)



# Neural Network
#install.packages("nnet")
#install.packages("neuralnet")


#sd(data$Salary)

#dummyVars()

train_nn<- train_set %>%
  select(-CollegeID2)

test_nn<- test_set %>%
  select(-CollegeID2)

#nn=nnet(Salary~.,data=normalise(train_nn), size=14, linout=TRUE)

mygrid = expand.grid(.decay=seq(0.01,0.1,0.01), .size=c(10:20))

train_params <- trainControl(method = "repeatedcv", number = 10, repeats=5) #repeatedcv

nn= train(Salary~., 
          method="nnet",
          trControl= train_params,
          data=train_nn,
          preProcess = c('center', 'scale'),
          na.action = na.omit,
          linout=TRUE,
          tuneGrid=mygrid
)

nn$bestTune
#Best Tune acheived within search range.

hist(nnet_pred_train)
hist(train_nn$Salary,breaks = 500,xlim=c(0,450000))

nnet_pred_train <- predict(nn, train_nn)
table(nnet_pred_train)

nrow(train_nn) + nrow(test_nn)

RMSE(nnet_pred_train,train_nn$Salary)


table(nnet_pred_train)

predicted_salaries = predict(nn, newdata=test_nn )

# train_nn %>% group_by(Degree) %>%
#   summarise(n=n())%>%
#   arrange(-n)%>%
#   view
# test_nn %>% group_by(Degree) %>%
#   summarise(n=n())%>%
#   arrange(-n)%>%
#   view


RMSE_nn<-RMSE(predicted_salaries,test_nn$Salary)




#XG Boost
grid_default <- expand.grid(
  nrounds = c(75,100),
  max_depth = 6,
  eta = c(0.1,0.2,0.3),
  gamma = 0,
  colsample_bytree = seq(0.5, 0.9, length.out = 5),
  min_child_weight = 1,
  subsample = 1
)

library(xgboost)


set.seed(42)
model_xgb <- train(Salary ~ .,
                          data = train_set,
                          method = "xgbTree",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv", 
                                                   number = 5, 
                                                   repeats = 3, 
                                                   allowParallel = TRUE,
                                                   savePredictions = TRUE, 
                                                   verboseIter = FALSE),
                   tuneGrid = grid_default)


prediction_xgb = predict(model_xgb, newdata = test_set)

RMSE(prediction_xgb, test_set$Salary)

model_xgb$bestTune

RMSE_xgb <- RMSE(prediction_xgb, test_set$Salary)


table(predicted_salaries)
mean(test_nn$Salary)




#State testing

total_imputed_set %>%
  group_by(CollegeState)%>%
  summarize(n=n())%>%
  arrange(-n)%>%
  slice_tail(n=9)%>%
  pull(n)%>%
  sum


total_imputed_set %>%
  group_by(CollegeState)%>%
  summarize(n=n())%>%
  arrange(-n)%>%
  view


lm_set %>%
  group_by(CollegeID2)%>%
  summarize(n=n())%>%
  arrange(-n)%>%
  pull(n)%>%
  sum
  
  
  slice_tail(n=9)%>%
  pull(n)%>%
  sum
  
  library(Matrix)
  cat(rankMatrix(as.matrix(train_set)), "\n") 

  
  getModelInfo("lm", regex = TRUE)[[1]]$param
  
  modelLookup("lm")
  
  
  res%>% view
  res[which.max(res$RMSE),]
      
  
  trainingDataSetSMOTEpred()

  
  for(attr in colnames(train))
  {
    if (is.factor(train[[attr]]))
    {
      new.levels <- setdiff(levels(train[[attr]]), levels(test[[attr]]))
      if ( length(new.levels) == 0 )
      { print(paste(attr, '- no new levels')) }
      else
      {
        print(c(paste(attr, length(new.levels), 'of new levels, e.g.'), head(new.levels, 2)))
        levels(test[[attr]]) <- union(levels(test[[attr]]), levels(train[[attr]]))
      }
    }
  }
   
  
  
  for(attr in colnames(train))
  {
    if (is.factor(train[[attr]]))
    {
      new.levels <- setdiff(levels(train[[attr]]), levels(test[[attr]]))
      if ( length(new.levels) == 0 )
      { print(paste(attr, '- no new levels')) }
      else
      {
        print(c(paste(attr, length(new.levels), 'of new levels, e.g.'), head(new.levels, 2)))
        levels(test[[attr]]) <- union(levels(test[[attr]]), levels(train[[attr]]))
      }
    }
  } 
  
  
  
  lm_set %>%
    group_by(GraduationYear)%>%
    summarise(n=n())
  
  
  model_xgb$bestTune
  
  
  
  dimres <- dimdesc(dat.famd, axes = 1, proba = 0.005)
  
  head(dimres$call$X, n=10)
  
  head(dimres$Dim.1$quanti, n=10)
  head(dimres$Dim.1$quali, n=10)
  head(dimres$Dim.1$category, n=10)
  
  
  btxgb<-model_xgb$bestTune
  
  
  fviz_screeplot(dat.famd)
  