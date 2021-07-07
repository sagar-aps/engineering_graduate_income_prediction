
install.packages('plotly')

library(dplyr)
library(ggplot2) #for graphics
library(stringr) #for data cleaning
library(tidyr)
library(caret) #for CreateDataPartition
library(ggthemes) #for ggplot theme economist
library(lubridate) #for dealing with dates
library(tidyverse)
library(plotly)


#-------------------------------------------------------------------      ---------------

min_coll <- 2
min_state <- 9


res <- data.frame()

for(min_coll in 5:25)
{
  
  # college_ID_99999 <- total_imputed_set[total_imputed_set$CollegeID %in% names(
  #   which(table(total_imputed_set$CollegeID)<min_coll)),"CollegeID"] 
  
  print(paste("min_coll = ",min_coll))
  
  for(min_state in 9:20)  
    
  {
    
    print(paste("min_state = ",min_state))
    
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
    
    state_other <-  as.character(names(
      which(table(lm_set$CollegeState)<min_state)))
    
    lm_set <- lm_set %>%
      mutate(
        CollegeState = as.character(CollegeState),
        CollegeState2 = replace_by(CollegeState,state_other),
        CollegeState2 = as.factor(CollegeState2)
      )%>%
      select(-CollegeState)
    

    
    set.seed(755, sample.kind = "Rounding")
    
    
    test_index <- createDataPartition(lm_set$CollegeID2, times = 1, p = 0.2, 
                                      list = FALSE)
    train_set <- lm_set[-test_index,]
    test_set <- lm_set[test_index,]
    nrow(test_set)/(nrow(train_set)+nrow(test_set))
    
  
      
      factor_names <- lm_set %>%
        select(-where(is.numeric))%>%
        colnames()
      
      
      removed <- test_set %>% 
        anti_join(train_set, by = factor_names)
      nrow(removed)
      test_set <- test_set %>% 
        semi_join(train_set, by = factor_names) 
      train_set <- train_set %>%
        rbind(removed)
      
      #factor_names=factor_names[-5]
      n=0
      
      while(nrow(test_set)/(nrow(train_set)+nrow(test_set))<0.2 & n<=100){    
      
      n=n+1  
      
      if(n%%10==0)
      {print(paste("Adjusting test set, test at",round((
        nrow(test_set)/(nrow(train_set)+nrow(test_set))
        )*100,digits = 2),"pc"))}
      
      set.seed(nrow(removed),sample.kind = "Rounding")
      
      readd_index <- sample(nrow(train_set),size = nrow(removed))
      test_set <- rbind(test_set, train_set[readd_index,])
      train_set<- train_set[-readd_index,]
      
      
      #if anti_join>1 then
      removed <- test_set %>% 
        anti_join(train_set, by = factor_names)
      nrow(removed)
      test_set <- test_set %>% 
        semi_join(train_set, by = factor_names) 
      train_set <- train_set %>%
        rbind(removed)
      #end if
      
      nrow(test_set)/(nrow(train_set)+nrow(test_set))
         #if(nrow(test_set)/(nrow(train_set)+nrow(test_set))>=0.2){break}
      
    }
    
    
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
    
    
    predicted_sal_lm = predict(lm_obj , test_set)
    
    res <- rbind(res,data.frame(MinColl= min_coll,MinState=min_state, Partition = nrow(test_set)/(nrow(train_set)+nrow(test_set)), RMSE=  RMSE(predicted_sal_lm,test_set$Salary)))
    
  }
}




res%>% view

res[which.min(res$RMSE),]

plot<-plot_ly(data = res,x=~MinColl,y=~MinState,z=~RMSE,color=~Partition,type="scatter3d",mode="markers")




