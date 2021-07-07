total_imputed_set <- imputed_data %>%
  cbind(CollegeID = cleaned_data$CollegeID)%>%
  cbind(CollegeState=data$CollegeState)%>%
  select(-TestsTaken)%>%
  select(-MaxScaledScore)%>%
  select(-Salary_Cat)



upper <- mean(data$Salary)+3*sd(data$Salary)
lower <- mean(data$Salary)-3*sd(data$Salary)

total_imputed_set <- total_imputed_set %>%
  filter(Salary<upper | Salary>lower)



total_imputed_set %>% group_by(CollegeID)%>%
  summarise(n=n())%>%
  arrange(-n)%>%
  view


total_imputed_set %>% group_by(CollegeState)%>%
  summarise(n=n())%>%
  arrange(-n)%>%
  view

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

median(data$GraduationYear)

total_imputed_set_2 %>% group_by(GraduationYear)%>%
  summarise(n=n())%>%
  arrange(-n)%>%
  view

results<- data.frame()
  
minCollegeID = seq(2,20,1)
results <- sapply(minCollegeID, function(min_coll){ 
  college_ID_99999 <- total_imputed_set[total_imputed_set$CollegeID %in% names(
  which(table(total_imputed_set$CollegeID)<min_coll)),"CollegeID"] 
  
  lm_set <- total_imputed_set_2 %>%
    mutate(
      CollegeID = as.character(CollegeID),
      CollegeID2 = replace_by(CollegeID,college_ID_99999,replaceby = "9999"),
      CollegeID2 = as.factor(CollegeID2)
    )%>%
    select(-CollegeID) 
  set.seed(755, sample.kind = "Rounding")
  
  factor_names <- lm_set %>%
    select(-where(is.numeric))%>%
    colnames()
  
  test_index <- createDataPartition(y = lm_set$CollegeID2, times = 1, p = 0.8, 
                                  list = FALSE)
  train_set <- lm_set[-test_index,]
  test_set <- lm_set[test_index,]
 
  # 
  # removed <- test_set %>% 
  #   anti_join(train_set, by = factor_names) 
  # test_set <- test_set %>% 
  #   semi_join(train_set, by = factor_names) 
  # train_set <- train_set %>%
  #   rbind(removed)
  lm_obj <- lm(Salary~. , data=train_set)
  predicted_sal_lm = predict.lm(lm_obj , test_set)
  row <- data.frame(MinColl= min_coll, Partition = nrow(test_set)/(nrow(train_set)+nrow(test_set)), RMSE=  RMSE(predicted_sal_lm,test_set$Salary))
  
  return(row)
  })

results
tresults<-as.data.frame(t(results))

tresults
which.min(tresults$RMSE)

#RMSE reduces until we make min college ID grouping <7. (We group CollegeID as 9999 if number of unique instances are 7)





















set.seed(755, sample.kind = "Rounding")

pc=seq(0.8,0.5,-0.1)


  #We use p=0.5 in the next line because we are going to remove a lot of rows with no matching factor values and wee want to end up with a ~20% representative test set.

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
  RMSE(predicted_sal_lm,test_set$Salary)/sd(data$Salary)





summary(lm_model)



total_imputed_set <- imputed_data %>%
  cbind(CollegeID = cleaned_data$CollegeID)%>%
  cbind(CollegeState=data$CollegeState)%>%
  select(-TestsTaken)%>%
  select(-MaxScaledScore)%>%
  select(-Salary_Cat)