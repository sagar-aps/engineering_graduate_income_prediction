


install.packages("rpart")
install.packages("rpart.plot")

library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)

modelLookup("rpart")


set.seed(42)
fit_dt <- rpart(Salary~.,
             data = train_set,
             method = "anova",
             control = rpart.control(xval = 10, 
                                     minbucket = 2, 
                                     cp = 0), 
             parms = list(split = "information"))

rpart.plot(fit_dt, extra = 100)

predicted_dt <- rpart.predict(fit_dt,
              newdata = test_set)

RMSE(predicted_dt,test_set$Salary)

#Random Forest
set.seed(42)
bestmtry <- tuneRF(x=train_set[,-24],
                   y=train_set[,24],
                   data = train_set
                   , stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)

mytuneGrid = expand.grid(mtry=seq(90,100,1))

model_rf <- train(Salary ~ .,
                         data = train_set,
                         method = "rf",
                         preProcess = c("scale", "center"),
                         trControl = trainControl(method = "repeatedcv", 
                                                  number = 5, 
                                                  repeats = 3, 
                                                  savePredictions = TRUE, 
                                                  verboseIter = FALSE),
                  tuneGrid=mytuneGrid)
importance <- varImp(model_rf, scale = TRUE)

model_rf$bestTune
#To do : mtry +- besttune

plot(importance,top = 20)

prediction_rf = predict(model_rf, newdata = test_set)
RMSE_rf <- RMSE(prediction_rf, test_set$Salary)


rbind(RMSE_glm,RMSE_lm,RMSE_rf)
kable(rbind(RMSE_glm,RMSE_lm,RMSE_rf))


print(kable(rbind(RMSE_glm,RMSE_lm,RMSE_rf),"rst"))



rmse_results <- rbind(RMSE_glm,RMSE_lm,RMSE_rf)
kable(rmse_results) %>%
kable_styling(bootstrap_options = "striped" , full_width = F , position = "center") %>%
kable_styling(bootstrap_options = "bordered", full_width = F , position ="center") %>%
column_spec(1,bold = T ) %>%
column_spec(2,bold =T ,color = "white" , background ="#D7261E")



rmse_results <- rbind(RMSE_glm,RMSE_lm,RMSE_rf) %>%
  `colnames<-` ("RMSE") %>%
  as.data.frame() %>%
  `rownames<-`(sub("RMSE_","",rownames(rmse_results)))


rmse_results <- do.call(rbind, lapply( ls(pattern = 'RMSE*'), get) ) %>%
  as.data.frame() %>%
  `colnames<-` ("RMSE") %>%
  `rownames<-`(sub("RMSE_","",rownames(rmse_results)))


  
rmse_results

#str_sub(rownames(rmse_results),start = length(rmse_results)-5)

sub("RMSE_","",rownames(rmse_results))    

rmse_results


sapply(cleaned_NA_treated_data,  function(x) sum(is.na(x))) %>%
  as.data.frame() %>%
   `colnames<-` ("Nas") %>%
  filter(Nas>0) %>%
  mutate(percent = paste(round(Nas/nrow(cleaned_data)*100,1),"%"))%>%
  print.data.frame()




XX<- sapply(cleaned_data,  function(x) sum(is.na(x))) %>%
  as.data.frame()


# XX %>%
#   `colnames<-` ("Nas") %>%
  
