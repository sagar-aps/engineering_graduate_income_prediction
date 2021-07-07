#Tune NN

lower_decay = 0.03
higher_decay = lower_decay + 0.02
mid_decay = lower_decay + 0.01

lsize = 12
msize = lsize +1
hsize = lsize +2


while (!((nn$bestTune$size!=msize) & (nn$bestTune$decay != mid_decay))){
  nngrid = expand.grid(.decay=seq(lower_decay,higher_decay,0.01), .size=c(lsize,msize,hsize))
  #besttune at size = 15 and decay =0.1
  #10:20
  
  # train_params <- trainControl(method = "repeatedcv",
  #                              number = 10,
  #                              repeats=5,
  #                              verboseIter = FALSE)
  
  
  print(kable(rbind(
    hsize,
    msize,
    lsize,
    higher_decay,
    mid_decay,
    lower_decay,
    nn$bestTune$size,
    nn$bestTune$decay),"rst"))
  
  
  
  set.seed(42)
  nn= train(Salary~., 
            method="nnet",
   #         trControl= train_params,
            data=train_set,
            preProcess = c('center', 'scale'),
            na.action = na.omit,
            linout=TRUE,
            tuneGrid=nngrid,
            trace = FALSE
  )
  
  if (nn$bestTune$decay <= lower_decay) {
    lower_decay = nn$bestTune$decay - 0.01
    higher_decay = lower_decay + 0.02
    mid_decay = lower_decay + 0.01
  }
  
  
  if (nn$bestTune$decay >= higher_decay) {
    
    higher_decay = nn$bestTune$decay + 0.01
    mid_decay = higher_decay - 0.01
    lower_decay = higher_decay - 0.02
    next
  }
  
  
  if (nn$bestTune$size == lsize) {
    
    lsize = lsize - 1
    msize = lsize +1
    hsize = lsize +2
    next
  }
  
  
  if (nn$bestTune$size == hsize) {
    
    hsize = hsize +1
    msize = hsize -1
    lsize = hsize -2
    next
  }

  gc()
}


hsize
msize
lsize
higher_decay
mid_decay
lower_decay
nn$bestTune


print(kable(rbind(
hsize,
msize,
lsize,
higher_decay,
mid_decay,
lower_decay,
nn$bestTune$size,
nn$bestTune$decay),"rst"))




nngrid = expand.grid(.decay=seq(0.02,0.04,0.005), .size=c(13,14,15))
#besttune at size = 15 and decay =0.1
#10:20

train_params <- trainControl(method = "repeatedcv", 
                             number = 10, 
                             repeats=5,
                             verboseIter = FALSE) 

set.seed(42)
nn= train(Salary~., 
          method="nnet",
          trControl= train_params,
          data=train_set,
          preProcess = c('center', 'scale'),
          na.action = na.omit,
          linout=TRUE,
          tuneGrid=nngrid
)

