# Author: Sokratis Koseoglou

###### !!!!!! ########

# First run the SourceCode.R (kNN-SVM combined confidence-based model)
# and then run this code (Model.R)

# This code has 2 parts. 
# The first part evaluates how many swipes the attacker can do
# The second part evaluates if the device locks when the user operates it. If it locks, in how many swipes does this happen.

# press Ctrl+A and then Ctrl+Enter (for each part)

###### !!!!!! ########

neighbors = 3
gamma = 0.1
nu = 0.005

######################## HERE WE CHECK HOW MANY SWIPES THE ATTACKER DOES BEFORE THE DEVICE LOCKS #######################################################

# this is the number of tests we want to run in order to check the mean swipes that happened before lock

# IF WE PUT A BIG NUMBER OF ITERATIONS, IT WILL TAKE SOME TIME !!!
numberOfIterations = 10000

# this table keeps the number of swipes that happened before the device locked, in every iteration
AcceptedSwipes = c(1:numberOfIterations)

for(j in 1:numberOfIterations){

  # starting confidence level
  confidenceLevel = 100
  
  # threshold level for the device to close
  thresholdLevel = 30
  
  # sample a random swipe from the attacker data
  index <- sample(1:nrow(xattack), size = 1)
  swipeToBeClassified = swipes_Series[index,]
  
  # this while loop will run until the device locks
  flag = 0
  swipes = 0
  while(flag == 0){
    
    predKNN = nn2(xtrain, query = swipeToBeClassified, k = neighbors, treetype = c("kd","bd"), searchtype = c("radius"), radius = 10000)
    predKNN_mean = mean(predKNN$nn.dists)
    COP_kNN = limit - predKNN_mean
    
    predSVM = predict(svm_model, swipeToBeClassified, decision.value = T)
    predSVM_Distances = 100*attr(predSVM, "decision.values")
    COP_SVM = predSVM_Distances
    
    sumOfPredictions = COP_kNN + COP_SVM
    
    confidenceLevel = confidenceLevel + sumOfPredictions
    if(confidenceLevel < thresholdLevel){
      # cat("Device Locked")
      flag = 1
    }else if(confidenceLevel > 100){
      confidenceLevel = 100
    }
    
    index <- sample(1:nrow(xattack), size = 1)
    swipeToBeClassified = swipes_Series[index,]
    swipes = swipes + 1
  
  }
  
  # cat("Swipes before the device locks =", swipes)
  
  AcceptedSwipes[j] = swipes

}

meanSwipes = mean(AcceptedSwipes)
cat("Mean swipes before the device locks =", meanSwipes)

# plot(hist(AcceptedSwipes, xlab="Accepted Swipes", breaks = 20))



############################# HERE WE CHECK IN HOW MANY SWIPES THE DEVICE LOCKS WHEN THE AUTHORIZED USER HANDLES IT #####################################


# WE PUT A LIMIT OF 1000 swipes, in order for the algorithm to not stay in the while loop, in case the model works correctly and the device does not lock after 1000 swipes
maxSwipes = 1000

# this is the number of tests we want to run in order to check the mean swipes that happened before lock

# IF WE PUT A BIG NUMBER OF ITERATIONS, IT WILL TAKE SOME TIME !!!
numberOfIterations = 50

# this table keeps the number of swipes that happened before the device locked, in every iteration
userSwipesTable = c(1:numberOfIterations)

for(j in 1:numberOfIterations){
  
  # starting confidence level
  confidenceLevel = 100
  
  # threshold level for the device to close
  thresholdLevel = 30
  
  # sample a random swipe from the attacker data
  index <- sample((nrow(xattack) + 1):nrow(final), size = 1)
  swipeToBeClassified = swipes_Series[index,]
  
  # this while loop will run until the device locks
  flag = 0
  swipes = 0
  while(flag == 0 && swipes != maxSwipes){
    
    predKNN = nn2(xtrain, query = swipeToBeClassified, k = neighbors, treetype = c("kd","bd"), searchtype = c("radius"), radius = 10000)
    predKNN_mean = mean(predKNN$nn.dists)
    COP_kNN = limit - predKNN_mean
    
    predSVM = predict(svm_model, swipeToBeClassified, decision.value = T)
    predSVM_Distances = 100*attr(predSVM, "decision.values")
    COP_SVM = predSVM_Distances
    
    sumOfPredictions = COP_kNN + COP_SVM
    
    confidenceLevel = confidenceLevel + sumOfPredictions
    if(confidenceLevel < thresholdLevel){
      # cat("Device Locked")
      flag = 1
    }else if(confidenceLevel > 100){
      confidenceLevel = 100
    }
    
    index <- sample((nrow(xattack) + 1):nrow(final), size = 1)
    swipeToBeClassified = swipes_Series[index,]
    swipes = swipes + 1
    
  }
  
  # cat("Swipes before the device locks =", swipes)
  
  userSwipesTable[j] = swipes
  
}

meanSwipes = mean(userSwipesTable)
cat("Mean swipes before the device locks =", meanSwipes)

# plot(hist(userSwipesTable, breaks = 20))
