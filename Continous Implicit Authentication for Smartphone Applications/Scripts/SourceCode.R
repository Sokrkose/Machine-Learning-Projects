# Author: Sokratis Koseoglou

###### !!!!!! ########

# this code contains:
# - Data Preprocessing (Local Outlier Factor)
# - Mean & Standard Deviation Calculations of Training Set - Data Sampling & Partitioning (75%-25%)
# - kNN model (standalone)
# - SVM model (standalone)
# - kNN-SVM combined confidence-based model

###### !!!!!! ########

# setwd("../Desktop/DataErgasia")

rm(list = ls())

library(DMwR)
#install.packages(RANN)
library(class)
library(RANN)
library(MLmetrics)
library(e1071)


###### !!!!!! ########

# choose the user, for which the model will be trained AND the direction that the model will train

userToBeClassified = 2

direction = "UP"
# direction = "DOWN"
# direction = "LEFT"
# direction = "RIGHT"

###### !!!!!! ########


# ALL VARIABLES ARE DEFINES AS "_UP" BUT THEY ARE USED FOR ALL DIRECTIONS !!!


if(direction == "UP"){
  swipes_UP = read.csv("swipes_UP_BEST.csv", stringsAsFactors = TRUE)
  swipes_UP = as.data.frame(swipes_UP)
}else if(direction == "DOWN"){
  swipes_UP = read.csv("swipes_DOWN_BEST.csv", stringsAsFactors = TRUE)
  swipes_UP = as.data.frame(swipes_UP)
}else if(direction == "LEFT"){
  swipes_UP = read.csv("swipes_LEFT_BEST.csv", stringsAsFactors = TRUE)
  swipes_UP = as.data.frame(swipes_UP)
}else if(direction == "RIGHT"){
  swipes_UP = read.csv("swipes_RIGHT_BEST.csv", stringsAsFactors = TRUE)
  swipes_UP = as.data.frame(swipes_UP)
}

#################################################################################################################################

end_UP = nrow(swipes_UP)                    # end is the final swipe of data

users_UP = unique(swipes_UP[1:end_UP, 11])  # u is a vector with all the different user IDs
num_UP = length(users_UP)                   # num is the number of different users

swipes_UP_NumberOfData = c(1:(num_UP+1))    # sortedSwipes_UP_NumberOfData shows where the starting swipe of each user is
k = 1
i = 1
while(k <= num_UP){
  if(swipes_UP[i, 11] == users_UP[k]){
    swipes_UP_NumberOfData[k] = i 
    k = k + 1
  }
  i = i + 1
}
swipes_UP_NumberOfData[num_UP+1] = end_UP + 1

s = 1  
e = 202                                      # so we are going to play with 200 users
start_UP = swipes_UP_NumberOfData[s]        # start shows the start of the swipes
end_UP = swipes_UP_NumberOfData[e] - 1      # end shows the end of the swipes, here we change it in order to play with fewer data, not all the dataset

u = unique(swipes_UP[start_UP:end_UP, 11])
num_UP = length(u)

dataPerUser = c()
for(i in 1:length(users_UP)){
  dataPerUser[i] = swipes_UP_NumberOfData[i + 1] - swipes_UP_NumberOfData[i]
}

rm(i,k,u)

################################################# Local Outlier Factor #####################################################################################

# par(mfrow = c(4,3))   # plot data from start to end
# for(k in c(8:10, 12:16, 18:21)){
#   plot(swipes_UP[swipes_UP_NumberOfData[userToBeClassified]:(swipes_UP_NumberOfData[userToBeClassified+1]-1), k], ylim = c(min(swipes_UP[swipes_UP_NumberOfData[userToBeClassified]:(swipes_UP_NumberOfData[userToBeClassified+1]-1), k]), max(swipes_UP[swipes_UP_NumberOfData[userToBeClassified]:(swipes_UP_NumberOfData[userToBeClassified+1]-1), k])), col=as.factor(swipes_UP[swipes_UP_NumberOfData[userToBeClassified]:(swipes_UP_NumberOfData[userToBeClassified+1]-1), 11]), ylab=colnames(swipes_UP[k]), xlab=colnames(swipes_UP[k]))
# }
# par(mfrow = c(1,1))

info = swipes_UP_NumberOfData[userToBeClassified + 1] - swipes_UP_NumberOfData[userToBeClassified]


#9-2
for(a in userToBeClassified){
  
  lof <- lofactor(swipes_UP[swipes_UP_NumberOfData[a]:(swipes_UP_NumberOfData[a+1]-1), 9], k = 20)
  outliers = which(lof > 2)
  
  if(length(outliers) != 0){
    outliers = outliers + swipes_UP_NumberOfData[a] - 1
    swipes_UP = swipes_UP[-outliers, ]
  }
  
  end_UP = nrow(swipes_UP) # end is the final swipe of data
  users_UP = unique(swipes_UP[1:end_UP, 11]) # u is a vector with all the different user IDs
  num_UP = length(users_UP) # num is the number of different users
  swipes_UP_NumberOfData = c(1:(num_UP+1)) # sortedSwipes_UP_NumberOfData shows where the starting swipe of each user is
  k = 1
  i = 1
  while(k <= num_UP){
    if(swipes_UP[i, 11] == users_UP[k]){
      swipes_UP_NumberOfData[k] = i
      k = k + 1
    }
    i = i + 1
  }
  swipes_UP_NumberOfData[num_UP+1] = end_UP + 1
  
}

start_UP = swipes_UP_NumberOfData[s]
end_UP = swipes_UP_NumberOfData[e] - 1
u = unique(swipes_UP[start_UP:end_UP, 11])
num_UP = length(u)

#10-2
for(a in userToBeClassified){
  
  lof <- lofactor(swipes_UP[swipes_UP_NumberOfData[a]:(swipes_UP_NumberOfData[a+1]-1), 10], k = 20)
  outliers = which(lof > 2)
  
  if(length(outliers) != 0){
    outliers = outliers + swipes_UP_NumberOfData[a] - 1
    swipes_UP = swipes_UP[-outliers, ]
  }
  
  end_UP = nrow(swipes_UP) # end is the final swipe of data
  users_UP = unique(swipes_UP[1:end_UP, 11]) # u is a vector with all the different user IDs
  num_UP = length(users_UP) # num is the number of different users
  swipes_UP_NumberOfData = c(1:(num_UP+1)) # sortedSwipes_UP_NumberOfData shows where the starting swipe of each user is
  k = 1
  i = 1
  while(k <= num_UP){
    if(swipes_UP[i, 11] == users_UP[k]){
      swipes_UP_NumberOfData[k] = i
      k = k + 1
    }
    i = i + 1
  }
  swipes_UP_NumberOfData[num_UP+1] = end_UP + 1
  
}

start_UP = swipes_UP_NumberOfData[s]
end_UP = swipes_UP_NumberOfData[e] - 1
u = unique(swipes_UP[start_UP:end_UP, 11])
num_UP = length(u)

#20-2
for(a in userToBeClassified){

  lof <- lofactor(swipes_UP[swipes_UP_NumberOfData[a]:(swipes_UP_NumberOfData[a+1]-1), 20], k = 20)
  outliers = which(lof > 2)


  if(length(outliers) != 0){
    outliers = outliers + swipes_UP_NumberOfData[a] - 1
    swipes_UP = swipes_UP[-outliers, ]
  }

  end_UP = nrow(swipes_UP) # end is the final swipe of data
  users_UP = unique(swipes_UP[1:end_UP, 11]) # u is a vector with all the different user IDs
  num_UP = length(users_UP) # num is the number of different users
  swipes_UP_NumberOfData = c(1:(num_UP+1)) # sortedSwipes_UP_NumberOfData shows where the starting swipe of each user is
  k = 1
  i = 1
  while(k <= num_UP){
    if(swipes_UP[i, 11] == users_UP[k]){
      swipes_UP_NumberOfData[k] = i
      k = k + 1
    }
    i = i + 1
  }
  swipes_UP_NumberOfData[num_UP+1] = end_UP + 1

}

start_UP = swipes_UP_NumberOfData[s]
end_UP = swipes_UP_NumberOfData[e] - 1
u = unique(swipes_UP[start_UP:end_UP, 11])
num_UP = length(u)

#21-2
for(a in userToBeClassified){

  lof <- lofactor(swipes_UP[swipes_UP_NumberOfData[a]:(swipes_UP_NumberOfData[a+1]-1), 21], k = 20)
  outliers = which(lof > 2)

  if(length(outliers) != 0){
    outliers = outliers + swipes_UP_NumberOfData[a] - 1
    swipes_UP = swipes_UP[-outliers, ]
  }

  end_UP = nrow(swipes_UP) # end is the final swipe of data
  users_UP = unique(swipes_UP[1:end_UP, 11]) # u is a vector with all the different user IDs
  num_UP = length(users_UP) # num is the number of different users
  swipes_UP_NumberOfData = c(1:(num_UP+1)) # sortedSwipes_UP_NumberOfData shows where the starting swipe of each user is
  k = 1
  i = 1
  while(k <= num_UP){
    if(swipes_UP[i, 11] == users_UP[k]){
      swipes_UP_NumberOfData[k] = i
      k = k + 1
    }
    i = i + 1
  }
  swipes_UP_NumberOfData[num_UP+1] = end_UP + 1

}

start_UP = swipes_UP_NumberOfData[s]
end_UP = swipes_UP_NumberOfData[e] - 1
u = unique(swipes_UP[start_UP:end_UP, 11])
num_UP = length(u)

#8-2
for(a in userToBeClassified){
  
  lof <- lofactor(swipes_UP[swipes_UP_NumberOfData[a]:(swipes_UP_NumberOfData[a+1]-1), 8], k = 20)
  outliers = which(lof > 2)
  
  if(length(outliers) != 0){
    outliers = outliers + swipes_UP_NumberOfData[a] - 1
    swipes_UP = swipes_UP[-outliers, ]
  }
  
  end_UP = nrow(swipes_UP) # end is the final swipe of data
  users_UP = unique(swipes_UP[1:end_UP, 11]) # u is a vector with all the different user IDs
  num_UP = length(users_UP) # num is the number of different users
  swipes_UP_NumberOfData = c(1:(num_UP+1)) # sortedSwipes_UP_NumberOfData shows where the starting swipe of each user is
  k = 1
  i = 1
  while(k <= num_UP){
    if(swipes_UP[i, 11] == users_UP[k]){
      swipes_UP_NumberOfData[k] = i
      k = k + 1
    }
    i = i + 1
  }
  swipes_UP_NumberOfData[num_UP+1] = end_UP + 1
  
}

start_UP = swipes_UP_NumberOfData[s]
end_UP = swipes_UP_NumberOfData[e] - 1
u = unique(swipes_UP[start_UP:end_UP, 11])
num_UP = length(u)

#12-2
for(a in userToBeClassified){
  
  lof <- lofactor(swipes_UP[swipes_UP_NumberOfData[a]:(swipes_UP_NumberOfData[a+1]-1), 12], k = 20)
  outliers = which(lof > 2)
  
  if(length(outliers) != 0){
    outliers = outliers + swipes_UP_NumberOfData[a] - 1
    swipes_UP = swipes_UP[-outliers, ]
  }
  
  end_UP = nrow(swipes_UP) # end is the final swipe of data
  users_UP = unique(swipes_UP[1:end_UP, 11]) # u is a vector with all the different user IDs
  num_UP = length(users_UP) # num is the number of different users
  swipes_UP_NumberOfData = c(1:(num_UP+1)) # sortedSwipes_UP_NumberOfData shows where the starting swipe of each user is
  k = 1
  i = 1
  while(k <= num_UP){
    if(swipes_UP[i, 11] == users_UP[k]){
      swipes_UP_NumberOfData[k] = i
      k = k + 1
    }
    i = i + 1
  }
  swipes_UP_NumberOfData[num_UP+1] = end_UP + 1
  
}

start_UP = swipes_UP_NumberOfData[s]
end_UP = swipes_UP_NumberOfData[e] - 1
u = unique(swipes_UP[start_UP:end_UP, 11])
num_UP = length(u)

#13-2
for(a in userToBeClassified){
  
  lof <- lofactor(swipes_UP[swipes_UP_NumberOfData[a]:(swipes_UP_NumberOfData[a+1]-1), 13], k = 20)
  outliers = which(lof > 2)
  
  if(length(outliers) != 0){
    outliers = outliers + swipes_UP_NumberOfData[a] - 1
    swipes_UP = swipes_UP[-outliers, ]
  }
  
  end_UP = nrow(swipes_UP) # end is the final swipe of data
  users_UP = unique(swipes_UP[1:end_UP, 11]) # u is a vector with all the different user IDs
  num_UP = length(users_UP) # num is the number of different users
  swipes_UP_NumberOfData = c(1:(num_UP+1)) # sortedSwipes_UP_NumberOfData shows where the starting swipe of each user is
  k = 1
  i = 1
  while(k <= num_UP){
    if(swipes_UP[i, 11] == users_UP[k]){
      swipes_UP_NumberOfData[k] = i
      k = k + 1
    }
    i = i + 1
  }
  swipes_UP_NumberOfData[num_UP+1] = end_UP + 1
  
}

start_UP = swipes_UP_NumberOfData[s]
end_UP = swipes_UP_NumberOfData[e] - 1
u = unique(swipes_UP[start_UP:end_UP, 11])
num_UP = length(u)

#14-2
for(a in userToBeClassified){
  
  lof <- lofactor(swipes_UP[swipes_UP_NumberOfData[a]:(swipes_UP_NumberOfData[a+1]-1), 14], k = 20)
  outliers = which(lof > 2)
  
  if(length(outliers) != 0){
    outliers = outliers + swipes_UP_NumberOfData[a] - 1
    swipes_UP = swipes_UP[-outliers, ]
  }
  
  end_UP = nrow(swipes_UP) # end is the final swipe of data
  users_UP = unique(swipes_UP[1:end_UP, 11]) # u is a vector with all the different user IDs
  num_UP = length(users_UP) # num is the number of different users
  swipes_UP_NumberOfData = c(1:(num_UP+1)) # sortedSwipes_UP_NumberOfData shows where the starting swipe of each user is
  k = 1
  i = 1
  while(k <= num_UP){
    if(swipes_UP[i, 11] == users_UP[k]){
      swipes_UP_NumberOfData[k] = i
      k = k + 1
    }
    i = i + 1
  }
  swipes_UP_NumberOfData[num_UP+1] = end_UP + 1
  
}

start_UP = swipes_UP_NumberOfData[s]
end_UP = swipes_UP_NumberOfData[e] - 1
u = unique(swipes_UP[start_UP:end_UP, 11])
num_UP = length(u)

#15-2
for(a in userToBeClassified){
  
  lof <- lofactor(swipes_UP[swipes_UP_NumberOfData[a]:(swipes_UP_NumberOfData[a+1]-1), 15], k = 20)
  outliers = which(lof > 2)
  
  if(length(outliers) != 0){
    outliers = outliers + swipes_UP_NumberOfData[a] - 1
    swipes_UP = swipes_UP[-outliers, ]
  }
  
  end_UP = nrow(swipes_UP) # end is the final swipe of data
  users_UP = unique(swipes_UP[1:end_UP, 11]) # u is a vector with all the different user IDs
  num_UP = length(users_UP) # num is the number of different users
  swipes_UP_NumberOfData = c(1:(num_UP+1)) # sortedSwipes_UP_NumberOfData shows where the starting swipe of each user is
  k = 1
  i = 1
  while(k <= num_UP){
    if(swipes_UP[i, 11] == users_UP[k]){
      swipes_UP_NumberOfData[k] = i
      k = k + 1
    }
    i = i + 1
  }
  swipes_UP_NumberOfData[num_UP+1] = end_UP + 1
  
}

start_UP = swipes_UP_NumberOfData[s]
end_UP = swipes_UP_NumberOfData[e] - 1
u = unique(swipes_UP[start_UP:end_UP, 11])
num_UP = length(u)

#16-2
for(a in userToBeClassified){

  lof <- lofactor(swipes_UP[swipes_UP_NumberOfData[a]:(swipes_UP_NumberOfData[a+1]-1), 16], k = 20)
  outliers = which(lof > 2)

  if(length(outliers) != 0){
    outliers = outliers + swipes_UP_NumberOfData[a] - 1
    swipes_UP = swipes_UP[-outliers, ]
  }

  end_UP = nrow(swipes_UP) # end is the final swipe of data
  users_UP = unique(swipes_UP[1:end_UP, 11]) # u is a vector with all the different user IDs
  num_UP = length(users_UP) # num is the number of different users
  swipes_UP_NumberOfData = c(1:(num_UP+1)) # sortedSwipes_UP_NumberOfData shows where the starting swipe of each user is
  k = 1
  i = 1
  while(k <= num_UP){
    if(swipes_UP[i, 11] == users_UP[k]){
      swipes_UP_NumberOfData[k] = i
      k = k + 1
    }
    i = i + 1
  }
  swipes_UP_NumberOfData[num_UP+1] = end_UP + 1

}

start_UP = swipes_UP_NumberOfData[s]
end_UP = swipes_UP_NumberOfData[e] - 1
u = unique(swipes_UP[start_UP:end_UP, 11])
num_UP = length(u)

#19-2
for(a in userToBeClassified){
  
  lof <- lofactor(swipes_UP[swipes_UP_NumberOfData[a]:(swipes_UP_NumberOfData[a+1]-1), 19], k = 20)
  outliers = which(lof > 2)
  
  if(length(outliers) != 0){
    outliers = outliers + swipes_UP_NumberOfData[a] - 1
    swipes_UP = swipes_UP[-outliers, ]
  }
  
  end_UP = nrow(swipes_UP) # end is the final swipe of data
  users_UP = unique(swipes_UP[1:end_UP, 11]) # u is a vector with all the different user IDs
  num_UP = length(users_UP) # num is the number of different users
  swipes_UP_NumberOfData = c(1:(num_UP+1)) # sortedSwipes_UP_NumberOfData shows where the starting swipe of each user is
  k = 1
  i = 1
  while(k <= num_UP){
    if(swipes_UP[i, 11] == users_UP[k]){
      swipes_UP_NumberOfData[k] = i
      k = k + 1
    }
    i = i + 1
  }
  swipes_UP_NumberOfData[num_UP+1] = end_UP + 1
  
}

start_UP = swipes_UP_NumberOfData[s]
end_UP = swipes_UP_NumberOfData[e] - 1
u = unique(swipes_UP[start_UP:end_UP, 11])
num_UP = length(u)


infoNoiseFree = swipes_UP_NumberOfData[userToBeClassified + 1] - swipes_UP_NumberOfData[userToBeClassified]
OutliersSum = info - infoNoiseFree
infoLoss = 100*OutliersSum/info

swipes_UP = swipes_UP[start_UP:end_UP,]


# par(mfrow = c(4,3))   # plot data from start to end
# for(k in c(8:10, 12:16, 18:21)){
#   plot(swipes_UP[swipes_UP_NumberOfData[userToBeClassified]:(swipes_UP_NumberOfData[userToBeClassified+1]-1), k], ylim = c(min(swipes_UP[swipes_UP_NumberOfData[userToBeClassified]:(swipes_UP_NumberOfData[userToBeClassified+1]-1), k]), max(swipes_UP[swipes_UP_NumberOfData[userToBeClassified]:(swipes_UP_NumberOfData[userToBeClassified+1]-1), k])), col=as.factor(swipes_UP[swipes_UP_NumberOfData[userToBeClassified]:(swipes_UP_NumberOfData[userToBeClassified+1]-1), 11]), ylab=colnames(swipes_UP[k]), xlab=colnames(swipes_UP[k]))
# }
# par(mfrow = c(1,1))


# par(mfrow = c(4,3))   # plot data from start to end
# for(k in c(8:10, 12:16, 18:21)){
#   plot(swipes_UP[start_UP:end_UP, k], ylim = c(min(swipes_UP[start_UP:end_UP, k]), max(swipes_UP[start_UP:end_UP, k])), col=as.factor(swipes_UP[start_UP:end_UP, 11]), ylab=colnames(swipes_UP[k]), xlab=colnames(swipes_UP[k]))
# }
# par(mfrow = c(1,1))

################################ Mean & Standard Deviation Calculations of Training Set - Data Sampling ##########################################################################################

smp_size = floor(0.75*(swipes_UP_NumberOfData[userToBeClassified+1]-swipes_UP_NumberOfData[userToBeClassified]))

#set.seed(123)
train_indicator <- sample(seq_len(swipes_UP_NumberOfData[userToBeClassified+1]-swipes_UP_NumberOfData[userToBeClassified]), size = smp_size)
training_UP = swipes_UP[train_indicator + swipes_UP_NumberOfData[userToBeClassified] - 1, ]

mean_verticalAcceleration = mean(training_UP[, 19])
mean_verticalMeanPosition = mean(training_UP[, 20])
mean_verticalTraceLength = mean(training_UP[, 21])
mean_traceSlope = mean(training_UP[, 16])
mean_coefOfDetermination = mean(training_UP[, 12])
mean_meanAbsError = mean(training_UP[, 13])
mean_meanSquaredError = mean(training_UP[, 14])
mean_meadianAbsError = mean(training_UP[, 15])
mean_horizontalAcceleration = mean(training_UP[, 8])
mean_horizontalMeanPosition = mean(training_UP[, 9])
mean_horizontalTraceLength = mean(training_UP[, 10])

############################################## Data Partition 75-25 % ##################################################################################

Attributes = c(9, 10, 20, 21, 16)

if(mean_horizontalAcceleration > 5 || mean_horizontalAcceleration < -5) { Attributes = append(Attributes, 8) }
if(mean_verticalAcceleration > 40) { Attributes = append(Attributes, 19) }
if(mean_coefOfDetermination > 0.85 || mean_coefOfDetermination < 0.4) { Attributes = append(Attributes, 12) }
if(mean_meanAbsError > 50) { Attributes = append(Attributes, 13) }
if(mean_meanSquaredError > 4000) { Attributes = append(Attributes, 14) }
if(mean_meadianAbsError > 65) { Attributes = append(Attributes, 15) }

Attributes = append(Attributes, 11)  # 11 must be always the last column !!!

rm(mean_coefOfDetermination, mean_horizontalAcceleration, mean_horizontalMeanPosition, mean_horizontalTraceLength, mean_meadianAbsError, mean_meanAbsError, mean_meanSquaredError, mean_traceSlope, mean_verticalAcceleration, mean_verticalMeanPosition, mean_verticalTraceLength)

training_UP = training_UP[, Attributes]

testing_UP = swipes_UP[swipes_UP_NumberOfData[userToBeClassified]:(swipes_UP_NumberOfData[userToBeClassified + 1] - 1), Attributes]
testing_UP = testing_UP[-train_indicator, ]

attackers_UP = swipes_UP[-(swipes_UP_NumberOfData[userToBeClassified]:(swipes_UP_NumberOfData[userToBeClassified + 1] - 1)), Attributes]

xtrain = training_UP[, -length(Attributes)]
ytrain = training_UP[, length(Attributes)]

cohesion = kmeans(xtrain, centers=1)$tot.withinss

xtest = testing_UP[, -length(Attributes)]
ytest = testing_UP[, length(Attributes)]

xattack = attackers_UP[, -length(Attributes)]
yattack = attackers_UP[, length(Attributes)]

######################################################## k-NN ##############################################################################################

neighbors = 3

trainNN = nn2(xtrain, query = xtrain, k = neighbors, treetype = c("kd","bd"), searchtype = c("radius"), radius = 100)
# View(trainNN$nn.dists)

trainNN_means = c(1:nrow(xtrain))
for(i in 1:length(trainNN_means)){
  trainNN_means[i] = mean(trainNN$nn.dists[i, ])
}

trainMean = mean(trainNN_means)
trainSD = sd(trainNN_means)

limit = trainMean + 2.5*trainSD

testNN = nn2(xtrain, query = xtest, k = neighbors, treetype = c("kd","bd"), searchtype = c("radius"), radius = 10000)
# View(testNN$nn.dists)
attackNN = nn2(xtrain, query = xattack, k = neighbors, treetype = c("kd","bd"), searchtype = c("radius"), radius = 10000)
# View(attackNN$nn.dists)

testNN_means = c(1:nrow(xtest))
attackNN_means = c(1:nrow(xattack))
testPredkNN = c(1:nrow(xtest))
attackPredkNN = c(1:nrow(xattack))

for(i in 1:length(testNN_means)){
  testNN_means[i] = mean(testNN$nn.dists[i, ])
}

for(i in 1:length(attackNN_means)){
  attackNN_means[i] = mean(attackNN$nn.dists[i, ])
}


TP = 0
FN = 0
for(i in 1:length(testNN_means)){
  if(testNN_means[i] <= limit){
    TP = TP + 1
    testPredkNN[i] = 1
  }else{
    FN = FN + 1
    testPredkNN[i] = 0
  }
}

FP = 0
TN = 0
for(i in 1:length(attackNN_means)){
  if(attackNN_means[i] <= limit){
    FP = FP + 1
    attackPredkNN[i] = 0
  }else{
    TN = TN + 1
    attackPredkNN[i] = 1
  }
}

# View(testPredkNN)
# View(testNN_means)
# View(attackPredkNN)
# View(attackNN_means)


FRR_kNN = FN/length(testNN_means)
FAR_kNN = FP/length(attackNN_means)
TPR_kNN = TP/(TP + FN)
FPR_kNN = FP/(FP + FN)
Accuracy_kNN = 100*(TP + TN)/(TP + TN + FP + FN)

cat("Training Data Length: ", nrow(xtrain), "\n")
cat("Testing Data Length: ", nrow(xtest), "\n")
cat("Attackers Data Length: ", nrow(xattack), "\n")
cat("k-Nearest Neighbors are:", neighbors, "\n")
cat("Attributes for user", userToBeClassified, "are:", Attributes, "\n")
cat("FRR: ", FRR_kNN, "\nFAR: ", FAR_kNN, "\nTPR: ", TPR_kNN, "\nFPR: ", FPR_kNN, "\n")
cat("TP: ", TP, "\nFN: ", FN, "\nFP: ", FP, "\nTN: ", TN, "\n")
cat("k-NN Accuracy Percentage of user", userToBeClassified, "is: ", Accuracy_kNN, "%\n")


#################################################### SVM ##############################################################################################


gammavalues = c(0.0000001, 0.000001, 0.000005, 0.00001, 0.00005, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 1, 10, 100, 1000, 10000)

nu = c(0.01, 0.03, 0.05, 0.08, 0.1, 0.12, 0.15, 0.2, 0.25)

par(mfrow = c(3, 3))

for(nu in nu){

  training_error = c()
  for(gamma in gammavalues){
    svm_model = svm(xtrain, ytrain, kernel = "radial",type = "one-classification", gamma = gamma, nu = nu)
    pred = predict(svm_model, xtrain)
    true = 0
    false = 0
    for(i in 1:length(pred)){
      if(pred[i] == TRUE){
        true = true + 1
      }else{
        false = false + 1
      }
    }
    training_error = c(training_error, false/(true + false))
  }
  
  testing_error = c()
  for(gamma in gammavalues){
    svm_model = svm(xtrain, ytrain, type = "one-classification", gamma = gamma, nu = nu)
    pred = predict(svm_model, xtest)
    true = 0
    false = 0
    for(i in 1:length(pred)){
      if(pred[i] == TRUE){
        true = true + 1
      }else{
        false = false + 1
      }
    }
    testing_error = c(testing_error, false/(true + false))
  }
  
  attacker_error = c()
  for(gamma in gammavalues){
    svm_model = svm(xtrain, ytrain, type = "one-classification", gamma = gamma, nu = nu)
    pred = predict(svm_model, xattack)
    true = 0
    false = 0
    for(i in 1:length(pred)){
      if(pred[i] == TRUE){
        true = true + 1
      }else{
        false = false + 1
      }
    }
    attacker_error = c(attacker_error, true/(true + false))
  }
  
  plot(training_error, type = "l", col = "blue", ylim = c(0, 1), xlab = "Gamma", ylab = "Error", xaxt = "n", main = paste("nu = ", nu, sep = ""))
  axis(1, at = 1:length(gammavalues), labels = gammavalues)
  # plot(testing_error, type = "l", col = "blue", ylim = c(0, 0.8), xlab = "Gamma", ylab = "Error", xaxt = "n", main = paste("nu = ", nu, sep = ""))
  # axis(1, at = 1:length(gammavalues), labels = gammavalues)
  lines(testing_error, col = "red")
  lines(attacker_error, col = "black")
  #legend("right", c("Training Error", "Testing Error", "Attacker Error"), pch = c("-", "-", "-"), col = c("blue", "red", "black"))

}

par(mfrow = c(1, 1))


###### now select the best nu-gamma


gamma = 0.005
nu = 0.2

svm_model = svm(xtrain, ytrain, type = "one-classification", gamma = gamma, nu = nu)

predTest = predict(svm_model, xtest, decision.value = T)
predTest_Distances = 100*attr(predTest, "decision.values")
predAttack = predict(svm_model, xattack, decision.value=T)
predAttack_Distances = 100*attr(predAttack, "decision.values")

TP = 0
FN = 0
for(i in 1:length(predTest)){
  if(predTest[i] == TRUE){
    TP = TP + 1
  }else{
    FN = FN + 1
  }
}

FP = 0
TN = 0
for(i in 1:length(predAttack)){
  if(predAttack[i] == TRUE){
    FP = FP + 1
  }else{
    TN = TN + 1
  }
}

FRR_SVM = FN/nrow(xtest)
FAR_SVM = FP/nrow(xattack)
TPR_SVM = TP/(TP + FN)
FPR_SVM = FP/(FP + FN)
Accuracy_SVM = 100*(TP + TN)/(TP + TN + FP + FN)

cat("Training Data Length: ", nrow(xtrain), "\n")
cat("Testing Data Length: ", nrow(xtest), "\n")
cat("Attackers Data Length: ", nrow(xattack), "\n")
cat("Gamma:", gamma, "\n")
cat("n:", nu, "\n")
cat("Attributes for user", userToBeClassified, "are:", Attributes, "\n")
cat("FRR: ", FRR_SVM, "\nFAR: ", FAR_SVM, "\nTPR: ", TPR_SVM, "\nFPR: ", FPR_SVM, "\n")
cat("TP: ", TP, "\nFN: ", FN, "\nFP: ", FP, "\nTN: ", TN, "\n")
cat("SVM Accuracy Percentage of user", userToBeClassified, "is: ", Accuracy_SVM, "%\n")


################################################### kNN - SVM Colaboration #################################################################

swipes_Series = xattack
swipes_Series = rbind(swipes_Series, xtest)

actual_Values = matrix(0, ncol = 1, nrow = nrow(swipes_Series))
actual_Values[(nrow(xattack)+1):(nrow(xattack)+nrow(xtest))] = 1




neighbors = 3

trainNN = nn2(xtrain, query = xtrain, k = neighbors, treetype = c("kd","bd"), searchtype = c("radius"), radius = 10000)
# View(trainNN$nn.dists)

trainNN_means = c(1:nrow(xtrain))
for(i in 1:length(trainNN_means)){
  trainNN_means[i] = mean(trainNN$nn.dists[i, ])
}

trainMean = mean(trainNN_means)
trainSD = sd(trainNN_means)

limit = trainMean + 2.5*trainSD

predKNN = nn2(xtrain, query = swipes_Series, k = neighbors, treetype = c("kd","bd"), searchtype = c("radius"), radius = 10000)
# View(testNN$nn.dists)

predKNN_means = c(1:nrow(swipes_Series))
kNN = c(1:nrow(swipes_Series))

for(i in 1:length(predKNN_means)){
  predKNN_means[i] = mean(predKNN$nn.dists[i, ])
}

for(i in 1:length(predKNN_means)){
  if(predKNN_means[i] <= limit){
    kNN[i] = 1
  }else{
    kNN[i] = 0
  }
}




gamma = 0.1
nu = 0.005

svm_model = svm(xtrain, ytrain, type = "one-classification", gamma = gamma, nu = nu)
predSVM = predict(svm_model, swipes_Series, decision.value = T)
predSVM_Distances = 100*attr(predSVM, "decision.values")




final = cbind(actual_Values, predSVM)
final = cbind(final, predSVM_Distances)
final = cbind(final, kNN)
final = cbind(final, predKNN_means)

confidence = c(1:nrow(final))
a1 = c(1:nrow(final))
a2 = c(1:nrow(final))

for(i in 1:nrow(final)){
  
  a1[i] = final[i, 3]
  a2[i] = (limit - final[i, 5])

  confidence[i] = a1[i] + a2[i]
  
}

final = cbind(final, a1)
final = cbind(final, a2)
final = cbind(final, confidence)

FP = 0
TN = 0
for(i in 1:nrow(xattack)){
  if(final[i, 8] > 5){
    FP = FP + 1
  }else if(final[i, 8] < -5){
    TN = TN + 1
  }
}

FN = 0
TP = 0
for(i in (nrow(xattack) + 1):nrow(final)){
  if(final[i, 8] < -5){
    FN = FN + 1
  }else if (final[i, 8] > 5){
    TP = TP + 1
  }
}

FRR_kNN_SVM = FN/nrow(xtest)
FAR_kNN_SVM = FP/nrow(xattack)
TPR_kNN_SVM = TP/(TP + FN)
FPR_kNN_SVM = FP/(FP + FN)
Accuracy_kNN_SVM = 100*(TP + TN)/(TP + TN + FP + FN)

cat("Training Data Length: ", nrow(xtrain), "\n")
cat("Testing Data Length: ", nrow(xtest), "\n")
cat("Attackers Data Length: ", nrow(xattack), "\n")
cat("Attributes for user", userToBeClassified, "are:", Attributes, "\n")
cat("FRR: ", FRR_kNN_SVM, "\nFAR: ", FAR_kNN_SVM, "\nTPR: ", TPR_kNN_SVM, "\nFPR: ", FPR_kNN_SVM, "\n")
cat("TP: ", TP, "\nFN: ", FN, "\nFP: ", FP, "\nTN: ", TN, "\n")
cat("kNN-SVM Accuracy Percentage of user", userToBeClassified, "is: ", Accuracy_kNN_SVM, "%\n")
