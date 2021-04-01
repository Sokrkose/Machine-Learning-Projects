# Author: Sokratis Koseoglou

###### !!!!!! ########

# DO NOT RUN THIS CODE !!!
# THIS CODE TAKES A LOT OF TIME !!!
# This code is responsible for partitioning the dataset into smaller datasets
# The outputs of this code are the excel files

###### !!!!!! ########

################################################ Basic Data Preprocessing and Partitioning ############################################################

rm(list = ls())

setwd("../Desktop/DataErgasia")

# first we read the data, we sort them according to the playerID feature and we scale the data to 360x640

swipesData = read.csv("swipes.csv")

sortedSwipes = swipesData[order(swipesData$playerID), ]
sortedUsers = unique(sortedSwipes$playerID)
numberOfSortedUsers = length(sortedUsers)

# data scaling to (360x640)
for(i in 1:nrow(sortedSwipes)){
  if(sortedSwipes[i, 3] != 360){
    sortedSwipes[i, 9] = (360*sortedSwipes[i, 9])/sortedSwipes[i, 3]
    sortedSwipes[i, 10] = (360*sortedSwipes[i, 10])/sortedSwipes[i, 3]
  }
  
  if(sortedSwipes[i, 4] != 640){
    sortedSwipes[i, 20] = (640*sortedSwipes[i, 20])/sortedSwipes[i, 4]
    sortedSwipes[i, 21] = (640*sortedSwipes[i, 21])/sortedSwipes[i, 4]
  }
}

#write.csv(sortedSwipes, file = "sortedSwipesNormalized.csv", row.names = FALSE)

# here we read the normalized and sorted data

sortedSwipes = read.csv("sortedSwipesNormalized.csv")

sortedUsers_NumberOfData = c(1:(numberOfSortedUsers+1))
k = 1
for(i in 1:nrow(sortedSwipes)){
  if(sortedSwipes[i, 11] == sortedUsers[k]){
    sortedUsers_NumberOfData[k] = i 
    k = k + 1
  }
}
sortedUsers_NumberOfData[numberOfSortedUsers+1] = (nrow(sortedSwipes) + 1)


sortedUsersTruthTable = matrix(0, ncol = 4, nrow = numberOfSortedUsers)
colnames(sortedUsersTruthTable) <- c("up", "down", "left", "right")


# now from all the dataset, we find which users have more than 35 swipes to any direction, so as to not take into consideration
# the users that do not have a lot of data points

badUsersThreshold = 35

for(i in 1:(length(sortedUsers_NumberOfData) - 1)){
  up = 0
  down = 0
  left = 0
  right = 0
  
  for(j in sortedUsers_NumberOfData[i]:(sortedUsers_NumberOfData[i+1]-1)){
    if(sortedSwipes[j, 7] == "up"){
      up = up + 1
    }else if(sortedSwipes[j, 7] == "down"){
      down = down + 1
    }else if(sortedSwipes[j, 7] == "left"){
      left = left + 1
    }else if(sortedSwipes[j, 7] == "right"){
      right = right + 1
    }
  }
  
  if(up >= badUsersThreshold){
    sortedUsersTruthTable[i, 1] = 1
  }
  if(down >= badUsersThreshold){
    sortedUsersTruthTable[i, 2] = 1
  }
  if(left >= badUsersThreshold){
    sortedUsersTruthTable[i, 3] = 1
  }
  if(right >= badUsersThreshold){
    sortedUsersTruthTable[i, 4] = 1
  }
}
rm(up,down,left,right,badUsersThreshold)


# here we start the partitioning of the dataset to smaller ones

###################################################### THIS WILL TAKE A LOT OF TIME ###############################################################

for(i in 1:nrow(sortedUsersTruthTable)){
  if(sortedUsersTruthTable[i, 1] == 1){
    for(j in sortedUsers_NumberOfData[i]:(sortedUsers_NumberOfData[i+1]-1)){
      if(sortedSwipes[j, 7] != "up"){
        sortedSwipes[j, ] = NA
      }
    }
  }else{
    for(j in sortedUsers_NumberOfData[i]:(sortedUsers_NumberOfData[i+1]-1)){
      sortedSwipes[j, ] = NA
    }
  }
  print(i) #this is in order to know when it will finish, while running
}

sortedSwipes_UP = na.omit(sortedSwipes)

write.csv(sortedSwipes_UP, file = "sortedSwipes_UP.csv", row.names = FALSE)

rm(sortedSwipes)

sortedSwipes = read.csv("sortedSwipesNormalized.csv")

sortedUsers_NumberOfData = c(1:(numberOfSortedUsers+1))
k = 1
for(i in 1:nrow(sortedSwipes)){
  if(sortedSwipes[i, 11] == sortedUsers[k]){
    sortedUsers_NumberOfData[k] = i 
    k = k + 1
  }
}
sortedUsers_NumberOfData[numberOfSortedUsers+1] = (nrow(sortedSwipes) + 1)

for(i in 1:nrow(sortedUsersTruthTable)){
  if(sortedUsersTruthTable[i, 2] == 1){
    for(j in sortedUsers_NumberOfData[i]:(sortedUsers_NumberOfData[i+1]-1)){
      if(sortedSwipes[j, 7] != "down"){
        sortedSwipes[j, ] = NA
      }
    }
  }else{
    for(j in sortedUsers_NumberOfData[i]:(sortedUsers_NumberOfData[i+1]-1)){
      sortedSwipes[j, ] = NA
    }
  }
  print(i) #this is in order to know when it will finish, while running
}

sortedSwipes_DOWN = na.omit(sortedSwipes)

write.csv(sortedSwipes_DOWN, file = "sortedSwipes_DOWN.csv", row.names = FALSE)

rm(sortedSwipes)

sortedSwipes = read.csv("sortedSwipesNormalized.csv")

sortedUsers_NumberOfData = c(1:(numberOfSortedUsers+1))
k = 1
for(i in 1:nrow(sortedSwipes)){
  if(sortedSwipes[i, 11] == sortedUsers[k]){
    sortedUsers_NumberOfData[k] = i 
    k = k + 1
  }
}
sortedUsers_NumberOfData[numberOfSortedUsers+1] = (nrow(sortedSwipes) + 1)

for(i in 1:nrow(sortedUsersTruthTable)){
  if(sortedUsersTruthTable[i, 3] == 1){
    for(j in sortedUsers_NumberOfData[i]:(sortedUsers_NumberOfData[i+1]-1)){
      if(sortedSwipes[j, 7] != "left"){
        sortedSwipes[j, ] = NA
      }
    }
  }else{
    for(j in sortedUsers_NumberOfData[i]:(sortedUsers_NumberOfData[i+1]-1)){
      sortedSwipes[j, ] = NA
    }
  }
  print(i) #this is in order to know when it will finish, while running
}

sortedSwipes_LEFT = na.omit(sortedSwipes)

write.csv(sortedSwipes_LEFT, file = "sortedSwipes_LEFT.csv", row.names = FALSE)

rm(sortedSwipes)

sortedSwipes = read.csv("sortedSwipesNormalized.csv")

sortedUsers_NumberOfData = c(1:(numberOfSortedUsers+1))
k = 1
for(i in 1:nrow(sortedSwipes)){
  if(sortedSwipes[i, 11] == sortedUsers[k]){
    sortedUsers_NumberOfData[k] = i 
    k = k + 1
  }
}
sortedUsers_NumberOfData[numberOfSortedUsers+1] = (nrow(sortedSwipes) + 1)

for(i in 1:nrow(sortedUsersTruthTable)){
  if(sortedUsersTruthTable[i, 4] == 1){
    for(j in sortedUsers_NumberOfData[i]:(sortedUsers_NumberOfData[i+1]-1)){
      if(sortedSwipes[j, 7] != "right"){
        sortedSwipes[j, ] = NA
      }
    }
  }else{
    for(j in sortedUsers_NumberOfData[i]:(sortedUsers_NumberOfData[i+1]-1)){
      sortedSwipes[j, ] = NA
    }
  }
  print(i) #this is in order to know when it will finish, while running
}

sortedSwipes_RIGHT = na.omit(sortedSwipes)

write.csv(sortedSwipes_RIGHT, file = "sortedSwipes_RIGHT.csv", row.names = FALSE)

##############################

rm(sortedSwipes)

sortedSwipes = read.csv("sortedSwipesNormalized.csv")

sortedUsers_NumberOfData = c(1:(numberOfSortedUsers+1))
k = 1
for(i in 1:nrow(sortedSwipes)){
  if(sortedSwipes[i, 11] == sortedUsers[k]){
    sortedUsers_NumberOfData[k] = i 
    k = k + 1
  }
}
sortedUsers_NumberOfData[numberOfSortedUsers+1] = (nrow(sortedSwipes) + 1)

for(i in 1:nrow(sortedUsersTruthTable)){
  if(sortedUsersTruthTable[i, 1] != 1 || sortedUsersTruthTable[i, 2] != 1 || sortedUsersTruthTable[i, 3] != 1 || sortedUsersTruthTable[i, 4] != 1){
    for(j in sortedUsers_NumberOfData[i]:(sortedUsers_NumberOfData[i+1]-1)){
      sortedSwipes[j, ] = NA
    }
  }
  print(i) #this is in order to know when it will finish, while running
}

sortedSwipes_BEST = na.omit(sortedSwipes)

write.csv(sortedSwipes_BEST, file = "sortedSwipes_BEST_v1.csv", row.names = FALSE)




rm(list = ls())

swipes = read.csv("sortedSwipes_BEST.csv")

users = unique(swipes$playerID)
numberOfUsers = length(users)


swipes_NumberOfData = c(1:(numberOfUsers+1))
k = 1
for(i in 1:nrow(swipes)){
  if(swipes[i, 11] == users[k]){
    swipes_NumberOfData[k] = i 
    k = k + 1
  }
}
swipes_NumberOfData[numberOfUsers+1] = (nrow(swipes) + 1)
swipes_NumberOfData

for(i in 1:numberOfUsers){
  for(j in swipes_NumberOfData[i]:(swipes_NumberOfData[i+1]-1)){
    if(swipes[j, 7] != "up"){
      swipes[j, ] = NA
    }
  }
  print(i) #this is in order to know when it will finish, while running
}

swipes_UP = na.omit(swipes)

write.csv(swipes_UP, file = "swipes_UP_BEST.csv", row.names = FALSE)

rm(swipes)

swipes = read.csv("sortedSwipes_BEST.csv")

for(i in 1:numberOfUsers){
  for(j in swipes_NumberOfData[i]:(swipes_NumberOfData[i+1]-1)){
    if(swipes[j, 7] != "down"){
      swipes[j, ] = NA
    }
  }
  print(i) #this is in order to know when it will finish, while running
}

swipes_DOWN = na.omit(swipes)

write.csv(swipes_DOWN, file = "swipes_DOWN_BEST.csv", row.names = FALSE)

rm(swipes)

swipes = read.csv("sortedSwipes_BEST.csv")

for(i in 1:numberOfUsers){
  for(j in swipes_NumberOfData[i]:(swipes_NumberOfData[i+1]-1)){
    if(swipes[j, 7] != "left"){
      swipes[j, ] = NA
    }
  }
  print(i) #this is in order to know when it will finish, while running
}

swipes_LEFT = na.omit(swipes)

write.csv(swipes_LEFT, file = "swipes_LEFT_BEST.csv", row.names = FALSE)

rm(swipes)

swipes = read.csv("sortedSwipes_BEST.csv")

for(i in 1:numberOfUsers){
  for(j in swipes_NumberOfData[i]:(swipes_NumberOfData[i+1]-1)){
    if(swipes[j, 7] != "right"){
      swipes[j, ] = NA
    }
  }
  print(i) #this is in order to know when it will finish, while running
}

swipes_RIGHT = na.omit(swipes)

write.csv(swipes_RIGHT, file = "swipes_RIGHT_BEST.csv", row.names = FALSE)


################################################### next time start from here #################################################################

rm(list = ls())

setwd("../Desktop/DataErgasia")

# read the data
sortedSwipes_BEST = read.csv("sortedSwipes_BEST.csv", stringsAsFactors = TRUE)
sortedSwipes_BEST = as.data.frame(sortedSwipes_BEST)

# these files have all te swipes for each direcrion

sortedSwipes_UP = read.csv("sortedSwipes_UP.csv", stringsAsFactors = TRUE)
sortedSwipes_UP = as.data.frame(sortedSwipes_UP)
sortedSwipes_DOWN = read.csv("sortedSwipes_DOWN.csv", stringsAsFactors = TRUE)
sortedSwipes_DOWN = as.data.frame(sortedSwipes_DOWN)
sortedSwipes_LEFT = read.csv("sortedSwipes_LEFT.csv", stringsAsFactors = TRUE)
sortedSwipes_LEFT = as.data.frame(sortedSwipes_LEFT)
sortedSwipes_RIGHT = read.csv("sortedSwipes_RIGHT.csv", stringsAsFactors = TRUE)
sortedSwipes_RIGHT = as.data.frame(sortedSwipes_RIGHT)

# these files have anly the "best" users. The users that have more than 35 swipes for one or more directions

swipes_UP = read.csv("swipes_UP_BEST.csv", stringsAsFactors = TRUE)
swipes_UP = as.data.frame(swipes_UP)
swipes_DOWN = read.csv("swipes_DOWN_BEST.csv", stringsAsFactors = TRUE)
swipes_DOWN = as.data.frame(swipes_DOWN)
swipes_LEFT = read.csv("swipes_LEFT_BEST.csv", stringsAsFactors = TRUE)
swipes_LEFT = as.data.frame(swipes_LEFT)
swipes_RIGHT = read.csv("swipes_RIGHT_BEST.csv", stringsAsFactors = TRUE)
swipes_RIGHT = as.data.frame(swipes_RIGHT)


############################################### check if there are FAKE SWIPES or LONG SWIPES ############################################################

library("rjson")
badSwipes = 0
setwd("rawData/rawData")
for(n in 1:161041){
  filename1 = paste(as.character(sortedSwipes[1, 1]), ".json", sep = "")
  swipeRawData <- fromJSON(file = filename1)
  if(length(swipeRawData) <= 4 || length(swipeRawData) > 30){
    badSwipes = badSwipes + 1
  }
}
print(badSwipes)

