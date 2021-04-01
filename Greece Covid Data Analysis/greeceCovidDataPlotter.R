# Author: Sokratis Koseoglou

rm(list=ls())

# setwd("../Desktop/Data/myCovidProject")

######################################################## READ DATASETS ####################################################################

# install.packages("stringr")
library("readxl")
library("stringr")

covidData <- read_excel("owid-covid-data.xlsx")
schools <- read.csv("covid_impact_education.csv")
greeceCovidData <- read_excel("greece_covid_data.xlsx")

StatusNew = c(1:nrow(schools))
for(i in 1:nrow(schools)){
  if(schools$Status[i] == "Fully open"){
    StatusNew[i] = 2
  }else if(schools$Status[i] == "Partially open"){
    StatusNew[i] = 1
  }else{
    StatusNew[i] = 0
  }
}
schools = cbind(schools, StatusNew)

schoolsSplitted <- split(schools, schools$Country)
schoolsCountry = schoolsSplitted$Greece

covidDataSplitted <- split(covidData, covidData$location)
covidCountry = covidDataSplitted$Greece


########################################################################################################################################

# newDailyCases = greeceData$new_cases

dateNew = c(1:nrow(covidCountry))
dateNew = str_sub(covidCountry$date, -5, -1)
covidCountry = cbind(covidCountry, dateNew)

casesPerTestsPercentage = c(1:nrow(covidCountry))
for(i in 1:nrow(covidCountry)){
  if(is.na(covidCountry$new_tests[i])){
    casesPerTestsPercentage[i] = -10
  }else{
    casesPerTestsPercentage[i] = covidCountry$new_cases[i]/covidCountry$new_tests[i]
  }
}
covidCountry = cbind(covidCountry, casesPerTestsPercentage)

plotVector = c(6, 7, 9, 10, 12, 13, 15, 16, 30, 26)
par(mfrow = c(4, 3))
for(i in plotVector){
  plot(covidCountry[, i], ylim = c(min(covidCountry[1:nrow(covidCountry), i], na.rm = TRUE), max(covidCountry[1:nrow(covidCountry), i],  na.rm = TRUE)), main = colnames(covidCountry)[i], ylab = colnames(covidCountry)[i], xlab = "Date", na.rm = TRUE)
  
  axis(1, at = seq_along(covidCountry$dateNew), labels = covidCountry$dateNew)
}
plot(schoolsCountry$StatusNew, ylim = c(0, 2), main = "Schools", ylab = "Schools", xlab = "Date")
axis(1, at = seq_along(covidCountry$dateNew), labels = covidCountry$dateNew)
plot(covidCountry[, 61], ylim = c(-0.1, 0.3), main = colnames(covidCountry)[61], ylab = colnames(covidCountry)[61], xlab = "Date")
axis(1, at = seq_along(covidCountry$dateNew), labels = covidCountry$dateNew)

par(mfrow = c(1, 1))

