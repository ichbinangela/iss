setwd("D:/Thesis/Data")

library(ggplot2)

titanic <- read.csv("titanic_full.csv")
n <- nrow(titanic)

titanic$pclass <- factor(titanic$pclass)
titanic$survived <- factor(titanic$survived)

# Remove if age is NA
titanic <- titanic[!is.na(titanic$age),]
# Create kid variable for age < 7
titanic$kid <- as.factor(ifelse(titanic$age < 7,"1","0"))
# Remove if kid is dead
titanic <- titanic[-which(titanic$kid =="1" & titanic$survived == "0"),]
# Drop unused level
titanic <- droplevels(titanic)

titanic.kid <- titanic[which(titanic$kid == "1"),]
titanic.adult <- titanic[which(titanic$kid == "0"),]