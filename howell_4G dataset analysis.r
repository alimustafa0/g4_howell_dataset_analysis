#set the current working directory
setwd("C:/Users/Ali/Desktop")
#Load the data
df <- read.csv('G4_howell.csv')
#Remove 'kg' from weight column
df$weight <- gsub(' kg', '', df$weight)
#Drop OverWeight column
df$Overweight <- NULL
#Convert sex column to factor and weight column to numeric
df$sex <- as.factor(df$sex)
df$weight <- as.numeric(df$weight)
#Print summary statistics
print(summary(df))
#Impute missing values using mice package
library(mice)
imputation <- mice(df, m = 10, meth = c('', '', 'pmm', ''), maxit = 20)
new_df <- complete(imputation, 10)
#Save the modified data
write.csv(new_df, file = 'G4_howell_mod.csv')
#Answer some questions using the modified data
#What is the average age, weight, and height of the individuals in the dataset?
print(summary(new_df[c("age", "weight", "height")]))
cat('----------------------------------------------------------------------\n')
#How many males and females are in the dataset?
print(table(new_df$sex))
cat('----------------------------------------------------------------------\n')
#What is the average weight and height of males and females separately?
print(aggregate(cbind(weight, height) ~ sex, data = new_df, mean))
cat('----------------------------------------------------------------------\n')
#What is the correlation between weight and height in the dataset?
print(cor(new_df$weight, new_df$height))
cat('----------------------------------------------------------------------\n')
#What is the average age of males and females separately?
print(aggregate(age ~ sex, data = new_df, mean))
cat('----------------------------------------------------------------------\n')
#What is the minimum and maximum age, weight, and height in the dataset?
print(summary(new_df[c("age", "weight", "height")]))
cat('----------------------------------------------------------------------\n')
#What is the distribution of age, weight, and height in the dataset?
library(ggplot2)
ggplot(new_df, aes(age)) + geom_histogram()
ggplot(new_df, aes(weight)) + geom_histogram()
ggplot(new_df, aes(height)) + geom_histogram()
cat('----------------------------------------------------------------------\n')
#Is there any relationship between sex and weight or height in the dataset?
print(aggregate(cbind(weight, height) ~ sex, data = new_df, mean))