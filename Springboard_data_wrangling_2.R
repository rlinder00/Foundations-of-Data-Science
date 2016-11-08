#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("stringr")

library(dplyr)
library(tidyr)
library(stringr)

#0

data <- read.csv('/Users/Randy_Linder/Google Drive/Springboard/Foundations-of-Data-Science/titanic_original.csv', header = T, sep = ",")
data <- data.frame(data)

#1

#The embarked column has some missing values, which are known to correspond to passengers 
#who actually embarked at Southampton. Find the missing values and replace them with S.
#(Caution: Sometimes a missing value might be read into R as a blank or empty string.)

data$embarked[is.na(data$embarked) || data$embarked == ''] <- 'S'

#2: Age
#You’ll notice that a lot of the values in the Age column are missing. 
#While there are many ways to fill these missing values, using the mean or median of 
#the rest of the values is quite common in such cases.
#Calculate the mean of the Age column and use that value to populate the missing values
#Think about other ways you could have populated the missing values in the age column. 
#Why would you pick any of those over the mean (or not)?

data$Age[is.na(data$age)] <- mean(data$age)

# mean could be misleading if the variance of ages is substantial. For example, if there is one 100 year old
# and the rest are 15, the average would overly skew ages. 


#3: Lifeboat
#You’re interested in looking at the distribution of passengers in different lifeboats, 
#but as we know, many passengers did not make it to a boat :-( This means that there are a lot of missing 
#values in the boat column. Fill these empty slots with a dummy value e.g. the string 'None' or 'NA'

data$boat <- as.character(data$boat)
data$boat[is.na(data$boat)] <- "none"
                                                                                                                                                     
#4: Cabin
#You notice that many passengers don’t have a cabin number associated with them.
#Does it make sense to fill missing cabin numbers with a value?
#What does a missing value here mean?
#You have a hunch that the fact that the cabin number is missing might be a useful indicator of survival. \
#Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.

data <- data %>% mutate(has_cabin_number = !is.na(data$cabin))

#6: Submit the project on Github
#Include your code, the original data as a CSV file titanic_original.csv, and the cleaned up data as a CSV file called titanic_clean.csv.                                                                                                                                             Does it make sense to fill missing cabin numbers with a value?
write.csv(data, 'titanic_clean.csv')
