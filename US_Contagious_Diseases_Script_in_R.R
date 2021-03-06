#The start of working with the US Contagious Diseases

#Setting my working Directory in order to be in the folder where the US_Contagious_Diseases is.
setwd("C:/Users/micha/Desktop/DevLeague Begins Nov 7 2017/Project_Sprint_5/")

#Loading libraries and reading the CSV file "us_contagious_diseases into R.

#Loading the library dplyr in order for the 
library(dplyr)

#Loading the library ggplot2
library(ggplot2)

#Loading the library plyr in order to use the ddply() function
library("plyr")


#Creating the variable "sick" to represent the ""us_contagious_diseases.csv"" 
#which was pulled from the Internet
sick <- read.csv("us_contagious_diseases.csv")


#calculating the percentage of people that are sick for that particular disease per the population
sick <- transform(sick, percentage_sick = count / population)

#turning the year column of integers into a column that contains R categories (called factors) 
#Don't know for sure if I need to do this or not yet...
#sick$year <- integer(sick$year, ordered = TRUE)

#If I wanted to know the highest year in the dataset
highest_year <- max(sick$year) 

#If I wanted to know the highest counts of diseases in the dataset
highest_count <- max(sick$count) 

#If I wanted to know the highest percentage_sick that is found in the data set.  Also needed to do the na.rm=True 
#in order for it to work.
highest_sick <- (sick$percentage_sick, na.rm = TRUE
                 
lowest_sick <- min(sick$percentage_sick, na.rm = TRUE
                                    
                                    
filter(sick, !is.na(percentage_sick))
                                    
Sicker <- filter(sick, !is.na(percentage_sick))
                    
#In order to give me the highest percentage_sick for each state, still need to now figure
#out how to get the rest of the columns back
#States_highest_sick <- ddply(sick, 'state', summarize, High_State = max(percentage_sick))
                                    
States_highest_sick <- ddply(Sicker, 'state', function(x) x[x$percentage_sick==max(x$percentage_sick),])
View(States_highest_sick)

