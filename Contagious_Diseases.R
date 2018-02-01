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

#loading library car as it may be used for the Anova Calculation
libary("car")

#loading library ggpubr as it used for boxplots
library("ggpubr")

#loading the library multcomp to perform multiple comparison procedures for an ANOVA. glht stands for general linear hypothesis tests. 
library(multcomp)

#Creating the variable "sick" to represent the ""us_contagious_diseases.csv"" 
#which was pulled from the Internet
sick <- read.csv("us_contagious_diseases.csv")

#This line of code gives me the list of all the different diseases
unique(sick$disease) 

#calculating the percentage of people that are sick for that particular disease per the population
sick <- transform(sick, percentage_sick = count / population)

#turning the year column of integers into a column that contains R categories (called factors) 
#Don't know for sure if I need to do this or not yet...
#sick$year <- integer(sick$year, ordered = TRUE)

#If I wanted to know the highest year in the dataset
highest_year <- max(sick$year) 

#If I wanted to know the highest counts of diseases in the dataset
highest_count <- max(sick$count) 

#If I wanted to know the highest percentage_sick that is found in the data set.  
#Also needed to do the na.rm=True in order for it to work

highest_percentage_sick <- max(sick$percentage_sick, na.rm = TRUE)
                                    
#Filter out all of the NA's for the percentage_sick                                    
filter(sick, !is.na(percentage_sick))
                                    
#Create a new variable called "Sicker" that only has the percentage_sick values without any NA's
Sicker <- filter(sick, !is.na(percentage_sick))
                                   
                             
#Calculate the top disease per State (by the highest percentage_sick)                                    
States_highest_sick <- ddply(Sicker, 'state', function(x) x[x$percentage_sick==max(x$percentage_sick),])

For_Print_out_Highest_States <-States_highest_sick <- ddply(Sicker, 'state', function(x) x[x$percentage_sick==max(x$percentage_sick),])

#Calculate the mean of the top disease per state (by the highest percentage_sick) 
mean(States_highest_sick$percentage_sick)

#Calculate the Standard Deviation of the top disease per state (by the highest percentage_sick) 
sd(States_highest_sick$percentage_sick)

#Calculate the Summary Statistics of the top disease per state (by the highest percentage_sick)
summary(States_highest_sick$percentage_sick)


#Create a varibable name of Hepatitis_A_Only which will be used to View/Calculate further only that disease
Hepatitis_A_Only <- subset(Sicker, disease == "Hepatitis A")

#Calculate the Hepatatis A disease per State (by the highest percentage_sick)   
Hepatitis_A_States_highest <- ddply(Hepatitis_A_Only, 'state', function(x) x[x$percentage_sick==max(x$percentage_sick),])

#Calculate the mean of the highest Hepatatis_A disease per state (by the highest percentage_sick) 
mean(Hepatitis_A_States_highest$percentage_sick)

#Create a varible name "Hep_A_Mean" to be used for output
Hep_A_Mean <- mean(Hepatitis_A_States_highest$percentage_sick)



#Create a varibable name of Measles_Only which will be used to View/Calculate further only that disease
Measles_Only <- subset(Sicker, disease == "Measles")

#Calculate the Measles disease per State (by the highest percentage_sick)   
Measles_States_highest <- ddply(Measles_Only, 'state', function(x) x[x$percentage_sick==max(x$percentage_sick),])

#Calculate the mean of the highest Hepatatis_A disease per state (by the highest percentage_sick) 
mean(Measles_States_highest$percentage_sick)

#Create a varible name "Measles_Mean" to be used for output
Measles_Mean <- mean(Measles_States_highest$percentage_sick)



#Create a varibable name of Mumps_Only which will be used to View/Calculate further only that disease
Mumps_Only <- subset(Sicker, disease == "Mumps")

#Calculate the Mumps disease per State (by the highest percentage_sick)   
Mumps_States_highest <- ddply(Mumps_Only, 'state', function(x) x[x$percentage_sick==max(x$percentage_sick),])

#Calculate the mean of the highest Mumps disease per state (by the highest percentage_sick) 
mean(Mumps_States_highest$percentage_sick)

#Create a varible name "Mumps_Mean" to be used for output
Mumps_Mean <- mean(Mumps_States_highest$percentage_sick) 


#Create a varibable name of Pertussis_Only which will be used to View/Calculate further only that disease
Pertussis_Only <- subset(Sicker, disease == "Pertussis")

filter(Pertussis_Only, !is.na(weeks_reporting))
Pertussis_weeks_reporting <- filter(Pertussis_Only, !is.na(weeks_reporting))

filter(Pertussis_Only, !is.na(count))
Pertussis_count <- filter(Pertussis_Only, !is.na(count))


filter(Pertussis_Only, !is.na(percentage_sick))
Pertussis_percentage_sick <- filter(Pertussis_Only, !is.na(percentage_sick))

#Calculate the Pertussis disease per State (by the highest percentage_sick)   
Pertussis_States_highest <- ddply(Pertussis_percentage_sick, 'state', function(x) x[x$percentage_sick==max(x$percentage_sick),])

#Calculate the mean of the highest Pertussis disease per state (by the highest percentage_sick) 
mean(Pertussis_States_highest$percentage_sick)

#Create a varible name "Pertussis_Mean" to be used for output
Pertussis_Mean <- mean(Pertussis_States_highest$percentage_sick)


#Create a varibable name of Polio_Only which will be used to View/Calculate further only that disease
Polio_Only <- subset(Sicker, disease == "Polio")

#Calculate the Polio disease per State (by the highest percentage_sick)   
Polio_States_highest <- ddply(Polio_Only, 'state', function(x) x[x$percentage_sick==max(x$percentage_sick),])

#Calculate the mean of the highest Polio disease per state (by the highest percentage_sick) 
mean(Polio_States_highest$percentage_sick)

#Create a varible name "Pertussis_Mean" to be used for output
Polio_Mean <- mean(Polio_States_highest$percentage_sick)


#Create a varibable name of Rubella_Only which will be used to View/Calculate further only that disease
Rubella_Only <- subset(sick, disease == "Rubella")

#Calculate the Rubella disease per State (by the highest percentage_sick)   
Rubella_States_highest <- ddply(Rubella_Only, 'state', function(x) x[x$percentage_sick==max(x$percentage_sick),])

#Calculate the mean of the highest Rubella disease per state (by the highest percentage_sick) 
mean(Rubella_States_highest$percentage_sick)

#Create a varible name "Rubella_Mean" to be used for output
Rubella_Mean <- mean(Rubella_States_highest$percentage_sick)


#Create a varibable name of Smallpox_Only which will be used to View/Calculate further only that disease
Smallpox_Only <- subset(Sicker, disease == "Smallpox")

filter(Smallpox_Only, !is.na(weeks_reporting))
Smallpox_weeks_reporting <- filter(Smallpox_Only, !is.na(weeks_reporting))

filter(Smallpox_Only, !is.na(count))
Smallpox_count <- filter(Smallpox_Only, !is.na(count))

filter(Smallpox_Only, !is.na(population))
Smallpox_population <- filter(Smallpox_Only, !is.na(count))


Smallpox_percentage_sick <- filter(Smallpox_Only, !is.na(percentage_sick))




#Calculate the Smallpox disease per State (by the highest percentage_sick)   
Smallpox_States_highest <- ddply(Smallpox_Only, 'state', function(x) x[x$percentage_sick==max(x$percentage_sick),])

#Calculate the mean of the highest Smallpox disease per state (by the highest percentage_sick) 
mean(Smallpox_States_highest$percentage_sick)


#Create a varible name "Smallpox_Mean" to be used for output
Smallpox_Mean <- mean(Smallpox_States_highest$percentage_sick)


#View the top disease per State and show the percentage_sick
#View(States_highest_sick)
    

#Creating a table that shows all the Highest_Mean per Disease lined up
Total_Highest_Mean <- cbind(Hep_A_Mean, Measles_Mean, Mumps_Mean, Pertussis_Mean, Polio_Mean, Rubella_Mean, Smallpox_Mean)


View(States_highest_sick)

Combined_PDF = "C:/Users/micha/Desktop/DevLeague Begins Nov 7 2017/Project_Sprint_5/Combined_PDF.pdf"

pdf(file = Combined_PDF)


#Creating the labels which will be at the bottom of the Highest Mean Score for the bar graphs
labels <- list('Hepatitis_A', 'Measles', 'Mumps', 'Pertussis', 'Polio', 'Rubella', 'Smallpox')

#Creating the barplot


barplot(Total_Highest_Mean,names.arg=labels, beside = TRUE,col = "turquoise", border= "red", main="Highest_Mean_Per_Disease",ylab="Mean",las=2)


#aov(dependent~as.factor(independent1) * as.factor(independent2) ,data=filename)


#The below line did something useful...!!!
#aov(percentage_sick~as.factor(state)* as.factor(disease),data = Sicker)  

#aov(percentage_sick~as.factor(disease)* as.factor(state),data = Sicker)  

 

#aov(percentage_sick~as.factor(state)* as.factor(disease),data = Sicker)

dev.off()

#This is writing a PDF file called Total_Highest_Mean to my folder and displaying it on my screen
dev.print(pdf, 'Total_Highest_Mean.pdf')
#dev.print(pdf, 'For_Print_out_Highest_States.pdf')



# Plot weight by group and color by group
library("ggpubr")

ggboxplot(Sicker, x = "disease", y = "percentage_sick",
          color = "disease", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#00AF00", "#E7B855", "#FC4E44", "#00AF45"),
          order = c("Hepatitis A", "Measles", "Mumps", "Pertussis", "Polio", "Rubella", "Smallpox"),
          ylab = "Mean", xlab = "Disease")


res.aov <- aov(percentage_sick ~ disease, data = Sicker)

summary(res.aov)
TukeyHSD(res.aov)

