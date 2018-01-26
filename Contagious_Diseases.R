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

                                    
                                    
filter(sick, !is.na(percentage_sick))
                                    
Sicker <- filter(sick, !is.na(percentage_sick))
                                   
 #In order to give me the highest percentage_sick for each state, still need to now figure                              #out how to get the rest of the columns back

                                    
States_highest_sick <- ddply(Sicker, 'state', function(x) x[x$percentage_sick==max(x$percentage_sick),])
                                    View(States_highest_sick)