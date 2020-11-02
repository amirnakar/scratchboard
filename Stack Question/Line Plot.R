#### Filtering using R to a specific date range ####
# From: https://stackoverflow.com/questions/62926802/filtering-using-r-to-a-specific-date-range

# First, the data I took by copy and pasting from here: 
# https://stackoverflow.com/questions/63006201/mapping-sample-data-to-actual-csv-data
# and saved it as bookdata.csv with Excel
library(tidyverse)                                    # This will import some functions that you need, spcifically %>% and ggplot


setwd("C:/Users/di58lag/Documents/scratchboard/Scratchboard")
data = read.csv("librarydata.CSV") # Read the data into R

#head(data)                                            # Quality control, looks good
#colnames(data)
data = data %>% select(city,dates) # As city and classes are the same, only took these 2
data$dates = as.Date(data$dates, format = "%d-%b-%y") # This formats the date as dates for R

# Step 0: look that the data makes sense to you
summary(data$dates)
summary(data$city)
#summary(data$classes)
#table(data$city == data$classes)
#head(data)

# Step 1: filter the right data
start.date = as.Date("2015-01-01")
end.date   = as.Date("2015-06-30")

filtered = data %>% 
  filter(dates >= start.date & 
         dates <= end.date) # This will only take rows between those dates

head(filtered)
dim(filtered)

# Step 1.5: Counting the values
data.table = as.data.frame(table(filtered)) # This calculates the frequency of each date+location combination
data.table = data.table %>% filter(Freq>0)  # This is used to cut out any Freq=0 values (you don't want to plot cases where no event occured)
data.table$dates = as.Date(data.table$dates) # You need to rerun the "as.Date" func because it formats the dates back to "Factors"

#Quality control:
dim(filtered)   # Gives you the size of the dataframe before the counting
dim(data.table) # Gives the size after the counting
summary(data.table) # Will give you a summary of how many values are for each city, what is the date range and what is the Frequency range

# Now you can create the plot with ggplot:
# Notes: 
# I added geom_point() so that each X value gets a point. 
# I think it's easier to read. You can remove this if you like
# Also added color, because I like it, feel free to delete

Plot = ggplot(data.table, aes(x=dates, y=Freq, group = city)) + geom_line(aes(linetype=city, color = city)) + geom_point(aes(color=city))
Plot

# For a clean version of the plot:
clean.plot = ggplot(filtered, aes(x=dates, y=classes, group = city)) + geom_line(aes(linetype=city))
clean.plot
