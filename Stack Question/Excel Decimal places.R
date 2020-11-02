#### SA: R changing excel values during importing with read_excel
# https://stackoverflow.com/questions/63127910/r-changing-excel-values-during-importing-with-read-excel

####
# I am importing an excel file into R, however, 
# some of the values are changing from the original value by adding
# a significant number of decimal places.
#
# i.e The original value may be 77.21 but is changed to 72.209999999999994. 
# I figure it has something to do with floating point numbers?
# I know this example would round back to 72.21, however, sometimes it is occurring on much smaller numbers.
# I also need to ensure the reported value in the excel sheet is what is
# getting imported.
#
# Does anyone have ideas how to manage this?
####


# STEP 1: IMPORT LIBRARIES:
library(readxl) # Needed for 'read_excel'
library(dplyr)  # Needed for manipulation

# Step 2: Import data as dataframe:
data.unformatted = as.data.frame(read_excel("C:/Users/di58lag/Documents/scratchboard/Scratchboard/Stack Questions/sample numbers.xlsx"))

# Step 3: round the numbers to 2 digits:
data.only.2.dig = data.unformatted %>% 
  mutate_if(is.numeric, round, digits=2)

# Alternatively, step 2+3 in one line:
data = as.data.frame(
  read_excel(
    "C:/Users/di58lag/Documents/scratchboard/Scratchboard/Stack Questions/sample numbers.xlsx")
) %>% 
  mutate_if(is.numeric, round, digits=2)

#OUTPUT:
# Notice that it only affects the Num column, which is the 6th
head(data.unformatted) 
head(data.only.2.dig)