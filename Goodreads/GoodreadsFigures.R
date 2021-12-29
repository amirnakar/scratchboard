library(tidyverse)
library(readr)
library(lubridate)
library(emojifont)

### Read the data
url = "https://raw.githubusercontent.com/amirnakar/scratchboard/master/Goodreads/goodreads_library_export.csv"
local.path = "Goodreads/goodreads_library_export.csv"

goodreads_library_export <- read_csv(url) #read_csv("Goodreads/goodreads_library_export.csv")

# Defines the "Date Read" column as dates
goodreads_library_export$`Date Read` = as.Date(goodreads_library_export$`Date Read`, format= "%Y-%m-%d")

# Simpler variable name
full = goodreads_library_export


### Make a bar graph on number of books per year

# Add a column of just the years
full$Year.Read = year(full$`Date Read`)

# Specific data to plot:
data = full %>%
  group_by(Year.Read)%>%       # This line is needed for the summary
  filter(Year.Read>=2010) %>%  # Only after 2021
  summarise(freq=n())          # "freq" is the number of books per year


# Plot with ggplot:
Plot = 
  data %>% 
  ggplot(aes(x=Year.Read, y=freq, fill = (freq))) + # Define the variables
  geom_bar(stat = "identity") +
  
  # Add text labels
  labs(                                             
    x = "Year",
    y = "Books Read ",
    title = "Books read per year, since 2010",
    subtitle = "Source: Goodreads.com (amirnakar)",
    fill = (emoji('books'))
    ) + 
  
  # Define the theme (Style)
   theme_minimal() +                                                         
   theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.title=element_text(size=20, face = "bold"),
    axis.title=element_text(size=20)
         ) +
  scale_fill_gradient(low = "#FFFB00", high = "#6B3A00") + 
  ylim(0,26) + 
  
  # Add text labels on top of figure
  geom_text(aes(label=(freq), vjust=-1, hjust = 1)) + 
  geom_text(aes(label=emoji('books'), vjust=-1, hjust = 0))

Plot

### Save picture
### Save the plot
jpeg("Goodreads/Books Per Year.jpg") # Open the device

# Code of the plot
Plot

# Close the graphics device
dev.off() 



