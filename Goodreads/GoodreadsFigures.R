library(RColorBrewer)
library(tidyverse)
library(readr)
library(lubridate)
library(emojifont)

goodreads_library_export <- read_csv("Goodreads/goodreads_library_export.csv")
View(goodreads_library_export)

goodreads_library_export$`Date Read` = as.Date(goodreads_library_export$`Date Read`, format= "%Y-%m-%d")
full = goodreads_library_export

# Make a bar graph on number of books per year

full$Year.Read = year(full$`Date Read`)

data = full %>%
  group_by(Year.Read)%>%
  filter(Year.Read>=2010) %>% 
  summarise(freq=n())

full %>% 
  filter(Year.Read>=2010) %>% 
  ggplot(aes(x=Year.Read)) +
  geom_bar() + 
  labs(
    x = "Year",
    y = "Books Read",
    title = "Books read per year, since 2010",
    subtitle = "Source: Goodreads.com"
  ) + theme_minimal() + 
  geom_text(aes(label=y), vjust=0)

data %>% 
  ggplot(aes(x=Year.Read, y=freq, fill = (freq))) +
  geom_bar(stat = "identity") + 
  labs(
    x = "Year",
    y = "Books Read ",
    title = "Books read per year, since 2010",
    subtitle = "Source: Goodreads.com",
    fill = (emoji('books'))
  ) + theme_minimal() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.title=element_text(size=20, face = "bold"),
    axis.title=element_text(size=20),
  ) +
  geom_text(aes(label=(freq), vjust=-1, hjust = 1)) +
  geom_text(aes(label=emoji('books'), vjust=-1, hjust = 0)) +
  scale_fill_gradient(low = "#FFFB00", high = "#6B3A00")


