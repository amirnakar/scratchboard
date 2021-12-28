library(tidyverse)
library(readr)
goodreads_library_export <- read_csv("Goodreads/goodreads_library_export.csv")
View(goodreads_library_export)

goodreads_library_export$`Date Read`

read = goodreads_library_export %>% filter(grep(x = "`Date Read`", pattern = "2021"))
# Make a bar graph on number of books per year

