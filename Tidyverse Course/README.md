# R tidyverse course

This was written in 27-28 of April 2020

# summary

| **n** | **function** | **notes** |
| --- | --- | --- |
| 1 | filter | To filter specific **rows**. Just like EXCEL |
| 2 | select | To select specific **columns** |
| 3 | arrange | To sort by a specific column. Default is from min to max |
| 4 | mutate | To add columns with some function |
| 5 | group\_by and summarize | To get summary statistics, for the entire data or by group(similiar to filter) |

# Intro:

I keep seeing Tidyverse everywhere and how it makes manipulating dataframes easier, so I decided to learn about it and found some short videos. Here&#39;s my summary

# Topics:

## Part 1 - understanding data frames

1. Vectors, dataframes and lists
2. Working Directories
3. Saving &amp; loading data

## Part 2 - Tidyverse and dplyr

1. The Pipe " %>% "
2. The 5 verbs of dplyr
3. Understanding group by

Keyboard shortcut for "%>%" is cntrl+shift+m

# Understanding data frames

## 1. Vectors, dataframes and lists
<<<<<<< HEAD
Video: https://www.youtube.com/watch?v=XHAm_V-KZE8&list=PLLxj8fULvXwGOf8uHlL4Tr62oXSB5k_in&index=4



=======

Video: 
[https://www.youtube.com/watch?v=XHAm\_V-KZE8&amp;list=PLLxj8fULvXwGOf8uHlL4Tr62oXSB5k\_in&amp;index=4](https://www.youtube.com/watch?v=XHAm_V-KZE8&amp;list=PLLxj8fULvXwGOf8uHlL4Tr62oXSB5k_in&amp;index=4)
```{r}
library(tidyverse)
 mtcars
 class(mtcars)

# a dataframe is

# Create a datafram from vectors
 x = c(4, 7, 4, 2)
 y = 1:4
 data = data\_frame(x,y)

data

data$x

# if the vectors are not the same size, you will run into problems.
# Instead, you can use a list

w = c(4, 5, 2,1)
 z = 1:10

my\_list = list(w,z)

my\_list
```
## 2. Working Directories

video: [https://www.youtube.com/watch?v=lWe7sMmynJk&amp;list=PLLxj8fULvXwGOf8uHlL4Tr62oXSB5k\_in&amp;index=5](https://www.youtube.com/watch?v=lWe7sMmynJk&amp;list=PLLxj8fULvXwGOf8uHlL4Tr62oXSB5k_in&amp;index=5)

A working directory is where you pull your data from and export your stuff to.

You can do that from the top-bar: session %>% set working directory %>% Choose directoy

```{r}
# To get the working directory:
 getwd()

# To change the working directory:
 wd.path = "C:/Users/anakar/Desktop/Scratchboard/Tidyverse Course/"
 setwd(wd.path)
```
## 3. Saving &amp; loading data

Note: I skipped the part about csv files My notes start here [https://youtu.be/Y3Nzz6WEl\_E?t=200](https://youtu.be/Y3Nzz6WEl_E?t=200)

```{r}
#Saving dataframes as csvs:
 data = mtcars

class(data)
 write.csv(data, "mtcars.csv")
### This just made a new file in my wd called "mtcars.csv"

#Now let&#39;s read it:
 import.data = read.csv("mtcars.csv")

# But what if your data is NOT a dataframe?
# You can save it as an R readable file: .rds

Plot = plot(1:10)

saveRDS(Plot, "plot.rds")

import.rds = readRDS("plot.rds")

### How to save everything, all the stuff in your environment

save.image("tidyverse.Rdata")

load("tidyverse.Rdata")
```
# Tidyverse and dplyr

## 1. The Pipe " %>% "

Video: [https://www.youtube.com/watch?v=9yjhxvu-pDg&amp;list=PLLxj8fULvXwGOf8uHlL4Tr62oXSB5k\_in&amp;index=3](https://www.youtube.com/watch?v=9yjhxvu-pDg&amp;list=PLLxj8fULvXwGOf8uHlL4Tr62oXSB5k_in&amp;index=3)

```{r}
# First let&#39;s make some random data\_frame

x = c(4, 7, 4, 2)
 y = 1:4
 data = data\_frame(x,y)

data

# Looking at the data "classicly":

colSums(data)
 mean(colSums(data))

# With the pipe:
 data %>% colSums() %>% mean()

#It&#39;s just a bit cleaner. Looks even better like this:
 data %>%
 colSums() %>%
 mean()
```

### Keyboard shortcut for "%>%" is cntrl+shift+m

## 2. The 5 verbs of dplyr

[https://www.youtube.com/watch?v=sVISY\_27znA&amp;list=PLLxj8fULvXwGOf8uHlL4Tr62oXSB5k\_in&amp;index=7](https://www.youtube.com/watch?v=sVISY_27znA&amp;list=PLLxj8fULvXwGOf8uHlL4Tr62oXSB5k_in&amp;index=7)

This is all about using dyplyr

## summary

| **n** | **function** | **notes** |
| --- | --- | --- |
| 1 | filter | To filter specific **rows**. Just like EXCEL |
| 2 | select | To select specific **columns** |
| 3 | arrange | To sort by a specific column. Default is from min to max |
| 4 | mutate | To add columns with some function |
| 5 | group\_by and summarize | To get summary statistics, for the entire data or by group(similiar to filter) |

```{r}
mtcars = mtcars[1:10,]
 mtcars$name = rownames(mtcars)
 mtcars
```
### 1. Filter [specific rows]
```{r}
mtcars %>%
 filter(cyl == 6) # Only take cars with 6 cylinder

# And / Or clauses:
 mtcars %>%
 filter(cyl == 6 | mpg>21) # Only take cars with 6 cylinder OR 21+ mpg

mtcars %>%
 filter(cyl == 6 &amp; mpg>21) # Only take cars with 6 cylinderAND 21+ mpg
```
### 2. Select [specific columns]
```{r}
# to take only specific columns
 mtcars %>%
 select(name)

# to rearange the columns:

mtcars %>%
 select(name, cyl, mpg)

mtcars %>%
 select(name, mpg, cyl)

# To bring one thing to the front of everything else:
 mtcars %>%
 select(name, everything())
 mtcars = mtcars %>% select(name, everything())
```
### 3. arrange [sort]
```{r}
mtcars %>% #From min to max
 arrange(mpg)

mtcars %>% #From max to min
 arrange(desc(mpg))
# Also works with alphabetical

# multiple arrange:
 mtcars %>% #First by cylinder, then by mpg
 arrange(cyl, mpg)
```
### 4. mutate [add columns]
```{r}
# make a new column from mpg\*cyl

mtcars = mtcars %>%
 mutate(mpg\_times\_cyl = mpg\*cyl)

# multiple columns, seperate by comma:
 mtcars %>% #From min to max
 mutate(random = 1:10,
 random2 = 10:1) %>%
 select(name, random, random2) #Added this just for viewing
```
### 5. group\_by / summarize ###
```{r}
mtcars %>%
 group\_by(cyl) %>%
 summarize(mean(mpg))

# To change the column name for the summary:

#Summarize the whole data:
 mtcars %>%
 summarize(mean(mpg))

mtcars %>%
 summarize(mean(mpg),
 n(),
 sd(mpg),
 median(disp)
 )
# You can also get: min, max

# To change the column names:
 mtcars %>%
 summarize(colname = mean(mpg),
 number.of.samples = n(),
 standard.deviation.for.disp = sd(disp))

# Grouping the data

mtcars %>%
 group\_by(cyl)

# This actually does nothing

# But if you want to get a summary by group:
 mtcars %>%
 group\_by(cyl) %>%
 summarize(mean(mpg))
```
### 6. Combining with the pipe
```{r}
mtcars %>%
 select(name, mpg, cyl) %>%
 filter(mpg \&lt; 20) %>%
 mutate(km = floor(mpg/1.6)) %>%
 arrange(cyl,name)
```

## 3. Understanding group by

[https://www.youtube.com/watch?v=6xbGZDUu5W4&amp;list=PLLxj8fULvXwGOf8uHlL4Tr62oXSB5k\_in&amp;index=8](https://www.youtube.com/watch?v=6xbGZDUu5W4&amp;list=PLLxj8fULvXwGOf8uHlL4Tr62oXSB5k_in&amp;index=8)

```{r}

library(tidyverse)

data = tribble(~name, ~score,
"Jen", 8,
"Jen", 10,
"Sarah", 5,
"Sarah", 7,
"Ben", 4,
"Ben", 5,
"Ben", 6,)

 data

data %>% filter(score>6)
 data %>% mutate(factor = score+1)
 data %>% summarise(mean(score), sd(score))

# Group\_by

data %>%
 group\_by(name)
# It looks like nothing happend, but actually it did make an internal split

data %>%
 group\_by(name) %>%
 summarize(mean(score), sd(score))

data %>%
 group\_by(name) %>%
 mutate(mean = mean(score)) # We get for each row the mean for that specific group! so for Jen, the mean is 9 and for Sarah the mean is 6

data %>%
 group\_by(name) %>%
 mutate(mean = mean(score)) %>%
 filter(mean>7)
```
>>>>>>> 372af10d5b4b7aaddb3af5f7d245b39b0254ce42

