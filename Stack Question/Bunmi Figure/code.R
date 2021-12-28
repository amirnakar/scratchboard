### Load Libraries
library(readxl)
library(tidyverse)
library(RColorBrewer)

### Read Data
pred_DA_svm <- read_excel("Stack Question/Bunmi Figure/pred DA svm.xlsx", sheet = "Sheet3")
head(pred_DA_svm)
data = pred_DA_svm

data$Conc.Cat = as.character(data$Conc)
colnames(data)

### Plot Data

plot = data %>% 
  ggplot(aes(                             # Part 1: what are the axis
    x=Class,
    y=DS, 
    col=Conc.Cat, 
    fill = Conc.Cat)) +
  scale_fill_brewer(palette="Greens") +  # Part 2: Define the color pallete
  scale_color_brewer(palette="Greens") + # Part 2: Define the color pallete
 
  geom_dotplot(                          # Part 3: Make the dot plot
               binaxis = "y",         # which axis to bin along
               binwidth = 0.05,       # Minimal difference considered diffeerent
               stackdir = "center",   # Centered
               dotsize = 0.5          # Dot size
              ) + 
  stat_summary(                         # Part 4: Make a crossbar at the mean
    fun = mean, 
    fun.min = mean, 
    fun.max = mean, 
    geom = "crossbar",
    size = 0.3) +                      # Line thickness
  
  labs(                                  # Part 4: Change Legend Title
    fill="Concentration", 
    color = "Concentration")

### Display the plot
plot