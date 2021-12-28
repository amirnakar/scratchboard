library(readxl)
pred_DA_svm <- read_excel("Stack Question/Bunmi Figure/pred DA svm.xlsx", sheet = "Sheet3")
View(pred_DA_svm)

head(pred_DA_svm)
data = pred_DA_svm

library(tidyverse)

data$Conc.Cat = as.character(data$Conc)
colnames(data)
data %>% 
  ggplot(aes(
    x=Class,y=DS, col=Conc.Cat, fill = Conc.Cat)) +
  scale_fill_brewer(palette="Greens") + 
  scale_color_brewer(palette="Greens") + 
  #theme(panel.background = "black") + 
  stat_summary(
    fun = mean, fun.min = mean, fun.max = mean, 
    geom = "crossbar", 
    size = 0.3) + 
  geom_dotplot(,
               binaxis = "y",         # which axis to bin along
               binwidth = 0.05,       # Minimal difference considered diffeerent
               stackdir = "center",   # Centered
               dotsize = 0.5
  )


 # geom_dotplot(aes(x=Class,y=DS), method="histodot",stackgroups=TRUE, binwidth = 1)

library(RColorBrewer)
display.brewer.all()



p= data %>% 
  ggplot(aes(x=Class,y=DS)) +
  geom_point(size=3) +
  stat_summary(
    fun.y = mean, fun.ymin = mean, fun.ymax = mean, 
    geom = "crossbar", 
    color = "blue",
    size = 0.3)

