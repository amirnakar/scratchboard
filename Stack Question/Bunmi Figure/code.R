library(readxl)
pred_DA_svm <- read_excel("Stack Question/Bunmi Figure/pred DA svm.xlsx", sheet = "Sheet3")
View(pred_DA_svm)

head(pred_DA_svm)
data = pred_DA_svm

library(tidyverse)

data$Conc.Cat = as.character(data$Conc)
colnames(data)
data %>% 
  ggplot(aes(x=Class,y=DS)) +
  geom_boxplot(aes(fill = Conc)) +
  geom_jitter(aes(fill = Conc)) 
 # geom_dotplot(aes(x=Class,y=DS), method="histodot",stackgroups=TRUE, binwidth = 1)
