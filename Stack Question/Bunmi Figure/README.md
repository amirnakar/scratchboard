Bunmi Figures
================
Amir Nakar
12/28/2021

`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax
for authoring HTML, PDF, and MS Word documents. For more details on
using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that
includes both content as well as the output of any embedded R code
chunks within the document. You can embed an R code chunk like this:

\`\`\`{r cars} library(readxl) library(tidyverse) library(RColorBrewer)
pred_DA_svm \<- read_excel(“Stack Question/Bunmi Figure/pred DA
svm.xlsx”, sheet = “Sheet3”) #head(pred_DA_svm) data = pred_DA_svm

data*C**o**n**c*.*C**a**t* = *a**s*.*c**h**a**r**a**c**t**e**r*(*d**a**t**a*Conc)
colnames(data) plot = data %>% ggplot(aes( x=Class,y=DS, col=Conc.Cat,
fill = Conc.Cat)) + scale_fill_brewer(palette=“Greens”) +
scale_color_brewer(palette=“Greens”) + #theme(panel.background =
“black”) + stat_summary( fun = mean, fun.min = mean, fun.max = mean,
geom = “crossbar”, size = 0.3) + geom_dotplot(, binaxis = “y”, # which
axis to bin along binwidth = 0.05, # Minimal difference considered
diffeerent stackdir = “center”, # Centered dotsize = 0.5 )


    ## Including Plots

    You can also embed plots, for example:

    ```{r, echo=FALSE}
    plot
