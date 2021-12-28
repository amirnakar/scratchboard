Making a figure like MeMeds
================

`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

# What I want to get

I want to get a figure that looks like this one from

[A Novel Host-Proteome Signature for Distinguishing between Acute
Bacterial and Viral Infections Oved K, Cohen A, Boico O, Navon R,
Friedman T, et al. (2015) A Novel Host-Proteome Signature for
Distinguishing between Acute Bacterial and Viral Infections. PLOS ONE
10(3): e0120012.](https://doi.org/10.1371/journal.pone.0120012)

![Figure](https://storage.googleapis.com/plos-corpus-prod/10.1371/journal.pone.0120012/1/pone.0120012.g002.PNG_L?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=wombat-sa%40plos-prod.iam.gserviceaccount.com%2F20211225%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20211225T124521Z&X-Goog-Expires=86400&X-Goog-SignedHeaders=host&X-Goog-Signature=7b9122e2f0e559ef5d73368247be00a545b1a88b0bf67b19d7a05a2416975bd68a902b131448817e0aca7beba95e39bd962052d14587b13b5dbd9409b21609733390a1dbe176d00717d7e69d150afdcc75c22ec4230de92c0b546a4238cd55ea922befdfea294870f57b77b3e18608513ca891c36eab6d354dfd76dd8b34b85440d9e2384333d29dba5b60487c3c1619303b7a06a391d26dc94bcb439ddb8b4ff12adf52b67fbbdb703b62e9b6afc6f7d415982d86fc51aef05dc690fce37b1d0f0e27d9dcc1e373f81ca9cf58f58aa1496687e60f5fad474977a234932ffae34856bb28255a23a52de84d3c47db71fccc01b4976cb96f5989c2af728710f212)

# Data
In theory, I want to use the data from my own work.
X axis = Method (SC-RMS / FP-RS)
Y Axis = intensity of two signals: Amide I and CH2/CH3

![Figure](https://keep.google.com/u/0/media/v2/1k8A_P0y9PZmG2c996HvaYqxYLQbHPTEgrbOaJlU4i3cxGrdFDRqw0mE92zxqGNBvvISB/1OYK5wY2OVaYyEu4qtY9oydScqc_B85j8gNaGxiuFwcxUcqLBRoU0CeCu-yZhgZtTUopo/fa53561b-f6eb-40e0-b7ee-c4b16ca3ee54?accept=image%2Fgif%2Cimage%2Fjpeg%2Cimage%2Fjpg%2Cimage%2Fpng%2Cimage%2Fwebp%2Caudio%2Faac)

But I'm not sure about this data yet...

## Fake data: 

``` {r}
library(tidyverse)
patient = data.frame(replicate(3,runif(100, min=5, max=10))) %>% 
  pivot_longer(cols = c("X1","X2","X3"))


# Fiber
data.FP = read.csv(file = "C:/Users/di58lag/Desktop/PhD/Projects/EnteroP/Entero Paper/Data and Figures/NIR - Data, Parameters, Results/NIR-Results-Classification/preprocessed.csv",
                    sep = ",")
data.FP.Amide1 = data.viz$X1657/max(data.FP.Amide1)

plot(data.FP.Amide1)

max(data.FP.Amide1)

# SC-RMS (532)
data.SC = read.csv(file = "C:/Users/di58lag/Desktop/PhD/Projects/EnteroP/Entero Paper/Data and Figures/BPE - Data, Parameters, Results/Results - EnteroP - BPE (LDA 30)/preprocessed.csv",
                      sep = ";")
head(data.SC)

data.SC.Amide1 = data.SC$X1664

plot(data.SC.Amide1)

length(data.FP.Amide1) # 1250
length(data.SC.Amide1) # 5048
dif = 5048-1250        #3798

data.FP.Amide1 = c(data.FP.Amide1, rep(NA,3798))

Amide1 = data.frame(data.FP.Amide1, data.SC.Amide1)
boxplot(Amide1)

Amide1

colnames(Amide1) = c("FP", "SC")

longer = Amide1 %>% pivot_longer(cols = colnames(Amide1))
patient = data.frame(replicate(3,sample(0:300,300,rep=TRUE))) #%>% 
  pivot_longer(cols = c("X1","X2","X3"))



colnames(longer)

longer %>% 
  ggplot(aes(x=name,y=value))+
  geom_boxplot()+
  ggdist::stat_dots(alpha=.5)


patient %>% 
  ggplot(aes(x=name,y=value))+
  geom_boxplot()+
  geom_jitter()


head(patient)
head(cars)
head(mpg)



install.packages("ggbeeswarm")
library(ggbeeswarm)

# Beeswarm plot in ggplot2
ggplot(longer, aes(x = name, y = value)) +
    geom_boxplot(col = "blue", varwidth = T) + 
  geom_beeswarm(cex = 3, shape = 1)



```

# Plotting with ggplot



## GitHub Documents

This is an R Markdown format used for publishing markdown documents to
GitHub. When you click the **Knit** button all R code chunks are run and
a markdown file (.md) suitable for publishing to GitHub is generated.

## Including Code

You can include R code in the document as follows:

`{r cars} summary(cars)`

## Including Plots

You can also embed plots, for example:

`{r pressure, echo=FALSE} plot(pressure)`

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
