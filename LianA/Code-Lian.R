#####
# This is a script to check correlations between many variables for a friend.
# The data has ~20 rows (which indicate samples) and ~30 columns (which are the measured variables)
# The goal is to be able to spot any correlations, change the alpha level and also find non-linear correlations

### STEP 0: Import libraries

# These are some prewritten commands for later

Packages = c("Hmisc", "reshape2", "ggplot2", "tidyverse")
install.packages(Packages)

library(reshape2)
library(ggplot2)
library(Hmisc) 
library(tidyverse)
library(car)
library(caret)
library(readxl)


### STEP 1: Importing the data ###

Path = "C:/Users/di58lag/Desktop/PhD/R/Scratchboard/Lian"  # Where you keep the datafile and later export results
#data = read.csv(paste(Path, "bomregions.csv", sep = "/"))  # Reads the data in, under the name "data"
data <- as.data.frame(read_excel(paste(Path, "Results.xlsx", sep = "/"), sheet = 1), stringsAsFactors = TRUE)

# Quality control
head(data) # Shows the first few rows
dim(data)  # Shows the size of the data

colnames(data)[13:26] = data[1,13:26]

# cuttind things off to simplify
data = data[2:16, ]
data$co2mlo = NULL
data$name = 1:10
data

# STEP 2: Basic numbers and summary

summary(data)      # Gives you min, max, quantiles, mean and median of all columns
summary(data$swRain) # Does the same but only a specific column

# STEP 3: Linear correlations
# cormat = rcorr(as.matrix(data[,4:231]), type = "pearson")    # Calculates correlations and pVal of correlations on data. You can change to spearman if you prefer
cormat = rcorr(as.matrix(data), type = "pearson")    # Calculates correlations and pVal of correlations on data. You can change to spearman if you prefer 
cormat$sq = cormat$r^2
#To look at the numbers:
cormat$r
cormat$P
cormat$sq




# In graphical form:
### Correlation (r)
melted_cormat.r <- melt(cormat$r)  # Makes it readable for ggplot

# Makes the graph
Heatmap.r = ggplot(data = melted_cormat.r, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+ theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation")

Heatmap.r

### p.values:
alpha = 0.05
melted_cormat.p <- melt(cormat$P)  # Makes it readable for ggplot
melted_cormat.p$significance = melted_cormat.p$value<alpha

Heatmap.p = ggplot(data = melted_cormat.p, aes(Var2, Var1, fill = significance))+
  geom_tile() + theme(axis.text.x = element_text(angle = 90))

    #color = "white")+
  #scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
   #                    midpoint = alpha, limit = c(0,0.5), space = "Lab", 
    #                   name="pValue")

Heatmap.p

## As a list of significant correlations: 
List.of.sig.corr = cbind(melted_cormat.r,melted_cormat.p$value)
colnames(List.of.sig.corr) = c("var1", "var2", "r", "pvalue")
head(List.of.sig.corr)
List.of.sig.corr = List.of.sig.corr %>%      
  mutate(rsq = r^2)


filtered = List.of.sig.corr %>%
  filter(pvalue<0.05 & rsq>0.5)

back = List.of.sig.corr %>% filter(var1 == Back)


dim(filtered)

write.csv(filtered, file = paste(Path, "filtered-all.csv", sep = "/"))

### STEP 4: Tests for Normality, heterogeneity etc.
## Normal distribution
# Option one: the eye test, plot and look
ggplot(data=data, aes(x=swRain)) + geom_density()

# Option two: shapiro test
shapiro.test(data$swRain)$p.value   # Test if the data is normally distributed. If p>alpha = non-normal distribution

## Equal Variances
# For normally distributed data
bartlett.test(data)$p.value         # Test if the different groups are homogenous or not. If p>alpha = 

# For non-normally distributed data
leveneTest(southRain ~ name, data = data)

### STEP 5: looking for non-linear correlations
# Option one: plotting
plot(data$southRain, data$swRain) # Two specific variables

plot(data)                        # All at once

# Option two: Dimension Reduction (PCA)

pca <- prcomp(as.matrix(data[1:15,10:100]), center=F, scale.=F)

plot(pca$x[,1], pca$x[,2], pch = as.character(1:10)) # This way you can see how the samples are distributed
plot(pca$rotation[,1],pca$rotation[,2], pch = as.character(1:10))


# Option three: calculating non linear correlations

wilcox.test(data)

### Sources: 
# http://www.sthda.com/english/wiki/reading-data-from-excel-files-xls-xlsx-into-r
# http://www.sthda.com/english/articles/40-regression-analysis/168-multiple-linear-regression-in-r/
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
# http://www.sthda.com/english/wiki/compare-multiple-sample-variances-in-r
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/