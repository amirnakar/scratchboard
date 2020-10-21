### Code to add colums to metadata file ###
# Written by Amir Nakar 26/05/2020
# The goal is to add 2 columns to my metadata, which will be based on the column "type".
# I want to use this for running a model which batches together different genera as "cluster 1"


## Part 1: upload the file
Path = "C:/Users/di58lag/Desktop/CARBATECH Summary/Data/EnteroP"  # Where you keep the metadatafile
data = read.csv(paste(Path, "metadata.csv", sep = "/"))                  # Reads the data in, under the name "data"

# Quality control:
head(data)
colnames(data)
summary(data$type)

###

## Part 2: Install tidyverse
# I need these for the command "fct_collapse"

install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
library(dplyr)
library(forcats)

###

##Part 3: make new columns

# This will creat a new column called "ClusterB", which will give me 3 labels for the model: 
      # 1. Eschirichia/Shigella
      # 2. Enterobacter
      # 3. All others
data$Clusters = fct_collapse(data$type,
                                      Group1 = c("Citrobacter", "Salmonella", "serretia"),
                                      Group2 = c("Enterobacter","Proteus", "Shigella"),
                                      Group3 = c("Escherichia", "Klebsiella")
                            )
summary(data$Clusters)


# This will do the same for a column called "ClusterA", with these labels: 
      #1. Cluster1EnShEs
      #2. Cluster2CiSa
      #3. Cluster3SeKlPr
data$ClusterA = fct_collapse(data$type,
                             Cluster1EnShEs = c("Escherichia", "Shigella", "Enterobacter"),
                             Cluster2CiSa = c("Citrobacter", "Salmonella"),
                             Cluster3SeKlPr = c("Klebsiella", "Proteus", "serretia")
)

summary(data$ClusterA)

colnames(data)[1] = "file"
## Finally, export the file as .csv
write.csv(data, paste(Path, "metadata.csv", sep = "/"), row.names=FALSE, quote=FALSE)
