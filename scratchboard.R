
#### means stdev ####
#### This script will create for me a means with standev file from the original files ####

path = "C:/Users/anakar/Desktop/Scratchboard/Report - EnteroP/Report - EnteroP by genus - svm-rad-c0"   


### Creating a combined .csv file with the metadata and the spectra data ###
path = "C:/Users/anakar/Desktop/Scratchboard/Report - Differentiation by resistance - 1h"    # The path where the report files are
meta = read.csv(paste(path,"metadata.csv", sep = "/"), header = T, sep = ";")                # loads up the metadata.csv file
Prep = read.csv(paste(path,"preprocessed-adjusted.csv", sep = "/"), header = T, sep = ";")            # loads up the preprocessed.csv file

### this part is needed because somehow there are empty values in the .preprocessed file, 
###I don't know why but it makes the number of rows incorrect otherwise
num.rows.to.take = dim.data.frame(meta)[1]                                                   # the number of real rows is calculated from metadata.cst
num.cols.to.take = dim(prep)[2]                                                              # This is needed to know the number of columns needed to copy


# And now we combine both frames. Taking only the rows and columns with the spectral data from the preprocesses file
combined = as.data.frame(cbind(meta, prep[1:num.rows.to.take, 3:num.cols.to.take]))

# This saves the whole thing as "combined table.csv" in the same folder
write.csv(combined, paste(path, "combined table.csv", sep = "/"))

### I want to get the mean and stdev per group
# 
means = aggregate(combined[, 100:105], list(combined$Genus), mean)
means


combined = read.csv("C:/Users/anakar/Desktop/Scratchboard/Report - ColiP combined/Report - Differentiation by resistance - 1h/combined table.csv")
means = aggregate(combined, list(combined$Genus), mean)
combined[1:5,100:110]
means = NA

is.data.frame(combined)

means = aggregate.data.frame(combined ,as.list(by = combined$Resistant), FUN = mean)
?aggregate.data.frame
means = aggregate.data.frame(combined[ ,100:110],by = as.list(combined$Resistant), FUN = mean)
list((combined$Resistant))
length(combined[ , 100:110])



## ## ## ##  ## ## ## ##  ## ## ## ##  ## ## ## ##  ## ## ## ##  ## ## ## ##  

#### Timer ####
### I want to make a script to create for me this dataframe: 
# start time, end time, time spent, date
# to get the current time you need to use sys.time()
logger = c("date", "start time", "End Time","Time working", "comments")
# now we need to get each time 3 elements: 
     startT    = Sys.time() # Which runs at the START of the work
     EndT      = Sys.time() # Which runs at the END of the work
     TimeElaps = difftime(EndT, startT, units = "hours")
     DateT     = Sys.Date()
     Comments  = "no comments"
       
log = (c(as.character(startT), as.character(EndT), TimeElaps, DateT, "no comments"))
log
Table = as.data.frame(rbind(logger, log))
Table

#When you start to work: 
startT    = Sys.time() # Which runs at the START of the work

# When you finish and save
# To log a new line of work: 

EndT      = Sys.time()                               # Which runs at the END of the work
TimeElaps = difftime(EndT, startT, units = "hours")  # Calculates the time you worked
DateT     = as.character(Sys.Date())                 # returns the date
Comments  = "testrun"                            # You can wrtie any notes here

log = (c(as.character(startT), as.character(EndT), TimeElaps, DateT, Comments))
timelog = read.csv("C:/Users/anakar/Desktop/Scratchboard/Stock prices/Time Log.csv", header = T)[,-1]
timelog = as.data.frame(rbind(timelog,log))
timelog
write.csv(timelog, "C:/Users/anakar/Desktop/Scratchboard/Stock prices/Time Log.csv", row.names = F)


## ## ## ##  ## ## ## ##  ## ## ## ##  ## ## ## ##  ## ## ## ##  ## ## ## ##  ## ## ## ##  ## ## ## ##  ## ## ## ##  ## ## ## ##  ## ## ## ##  


#### Importing Xls files ####

install.packages("readxl")
library("readxl")
?read_xls

index.path = "C:/Users/anakar/Desktop/Scratchboard/Stock prices/Input Data/Indices"
index.files = list.files("C:/Users/anakar/Desktop/Scratchboard/Stock prices/Input Data/Indices")

index.files

df = dow_jones
df$file = "test"

for(i in 1:length(index.files)) {
        filename = index.files[i]
        file = paste(index.path, filename, sep = "/")
        read = as.data.frame(read_xls(file, range = "A8:C1570", col_names = F))
        read[,2] = NULL
        colnames(read) = c("Date", "index")
        read$file = filename
        df = rbind(df,read)
}

head(df)
dim(df)
dim(dow_jones)
df$file = as.factor(df$file)
levels(df$file)
df$name = substr(df$file, start = 1, stop = 5)

list = df$file == "test"
list



dow_jones = as.data.frame(read_xls("C:/Users/anakar/Desktop/Scratchboard/Stock prices/Input Data/Indices/Dow Jones U.S. Index.xls", range = "A8:C1570", col_names = F))
dow_jones[,2] = NULL
colnames(dow_jones) = c("Date", "index")
head(dow_jones)

library(ggplot2)


plot.indices = ggplot(df, aes(x=Date, y=index, col=name)) + geom_point()

plot.indices +theme_bw(base_size = 20) + theme(legend.position="bottom")

dow_jones_plot = ggplot(dow_jones,aes(x=Date,y=index)) + geom_point()
dow_jones_plot

## ## ## ## ## ## ##  ## ## ## ## ## ## ##  ## ## ## ## ## ## ##  ## ## ## ## ## ## ##  ## ## ## ## ## ## ##  ## ## ## ## ## ## ##  


#### A tiemr function which doesn?t reallz work...#####

timer <- function(interval, units) {
        require(beepr)
        t0 <- Sys.time()
        stopwatch <- round(as.double(difftime(Sys.time(), t0, u = units)))
        while(stopwatch < interval){
                stopwatch <- round(as.double(difftime(Sys.time(), t0, u = units)))
        }
        beep(2)
}

timer(1, "mins")

## ## ## ## ## ## ##  ## ## ## ## ## ## ##  ## ## ## ## ## ## ##  ## ## ## ## ## ## ##  ## ## ## ## ## ## ##  ## ## ## ## ## ## ##  


#### Adding a lable in ggplot ####

#### From stack overflow: 
### https://stackoverflow.com/questions/61459272/adding-a-label-in-geom-line-in-r
library(tidyverse)
x = (1997:2013)
y1 = floor(runif(17, min=0, max=10))
y2 = floor(runif(17, min=2000, max=10000))

data = data_frame(x,y1,y2)
colnames(data) = c("year", "frequency", "severity")

sec_plot <- ggplot(data, aes_string (x = x, group = 1)) +
        geom_col(aes_string(y = y1), fill = "orange", alpha = 0.5) +
        geom_line(aes(y = y2))

sec_plot

data



data<-data.frame(year= 2000:2005, frequency=3:8, severity=as.integer(runif(6, 4000, 8000)))

library(ggplot2)
library(scales)

sec_plot <- ggplot(data, aes(x = year)) +
        geom_col(aes(y = frequency, fill = "orange"), alpha = 0.6) +
        geom_line(aes(y = severity/1000, color = "black", group=1)) +
        scale_fill_identity(guide = "legend", label="Claim frequency (Number of paid claims per 100 Insured exposure)", name=NULL) +
        scale_color_identity(guide = "legend", label="Claim Severity (Average insurance payment per claim)", name=NULL) +
        theme(legend.position = "bottom") +
        scale_y_continuous(sec.axis =sec_axis( ~ . *1, labels = label_dollar(scale=1000), name="Severity") ) +  #formats the 2nd axis
        guides(fill = guide_legend(order = 1),  color = guide_legend(order = 2))                                #control which scale plots first

sec_plot




### ###

df = as.data.frame(  cbind(x,y1,y2))
df

plot = ggplot(df, aes(x=x,y=y1, size=y2)) + geom_point()

plot
dim(df
   )
col = c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3)
col
length(col)
shape = c(1:4,1:4,1:4,1:4,1:4)
df = as.data.frame(  cbind(df,col[1:17],shape[1:17]))
colnames(df) = c("x","y1","ysize","ycolor","yshape")
df$yshape = as.factor(df$yshape)
plot = ggplot(df, aes(x=x,y=y1, size=ysize, shape=yshape, col = ycolor )) + geom_point()
plot



### ###

met = read.csv("C:/Users/anakar/Desktop/Send to Oleg/metadata.csv", header = T, sep = ",")
met$type
summary(met$type)
levels(met$type) = levels
levels = c("standard", "CRE", "ESBL", "Non-Pathogenic","Sensitive")
levels
length(levels)
length(levels(met$type))
t(levels(met$type))
met
write.csv(met,"C:/Users/anakar/Desktop/Send to Oleg/metadata.csv")

#### svm ####

### manualSvmCv ###
#      data: Eine Matrix mit Datens?tzen
#    labels: Ein Faktor mit Klassenzugeh?rigkeiten
#    kernel: Ein Kernel f?r die SVM, z.B. "rbfdot" oder "vanilladot"
#      kpar: Eine Liste mit Hyperparametern f?r den Kernel
#     cross: Die Anzahl an Kreuzvalidierungsschritten
#
#     Da mit dem SVM-Modell zwar ein Kreuzvalidierungsfehler, aber keine 
#     Kreuzvalidierungsmatrix zur?ckgegeben wird, kann die Matrix hiermit 
#     'per Hand' berechnet werden. Die Funktion imitiert das Verhalten von
#     ksvm.
####
manualSvmCv<-function(data, labels, kernel, kpar, cost = 1, cross = 10, ...) {
  nr <- nrow(data)
  suppressWarnings(splits<-split(sample(1:nr,nr),1:cross))
  
  pred <- integer(length(labels))
  data <- scale(data)
  for (i in seq_len(cross)) {
    cind <- unsplit(splits[-i],factor(rep((1:cross)[-i],unlist(lapply(splits[-i],length)))))
    
    if (kernel == "vanilladot" || kernel == "splinedot") {
      svmModel <- ksvm(data[cind,],labels[cind], kernel = kernel, C = cost, scaled = FALSE, fit = FALSE, ...)      
    } else {
      stopifnot(!missing(kpar))
      svmModel <- ksvm(data[cind,],labels[cind], kernel = kernel, kpar = kpar, C = cost, scaled = FALSE, fit = FALSE, ...)
    }
    pred[splits[[i]]] <- predict(svmModel, data[splits[[i]],,drop=FALSE])
  }
  pred
}


cost <- 100
system.time(svmModel <- ksvm(Prep[4:10], Prep$label, type="C-svc", cross = 10, C = cost, kernel = "vanilladot"))
svmModel

colnames(combined)
cost=1
system.time(predTrain <- manualSvmCv(Prep[4:10], Prep$label, type="C-svc", kernel = "vanilladot", kparList, cost = cost, cross = 10))
predTrain
svmModel

#### Creating a combined .csv file with the metadata and the spectra data ####
path = "C:/Users/di58lag/Desktop/PCAno1"    # The path where the report files are
meta = read.csv(paste(path,"metadata.csv", sep = "/"), header = T, sep = ";")                # loads up the metadata.csv file
Prep = read.csv(paste(path,"PCA scores.csv", sep = "/"), header = T, sep = ";")            # loads up the preprocessed.csv file

### this part is needed because somehow there are empty values in the .preprocessed file, 
###I don't know why but it makes the number of rows incorrect otherwise
num.rows.to.take = dim.data.frame(meta)[1]                                                   # the number of real rows is calculated from metadata.cst
num.cols.to.take = dim(Prep)[2]                                                              # This is needed to know the number of columns needed to copy


# And now we combine both frames. Taking only the rows and columns with the spectral data from the preprocesses file
combined = as.data.frame(cbind(meta, Prep[1:num.rows.to.take, 3:num.cols.to.take]))

combined = merge(meta, Prep)

# This saves the whole thing as "combined table.csv" in the same folder
write.csv(combined, paste(path, "combined table merge.csv", sep = "/"))


summary(Prep$label)


###Making mean spectra from the combined file ###
#I want to make a new dataframe for each class, so first let's identify the classes:
# In this example, I used split() to seperate combined according to resistance

split.list = split(combined, combined$Resistant)             #creates a list with elements according to the levels of "resistant"
gr.resistant = as.data.frame(split.list[1])
gr.sensitive = as.data.frame(split.list[2])

### Quality control, check number of rows and columns if they fit
A = dim(gr.resistant)
B = dim(gr.sensitive)
C = dim(combined)
A+B
A[1]+B[1] == C[1]

### Read Spectra ###

spectra = read.csv("C:/Users/di58lag/Desktop/PhD/Projects/IRAgar/Data/200604 - Converted/E. coli/an0009.spc  - converted.csv")
dim(spectra)
plot(spectra)

colnames(spectra)
Base = ggplot(spectra,aes(x=Wavenumber,y=Intensity)) + geom_line()
Base

readDirs <- function(path, pattern="\\.txt$", recursive=TRUE, ...) {
  classDirs <- dir(path, full.names=TRUE)
  classDirs <- classDirs[file.info(classDirs)$isdir] # logische Indizierung
  stopifnot(length(classDirs) > 0)
  
  specFiles <- dir(classDirs, full.names=TRUE, pattern=pattern, recursive=recursive)
  spectra <- lapply(specFiles, read.table, ...)
  intensities <- lapply(spectra, "[[", 2)
  intensities <- matrix(unlist(intensities), nrow=length(intensities), byrow=TRUE)
  
  # Um zu verhindern, dass Verzeichnisse, deren Namen andere Verzeichnisnamen enthalten
  # von grep gefunden werden, wird an die Verzeichnisse ein Trennzeichen geh?ngt. [130905]
  labels <- lapply(paste0(classDirs, .Platform$file.sep), grep, x=specFiles, fixed=TRUE)
  labels <- as.factor(rep.int(seq_len(length(labels)),lapply(labels, length)))
  
  newData <- structure(list(x=unlist(spectra[[1]][1], use.names=FALSE), data=intensities,
                            files=specFiles, labels=labels, dir=classDirs),
                       class="ipcDataset")
}



trainData <- readDirs(path, recursive = TRUE, colClasses = "numeric")


#### Filtering using R to a specific date range ####
# From: https://stackoverflow.com/questions/62926802/filtering-using-r-to-a-specific-date-range

# First, I downloaded a sample dataset with dates and categorical data from here: 
# https://vincentarelbundock.github.io/Rdatasets/datasets.html
# Specifically, I got weather.csv


setwd("F:/Home Office/R")

data = read.csv("weather.csv") # Read the data into R
head(data)                     # Quality control, looks good
data = data[,2:3]        # For this example, I cut it to only take the relevant columns
data$date = as.Date(data$date) # This formats the date as dates for R
library(tidyverse)             # This will import some functions that you need, spcifically %>% and ggplot

# Step 0: look that the data makes sense to you
summary(data$date)
summary(data$city)

# Step 1: filter the right data
filtered = data %>% 
  filter(date > as.Date("2016-07-01") & date < as.Date("2017-07-01")) # This will only take rows between those dates

# Step 2: Plot the filtered data
# Using a bar plot: 
plot = ggplot(filtered, aes(x=city, fill = city)) + geom_bar() # You don't really need the fill, but I like it
plot

# Quality control: look at the numbers before and after the filtering:
summary(data$city)
summary(filtered$city)

#### Rename .LPE files from Bjorn ####
# 14/07/2020
# This script was written to change file names in a folder in a 
# specific way

path = "C:/Users/di58lag/Desktop/PhD/Projects/Blood-Iso/1. Sepsis Dataset/Data - Raw/Working Folder/spRaw/4-AAP"
list.files(path)

Original.names = list.files(path, full.names = T)
Mod.names = Original.names
#Mod.names = gsub('_', '=', Mod.names)
Mod.names = paste(Mod.names, "=4AAP",sep = "")
file.rename(Original.names, Mod.names)



m1 = round(matrix(c(rnorm(18,5,1)), nrow = 6, ncol = 3), digits = 0)

rownames(m1)<- c(LETTERS[1:5], "J")
colnames(m1) <- c(letters[1:3])

m2 = round(matrix(c(rnorm(6, mean = 2)), nrow = 3, ncol = 2), digits = 0)
rownames(m2)<-c("A", "C", "J")
colnames(m2) <- c(letters[1:2])

m.merged = merge(m1, m2, by = "row.names", all = TRUE)
rownames(m.merged) = m.merged$Row.names




###########
library(dplyr)
a = c("A", "B", "B", "C", "D", "D", "D")
b = c(6842, 5750, 5750, 4860, 7284, 7284, 7284)
df = as_tibble(cbind(a,b))
df
df$b = as.integer(df$b)
colnames(df) = c("cat", "long")

data <- df %>%
  group_by(cat) %>% 
  summarize(sum(long))
data



#### Adding row to a dataframe ####
x <- data.frame("Year" = c(1945,1945,1946,1946,1947,1947), "Age" = c(1,2,1,2,1,2),"Value" = c(4,5,4,5,4,6))
y <- data.frame("Year" = c(1945, 1945,1945,1945,1946,1946,1946,1946,1947,1947,1947,1947), "Age" = c(1,1,2,2,1,1,2,2,1,1,2,2), "Value" = c(4,3,5,4,4,3,5,4,4,3,5,4))

library(dplyr)
library(ggplot2)
df <- tibble(in_sample= c('yes', 'no', 'no', 'no', 'yes', 'yes', 'no', 'no', 'no', 'yes', 'no', 'yes', 
                          'no', 'no', 'no', 'yes', 'yes', 'no', 'yes', 'yes', 'no', 'yes', 'yes', 'no',
                          'yes', 'no', 'yes', 'no' , 'yes', 'no', 'yes', 'no', 'no', 'no', 'yes', 'yes', 
                          'no', 'no', 'no', 'yes', 'no', 'yes', 'no', 'no', 'no', 'yes', 'yes', 'no', 
                          'yes', 'yes', 'no', 'yes', 'yes', 'no', 'yes', 'no', 'yes', 'no' , 'yes', 'no'),
             region = c('West','East', 'South', 'North', 'West', 'East',
                        'South', 'North', 'South', 'West', 'South', 'West',
                        'East', 'South', 'East', 'East', 'East', 'East',
                        'North', 'East', 'South', 'West', 'West', 'East',
                        'North', 'North', 'East', 'South', 'West', 'West',
                        'West','West', 'South', 'West', 'West', 'West',
                        'West', 'West', 'West', 'West', 'North', 'North',
                        'North', 'North', 'South', 'North', 'East', 'North',
                        'North', 'North', 'South', 'North', 'North', 'North',
                        'South', 'South', 'South', 'South', 'North', 'North'))
df

ggplot(data = df) +
        geom_bar(mapping = aes(x = region, y = ..prop.., group = 1), stat = "count") +
        scale_y_continuous(breaks = seq(0, .4, .05), labels = scales::label_percent(accuracy = 1)) +
        facet_wrap(~in_sample, labeller = labeller(in_sample = c("no" = "Population", "yes" = "Sample"))) +
        theme_bw()



#### GDP ####
t = c(1:10)
gdp = -1*t*10 + 100
u.rate = t*1.6 + 20
r.inflation = -1*t

econ = data.frame(t,gdp,)

plot(t,u.rate)
lines(t,gdp)


#### Plot spectra from NIR ####

library(readxl)
library(reshape2)
library(ggplot2)


data = read_xlsx("C:/Users/di58lag/Desktop/PhD/Teaching/Ausbildung Students/Charlotte 13.01.20/My data (Amir).xlsx", skip = 1)
head(data)
dim(data)

data %>% ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free", ncol = 3) +
  geom_histogram()

colnames(data)

dataA = data[,1:5]
dataB = data[,9:10]

dataR = cbind(dataA, dataB)

head(dataR)

data_melted = melt(dataR, id.vars = "Wavenumber")
head(data_melted)

ggplot(data_melted, aes(x = Wavenumber, y = value, color = variable)) +
  geom_line() +
  facet_wrap(variable ~ ., ncol=3, scales = "free") + 
  theme_bw() + guides(fill=FALSE)

