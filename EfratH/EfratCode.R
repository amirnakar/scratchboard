# importing Efrat's stuff

path = "C:/Users/anakar/Desktop/scratchboard/Efrat"                              # sets the directory (obviously, adjust to yours)
list.files(path)
filename = list.files(path)[1]
filename
file = paste(path, filename, sep = "/")

df = read.csv(file, skip = 3, header = T, sep = "\t")

colnames(df)[2] = "length"

head(df)

list.files(path)[1:4]
df$file = filename
df$Legth.category = NULL
head(df)


#### Import other 3 files into one big df #####


for(i in 2:4) {
  filename = list.files(path)[i]
  file = paste(path, filename, sep = "/")
  read = read.csv(file, skip = 3, header = T, sep = "\t")
  colnames(read)[2] = "length"
  read$file = filename
  df = rbind(df,read)

  
  
}

####


dim(df)
df$file = as.factor(df$file)
summary(df$file)

library(plyr) # this library has a useful revalue() function
df$type <- revalue(df$file, c("17starter  C 0_5hr Syber  CM-EDF_14_26 Oct.txt"="Con, 0.5h, 14",
                              "17starter  C 1_5hr Syber  CM-EDF_12_26 Oct.txt" = "Con, 1.5h, 12",
                              "17starter  Xp+ 0_5hr Syber  CM-EDF_13_26 Oct.txt" = "XP, 0.5h, 13",
                              "17starter  Xp+ 1_5hr Syber  CM-EDF_11_26 Oct.txt" = "XP, 1.5h, 11"))
summary(df$type)


#########
Data=data.frame(x=c(3,4,6,12))
 data$group = cut(data$x,c(0,5,10,15))
 data
#x   group
#1  3   (0,5]
#2  4   (0,5]
#3  6  (5,10]
#4 12 (10,15]

#What you've created there is a factor object in a column of your data frame. The text displayed is the levels of the factor, and you can change them by assignment:

levels(data$group) = c("3-7","7-12","12+")
data
#   x group
#1  3   0-5
#2  4   0-5
#3  6  6-10
#4 12   >10
###########


df$length.group = cut(df$length,c(0,3,7,12, 1000))
head(df)
df[31:100, ]
levels(df$length.group)
levels(df$length.group) = c("0-3","3-7","7-12","12+")
levels(df$type)

library(stringr)
types = str_split_fixed(df$type, ", ", n=Inf)
df = cbind(df, types)
colnames(df)[6:8] = c("Con", "GrowthT", "Num")
levels(df$Con)
summary(df$Con)

#Visualization
library(ggplot2)
ggplot(df,aes(x=type,y=length, fill=length.group)) + geom_boxplot()




### Testing the hypothesis ###
# The null hypothesis is that treatment does not affect the rate of elongation
# The rate of elongation is the amount of elongated cells after treatment

#First idea, compare the cell size between con and XP in the 
#            pre and post exposure times. Showing that in the first
#            they are identical and in the second different.

# STEP 1 - Clean out the 0-3 category

dfclean = !df$length.group == "0-3"     # Makes a vector where every line is TRUE except anywhere where you have "0-3"
dfclean
dfclean = df[dfclean, ]                 # Makes a new dataframe taking only the TRUE rows.

head(dfclean)
ggplot(dfclean,aes(x=type,y=length, fill=length.group)) + geom_boxplot() + ylim(0,75)

ggplot(dfclean,aes(x=type, fill=length.group)) + geom_bar()

# STEP 2 - Make 4 new dfs: con-bef / con-aft /// XP-be / XP-aft

levels(dfclean$type)
dfclean$Wilcox = dfclean$type
dfclean$Wilcox <- revalue(dfclean$Wilcox, c("Con, 0.5h, 14" = "con-bef",
                                        "Con, 1.5h, 12" = "con-aft",
                                        "XP, 0.5h, 13" = "XP-be",
                                        "XP, 1.5h, 11" = "XP-aft"))
head(dfclean)

con.bef = dfclean$Wilcox == "con-bef"
con.aft = dfclean$Wilcox == "con-aft"
XP.bef  = dfclean$Wilcox == "XP-be"
XP.aft  = dfclean$Wilcox == "XP-aft"

# STEP 3 - Run Wilcoxons between: 1.  Con-bef and XP-bef
#                                 2.  Con-aft and XP-aft


?wilcox.test
test1 = wilcox.test(dfclean$length[con.bef], dfclean$length[XP.bef], alternative = "l", conf.int = T)
test2 = wilcox.test(dfclean$length[con.aft], dfclean$length[XP.aft])

test1$p.value
test2$p.value

