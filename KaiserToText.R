# This is a junk piece of code

#### Intro ####
# Written by Amir Nakar 19/06/2020
# This code is meant for converting Kaiser .spc data into csv files
# It is based on this old code I got from Haodong:

#### Old Code: ####

install.packages("hyperSpec")
library(hyperSpec)

KaiserToTxt <- function(path)
{
  require(hyperSpec)
  Specs <- sapply(dir(path,full.names=T),read.spc,USE.NAMES = T,simplify=F)
  dir.create(paste0(path,"/txt"))
  lapply(names(Specs),function(nam)
  {
    Spec <- Specs[[nam]]
    df <- data.frame(Wavenumber=Spec@wavelength,Intensity=Spec@data$spc[1,])
    new.File <- paste0(paste0(path,"/txt/"),sub("//.spc","//.txt",basename(nam)))
    write.table(df,file=new.File,row.names=F,col.names=c("Wavenumber","Counts"))
  })
}

path = "C:/Users/di58lag/Desktop/PhD/Data - Raw/Kaiser Data/"
KaiserToTxt(path)


#### What I know ####
# 1. read.spc loads up the spectra. It needs a full path of the file
# 2. dir makes a list of all files in a folder
# 3. to make a df from the read.spec: 
#    data.frame(Wavenumber=Spec@wavelength,Intensity=Spec@data$spc[1,])

#### Rough Code ####
# So I want to do something like this: 
path = "C:/Users/di58lag/Desktop/PhD/Data - Raw/Kaiser Data/"
list = dir(path, pattern = ".spc", recursive = T, full.names = T)
numberoffiles = length(list)

# Here I want a recursive thing where for each line in the list I run: 
specs = read.spc(list[1]) # Reads the spectra
df <- data.frame(Wavenumber=specs@wavelength,Intensity=specs@data$spc[1,]) # Makes it into df
write.csv(df, file = paste(list[1]," - converted.csv"), row.names = F, col.names = T) # Write the df as a new file "converted-ORIGINAL.FILE.NAME"



#### How to Loop ####
# Create a vector filled with random normal values
u1 <- rnorm(30)
print("This loop calculates the square of the first 10 elements of vector u1")

# Initialize `usq`
usq <- 0

for(i in 1:10) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  usq[i] <- u1[i]*u1[i]
  print(usq[i])
}

print(i)


#### My Loop ####
for(i in numberoffiles-2:numberoffiles) {
  specs = read.spc(list[i]) # Reads the spectra
  df <- data.frame(Wavenumber=specs@wavelength,Intensity=specs@data$spc[1,]) # Makes it into df
  write.csv(df, file = paste(list[i]," - converted.csv"), row.names = F) # Write the df as a new file "converted-ORIGINAL.FILE.NAME"
  
}
