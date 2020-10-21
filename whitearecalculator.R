### This script is to check the white zone in a picture (like a poster)
### the source is : https://stackoverflow.com/questions/25670005/measuring-whitespace-in-a-jpeg

# Required package
library(jpeg)

# Load and plot data
jpg <- "C:\\my_image.jpg"
my_jpg <- readJPEG(jpg)

# or for stand-alone reproducibility: 
# my_jpg <- readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))

# have a look at the original image
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(my_jpg,0,0,1,1)
# prints the jpg, just to make sure it's gone in ok


# Following Carl's example, subset each channel to get
# the pixels with white values (ie. close to 1) and make
# non-white pixels black for convienence. As Carl says,
# you'll need to adjust the values from 0.99 for your
# use case 
white_red_channel <- ifelse(my_jpg[,,1] > 0.9999, 1,0)
white_green_channel <- ifelse(my_jpg[,,2] > 0.9999, 1,0)
white_blue_channel <- ifelse(my_jpg[,,3] > 0.9999, 1,0)
# combine channels into array
white <- simplify2array(list(white_red_channel, 
                             white_green_channel, 
                             white_blue_channel))

# plot white/near-white pixels only
plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(white, 0, 0, 1, 1)
# looks pretty good, whiter areas on original are highlighted here:


# find proportion of image that is not black
whites <- white_red_channel + white_green_channel + white_blue_channel # sum channels
not_black <- sum(whites > 0) # count pixels that are not black
total_pixels <- ncol(whites) * nrow(whites) # find total number of pixels
not_black / total_pixels # proportion of non-black pixels

#This will output the precentage