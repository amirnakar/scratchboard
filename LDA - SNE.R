### This script was taken from here: https://rpubs.com/marwahsi/tnse
### Credit goes to shruti marwaha
### The idea is to transform LDA data, which has >2 dimensions
### Into SNE data which is 1-2 dimensions but still captures the SEPERATION ability.
### For a better understanding please see here: https://www.youtube.com/watch?v=NEaUSP4YerM&

# This is a random piece of change
# tSNE_introduction #
#Load Required Packages

library(readr)
library(Rtsne)

# Load data #


training_set <- bigX
#training_set <- read_csv("/Users/mshruti/GIT/Rcodes/MINST/train.csv")
## Parsed with column specification:
## cols(
##   .default = col_integer()
## )
## See spec(...) for full column specifications.
training_set$label <- as.factor(training_set$label)
dim(training_set)
## [1] 42000   785
# shrinking the size for the time limit
numTrain <- 50
set.seed(1)
rows <- sample(1:nrow(training_set), numTrain)
train <- training_set[rows,]

# Running tSNE #
set.seed(1) # for reproducibility
tsne <- Rtsne(train[,-1], dims = 2, perplexity=3, verbose=TRUE, max_iter = 500)


# visualizing
colors = rainbow(length(unique(train$label)))
names(colors) = unique(train$label)
par(mgp=c(2.5,1,0))
plot(tsne$Y, t='n', main="tSNE", xlab="tSNE dimension 1", ylab="tSNE dimension 2", "cex.main"=2, "cex.lab"=1.5)
text(tsne$Y, labels=train$label, col=colors[train$label])



result = cbind(result, bigX$Label)
plot(1 ,result[,1],col=result[,2])
result = tsne(bigX[,2:5], initial_config = NULL, k = 1, initial_dims = 5, perplexity = 1,
              max_iter = 500, min_cost = 0, epoch_callback = NULL, whiten = TRUE,
              epoch=100)
