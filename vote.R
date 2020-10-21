require(caret)

###################################
## define necessary functions 

aggr_fun <- function (x, SD = F) {
  res <- NA
  if (!is.matrix(x) & !is.data.frame(x)) {
    if (is.numeric(x)) {
      res <- mean(x, na.rm = T)
      if (SD) 
        res <- c(mean = res, sd = sd(x, na.rm = T))
    }
    else {
      res <- names(which.max(summary(as.factor(x))))
    }
  }
  else res <- apply(x, 2, aggr_fun)
  return(res)
}

###################################


####################################
## input parameters

results_directory <- 'C:/Users/di58lag/Desktop/carbatech summary/RESULTS FOR VOTING/enterop'   # path to the directory with prediction.csv and metadata.csv

csv_format <- c(';',',')   # saparator between columns and decimal separator used in CSV files
vote_within <- c('sensitivity','Genus')    # column names of groups whithin which to vote (e.g. type and batch)
vote_each <- 49 # set to 0 or NULL to include maximal number of votes (e.g. vote over the batch)


## read data

pred_fn <- file.path(results_directory, "prediction.csv")
metadata_fn <- file.path(results_directory, "metadata.csv")

pred <- read.csv(pred_fn, sep=csv_format[1], dec=csv_format[2])

originalmatrix = confusionMatrix(pred$label,pred$predicted)

metadata <- read.csv(metadata_fn, sep=csv_format[1], dec=csv_format[2])

results <- merge(metadata, pred)
####################################



###################################
## aggregate results to obtain major votes

cols <- intersect(colnames(results), vote_within)
aggr_by <- apply(results[cols],1,paste, collapse='|')

if (!is.null(vote_each)) # & vote_each > 1) 
  for (u in unique(aggr_by))
  {
    w <- aggr_by == u
    s <- floor(seq(0, vote_each, length.out = sum(w)+1))[-1]
    aggr_by[w] <- paste(u, s, sep='|')
  }

res_aggr <- aggregate(results,list(aggr_by), aggr_fun)

###################################
## get summary

reference <- as.factor(res_aggr$X__type__)
pred <- factor(res_aggr$predicted, levels(reference))
(mat <- confusionMatrix(pred, reference))
cat("Balanced accuracy (overall):", mean(mat$byClass[,"Sensitivity"]))

originalmatrix

