### 
#
#This script is a mix of the PCA+LDA script and the SVM script to run a PCA+SVM analysis
#The first part is copied from the PCA+LDA script for loading the data and running the PCA analysis.
#This goes until you load the data as 
#The second part is copied from the SVM

################################################
path <- 'C:/Users/di58lag/Desktop/PhD/Projects/Enterobacteriacea database/Data/Experiment - 2019-08-22/04. Grouped/structured 2 - validation/Training'
testPath <- 'C:/Users/di58lag/Desktop/PhD/Projects/Enterobacteriacea database/Data/Experiment - 2019-08-22/04. Grouped/structured 2 - validation/Test'

####
# PART 1: Loading data and PCA
####


# Einlesen der Trainings- und Testdaten (gibt verbrauchte Zeit zurück)
printSubfolders(path)
system.time(trainData <- readDirs(path, recursive = TRUE, colClasses = "numeric"))
printSubfolders(testPath)
system.time(testData <- readDirs(testPath, recursive = TRUE, colClasses = "numeric"))

#added by Amir Nakar 23/08/2019.
#This will be used later for having the numbers and names next to each other
labelsforfolders = cbind(1:length(dir(path)),dir(path))
labelsforfolderstest = t(dir(path))

# Zurechtschneiden der Spektren inkl. Wellenzahlangabe (trainData$x)
plot(trainData$x,trainData$data[1,],type="l")
region <- selectRange(trainData$x, trainData$data[1,])
print(region)
region <- c(1:444, 691:881) # Fingerprintbereich und CH-Bande
trainData$data <- trainData$data[,region]
trainData$x <- trainData$x[region]
testData$data <- testData$data[,region]
plot(trainData$x,trainData$data[1,],type="l")


# Normierung der Daten (Vektornorm)
trainNorm <- vectorNorm(trainData$data)
testNorm <- vectorNorm(testData$data)
plot(trainData$x,trainNorm[1,],type="l")

## vorgelagerte PCA
pcaObj <- prcomp(trainNorm, center=TRUE, scale.=TRUE)

# Added by Amir 29/08/2019
# Look at PCs
# change the numbers according to which PCs you wish to look at
PCx = 1
PCy = 2
plot(pcaObj$x[,PCx], pcaObj$x[,PCy], type='p', col=trainData$labels, xlab=paste("PC",PCx), ylab=paste("PC",PCy), pch=as.character(trainData$labels))
plot(trainData$labels, pcaObj$x[,PCy], ylab=paste("PC",PCy))


# 1. Anhand der Klassifikationsrate
pcMax <- min(50, nrow(trainData$data) - 1)
pcResults <- pcOpt(pcMax, pcaObj, trainData$labels, print=TRUE)
plot(1:pcMax, pcResults, xlab="PCs", ylab="% Accuracy", t='b', pch=as.character(1:pcMax %% 10), cex=.75)
pcParam <- identify(1:pcMax, pcResults, n = 1)

## Berechnung der LDA
ldaObj <- lda(pcaObj$x[,1:pcParam], grouping=as.factor(trainData$labels), CV=FALSE)
ldaCvObj <- lda(pcaObj$x[,1:pcParam], grouping=as.factor(trainData$labels), CV=TRUE)

####
# PART 2: SVM
####
## DE: Modelle berechnen
## EN: Calculate models
# 1. Klassische SVM mit linearem Kernel
# 1. classic SVM with linear kernel
cost <- 100
system.time(svmModel <- ksvm(pcaObj$x[,1:pcParam], trainData$labels, kernel = "vanilladot", cross = 10, C = cost))
svmModel
capture.output(svmModel, file = paste(path, "svmModel1.txt"))


##DE:  besten Strafterm (C) durch Kreuzvalidierung bestimmen
# Die auszuprobierenden Werte haben logarithmisch den gleichen Abstand
## EN: best penelty parameter (C) by cross-validation
# The values to be tried out have logarithmically the same distance
cost <- 2^(-2:16)
nSV <- integer(length(cost))
svmError <- numeric(length(cost))
svmCross <- numeric(length(cost))
for (i in seq_along(cost)) {
  svmModel <- ksvm(pcaObj$x[,1:pcParam], trainData$labels, kernel = "vanilladot", cross = 10, C = cost[i])
  nSV[i] <- svmModel@nSV
  svmError[i] <- svmModel@error
  svmCross[i] <- svmModel@cross
  print(paste(i, "of 19 done"))
  Sys.time()
}
svmModel
capture.output(svmModel, file = paste(path, "svmModel2 - Optimized.txt"))
# DE: Tabellarische Darstellung aller Ergebniswerte
# EN: Tabular display of all result values
data.frame(C = cost, nSV, svmError, svmCross)

# Kreuzvalierung für Modell berechnen
# Calculate cross validation for model
system.time(predTrain <- manualSvmCv(pcaObj$x[,1:pcParam], trainData$labels, kernel = "vanilladot", cost = cost, cross = 10))

# DE:  2. Klassische SVM mit RBF-Kernel (sigma wird automatisch abgeschätzt)
# EN:  2. classical SVM with RBF kernel (sigma is automatically estimated)
cost <- 100
system.time(svmModel <- ksvm(pcaObj$x[,1:pcParam], trainData$labels, type="C-svc", cross = 10, C = cost))
svmModel


# DE: Automatisch den Sigmawert aus dem SVM-Modell in eine Parameterliste übertragen
# EN: Automatically transfer the sigma value from the SVM model to a parameter list.
if (class(kernelf(svmModel)) == "rbfkernel") {
  kparList <- list(sigma = kernelf(svmModel)@kpar$sigma)
}

# DE: Kreuzvalierung für Modell berechnen
# EN: # Calculate cross validation for model
system.time(predTrain <- manualSvmCv(pcaObj$x, trainData$labels, type="C-svc", kernel = "vanilladot", kparList, cost = cost, cross = 10))

###

#added by Amir Nakar 23/08/2019.
#This will be used later for having the numbers and names next to each other
labelsforfolders = cbind(1:length(dir(path)),dir(path))
labelsforfolderstest = cbind(1:length(dir(testPath)),dir(testPath))

# DE: Zusammenfassung aller missklassifizierten Spektren
# EN: Summary of all misclassified spectra
confMistakes <- listMistakes(trainData$labels, predTrain, trainData$files)
# DE: In Datei schreiben oder auf den Bildschirm ausgeben lassen
# EN: Write to file or output to screen
capture.output(confMistakes, file = paste(path, "PCA+SVM-confMistakes.txt"))
print(confMistakes)

## DE: alle Statistiken über die Kreuzvalidierung
## EN: all statistics about the cross validation
confMat <- confusionMatrix(as.factor(predTrain), trainData$labels)
print(confMat)
capture.output(labelsforfolders, cat("\n"),svmModel, confMat, file = paste(path, "PCA+SVM-confMat.txt"))

# DE: oder Konfusionsmatrix plus Sensitivität & Spezitivität
# EN: or confusion matrix plus sensitivity & specitivity
confSensSpec(confMat)

# Insgesamt richtig & falsch
correctTrain <- confMat$overall["Accuracy"] * 100
falseTrain <- 100 - correctTrain
cat("Correct:", correctTrain, "%", "\n")
cat("False:", falseTrain, "%", "\n")

# DE: Vorhersage auf dem Testdatensatz
# EN: Prediction on the test dataset

pcaTest <- scale(testNorm, center = pcaObj$center, scale = pcaObj$scale) %*% pcaObj$rotation

predTest <- predict(svmModel, pcaTest[,1:pcParam])

## Zusammenfassung wie oben
confMistakesTest <- listMistakes(testData$labels, predTest, testData$files)
# In Datei schreiben oder auf den Bildschirm ausgeben lassen
#capture.output(confMistakesTest, file = "Konfusion-Validierung-SVM.txt")
capture.output(labelsforfolders, confMistakesTest, file = paste(path, "PCA+SVM-confMistakes-Test.txt"))
print(confMistakesTest)

## Statistiken über die Vorhersage 
confMatTest <- confusionMatrix(predTest, testData$labels)
capture.output(labelsforfolders, confMatTest, file = paste(path, "PCA+SVM-confMat-Test.txt"))

print(confMatTest)
confSensSpec(confMatTest)

correctTest <- confMatTest$overall["Accuracy"] * 100
falseTest <- 100 - correctTest
cat("Correct:", correctTest, "%", "\n")
cat("False:", falseTest, "%", "\n")

### Erweiterte Statistiken erstellen ###

# Kreuzvalidierungsrate über mehrere Durchgänge mitteln
cost <- 100
crossRounds <- 25
crossVals <- double(crossRounds)
for (i in seq_along(crossVals)) {
  #    system.time(svmModel <- ksvm(trainNorm, trainData$labels, type="C-svc", cross = 10, C = cost)) # Standard-SVM mit Rbf
  system.time(svmModel <- ksvm(trainNorm, trainData$labels, kernel = "vanilladot", cross = 10, C = cost)) #Standard-SVM mit Linearkernel
  crossVals[i] <- svmModel@cross
}

# Statistikausgaben für die Kreuzvalidierungsraten
meanCross <- (1 - mean(crossVals)) * 100
sdCross <- sd(crossVals) * 100
cat("Cross-validation result for", crossRounds, "rounds:",
    "\nMean cross-validation rate:", meanCross, "%",
    "\nStandard deviation", sdCross, "\n")
boxplot((1 - crossVals) * 100)
title("Range of cross-validation values", paste(length(crossVals), "evaluations"))

