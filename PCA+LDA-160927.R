library(caret)     # für confusionMatrix
library(e1071)     # wird von caret benötigt
library(randomForest) # für randomForest, zur Variablenselektion
library(MASS)      # für lda

####
#         path: Gibt das Verzeichnis an, in dem nach Spektren gesucht wird
#      pattern: Einschränkung auf Dateien, die dem Muster entsprechen (siehe ?dir)
#          ...: zusätzliche Argumente für read.table
#
#    Liest das angegebene Verzeichnis ein und liefert eine Liste mit 
#      Wellenzahlachse (x), Spektren (data) und den dazugehörigen 
#      Dateinamen (files) zurück.
####
readDir <- function(path, pattern="\\.txt$", ...) {
   specFiles <- dir(path, full.names=TRUE, pattern=pattern)
   spectra <- lapply(specFiles, read.table, ...)
   intensities <- lapply(spectra, "[[", 2)
   intensities <- matrix(unlist(intensities), nrow=length(intensities), byrow=TRUE)
   newData <- structure(list(x=unlist(spectra[[1]][1], use.names=FALSE), data=intensities,
                              files=specFiles), class="ipcDataset")
}

####
#         path: Gibt das Hauptverzeichnis an, in dem nach Spektren gesucht wird
#      pattern: Einschränkung auf Dateien, die dem Muster entsprechen (siehe ?dir)
#    recursive: Sollen auch Unterordner mit aufgelistet werden (siehe ?dir)
#          ...: zusätzliche Argumente für read.table
#
#    Liest die Unterverzeichnisse von 'path' ein und liefert eine Liste mit 
#      Wellenzahlachse (x), Spektren (data), den dazugehörigen Dateinamen (files),
#      Gruppenzugehörigkeiten als Faktor (labels) und Verzeichnisnamen (dir) zurück.
####
readDirs <- function(path, pattern="\\.txt$", recursive=TRUE, ...) {
   classDirs <- dir(path, full.names=TRUE)
   classDirs <- classDirs[file.info(classDirs)$isdir] # logische Indizierung
   stopifnot(length(classDirs) > 0)

   specFiles <- dir(classDirs, full.names=TRUE, pattern=pattern, recursive=recursive)
   spectra <- lapply(specFiles, read.table, ...)
   intensities <- lapply(spectra, "[[", 2)
   intensities <- matrix(unlist(intensities), nrow=length(intensities), byrow=TRUE)

   # Um zu verhindern, dass Verzeichnisse, deren Namen andere Verzeichnisnamen enthalten
   # von grep gefunden werden, wird an die Verzeichnisse ein Trennzeichen gehängt. [130905]
   labels <- lapply(paste0(classDirs, .Platform$file.sep), grep, x=specFiles, fixed=TRUE)
   labels <- as.factor(rep.int(seq_len(length(labels)),lapply(labels, length)))

   newData <- structure(list(x=unlist(spectra[[1]][1], use.names=FALSE), data=intensities,
                              files=specFiles, labels=labels, dir=classDirs),
                              class="ipcDataset")
}

####
#    x: Eine Matrix, die zeilenweise die Intensitäten von Spektren enthält
#
#    Führt Vektornormierung der Spektren durch. 
####
vectorNorm<-function(x) {
    return(x / sqrt(rowSums(x^2))) 
}

####
#    mat: Ein Objekt vom Typ 'confusionMatrix' (aus caret)
#
#    Gibt eine durch Sensitivität und Spezifität erweiterte Konfusionsmatrix aus.
####
confSensSpec<-function(mat) {
   stopifnot(class(mat) == "confusionMatrix")
   m <- cbind(mat$table,
              round(mat$byClass[,"Sensitivity"]*100, 2),
              round(mat$byClass[,"Specificity"]*100, 2)
             )
   colnames(m) <- c(colnames(mat$table), "Sensitivity", "Specificity")
   m
}

####
#     labels: ein Faktor, der die Klassenzugehörigkeiten angibt
#    classes: ein Vektor mit den vorhergesagten Klassenzugehörigkeiten
#      files: eine Liste mit den zu den Labeln gehörenden Dateinamen
#
#    Gibt eine Tabelle mit den Indizes, Klassen & Dateinamen der 
#      missklassifizierten Spektren zurück.
####
listMistakes<-function(labels, classes, files) {
   miss <- which(labels != classes)
   if (length(miss) != 0) {
      dframe <- data.frame(index = miss, class = labels[miss], predicted = classes[miss], file = basename(files[miss]))
      return(dframe)
   } else {
      message("Keine missklassifizierten Spektren gefunden.\n")
   }
}


#### EN:
# pcMax: The maximum value of the main components to be used.
# pcaObj: an object of type 'prcomp' containing the results of the principal component analysis.
# labels: A factor that indicates the class affiliations of the training data set.
# print: specifies whether the best classification rates should be displayed (TRUE/FALSE)
#
# Creates a vector with result rates for classification when using up to
# 'pcMax' many major components... #
####

#### DE:
#     pcMax: Der Maximalwert der zu verwendenden Hauptkomponenten
#    pcaObj: ein Objekt vom Typ 'prcomp', das die Ergebnisse der Hauptkomponentenanalyse enthält
#    labels: Ein Faktor, der die Klassenzugehörigkeiten des Trainingsdatensatzes angibt
#     print: gibt an, ob die besten Klassifikationsraten angezeigt werden sollen (TRUE/FALSE)
#
#    Erzeugt einen Vektor mit Ergebnisraten für die Klassifikation bei Verwendung von bis zu
#      'pcMax' vielen Hauptkomponenten.
####
pcOpt<-function(pcMax, pcaObj, labels, print=FALSE) {
    pcResults <- double(pcMax)
    pcList <- lapply(seq_len(pcMax), seq_len)
    for (i in seq_along(pcList)){
        ldaCvObj <- lda(pcaObj$x[,pcList[[i]], drop=FALSE], CV=TRUE, grouping=labels)
        cvTable <- table(ldaCvObj$class, labels)
        pcResults[i] <- sum(diag(cvTable))/sum(cvTable)*100 # Prozentwert
    }

    if (print) {
        highest <- unique(sort(pcResults, decreasing=TRUE))
        length(highest) <- min(length(highest), 10)
        nPcs <- seq_len(pcMax)

        for (i in seq_along(highest)){
            print(paste(formatC(highest[i], digits=5, flag='-'), "%:", 
                  paste(nPcs[which(pcResults == highest[i])], collapse=', ')), quote=FALSE)
        }
    }
    pcResults
}

####
#       pcMax: Der Maximalwert der zu verwendenden Hauptkomponenten
#      pcaObj: ein Objekt vom Typ 'prcomp', das die Ergebnisse der Hauptkomponentenanalyse enthält
#    testData: Eine Matrix mit Spektren zum Testen des Modells
# trainLabels: Ein Faktor, der die Klassenzugehörigkeiten des Trainingsdatensatzes angibt
#  testLabels: Ein Faktor, der die Klassenzugehörigkeiten des Testdatensatzes angibt
#       print: gibt an, ob die besten Klassifikationsraten angezeigt werden sollen (TRUE/FALSE)
#
#    Erzeugt einen Vektor mit Ergebnisraten für die Vorhersage bei Verwendung von bis zu
#      'pcMax' vielen Hauptkomponenten.
####
pcOptIdent<-function(pcMax, pcaObj, testData, trainLabels, testLabels, print=FALSE) {
   pcResults <- double(pcMax)
   pcList <- lapply(seq_len(pcMax), seq_len)
   for (i in seq_along(pcList)){
      ldaObj <- lda(pcaObj$x[,pcList[[i]], drop=FALSE], CV=FALSE, grouping=trainLabels)
      pcaTest <- scale(testData, center=pcaObj$center, scale=pcaObj$scale) %*% pcaObj$rotation

      pred <- predict(ldaObj, pcaTest[,pcList[[i]], drop=FALSE])
      confMat <- confusionMatrix(pred$class, testLabels)
      pcResults[i] <- confMat$overall["Accuracy"] * 100 # Prozentwert
   }

   if (print) {
      highest <- unique(sort(pcResults, decreasing=TRUE))
      length(highest) <- min(length(highest), 10)
      nPcs <- seq_len(pcMax)

      for (i in seq_along(highest)){
         print(paste(formatC(highest[i], digits=5, flag='-'), "%:", 
               paste(nPcs[which(pcResults == highest[i])], collapse=', ')), quote=FALSE)
      }
   }
   pcResults
}

####
#    path: Gibt das Wurzelverzeichnis an
#
#    Listet alle direkten Unterordner im Pfad auf.
####
printSubfolders<-function(path) {
    folders <- dir(path, full.names=TRUE)
    folders <- folders[file.info(folders)$isdir]
    if (length(folders) == 0) {
        stop(paste("Keine Unterordner in", path, "gefunden!"))
    }
    for (i in seq_along(folders)) {
        cat(i, ":", basename(folders[i]), "\n")
    }
    cat("\n")
}

################################################
path <- 'E:/Fun with R/Ente'

testPath <- 'C:\\Users\\di58lag\\Desktop\\04. Grouped/Coli Shig/\\val'

# Einlesen der Trainings- und Testdaten (gibt verbrauchte Zeit zurück)
printSubfolders(path)
system.time(trainData <- readDirs(path, recursive = TRUE, colClasses = "numeric"))
printSubfolders(testPath)
system.time(testData <- readDirs(testPath, recursive = TRUE, colClasses = "numeric"))

#added by Amir Nakar 23/08/2019.
#This will be used later for having the numbers and names next to each other
labelsforfolders = cbind(1:length(dir(path)),dir(path))
labelsforfolderstest = cbind(1:length(dir(testPath)),dir(testPath))

# Zurechtschneiden der Spektren inkl. Wellenzahlangabe (trainData$x)
plot(trainData$x,trainData$data[1,],type="l")
region <- selectRange(trainData$x, trainData$data[1,])
print(region)
region <- c(1:420, 707:881) # Fingerprintbereich und CH-Bande
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

## Bestimmung der Anzahl von Hauptkomponenten 
# 1. Anhand der Klassifikationsrate
pcMax <- min(50, nrow(trainData$data) - 1)
pcResults <- pcOpt(pcMax, pcaObj, trainData$labels, print=TRUE)
plot(1:pcMax, pcResults, xlab="PCs", ylab="% Accuracy", t='b', pch=as.character(1:pcMax %% 10), cex=.75)
pcParam <- identify(1:pcMax, pcResults, n = 1)

# 2. Anhand der Vorhersagerate auf einem unabhängigen Datensatz
pcMax <- min(50, nrow(trainData$data) - 1)
pcResults <- pcOptIdent(pcMax, pcaObj, testNorm, trainData$labels, testData$labels, TRUE)
plot(1:pcMax, pcResults, xlab="PCs", ylab="% Accuracy", t='b', pch=as.character(1:pcMax %% 10), cex=.75)
pcParam <- identify(1:pcMax, pcResults, n = 1)

# 3. Abschätzung über einen Random Forest-Klassifikator
# (siehe "Leo Breiman, Random Forests, DOI: 10.1023/A:1010933404324")
pcMax <- min(50, nrow(trainData$data) - 1)
forest <- randomForest(pcaObj$x[,seq_len(pcMax), drop=FALSE], trainData$labels, importance = TRUE, maxnodes = 10, ntree = 1000)
plot(forest$importance[,"MeanDecreaseAccuracy"], type="b", ylab = "Mean decrease in accuracy")
pcParam <- identify(forest$importance[,"MeanDecreaseAccuracy"], n = 1)

## Berechnung der LDA
ldaObj <- lda(pcaObj$x[,1:pcParam], grouping=as.factor(trainData$labels), CV=FALSE)
ldaCvObj <- lda(pcaObj$x[,1:pcParam], grouping=as.factor(trainData$labels), CV=TRUE)

#### NOTES: the output of lda function are:
# prior: The ratio between different groups / the prior probabilities used.
# Counts: number of samples per class
# means: the mean of each variable per class (for example, mean of PC1 for each strain)
# Scaling: a matrix of the weights for each LD, on each PC / 
#          a matrix which transforms observations to discriminant functions, normalized
#          so that within groups covariance matrix is spherical.
# lev: the numbers and names of different classes. class = levels (usually "1", "2", "3")
# svd: the singular values, which give the ratio of the between- and within-group standard deviations
#     on the linear discriminant variables. Their squares are the canonical F-statistics.
# N: The number of observations used.
# Call: The (matched) function call.


    
## Zusammenfassung aller missklassifizierten Spektren
confMistakes <- listMistakes(trainData$labels, ldaCvObj$class, trainData$files)
# In Datei schreiben oder auf den Bildschirm ausgeben lassen
options(max.print = 10000)
capture.output(confMistakes, file = paste(path, "confMistakes.txt"))
print(confMistakes)

## Plot der LDA-Komponenten
plot(ldaObj, col=as.numeric(trainData$labels), cex = 1.2)

# alle Statistiken über die Kreuzvalidierung 
confMat <- confusionMatrix(ldaCvObj$class, trainData$labels)
print(confMat)
capture.output(labelsforfolders, confMat, file = paste(path, "confMat.txt"))

# oder Konfusionsmatrix plus Sensitivität & Spezitivität
confSensSpec(confMat)

# Insgesamt richtig & falsch
correctTrainLda <- confMat$overall["Accuracy"] * 100
falseTrainLda <- 100 - correctTrainLda
cat("Correct:", correctTrainLda, "%", "\n")
cat("False:", falseTrainLda, "%", "\n")

## Transformation der Testdaten in den PCA-Raum und Vorhersage
pcaTest <- scale(testNorm, center = pcaObj$center, scale = pcaObj$scale) %*% pcaObj$rotation
predLda <- predict(ldaObj, pcaTest[,1:pcParam])

## Zusammenfassung wie oben
confMistakesTest <- listMistakes(testData$labels, predLda$class, testData$files)
# In Datei schreiben oder auf den Bildschirm ausgeben lassen
capture.output(confMistakesTest, file = paste(path, "confMistakesTest.txt"))
print(confMistakesTest)

## Statistiken über die Vorhersage
confMatTest <- confusionMatrix(predLda$class, testData$labels)
print(confMatTest)
capture.output(confMatTest, file = paste(path, "confMatTest.txt"))

confSensSpec(confMatTest)

correctTest <-  confMatTest$overall["Accuracy"] * 100
falseTest <- 100 - correctTest
cat("Correct:", correctTest, "%", "\n")
cat("False:", falseTest, "%", "\n")

##
# Transformation der Trainings- und Testdaten in den LDA-Raum
# Kann für Plots benutzt werden (z.B. in 3D, etc.)
ldaValues <- scale(pcaObj$x[,1:pcParam], center=colMeans(ldaObj$means), scale=FALSE) %*% ldaObj$scaling
predValues <- scale(pcaTest[,1:pcParam], center=colMeans(ldaObj$means), scale=FALSE) %*% ldaObj$scaling
plot(predValues[,c(1,2)], col=as.numeric(testData$labels), pch = as.character(testData$labels), cex = 1.2)

