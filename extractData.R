
directory <- "Berthomieux2012/mainTextData/"
fileNames <- list.files(directory)
figNames <- sapply(fileNames,function(x) readLines(paste(directory, x, sep = ""),n=1))
figNames <- sub(":.*","",figNames)

filePath <- paste(directory,fileNames[1],sep = "")
fileContent <- readLines(filePath)
floure <- grep("^Time,",fileContent)
emptyRow <- which(fileContent=="")
emptyRow <- emptyRow[1]
skipRows <- floure[1] + 1
rowNumber <- emptyRow - ( skipRows + 1 )

## extracting flourescence data
floure.df <- read.csv(filePath,header = FALSE,skip = skipRows,nrows = rowNumber)
assignedNames <- strsplit(fileContent[floure[1]],",")
assignedNames <- assignedNames[[1]]
names(floure.df) <- assignedNames

## extracting DO data
skipRows <- floure[2] + 1
DO.df <- read.csv(filePath,header = FALSE,skip = skipRows)
assignedNames <- strsplit(fileContent[floure[2]],",")[[1]]
names(DO.df) <- assignedNames


par(mar=c(5,5,4,5))
with(DO.df, plot(Time,Absorbance,type="l",col="steelblue",lwd=2.5))
par(new=TRUE)
meanCol <- grep("mean",names(floure.df))
plot(floure.df$Time, floure.df[,meanCol],axes = FALSE,pch=20,
     ylab = "",xlab = "")
axis(side=4)