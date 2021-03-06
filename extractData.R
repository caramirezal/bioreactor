
## specific function defined for
## data minning of the data of Berthomieux et al, 2013
extractData <- function(directory,figure,output="raw") {
        ## locating and setting the data
        fileNames <- list.files(directory)
        figNames <- sapply(fileNames,function(x) readLines(paste(directory, x, sep = ""),n=1))
        figNames <- sub(":.*","",figNames)
        figNumber <- grep(figure,figNames)
        filePath <- paste(directory,fileNames[figNumber],sep = "")
        fileContent <- readLines(filePath)
        
        ## extracting flourescence data
        floure <- grep("^Time,",fileContent)
        emptyRow <- which(fileContent=="")
        emptyRow <- emptyRow[1]
        skipRows <- floure[1] + 1
        rowNumber <- emptyRow - ( skipRows + 1 )
        floure.df <- read.csv(filePath,header = FALSE,skip = skipRows,nrows = rowNumber)
        assignedNames <- unlist(strsplit(fileContent[floure[1]],","))
        names(floure.df) <- assignedNames
        
        ## extracting DO data
        skipRows <- floure[2] + 1
        DO.df <- read.csv(filePath,header = FALSE,skip = skipRows)
        assignedNames <- unlist(strsplit(fileContent[floure[2]],","))
        names(DO.df) <- assignedNames
        
        if ( output == "raw") {
                return(list("flourescence"=floure.df,"DO"=DO.df))
        }
        if ( output == "plot") {
                # plotting the data
                par(mar=c(5,5,4,5))
                with(DO.df, plot(Time,Absorbance,type="l",col="steelblue",lwd=2.5))
                par(new=TRUE)
                meanCol <- grep("mean",names(floure.df))
                plot(floure.df$Time, floure.df[,meanCol],axes = FALSE,pch=20,
                     ylab = "",xlab = "")
                axis(side=4)
                
        }
        
       
}

#rm(list = ls())
directory <- "Berthomieux2012/mainTextData/"
extractData(directory,"3A","plot")


