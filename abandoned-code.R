#
# Most of what is present here is not documented.  These were dead ends or
# experiments done while working on the assignment.  They are being saved
# as there are a few items that might be of future interest.
#

#
# Original code written to download data file before noticed that assignment
# doesn't require that to be done.  But does require you to unzip the data
# file.

downloadData <- FALSE

#
# The assignment did not require us to download the data separately, since it
# was included in the forked GitHub repository we started working from.  I
# wrote it anyhow, however, for my own practice.  I am leaving it included but
# disabled from running.
#
if (downloadData) {
    dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    dataFile <- "activity.csv"
    dataFileTimestampFile <- paste0(dataFile, "-timestamp")
    
    # If the extracted data file is not already available, download it.
    if (!file.exists(dataFile)) {
        temp <- tempfile()
        download.file(dataURL, temp, mode="wb") # download ZIP to temp file
        dataDownloadTime <- Sys.time()  # record when we downloaded it
        unzip (temp, dataFile)  # unzip the downloaded file
        file.remove (temp)  # remove the ZIP file leaving just extracted data
        
        # save for future reference when we downloaded the data file
        write(unclass(dataDownloadTime), file = dataFileTimestampFile)
    } else {
        # Otherwise, load cached data on when we previously downloaded it.
        dataDownloadTime <- scan(file = dataFileTimestampFile, quiet=TRUE)
        dataDownloadTime <- as.POSIXct(dataDownloadTime,
                                       origin = "1970-01-01")
    }
    
    print (sprintf ("Using data from %s, downloaded at %s",
                    dataURL, dataDownloadTime))
}




#
# More difficult ways of working with the missing values.
#
naData <- data[is.na(data$steps),]

addMissingVal <- function (stepVal, intVal) {
    print (length(stepVal))
    print (length(intVal))
    if (!is.na (stepVal)) {
        return (stepVal)
    } else {
        return (as.numeric(plotData[plotData$interval==intVal,"steps"]))
    }
}

genMissingVals <- function(intVal) {
    return (as.numeric(plotData[plotData$interval==intVal,"steps"]))
}

tempFunc <- function (stepVal, intVal) {
    newCol <- vector()
    
    for (elt in 1:length(stepVal)) {
        if (!is.na (stepVal[elt])) {
            newCol[elt] <- stepVal[elt]
        } else {
            newCol[elt] <- as.numeric(plotData[plotData$interval==intVal[elt],"steps"])
        }
    }
    
    #print (length(newCol))
    return (newCol)
}


#
# Plotting attempts
#
newData <- transform(data, steps = tempFunc(steps, interval))

newplotData <- summarize(group_by(newData, interval), steps = mean(steps))
newplotMaxInt <- newplotData[which(newplotData$steps==max(newplotData$steps)),]$interval

plot(newplotData$interval, newplotData$steps, type="l", xlab="5-second interval",
     ylab="# of steps", main="New Average Number of Steps Per 5-Second Interval")
abline(v = newplotMaxInt, col="blue", lwd=2)




par(mfrow = c(2,1))

set.panel(m=2,n=1)
for (typeofday in as.factor(c("weekend","weekday"))) {
    plot(fullData_SbyD[which(fullData_SbyD$typeofday==typeofday),]$interval,
         fullData_SbyD[which(fullData_SbyD$typeofday==typeofday),]$steps,
         type="l",
         main=typeofday)
}



##
## Attempt for final question using ggplot
##
g <- ggplot(fullData2_SbyD, aes(interval, steps)) +
    geom_line() +
    facet_wrap(~ typeofday, ncol=1, as.table=FALSE) +
    labs(x = "Interval", y = "Number of steps") +
    theme(strip.background = element_rect(fill = "tan"),
          strip.text = element_text(size=14))   

print (g)

# Following found on Internet and comes closer.
# qplot(x=interval, y=steps,data=subset(activity, complete.cases(activity)),geom='smooth', stat='summary', fun.y=mean) + facet_grid(dayType~.) + facet_wrap(~dayType,nrow=2) + theme(strip.background = element_rect(fill="#ffe5cc")) + labs(title=' Average steps per days, analyzing weekdays and weekend patterns')



##
## Attempt for final question using base plot.
##
par(mfrow = c(2,1))

typeofday <- "weekend"
par(mar=c(0,4,4,2))
plot(fullData2_SbyD[which(fullData2_SbyD$typeofday==typeofday),]$interval,
     fullData2_SbyD[which(fullData2_SbyD$typeofday==typeofday),]$steps,
     type="l", yaxt="n", xlab="", ylab="", xaxt="n")
title(main="weekend", col.main="blue")
axis(side=2, at=c(50,100,150, 200), labels=rep(" ", 4))
axis(side=4, at=c(50,100,150, 200))
mtext("WEEKDAY")

typeofday <- "weekday"
par(mar=c(5,4,0,2))
plot(fullData2_SbyD[which(fullData2_SbyD$typeofday==typeofday),]$interval,
     fullData2_SbyD[which(fullData2_SbyD$typeofday==typeofday),]$steps,
     type="l",
     xlab="", ylab="", main="weekday")
axis(side=4, at=c(50,100,150, 200), labels=rep(" ",4))