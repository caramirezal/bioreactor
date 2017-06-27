library(tcltk2)
library(zoo)
library(jpeg)

#digitize functions
ReadAndCal = function(fname)
{
        ReadImg(fname)
        calpoints <- locator(n=4,type='p',pch=4,col='blue',lwd=2)
        return(calpoints)
}

ReadImg = function(fname)
{
        img <- readJPEG(fname)
        op <- par(mar=c(0,0,0,0))
        on.exit(par(op))
        plot.new()
        rasterImage(img,0,0,1,1)
}

DigitData = function(col='red',type='p',...)
{
        type <- ifelse(type=='b','o',type)
        type <- ifelse(type%in%c('l','o','p'),type,'p')
        locator(type=type,col=col,...)
}

Calibrate = function(data,calpoints,x1,x2,y1,y2)
{
        x 		<- calpoints$x[c(1,2)]
        y 		<- calpoints$y[c(3,4)]
        
        cx <- lm(formula = c(x1,x2) ~ c(x))$coeff
        cy <- lm(formula = c(y1,y2) ~ c(y))$coeff
        
        data$x <- data$x*cx[2]+cx[1]
        data$y <- data$y*cy[2]+cy[1]
        
        return(as.data.frame(data))
}

jpegfile <- "fig/S1Aplot.jpg"

#digitize a graph
(cal = ReadAndCal(jpegfile))
## This opens the jpeg in a plotting window and lets you define points on the x and y axes. 
## You must start by clicking on the left-most x-axis point, then the right-most axis point, 
## followed by the lower y-axis point and finally the upper y-axis point. You don’t need 
## to choose the end points of the axis, only two points on the axis that you know 
## the x or y value for. As you click on each of the 4 points, the coordinates are saved 
## in the object cal.


(data.points = DigitData(col = 'red'))
## You return to the figure window, and now you can click on each of the data points
## you’re interested in retrieving values for. The function will place a dot 
## (colored red in this case) over each point you click on, and the raw x,y coordinates 
## of that point will be saved to the data.points list. When you’re finished clicking points,
## you need to hit stop/Finish or right-click to stop the data point collection.

df = Calibrate(data.points, cal, 550, 950, 0, 0.6)
## Finally, you need to convert those raw x,y coordinates into the same scale as the 
## original graph. You do this by calling the Calibrate function and feeding it your 
## data.point list, the cal list that contains your 4 control points from the first step, 
## and then 4 numeric values that represent the 4 original points you clicked on 
## the x and y axes. These values should be in the original scale of the figure
## (i.e. read the values off the graph’s tick marks).

