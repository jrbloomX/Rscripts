

#Multiple Regression analysis and Exploratory Data Analysis (EDA)
#MIDDLE FORK COQUILLE RIVER
#DO and DOPerSat = f(Time of day, temperature, river flow, nutrients)
#Jim Bloom

#quick youtube vid on MLR models: https://www.youtube.com/watch?v=q1RD5ECsSB0

#"The general rule of thumb (based on stuff in Frank Harrell's book, Regression Modeling Strategies) 
# is that if you expect to be able to detect reasonable-size effects with reasonable power, you 
# need 10-20 observations per parameter (covariate) estimated."
#https://stats.stackexchange.com/questions/29612/minimum-number-of-observations-for-multiple-linear-regression?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

library(tidyr) #"An evolution of 'reshape2'. It's designed specifically for data tidying 
# (not general reshaping or aggregating) and works well with 'dplyr' data pipelines."
library(car) # Companion to Applied Regression - includes some() function
library(MASS)
library(lattice); citation("lattice")
#multi-panel plots via "ggpairs:
library(ggplot2)
#plotmatrix(dDO) + geom_smooth() ## Note: The plotmatrix() function has been replaced by 
# the ggpairs() function from the GGally package 
library(GGally)

### make multi-panel plot by using "pairs"
# This is from my ESR 550 class Y. Pan Winter 2009
##create histograms for each variable
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
## put (absolute) correlations on the upper panels, with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}

### CI.LINES Function that draws 95% confidence intervals for a regression
#     via The R Book, 2nd Ed., Crawley, p. 474
#     REGRESSIONS - LINEAR MODEL - m <- lm(y ~ x); m
ci.lines <- function(model, colorci){
  xm <- sapply(model[[12]][2],mean)
  n <- sapply(model[[12]][2],length)
  ssx <- sum(model[[12]][2]^2)-sum(model[[12]][2])^2/n
  s.t <- qt(0.975,(n-2))
  xv <- seq(min(model[[12]][2]),max(model[[12]][2]),length=100)
  yv <- coef(model)[1]+coef(model)[2]*xv
  se <- sqrt(summary(model)[[6]]^2*(1/n+(xv-xm)^2/ssx))
  ci <- s.t*se
  uyv <- yv+ci
  lyv <- yv-ci
  lines(xv,uyv,lty=2,col=colorci)
  lines(xv,lyv,lty=2,col=colorci)
}

#setwd("I:/TMDL_WR/South_Coast/Coquille/Data/11485_33922_MFCoquille/") 
setwd("I:/Library (weekly backup)/Models_Software/R/Script Library/BaseGraphicsExamples/EDA/Data/")

dfile <- "11485_33922_MFCoquille_1982to2017viaAWQMSandLASAR_v6.csv" #Summer only

dfileAll <- "11485_33922_MFCoquille_1982to2017viaAWQMSandLASAR_AllMonths_v2.csv"

outpath <- "I:/Library (weekly backup)/Models_Software/R/Script Library/BaseGraphicsExamples/EDA/Output/"



#Final plots are based on expanatory variables: Time-of-day, temperature, river flow (SF at Powers), and orthophosphate as P

#Database pre-processed to include only summer data (Jun 1 to Sep 30)
d1 <- read.csv(dfile); head(d1)

#Data for all months for exploratory data analysis (EDA) and monthly box plots - A for All
d1A <- read.csv(dfileAll); d1A
str(d1A)
some(d1A, n=8L)

#Time functions
#Convert date/time to POSIXct and/or POSIXlt format (see p. 90 of The R Book, M.J. Crawley)
#  Class POSIXct - continuous time (i.e. a number of seconds) - represents the (signed) number 
#  of seconds since the beginning of 1970 (in the UTC time zone) as a numeric vector.
#  Therefore, 1/1/1980 would be 
#testit <- "09/21/2011 12:01 AM"
#testit2 <- as.POSIXct(testit, format="%m/%d/%Y %I:%M %p" ,tz="America/Los_Angeles")
#as.numeric(testit)
#date()
## suppose we have a time in seconds since 1960-01-01 00:00:00 GMT
## (the origin used by SAS)
#z <- 1472562988
# ways to convert this
#as.POSIXct(z, origin = "1960-01-01")                # local
#as.POSIXct(z, origin = "1960-01-01", tz = "GMT")    # in UTC
#z <- 28800     #1970-01-01
#as.POSIXct(z1, origin = "1970-01-01")                # local
#z <- 315561600 #1980-01-01
#as.POSIXct(z, origin = "1970-01-01")                # local
#z <- 631180800 #1990-01-01
#as.POSIXct(z, origin = "1970-01-01")                # local
#z <- 946713600 # 2000-01-01
#as.POSIXct(z, origin = "1970-01-01")                # local
#z <- 1262332800 # 2010-01-01
#as.POSIXct(z, origin = "1970-01-01")                # local
#z <- 1577865600 # 2020-01-01
#as.POSIXct(z, origin = "1970-01-01")                # local
#
#315561600 #1980-01-01
#631180800 #1990-01-01
#946713600 # 2000-01-01
#1262332800 # 2010-01-01
#1577865600 # 2020-01-01

#  Class POSIXlt - list time (i.e. a list of all the various categorical descriptions of the 
#  time, including day of the week and so forth).
#  You can obtain things like hour and min from as.POSIXlt version
#    d1$DateTimelt$hour
#    d1$DateTimelt$min
#    d1$DateTimelt$mon #0-11: months after the first of the year - add 1 
#    d1$DateTimelt$yday #0-365: day of the year - add 1

#GATHER command wants date in POSIXct format
d1$DateTimect <- as.POSIXct(d1$DateTime_PST, format="%m/%d/%Y %I:%M %p" ,tz="America/Los_Angeles")
#You can obtain things like hour and min from POSIXlt format
d1$DateTimelt <- as.POSIXlt(d1$DateTime_PST, format="%m/%d/%Y %I:%M %p" ,tz="America/Los_Angeles")
str(d1)
#d1$DateTimelt$mon
#d1$DateTimelt$hour
#d1$DateTimelt$min
d1A$DateTimect <- as.POSIXct(d1A$DateTime_PST, format="%m/%d/%Y %I:%M %p" ,tz="America/Los_Angeles")
d1A$DateTimelt <- as.POSIXlt(d1A$DateTime_PST, format="%m/%d/%Y %I:%M %p" ,tz="America/Los_Angeles")
d1A$Month <- d1A$DateTimelt$mon + 1 #since Jan is mon 0
head(d1A)

#Select only necessary columns and place in order that worksfor TIDYR function SPREAD
#d2 <- data.frame(d1$Parameter, d1$DateTimelt, d1$HourPST, d1$Result_est)
#d2 <- data.frame(d1[9], d1[16], d1[8], d1[11]) #Parameter, DateTimect, HourPST, Result_est - get clean variable names 
#str(d2)
#SPREAD via TIDYR package takes two columns (key & value) and spreads in to multiple columns, it makes "long" data wide
#d2_wide <- spread(data = d2, key = Parameter, value = Result_est)

#Select only necessary columns and place in order that worksfor TIDYR function SPREAD
#Include DateTimelt in order to extract month and julian day - consider adding month or julian day as an explanatory varible
d2 <-   data.frame(d1[9],d1[17],d1[18], d1[8],d1[11]);
set.seed(1);some(d2) #Parameter, DateTimect, DateTimelt, HourPST, Result_est - set.seed makes selection by some same each time
#SPREAD via TIDYR package takes two columns (key & value) and spreads in to multiple columns, it makes "long" data wide
d2_wide <- spread(data = d2, key = Parameter, value = Result_est)
d2_wide$Month <- d2_wide$DateTimelt$mon + 1 #0-11: months after the first of the year - add 1 
d2_wide$Jday <- d2_wide$DateTimelt$yday +1 #0-365: day of the year - add 1
set.seed(1); some(d2_wide)

#Use data for all months for exploratory data analysis and monthly box plots - A for All
str(d1A)
d2A <- data.frame(d1A[9], d1A[16], d1A[8], d1A[11], d1A[18]) #Parameter, DateTimect, HourPST, Result_est, Month - get clean variable names 
str(d2A)
d2A_wide <- spread(data = d2A, key = Parameter, value = Result_est)
set.seed(1); some(d2A_wide, n=10L)

#Exploratory Data Analysis
#1. Plot of all DO vs time
#2. Plot of all DOPerSat vs time
#3. Plot of all Temperature vs time
#4. Plot of all pH vs time
#5. Monthly box plot of DO
#6. Monthly box plot of DOPerSat
#7. Monthly box plot of Temperature
#8. Monthly box plot of pH
#
#PLOTS OF DO, TIME OF SAMPLING, TEMPERATURE, ETC. VS. TIME
str(d2A_wide)
d2A_wide_Summer <- subset(d2A_wide,  (Month == 6 | Month == 7 | Month == 8 | Month == 9))
head(d2A_wide_Summer)

#Titles
stations <- "11485_33922_MFCoquille"
stations2  <- "MF Coquille R - 11485 and 33922"

# Plots of results over time ----------------------------------------------

#par(mfrow=c(2,1))
?plot #BaseGrafX
?par  #BaseGrafX
?lines
png(file=paste(outpath,stations,"_","DOvTime.png",sep=""), width = 1000, height = 400)
plot(d2A_wide$DateTimect, d2A_wide$DO,  main=paste("Measured Dissolved Oxygen vs. Time","-",stations2), 
     pch=21, col="black", bg="gray",
     xlim=as.POSIXct(c("1980-01-01 00:00:00 PDT","2020-01-01 00:00:00 PDT")), ylim=c(6,14), 
     xlab="Year", ylab = "DO mg/L",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
lines(d2A_wide$DateTimect, d2A_wide$DO, type="l", lty=3, col="gray")
points(d2A_wide_Summer$DateTimect, d2A_wide_Summer$DO, pch=20, cex=1.5, col="blue", bg="blue")
#axTicks(1)
abline(h = 14, lty = "dotted", lwd=1, col="gray" )
abline(h = 13, lty = "dotted", lwd=1, col="gray" )
abline(h = 12, lty = "dotted", lwd=1, col="gray" )
abline(h = 11, lty = "dotted", lwd=1, col="gray" )
abline(h = 10, lty = "dotted", lwd=1, col="gray" )
abline(h = 9, lty = "dotted", lwd=1, col="gray" )
abline(h = 8, lty = "dotted", lwd=1, col="gray" )
abline(h = 7, lty = "dotted", lwd=1, col="gray" )
abline(h = 6, lty = "dotted", lwd=1, col="gray" )
abline(v = 315561600, lty = "dotted", lwd=1, col="gray" )  #1980-01-01 Number of secs since 1970-01-01
abline(v = 631180800, lty = "dotted", lwd=1, col="gray" )  #1990-01-01
abline(v = 946713600, lty = "dotted", lwd=1, col="gray" )  #2000-01-01
abline(v = 1262332800, lty = "dotted", lwd=1, col="gray" ) #2010-01-01
abline(v = 1577865600, lty = "dotted", lwd=1, col="gray" ) #2020-01-01
#grid(nx=5,col = "gray", lty = "dotted", lwd = 1)
critx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
crity <- c(8,8)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
#legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=14.3, bty="n",  legend = "Nov-May", 
       pch=21, cex=1.3, col="black", pt.bg="gray")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=13.8, bty="n",  legend = "Jun-Sep \"Summer\"", 
       pch=20, cex=1.3, pt.cex=2.0, col="blue", bg="blue")
dev.off()

png(file=paste(outpath,stations,"_","HourPSTvTime.png",sep=""), width = 1000, height = 400)
plot(d2A_wide$DateTimect, d2A_wide$HourPST, main=paste("Sampling Time vs. Time","-",stations2), 
     pch=21, col="black", bg="gray",
     xlim=as.POSIXct(c("1980-01-01 00:00:00 PDT","2020-01-01 00:00:00 PDT")), ylim=c(6,18), 
     xlab="Year", ylab = "Sampling Time",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
lines(d2A_wide$DateTimect, d2A_wide$HourPST, type="l", lty=3, col="gray")
points(d2A_wide_Summer$DateTimect, d2A_wide_Summer$HourPST, pch=20, cex=1.5, col="blue", bg="blue")
abline(h = 18, lty = "dotted", lwd=1, col="gray" )
abline(h = 16, lty = "dotted", lwd=1, col="gray" )
abline(h = 14, lty = "dotted", lwd=1, col="gray" )
abline(h = 12, lty = "dotted", lwd=1, col="gray" )
abline(h = 10, lty = "dotted", lwd=1, col="gray" )
abline(h = 8, lty = "dotted", lwd=1, col="gray" )
abline(h = 6, lty = "dotted", lwd=1, col="gray" )
abline(v = 315561600, lty = "dotted", lwd=1, col="gray" )  #1980-01-01 Number of secs since 1970-01-01
abline(v = 631180800, lty = "dotted", lwd=1, col="gray" )  #1990-01-01
abline(v = 946713600, lty = "dotted", lwd=1, col="gray" )  #2000-01-01
abline(v = 1262332800, lty = "dotted", lwd=1, col="gray" ) #2010-01-01
abline(v = 1577865600, lty = "dotted", lwd=1, col="gray" ) #2020-01-01
critx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
crity <- c(12,12)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=18.4, bty="n",  legend = "Nov-May",
       pch=21, cex=1.3,  col="black", pt.bg="gray")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=17.7, bty="n",  legend = "Jun-Sep \"Summer\"", 
       pch=20, cex=1.3, pt.cex=2.0, col="blue", bg="blue")
dev.off()

png(file=paste(outpath,stations,"_","TvTime.png",sep=""), width = 1000, height = 400)
plot(d2A_wide$DateTimect, d2A_wide$Temperature, main=paste("Measured Temperature vs. Time","-",stations2), 
     pch=21, col="black", bg="gray",
     xlim=as.POSIXct(c("1980-01-01 00:00:00 PDT","2020-01-01 00:00:00 PDT")), ylim=c(0,30), 
     xlab="Year", ylab = "Celsius",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
lines(d2A_wide$DateTimect, d2A_wide$Temperature, type="l", lty=3, col="gray")
points(d2A_wide_Summer$DateTimect, d2A_wide_Summer$Temperature, pch=20, cex=1.5, col="blue", bg="blue")
abline(h = 30, lty = "dotted", lwd=1, col="gray" )
abline(h = 25, lty = "dotted", lwd=1, col="gray" )
abline(h = 20, lty = "dotted", lwd=1, col="gray" )
abline(h = 15, lty = "dotted", lwd=1, col="gray" )
abline(h = 10, lty = "dotted", lwd=1, col="gray" )
abline(h = 5, lty = "dotted", lwd=1, col="gray" )
abline(h = 0, lty = "dotted", lwd=1, col="gray" )
abline(v = 315561600, lty = "dotted", lwd=1, col="gray" )  #1980-01-01 Number of secs since 1970-01-01
abline(v = 631180800, lty = "dotted", lwd=1, col="gray" )  #1990-01-01
abline(v = 946713600, lty = "dotted", lwd=1, col="gray" )  #2000-01-01
abline(v = 1262332800, lty = "dotted", lwd=1, col="gray" ) #2010-01-01
abline(v = 1577865600, lty = "dotted", lwd=1, col="gray" ) #2020-01-01
critx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
crity <- c(18,18)
lines(critx,crity, type="l", lty=2, lwd=2, col="blue")
crity <- c(13,13)
lines(critx,crity, type="l", lty=2, lwd=2, col="gray")
#legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=31, bty="n",  legend = "Nov-May", 
       pch=21, cex=1.3, col="black", pt.bg="gray")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=29, bty="n",  legend = "Jun-Sep \"Summer\"", 
       pch=20, cex=1.3, pt.cex=2.0, col="blue", bg="blue")
legend(x=as.POSIXct("2014-01-01 00:00:00 PDT"), y=31, bty="n",  legend = "18C criterion", 
       cex=1.3,lty=2, lwd=2, col="blue")
legend(x=as.POSIXct("2014-01-01 00:00:00 PDT"), y=29, bty="n",  legend = "13C criterion", 
       cex=1.3,lty=2, lwd=2, col="gray")
dev.off()

png(file=paste(outpath,stations,"_","pHvTime.png",sep=""), width = 1000, height = 400)
plot(d2A_wide$DateTimect, d2A_wide$pH, main=paste("Measured pH vs. Time","-",stations2), 
     pch=21, col="black", bg="gray",
     xlim=as.POSIXct(c("1980-01-01 00:00:00 PDT","2020-01-01 00:00:00 PDT")), ylim=c(6,9), 
     xlab="Year", ylab = "pH su",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
lines(d2A_wide$DateTimect, d2A_wide$pH, type="l", lty=3, col="gray")
points(d2A_wide_Summer$DateTimect, d2A_wide_Summer$pH, pch=20, cex=1.5, col="blue", bg="blue")
abline(h = 9, lty = "dotted", lwd=1, col="gray" )
abline(h = 8.5, lty = "dotted", lwd=1, col="gray" )
abline(h = 8, lty = "dotted", lwd=1, col="gray" )
abline(h = 7.5, lty = "dotted", lwd=1, col="gray" )
abline(h = 7, lty = "dotted", lwd=1, col="gray" )
abline(h = 6.5, lty = "dotted", lwd=1, col="gray" )
abline(h = 6, lty = "dotted", lwd=1, col="gray" )
abline(v = 315561600, lty = "dotted", lwd=1, col="gray" )  #1980-01-01 Number of secs since 1970-01-01
abline(v = 631180800, lty = "dotted", lwd=1, col="gray" )  #1990-01-01
abline(v = 946713600, lty = "dotted", lwd=1, col="gray" )  #2000-01-01
abline(v = 1262332800, lty = "dotted", lwd=1, col="gray" ) #2010-01-01
abline(v = 1577865600, lty = "dotted", lwd=1, col="gray" ) #2020-01-01
critx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
crity <- c(8.5,8.5)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
crity <- c(6.5,6.5)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=9.1, bty="n",  legend = "Nov-May", 
       pch=21, cex=1.3, col="black", pt.bg="gray")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=8.9, bty="n",  legend = "Jun-Sep \"Summer\"", 
       pch=20, cex=1.3, pt.cex=2.0, col="blue", bg="blue")
legend(x=as.POSIXct("2013-01-01 00:00:00 PDT"), y=9.1, bty="n",  legend = "6.5-8.5 criteria", 
       cex=1.3,lty=2, lwd=2, col="black")
#legend(x=as.POSIXct("2016-01-01 00:00:00 PDT"), y=29, bty="n",  legend = "13C crit", 
#       cex=1.3,lty=2, lwd=2, col="gray")
dev.off()


# Derive Month_order factor for Box Plots ---------------------------------
#BOX PLOTS
head(d2A_wide)
Month_orderA <- factor(d2A_wide$Month,levels=c(1:12), ordered=TRUE) #all months plotted, including those w/ no data
Month_order <- factor(d2A_wide$Month,levels=c(1:12), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) #all months plotted, including those w/ no data

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_DO.png",sep=""), width = 665, height = 500) #dev.off()
#plot(DO~Month_order,ylim=c(6,max(DO,na.rm=TRUE)+0.5),ylab="DO mg/L",xlab="Month",
#     notch=TRUE,
#     main=paste("Dissolved Oxygen","-", stations2),lty=1,las=1,varwidth = TRUE,
#     cex.axis=1.2, cex.lab=1.2) 
# if notch is TRUE, a notch is drawn in each side of the boxes. If the notches of two plots do not overlap 
# this is 'strong evidence' that the two medians differ
plot(d2A_wide$DO~Month_order,ylim=c(6,max(d2A_wide$DO,na.rm=TRUE)+0.5),ylab="DO mg/L",xlab="Month",
     main=paste("Dissolved Oxygen","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
#plot(d2A_wide$DO~Month_order,ylim=c(6,14),ylab="DO mg/L",xlab="Month",
#     main=paste("Dissolved Oxygen","-", stations2),lty=1,las=1,varwidth = FALSE,
#     cex.axis=1.2, cex.lab=1.2) 
#?boxplot #alternative, produces same plot
#boxplot(d2A_wide$DO~Month_order, ylim=c(6,max(d2A_wide$DO,na.rm=TRUE)+0.5),ylab="DO mg/L",xlab="Month",
#        main=paste("Dissolved Oxygen","-", stations2),lty=1,las=1,varwidth = TRUE,
#        cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
#CRITERIA: 
#  SUMMER_MAX	18
#  USE_CODE	3
#  SPN_FINAL	October 15-May 15
critx <- c(5,10) #REARING AND MIGRATION
crity <- c(8.,8.) #30-day mean minimum as defined in OAR 340-41-006
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
crity <- c(6.5,6.5) #7-day min mean criterion"
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
crity <- c(6.,6.) #Absolute min criterion
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(0,5) #SPAWNING
crity <- c(11.,11.) #7-day mean minimum as defined in OAR 340-41-006
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
crity <- c(9.,9.) #if IGDO >= 8.0 then 7-day mean minimum as defined in OAR 340-41-006
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(10,13) #Spawning Criteria
crity <- c(11.,11.) # 7-day mean minimum as defined in OAR 340-41-006
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
crity <- c(9.,9.) #if IGDO >= 8.0 then 7-day mean minimum as defined in OAR 340-41-006
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.2, 7.5,labels = "Oct 15 to May 15 - Spawning",cex = 0.9, adj=c(0,0))
text(0.5, 7.25,labels = "7-day mean min = 11.0",cex = 0.9, adj=c(0,0))
text(0.5, 7.0,labels = " (if IGDO>8 then 9.0)",cex = 0.9, adj=c(0,0))
text(0.2, 6.65,labels = "May 15 to Oct 15 - Rearing/Migration",cex = 0.9, adj=c(0,0))
text(0.5, 6.4,labels = "30-day mean min = 8.0",cex = 0.9, adj=c(0,0))
text(0.5, 6.15,labels = "7-day min mean = 6.5",cex = 0.9, adj=c(0,0))
text(0.5, 5.9,labels = "Absolute min = 6.0",cex = 0.9, adj=c(0,0))
dev.off()
#
#PERCENT DO SATURATION
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_DOPerSat.png",sep=""), width = 665, height = 500)
plot(d2A_wide$DOPerSat~Month_order,ylim=c(min(d2A_wide$DOPerSat,na.rm=TRUE)-5,max(d2A_wide$DOPerSat,na.rm=TRUE)+10),
     ylab="DO % Sat",xlab="Month",
     main=paste("DO Percent Saturation","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(5,10) #REARING AND MIGRATION
crity <- c(90.,90.) #90% sat applies if 30-day mean minimum < 8.0
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(0,5) #SPAWNING
crity <- c(95.,95.) #95% sat applies if 7-day mean minimum < 11.0/9.0
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(10,13) #Spawning Criteria
crity <- c(95.,95.)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.2, 118,labels = "Oct 15 to May 15 - Spawning",cex = 0.9, adj=c(0,0))
text(0.5, 116.5,labels = "If DO<11.0 then 95% sat applies",cex = 0.9, adj=c(0,0))
text(0.2, 114,labels = "May 15 to Oct 15 - Rearing/Migration",cex = 0.9, adj=c(0,0))
text(0.5, 112.5,labels = "If DO<8.0 then 90% sat applies",cex = 0.9, adj=c(0,0))
dev.off()
#
#TEMPERATURE
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_Temperature.png",sep=""), width = 665, height = 500)
plot(d2A_wide$Temperature~Month_order,ylim=c(min(d2A_wide$Temperature,na.rm=TRUE)-5,max(d2A_wide$Temperature,na.rm=TRUE)+5),
     ylab="Celsius",xlab="Month",
     main=paste("Temperature","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(5,10) #REARING AND MIGRATION
crity <- c(18.,18.) #90% sat applies if 30-day mean minimum < 8.0
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(0,5) #SPAWNING
crity <- c(13.,13.) #95% sat applies if 7-day mean minimum < 11.0/9.0
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(10,13) #Spawning Criteria
crity <- c(13.,13.)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.2, 30,labels = "May 15 to Oct 15 - Rearing/Migration BBNC = 18C",cex = 0.9, adj=c(0,0))
text(0.2, 28.5,labels = "Oct 15 to May 15 - Spawning BBNC = 13C",cex = 0.9, adj=c(0,0))
dev.off()
#
#PH
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_pH.png",sep=""), width = 665, height = 500)
plot(d2A_wide$pH~Month_order,ylim=c(min(d2A_wide$pH,na.rm=TRUE)-.5,max(d2A_wide$pH,na.rm=TRUE)+0.5),
     ylab="pH su",xlab="Month",
     main=paste("pH","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
#Water Quality Standards and Policies for South Coast Basin
#pH (Hydrogen ion concentration) pH values may not fall outside the following ranges:
#  Estuarine and fresh waters: 6.5-8.5.
critx <- c(0,13)
crity <- c(8.5,8.5)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
crity <- c(6.5,6.5)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.2, 6.08,labels = "pH may not fall outside range 6.5 to 8.5",cex = 0.9, adj=c(0,0))
text(0.2, 6.0,labels = "(South Coast Basin - Estuarine and fresh waters)",cex = 0.9, adj=c(0,0))
dev.off()

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_OrthophosphateP.png",sep=""), width = 665, height = 500)
plot(d2A_wide$OrthophosphateP~Month_order,ylim=c(min(d2A_wide$OrthophosphateP,na.rm=TRUE)-0.0,max(d2A_wide$OrthophosphateP,na.rm=TRUE)+0.0),
     ylab="mg/L",xlab="Month",
     main=paste("Orthophosphate, as P","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(0,13)
crity <- c(0.005,0.005)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 0.028,labels = "0.005 mg/L (5 ug/L) is est. conc. at",cex = 0.9, adj=c(0,0))
text(0.5, 0.027,labels = "which P limitation may be significant",cex = 0.9, adj=c(0,0))
dev.off()

str(d2A_wide)
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_TP.png",sep=""), width = 665, height = 500)
plot(d2A_wide$TP~Month_order,ylim=c(min(d2A_wide$TP,na.rm=TRUE)-0.0,max(d2A_wide$TP,na.rm=TRUE)+0.0),
     ylab="mg/L",xlab="Month",
     main=paste("Total Phosphorus","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
dev.off()

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_Chlorophylla.png",sep=""), width = 665, height = 500)
plot(d2A_wide$Chlorophylla~Month_order,ylim=c(min(d2A_wide$Chlorophylla,na.rm=TRUE)-0.0,15),
     ylab="mg/L",xlab="Month",
     main=paste("Chlorophyll a","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(0,13)
crity <- c(15,15)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 14.5,labels = "15 ug/L is action level",cex = 0.9, adj=c(0,0))
text(0.5, 14.05,labels = "Note that benthic algae also important",cex = 0.9, adj=c(0,0))
dev.off()

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_BOD5.png",sep=""), width = 665, height = 500)
plot(d2A_wide$BOD5~Month_order,ylim=c(min(d2A_wide$BOD5,na.rm=TRUE)-0.0,max(d2A_wide$BOD5,na.rm=TRUE)+0.0),
     ylab="mg/L",xlab="Month",
     main=paste("BOD5","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
dev.off()

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_TOC.png",sep=""), width = 665, height = 500)
plot(d2A_wide$TOC~Month_order,ylim=c(min(d2A_wide$TOC,na.rm=TRUE)-0.0,max(d2A_wide$TOC,na.rm=TRUE)+0.0),
     ylab="mg/L",xlab="Month",
     main=paste("TOC","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
dev.off()

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_AmmoniaN.png",sep=""), width = 665, height = 500)
plot(d2A_wide$AmmoniaN~Month_order,ylim=c(min(d2A_wide$AmmoniaN,na.rm=TRUE)-0.0,max(d2A_wide$AmmoniaN,na.rm=TRUE)+0.0),
     ylab="mg/L",xlab="Month",
     main=paste("Ammonia, as N","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
dev.off()

#Derive dissolved inorganic nitrogen for observations where both ammonia and nitrate/nitrite measured
d2A_wide_DIN_1 <- d2A_wide #Calculate dissolved inorganic nitrogen in order to generate box plot
d2A_wide_DIN_2 <- subset(d2A_wide_DIN_1, AmmoniaN != "NA"); str(d2A_wide_DIN_2)
d2A_wide_DIN_3 <- subset(d2A_wide_DIN_2, NitrateNitriteN != "NA"); str(d2A_wide_DIN_3)
d2A_wide_DIN <- d2A_wide_DIN_3
d2A_wide_DIN$DIN <- d2A_wide_DIN$AmmoniaN + d2A_wide_DIN$NitrateNitriteN; d2A_wide_DIN$DIN
#Plot DIN
head(d2A_wide_DIN)
#detach()
#attach(d2A_wide_DIN) # d2A_wide_DIN$
Month_orderAN <- factor(d2A_wide_DIN$Month,levels=c(1:12), ordered=TRUE)#all months plotted, including those w/ no data
Month_orderN <- factor(d2A_wide_DIN$Month,levels=c(1:12), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) #all months plotted, including those w/ no data
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_DIN.png",sep=""), width = 665, height = 500)
plot(d2A_wide_DIN$DIN~Month_orderN,ylim=c(min(d2A_wide_DIN$DIN,na.rm=TRUE)-0.0, 0.7),
     ylab="DIN mg/L",xlab="Month",
     main=paste("Dissolved Inorganic N","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(0,13)
crity <- c(0.05,0.05)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 0.02,labels = "0.05 mg/L (50 ug/L) is est. conc. below",cex = 0.9, adj=c(0,0))
text(0.5, 0.00,labels = "which N limitation may be significant",cex = 0.9, adj=c(0,0))
dev.off()

#Generate box plot for DIN to PO4P ratio
d2A_wide_NtoP <- subset(d2A_wide_DIN, OrthophosphateP != "NA"); str(d2A_wide_NtoP)
d2A_wide_NtoP$NtoPratio <- d2A_wide_NtoP$DIN / d2A_wide_NtoP$OrthophosphateP
d2A_wide_NtoP$NtoPratio

Month_orderAN <- factor(d2A_wide_NtoP$Month,levels=c(1:12), ordered=TRUE)#all months plotted, including those w/ no data
Month_orderN <- factor(d2A_wide_NtoP$Month,levels=c(1:12), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) #all months plotted, including those w/ no data

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_NtoP.png",sep=""), width = 665, height = 500)
plot(d2A_wide_NtoP$NtoPratio~Month_orderN,ylim=c(0.0, 100.),
     ylab="DIN:PO4P ratio",xlab="Month",
     main=paste("Dissolved Inorganic N to Dissolved Orthophosphate P","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(0,13)
crity <- c(7,7)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 10,labels = "Ratio >> 7 P limited",cex = 0.9, adj=c(0,0))
text(0.5, 3,labels = "Ratio << 7 N limited",cex = 0.9, adj=c(0,0))
dev.off()


#REMOVE A FEW RECORDS
#Delete records for which DO is NA
str(d2_wide)
d3_wide <- subset(d2_wide, DO != "NA"); str(d3_wide)
#Delete 1 high flow record (6/9/1993 SF Coquille R Flow rate = 847 cfs)
d4_wide <- subset(d3_wide, QatSFCoqPowers < 200); str(d4_wide)

#DERIVE ORGANIC P, E COLI FROM FECAL COLIFORM, DISSOLVED ORGANIC N , AND ORGANIC N 

d4_wide$OrgP <- d4_wide$TP - d4_wide$OrthophosphateP
d4_wide$OrgP <- ifelse(d4_wide$OrgP < 0, 0, d4_wide$OrgP)

d4_wide$EcoliEst <- 0.531*d4_wide$Fecalcoliform^1.06 #Curtis Cude equation to estimate E. coli from Fecal Coliform
d4_wide$Ecoli2 <- ifelse(is.na(d4_wide$Ecoli)==T, d4_wide$EcoliEst, d4_wide$Ecoli) #use estimate if E. coli not available

d4_wide$DIN <- d4_wide$AmmoniaN + d4_wide$NitrateNitriteN;  

d4_wide$OrgN <- d4_wide$TKN - d4_wide$AmmoniaN #TKN not measured recently, so Organic N cannot be derived for recent data

d4_wide$TN <- d4_wide$TKN + d4_wide$NitrateNitriteN

d4_wide$Time <- d4_wide$HourPST/24

head(d4_wide)
colnames(d4_wide)

str(d4_wide)
write.table(d4_wide,"11485_33922_MFCoquille_1982to2017viaAWQMSandLASAR_withDO_d4_wide.csv", sep=",", row.names = FALSE) #short column headers

#reorg columns
#d5_wide <- d4_wide[,c(1,29,8,9, 16,20,13,23,28,26,3,12,27, 4, 6,18,21,22,25, 5,15)]
d5_wide  <- d4_wide[,c(1,32, 9,10,17,21,14,26,31,29, 4,13,30, 5, 7,19,22,23,28, 6,16,24,25)] #Added Month and Jday
#                      1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23      
str(d5_wide)
#1  DateTimect     
#2  Time        
#3  DO             
#4  DOPerSat       
#5  Temperature    
#6  TP             
#7  OrthophosphateP
#8  OrgP           
#9  TN             
#10 DIN            
#11 AmmoniaN       
#12 NitrateNitriteN
#13 OrgN           
#14 BOD5           
#15 COD            
#16 TOC            
#17 TSS            
#18 Turbidity      
#19 Ecoli2         
#20 Chlorophylla   
#21 QatSFCoqPowers
#22 Month
#23 Jday
write.table(d5_wide,"11485_33922_MFCoquille_1982to2017viaAWQMSandLASAR_withDO_d5_wide.csv", sep=",", row.names = FALSE) #short column headers

png(file=paste(outpath,stations,"_","ggpairs_loess_DOvAll.png",sep=""), width = 1000, height = 1000)
ggpairs(data=d5_wide, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
dev.off()

#Remove records from d4_wide which lack OrthophosphateP data
d4B <- subset(d4_wide, OrthophosphateP != "NA"); str(d4B)
#Remove records from d4_wide which lack BOD5 data
#d4C <- subset(d4B, BOD5 != "NA"); str(d4C)
#Remove records from d4_wide which lack DIN data
#d4D <- subset(d4C, DIN != "NA"); str(d4D)
d4D <- subset(d4B, DIN != "NA"); str(d4D) #Use for N to P ratio
#d4D <- na.omit(d4_wide) 

#Calculate DIN to PO4P ratio
d4D$NtoPratio <- d4D$DIN / d4D$OrthophosphateP

write.table(d4D,"11485_33922_MFCoquille_1982to2017viaAWQMSandLASAR_withDOandPandBOD_wide.csv", sep=",", row.names = FALSE) #short column headers
str(d4D)

#Select columns of interest from records that all contain OrthophosphateP
colnames(d4B)
dDO <-  d4B[,c(9, 32, 17, 16, 14, 24, 25)] #includes Month and Jday as potential explanatory variables
set.seed(1);some(dDO)
#1 DO
#2 Time
#3 Temperature
#4 QatSFCoqPowers
#5 OrthophosphateP
#6 Month
#7 Jday

#MORNING DATA SUBSET
dAM <- subset(dDO, Time <= 0.5); str(dAM)
set.seed(1);some(dAM)


#OBSERVED MEANS MEDIANS AND QUANTILES

##Saturation DO
#ElevFt <- 23 
#dDO$DOsat <-(14.62- 0.3898*dDO$Temperature +(0.006969*dDO$Temperature^2)-(0.00005897*dDO$Temperature^3))*(1-(0.00000697*ElevFt))^5.167
#Regulatory DO - Sets DO > DOsat to DOsat
#dDO$DOreg <- ifelse(dDO$DO > dDO$DOsat, dDO$DOsat, dDO$DO)
#dDO$Tfuture <- dDO$DO - 2.7
#dDO$DOsatfuture <-(14.62- 0.3898*dDO$Tfuture +(0.006969*dDO$Tfuture^2)-(0.00005897*dDO$Tfuture^3))*(1-(0.00000697*ElevFt))^5.167
#dAM$DOsat <-(14.62- 0.3898*dAM$Temperature +(0.006969*dAM$Temperature^2)-(0.00005897*dAM$Temperature^3))*(1-(0.00000697*ElevFt))^5.167
##Regulatory DO - Sets DO > DOsat to DOsat
#dAM$DOreg <- ifelse(dAM$DO > dAM$DOsat, dAM$DOsat, dAM$DO)

mean(dDO$DO)
quantile(dDO$DO,c(.25, .50, .75)) #observed current
mean(dAM$DO)
quantile(dAM$DO,c(.25, .50, .75)) #observed current
#mean(dDO$DOsat)
#quantile(dDO$DOsat,c(.25, .50, .75)) #observed current
#mean(dDO$DOreg)
#quantile(dDO$DOreg,c(.25, .50, .75)) #observed current
#mean(dDO$DOsatfuture)
#quantile(dDO$DOsatfuture,c(.25, .50, .75)) #future
mean(dDO$Time)
quantile(dDO$Time,c(.25, .50, .75)) #observed current
mean(dAM$Time)
quantile(dAM$Time,c(.25, .50, .75)) #observed current
mean(dDO$Temperature)
quantile(dDO$Temperature,c(.25, .50, .75)) #observed current
mean(dAM$Temperature)
quantile(dAM$Temperature,c(.25, .50, .75)) #observed current
mean(dDO$QatSFCoqPowers)
quantile(dDO$QatSFCoqPowers,c(.25, .50, .75)) #observed current
mean(dAM$QatSFCoqPowers)
quantile(dAM$QatSFCoqPowers,c(.25, .50, .75)) #observed current
mean(dDO$OrthophosphateP)
quantile(dDO$OrthophosphateP,c(.25, .50, .75)) #observed current
mean(dAM$OrthophosphateP)
quantile(dAM$OrthophosphateP,c(.25, .50, .75)) #observed current
quantile(dDO$Jday,c(.25, .50, .75)) #observed current
mean(dDO$Month)
quantile(dDO$Month,c(.25, .50, .75)) #observed current
mean(dAM$DO)
quantile(dAM$DO,c(.25, .50, .75)) #observed current
#mean(dAM$DOreg)
#quantile(dAM$DOreg,c(.25, .50, .75)) #observed current

#TRANSFORMATIONS
#For both methods, responsed variable (DO) to be normally distribute
#For Method 1 (Koch), explanatory variables also to be normally distributed.
#For Method 2 (Pan), explanatory variables do not need to be normally distributed.
#Evaluate transformations that will produce normally distributed datasets
# Lambda ??   Transformation             Transformation
#  0         Log                        log(Y)
#  0.33      cube root                  Y^0.33
#  0.5       square root                Y^.5
#  1         linear (no transformation) Y
# -1         reciprical                 1/Y
#
#Example of normally distribute dataset, log transformation, square root transformation, reciprocal transformation
par(mfrow=c(2,2))
#xtest <- rnorm(1000, mean = 5, sd = 3)
xtest <- rnorm(1000, mean = 100, sd = 30)
shapiro.test(xtest) #if p-value > 0.05, then normally distributed
hist(xtest)
qqPlot(xtest, dist="norm", main = "Normal QQ Plot of DO" ) #Quantile-Comparison Plots - via "car" package - plot on line if normally dist
#
xtestsq <- xtest^2
hist(xtestsq)
qqPlot(xtestsq, dist="norm", main = "Normal QQ Plot" )
#
xtestexp <- exp(xtest)
hist(xtestexp)
qqPlot(xtestexp, dist="norm", main = "Normal QQ Plot" )
#
xtestrec <- 1/xtest
hist(xtestrec)
qqPlot(xtestrec, dist="norm", main = "Normal QQ Plot" )
#
xtestdf <- data.frame(xtest, xtestsq, xtestexp, xtestrec); head(xtestdf)
#
bc <- boxcox(xtest~., data=xtestdf) #Why do plots not show at 1?
bc <- boxcox(xtest~., data=xtestdf, lambda=seq(0,1,by=.05))
lambda <- bc$x[which.max(bc$y)]; lambda #Should be close to 1.0, no transformation needed, Y^1
#
bc <- boxcox(xtestsq~., data=xtestdf)
bc <- boxcox(xtestsq~., data=xtestdf, lambda=seq(0,1,by=.05))
lambda <- bc$x[which.max(bc$y)]; lambda #Should be close to 0.5, square root, Y^.5
#
bc <- boxcox(xtestexp~., data=xtestdf)
bc <- boxcox(xtestexp~., data=xtestdf, lambda=seq(-0.5,0.5,by=.05))
lambda <- bc$x[which.max(bc$y)]; lambda #Should be close to 0.0, Log, log(Y) (natural log)
#
bc <- boxcox(xtestrec~., data=xtestdf) #Plot shows singularity at -2 and -1
bc <- boxcox(xtestrec~., data=xtestdf, lambda=seq(-1.5,-0.5,by=.05))
lambda <- bc$x[which.max(bc$y)]; lambda #Should be close to -1.0, reciprical, 1/Y or Y^-1

#TRANSFORMATIONS - ALL DATA
#Note strip out Month and Jday since get different results with them in for boxcox
dDOx <- dDO[,-c(7,8)]; dDOx 
str(dDOx)

#DO
par(mfrow=c(2,2))
hist(dDO$DO, main = "Histogram of DO")
#qqnorm(Time); qqline(Time, col = "blue")
qqPlot(dDO$DO, dist="norm", main = "Normal QQ Plot of DO" ) #Quantile-Comparison Plots - via "car" package
shapiro.test(dDO$DO) #p-value > 0.05, normally distributed
#Shapiro-Wilk normality test
# data:  dDO$DO
# W = 0.97153, p-value = 0.1659
bc <- boxcox(DO~., data=dDOx)
bc <- boxcox(DO~., data=dDOx, lambda=seq(0,2,by=.1))
lambda <- bc$x[which.max(bc$y)]; lambda
# lambda 1.979798
shapiro.test(dDO$DO^lambda) #p-value > 0.05, normally distributed
#Shapiro-Wilk normality test
# data:  dDO$DO^lambda
# W = 0.97271, p-value = 0.1897
hist(dDO$DO^lambda, main = c("Histogram of DO^",lambda))
qqPlot(dDO$DO^lambda, dist="norm", main = c("Normal QQ Plot of DO^",lambda)) #Quantile-Comparison Plots - via "car" package
# lambda close to 2 so try DO^2
shapiro.test(dDO$DO^2) #p-value > 0.05, normally distributed
#Shapiro-Wilk normality test
# data:  dDO$DO^2
# W = 0.97271, p-value = 0.1896
hist(dDO$DO^2, main = "Histogram of DO^2")
qqPlot(dDO$DO^2, dist="norm", main = "Normal QQ Plot of DO^2") #Quantile-Comparison Plots - via "car" package
#Both DO and DO^2 are normally distributed according to Shapiro-Wilk normality test
#Q-Q plots look similar
#Doesn't seem to be worth it to transform DO
#Plot to file
png(file=paste(outpath,stations,"_","DO_HistandQQ.png",sep=""), width = 650, height = 650)
par(mfrow=c(2,2))
hist(dDO$DO, main = "Histogram of DO")
qqPlot(dDO$DO, dist="norm", main = "Normal QQ Plot of DO" ) #Quantile-Comparison Plots - via "car" package
hist(dDO$DO^2, main = "Histogram of DO^2")
qqPlot(dDO$DO^2, dist="norm", main = "Normal QQ Plot of DO^2") 
dev.off()
#DO NOT TRANSFORM DO

#Time
par(mfrow=c(2,2))
hist(dDO$Time, main = "Histogram of Time")
qqPlot(dDO$Time, dist="norm", main = "Normal QQ Plot of Time" ) #Quantile-Comparison Plots - via "car" package
shapiro.test(dDO$Time) #if p-value > 0.05 then normally distributed; in this case not normally distribute
bc <- boxcox(Time~., data=dDO)
bc <- boxcox(Time~., data=dDO, lambda=seq(-1,1,by=.05))
lambda <- bc$x[which.max(bc$y)]; lambda
bc <- boxcox(Time~., data=dDOx)
lambda <- bc$x[which.max(bc$y)]; lambda
#lambda close to zero suggest log
hist(log(dDO$Time), main = "Histogram of log(Time)")
qqPlot(log(dDO$Time), dist="norm", main = "Normal QQ Plot of log(Time)")
shapiro.test(log(dDO$Time)) #if p-value > 0.05 then normally distributed; in this case not normally distribute
#Time not normally distributed and transformations don't help
#DO NOT TRANSFORM TIME

#Temperature
par(mfrow=c(2,2))
hist(dDO$Temperature, main = "Histogram of Temperature")
qqPlot(dDO$Temperature, dist="norm", main = "Normal QQ Plot of Temperature")
shapiro.test(dDO$Temperature) #p-value > 0.05, normally distributed
#Shapiro-Wilk normality test
#data:  dDO$Temperature
#W = 0.9955, p-value = 0.9987
#no transformation needed
#DO NOT TRANSFORM TEMPERATURE
png(file=paste(outpath,stations,"_","T_HistandQQ.png",sep=""), width = 650, height = 350)
par(mfrow=c(1,2))
hist(dDO$Temperature, main = "Histogram of Temperature")
qqPlot(dDO$Temperature, dist="norm", main = "Normal QQ Plot of Temperature" ) #Quantile-Comparison Plots - via "car" package
dev.off()


#River flow rate (SF Coquille)
par(mfrow=c(2,2))
hist(dDO$QatSFCoqPowers, main = "Histogram of QatSFCoqPowers")
qqPlot(dDO$QatSFCoqPowers, dist="norm", main = "Normal QQ Plot of QatSFCoqPowers" )
shapiro.test(dDO$QatSFCoqPowers) # #if p-value > 0.05 then normally distributed; in this case not normally distribute
#Shapiro-Wilk normality test
# data:  dDO$QatSFCoqPowers
# W = 0.80708, p-value = 1.707e-07
bc <- boxcox(QatSFCoqPowers~., data=dDO)
bc <- boxcox(QatSFCoqPowers~., data=dDO, lambda=seq(-0.5,0.5,by=0.05))
lambda <- bc$x[which.max(bc$y)]; lambda #lambda for data without month and jday
#Note that without Month and Jday, get different result than above, Lambda ~ -0.3 vs 0.0
bc <- boxcox(QatSFCoqPowers~., data=dDOx)
lambda <- bc$x[which.max(bc$y)]; lambda #use this lambda
lambda <- round(bc$x[which.max(bc$y)],2); lambda
hist(dDO$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dDO$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
shapiro.test(dDO$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
lambda <- -0.3 #round up
hist(dDO$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dDO$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
shapiro.test(dDO$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
lambda <- -0.33 # reciprocal of cube root
hist(dDO$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dDO$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
shapiro.test(dDO$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
hist(log(dDO$QatSFCoqPowers), main = "Histogram of log(Q)")
qqPlot(log(dDO$QatSFCoqPowers), dist="norm", main = "Normal QQ Plot of log Q")
shapiro.test(log(dDO$QatSFCoqPowers)) #NORMALLY DISTRIBUTED
png(file=paste(outpath,stations,"_","Trans_Q.png",sep=""), width = 650, height = 650)
par(mfrow=c(2,2))
hist(dDO$QatSFCoqPowers, main = "Histogram of QatSFCoqPowers")
qqPlot(dDO$QatSFCoqPowers, dist="norm", main = "Normal QQ Plot of QatSFCoqPowers" )
hist(dDO$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dDO$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
dev.off()
#TRANSFORM RIVER FLOW VIA LAMBDA = -0.3
lambda <- -0.3
dDO$transQ <- dDO$QatSFCoqPowers^lambda
shapiro.test(dDO$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
#Shapiro-Wilk normality test
#data:  dDO$QatSFCoqPowers^lambda
#W = 0.99108, p-value = 0.9373
hist(dDO$QatSFCoqPowers^lambda, main = c("Histogram of Q^", lambda))
qqPlot(dDO$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^", lambda))
#

#Orthophosphate
par(mfrow=c(2,2))
x=1:14; x
x2= 0.001*x; x2
hist(dDO$OrthophosphateP, breaks = x2, main = "Histogram of OrthophosphateP")
qqPlot(dDO$OrthophosphateP, dist="norm", main = "Normal QQ Plot of OrthophosphateP" )
shapiro.test(dDO$OrthophosphateP) #not normally distributed
bc <- boxcox(OrthophosphateP~., data=dDO)
lambda <- bc$x[which.max(bc$y)]; lambda
bc <- boxcox(OrthophosphateP~., data=dDOx)
lambda <- bc$x[which.max(bc$y)]; lambda
#since close to zero use log
hist(log(dDO$OrthophosphateP), main = "Histogram of log OrthophosphateP")
qqPlot(log(dDO$OrthophosphateP), dist="norm", main = "Normal QQ Plot of Log(OrthophosphateP)" )
shapiro.test(log(dDO$OrthophosphateP))# not normally distributed
#No success transforming
#DO NOT TRANSFORM Orthophosphate



#TRANSFORMATIONS - AM DATA ONLY
#DO

par(mfrow=c(2,2))
hist(dAM$DO, main = "Histogram of DO")
qqPlot(dAM$DO, dist="norm", main = "Normal QQ Plot of DO" ) #Quantile-Comparison Plots - via "car" package
shapiro.test(dAM$DO) #p-value > 0.05, normally distributed
bc <- boxcox(DO~., data=dAM)
#bc <- boxcox(DO~., data=dAM, lambda=seq(-1,1,by=.1))
lambda <- bc$x[which.max(bc$y)]; lambda ##  0 Log log(Y)
hist(dAM$DO^lambda, main = c("Histogram of DO^",lambda))
qqPlot(dAM$DO^lambda, dist="norm", main = c("Normal QQ Plot of DO^",lambda))
shapiro.test(dAM$DO^lambda) #p-value > 0.05, normally distributed
bc <- boxcox(DO~., data=dAMx) # with month and jday
lambda <- bc$x[which.max(bc$y)]; lambda ##  0 Log log(Y)
shapiro.test(log(dAM$DO)) #p-value > 0.05, normally distributed
hist(log(dAM$DO), main = "Histogram of logDO")
qqPlot(log(dAM$DO), dist="norm", main = "Normal QQ Plot of log DO") #Quantile-Comparison Plots - via "car" package
png(file=paste(outpath,stations,"_","DO_AM_HistandQQ.png",sep=""), width = 650, height = 650)
par(mfrow=c(2,2))
hist(dAM$DO, main = "Histogram of DO")
qqPlot(dAM$DO, dist="norm", main = "Normal QQ Plot of DO" ) #Quantile-Comparison Plots - via "car" package
hist(log(dAM$DO), main = "Histogram of logDO")
qqPlot(log(dAM$DO), dist="norm", main = "Normal QQ Plot of log DO") #Quantile-Comparison Plots - via "car" package
dev.off()
#Both untransformed and log transformed pass Shapiro
#Log of AM DO provides small improvement in normality
#Don't tranform DO

#Time
par(mfrow=c(2,2))
hist(dAM$Time, main = "Histogram of Time")
qqPlot(dAM$Time, dist="norm", main = "Normal QQ Plot of Time" ) #Quantile-Comparison Plots - via "car" package
shapiro.test(dAM$Time) #p-value > 0.05, normally distributed
bc <- boxcox(Time~., data=dAM)
bc <- boxcox(Time~., data=dAM, lambda=seq(1,2,by=.05))
lambda <- bc$x[which.max(bc$y)]; lambda
shapiro.test(dAM$Time^lambda) #p-value > 0.05, normally distributed
hist(dAM$Time^lambda, main = c("Histogram of Time^",lambda))
qqPlot(dAM$Time^lambda, dist="norm", main = c("Normal QQ Plot of Time^",lambda))
#Time not normally distributed and transformations don't help
#DO NOT TRANSFORM TIME

#Temperature
par(mfrow=c(2,2))
hist(dAM$Temperature, main = "Histogram of Temperature")
qqPlot(dAM$Temperature, dist="norm", main = "Normal QQ Plot of Temperature")
shapiro.test(dAM$Temperature) #p-value > 0.05, normally distributed
#no transformation needed
#DO NOT TRANSFORM TEMPERATURE

#River flow rate (SF Coquille)
hist(dAM$QatSFCoqPowers, main = "Histogram of QatSFCoqPowers")
qqPlot(dAM$QatSFCoqPowers, dist="norm", main = "Normal QQ Plot of QatSFCoqPowers" )
shapiro.test(dAM$QatSFCoqPowers) # p-value not > 0.05 so not normally distribute
bc <- boxcox(QatSFCoqPowers~., data=dAM)
bc <- boxcox(QatSFCoqPowers~., data=dAM, lambda=seq(-1,0,by=.1)) # lambda = -0.3
lambda <- bc$x[which.max(bc$y)]; lambda
lambda <- round(bc$x[which.max(bc$y)],2); lambda
shapiro.test(dAM$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
hist(dAM$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dAM$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
#Repeat with dAMx (w/o jday and month)
bc <- boxcox(QatSFCoqPowers~., data=dAMx)
lambda <- bc$x[which.max(bc$y)]; lambda
lambda <- round(bc$x[which.max(bc$y)],2); lambda
shapiro.test(dAM$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
hist(dAM$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dAM$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
lambda <- -0.2
shapiro.test(dAM$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
hist(dAM$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dAM$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
lambda <- -0.3
shapiro.test(dAM$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
hist(dAM$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dAM$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
lambda <- -0.4
shapiro.test(dAM$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
hist(dAM$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dAM$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
lambda <- -0.5
shapiro.test(dAM$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
hist(dAM$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dAM$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
lambda <- -0.6 #Looks like the best
shapiro.test(dAM$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
hist(dAM$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dAM$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
lambda <- -0.7
shapiro.test(dAM$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
hist(dAM$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dAM$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
png(file=paste(outpath,stations,"_","Trans_AM_Q.png",sep=""), width = 650, height = 650)
par(mfrow=c(2,2))
hist(dAM$QatSFCoqPowers, main = "Histogram of QatSFCoqPowers")
qqPlot(dAM$QatSFCoqPowers, dist="norm", main = "Normal QQ Plot of QatSFCoqPowers" )
lambda <- -0.6
shapiro.test(dAM$QatSFCoqPowers^lambda) #NORMALLY DISTRIBUTED
hist(dAM$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dAM$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
dev.off()
#TRANSFORM AM FLOW VIA LAMBDA = -0.6
lambda <- -0.6
dAM$transamQ <- dAM$QatSFCoqPowers^lambda; dAM
hist(dAM$QatSFCoqPowers^lambda, main = c("Histogram of Q^",lambda))
qqPlot(dAM$QatSFCoqPowers^lambda, dist="norm", main = c("Normal QQ Plot of Q^",lambda))
shapiro.test(dAM$QatSFCoqPowers^lambda)

#Orthophosphate
par(mfrow=c(2,2))
hist(dAM$OrthophosphateP, main = "Histogram of OrthophosphateP")
qqPlot(dAM$OrthophosphateP, dist="norm", main = "Normal QQ Plot of OrthophosphateP" )
x=1:14; x
x2= 0.001*x; x2
hist(dAM$OrthophosphateP, breaks = x2, main = "Histogram of OrthophosphateP")
qqPlot(dAM$OrthophosphateP, dist="norm", main = "Normal QQ Plot of OrthophosphateP" )
shapiro.test(dAM$OrthophosphateP) #not normally distributed
bc <- boxcox(OrthophosphateP~., data=dAM)
bc <- boxcox(OrthophosphateP~., data=dAM, lambda=seq(0,1,by=.1)) # lambda = -0.3
lambda <- bc$x[which.max(bc$y)]; lambda
hist(dAM$OrthophosphateP^lambda, main = "Histogram of log OrthophosphateP")
qqPlot(dAM$OrthophosphateP^lambda, dist="norm", main = "Normal QQ Plot of Log(OrthophosphateP)" )
shapiro.test(dAM$OrthophosphateP^lambda)# not normally distributed
#No success transforming
#DO NOT TRANSFORM Orthophosphate


#METHOD 1 - ADDITIVE METHOD A LA ROY KOCH CE566/ESR566 CLASS
#1.tranform all variables (response and explanatory) so that normally distributed
#2.correlate response variable with explanatory variables
#3.1 parameter model via highest correlated explanatory variable
#4.Check if p<0.05
#5.Check residuals for normality (good) and heteroscedastidity (bad)
#6.Correlate residuals with remaining parameters
#7.2 parm model with parameter most highly correlated with residuals added
#8.Repeat
#dDO$transQ <- dDO$QatSFCoqPowers^-0.3
#dDO$transBOD5 <- dDO$BOD5^0.7
str(dDO)
#dDO3 <- dDO[,c(1,2,3,7,5,8)]; str(dDO3)
#dDO3 <- dDO[,c(1,2,3,9,5,10)]; str(dDO3)
dDO3 <- dDO[,c(1,2,3,8,5)]; str(dDO3)

pairs(dDO3, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)

png(file=paste(outpath,stations,"_","Pairs_DOvTimeTtransQP.png",sep=""), width = 650, height = 650)
pairs(dDO3, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor) #transformed
dev.off()
png(file=paste(outpath,stations,"_","ggpairs_loess_DOvTimeTtransQP.png",sep=""), width = 650, height = 650)
ggpairs(data=dDO3, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
dev.off()

mod.DOvTime<- lm(DO~Time, data=dDO3)
summary(mod.DOvTime)
# prediction equation
# y = b + mx + error
# DO =  b +  m * Time
#   Pr(>|t|) is probability that result is due to chance
#   if p<0.05 we say it is significant at 5% level.
#   "The strongest correlation is with time.  The p-value of the slope for the regression with time 
#    is <0.05 (p-value is the probability of obtaining the computed test statistic, or one even less likely, 
#    when the null hypothesis is true).   Since the p-value is less than 0.05, the null hypothesis, Ho, 
#    that the slope = 0 is rejected (<5% probability of rejecting Ho when Ho is actually true).  
#    Therefore, the correlation is statistically significant."     
#
slope <- coefficients(mod.DOvTime)[2]; slope
intercept <- coefficients(mod.DOvTime)[1]; intercept
confint(mod.DOvTime) # 95% condfidence interval for the intercept and slope
coef(mod.DOvTime) # p. 359 extracting info from model objects
summary(mod.DOvTime)
resid(mod.DOvTime)
summary(mod.DOvTime)[[3]] #Residuals?
summary(mod.DOvTime)[[6]] #Residual standard error
summary(mod.DOvTime)[[8]] #coef of determination = r squared
summary(mod.DOvTime)[[11]] #
mod.DOvTime[[12]]
shapiro.test(resid(mod.DOvTime)) #p-value > 0.05, normally distributed residuals
#  Shapiro-Wilk normality test
#  data:  resid(mod.DOvTime)
#  W = 0.98532, p-value = 0.6967
#Plots to evaluate residuals 
png(file=paste(outpath,stations,"_","Model_1parm_residuals.png",sep=""), width = 500, height = 500)
par(mfrow=c(2,2))
plot(mod.DOvTime)
dev.off()
#Plot calc DO vs Time via base graphics
par(mfrow=c(1,1))
png(file=paste(outpath,stations,"_","Model_1parm_DOfTime_DOvTime.png",sep=""), width = 500, height = 500) #dev.off()
plot(dDO3$Time, dDO3$DO,  main="Model Calculated DO vs. Time - DO = f(Time)",
     pch=21, col="black", bg="gray",
     xlim=c(.2,.7), ylim=c(6,11), xlab = "Time", ylab = "DO mg/L",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
abline(mod.DOvTime,col="red") #abline(intercept, slope)
ci.lines(mod.DOvTime, colorci="blue")
dev.off()
#Plot model calc DO vs obs DO
s <- summary(mod.DOvTime)
names(s)
round(s$coefficients,3)
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,stations,"_","Model_1parm_DOfTime.png",sep=""), width = 500, height = 500)
plot(dDO3$DO, fitted(mod.DOvTime), main="Model Calculated vs. Observed - DO = f(Time)", 
     pch=21, col="black", bg="gray",
     xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#Correlate residuals vs remaining explanatory variables
s$residuals
d7 <- cbind(s$residuals,dDO3); d7
str(d7)
d8 <- d7[,c(1,4,5,6)]; d8
names(d8) <- c("Residuals","Temperature","transQ", "OrthophosphateP"); head(d8)    
png(file=paste(outpath,stations,"_","Pairs_1parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d8, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
#Temperature and BOD5 have highest correlation coefficients for residuals vs. remaining parameters
dev.off()
png(file=paste(outpath,stations,"_","ggpairs_1parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d8, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
dev.off()

#2 parm model - Time and Temperature
mod.DOvTimeT <- lm(DO~Time+Temperature, data=dDO3)
s=summary(mod.DOvTimeT); s
#s1=s[[4]]; s1
#s1[2,2]
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
par(mfrow=c(2,2))
plot(mod.DOvTimeT)
mod.DOvTimeT[[12]]
#Plot via base graphics
png(file=paste(outpath,stations,"_","Model_2parm_DOfTimeT.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDO3$DO, fitted(mod.DOvTimeT), main="Model vs. Observed - DO = f(Time, T)",xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
shapiro.test(resid(mod.DOvTimeT)) #p-value > 0.05, normally distributed residuals
# Shapiro-Wilk normality test
# data:  resid(mod.DOvTimeT)
# W = 0.9928, p-value = 0.9783
png(file=paste(outpath,stations,"_","Model_2parm_residuals.png",sep=""), width = 500, height = 500)
par(mfrow=c(2,2))
plot(mod.DOvTimeT)
dev.off()
#Residuals of mod.DOvTimeT vs remaining parameters
#names(s)
round(s$coefficients,3)
s$residuals
d9 <- cbind(s$residuals,dDO3); head(d9)
d10 <- d9[,c(1,5,6)]; head(d10)
names(d10) <- c("Residuals","transQ","OrthophosphateP")
head(d10)
png(file=paste(outpath,stations,"_","Pairs_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d10, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
dev.off()
png(file=paste(outpath,stations,"_","ggpairs_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d10, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
dev.off()


# FUTURE SCENARIO A mod.DOvTimeTQ 3 parm D = f(Time, T, Flow)  --------
mod.DOvTimeTQ <- lm(DO~Time+Temperature+transQ, data=dDO3) #transQ <- QatSFCoqPowers^-0.3
s=summary(mod.DOvTimeTQ); s
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 10.99192    0.64031  17.166  < 2e-16 ***
# Time         5.30504    0.57742   9.188 7.64e-13 ***
# Temperature -0.15921    0.02756  -5.777 3.32e-07 ***
# transQ      -4.68592    1.00577  -4.659 1.95e-05 ***
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
mod.DOvTimeTQ[[12]]
#Plot via base graphics
modelname <- "mod_DOvTimeTQ" #for plot title
#png(file=paste(outpath,stations,"_","Model_3parm_DOfTimeTQ.png",sep=""), width = 500, height = 500)
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500)
par(mfrow=c(1,1))
plot(dDO3$DO, fitted(mod.DOvTimeTQ), main="Model mod.DOvTimeTQ DO vs. Observed",xlim = c(6.5,10.5), ylim=c(6.5,10.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
shapiro.test(resid(mod.DOvTimeTQ)) #p-value > 0.05, normally distributed residuals
# Shapiro-Wilk normality test
# data:  resid(mod.DOvTimeTQ)
# W = 0.97816, p-value = 0.3558
durbinWatsonTest(mod.DOvTimeTQ) # Computes residual autocorrelations and generalized Durbin-Watson 
#                                        statistics and their bootstrapped p-values.
# lag Autocorrelation D-W Statistic p-value
# 1       0.1849895       1.62918   0.122
# Alternative hypothesis: rho != 0
# since p-value >0.05, there is no evidence of serial correlation in these residuals
#
#png(file=paste(outpath,stations,"_","Model_3parm_residuals.png",sep=""), width = 500, height = 500)
png(file=paste(outpath,"Model_",modelname,"_3parm_residuals.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOvTimeTQ)
dev.off()
#Residuals of mod.DOvTimeTQ vs remaining parameters
round(s$coefficients,3)
s$residuals
d11 <- cbind(s$residuals,dDO3); head(d11)
d12 <- d11[,c(1,6)]; head(d12)
names(d12) <- c("Residuals","OrthophosphateP");head(d12)
pairs(d12, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
png(file=paste(outpath,stations,"_","Pairs_3parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d12, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
dev.off()
png(file=paste(outpath,stations,"_","ggpairs_3parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d12, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
dev.off()
#Very low correlation of OrthophosphateP with residuals
#4 parm model 
mod.DOvTimeTQP <- lm(DO~Time+Temperature+transQ+OrthophosphateP, data=dDO3)
s=summary(mod.DOvTimeTQP); s
#Pr(>|t|) 0.27448 
#Don't add OrthophosphateP

#Test 3 parm model for diagnosis of Multi-collineartity
vif(mod.DOvTimeTQ)
#Variance Inflation Factor - VIF measures how much the variances of the estimated regression
# coefficients are inflated as compared to when the independent variables are not linearly related
# VIF > 4 or 5 suggests multi-collineartiy
# VIF > 10 is strong evidence that collineartiy is affecting the regression coefficients
#      Time Temperature      transQ   transBOD5 
#  1.295312    1.344258    1.080774    1.043854 
#Therefore, VIF does not suggest diagnosis of multi-collinearity


#METHOD 2 - VIA PAN ESR 566 NOTES
#Stepwise test-based procedure, backward selection
#BUILD MODEL (MEGAMODEL) WITH ALL PARAMETERS, ALL PARMS TIMES EACH OTHER, AND ALL PARMS SQUARED
#THEN USE STEP-WISE ELIMINATION TO OBTAIN SIMPLEST MODEL
#1.Transform response variable, if necessary, such that normally distributed
#2.Build initial model
#3.Check residuals for normality (good) and heteroscedastidity (bad)
#4.Select parameter with highest Pr(>|t|) for and eliminate (assuming Pr(>|t|) > 0.05)  
#  (If Y1 value has higher Pr(>|t|) than Y1*Y2 or Y1^2 do not eliminate) 
#5.Repeat until all Pr(>|t|) < 0.05
#  
#MANUAL STEP-WISE DELETION:
mod.mega1 <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                  Time*Temperature+Time*QatSFCoqPowers+Time*OrthophosphateP+
                  Temperature*QatSFCoqPowers+Temperature*OrthophosphateP+
                  QatSFCoqPowers*OrthophosphateP+
                  I(Time^2)+I(Temperature^2)+I(QatSFCoqPowers^2)+I(OrthophosphateP^2), 
                data=dDO)
str(dDO); 
modelname <- "mod_mega1" #for plot name
s=summary(mod.mega1); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.mega1)
dev.off()
shapiro.test(s$residuals) #it p-value > 0.05 then normally distributed
#Shapiro-Wilk normality test
# W = 0.97325, p-value = 0.2098
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDO$DO, fitted(mod.mega1), main="Model mod.mega1 DO vs. Observed",xlim = c(6.5,10.5), ylim=c(6.5,10.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#
#Remove terms one by one until all p-values all < 0.10
# First remove Temperature:QatSFCoqPowers with p-value = 0.6967  
mod.mega1 <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                  Time*Temperature+Time*QatSFCoqPowers+Time*OrthophosphateP+
                  Temperature*OrthophosphateP+
                  QatSFCoqPowers*OrthophosphateP+
                  I(Time^2)+I(Temperature^2)+I(QatSFCoqPowers^2)+I(OrthophosphateP^2), 
                data=dDO)
str(dDO); 
modelname <- "mod_mega1" #for plot name
s=summary(mod.mega1); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
# Repeat until get: 
mod.mega <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                 Time*OrthophosphateP, 
                data=dDO)
modelname <- "mod_mega" #for plot name
s=summary(mod.mega); s

#lm(formula = DO ~ Time + Temperature + QatSFCoqPowers + OrthophosphateP + 
#     Time * OrthophosphateP, data = dDO)
#                         Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)           1.031e+01  7.602e-01  13.557  < 2e-16 ***
#  Time                  2.657e+00  1.202e+00   2.210  0.03127 *  
#  Temperature          -1.498e-01  2.525e-02  -5.933 2.06e-07 ***
#  QatSFCoqPowers        7.137e-03  2.152e-03   3.317  0.00162 ** 
#  OrthophosphateP      -2.435e+02  9.634e+01  -2.527  0.01440 *  
#  Time:OrthophosphateP  4.546e+02  2.030e+02   2.239  0.02923 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.4253 on 55 degrees of freedom
#Multiple R-squared:  0.6851,	Adjusted R-squared:  0.6565 
#F-statistic: 23.94 on 5 and 55 DF,  p-value: 1.063e-12
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.mega)
dev.off()
shapiro.test(s$residuals) #it p-value > 0.05 then normally distributed
# W = 0.98801, p-value = 0.8146
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDO$DO, fitted(mod.mega), main="Model mod.mega1 DO vs. Observed",xlim = c(6.5,10.5), ylim=c(6.5,10.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#STEP method
#MODEL mod.step GENERATED BY STEP  - STEP model
mod.mega1 <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                  Time*Temperature+Time*QatSFCoqPowers+Time*OrthophosphateP+
                  Temperature*QatSFCoqPowers+Temperature*OrthophosphateP+
                  QatSFCoqPowers*OrthophosphateP+
                  I(Time^2)+I(Temperature^2)+I(QatSFCoqPowers^2)+I(OrthophosphateP^2), 
                data=dDO)
mod.step <- step(mod.mega1)
modelname <- "mod_step" #for plot title
s=summary(mod.step); s
#Remove I(QatSFCoqPowers^2) Time:QatSFCoqPowers 

# FUTURE SCENARIO B STEP 4-parm mod.step -----------------------------------------
mod.mega1 <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                  Time*Temperature+Time*OrthophosphateP+
                  Temperature*QatSFCoqPowers+Temperature*OrthophosphateP+
                  QatSFCoqPowers*OrthophosphateP+
                  I(Time^2)+I(Temperature^2)+I(OrthophosphateP^2), 
                data=dDO)
mod.step <- step(mod.mega1)
modelname <- "mod_step" #for plot title
coef(mod.step)
#Final model via STEP+:
#lm(formula = DO ~ Time + Temperature + QatSFCoqPowers + OrthophosphateP + Time:OrthophosphateP, 
# data = dDO)
#Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)           1.031e+01  7.602e-01  13.557  < 2e-16 ***
#  Time                  2.657e+00  1.202e+00   2.210  0.03127 *  
#  Temperature          -1.498e-01  2.525e-02  -5.933 2.06e-07 ***
#  QatSFCoqPowers        7.137e-03  2.152e-03   3.317  0.00162 ** 
#  OrthophosphateP      -2.435e+02  9.634e+01  -2.527  0.01440 *  
#  Time:OrthophosphateP  4.546e+02  2.030e+02   2.239  0.02923 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.4253 on 55 degrees of freedom
#Multiple R-squared:  0.6851,	Adjusted R-squared:  0.6565 
#F-statistic: 23.94 on 5 and 55 DF,  p-value: 1.063e-12
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.step)
dev.off()
shapiro.test(s$residuals) #it p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDO$DO, fitted(mod.step), main="Model mod.megaS DO vs. Observed",xlim = c(6.5,10.5), ylim=c(6.5,10.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#Compare 3 parameter model via Method 1 to 4 parameter via Method 2
AIC(mod.DOvTimeTQ,mod.step) # 
#                df      AIC
# mod.DOvTimeTQP  6 83.69557
# mod.step        7 76.45167
#AIC is a measure of the fit of the model; the smaller the AIC, the better the fit


#MODELS VIA MORNING DATA ONLY

#METHOD 1 - ADDITIVE METHOD A LA ROY KOCH CE566/ESR566 CLASS - AM Data
str(dAM)
dAM3 <- dAM[,c(1,2,3,8,5)]; 
str(dAM3)
pairs(dAM3, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)

#ALSO DO ANALYSIS WITH EARLY MORNING DATA SUBSET 11:00am or earlier
dAMe <- subset(dAM, Time <= 0.4583333); str(dAMe) #e for early
dAMe3 <- dAMe[,c(1,2,3,8,5)]
str(dAMe3)
pairs(dAMe3, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)


#png(file=paste(outpath,stations,"_","Pairs_AM_DOvTimeTtransQPtransBOD.png",sep=""), width = 650, height = 650)
png(file=paste(outpath,stations,"_","Pairs_AM_DOvTimeTtransQP.png",sep=""), width = 650, height = 650)
pairs(dAM3, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor) #transformed
dev.off()

png(file=paste(outpath,stations,"_","ggpairs_AM_loess_DOvTimeTtransQP.png",sep=""), width = 650, height = 650)
ggpairs(data=dAM3, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
dev.off()

png(file=paste(outpath,stations,"_","ggpairs_AMearly_loess_DOvTimeTtransQP.png",sep=""), width = 650, height = 650)
ggpairs(data=dAMe3, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
dev.off()

#1-parm model - AM only
mod.DOvTimeAM <- lm(DO~Time, data=dAM3)
#mod.DOvNAM <- lm(DO~transamDIN, data=dAM3)
summary(mod.DOvTimeAM)
shapiro.test(resid(mod.DOvTimeAM)) #p-value > 0.05, normally distributed residuals
#Plots to evaluate residuals 
png(file=paste(outpath,stations,"_","Model_AM_1parm_residuals.png",sep=""), width = 500, height = 500)
par(mfrow=c(2,2))
plot(mod.DOvTimeAM)
dev.off()
#Plot model calc DO vs obs DO
s <- summary(mod.DOvTimeAM)
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
par(mfrow=c(1,1))
png(file=paste(outpath,stations,"_","Model_AM_1parm.png",sep=""), width = 500, height = 500)
plot(dAM3$DO, fitted(mod.DOvTimeAM), main="Model Calculated DO vs. Observed - AM DO = f(Time)", 
     pch=21, col="black", bg="gray",
     xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#Plot residuals vs remaining explanatory variables
s$residuals
d7 <- cbind(s$residuals,dAM3); d7
head(d7)
d8 <- d7[,c(1,4,5,6)]
head(d8)
names(d8) <- c("Residuals","Temperature","transamQ","OrthophosphateP")    
png(file=paste(outpath,stations,"_","Pairs_AM_1parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d8, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
#transamQ has highest correlation coefficient for residuals vs. remaining parameters
dev.off()
png(file=paste(outpath,stations,"_","ggpairs_AM_1parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d8, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
dev.off()

#2 parm model - Time and transamQ
mod.DOvTimeQAM <- lm(DO~Time+transamQ, data=dAM3)
s=summary(mod.DOvTimeQAM); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
par(mfrow=c(2,2))
plot(mod.DOvTimeQAM)
mod.DOvTimeQAM[[12]]
#Plot via base graphics
png(file=paste(outpath,stations,"_","Model_AM_2parm_DOfQTime.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dAM3$DO, fitted(mod.DOvTimeQAM), main="Model Calculated DO vs. Observed - AM DO = f(Time, Q)",
     xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
shapiro.test(resid(mod.DOvTimeQAM)) #p-value > 0.05, normally distributed residuals
png(file=paste(outpath,stations,"_","Model_AM_2parm_residuals.png",sep=""), width = 500, height = 500)
par(mfrow=c(2,2))
plot(mod.DOvTimeQAM)
dev.off()
#Residuals of mod.DOvTimeQAM vs remaining parameters
d9 <- cbind(s$residuals,dAM3)
head(d9)
d10 <- d9[,c(1,4,6)]
head(d10)
names(d10) <- c("Residuals","Temperature","OrthophosphateP")
head(d10)
png(file=paste(outpath,stations,"_","Pairs_AM_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d10, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
dev.off()
png(file=paste(outpath,stations,"_","ggpairs_AM_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d10, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
dev.off()

# FUTURE SCENARIO C AM manual addition model 3-parm mod.DOvTimeTQAM ----------------------------------
#
mod.DOvTimeTQAM <- lm(DO~Time + Temperature + transamQ, data=dAM3) # FUTURE SCENARIO C mod.DOvTimeTQAM
s=summary(mod.DOvTimeTQAM); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
coef(mod.DOvTimeTQAM)
#Plot via base graphics
modelname <- "mod_DOvTimeTQAM" #for plot title
#png(file=paste(outpath,stations,"_","Model_AM_3parm.png",sep=""), width = 500, height = 500) #dev.off()
png(file=paste(outpath,modelname,"Model_AM_3parm_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500)
par(mfrow=c(1,1))
plot(dAM3$DO, fitted(mod.DOvTimeTQAM), main="Model mod.DOvTimeTQAM DO vs. Observed", xlim = c(6.5,10.5), ylim=c(6.5,10.5),
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
shapiro.test(resid(mod.DOvTimeTQAM)) #p-value > 0.05, normally distributed residuals
shapiro.test(s$residuals) #This also works - if p-value > 0.05 then normally distributed
#png(file=paste(outpath,stations,"_","Model_AM_3parm_residuals.png",sep=""), width = 500, height = 500)
png(file=paste(outpath,modelname,"Model_AM_3parm__Diagnostics.png",sep=""), width = 500, height = 500)
par(mfrow=c(2,2))
plot(mod.DOvTimeTQAM)
dev.off()
d11 <- cbind(s$residuals,dAM3)
head(d11)
d12 <- d11[,c(1,6)] #Residuals vs remaining parameter
head(d12)
names(d12) <- c("Residuals","OrthophosphateP")
head(d12)
png(file=paste(outpath,stations,"_","Pairs_AM_3parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d12, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor) #highest correlation is with Q
dev.off()
png(file=paste(outpath,stations,"_","ggpairs_AM_3parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d12, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
dev.off()
#No correlation with OrthophosphateP - DO not add





#METHOD 2 FOR AM DATA - VIA PAN ESR 566 NOTES - Time T Q P - 4-parameter STEP model AM only 
#Stepwise test-based procedure, backwards elimination
#No transformations
str(dAM)
mod.megaAM1 <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                  Time*Temperature+Time*QatSFCoqPowers+Time*OrthophosphateP+
                  Temperature*QatSFCoqPowers+Temperature*OrthophosphateP+
                  QatSFCoqPowers*OrthophosphateP+
                  I(Time^2)+I(Temperature^2)+I(QatSFCoqPowers^2)+I(OrthophosphateP^2), 
                data=dAM)
mod.stepAMa <- step(mod.megaAM1)
#DO ~ Time + Temperature + QatSFCoqPowers + OrthophosphateP + 
#  I(Time^2) + I(OrthophosphateP^2) + Time:QatSFCoqPowers + 
#  QatSFCoqPowers:OrthophosphateP
modelname <- "mod_stepAMa" #for plot name
s=summary(mod.stepAMa); s
#Note that p-value for 2 terms >0.10
#  Coefficients:
#                                   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)                      17.22483    2.81028   6.129 8.50e-07 ***
#  Time                            -33.31839   13.13209  -2.537  0.01642 *  
#  Temperature                      -0.19311    0.02994  -6.450 3.43e-07 ***
#  QatSFCoqPowers                    0.03012    0.01801   1.673  0.10449    
#  OrthophosphateP                -282.10985  118.21787  -2.386  0.02331 *  
#  I(Time^2)                        55.44342   16.22091   3.418  0.00178 ** 
#  I(OrthophosphateP^2)           9463.16580 6272.99812   1.509  0.14154    
#  Time:QatSFCoqPowers              -0.08367    0.05085  -1.645  0.10999    
#  QatSFCoqPowers:OrthophosphateP    2.69306    1.36406   1.974  0.05731 .  
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.stepAMa)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed 
# W = 0.9467, p-value = 0.05843  (MARGINAL, JUST SLIGHTLY > 0.05)
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dAM$DO, fitted(mod.stepAMa), main="Model mod.stepAMa DO vs. Observed",xlim = c(6.5,10.5), ylim=c(6.5,10.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#Remove just I(OrthophosphateP^2)   
mod.megaAM2 <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                    Time*Temperature+Time*QatSFCoqPowers+Time*OrthophosphateP+
                    Temperature*QatSFCoqPowers+Temperature*OrthophosphateP+
                    QatSFCoqPowers*OrthophosphateP+
                    I(Time^2)+I(Temperature^2)+I(QatSFCoqPowers^2), 
                  data=dAM)
mod.stepAMb <- step(mod.megaAM2)
modelname <- "mod.stepAMb" #for plot name
s=summary(mod.stepAMb); s
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)          1.533e+01  2.637e+00   5.815 1.86e-06 ***
#  Time                -3.020e+01  1.269e+01  -2.380  0.02342 *  
#  Temperature         -1.859e-01  2.859e-02  -6.503 2.55e-07 ***
#  QatSFCoqPowers       6.111e-02  2.234e-02   2.735  0.01008 *  
#  OrthophosphateP     -6.419e+01  2.451e+01  -2.619  0.01335 *  
#  I(Time^2)            5.197e+01  1.558e+01   3.336  0.00216 ** 
#  I(QatSFCoqPowers^2) -8.422e-05  4.416e-05  -1.907  0.06550 .   CONSIDER REMOVING
#  Time:QatSFCoqPowers -1.042e-01  5.279e-02  -1.973  0.05718 .  
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.stepAMb)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
#RESIDUALS NOT NORMALLY DISTRIBUTED since p-value not > 0.05 and non-linear Q-Q plot
#also remove  I(QatSFCoqPowers^2) from mod.megaAM1 since p-value > 0.5
mod.megaAM3 <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                    Time*Temperature+Time*QatSFCoqPowers+Time*OrthophosphateP+
                    Temperature*QatSFCoqPowers+Temperature*OrthophosphateP+
                    QatSFCoqPowers*OrthophosphateP+
                    I(Time^2)+I(Temperature^2), 
                  data=dAM)
mod.stepAMc <- step(mod.megaAM3)
# DO ~ Time + Temperature + QatSFCoqPowers + OrthophosphateP + 
#   I(Time^2) + Time:QatSFCoqPowers
modelname <- "mod_stepAMc" #for plot name
s=summary(mod.stepAMc); s
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)          16.09639    2.70884   5.942 1.14e-06 ***
#  Time                -31.67356   13.15877  -2.407  0.02184 *  
#  Temperature          -0.17344    0.02892  -5.997 9.74e-07 ***
#  QatSFCoqPowers        0.03488    0.01829   1.906  0.06533 .  
#  OrthophosphateP     -81.15151   23.73158  -3.420  0.00169 ** 
#  I(Time^2)            52.10997   16.18892   3.219  0.00289 ** 
#  Time:QatSFCoqPowers  -0.07087    0.05178  -1.369  0.18032     QUITE HIGH
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.stepAMc)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed 
#W = 0.96715, p-value = 0.2913 LOOKS GOOD
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dAM$DO, fitted(mod.stepAMc), main="Model mod.stepAMc DO vs. Observed",xlim = c(6.5,10.5), ylim=c(6.5,10.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#Remove Time:QatSFCoqPowers from mod.megaAM3
mod.megaAM4 <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                    Time*Temperature+Time*QatSFCoqPowers+Time*OrthophosphateP+
                    Temperature*QatSFCoqPowers+Temperature*OrthophosphateP+
                    QatSFCoqPowers*OrthophosphateP+
                    I(Time^2)+I(Temperature^2), 
                  data=dAM)
mod.stepAMd <- step(mod.megaAM4)
s=summary(mod.stepAMd); s
#
#Remove Time:QatSFCoqPowers from mod.megaAM4
mod.megaAM5 <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                    Time*Temperature+Time*OrthophosphateP+
                    Temperature*QatSFCoqPowers+Temperature*OrthophosphateP+
                    QatSFCoqPowers*OrthophosphateP+
                    I(Time^2)+I(Temperature^2), 
                  data=dAM)
mod.stepAMe <- step(mod.megaAM5)
s=summary(mod.stepAMe); s
#
# FUTURE SCENARIO D AM STEP model - 4 parm - mod.stepAM ---------
#Remove Temperature:QatSFCoqPowers from mod.megaAM5
mod.megaAM6 <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                    Time*Temperature+Time*OrthophosphateP+
                    Temperature*OrthophosphateP+
                    QatSFCoqPowers*OrthophosphateP+
                    I(Time^2)+I(Temperature^2), 
                  data=dAM)
mod.stepAM <- step(mod.megaAM6) #FINAL
s=summary(mod.stepAM); s
#DO ~ Time + Temperature + QatSFCoqPowers + OrthophosphateP + I(Time^2)
modelname <- "mod_stepAM" #for plot name
s=summary(mod.stepAM); s
coef(mod.stepAM)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      17.403561   2.567272   6.779 8.54e-08 ***
#  Time            -36.602776  12.817961  -2.856  0.00727 ** 
#  Temperature      -0.178373   0.029064  -6.137 5.72e-07 ***
#  QatSFCoqPowers    0.009995   0.002083   4.799 3.12e-05 *** 
#  OrthophosphateP -68.327403  22.082182  -3.094  0.00393 ** 
#  I(Time^2)        55.590301  16.192082   3.433  0.00159 ** 
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.stepAM)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed 
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dAM$DO, fitted(mod.stepAM), main="Model mod.stepAM DO vs. Observed",xlim = c(6.5,10.5), ylim=c(6.5,10.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#Compare 3 parameter model Method 1 to Method 2
AIC(mod.DOvTimeTQAM,mod.stepAM) # 
#                df      AIC
#mod.DOvTimeTQAM  5 47.04301
#mod.stepAM     15  36.65046 BETTER MODEL?
#AIC is a measure of the fit of the model; the smaller the AIC, the better the fit

# EARLY AM ANALYSIS - VALUES 11am and before ------------------------------
#1-parm model - AM only
mod.DOvQAMe <- lm(DO~transamQ, data=dAMe3)
summary(mod.DOvQAMe)
shapiro.test(resid(mod.DOvQAMe)) #p-value > 0.05, normally distributed residuals
#Plots to evaluate residuals 
png(file=paste(outpath,stations,"_","Model_AM_1parm_residuals.png",sep=""), width = 500, height = 500)
par(mfrow=c(2,2))
plot(mod.DOvQAMe)
dev.off()
#Plot model calc DO vs obs DO
s <- summary(mod.DOvQAMe)
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
par(mfrow=c(1,1))
png(file=paste(outpath,stations,"_","Model_AMearly_1parm.png",sep=""), width = 500, height = 500)
plot(dAMe3$DO, fitted(mod.DOvQAMe), main="Model Calculated DO vs. Observed - Before 11AM - DO = f(Time)", 
     pch=21, col="black", bg="gray",
     xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#Plot residuals vs remaining explanatory variables
s$residuals
d7 <- cbind(s$residuals,dAMe3); d7
head(d7)
d8 <- d7[,c(1,3,4,6)]
head(d8)
names(d8) <- c("Residuals","Time","Temperature","OrthophosphateP")    
png(file=paste(outpath,stations,"_","Pairs_AMearly_1parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d8, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
#transamQ has highest correlation coefficient for residuals vs. remaining parameters
dev.off()
png(file=paste(outpath,stations,"_","ggpairs_AMearly_1parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d8, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
dev.off()
#2 parm model - Temperature and transamQ - Early AM analysis
mod.DOvTQAMe <- lm(DO~Temperature+transamQ, data=dAMe3)
s=summary(mod.DOvTQAMe); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
par(mfrow=c(2,2))
plot(mod.DOvTQAMe)
mod.DOvTQAMe[[12]]
#Plot via base graphics
png(file=paste(outpath,stations,"_","Model_AMearly_2parm_DOfTQ.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dAMe3$DO, fitted(mod.DOvTQAMe), main="Model Calculated DO vs. Observed - AM DO = f(Time, Q)",
     xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
shapiro.test(resid(mod.DOvTQAMe)) #if p-value > 0.05 then normally distributed - IN THIS CASE NOT NORMALLY DIST
png(file=paste(outpath,stations,"_","Model_AMearly_2parm_residuals.png",sep=""), width = 500, height = 500)
par(mfrow=c(2,2))
plot(mod.DOvTQAMe)
dev.off()
#Residuals of mod.DOvTQAMe vs remaining parameters
d9 <- cbind(s$residuals,dAMe3)
head(d9)
d10 <- d9[,c(1,3,6)]
head(d10)
names(d10) <- c("Residuals","Time","OrthophosphateP")
head(d10)
png(file=paste(outpath,stations,"_","Pairs_AMearly_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d10, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
dev.off()
png(file=paste(outpath,stations,"_","ggpairs_AMearly_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d10, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
dev.off()
#3 parm model - Time Temperature and transamQ - Early AM analysis
mod.DOvTimeTQAMe <- lm(DO~Time + Temperature + transamQ, data=dAMe3) 
s=summary(mod.DOvTimeTQAMe); s
rsquared <- round(s[[8]],2); rsquared 
StdError <- round(s[[6]],2); StdError #Residual standard error
par(mfrow=c(2,2))
plot(mod.DOvTimeTQAMe)
mod.DOvTimeTQAMe[[12]]
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,modelname,"Model_EarlyAM_3parm__Diagnostics.png",sep=""), width = 500, height = 500)
par(mfrow=c(2,2))
plot(mod.DOvTimeTQAMe)
dev.off()
d11 <- cbind(s$residuals,dAMe3)
head(d11)
d12 <- d11[,c(1,6)] #Residuals vs remaining parameter
head(d12)
names(d12) <- c("Residuals","OrthophosphateP")
head(d12)
pairs(d12, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor) #highest correlation is with Q
ggpairs(data=d12, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth_loess", combo = "dot_no_facet"))
#NO CORRELATION WITH P
#This early AM model (32 obs) still requires time to pass diagnostics so is
#not an improvement over AM model (40 obs). Ratio of obs:parms reduced from 13 to 11.

#Try Method 2 - STEP 
str(dAMe)
mod.megaAMe1 <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                    Time*Temperature+Time*QatSFCoqPowers+Time*OrthophosphateP+
                    Temperature*QatSFCoqPowers+Temperature*OrthophosphateP+
                    QatSFCoqPowers*OrthophosphateP+
                    I(Time^2)+I(Temperature^2)+I(QatSFCoqPowers^2)+I(OrthophosphateP^2), 
                  data=dAMe)
mod.stepAMe <- step(mod.megaAMe1)
modelname <- "mod_stepAMe" #for plot name
par(mfrow=c(2,2))
plot(mod.stepAMe)
s=summary(mod.stepAMe); s
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed 
#Remove Temperature:QatSFCoqPowers I(Temperature^2) 
mod.megaAMe1 <- lm(DO~Time+Temperature+QatSFCoqPowers+OrthophosphateP+
                     Time*Temperature+Time*QatSFCoqPowers+Time*OrthophosphateP+
                     Temperature*OrthophosphateP+
                     QatSFCoqPowers*OrthophosphateP+
                     I(Time^2)+I(QatSFCoqPowers^2)+I(OrthophosphateP^2), 
                   data=dAMe)
mod.stepAMe <- step(mod.megaAMe1)
modelname <- "mod_stepAMe" #for plot name
par(mfrow=c(2,2))
plot(mod.stepAMe)
s=summary(mod.stepAMe); s
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed 
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.stepAMe)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed 
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dAMe$DO, fitted(mod.stepAMe), main="Model mod.stepAMe DO vs. Observed",xlim = c(6.5,10.5), ylim=c(6.5,10.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(9.0, 6.8,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(9.0, 6.6,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
# DO ~ Time + Temperature + QatSFCoqPowers + OrthophosphateP + 
#      I(OrthophosphateP^2) + Time:Temperature + Time:OrthophosphateP + 
#      QatSFCoqPowers:OrthophosphateP, data = dAMe)
#Rather complicated model - but passes tests - FUTURE SCENARIO E 
# This model doesn't add anything that the other models don't already provide.
# Also, the 3-parm manual addition version for early AM has problems.  
# Don't use this model.


# Model Sensitivites via dsens 25th to 75th %tiles ------------------------
#
# Input table which has median, 25th and 75th values for explanatory variables
#dsens <- read.csv("I:/TMDL_WR/South_Coast/Coquille/MLR_Models/MFCoquille/Data/Data_Mean25th75th_Sens_2.csv")
dsens <- read.csv("I:/TMDL_WR/South_Coast/Coquille/MLR_Models/MFCoquille/Data/Data_Mean25th75th_Sens_4.csv")
dsens
dsensAM <- read.csv("I:/TMDL_WR/South_Coast/Coquille/MLR_Models/MFCoquille/Data/Data_Mean25th75th_Sens_AM.csv")
dsensAM

str(dDO) # DO   Time  Temperature QatSFCoqPowers OrthophosphateP BOD5
str(dAM) # DO   Time  Temperature QatSFCoqPowers OrthophosphateP BOD5        - AM only subset of dDO
str(dDO3) #DO   Time  Temperature transQ         OrthophosphateP transBOD5   
str(dAM3) #DO   Time  Temperature transamQ       OrthophosphateP transamBOD5 - AM only subset
#
#Transformations for model based on ALL data (AM and PM)
dsens$transQ <- dsens$QatSFCoqPowers^-0.3
dsensAM$transQ <- dsensAM$QatSFCoqPowers^-0.3
#dsens$transBOD5 <- dsens$BOD5^0.7 
#dsens$transDIN <- log(dsens$DIN)

#Transformations for model based on AM only data
dsens$transamQ <- dsens$QatSFCoqPowers^-0.6
dsensAM$transamQ <- dsensAM$QatSFCoqPowers^-0.6
#dsens$transamBOD5 <- dsens$BOD5^0.3 
#dsens$transamDIN <- log(dsens$DIN)

str(dsens)
#FUTURE SCENARIO A - 3 parameter model - Via method 1 mod.DOvTimeTQ
# DO ~ Time + Temperature + transQ, data=dDO3) #transQ <- QatSFCoqPowers^-0.3 - FUTURE SCENARIO A
DO.pred.DOvTimeTQ <- predict(mod.DOvTimeTQ, newdata=dsens); DO.pred.DOvTimeTQ #Koch 3-parm
summary(mod.DOvTimeTQ, digits = 8)
coef(mod.DOvTimeTQ) # p. 359 extracting info from model objects, this has greater precision than summary
DO.pred.DOvTimeTQ_am <- predict(mod.DOvTimeTQ, newdata=dsensAM); DO.pred.DOvTimeTQ_am

#FUTURE SCENARIO B - 4 parameter step model GENERATED BY STEP - mod.step
# DO ~ Time + Temperature + QatSFCoqPowers + OrthophosphateP + Time:OrthophosphateP, data = dDO
DO.pred.step <- predict(mod.step, newdata=dsens)
s=summary(mod.step); s
coef(mod.step)
DO.pred.step
DO.pred.step_am <- predict(mod.step, newdata=dsensAM)
DO.pred.step_am

#FUTURE SCENARIO C - 3 parameter model AM only - mod.DOvTimeTQAM
# DO ~ Time + Temperature + transamQ, data=dAM3)
DO.pred.DOvTimeTQAM <- predict(mod.DOvTimeTQAM, newdata=dsens)
s=summary(mod.DOvTimeTQAM); s
coef(mod.DOvTimeTQAM)
DO.pred.DOvTimeTQAM
DO.pred.DOvTimeTQAM2 <- predict(mod.DOvTimeTQAM, newdata=dsensAM)
DO.pred.DOvTimeTQAM2

#FUTURE SCENARIO D - 4 parameter STEP model AM only - mod.stepAM
# DO ~ Time + Temperature + QatSFCoqPowers + OrthophosphateP + I(Time^2)
# data=dAM
DO.pred.stepAM <- predict(mod.stepAM, newdata=dsens)
s=summary(mod.stepAM); s
coef(mod.stepAM)
DO.pred.stepAM
DO.pred.stepAM2 <- predict(mod.stepAM, newdata=dsensAM)
DO.pred.stepAM2

dsens4 <- cbind(dsens, DO.pred.DOvTimeTQ, DO.pred.step, DO.pred.DOvTimeTQAM, DO.pred.stepAM)
dsens4
write.table(dsens4,paste(outpath,"ModelSensitivities4.csv"),sep=",", row.names = FALSE) #short column headers

dsens4AM <- cbind(dsensAM, DO.pred.DOvTimeTQ_am, DO.pred.step_am, DO.pred.DOvTimeTQAM2, DO.pred.stepAM2)
dsens4AM
write.table(dsens4AM,paste(outpath,"ModelSensitivities4AM.csv"),sep=",", row.names = FALSE) #short column headers

############## PHASE III - SENSITIVITY TO EXPLANATORY VARIABLES #############
# 1. Test response - sensistivity to explanatory variables
#   up to 2.7 C reduction in Temperature
#   0 to 30% reduction in nutrients and/or BOD
#   0 to 20% increase in flow
# Future Scenario results for six models produced below
# For each set of future scenarios, change Tchange, Qchange, and PO4Pchange at each location 
# Note that PO4Pchange only affects the two 4-parm STEP models
#
# Tchange = -2.7 #degree C reduction - additive
# Qchange = 1.0 #cfs change - ratio
# PO4Pchange = 1.0 #1.0 is no reduction, for 30% use 0.7 #ratio 
# outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/MFCoquille/Plots_TRed/" #Temperature reduction of 2.7C
#
# Tchange = -2.7 #degree C reduction - additive
# Qchange = 1.2 #cfs change - ratio
# PO4Pchange = 1.0 #1.0 is no reduction, for 30% use 0.7 #ratio 
#outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/MFCoquille/Plots_TredQinc/" #T red of 2.7C and 20% flow increase
#
# Tchange = -2.7 #degree C reduction - additive
# Qchange = 1.0 #cfs change - ratio
# PO4Pchange = 0.7 #1.0 is no reduction, for 30% use 0.7 #ratio
# outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/MFCoquille/Plots_TredPred/" #T red of 2.7C and 20% PO4P red
#
# Tchange = -2.7 #degree C reduction - additive
# Qchange = 1.2 #cfs change - ratio
# PO4Pchange = 0.7 #1.0 is no reduction, for 30% use 0.7 #ratio
#outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/MFCoquille/Plots_TredQincPred/" #T red of 2.7C, 20% flow incr, 30% P red
#
# Tchange = -2.7 #degree C reduction - additive
# Qchange = 0.7 #1.0 #cfs change - ratio
# PO4Pchange = 1.0 #1.0 is no reduction, for 30% use 0.7 #ratio 
#outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/MFCoquille/Plots_TredQred/" #T red of 2.7C, 30% flow reduction

#THE NEXT IS A CLIMATE CHANGE FLOW REDUCTION AND NO T CHANGE SCENARIO
 Tchange = 0.0 #degree C reduction - additive
 Qchange = 0.7 #1.0 #cfs change - ratio
 PO4Pchange = 1.0 #1.0 is no reduction, for 30% use 0.7 #ratio 
outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/MFCoquille/Plots_Qred/" #T red of 2.7C, 30% flow reduction


# FUTURE SCENARIO A - 3 parm model DO=f(Time,Temperature,transQ) - --------
#transQ <- QatSFCoqPowers^-0.3
#Final model via METHOD 1
#DO~Time+Temperature+transQ
#data=dDO3
mtitle <- "Model \"mod.DOvTimeTQ\" DO=f(Time,T,Q)"; mtitle
modelname <- "mod_DOvTimeTQ"
dDOF <- dDO #F for future scenario
head(dDOF)
head(dDOF)
#dDOF$Temperature.C <- dDO$Temperature #current temperature
dDOF$DO.Cpred <- predict(mod.DOvTimeTQ, newdata=dDOF) #predicted DO for Current conditions
head(dDOF) #
#Derive correction factors based on observed current vs predicted current 
dDOF$DO.ErrRat <- dDOF$DO / dDOF$DO.Cpred #error correction Ratio   Obs / Model calculated
dDOF$DO.CpredCa <- dDOF$DO.ErrRat * dDOF$DO.Cpred #predicted Current DO corrected via correction ratio
PO4Pred <- (1.0 - PO4Pchange) * 100; PO4Pred #for plot name
dDOF$Temperature <- dDO$Temperature + Tchange
dDOF$OrthophosphateP <- PO4Pchange * dDO$OrthophosphateP
dDOF$QatSFCoqPowers <- Qchange * dDO$QatSFCoqPowers
dDOF$transQ <- dDOF$QatSFCoqPowers^-0.3 #Transformations for model based on ALL data (AM and PM)
dDOF$DO.Fpred <- predict(mod.DOvTimeTQ, newdata=dDOF) #predicted DO for future conditions
dDOF$DO.FpredCa <- dDOF$DO.ErrRat * dDOF$DO.Fpred #predicted Future DO corrected via correction ratio
head(dDOF)
q.obs <- format(round(quantile(dDOF$DO,c(.05,.10, .25, .5)),2),nsmall=2); q.obs #observed current
q.Cpred <- format(round(quantile(dDOF$DO.Cpred,c(.05,.10, .25, .5)),2),nsmal=2); q.Cpred #predicted Current
q.CpredCa <- format(round(quantile(dDOF$DO.CpredCa,c(.05,.10, .25, .5)),2),nsmall=2); q.CpredCa #predicted Current with correction; same as q.obs
q.Fpred <- format(round(quantile(dDOF$DO.Fpred,c(.05,.10, .25, .5)),2),nsmall=2); q.Fpred #predicted Future
q.FpredCa <- format(round(quantile(dDOF$DO.FpredCa,c(.05,.10, .25, .5)),2),nsmall=2); q.FpredCa #predicted Future with correction
min(dDOF$DO.FpredCa)
#PLOT 1 - Plot model calculated current condition DO vs observed current condition
#         and model calculated future  condition DO vs observed current condition 
png(file=paste(outpath,modelname,"_Plot1_","CalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOF$DO, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,8,8)
DO.crity <- c(8,8,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(8.1, 6.9,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                             " 10th %tile=", q.Fpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.7,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                             " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.5,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(8.1, 7.4,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(8.1, 7.2,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
#text(9.3, 7.4,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=6.4, y=10.45, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 2 - Same data as Plot 1 but vs. Time of Day - Model calculated DO for current and future conditions (uncorrected)
png(file=paste(outpath,modelname,"_Plot2_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", 
     cex.main=1.0, bg="gray", xlim = c(0.2,0.8), ylim=c(6.0,11), xlab = "Time of Day", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(8,8)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 6.5,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                             " 10th %tile=", q.Fpred[2]), cex = 0.8, adj=c(0,0))
text(0.5, 6.25,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                              " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(0.5, 6.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(0.2, 6.6,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 6.4,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
#text(0.2, 6.2,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=11.2, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=10.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 3 - Plot CORRECTED model calculated current condition DO vs observed current condition
#         and CORRECTED model calculated future  condition DO vs observed current condition 
#Plot shows how corrected predicted values for current conditions match observed
png(file=paste(outpath,modelname,"_Plot3_","CalculatedCorrectedCurrentDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=19, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Corrected Model Calculated DO")
mtext("Model Calculated Current and \"Corrected\" Calculated Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Cpred, cex=0.7, col="red", bg="red")
#abline(a=0,b=1) 
text(8.1, 6.9,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                             " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.7,labels = paste("Corrected Model Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                             " 10th %tile=", q.CpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.5,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
legend(x=6.5, y=10.25, bty="n", legend = "Corrected calculated DO", pch=21, cex=0.9, col="black", bg = "gray")
legend(x=6.5, y=10.5, bty="n", legend = "Calculated DO", pch=21, cex=0.9, col="red", bg = "red")
dev.off()
#PLOT 4 - Next plot corrected calculated DO for future conditions vs current observed DO
#         and corrected calculated DO for current conditions vs current observed DO
png(file=paste(outpath,modelname,"_Plot4_","CorrectedCalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("\"Corrected\" Model Calculated Future and Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,8,8)
DO.crity <- c(8,8,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(8.1, 7.0,labels = paste("Corrected Calculated Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                             " 10th %tile=", q.FpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.8,labels = paste("Corrected Calculated Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                             " 10th %tile=", q.CpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.6,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(8.1, 7.4,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(8.1, 7.2,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
#text(9.3, 7.4,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.45, bty="n", legend = "Corrected calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
legend(x=6.4, y=10.7, bty="n",  legend = "Corrected calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
dev.off()
#PLOT 5 - Plot vs Time of Day - CORRECTED Model calculated DO for current conditions and future conditions
png(file=paste(outpath,modelname,"_Plot5_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray",
     cex.main=1.0, xlim = c(0.2,0.8), ylim=c(6.0,11), xlab = "Time of Day", ylab = "Corrected Model Calculated DO")
mtext("\"Corrected\" Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(8,8)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
#text(0.48, 6.5,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
#                              " 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
#text(0.48, 6.25,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
#                               " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
#text(0.48, 6.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
#                              " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.47, 6.6,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                              " 10th=", q.FpredCa[2]," 5th=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.47, 6.4,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                              " 10th=", q.CpredCa[2]," 5th=", q.CpredCa[1]),  cex = 0.8, adj=c(0,0))
text(0.47, 6.2,labels = paste("Observed Current DO:         Mean =", round(mean(dDOF$DO),2),
                              " 10th=", q.obs[2]," 5th=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 6.6,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 6.4,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
#text(0.2, 6.2,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=11.2, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=10.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()


# FUTURE SCENARIO B - STEP MODEL 4 parm model DO=f(Time,Temperatur --------
#DO ~ Time + Temperature + QatSFCoqPowers + OrthophosphateP + Time:OrthophosphateP
# data=dDO
#mod.step
mtitle <- "Model \"mod.step\" DO=f(Time,T,Q,P)"; mtitle
modelname <- "mod_step"
dDOF <- dDO #F for future scenario
head(dDOF)
#Transformations for model based on ALL data (AM and PM)
#dDOF$transQ <- dDOF$QatSFCoqPowers^-0.3 NOT TRANSFORMED FOR STEP MODEL
head(dDOF)
#dDOF$Temperature.C <- dDO$Temperature #current temperature
dDOF$DO.Cpred <- predict(mod.step, newdata=dDOF) #predicted DO for Current conditions
head(dDOF) #
#Derive correction factors based on observed current vs predicted current 
dDOF$DO.ErrRat <- dDOF$DO / dDOF$DO.Cpred #error correction Ratio   Obs / Model calculated
dDOF$DO.CpredCa <- dDOF$DO.ErrRat * dDOF$DO.Cpred #predicted Current DO corrected via correction ratio
PO4Pred <- (1.0 - PO4Pchange) * 100; PO4Pred #for plot name
dDOF$Temperature <- dDO$Temperature + Tchange
dDOF$OrthophosphateP <- PO4Pchange * dDO$OrthophosphateP
dDOF$QatSFCoqPowers <- Qchange * dDO$QatSFCoqPowers
dDOF$DO.Fpred <- predict(mod.step, newdata=dDOF) #predicted DO for future conditions
dDOF$DO.FpredCa <- dDOF$DO.ErrRat * dDOF$DO.Fpred #predicted Future DO corrected via correction ratio
head(dDOF)
q.obs <- format(round(quantile(dDOF$DO,c(.05,.10, .25, .5)),2),nsmall=2); q.obs #observed current
q.Cpred <- format(round(quantile(dDOF$DO.Cpred,c(.05,.10, .25, .5)),2),nsmal=2); q.Cpred #predicted Current
q.CpredCa <- format(round(quantile(dDOF$DO.CpredCa,c(.05,.10, .25, .5)),2),nsmall=2); q.CpredCa #predicted Current with correction; same as q.obs
q.Fpred <- format(round(quantile(dDOF$DO.Fpred,c(.05,.10, .25, .5)),2),nsmall=2); q.Fpred #predicted Future
q.FpredCa <- format(round(quantile(dDOF$DO.FpredCa,c(.05,.10, .25, .5)),2),nsmall=2); q.FpredCa #predicted Future with correction
min(dDOF$DO.FpredCa)
#PLOT 1 - Plot model calculated current condition DO vs observed current condition
#         and model calculated future  condition DO vs observed current condition 
png(file=paste(outpath,modelname,"_Plot1_","CalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOF$DO, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,8,8)
DO.crity <- c(8,8,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(8.1, 6.9,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                             " 10th %tile=", q.Fpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.7,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                             " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.5,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(8.1, 7.4,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(8.1, 7.2,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
text(9.3, 7.4,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=6.4, y=10.45, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 2 - Same data as Plot 1 but vs. Time of Day - Model calculated DO for current and future conditions (uncorrected)
png(file=paste(outpath,modelname,"_Plot2_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", 
     cex.main=1.0, bg="gray", xlim = c(0.2,0.8), ylim=c(6.0,11), xlab = "Time of Day", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(8,8)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 6.5,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                             " 10th %tile=", q.Fpred[2]), cex = 0.8, adj=c(0,0))
text(0.5, 6.25,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                              " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(0.5, 6.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(0.2, 6.6,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 6.4,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
text(0.2, 6.2,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=11.2, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=10.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 3 - Plot CORRECTED model calculated current condition DO vs observed current condition
#         and CORRECTED model calculated future  condition DO vs observed current condition 
#Plot shows how corrected predicted values for current conditions match observed
png(file=paste(outpath,modelname,"_Plot3_","CalculatedCorrectedCurrentDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=19, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Corrected Model Calculated DO")
mtext("Model Calculated Current and \"Corrected\" Calculated Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Cpred, cex=0.7, col="red", bg="red")
#abline(a=0,b=1) 
text(8.1, 6.9,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                             " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.7,labels = paste("Corrected Model Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                             " 10th %tile=", q.CpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.5,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
legend(x=6.5, y=10.25, bty="n", legend = "Corrected calculated DO", pch=21, cex=0.9, col="black", bg = "gray")
legend(x=6.5, y=10.5, bty="n", legend = "Calculated DO", pch=21, cex=0.9, col="red", bg = "red")
dev.off()
#PLOT 4 - Next plot corrected calculated DO for future conditions vs current observed DO
#         and corrected calculated DO for current conditions vs current observed DO
png(file=paste(outpath,modelname,"_Plot4_","CorrectedCalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("\"Corrected\" Model Calculated Future and Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,8,8)
DO.crity <- c(8,8,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(8.1, 7.0,labels = paste("Corrected Calculated Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                             " 10th %tile=", q.FpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.8,labels = paste("Corrected Calculated Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                             " 10th %tile=", q.CpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.6,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(8.1, 7.4,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(8.1, 7.2,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
text(9.3, 7.4,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.45, bty="n", legend = "Corrected calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
legend(x=6.4, y=10.7, bty="n",  legend = "Corrected calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
dev.off()
#PLOT 5 - Plot vs Time of Day - CORRECTED Model calculated DO for current conditions and future conditions
png(file=paste(outpath,modelname,"_Plot5_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray",
     cex.main=1.0, xlim = c(0.2,0.8), ylim=c(6.0,11), xlab = "Time of Day", ylab = "Corrected Model Calculated DO")
mtext("\"Corrected\" Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(8,8)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
#text(0.48, 6.5,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
#                              " 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
#text(0.48, 6.25,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
#                               " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
#text(0.48, 6.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
#                              " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.47, 6.6,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                              " 10th=", q.FpredCa[2]," 5th=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.47, 6.4,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                              " 10th=", q.CpredCa[2]," 5th=", q.CpredCa[1]),  cex = 0.8, adj=c(0,0))
text(0.47, 6.2,labels = paste("Observed Current DO:         Mean =", round(mean(dDOF$DO),2),
                              " 10th=", q.obs[2]," 5th=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 6.6,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 6.4,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
text(0.2, 6.2,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=11.2, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=10.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()


# FUTURE SCENARIO C - 3 parm model - AM only - mod.DOvTimeTQAM ------------
# DO~Time + Temperature + transamQ
# data=dAM3
# mod.DOvTimeTQAM
# transamQ <- QatSFCoqPowers^-0.6
mtitle <- "Model \"mod.DOvTimeTQAM\"AM DO=f(Time,T,transamQ)"; mtitle
modelname <- "mod_DOvTimeTQAM"
dDOF <- dAM #F for future scenario
head(dDOF)
#Transformations for model based on AM data 
#dDOF$Temperature.C <- dDOF$Temperature #current temperature
dDOF$DO.Cpred <- predict(mod.DOvTimeTQAM, newdata=dDOF) #predicted DO for Current conditions
#Derive correction factors based on observed current vs predicted current 
dDOF$DO.ErrRat <- dDOF$DO / dDOF$DO.Cpred; dDOF$DO.ErrRat #error correction Ratio   Obs / Model calculated
dDOF$DO.CpredCa <- dDOF$DO.ErrRat * dDOF$DO.Cpred ; dDOF$DO.CpredCa #predicted Current DO corrected via correction ratio
dDOF$Temperature <- dAM$Temperature + Tchange
dDOF$QatSFCoqPowers <- Qchange * dAM$QatSFCoqPowers
dDOF$transamQ <- dDOF$QatSFCoqPowers^-0.6
dDOF$DO.Fpred <- predict(mod.DOvTimeTQAM, newdata=dDOF) #predicted DO for future conditions
dDOF$DO.FpredCa <- dDOF$DO.ErrRat * dDOF$DO.Fpred #predicted Future DO corrected via correction ratio
head(dDOF)
q.obs <- format(round(quantile(dDOF$DO,c(.05,.10, .25, .5)),2),nsmall=2); q.obs #observed current
q.Cpred <- format(round(quantile(dDOF$DO.Cpred,c(.05,.10, .25, .5)),2),nsmal=2); q.Cpred #predicted Current
q.CpredCa <- format(round(quantile(dDOF$DO.CpredCa,c(.05,.10, .25, .5)),2),nsmall=2); q.CpredCa #predicted Current with correction; same as q.obs
q.Fpred <- format(round(quantile(dDOF$DO.Fpred,c(.05,.10, .25, .5)),2),nsmall=2); q.Fpred #predicted Future
q.FpredCa <- format(round(quantile(dDOF$DO.FpredCa,c(.05,.10, .25, .5)),2),nsmall=2); q.FpredCa #predicted Future with correction
min(dDOF$DO.FpredCa)
#PLOT 1 - Plot model calculated current condition DO vs observed current condition
#         and model calculated future  condition DO vs observed current condition 
png(file=paste(outpath,modelname,"_Plot1_","CalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOF$DO, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,8,8)
DO.crity <- c(8,8,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(8.1, 6.9,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                             " 10th %tile=", q.Fpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.7,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                             " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.5,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(8.1, 7.4,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(8.1, 7.2,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
#text(9.3, 7.4,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=6.4, y=10.45, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 2 - Same data as Plot 1 but vs. Time of Day - Model calculated DO for current and future conditions (uncorrected)
png(file=paste(outpath,modelname,"_Plot2_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", 
     cex.main=1.0, bg="gray", xlim = c(0.2,0.8), ylim=c(6.0,11), xlab = "Time of Day", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(8,8)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 6.5,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                             " 10th %tile=", q.Fpred[2]), cex = 0.8, adj=c(0,0))
text(0.5, 6.25,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                              " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(0.5, 6.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(0.2, 6.6,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 6.4,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
#text(0.2, 6.2,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=11.2, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=10.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 3 - Plot CORRECTED model calculated current condition DO vs observed current condition
#         and CORRECTED model calculated future  condition DO vs observed current condition 
#Plot shows how corrected predicted values for current conditions match observed
png(file=paste(outpath,modelname,"_Plot3_","CalculatedCorrectedCurrentDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=19, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Corrected Model Calculated DO")
mtext("Model Calculated Current and \"Corrected\" Calculated Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Cpred, cex=0.7, col="red", bg="red")
#abline(a=0,b=1) 
text(8.1, 6.9,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                             " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.7,labels = paste("Corrected Model Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                             " 10th %tile=", q.CpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.5,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
legend(x=6.5, y=10.25, bty="n", legend = "Corrected calculated DO", pch=21, cex=0.9, col="black", bg = "gray")
legend(x=6.5, y=10.5, bty="n", legend = "Calculated DO", pch=21, cex=0.9, col="red", bg = "red")
dev.off()
#PLOT 4 - Next plot corrected calculated DO for future conditions vs current observed DO
#         and corrected calculated DO for current conditions vs current observed DO
png(file=paste(outpath,modelname,"_Plot4_","CorrectedCalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("\"Corrected\" Model Calculated Future and Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,8,8)
DO.crity <- c(8,8,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(8.1, 7.0,labels = paste("Corrected Calculated Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                             " 10th %tile=", q.FpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.8,labels = paste("Corrected Calculated Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                             " 10th %tile=", q.CpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.6,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(8.1, 7.4,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(8.1, 7.2,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
#text(9.3, 7.4,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.45, bty="n", legend = "Corrected calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
legend(x=6.4, y=10.7, bty="n",  legend = "Corrected calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
dev.off()
#PLOT 5 - Plot vs Time of Day - CORRECTED Model calculated DO for current conditions and future conditions
png(file=paste(outpath,modelname,"_Plot5_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray",
     cex.main=1.0, xlim = c(0.2,0.8), ylim=c(6.0,11), xlab = "Time of Day", ylab = "Corrected Model Calculated DO")
mtext("\"Corrected\" Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(8,8)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
#text(0.48, 6.5,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
#                              " 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
#text(0.48, 6.25,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
#                               " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
#text(0.48, 6.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
#                              " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.47, 6.6,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                              " 10th=", q.FpredCa[2]," 5th=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.47, 6.4,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                              " 10th=", q.CpredCa[2]," 5th=", q.CpredCa[1]),  cex = 0.8, adj=c(0,0))
text(0.47, 6.2,labels = paste("Observed Current DO:         Mean =", round(mean(dDOF$DO),2),
                              " 10th=", q.obs[2]," 5th=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 6.6,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 6.4,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
#text(0.2, 6.2,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=11.2, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=10.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()


# FUTURE SCENARIO D - 4 parm STEP model - AM only - mod.stepAM ------------
#DO ~ Time + Temperature + QatSFCoqPowers + OrthophosphateP + I(Time^2)
# data=dAM
# mod.stepAM
mtitle <- "Model \"mod.stepAM\"AM DO=f(Time,T,Q,P)"; mtitle
modelname <- "mod_stepAM"
dDOF <- dAM #F for future scenario
head(dDOF)
dDOF$DO.Cpred <- predict(mod.stepAM, newdata=dDOF) #predicted DO for Current conditions
#Derive correction factors based on observed current vs predicted current 
dDOF$DO.ErrRat <- dDOF$DO / dDOF$DO.Cpred #error correction Ratio   Obs / Model calculated
dDOF$DO.CpredCa <- dDOF$DO.ErrRat * dDOF$DO.Cpred #predicted Current DO corrected via correction ratio
dDOF$Temperature <- dAM$Temperature + Tchange
PO4Pred <- (1.0 - PO4Pchange) * 100; PO4Pred #for plot name
dDOF$OrthophosphateP <- PO4Pchange * dAM$OrthophosphateP
dDOF$QatSFCoqPowers <- Qchange * dAM$QatSFCoqPowers
dDOF$DO.Fpred <- predict(mod.step, newdata=dDOF) #predicted DO for future conditions
dDOF$DO.FpredCa <- dDOF$DO.ErrRat * dDOF$DO.Fpred #predicted Future DO corrected via correction ratio
head(dDOF)
#q.obs <- round(quantile(dDOF$DO,c(.05,.10, .25, .5)),2); q.obs #observed current
#q.Cpred <- round(quantile(dDOF$DO.Cpred,c(.05,.10, .25, .5)),2); q.Cpred #predicted Current
#q.CpredCa <- round(quantile(dDOF$DO.CpredCa,c(.05,.10, .25, .5)),2); q.CpredCa #predicted Current with correction; same as q.obs
#q.Fpred <- round(quantile(dDOF$DO.Fpred,c(.05,.10, .25, .5)),2); q.Fpred #predicted Future
#q.FpredCa <- round(quantile(dDOF$DO.FpredCa,c(.05,.10, .25, .5)),2); q.FpredCa #predicted Future with correction
q.obs <- format(round(quantile(dDOF$DO,c(.05,.10, .25, .5)),2),nsmall=2); q.obs #observed current
q.Cpred <- format(round(quantile(dDOF$DO.Cpred,c(.05,.10, .25, .5)),2),nsmal=2); q.Cpred #predicted Current
q.CpredCa <- format(round(quantile(dDOF$DO.CpredCa,c(.05,.10, .25, .5)),2),nsmall=2); q.CpredCa #predicted Current with correction; same as q.obs
q.Fpred <- format(round(quantile(dDOF$DO.Fpred,c(.05,.10, .25, .5)),2),nsmall=2); q.Fpred #predicted Future
q.FpredCa <- format(round(quantile(dDOF$DO.FpredCa,c(.05,.10, .25, .5)),2),nsmall=2); q.FpredCa #predicted Future with correction
min(dDOF$DO.FpredCa)
#PLOT 1 - Plot model calculated current condition DO vs observed current condition
#         and model calculated future  condition DO vs observed current condition 
png(file=paste(outpath,modelname,"_Plot1_","CalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOF$DO, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,8,8)
DO.crity <- c(8,8,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(8.1, 6.9,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                             " 10th %tile=", q.Fpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.7,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                             " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.5,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(8.1, 7.4,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(8.1, 7.2,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
text(9.3, 7.4,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=6.4, y=10.45, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 2 - Same data as Plot 1 but vs. Time of Day - Model calculated DO for current and future conditions (uncorrected)
png(file=paste(outpath,modelname,"_Plot2_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", 
     cex.main=1.0, bg="gray", xlim = c(0.2,0.8), ylim=c(6.0,11), xlab = "Time of Day", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(8,8)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 6.5,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                             " 10th %tile=", q.Fpred[2]), cex = 0.8, adj=c(0,0))
text(0.5, 6.25,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                              " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(0.5, 6.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(0.2, 6.6,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 6.4,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
text(0.2, 6.2,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=11.2, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=10.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 3 - Plot CORRECTED model calculated current condition DO vs observed current condition
#         and CORRECTED model calculated future  condition DO vs observed current condition 
#Plot shows how corrected predicted values for current conditions match observed
png(file=paste(outpath,modelname,"_Plot3_","CalculatedCorrectedCurrentDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=19, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Corrected Model Calculated DO")
mtext("Model Calculated Current and \"Corrected\" Calculated Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Cpred, cex=0.7, col="red", bg="red")
#abline(a=0,b=1) 
text(8.1, 6.9,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                             " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.7,labels = paste("Corrected Model Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                             " 10th %tile=", q.CpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.5,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
legend(x=6.5, y=10.25, bty="n", legend = "Corrected calculated DO", pch=21, cex=0.9, col="black", bg = "gray")
legend(x=6.5, y=10.5, bty="n", legend = "Calculated DO", pch=21, cex=0.9, col="red", bg = "red")
dev.off()
#PLOT 4 - Next plot corrected calculated DO for future conditions vs current observed DO
#         and corrected calculated DO for current conditions vs current observed DO
png(file=paste(outpath,modelname,"_Plot4_","CorrectedCalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("\"Corrected\" Model Calculated Future and Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,8,8)
DO.crity <- c(8,8,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(8.1, 7.0,labels = paste("Corrected Calculated Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                             " 10th %tile=", q.FpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.8,labels = paste("Corrected Calculated Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                             " 10th %tile=", q.CpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.6,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(8.1, 7.4,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(8.1, 7.2,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
text(9.3, 7.4,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.45, bty="n", legend = "Corrected calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
legend(x=6.4, y=10.7, bty="n",  legend = "Corrected calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
dev.off()
#PLOT 5 - Plot vs Time of Day - CORRECTED Model calculated DO for current conditions and future conditions
png(file=paste(outpath,modelname,"_Plot5_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray",
     cex.main=1.0, xlim = c(0.2,0.8), ylim=c(6.0,11), xlab = "Time of Day", ylab = "Corrected Model Calculated DO")
mtext("\"Corrected\" Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(8,8)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
#text(0.48, 6.5,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
#                              " 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
#text(0.48, 6.25,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
#                               " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
#text(0.48, 6.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
#                              " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.47, 6.6,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                              " 10th=", q.FpredCa[2]," 5th=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.47, 6.4,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                              " 10th=", q.CpredCa[2]," 5th=", q.CpredCa[1]),  cex = 0.8, adj=c(0,0))
text(0.47, 6.2,labels = paste("Observed Current DO:         Mean =", round(mean(dDOF$DO),2),
                              " 10th=", q.obs[2]," 5th=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 6.6,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 6.4,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
text(0.2, 6.2,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=11.2, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=10.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()


# FUTURE SCENARIO E - 4 parm STEP model - Early AM only <=11:00am - mod.stepAMe ------------
# DO ~ Time + Temperature + QatSFCoqPowers + OrthophosphateP + 
#      I(OrthophosphateP^2) + Time:Temperature + Time:OrthophosphateP + 
#      QatSFCoqPowers:OrthophosphateP, data = dAMe)
# mod.stepAMe
mtitle <- "Model \"mod.stepAMe\"AM DO=f(Time,T,Q,P)"; mtitle
modelname <- "mod_stepAMe"
dDOF <- dAM #F for future scenario
head(dDOF)
dDOF$DO.Cpred <- predict(mod.stepAMe, newdata=dDOF) #predicted DO for Current conditions
#Derive correction factors based on observed current vs predicted current 
dDOF$DO.ErrRat <- dDOF$DO / dDOF$DO.Cpred #error correction Ratio   Obs / Model calculated
dDOF$DO.CpredCa <- dDOF$DO.ErrRat * dDOF$DO.Cpred #predicted Current DO corrected via correction ratio
dDOF$Temperature <- dAM$Temperature + Tchange
PO4Pred <- (1.0 - PO4Pchange) * 100; PO4Pred #for plot name
dDOF$OrthophosphateP <- PO4Pchange * dAM$OrthophosphateP
dDOF$QatSFCoqPowers <- Qchange * dAM$QatSFCoqPowers
dDOF$DO.Fpred <- predict(mod.step, newdata=dDOF) #predicted DO for future conditions
dDOF$DO.FpredCa <- dDOF$DO.ErrRat * dDOF$DO.Fpred #predicted Future DO corrected via correction ratio
head(dDOF)
#q.obs <- round(quantile(dDOF$DO,c(.05,.10, .25, .5)),2); q.obs #observed current
#q.Cpred <- round(quantile(dDOF$DO.Cpred,c(.05,.10, .25, .5)),2); q.Cpred #predicted Current
#q.CpredCa <- round(quantile(dDOF$DO.CpredCa,c(.05,.10, .25, .5)),2); q.CpredCa #predicted Current with correction; same as q.obs
#q.Fpred <- round(quantile(dDOF$DO.Fpred,c(.05,.10, .25, .5)),2); q.Fpred #predicted Future
#q.FpredCa <- round(quantile(dDOF$DO.FpredCa,c(.05,.10, .25, .5)),2); q.FpredCa #predicted Future with correction
q.obs <- format(round(quantile(dDOF$DO,c(.05,.10, .25, .5)),2),nsmall=2); q.obs #observed current
q.Cpred <- format(round(quantile(dDOF$DO.Cpred,c(.05,.10, .25, .5)),2),nsmal=2); q.Cpred #predicted Current
q.CpredCa <- format(round(quantile(dDOF$DO.CpredCa,c(.05,.10, .25, .5)),2),nsmall=2); q.CpredCa #predicted Current with correction; same as q.obs
q.Fpred <- format(round(quantile(dDOF$DO.Fpred,c(.05,.10, .25, .5)),2),nsmall=2); q.Fpred #predicted Future
q.FpredCa <- format(round(quantile(dDOF$DO.FpredCa,c(.05,.10, .25, .5)),2),nsmall=2); q.FpredCa #predicted Future with correction
min(dDOF$DO.FpredCa)
#PLOT 1 - Plot model calculated current condition DO vs observed current condition
#         and model calculated future  condition DO vs observed current condition 
png(file=paste(outpath,modelname,"_Plot1_","CalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOF$DO, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,8,8)
DO.crity <- c(8,8,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(8.1, 6.9,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                             " 10th %tile=", q.Fpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.7,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                             " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.5,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(8.1, 7.4,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(8.1, 7.2,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
text(9.3, 7.4,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=6.4, y=10.45, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 2 - Same data as Plot 1 but vs. Time of Day - Model calculated DO for current and future conditions (uncorrected)
png(file=paste(outpath,modelname,"_Plot2_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", 
     cex.main=1.0, bg="gray", xlim = c(0.2,0.8), ylim=c(6.0,11), xlab = "Time of Day", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(8,8)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 6.5,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                             " 10th %tile=", q.Fpred[2]), cex = 0.8, adj=c(0,0))
text(0.5, 6.25,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                              " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(0.5, 6.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(0.2, 6.6,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 6.4,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
text(0.2, 6.2,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=11.2, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=10.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 3 - Plot CORRECTED model calculated current condition DO vs observed current condition
#         and CORRECTED model calculated future  condition DO vs observed current condition 
#Plot shows how corrected predicted values for current conditions match observed
png(file=paste(outpath,modelname,"_Plot3_","CalculatedCorrectedCurrentDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=19, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Corrected Model Calculated DO")
mtext("Model Calculated Current and \"Corrected\" Calculated Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Cpred, cex=0.7, col="red", bg="red")
#abline(a=0,b=1) 
text(8.1, 6.9,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                             " 10th %tile=", q.Cpred[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.7,labels = paste("Corrected Model Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                             " 10th %tile=", q.CpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.5,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
legend(x=6.5, y=10.25, bty="n", legend = "Corrected calculated DO", pch=21, cex=0.9, col="black", bg = "gray")
legend(x=6.5, y=10.5, bty="n", legend = "Calculated DO", pch=21, cex=0.9, col="red", bg = "red")
dev.off()
#PLOT 4 - Next plot corrected calculated DO for future conditions vs current observed DO
#         and corrected calculated DO for current conditions vs current observed DO
png(file=paste(outpath,modelname,"_Plot4_","CorrectedCalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(6.5,10.5), ylim=c(6.5,10.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("\"Corrected\" Model Calculated Future and Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,8,8)
DO.crity <- c(8,8,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(8.1, 7.0,labels = paste("Corrected Calculated Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                             " 10th %tile=", q.FpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.8,labels = paste("Corrected Calculated Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                             " 10th %tile=", q.CpredCa[2]), cex = 0.8, adj=c(0,0))
text(8.1, 6.6,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                             " 10th %tile=", q.obs[2]), cex = 0.8, adj=c(0,0))
text(8.1, 7.4,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(8.1, 7.2,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
text(9.3, 7.4,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.45, bty="n", legend = "Corrected calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
legend(x=6.4, y=10.7, bty="n",  legend = "Corrected calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
dev.off()
#PLOT 5 - Plot vs Time of Day - CORRECTED Model calculated DO for current conditions and future conditions
png(file=paste(outpath,modelname,"_Plot5_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray",
     cex.main=1.0, xlim = c(0.2,0.8), ylim=c(6.0,11), xlab = "Time of Day", ylab = "Corrected Model Calculated DO")
mtext("\"Corrected\" Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(8,8)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
#text(0.48, 6.5,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
#                              " 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
#text(0.48, 6.25,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
#                               " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
#text(0.48, 6.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
#                              " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.47, 6.6,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                              " 10th=", q.FpredCa[2]," 5th=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.47, 6.4,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                              " 10th=", q.CpredCa[2]," 5th=", q.CpredCa[1]),  cex = 0.8, adj=c(0,0))
text(0.47, 6.2,labels = paste("Observed Current DO:         Mean =", round(mean(dDOF$DO),2),
                              " 10th=", q.obs[2]," 5th=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 6.6,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 6.4,labels = paste("River flow increase = ", (Qchange-1)*100, "%"), cex = 0.8, adj=c(0,0))
text(0.2, 6.2,labels = paste("PO4P reduction = ", PO4Pred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=11.2, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=10.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()

########################   END OF SUMMER REARING AND MIGRATION ANALYSIS   ############################
######################################################################################################
