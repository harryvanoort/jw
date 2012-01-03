# CBA - Harry van Oort
#this code comes with no warranty


#########INPUT DATA REQUIRED To run script################
# 'jw.csv'
####################################################



#CLBMON40 Jarvis and Woods SCRIPT

####SET YOUR HOME DIRECTORY HERE (the working sub-directories are modified in script)


dir = "C:/r_files/clbmon40/jw/"


#--------------------------------------------------------------------
###open libraries and set the graphics themes

graphics.off()
library(ggplot2)
#theme_set(theme_bw())
#theme_update(legend.key=theme_blank())

#--------------------------------------------------------------------

setwd (paste(dir, "input_data", sep = ""))

#-------------START ANALYSIS TASKS------------------------------------------------

library(mgcv)
library(lattice)

dat <- read.csv("jw.csv")
setwd (dir)
dat$year <- as.factor(dat$year)
dat$date <- as.Date(dat$date)
dat$ssn <- as.Date(dat$ssn)
dat$month <- as.factor(dat$month)
dat$zone <- as.factor(dat$zone)

summary(dat)

#NOTES
#ZONES: A=9 Mile, B=Robs Willows, C=Cartier Bay, D=Montana Bay, E=LCO, F=Airport Marsh, G=Downie Marsh





##### SECTION 1. DATA EXPORLATION


####******* NOTE THAT SOME OF THESE GRAPHS WITHOUT ZONE, SHOULD PROBABLY USE AGREGATED DATA???

# 1.1. Lattice xy plot of elev and wf percentage shown for each wetland area 'zone'
xyplot(wfpct~alr | factor(zone), type = "p",
	xlab = "ALR", col = 1, ylab = "wfpct", data=dat)
	### The above graph is interesting and shows potential impact of reservoir
	### Notice interesting increase with zone A (LCO)
	### Note that the variance differs by reservoir elevation.
	### Zone B has a couple of potential outliers (note this is as percent)

##GGPLOT 1.1
p<-qplot(alr,wfpct,data=dat,alpha=0.2, geom = c('point','smooth'),facets=zone~.)
	ggsave(file ='1.1_pct_alr_zone.jpg', width=6, height =8,  path = paste(dir, "figures", sep = ""))

p


# 1.2. Lattice xy plot of elev and total wf shown for each month of the year
xyplot(wf~alr | factor(month), type = "p",
	xlab = "ALR", col = 1, ylab = "wf", data=dat)
	### The above graph showes seasonal patterns of reservoir elevations as you would expect
	### note that the variance will differ by month and in some months by rservoir elevation!
	### Months 10, 12, 5, and 1 have potential outliers

##GGPLOT 1.2
p<-qplot(alr,wf,data=dat,alpha=0.1, geom = 'point',facets=month~.)
	ggsave(file ='1.2_wf_alr_month.jpg', width=6, height =8,  path = paste(dir, "figures", sep = ""))

#1.2.1. Total wf by elev for each occasion after aggrigating
# first need to aggregate data

tw<-dat$total_wf
date<-dat$date
alr<-dat$alr
month<-as.numeric(dat$month)

dat1<-data.frame(date,tw)
dat2<-data.frame(date,alr)
dat3<-data.frame(date,month)

ag1 <- ddply(dat1, .(date), summarise, twf = mean(tw, na.rm = TRUE))
ag2 <- ddply(dat2, .(date), summarise, alr = mean(alr, na.rm = TRUE))
ag3 <- ddply(dat3, .(date), summarise, month = mean(month, na.rm = TRUE))
ag3
 
wf<-ag1$twf
date<-ag1$date
alr<-ag2$alr
month<-ag3$month

dat1.2.1<-data.frame(date,wf,alr,month)


## second, the graph: total waterfowl by date and reservoir operation

##GGPLOT 1.2
p<-qplot(alr,wf,data=dat1.2.1,alpha=0.2, geom = 'point', facets=month~.)
p
ggsave(file ='1.2.1_wf_alr_month.jpg', width=6, height =8,  path = paste(dir, "figures", sep = ""))




# 1.3. Lattice xy plot of seasonality and total wf
xyplot(wf~julien | factor(zone), type = "p",
xlab = "time", col = 1, ylab = "wf", data=dat)
	### Clearly, some zones are used more heavily by migrants. 
	### Obviously, variance changes with season, so a differnt smoother is likely for each zone... unless this is explained by alr
	### Notice that zone A gets migration usage in fall, but not spring.
	### Zone B, E, and C have potential outliers (note this is total numbers)
	### Notice that zone A is one again different - its better in the fall, which matches 1.1, where it was better when full


##GGPLOT 1.3

p<-qplot(ssn,wf,data=dat,alpha=0.2, geom=c('point','smooth'), facets=zone~.)
	p <- p + scale_x_date(format = "%b")
	p <- p + labs(x = "Date", y = "Number of Watefowl")
	ggsave(file ='1.3_season_zone.jpg', width=6, height =8,  path = paste(dir, "figures", sep = ""))


# 1.4. Are julien and alr colinear? an xy plot showing typical reservoir operations, with strong pattern!

p<-qplot(julien,alr,data=dat)
p


# 1.4.1 What about depth?

p<-qplot(julien,depth,data=dat, alpha = 0.2)
p


# 1.5 Use Variance Inflation Factors to assess collinearity of depth and julien


library(AED)

depth = dat$depth
julien = dat$julien
year = dat$year
x=cbind(depth,julien,year)
corvif(x)

# all less than 3, so this is excellent













#..............END





