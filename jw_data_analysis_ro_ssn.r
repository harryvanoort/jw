# CBA - Harry van Oort
#this code comes with no warranty


#########INPUT DATA REQUIRED To run script################
# 'jw.csv'
####################################################



#CLBMON40 Jarvis and Woods SCRIPT


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

dat$occasion <- dat$year + dat$julien/365
dat$year <- as.factor(dat$year)
dat$date <- as.Date(dat$date)
dat$ssn <- as.Date(dat$ssn)
dat$month <- as.factor(dat$month)
dat$zone <- as.factor(dat$zone)
dat$occasion <- as.factor(dat$occasion)


#dat$total_wf <- as.numeric(dat$total_wf)

#NOTES
#ZONES: A=9 Mile, B=Robs Willows, C=Cartier Bay, D=Montana Bay, E=LCO, F=Airport Marsh, G=Downie Marsh


#		START ANALYSIS


# See original jw script for M1

# First decent model - Poisson, yearly smoother, wetland X depth smoother, and year
M2 <- gam(wf ~ s(julien, bs="cc")+ s(depth, by = zone, bs="cr") + year, data = dat, family=poisson)

	# Shows clear non-linear effects of depth at each zone
	# Shows clear non-linear effects of julien
	# Very narrow confidence intervals - SUSPICIOUS!
	# Residuals are biassed low for each zone, and throughout julien

#...time to change distribution, primarily because of the confidence interval, and residuals

# Next two use quasipoisson... M4 has a year term
M3 <- gam(wf ~ s(julien, bs="cc")+ s(depth, by = zone, bs="cr"), data = dat, family=quasipoisson)
M4 <- gam(wf ~ s(julien, bs="cc")+ s(depth, by = zone, bs="cr") + year, data = dat, family=quasipoisson)

	# Confidence intervals look a lot more believable for both of these
	# Residuals are still biased low
	# Some patterns in the residuals across julien





############################################################# MODEL CHECKS/VALIDATION

# GAM

MC <- M4 # use for gam, glm


summary(MC)
anova(MC)
plot(MC,scale=0)
gam.check(MC)

	df <- data.frame(residuals(MC),fitted(MC), dat$julien, dat$zone, dat$depth, dat$alr)
	qplot(fitted.MC., residuals.MC., data = df, size = 3, alpha = 0.1)
	qplot(dat$julien, residuals.MC., data = df, size = 3, alpha = 0.1)
	qplot(dat$zone, residuals.MC., data = df, geom = "boxplot")
	qplot(dat$year, residuals.MC., data = df, geom = "boxplot")


vis.gam(MC,view=c("depth","zone"),theta=45)


### GAMM


MM <- M7 # use for gamm
summary(MM$gam)
anova(MM$gam)
plot(MM$gam,scale=0)
gam.check(MC)



############################################################# END 


#not sure if this works... need to let it crunch.

MW2 <- gam(wf ~ s(julien, bs="cc")+ s(alr, by = zone, bs="cr") + year, data = dat, family = negbin(1))






##### Okay, time to start tweaking this model. GAMM1 is what i am shooting for:

GAMM1 <- gamm(wf ~ s(depth, by = zone, bs="cr") + s(julien, bs="cc"), 
	random = list(year =~ 1), data = dat, family=poisson)    #Does not converge!








###SOME OLD MODELS

#Mixed model, with nested day, smoother for depth at each site.
#M7 <- gamm(wf ~ s(depth, by = zone, bs="cr"), random = list(occasion =~1), data = dat, family=quasipoisson)
	# fails to converget no matter how i nest the occasion, 




#try offset total_wf instead of nesting to control for seasonal effects.

M8 <- gam(wf ~ offset(total_wf) + s(depth, by = zone, bs="cr"), data = dat, family = quasipoisson)
# get error message to do with  the offset - should figure this out

#Next, how about PROPORTIONAL data, try that with binomial
dat$wf_neg <- dat$total_wf - dat$wf
dat$wf_pos <- dat$wf #just to make this obvious!

M9 <- glm(cbind(wf_pos,wf_neg)~ depth*zone, family = binomial, data = dat) #Clearly not linear....
M10 <- gam(cbind(wf_pos,wf_neg)~ s(depth, by = zone, bs="cr"), family = binomial, data = dat)









#Question 2.2. Does the proportion of birds vary with reservoir elevation?



#..............END





