
setwd("C:/Statistics") #set to own working directory C:/Statistics)


Boston=read.csv("boston.csv",header=T,na.strings="?")  # Loading Boston Data from CSV
fix(Boston)     # print dataframe
names(Boston)    # list attributes
str(Boston)

#The pears diagrams of all values
pairs = testpairs [,c("CRIM", "DIS", "PT", "B", "LSTAT")]
pairs(~ Boston$CRIM + Boston$DIS + Boston$PT + Boston$B + Boston$LSTAT, Boston, main ="Pairs matrix",  col="dodgerblue4") #Scatter diagram of values which beeing analyzes in the assignement
pairs(~ Boston$CRIM + Boston$ZN + Boston$INDUS + Boston$CHAS + Boston$NOX, Boston, main ="Pairs matrix",  col="dodgerblue4") 
pairs(~ Boston$CRIM + Boston$RAD + Boston$TAX + Boston$MV + Boston$RM 
      + Boston$AGE, Boston, main ="Pairs matrix",  col="dodgerblue4")  

#Regression analysis of all variables in the CRIM data set

#7  DIS
ln.fit=lm(Boston$CRIM~Boston$DIS) #Correletion with weighted mean of distance to five Boston employment centres
ln.fit
coef(ln.fit) 
confint(ln.fit)
plot(Boston$DIS,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Weighted distance to five Boston employment centers", ylab="Crime")
abline(ln.fit, lwd=2, col = "red")
summary(ln.fit)
cor(Boston$DIS,Boston$CRIM)

# DIS quadratic and cubic models 
fit.dis <- lm(Boston$CRIM~poly(Boston$DIS,3))
summary(fit.dis)
              
#residual for DIS
RESIDUALS<-residuals(ln.fit) #for each variable x wwill bring error msg
ln.fitr<-lm(RESIDUALS~Boston$DIS) #distribution of risidual errors
plot(Boston$DIS, RESIDUALS, col="dodgerblue4", xlab = "DIS")
abline(ln.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS) #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS)
ln.fit=lm(Boston$CRIM~poly(Boston$DIS,2)) #doesnt work
plot(Boston$DIS,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Weighted distance to five Boston employment centers", ylab="Crime")
abline(ln.fit, lwd=2, col = "red")

#10 PT
lb.fit=lm(Boston$CRIM~Boston$PT) #Correletion with pupil-teacher ratio by town
lb.fit
coef(lb.fit) 
confint(lb.fit)
plot(Boston$PT,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Pupil-teacher ratio by town", ylab="Crime")
abline(lb.fit, lwd=2, col = "red")
summary(lb.fit)
cor(Boston$PT,Boston$CRIM)

#quadratic and cubic models 
fit.pt <- lm(Boston$CRIM~poly(Boston$PT,3))
summary(fit.pt)

#residual for PT
RESIDUALS<-residuals(lb.fit) #for each variable x wwill bring error msg
lb.fitr<-lm(RESIDUALS~Boston$PT) #distribution of risidual errors
plot(Boston$PT, RESIDUALS, col="dodgerblue4", xlab = "PT")
abline(lb.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS) #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS)

#11 B
lv.fit=lm(Boston$CRIM~Boston$B) #Proportion of the IC2 and IC6 race people by town
lv.fit
coef(lv.fit) 
confint(lv.fit)
plot(Boston$B,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Proportion of the IC2 and IC6 race people by town", ylab="Crime")
abline(lv.fit, lwd=2, col = "red")
summary(lv.fit)
cor(Boston$B,Boston$CRIM)
#quadratic and cubic models 
fit.b <- lm(Boston$CRIM~poly(Boston$B,3))
summary(fit.b)

#residuals for B
RESIDUALS<-residuals(lv.fit) #for each variable x wwill bring error msg
lv.fitr<-lm(RESIDUALS~Boston$B) #distribution of risidual errors
plot(Boston$PT, RESIDUALS, col="dodgerblue4", xlab = "B")
abline(lv.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS) #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS)

#12 LSTAT
lm.fit=lm(Boston$CRIM~Boston$LSTAT) #Correletion with Lower Status of the population variable
lm.fit
coef(lm.fit) 
confint(lm.fit)
plot(Boston$LSTAT,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Lower status of the population (%)", ylab="Crime")
abline(lm.fit, lwd=2, col = "red")
summary(lm.fit) #shows r squered relation ship between variable (Adjsuted r-squere)
cor(Boston$LSTAT,Boston$CRIM)

fit.lstat <- lm(Boston$CRIM~poly(Boston$LSTAT,3))
summary(fit.lstat)

library(polynom)
fit <-lm(Boston$CRIM~Boston$LSTAT+I(Boston$LSTAT^2))
plot(Boston$LSTAT,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Lower status of the population (%)", ylab="Crime")
points(Boston$LSTAT, fitted(fit), col='red', pch=20)
lines(Boston$LSTAT, fitted(fit), col='red', type='b')
lines(sort(Boston$LSTAT), fitted(fit)[order(Boston$LSTAT)], col='red', type='b') 
summary(fit)
           
RESIDUALS<-residuals(lm.fit) #for each variable x wwill bring error msg
lm.fitr<-lm(RESIDUALS~Boston$LSTAT) #distribution of risidual errors
plot(Boston$LSTAT, RESIDUALS, col="dodgerblue4", xlab = "LSTAT")
abline(lm.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS) #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS)

#polynomial
newdat = data.frame(LSTAT = seq(min(Boston$LSTAT), max(Boston$LSTAT), length.out = 506))
newdat$pred = predict(fit, newdata = newdat)
plot(Boston$LSTAT,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Lower status of the population (%)", ylab="Crime")
with(newdat, lines(x=Boston$LSTAT, y = pred, lwd=2, col = "red"))
summary(fit)

#1  ZN
lc.fit=lm(Boston$CRIM~Boston$ZN) #Proportion of residential land zoned for lots over 25,000 sq.ft. 
lc.fit
coef(lc.fit) 
confint(lc.fit)
plot(Boston$ZN,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Proportion of residential land zoned for lots over 25,000 sq.ft. ", ylab="Crime")
abline(lc.fit, lwd=2, col = "red")
summary(lc.fit)
cor(Boston$ZN, Boston$CRIM)

fit.zn <- lm(Boston$CRIM~poly(Boston$ZN,3))
summary(fit.zn)

RESIDUALS<-residuals(lc.fit) #for each variable x wwill bring error msg
lc.fitr<-lm(RESIDUALS~Boston$ZN) #distribution of risidual errors
plot(Boston$ZN, RESIDUALS, col="dodgerblue4", xlab = "ZN")
abline(lc.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS, col="dodgerblue4") #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS,lwd=2, col = "red")


#2  INDUS
lx.fit=lm(Boston$CRIM~Boston$INDUS) #Proportion of non-retail business acres per town 
lx.fit
coef(lx.fit) 
confint(lx.fit)
plot(Boston$INDUS,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Proportion of non-retail business acres per town", ylab="Crime")
abline(lx.fit, lwd=2, col = "red")
summary(lx.fit)

fit.indus <- lm(Boston$CRIM~poly(Boston$INDUS,3))
summary(fit.indus)

RESIDUALS<-residuals(lx.fit) #for each variable x wwill bring error msg
lx.fitr<-lm(RESIDUALS~Boston$INDUS) #distribution of risidual errors
plot(Boston$INDUS, RESIDUALS, col="dodgerblue4", xlab = "INDUS")
abline(lx.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS, col="dodgerblue4") #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS,lwd=2, col = "red")

#3  CHAS
lz.fit=lm(Boston$CRIM~Boston$CHAS) #Charles River dummy variable 
lz.fit
coef(lz.fit) 
confint(lZ.fit)
plot(Boston$CHAS,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Charles River dummy variable", ylab="Crime")
abline(lZ.fit, lwd=2, col = "red")
summary(lz.fit)


fit.chas <- lm(Boston$CRIM~poly(Boston$CHAS,1))
summary(fit.chas)

RESIDUALS<-residuals(lZ.fit) #for each variable x wwill bring error msg
lz.fitr<-lm(RESIDUALS~Boston$CHAS) #distribution of risidual errors
plot(Boston$CHAS, RESIDUALS, col="dodgerblue4", xlab = "CHAS")
abline(lZ.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS, col="dodgerblue4") #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS,lwd=2, col = "red")

#4  NOX
la.fit=lm(Boston$CRIM~Boston$NOX) #Nitric oxides concentration 
la.fit
coef(la.fit) 
confint(la.fit)
plot(Boston$NOX,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Nitric oxides concentration (parts per 10 million)", ylab="Crime")
abline(la.fit, lwd=2, col = "red")
summary(la.fit)

fit.nox <- lm(Boston$CRIM~poly(Boston$NOX,3))
summary(fit.nox)

RESIDUALS<-residuals(la.fit)
la.fitr<-lm(RESIDUALS~Boston$NOX) #distribution of risidual errors
plot(Boston$NOX, RESIDUALS, col="dodgerblue4", xlab = "NOX")
abline(la.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS, col="dodgerblue4") #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS,lwd=2, col = "red")

#8  RAD
ls.fit=lm(Boston$CRIM~Boston$RAD) #Index of accessibility to radial highways 
ls.fit
coef(ls.fit) 
confint(ls.fit)
plot(Boston$RAD,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Index of accessibility to radial highways", ylab="Crime")
abline(ls.fit, lwd=2, col = "red")
summary(ls.fit)

fit.rad <- lm(Boston$CRIM~poly(Boston$RAD,3))
summary(fit.rad)

RESIDUALS<-residuals(ls.fit) #for each variable x wwill bring error msg
ls.fitr<-lm(RESIDUALS~Boston$RAD) #distribution of risidual errors
plot(Boston$RAD, RESIDUALS, col="dodgerblue4", xlab = "RAD")
abline(ls.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS, col="dodgerblue4") #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS,lwd=2, col = "red")


#9  TAX
ld.fit=lm(Boston$CRIM~Boston$TAX) #Full-value property-tax rate per $10,000 
ld.fit
coef(ld.fit) 
confint(ld.fit)
plot(Boston$TAX,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Full-value property-tax rate per $10,000 ", ylab="Crime")
abline(ld.fit, lwd=2, col = "red")
summary(ld.fit)

fit.tax <- lm(Boston$CRIM~poly(Boston$TAX,3))
summary(fit.tax)

RESIDUALS<-residuals(ld.fit) #for each variable x wwill bring error msg
ld.fitr<-lm(RESIDUALS~Boston$TAX) #distribution of risidual errors
plot(Boston$TAX, RESIDUALS, col="dodgerblue4", xlab = "TAX")
abline(ld.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS, col="dodgerblue4") #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS,lwd=2, col = "red")

#13 MV
lf.fit=lm(Boston$CRIM~Boston$MV) #Median value of owner-occupied homes in $1000's
lf.fit
coef(lf.fit) 
confint(lf.fit)
plot(Boston$MV,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Median value of owner-occupied homes in $1000's", ylab="Crime")
abline(lf.fit, lwd=2, col = "red")
summary(lf.fit)

fit.mv <- lm(Boston$CRIM~poly(Boston$MV,3))
summary(fit.mv)

RESIDUALS<-residuals(lf.fit) #for each variable x wwill bring error msg
lf.fitr<-lm(RESIDUALS~Boston$MV) #distribution of risidual errors
plot(Boston$MV, RESIDUALS, col="dodgerblue4", xlab = "MV")
abline(lf.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS, col="dodgerblue4") #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS,lwd=2, col = "red")

#5  RM
lg.fit=lm(Boston$CRIM~Boston$RM) #Median value of owner-occupied homes in $1000's
lg.fit
coef(lg.fit) 
confint(lg.fit)
plot(Boston$RM,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Average number of rooms per dwelling ", ylab="Crime")
abline(lg.fit, lwd=2, col = "red")
summary(lg.fit)

fit.rm <- lm(Boston$CRIM~poly(Boston$RM,3))
summary(fit.rm)

RESIDUALS<-residuals(lg.fit) #for each variable x wwill bring error msg
lg.fitr<-lm(RESIDUALS~Boston$RM) #distribution of risidual errors
plot(Boston$RM, RESIDUALS, col="dodgerblue4", xlab = "RM")
abline(lg.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS, col="dodgerblue4") #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS,lwd=2, col = "red")

#6  AGE
lh.fit=lm(Boston$CRIM~Boston$AGE) #Proportion of owner-occupied units built prior to 1940 
lh.fit
coef(lh.fit) 
confint(lh.fit)
plot(Boston$AGE,Boston$CRIM, pch=20, col="dodgerblue4", xlab="Proportion of owner-occupied units built prior to 1940 ", ylab="Crime")
abline(lh.fit, lwd=2, col = "red")
summary(lh.fit)

fit.age <- lm(Boston$CRIM~poly(Boston$AGE,3))
summary(fit.age)

RESIDUALS<-residuals(lh.fit) #for each variable x wwill bring error msg
lh.fitr<-lm(RESIDUALS~Boston$AGE) #distribution of risidual errors
plot(Boston$AGE, RESIDUALS, col="dodgerblue4", xlab = "AGE")
abline(lh.fitr, lwd=2, col = "red")
qqnorm(RESIDUALS, col="dodgerblue4") #x line normal distrubution and trying to map 1:1 teoretical values and risidual values
qqline(RESIDUALS,lwd=2, col = "red")

library(corrplot) #heat colleration
M<-cor(Boston)
corrplot(M, order = "hclust", addrect = 2)

cor(Boston[-c(0, 4)])

#or

library(MASS)
library(corrplot) 

# cor function calculates the pearson correlation coefficient
cor(Boston)

# corrplot plots the correlations
corrplot(cor(Boston), method = "ellipse")

chas.des = table (Boston$CHAS) #pie diagram for charlies dummy variable
pie (chas.des, main = "Charles River dummy variable
     1 if tract bounds river; 
     0 otherwise") 

summary (Boston) #summary of all data


# Multiple Linear Regression

# In order to fit a multiple regression model using least squares, we
# again use the lm() function. The syntax lm(y~x1+x2+x3) is used to fit
# a model with three persdictors, x1, x2, x3. The summary() function now
# outputs the regression coefficients for all the predictors.

lm.fit=lm(CRIM~LSTAT+AGE+NOX,data=Boston) #fit one than more vriabele
summary(lm.fit) #

# to fit all predictors use CRIM~. 
lj.fit=lm(CRIM~.,data=Boston)#~. all variable fited in the model, all variables related to the CRIME
summary(lj.fit) #change lm variable
coef(lj.fit) #all coficiaents

#simple coef
coef(lc.fit)
coef(lx.fit)
coef(lZ.fit)
coef(la.fit)
coef(lg.fit)
coef(lh.fit)
coef(ln.fit)
coef(ls.fit)
coef(ld.fit)
coef(lb.fit)
coef(lv.fit)
coef(lm.fit)
coef(lf.fit)

Simple = c(-0.07393,0.5097763, -1.892777, 31.24853, -2.684051,0.1077862,-1.550902, 0.6179109, 0.02974225, 1.151983, -0.03627964, 0.5488048, -0.3631599) #add single estimate (variable for each can be assigned as coef)
Multiple = c(0.044855, -0.063855, -0.749134,-10.313531538, 0.430130434, 0.001451641, -0.987175625, 0.588208588, -0.003780016, -0.271080510, -0.007537506, 0.126211368, -0.1988886813) #add all multiple values

plot (Simple, Multiple, pch= 1, lwd = 7, col="dodgerblue4")
lq.fit=lm(Simple~Multiple)
abline(lq.fit, lwd=2, col = "red")
summary(lq.fit)

# Non-linear Transformations of the Predictors
lm.fit2=lm(CRIM~LSTAT+I(LSTAT^2),data=Boston)

summary(lm.fit2)

# the poly function can be used to plot hogher order polynomials
lm.fit3=lm(CRIM~poly(LSTAT,5),data=Boston)
summary(lm.fit3)


