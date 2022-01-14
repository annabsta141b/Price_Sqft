
###################### read in data ######################################
KingCounty=read.csv("~/LocalDocuments/UCDavis/Year2/STA108/datasets/KingCounty.csv")

#################### EDA ##############################
names(KingCounty)
sum(is.na(KingCounty)) #no missing values

### plot linear relationship of X & Y
library(ggplot2)
ggplot(KingCounty,aes(x=sqft_living, y=price))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)+
  ggtitle("Sqft. vs House Price")+
  ylab("Price of Houses")+
  xlab("Square Feet")+
  theme_bw()

#### histogram of Y
ggplot(KingCounty, aes(x = sqft_living)) +
  geom_histogram(binwidth = 400)+
  theme_bw()

### boxplot
boxplot(KingCounty$sqft_living, main="Boxplot")

#### summary stats

summary(KingCounty)
##################### Diagnostics #######################################

## regression line
king_reg_line = lm(price~sqft_living, data=KingCounty)

par(mfrow= c(2,2))
## QQ plot(aka test for normality, points should follow the line closely)
qqnorm(king_reg_line$residuals)
qqline(king_reg_line$residuals)

### scatterplot Y vs X
plot(KingCounty$sqft_living, KingCounty$price, main="Scatterplot", pch = 19,font = 2,
     font.lab = 2, ylab = "Y variable",xlab = "X variable")

### histogram
hist(king_reg_line$residuals,main = "Residuals", xlab = "ei",
     pch = 19,font = 2,font.lab = 2)

### resids vs X
plot(king_reg_line$residuals, KingCounty$sqft_living, main = "Scatterplot",
     pch = 19,font = 2,font.lab = 2, ylab = "ei values",xlab = "X variable")
abline(h = 0, lwd = 2, col = "purple")

############# test for constant variance

###plot ei vs fitted values
library(scales)
par(mfrow = c(1,1))
plot(king_reg_line$fitted.values, king_reg_line$residuals, xlab = "Fitted Values",
     ylab="Errors", main = "Errors vs Fitted Values")
abline(h = 0,col = "purple")


KingCounty$yhat = king_reg_line$fitted.values
KingCounty$ei = king_reg_line$residuals

qplot(yhat,ei, data=KingCounty)+ggtitle("Errors vs Fitted Values") + 
  xlab("Fitted Values") + geom_hline(yintercept = 0,col = "purple")+
  scale_y_continuous(labels = comma)+
  scale_x_continuous(labels = comma)
  

########  Fligner Killeen test (FK test) 

Group = rep("Lower",nrow(KingCounty)) #Creates a vector that repeats "Lower" n times
Group[KingCounty$price > median(KingCounty$price)] = "Upper" #Changing the appropriate values to "Upper"
Group = as.factor(Group) #Changes it to a factor, which R recognizes as a grouping variable.
cats$Group = Group
the.FKtest= fligner.test(KingCounty$ei, KingCounty$Group)
the.FKtest


######## Shapiro Wilks test (SW test) 
shapiro.test(KingCounty$ei)

######## outliers
# from the plot of Y vs X we have 1 outlier which is a house >$4million with >8000 sqft
ei.s = king_reg_line$residuals/sqrt(sum(king_reg_line$residuals^2)/(nrow(KingCounty) -length(king_reg_line$coefficients)))
ri = rstandard(king_reg_line)
ti = rstudent(king_reg_line)

alpha = 0.01
n = nrow(KingCounty)
p = length(king_reg_line$coefficients)
cutoff = qt(1-alpha/(2*n), n -p )
cutoff_deleted = qt(1-alpha/(2*n), n -p -1 )

outliers = which(abs(ei.s)> cutoff | abs(ri) > cutoff | abs(ti) > cutoff_deleted)
outliers

newKingCounty = KingCounty[-outliers,]

################### hypothesis test & confidence intervals ########################

## eqn of line w/ outliers removed
no_outlers_model = lm(price~sqft_living, data=newKingCounty)

no_outlers_model
##HT
all.sum = summary(no_outlers_model)
HT = all.sum$coefficients
HT.b1 = HT[2,]
HT.b1


confint(no_outlers_model, level=0.95)

#################### predictions ##############################
## avg house price for houses with living square footage 2800.
xs2 = data.frame(sqft_living = 2800) 
pred.int = predict(no_outlers_model,xs2,interval = "confidence",se.fit = TRUE,level = 0.95)
CI = pred.int$fit 
CI
## house with living square footage 3200.
xs3 = data.frame(sqft_living = 3200) 
pred.int3 = predict(no_outlers_model,xs3,interval = "prediction",se.fit = TRUE,level = 0.95)
pred.int3$fit

## house with living square footage 8000.

xs4 = data.frame(sqft_living = 8000) 
pred.int4 = predict(no_outlers_model,xs4,interval = "prediction",se.fit = TRUE,level = 0.95)
pred.int4$fit
