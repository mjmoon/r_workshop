#------------------------------------------------------------#
#                                                            #
#         Introduction to data analysis with R               #
#                                                            #
#------------------------------------------------------------#

library(xlsx)  ## require To read the excel (.xlsx) files
library(gdata)

### Set working directory : All the Finala we read and write will be in this directory ###
# setwd('~/Desktop/R_intro')

## Read The Finala from .csv file ##
Final<- read.csv('data/R_intro_data.csv', header = T)

## Read The Finala from .xlsx file ##
Final_xlsx<- read.xlsx("data/R_intro_excel.xlsx", sheetName="Data")
# Final_xlsx <- read.xls("data/R_intro_excel.xlsx", sheet=1, header=TRUE)

#--------------------------------------------------#
#                                                  #
#                Data management                   #
#                                                  #
#--------------------------------------------------#


## Check dimensions and names and type of variables ##
dim(Final)
data.frame(names(Final))
head(Final)
summary(Final)

## For categorical variables: Check table, proportion tables and plots ##
y<- summary(Final$Race)
cbind(y, prop.table(y))
cbind(y, prop.table(y)*100)
paste(y, " (", prop.table(y)*100, "%", ")", sep="") 

## What would be different if y<- table(Final$Sex) 
paste(table(Final$Race), " (", prop.table(table(Final$Race))*100, "%", ")", sep="") 

## Same way do it for race, area, smoke and ses ##



######  Missing value and cleaning the data for categorical data  ####### 

sum(is.na(Final$Sex))
sum(is.na(Final$Race))
sum(is.na(Final$Area))
sum(is.na(Final$SES))
sum(is.na(Final$smoke))

Final$smoke[Final$smoke==-1]<- NA
Final$Race[is.na(Final$Race)]<- 3   ## What Happens??

Final$Race<- as.numeric(Final$Race)
Final$Race[is.na(Final$Race)]<- max(Final$Race)+1
summary(Final$Race)

Final$Race[is.na(Final$Race)]<- max(Final$Race, na.rm = T)+1

Final$Race<- factor(Final$Race, labels = c("Other", "White", "Missing"))


Final$SES<- as.numeric(Final$SES)

Final$SES[is.na(Final$SES)]<- max(Final$SES, na.rm = T)+1

Final$SES<- factor(Final$SES, labels = c("High", "Midium", "Low", "Missing"))


###  Some Simple plots  ###

SES<- table(Final$SES)

x<-barplot(SES, names.arg=c("High", "Medium", "Low", "Missing"))

x<-barplot(SES, names.arg=c("High", "Medium", "Low", "Missing"), ylim=c(0,60))

x<-barplot(SES, names.arg=c("High", "Medium", "Low", "Missing"), ylim=c(0,60), 
           ylab="Percent(%)", xlab="Socio Economic Status", col=c("Blue", "Green", "Red", "Yellow"))

par(family = 'serif')

x<-barplot(SES, names.arg=c("High", "Medium", "Low", "Missing"), ylim=c(0,60), ylab="Percent(%)", xlab="Socio Economic Status", col=c("Blue", "Green", "Red", "Yellow"), main="Figure 3A: Socio Economic Status of some group in Ontario area with Lung cancer incidences")


x<-barplot(SES, names.arg=c("High", "Medium", "Low", "Missing"), 
           ylim=c(0,60), 
           ylab="Percent(%)", xlab="Socio Economic Status", 
           col=c("Blue", "Green", "Red", "Yellow"), 
           main="Figure 3A: Socio Economic Status of some group \n in
Ontario area with 
           Lung cancer \n incidences")
text(x,SES+2,labels=round(SES,0), col="Black")

## For two variables

SES_smk<- prop.table(table(Final$SES, Final$smoke),2)

## Stack plot
x<-barplot(SES_smk*100, names.arg=c("Non smoker", "Smoker"), 
           ylim=c(0,100), ylab="Percent(%)", xlab="Socio Economic Status", 
           col=c("Blue", "Green", "Red", "Yellow"), 
           main="Figure 3A: Socio Economic Status of some group \n in Ontario area with Lung cancer \n incidences")

## Side by side
x<-barplot(SES_smk*100, names.arg=c("Non smoker", "Smoker"), ylim=c(0,50), ylab="Percent(%)", xlab="Socio Economic Status", col=c("Blue", "Green", "Red", "Yellow"), main="Figure 3A: Socio Economic Status of some group \n in Ontario area with Lung cancer \n incidences", beside = T, cex.main=0.8)
text(x,SES_smk*100+2,labels=round(SES_smk*100,0), col="Black")


####   Bivariate Association  ####

tab<- table(Final$Sex, Final$smoke)

prop.table(tab)

prop.table(tab, margin = 1)

round(prop.table(tab, margin = 2), 2)#*100

chisq.test(Final$Sex, Final$smoke)

#chisq.test(Final$Sex[!is.na(Final$smoke)], Final$smoke[!is.na(Final$smoke)])


## For Continuous variables: Check mean, SD, summary and plots ##

#  For the Age variable: A covariate, possible confounder  ##  

mean(Final$Age)   ### Why??

mean(Final$Age, na.rm = T)

sd(Final$Age, na.rm = T)

summary(Final$Age)

boxplot(Final$Age)

hist(Final$Age)

Final$Age[is.na(Final$Age)]<- 99

Final$Cat_Age<- cut(Final$Age, quantile(Final$Age, prob=c(0.0, 0.2, 0.4, 0.6, 0.8, 1), labels=F))

Final$Cat_Age<- cut(Final$Age, c(59, 65, 71, 99))

Final$Cat_Age<- cut(Final$Age, c(59, 65, 71, 99), labels = c("60-65", "65-70", "Missing"))


#  For the Blood pressure variable: Exposure for survival analysis and one of the outcomes  #  

mean(Final$BP, na.rm = T)

sd(Final$BP, na.rm = T)

which(is.na(Final$BP))

summary(Final$BP)

boxplot(Final$BP)

boxplot(Final$BP~Final$Sex)

boxplot(Final$BP~Final$Sex + Final$smoke)

hist(Final$BP)


## Summarizing blood pressure by categorical variables ##

x<-aggregate(Final$BP, by = list(Final$SES, Final$smoke), mean, na.rm = T)

colnames(x)<- c("SES", "Smoking", "BP")

xtabs(x$BP~ x$SES + x$Smoking)

round(xtabs(x$BP~ x$SES + x$Smoking), 2)




#-----------------------------------------------#
#                                               #
#                Data Analysis                  #
#                                               #
#-----------------------------------------------#


## Linear regression prediction and confidence intervals

lr_BP<- lm(BP ~ smoke + SES + Cat_Age + Sex + Race + Area, data = Final)

lr_BP<- lm(BP ~ Age, data = Final)

plot(Final$Age, Final$BP)

k<-cbind(k<-loess.smooth(Final$Age, Final$BP)$x, loess.smooth(Final$Age, Final$BP)$y)

plot(k[,1], k[,2], main="Lowess smoth graphs",type = 'l', xlab = "Age in years", ylab="Average BP", col = "Red", lwd = 2, xlim = c(60,100))

lr_BP<- lm(BP ~ Age, data = Final[Final$Age!=99 & !is.na(Final$BP),])

k<-cbind(k<-loess.smooth(Final$Age[Final$Age!=99], Final$BP[Final$Age!=99])$x, loess.smooth(Final$Age[Final$Age!=99], Final$BP[Final$Age!=99])$y)

plot(k[,1], k[,2], main="Lowess smoth graphs",type = 'l', xlab = "Age in years", ylab="Average BP", col = "Red", lwd = 2, xlim = c(60,70))

conf<-predict(lr_BP, interval=c("confidence"))
prd<-predict(lr_BP, interval=c("prediction"))

pred<- cbind(Final$Age[Final$Age!=99 & !is.na(Final$BP)], conf[,1], conf[,2], conf[,3], prd[,2], prd[,3])

pred<- pred[order(pred[,1]),]

plot(pred[,1], pred[,2], type = "l", lty = 1, col = "black", ylim=c(50,100), xlab = "Age in years", ylab="Predicted Blood Pressure", main = "Prediction and confidence interval")
lines(pred[,1], pred[,3], lty = 1, col = "red")
lines(pred[,1], pred[,4], lty = 1, col = "red")
lines(pred[,1], pred[,5], lty = 2, col = "blue")
lines(pred[,1], pred[,6], lty = 2, col = "blue")

pdf("Figure 1", pointsize = 13)
par(family='serif')
plot(pred[,1], pred[,2], type = "l", lty = 1, col = "black", ylim=c(50,100), xlab = "Age in years", ylab="Predicted Blood Pressure", main = "Prediction and confidence interval", bty = 'n')
lines(pred[,1], pred[,3], lty = 1, col = "red")
lines(pred[,1], pred[,4], lty = 1, col = "red")
lines(pred[,1], pred[,5], lty = 2, col = "blue")
lines(pred[,1], pred[,6], lty = 2, col = "blue")
dev.off()


## Logistic regression

glm_smk<- glm(smoke ~ Race + Sex + Cat_Age + SES + Area , data = Final, family = binomial(link = "logit"))

glm_smk<- glm(smoke ~ Sex + Cat_Age + SES + Area , data = Final, family = binomial(link = "logit"))


#### Managing dates ####

as.Date("01/01/2010", "%d/%m/%Y")

as.Date("01/01/2010", "%d/%m/%y")

as.Date("01/01/2010", "%d/%M/%Y")


#### Managing the dates in the data and calculating survival time  ####

Final$start1<- as.Date(Final$Start, "%d/%m/%Y")

Final$stop1<- as.Date(Final$stop, "%d/%m/%Y")

Final$surv<- as.numeric(Final$stop1) - as.numeric(Final$start1)

Final$start2<-as.POSIXct(strptime(Final$Start, "%d/%m/%Y"))

Final$stop2<-as.POSIXct(strptime(Final$stop, "%d/%m/%Y"))

Final$surv1<- as.numeric(Final$stop2) - as.numeric(Final$start2)


### Survival Analysis ####

library(survival)

y<- coxph(Surv(surv, event) ~ Cat_Age + Sex + smoke + BP + Race + Area + SES, data = Final)
y<- coxph(Surv(surv, event) ~ Cat_Age + Sex + smoke + BP + Area + SES, data = Final)

est<-exp(coef(y))

CI<- exp(confint(y))

est_CI<- paste(round(est,2), " (", round(CI[,1],2), ", ", round(CI[,2], 2), ")", sep="")

est_CI_p<- cbind(est_CI, round(summary(y)$coefficients[,5],2))

colnames(est_CI_p)<- c("HR (95% CI)", "p-value")


write.csv(est_CI_p, "Hazard Ratio Estimate.csv")

# http://www.stat.berkeley.edu/~s133/dates.html
