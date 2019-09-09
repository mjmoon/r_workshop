# Data retreived from BuzzFeed
# https://github.com/BuzzFeedNews/2015-12-mass-shooting-intervals

# read and match names
data.dir <- paste0(getwd(), '/Projects/r_workshop/data') # change to your path
df <- read.csv(paste0(data.dir, '/shooting-usa.csv'))
# class conversion
df$Location <- as.character(df$Location)
df$Date <- as.Date(df$Date, '%m/%d/%Y')
# date attributes
df$Year <- as.numeric(format(df$Date,'%Y'))
df$Month <- months(df$Date)
df$Week.Day <- weekdays(df$Date)
df$Date.Ind <- julian(df$Date, as.Date('2013-01-01'))
head(df[c('Date', 'Year', 'Month', 'Week.Day', 'Date.Ind')])
# encode month and week day as factors
df$Month <- factor(
  df$Month, 
  c('January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December'))
df$Week.Day <- factor(
  df$Week.Day, c('Sunday', 'Monday', 'Tuesday', 
                 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
# text manipulation - extract states
df$State <- sub('.*, ?', '', df$Location)
unique(df$State)
# fix oddities
df$State <- toupper(df$State)
df$State[grep('D.?C.?', df$State)] <- 'DC'
df$State[grep('ILLINOIS', df$State)] <- 'IL'
df$State[grep('KANSAS', df$State)] <- 'KS'
df$State[grep('LOUISIANA', df$State)] <- 'LA'
df$State[grep('PUERTO', df$State)] <- 'PR'
df$State[grep('TENNESSEE', df$State)] <- 'TN'
sort(unique(df$State))
# ---------------- plotting ---------------- #
# color scheme setting
library(RColorBrewer)
library(scales)
palette(brewer.pal(8, 'Accent'))
# scatter plot
plot(df$Injured, df$Dead, 
     col=alpha(brewer.pal(8, 'Accent')[2], 0.5),
     pch=16, axes=FALSE, xlim=c(0,20), ylim=c(0,15),
     xlab='Injuries', ylab='Fatalities')
axis(1, c(0, 10, 20))
axis(2, c(0, 5, 10, 15))
mtext('Number of fatalities and injuries per shooting incident\nin the U.S. between 2013 and 2015.', adj=0)
# line chart
# sum by month
df$Year.Month <- format(df$Date,'%Y-%m')
incident.by.ym <- table(df$Year.Month)
injury.by.ym <- tapply(df$Injured, df$Year.Month, sum)
dead.by.ym <- tapply(df$Dead, df$Year.Month, sum)
# plot
plot(injury.by.ym, col=2, type='l', ylim=c(0,200), axes=FALSE)
lines(c(incident.by.ym), col=8)
lines(dead.by.ym, col=6)
axis(1, c(-5, 1, 13, 25, 37, 50), c('','2013', '2014', '2015', '2016', ''))
axis(2, c(0, 100, 200))
legend(
  0, 200, c('Incident count', 'Injury count', 'Fatality count'),
  bty='n', col=c(8, 2, 6), lty=1, cex=0.7)
mtext('Fatalities and injuries from shooting incidents in the U.S per month over time.',
      adj=0)
# barplot
# create a barplot and save the x-coordinates of the mid-points
plot.new()
xc <- barplot(table(df$Month), border=NA, 
              names.arg=substr(levels(df$Month),0,3),
              cex.names=0.7, ylim=c(0,200), axes=FALSE)
lines(xc, tapply(df$Injured, df$Month, sum)/c(rep(3,11),2), col=2)
points(xc, tapply(df$Injured, df$Month, sum)/c(rep(3,11),2), pch=16, col=2)
lines(xc, tapply(df$Dead, df$Month, sum)/c(rep(3,11),2), col=6)
points(xc, tapply(df$Dead, df$Month, sum)/c(rep(3,11),2), pch=16, col=6)
axis(2, c(0, 100, 200), las=2)
mtext('Fatalities and injuries from shooting incidents in the U.S per month.',
      adj=0)
# ------------- model fitting ------------- #
# model fitting
library(mgcv) # required library for gam
# encode casualty couns and week number
df$Casualty <- df$Injured + df$Dead
df$Week.Ind <- floor(
  julian(df$Date, as.Date('2013-01-01'))/7
)
# aggregate by week number
df.week <- aggregate(
  df$Casualty, list(Week=df$Week.Ind), sum)
names(df.week) <- c('Week', 'Count')
df.week$Date <- as.Date(as.POSIXct(
  df.week$Week*7*24*60*60, origin = "2013-01-01", tz="UTC"))
df.week$Month <- factor(
  months(df.week$Date), 
  c('January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December'))
# fit a gam model
fitted <- gam(Count ~ s(Week) + Month, data=df.week)
# plot estimates
week.seq <- data.frame(
  Date=seq(min(df$Date), as.Date('2017-01-01'), by='week'))
week.seq$Week <- floor(julian(week.seq$Date, as.Date('2013-01-01'))/7)
week.seq$Month <- factor(
  months(week.seq$Date), 
  c('January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December'))
week.pred <- predict.gam(fitted, week.seq, se.fit=TRUE)
week.seq$fit <- week.pred$fit
week.seq$lwr <- week.pred$fit - qnorm(0.975)*week.pred$se.fit
week.seq$upr <- week.pred$fit + qnorm(0.975)*week.pred$se.fit
# plot time trend
plot(week.seq$Week, week.seq$fit, col=1, type='l',
     xlab='', ylab='Weekly shooting casualty counts', 
     axes=FALSE, ylim=c(0,100))
# shade 95% CI
polygon(c(rev(week.seq$Week), week.seq$Week),
        c(rev(week.seq$lwr), week.seq$upr),
        border=NA, col=alpha(8, 0.2))
points(df.week$Week, df.week$Count, pch=16, col=alpha(7, 0.5))
axis(1, julian(
  as.Date(c(
    '2013-01-01', '2014-01-01', '2015-01-01', '2016-01-01',
    '2017-01-01')), 
  as.Date('2013-01-01'))/7, 
  labels=c('2013', '2014', '2015', '2016', '2017'))
axis(2, c(0, 50, 100), las=2)
mtext('Estimated weekly casualty counts from shooting incidents in the U.S.', 
      adj=0)
# plot the estimated monthly seasonality
month.seq <- data.frame(
  Date = seq(as.Date('2013-01-01'), by='months', length.out=12))
month.seq$Week <- floor(julian(
  month.seq$Date, as.Date('2013-01-01'))/7)
month.seq$Month <- factor(
  months(month.seq$Date), 
  c('January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December'))
month.pred <- predict.gam(fitted, month.seq, se.fit=TRUE, type='terms', terms='Month')
month.seq$fit <- month.pred$fit
month.seq$lwr <- month.pred$fit - qnorm(0.975)*month.pred$se.fit
month.seq$upr <- month.pred$fit + qnorm(0.975)*month.pred$se.fit
# plot seasonality
plot(month.seq$Week, month.seq$fit, col=1, pch=16,
     xlab='', ylab='Change in weekly casualty counts',
     ylim=c(-10, 50), axes=FALSE)
lines(month.seq$Week, month.seq$fit, col=1)
# shade 95% CI
polygon(c(rev(month.seq$Week), month.seq$Week),
        c(rev(month.seq$lwr), month.seq$upr),
        border=NA, col=alpha(8, 0.2))
axis(1, month.seq$Week, labels=substr(month.seq$Month,0,3), cex.axis=0.7)
axis(2, c(-10, 0, 25, 50))
mtext('Estimated monthly change in weekly casualty counts\nfrom shooting incidents in the U.S.', adj=0)
# plot the estimated trend over time
week.seq <- data.frame(
  Date=seq(min(df$Date), max(df$Date), by='week'))
week.seq$Week <- julian(week.seq$Date, as.Date('2013-01-01'))/7
week.seq$Month <- factor(
  months(week.seq$Date), 
  c('January', 'February', 'March', 'April', 'May', 'June',
    'July', 'August', 'September', 'October', 'November', 'December'))
week.pred <- predict.gam(fitted, week.seq, se.fit=TRUE,
                         type='terms', terms='s(Week)')
week.seq$fit <- week.pred$fit + min(week.pred$fit)
week.seq$lwr <- week.pred$fit + min(week.pred$fit) - qnorm(0.975)*week.pred$se.fit
week.seq$upr <- week.pred$fit + min(week.pred$fit) + qnorm(0.975)*week.pred$se.fit
# plot time trend
plot(week.seq$Week, week.seq$fit, col=1, type='l',
     xlab='', ylab='Change in shooting incident counts',
     axes=FALSE, ylim=c(-7, 7))
# shade 95% CI
polygon(c(rev(week.seq$Week), week.seq$Week),
        c(rev(week.seq$lwr), week.seq$upr),
        border=NA, col=alpha(8, 0.2))
axis(1, floor(julian(
  as.Date(c('2013-01-01', '2014-01-01', '2015-01-01', '2016-01-01')), 
  as.Date('2013-01-01'))/7), 
  labels=c('2013', '2014', '2015', '2016'))
axis(2, c(-7, 0, 7), las=2)
mtext('Estimated trend in weekly casualty counts\nfrom shooting incidents in the U.S.', 
      adj=0)
