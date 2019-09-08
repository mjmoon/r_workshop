library(dplyr)
# Data retreived from BuzzFeed
# https://github.com/BuzzFeedNews/2015-12-mass-shooting-intervals

# read and match names
data.dir <- paste0(getwd(), '/Projects/r_workshop/data')
df.2013 <- read.csv(paste0(data.dir, '/orig/2013MASTER.csv')) %>% 
  select(date, killed, wounded, location) %>%
  rename(Date=date, Dead=killed, Injured=wounded, Location=location)
df.2014 <- read.csv(paste0(data.dir, '/orig/2014MASTER.csv')) %>% 
  select(Date, Dead, Injured, Location)
df.2015 <- read.csv(paste0(data.dir, '/orig/2015CURRENT.csv')) %>% 
  select(Date, Dead, Injured, Location)
# join
df.all <- rbind(
  df.2013, df.2014, df.2015
)
# write 
write.csv(df.all, paste0(data.dir, '/shooting-usa.csv'), row.names=FALSE)
