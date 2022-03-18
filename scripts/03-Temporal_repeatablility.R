## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Individual consistency in migration strategies of a tropical seabird, the Round Island petrel. Movement Ecology
## Kirsty A. Franklin
## 03: Temporal repeatability of petrel migration
## 01-03-2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load packages ------------------------------------------------------------------------------------------
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('lubridate')) install.packages('lubridate'); library(lubridate)
if (!require('reshape2')) install.packages('reshape2'); library(reshape2)
if (!require('plyr')) install.packages('plyr'); library(plyr)
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('rptR')) install.packages('rptR'); library(rptR)
if (!require('tidyr')) install.packages('tidyr'); library(tidyr)

# Read in file containing petrel migration dates in order to calculate migration duration
dates <- read.csv("data/All_repeat_mig_dates_binomial_HMM_long_format.csv")
# Read in file containing petrel migration start and end dates of migration (in RI Julian day i.e. Day 1 = 1 June)
# Note, Round Island julian days have already been calculated (and are study-specific) so we just read in the df here for simplicity
start_dates <- read.csv("data/start_dups.csv")
end_dates <- read.csv("data/end_dups.csv")

# Repeatability for START dates ---------------------------------------------------------------

hist(start_dates$julian.day)
rep1 <- rpt(julian.day ~ (1 | Petrel), grname = "Petrel", data = start_dates, datatype = "Gaussian", 
            nboot = 1000, npermut = 0)
print(rep1)
summary(rep1$mod) # get variance components for within- and between- individuals

# Repeatability for END dates ---------------------------------------------------------------

hist(end_dates$julian.day) #gaussian distribution
rep2 <- rpt(julian.day ~ (1 | Petrel), grname = "Petrel", data = end_dates, datatype = "Gaussian", 
            nboot = 1000, npermut = 0)
print(rep2)
summary(rep2$mod) # get variance components for within- and between- individuals


# Repeatability in DURATION of petrel migrations -----------------------------------------------------------
dates$start <- as.Date(dates$start); dates$end <- as.Date(dates$end) # convert all dates to date format
dates$migration <- as.factor(dates$migration)
str(dates)

# Calculate duration of petrel migrations
dates$duration<- as.numeric(difftime(dates$end ,dates$start , units = c("days")))

# Remove '_2' from the birds with multiple deplyoments so ID is the same (5H00584 and 5H33506)
dates$petrel <- revalue(dates$petrel, c("5H00584_2"="5H00584"))
dates$petrel <- revalue(dates$petrel, c("5H33506_2"="5H33506"))
dates$petrel <- revalue(dates$petrel, c("5H33239_2"="5H33239"))
dates$petrel <- revalue(dates$petrel, c("5H30273_2"="5H30273"))
dates$petrel <- revalue(dates$petrel, c("5H14280_2"="5H14280"))

# Remove any NAs
dates <- drop_na(dates)

hist(dates$duration) 
rep3 <- rpt(duration ~ (1 | petrel), grname = "petrel", data = duration_dups, datatype = "Gaussian", 
            nboot = 1000, npermut = 0)
print(rep3)
summary(rep3$mod) # get variance components for within- and between- individuals


# Figure 1 -----------------------------------------------------------------
# Dumbbell plot with multiple points
# Start dates first

# Calculate mean of start dates for each petrel and use this to order dumbbell plot
mean_julian_start <- aggregate(x = start_dates$julian.day,
                               by = list(start_dates$Petrel),
                               FUN = mean, na.rm=T)  

names(mean_julian_start)[1] <- "Petrel"
names(mean_julian_start)[2] <- "mean_start"
mean_julian_start$mean_start <- floor(mean_julian_start$mean_start) # round to whole numbers

mean_julian_start <- mean_julian_start[order(mean_julian_start$mean_start),] # order df based on mean_start value

mean_julian_start$order <- seq(1, length(mean_julian_start$mean_start),1) # add order column

names(start_dates)
s2 <- start_dates[, c(1:3)] #extract relevant columns
s3 <- plyr::join(s2, mean_julian_start) #combine df with mean start values and order with julian day info
s3$year <- as.factor(s3$year)

##### maximum value of the column by group 
max <- aggregate(x= start_dates$julian.day,
                 by= list(start_dates$Petrel),
                 FUN=max, na.rm=T)
min <- aggregate(x= start_dates$julian.day,
                 by= list(start_dates$Petrel),
                 FUN=min, na.rm=T)

range <- merge(min, max, by="Group.1")
names(range)[1] <- "Petrel"
names(range)[2] <- "min"
names(range)[3] <- "max"

range_start <- plyr::join(range, mean_julian_start)

ggplot() +   
  geom_point(data=s3, aes(x=order, y=julian.day, colour = year), size = 2) +
  geom_segment(data=range_start, aes(x = order, xend = order, y = min, yend = max), colour = "darkgrey")

# End dates
# calculate mean of start dates for each petrel and use this to order dumbbell plot
mean_julian_end <- aggregate(x = end_dates$julian.day,
                             by = list(end_dates$Petrel),
                             FUN = mean, na.rm=T)  

names(mean_julian_end)[1] <- "Petrel"
names(mean_julian_end)[2] <- "mean_end"
mean_julian_end$mean_end <- floor(mean_julian_end$mean_end)

mean_julian_end <- mean_julian_end[order(mean_julian_end$mean_end),]

mean_julian_end$order <- seq(1, length(mean_julian_end$mean_end),1)

names(end_dates)
e2 <- end_dates[, c(1:3)] #extract relevant columns
e3 <- plyr::join(e2, mean_julian_end) #combine df with mean start values and order with julian day info
e3$year <- as.factor(e3$year)

##### maximum value of the column by group 
max_end <- aggregate(x= end_dates$julian.day,
                     by= list(end_dates$Petrel),
                     FUN=max, na.rm=T)
min_end <- aggregate(x= end_dates$julian.day,
                     by= list(end_dates$Petrel),
                     FUN=min, na.rm=T)

range_end <- merge(min_end, max_end, by="Group.1")
names(range_end)[1] <- "Petrel"
names(range_end)[2] <- "min"
names(range_end)[3] <- "max"

range_end2 <- plyr::join(range_end, mean_julian_end)

ggplot() +   
  geom_point(data=e3, aes(x=order, y=julian.day, colour = year), size = 2) +
  geom_segment(data=range_end2, aes(x = order, xend = order, y = min, yend = max), colour = "darkgrey")

# Join dataframes together for plotting
## now both plots are working for start and end dates separately, will combine df's to make final plot and facet

e3$time <- "end"
s3$time <- "start"
names(e3)[4] <- "mean"
names(s3)[4] <- "mean"
d1 <- rbind(s3, e3)
d1$time2 = factor(d1$time, levels=c('start','end'))
summary(d1$year)
d1$year <- factor(d1$year, levels=c("2009", "2010", "2011", "2012", "2014", "2015", "2016", "2017"))

range_start$time <- "start"
range_end2$time <- "end"
names(range_start)[4] <- "mean"
names(range_end2)[4] <- "mean"
ranges <- rbind(range_start, range_end2)
ranges$time2 = factor(ranges$time, levels=c('start','end'))

# Read in file which says which petrel year each of the arrival/departure dates is in so we can colour by this
pmigyear <- read.csv("data/year_in_relation_to_julian_day_long.csv")
str(pmigyear)
pmigyear$year <- as.factor(pmigyear$year)

d4 <- join(d1, pmigyear)

summary(d4$year)

# The colour blind friendly palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Text to be added to plot
dat_text <- data.frame(
  label = c("Departure from colony", "Arrival at colony"),
  time2   = c("start", "end"),
  x     = c(0, 0),
  y     = c(310, 310))


ggplot() +   
  geom_point(data=d1, aes(x=order, y=julian.day, colour = year), size = 2) +
  geom_segment(data=ranges, aes(x = order, xend = order, y = min, yend = max), colour = "darkgrey") +
  theme_bw() +
  xlab("Individual") +
  scale_x_continuous(expand=c(0.01,0)) +
  scale_y_continuous(#name="Date", 
    breaks=c(-60, 1, 62, 123, 184, 246, 305, 366), 
    labels=c("1 April", "1 June", "1 Aug", "1 Oct", "1 Dec", "1 Feb", "1 April", "1 June")) +
  scale_colour_manual(values=cbp1) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank(), 
        strip.background = element_rect(fill = NA), 
        strip.text.y = element_blank(), axis.title.y = element_blank(),
        panel.grid.major.y = element_line(colour = "light grey", linetype = "dashed", size= 0.1)) +
  facet_grid(time2 ~ .) + #, labeller = labeller(time2 = labs)) + 
  geom_rect(data = data.frame(time2 = "start"), aes(xmin = -Inf, xmax = Inf, ymin = 123, ymax = 304), alpha = 0.2, fill="grey", inherit.aes = FALSE) +
  geom_rect(data = data.frame(time2 = "end"), aes(xmin = -Inf, xmax = Inf, ymin = 123, ymax = 304), alpha = 0.2, fill="grey", inherit.aes = FALSE) +
  guides(colour=guide_legend(title="Petrel year")) +
  geom_text(data=dat_text,   mapping = aes(x = x, y = y, label = label), hjust= -0.1, vjust= -1, colour= "#636363")


jpeg(filename="figs/04-Barbell_plot_76_inds.jpg", width = 600, height = 400)
