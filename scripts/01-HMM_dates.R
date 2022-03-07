## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Individual consistency in migration strategies of a tropical seabird, the Round Island petrel. Movement Ecology
## Kirsty A. Franklin
## 01: Identifying start and end dates of petrel migration using HMM
## 01-03-2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load libraries --------------------
library(dplyr)     
library(tidyr)
library(lubridate) 
library(depmixS4)  
library(sp)
library(geosphere)
library(BAStag)

# Run three custom functions --------------------

# Run function for reading in Intigeo activity data 
readMTdeg2 <- function(file,skip=20) {
  d <- read.table(file,header=FALSE,skip=skip,  sep="\t",col.names=c("DateTime","Wet"), colClasses=c("character","numeric"))
  d
}

# HMM function for BAS TAGS
calculateDates <- function(tag, deployment.date, recovery.date, path){
  
  ID <- tag
  
  #########################################################################
  #read in and prepare the required data files (column names, format times)
  #########################################################################
  
  #activity data
  act = readAct(paste0(path, ID, "/", ID, ".act"))
  names(act)<-c("OK","DateTime","XLTime","Wet","Temp", "Day", "Time")

  #light data
  lig <- readLig(paste0(path, ID, "/", ID, ".lig"))
  names(lig)<-c("OK","DateTime","XLTime","Light")

  #sunrise and sunset data (from preprocess script of BAStag)
  twl<-read.csv(paste0(path, ID, "/", "twilight.csv"))
  twl$DateTime<-(as.POSIXct(twl$Twilight,format="%Y-%m-%d %H:%M", tz="GMT"))
  
  #read in Estelle path from SGAT
  locs<-read.csv(paste0(path, ID, "/", ID, "estelleSST.csv"))
  locs$Time<-(as.POSIXct(locs$Time,format="%Y-%m-%d %H:%M:%S", tz="GMT"))
  
  ########################################################################################################
  #subsample by the device deployment period; note twilights and path already subsetted during processing
  #so only need to do it for the raw light and activity dataframes
  ########################################################################################################
  Start.Date<-as.POSIXct(deployment.date)
  End.Date<-as.POSIXct(recovery.date)
  act<-act%>% filter(between(DateTime, Start.Date, End.Date))
  lig<-lig%>% filter(between(DateTime, Start.Date, End.Date))
  
  #################################################################################################
  #Sum wet times within calendar dates
  #################################################################################################
  d.wet<-act %>% group_by(Day=floor_date(DateTime, "day")) %>%
    summarize(Wet=sum(Wet))
  d.wet$P_wet<-d.wet$Wet/(144*200) #denominator - max wet 200 in each 10 min block and 144 10 min periods in a day
  
  ####################################################################################################
  #Need to do the same for light interference BUT only during daytime. Use twilights file to 
  #select out light values during daylight and then calculate light interference within those periods
  ####################################################################################################
  #create dataframe of time intervals
  n<-nrow(twl)
  head(twl)
  dates<-dplyr::select(twl,DateTime,Rise)   #extract GMT and dry columns from locs
  start<-dates[1:n-1,]                      #create columns for start intervals and drop last row
  end<-dates[2:n,]                          #create columns for end intervals and drop first row
  names(start)<-c("start_time","start_rise") #rename colums
  names(end)<-c("end_time","end_rise")       #rename colums
  intervals<-cbind(start,end)               #and put them both togther
  head(intervals)
  
  #label locs falling in daylight interval as TRUE and those in night intervals as FALSE
  
  day<-filter(intervals,start_rise==TRUE)  #select out day intervals
  
  day_intervals<-as.list(interval(day$start_time,day$end_time)) #make into list
  
  lig$Day<-lig$DateTime %within% day_intervals #label locations as being during day (TRUE) or night (FALSE)
  
  day_lig<-filter(lig,Day==TRUE)  #select out light values that were sampled during daylight hours
  
  #class interference as light samples less than 64
  day_lig$intf<-ifelse(day_lig$Light<64,1,0)
  day_lig$tally<-rep(1,nrow(day_lig))          #column of ones to sum for denominator
  
  #tally the number of 10 min periods during daylight and the number affected by interference
  d.int<-day_lig %>% group_by(Day=floor_date(DateTime, "day")) %>%
    summarize(intf=sum(intf),Tot=sum(tally))
  
  d.int$P_int<-d.int$intf/d.int$Tot
  
  d.int$P_int1 <- cbind(d.int$intf,(d.int$Tot - d.int$intf))
  
  head(d.int)
  
  #############################################################
  #calculate distance from each GLS location to the colony   ##
  #############################################################
  p1<-as.matrix(cbind(locs$Lon.mean,locs$Lat.mean))
  p2<-as.matrix(cbind(57.79,-19.84)) ##location of Round Island
  locs$dist<-distGeo(p1, p2,a=6378137, f=1/298.257223563)/1000#distance from colony in km
  
  #take the maximum distance from the colony for each calendar date
  d.dist<-locs %>% 
    group_by(Day=floor_date(Time, "day")) %>%
    summarize(dist=max(dist))
  
  #join wets and interference by day,
  HMM.dat<-left_join(d.wet,d.int, by = ('Day'))
  HMM.dat2<-left_join(HMM.dat,d.dist, by = ('Day'))
  HMM.dat2 <- na.omit(HMM.dat2)

  set.seed(1)
  M7 <- depmix(list(Wet ~ 1, dist ~ 1, P_int1 ~ 1), data = HMM.dat2, nstates = 2, family = list(gaussian(), gaussian(), binomial()))
  F7 <- fit(M7)
  
  # predict the states from the wet only fit by estimating the posterior. Identified three period when
  #bird was ashore at start, middle and end of deployment. Areas inbetween can be seen as bird being
  #on migration at sea. 
  est.states <- posterior(F7)
  
  #combine with observed data
  HMM.dat3<-cbind(HMM.dat2,est.states)
  HMM.dat3$state<-HMM.dat3$state-1   #make state 1 (at colony) or 0 (migrating) for bar chart
  
  plotpath <- file.path(paste0(path, ID),paste0("HMM_plot_", ID, ".jpg", sep = ""))
  
  jpeg(file=plotpath, width=1500, height=1000)
  
  par(mfrow=c(4,1))
  plot(HMM.dat3$Day,HMM.dat3$Wet,type="l",xlab="Time",ylab="Wet")
  plot(HMM.dat3$Day,HMM.dat3$intf,type="l",xlab="Time",ylab="Interf")
  plot(HMM.dat3$Day,HMM.dat3$dist,type="l",xlab="Time",ylab="Dist")
  barplot(HMM.dat3$state,col="black",xlab="Time",ylab="Migrating")
  
  dev.off()
  
  #pick out start and end days of migration events
  n<-nrow(HMM.dat3)
  transition<-rep(0,n) #create vector of zeros to populate with output
  
  #run loop to identify which state values differ from previous ones
  for(i in 2:nrow(HMM.dat3)){
    transition[i]<-ifelse(HMM.dat3$state[i]==HMM.dat3$state[i-1],0,1)
  }
  
  HMM.dat3$transition<-transition
  
  #pick out the cases where state changes
  M.d<-subset(HMM.dat3,HMM.dat3$transition==1)
  
  #label start and ends
  M.d$Start.End<-ifelse(M.d$state==1,"Start","End")
  M.d2<-data.frame(Start.End=M.d$Start.End,Date=M.d$Day)
  
  return(M.d2)
  
}


# HMM in a function for MIGRATE TECH
calculateDatesMT <- function(tag, deployment.date, recovery.date, path){
  
  ID <- tag
  
  #########################################################################
  #read in and prepare the required data files (column names, format times)
  #########################################################################
  
  #activity data
  act = readMTdeg2(paste0(path, ID, "/", ID, ".deg"))
  act$DateTime<-(as.POSIXct(act$DateTime,format="%d/%m/%Y %H:%M", tz="GMT"))
  
  #light data
  lig <- readMTlux(paste0(path, ID, "/", ID, ".lux"))
  names(lig)<-c("DateTime","Light")
  lig$Light2 <- with(lig, ifelse(Light > 100, 100 , Light+0))
  
  #sunrise and sunset data (from preprocess script of BAStag)
  twl<-read.csv(paste0(path, ID, "/", "twilight.csv"))
  twl$DateTime<-(as.POSIXct(twl$Twilight,format="%Y-%m-%d %H:%M", tz="GMT"))
  
  #read in Estelle path from SGAT
  locs<-read.csv(paste0(path, ID, "/", ID, "estelleSST.csv"))
  locs$Time<-(as.POSIXct(locs$Time,format="%Y-%m-%d %H:%M:%S", tz="GMT"))
  
  ########################################################################################################
  #subsample by the device deployment period; note twilights and path already subsetted during processing
  #so only need to do it for the raw light and activity dataframes
  ########################################################################################################
  Start.Date<-as.POSIXct(deployment.date, tz="GMT")
  End.Date<-as.POSIXct(recovery.date, tz="GMT")
  act<-act%>% filter(between(DateTime, Start.Date, End.Date))
  lig<-lig%>% filter(between(DateTime, Start.Date, End.Date))

  #################################################################################################
  #Sum wet times within calendar dates
  #################################################################################################
  d.wet<-act %>% group_by(Day=floor_date(DateTime, "day")) %>%
    summarize(Wet=sum(Wet))
  d.wet$P_wet<-d.wet$Wet/(144*20) #denominator - max wet 200 in each 10 min block and 144 10 min periods in a day

  ####################################################################################################
  #Need to do the same for light interference BUT only during daytime. Use twilights file to 
  #select out light values during daylight and then calculate light interference within those periods
  ####################################################################################################
  #create dataframe of time intervals
  n<-nrow(twl)
  head(twl)
  dates<-dplyr::select(twl,DateTime,Rise)   #extract GMT and dry columns from locs
  start<-dates[1:n-1,]                      #create columns for start intervals and drop last row
  end<-dates[2:n,]                          #create columns for end intervals and drop first row
  names(start)<-c("start_time","start_rise") #rename colums
  names(end)<-c("end_time","end_rise")       #rename colums
  intervals<-cbind(start,end)               #and put them both togther
  head(intervals)
  
  #label locs falling in daylight interval as TRUE and those in night intervals as FALSE
  
  day<-filter(intervals,start_rise==TRUE)  #select out day intervals
  
  day_intervals<-as.list(interval(day$start_time,day$end_time)) #make into list
  
  lig$Day<-lig$DateTime %within% day_intervals #label locations as being during day (TRUE) or night (FALSE)
  
  day_lig<-filter(lig,Day==TRUE)  #select out light values that were sampled during daylight hours
  
  #class interference as light samples less than 64
  day_lig$intf<-ifelse(day_lig$Light<100,1,0)
  day_lig$tally<-rep(1,nrow(day_lig))          #column of ones to sum for denominator
  
  #tally the number of 10 min periods during daylight and the number affected by interference
  d.int<-day_lig %>% group_by(Day=floor_date(DateTime, "day")) %>%
    summarize(intf=sum(intf),Tot=sum(tally))
  
  d.int$P_int<-d.int$intf/d.int$Tot
  
  d.int$P_int1 <- cbind(d.int$intf,(d.int$Tot - d.int$intf))
  
  head(d.int)
  
  #############################################################
  #calculate distance from each GLS location to the colony   ##
  #############################################################
  p1<-as.matrix(cbind(locs$Lon.mean,locs$Lat.mean))
  p2<-as.matrix(cbind(57.79,-19.84)) ##location of Round Island
  locs$dist<-distGeo(p1, p2,a=6378137, f=1/298.257223563)/1000 #distance from colony in km
  
  #take the maximum distance from the colony for each calendar date
  d.dist<-locs %>% 
    group_by(Day=floor_date(Time, "day")) %>%
    summarize(dist=max(dist))
  
  #join wets and interference by day,
  HMM.dat<-left_join(d.wet,d.int, by = ('Day'))
  HMM.dat2<-left_join(HMM.dat,d.dist, by = ('Day'))
  HMM.dat2 <- na.omit(HMM.dat2)
  
  set.seed(1)
  M7 <- depmix(list(Wet ~ 1, dist ~ 1, P_int1 ~ 1), data = HMM.dat2, nstates = 2, family = list(gaussian(), gaussian(), binomial()))
  F7 <- fit(M7)
 
  # predict the states from the wet only fit by estimating the posterior. Identified three period when
  #bird was ashore at start, middle and end of deployment. Areas inbetween can be seen as bird being
  #on migration at sea. 
  est.states <- posterior(F7)
  
  #combine with observed data
  HMM.dat3<-cbind(HMM.dat2,est.states)
  HMM.dat3$state<-HMM.dat3$state-1   #make state 1 (at colony) or 0 (migrating) for bar chart
  
  plotpath <- file.path(paste0(path, ID),paste0("HMM_plot_", ID, ".jpg", sep = ""))
  
  jpeg(file=plotpath, width=1500, height=1000)
  
  par(mfrow=c(4,1))
  plot(HMM.dat3$Day,HMM.dat3$Wet,type="l",xlab="Time",ylab="Wet")
  plot(HMM.dat3$Day,HMM.dat3$P_int,type="l",xlab="Time",ylab="Interf")
  plot(HMM.dat3$Day,HMM.dat3$dist,type="l",xlab="Time",ylab="Dist")
  barplot(HMM.dat3$state,col="black",xlab="Time",ylab="Migrating")
  
  dev.off()
  
  #pick out start and end days of migration events
  n<-nrow(HMM.dat3)
  transition<-rep(0,n) #create vector of zeros to populate with output
  
  #run loop to identify which state values differ from previous ones
  for(i in 2:nrow(HMM.dat3)){
    transition[i]<-ifelse(HMM.dat3$state[i]==HMM.dat3$state[i-1],0,1)
  }
  
  HMM.dat3$transition<-transition
  
  #pick out the cases where state changes
  M.d<-subset(HMM.dat3,HMM.dat3$transition==1)
  
  #label start and ends
  M.d$Start.End<-ifelse(M.d$state==1,"Start","End")
  M.d2<-data.frame(Start.End=M.d$Start.End,Date=M.d$Day)
  
  return(M.d2)
  
}


# Loop for BAS tag 2009 deployments (as an example) --------------------

petrels2009 <- read.csv("data/2009_deploy_and_recovery_dates.csv")
petrels2009$Recovery.datetime <- as.POSIXct(paste(petrels2009$R.date, petrels2009$R.time), format="%Y-%m-%d %H:%M:%S", tz="GMT")
petrels2009$Deployment.datetime <- as.POSIXct(paste(petrels2009$D.date, petrels2009$D.time), format="%Y-%m-%d %H:%M:%S", tz="GMT")

df2009 <- data.frame()  # create empty dataframe

uniquepetrels <- levels(unique(petrels2009$ID))

for(i in 1:length(uniquepetrels)){
  Individual <- uniquepetrels[i]
  Deploy <- (petrels2009$Deployment.datetime[i])
  Recovery <-   (petrels2009$Recovery.datetime[i])
  Path <- "C:/RI Petrels/GLS analysis/2009 deployment/Single migration birds/" # path of where gls and data files are
  
  HMM.dates <- calculateDates(Individual, Deploy, Recovery, Path)
  HMM.dates$ID <- rep(paste0(Individual),nrow(HMM.dates)) # add new column containing ID
  
  df2009 <- rbind(df2009, HMM.dates) 
  HMM.dates <- data.frame() #clear dataframe after every run
}

