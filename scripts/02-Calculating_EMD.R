## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Individual consistency in migration strategies of a tropical seabird, the Round Island petrel. Movement Ecology
## Kirsty A. Franklin
## 02: Calculating and analysing Earth mover's distance (EMD) 'effort' values
## 01-03-2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Certain elements of this script were conducted on the University of East Anglia's High Performance Cluster: https://www.uea.ac.uk/groups-and-centres/research-and-specialist-computing/high-performance-computing
# As such, these elements may take considerable time to run

# Load libraries --------------------
if (!require('dplyr')) install.packages('dplyr'); library(dplyr)
if (!require('move')) install.packages('move'); library(move)
if (!require('tidyr')) install.packages('tidyr'); library(tidyr)
if (!require('ggplot2')) install.packages('ggplot2'); library(ggplot2)
if (!require('ggpubr')) install.packages('ggpubr'); library(ggpubr)
if (!require('lubridate')) install.packages('lubridate'); library(lubridate)
if (!require('rsq')) install.packages('rsq'); library(rsq)
if (!require('cowplot')) install.packages('cowplot'); library(cowplot)
if (!require('sf')) install.packages('sf'); library(sf)
if (!require('adehabitatHR')) install.packages('adehabitatHR'); library(adehabitatHR)
if (!require('tibble')) install.packages('tibble'); library(tibble)
if (!require('reshape2')) install.packages('reshape2'); library(reshape2)
if (!require('emmeans')) install.packages('emmeans'); library(emmeans)

# Read in data --------------------
# Load in dataframe of all petrel migrations (file for BirdLife Seabird Tracking Database)
df <- read.csv("data/267_RIP_migrations_for_SeabirdTrackingDatabase.csv")
# Subset data to only include birds with repeat tracks e.g. TrackId > 1 for each BirdId
num_migs <- df %>% group_by(BirdId) %>% dplyr::summarise(n = n_distinct(TrackId))
# Join two dataframes together
df <- merge(df, num_migs)
# Keep only those which have more than one migration
df <- subset(df, n > 1)
df <- droplevels(df)
# Keep only relevant columns
df <- df[,c("BirdId", "TrackId", "Latitude",  "Longitude", "DateGMT", "TimeGMT")]
# Check I have the right number of birds
nlevels(unique(df$BirdId)) # 62
nlevels(unique(df$TrackId)) # 131

# Save this dataframe
write.csv(df, "data/all_repeat_migs_for_EMD.csv", row.names=F)
# Read in dataframe
df <- read.csv("data/all_repeat_migs_for_EMD.csv")

# Calculating EMD for all whole petrel/year migration comparions --------------------
# Note: this next section is very computationally heave so code was run on UEA's HPC 

uniqueIDmig <- levels(df$TrackId)

results_emd_move <- data.frame(matrix(ncol=length(uniqueIDmig), nrow=length(uniqueIDmig))) # create empty matrix for data to go in

 for (k in 1:length(uniqueIDmig)){
   
   ind <- uniqueIDmig[k]
   petrel1<- subset(dataframe, TrackId == ind) # subset each Track
   petrel1sp <- SpatialPoints(petrel1[,c("Longitude", "Latitude")]) # convert to spatial data
   proj4string(petrel1sp) <- CRS("+proj=longlat +ellps=WGS84
                       +datum=WGS84 +no_defs") # set coordinate reference system
   
   for (j in 1:length(uniqueIDmig)){  

     if (j < k) next
     ind2 <- uniqueIDmig[j]
     petrel2<- subset(dataframe, TrackId == ind2)
     petrel2sp <- SpatialPoints(petrel2[,c("Longitude", "Latitude")])
     proj4string(petrel2sp) <- CRS("+proj=longlat +ellps=WGS84
                       +datum=WGS84 +no_defs")
     
     emd_val <- emd(petrel1sp, petrel2sp, gc=T) # calculate EMD using Haversine distance
     
     colnames(results_emd_move)[j] <- paste(ind2)
     
     results_emd_move[k,j] <- emd_val[1]
     
  }
  
  rownames(results_emd_move)[k] <- paste(ind)
  
 }

# Load in dataframe of results
emd_results <- read.csv("data/EMD_all_results_long.csv")
names(emd_results) <- c('FocalID','CompID','EMD') # rename columns
# Remove the 'X' at the beginning of all the TrackId's
emd_results$FocalID<-gsub("X","",as.character(emd_results$FocalID))
emd_results$CompID<-gsub("X","",as.character(emd_results$CompID))
# Subset data to remove same petrel/year comparisons (EMD = 0)
emd_results <- subset(emd_results, FocalID != CompID) 

# Add 'Within' variable --------------------------------------
# Separate TrackId to give just BirdId and Migration so we can identify if it's the same individual
emd_results <- emd_results %>% separate(FocalID, c("ID","Migration"), "_")
emd_results <- emd_results %>% separate(CompID, c("ID2","Migration2"), "_")
# Make new column which says if EMD is comparing the same (1) or different (0) individuals
emd_results <- transform(emd_results, Within= ifelse(ID==ID2, 1, 0))
emd_results$Within <- as.factor(emd_results$Within)
summary(emd_results) # 78 within comparisons is correct
# Go back to having FocalID and CompID
emd_results$FocalID <- paste0(emd_results$ID, "_", emd_results$Migration)
emd_results$CompID <- paste0(emd_results$ID2, "_", emd_results$Migration2)

# Add 'Tdiff' (difference in departure in days) variable --------------------------------------
# Using dataframe from above, select the first date for each TrackID which equals the start date of migration
# Make 'DateGMT' into Date format
df$DateGMT <- dmy(df$DateGMT)
# Extract migration start dates (first row)
mig_dates <- df %>% group_by(TrackId) %>% arrange(DateGMT) %>% slice(1L)
mig_dates <- mig_dates[,c("TrackId", "DateGMT")] # Select columns needed
mig_dates$julian <- yday(mig_dates$DateGMT) # Convert date to julian day

emd_results2 <- merge(emd_results, mig_dates[,c("TrackId", "julian")], by.x="FocalID", by.y="TrackId")
names(emd_results2)[names(emd_results2) == "julian"] <- "julian_focalID"
emd_results2 <- merge(emd_results2, mig_dates[,c("TrackId", "julian")], by.x="CompID", by.y="TrackId")
names(emd_results2)[names(emd_results2) == "julian"] <- "julian_CompID"

# Calculate difference between Julian start dates
emd_results2$diff.julian <- abs(emd_results2$julian_focalID - emd_results2$julian_CompID)

# subset those values where diff.julian is larger than 6 months (182 days)
emd_results2$diff.julian2 <- with(emd_results2, ifelse(diff.julian > 182, 365 - diff.julian, diff.julian+0))

names(emd_results2)
emd_results2 <- emd_results2[,c("FocalID", "CompID", "EMD", "Within", "diff.julian2")]

write.csv(emd_results2, "data/emd_whole_migs_df.csv", row.names=F)

# GLM - EMD whole migrations --------------------

model1 <- glm(EMD ~  Within + diff.julian2 + Within:diff.julian2,
              family = Gamma(link=identity), na.action=na.pass,
              data = emd_results2)

drop1(model1, test="F")

model1.2 <- glm(EMD ~  Within + diff.julian2,
                family = Gamma(link=identity), na.action=na.pass,
                data = emd_results2)

drop1(model1.2, test="F")

model1.3 <- glm(EMD ~  Within,
                family = Gamma (link=identity), na.action=na.pass,
                data = emd_results2)

model1.4 <- glm(EMD ~  diff.julian2,
                family = Gamma (link=identity), na.action=na.pass,
                data = emd_results2)

anova(model1, model1.2, test="F") #test interaction
anova(model1.3, model1.2, test="F") #test start date 
anova(model1.4, model1.2, test="F") #test same individual

summary(model1.2)
rsq(model1.2, adj=T)

# Figure 4 --------------------

# Make plot using predict/predframe
# Line for between-individual comparisons
pdat <- expand.grid(Within = "0", diff.julian2=seq(0,182, 0.1))
pred <- predict(model1.2, pdat, se.fit = TRUE, type = "response", print.matrix = TRUE)
predframe <- data.frame(pdat, preds = pred)

# Line for within-individual comparisons
pdat2 <- expand.grid(Within = "1", diff.julian2=seq(0,182, 0.1))
pred2 <- predict(model1.2, pdat2, se.fit = TRUE, type = "response", print.matrix = TRUE)
predframe2 <- data.frame (pdat2, preds = pred2)

# Plot with both lines (between and within individual)
(Fig4b <- ggplot(emd_results2 %>% arrange(Within), aes(x=diff.julian2, y=EMD)) +
    geom_point(mapping=aes(colour=Within), alpha=0.5, size=.1) + scale_y_continuous(breaks=c(0,1000,2000,3000,4000,5000)) +
    geom_line(data=predframe, mapping=aes(x=diff.julian2,y=preds.fit), size=.2, colour="#2d708e") +
    geom_line(data=predframe, mapping=aes(x=diff.julian2,y=(preds.fit + (preds.se.fit*1.96))), linetype="dashed", size=.2, colour="#2d708e") +
    geom_line(data=predframe, mapping=aes(x=diff.julian2,y=(preds.fit - (preds.se.fit*1.96))), linetype="dashed", size=.2, colour="#2d708e") +
    geom_line(data=predframe2, mapping=aes(x=diff.julian2,y=preds.fit), colour="DeepPink2", size=.2) +
    geom_line(data=predframe2, mapping=aes(x=diff.julian2,y=(preds.fit + (preds.se.fit*1.96))), linetype="dashed", colour="DeepPink2", size=.2) +
    geom_line(data=predframe2, mapping=aes(x=diff.julian2,y=(preds.fit - (preds.se.fit*1.96))), linetype="dashed", colour="DeepPink2", size=.2) +
    theme_pubr()+ scale_color_manual(values=c("grey90", "black")) +
    theme(legend.position = "none", axis.title = element_text(size=8), axis.text = element_text(size=7), axis.line = element_line(size=.2), axis.ticks = element_line(size=.2)) +
    labs(x= "Difference in departure (days)"))

# Making predictions for 'same individual'
pdat3 <- expand.grid(Within = "0", diff.julian2=79.0) # Using median diff.julian2
pred3 <- predict(model1.2, pdat3, se.fit = TRUE, type = "response", print.matrix = TRUE)
predframe3 <- data.frame (pdat3, preds = pred3)

pdat4 <- expand.grid(Within = "1", diff.julian2=79.0)
pred4 <- predict(model1.2, pdat4, se.fit = TRUE, type = "response", print.matrix = TRUE)
predframe4 <- data.frame (pdat4, preds = pred4)

(Fig4a <- ggplot(emd_results2, aes(x=Within, y=EMD)) + 
    geom_point(mapping=aes(colour=Within), alpha=0.5,position = position_jitter(), size=.1) +
    theme_pubr()+ scale_color_manual(values=c("grey90", "black")) + theme(legend.position = "none", axis.title = element_text(size=8), axis.text = element_text(size=7), axis.line = element_line(size=.2), axis.ticks = element_line(size=.2)) +
    scale_x_discrete(name = "Migration comparison", labels=c("Between-individual", "Within-individual")) + scale_y_continuous(breaks=c(0,1000,2000,3000,4000,5000)) +
    geom_errorbar(data=predframe3, mapping=aes(x = Within, y = preds.fit, ymin = (preds.fit - (preds.se.fit*1.96)), ymax = (preds.fit + (preds.se.fit*1.96))), colour="#2d708e", size=.4, width=.1) +
    geom_point(data=predframe3, mapping=aes(x=Within, y=preds.fit), size=.4, colour="#2d708e") +
    geom_point(data=predframe4, mapping=aes(x=Within, y=preds.fit), size=.4, colour="DeepPink2") +
    geom_errorbar(data=predframe4, mapping=aes(x = Within, y = preds.fit, ymin = (preds.fit - (preds.se.fit*1.96)), ymax = (preds.fit + (preds.se.fit*1.96))), colour="DeepPink2", size=.4, width=.1))

# Join two plots together
(Figure4 <- ggdraw() +
    draw_plot(Fig4a, x = 0, y = 0, width = 0.5, height = 1) +
    draw_plot(Fig4b, x = 0.5, y = 0, width = 0.5, height = 1) +
    draw_plot_label(label = c("A", "B"), size = 10, x = c(0.0, 0.49), y = c(0.98, 0.98)))

ggsave(filename="figs/Figure4.pdf", plot=last_plot(), width=170, height=80, units="mm", dpi=600)


# Calculating EMD for migrations split into six equal sized stages --------------------
# Using same dataframe as above (data/267_RIP_migrations_for_SeabirdTrackingDatabase.csv)
uniqueIDmig <- levels(df$TrackId)
six_split_df <- data.frame() # Make an empty dataframe for data to go in

for (h in 1:length(uniqueIDmig)){
  
  ind <- uniqueIDmig[h]
  petrel <- subset(df, TrackId == ind)
  split_df1 <- split(petrel, rep(1:6, length.out=nrow(petrel), each=ceiling(nrow(petrel)/6)))
  split_df2 <- bind_rows(split_df1, .id = "Split") #to convert list into dataframe
  six_split_df <- rbind(six_split_df, split_df2)
}  

lvls_six_split_df <- six_split_df %>% group_by(TrackId) %>% count(Split) # checking that the number of rows per split was what I expected but remember to divide by 2 as there are 2 locations per day!

# Make a variable that says which migration it is
six_split_df <- six_split_df %>% tidyr::separate(TrackId, c("ID","Migration"), "_", remove=FALSE)
# Make a variable that is ID & split combined
six_split_df$IDsplit  <- as.factor(paste0(six_split_df$ID, "_split_", six_split_df$Split))

# Calculating EMD
# Run on own laptop

uniqueIDsplit <- levels(six_split_df$IDsplit) # Levels of IDsplit

results_6split <- data.frame(matrix(ncol=4, nrow=length(uniqueIDsplit))) 
colnames(results_6split) <- c('ID','Mig_A','Mig_B', 'EMD')

counter <- 1 # Make sure counter is 1 before running

for (i in 1:length(uniqueIDsplit)){
  
  ind <- uniqueIDsplit[i]
  mig <- subset(six_split_df, IDsplit == ind)
  
  mig_num <- levels(as.factor(mig$Migration))
  
  if(length(mig_num) ==2){
    
    mig1 <- subset(six_split_df, IDsplit == ind & Migration == 1)
    mig1sp <- SpatialPoints(mig1[,c("Longitude", "Latitude")])
    proj4string(mig1sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    mig2 <- subset(six_split_df, IDsplit == ind & Migration == 2)
    mig2sp <- SpatialPoints(mig2[,c("Longitude", "Latitude")])
    proj4string(mig2sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    
    emd_val_split <- emd(mig1sp, mig2sp, gc=T)
    
    results_6split[counter,1] <- ind
    results_6split[counter,2] <- 'Mig 1'
    results_6split[counter,3] <- 'Mig 2'
    results_6split[counter,4] <- emd_val_split[1]
    counter <- counter + 1
  }
  
  if(length(mig_num) ==3){
    
    mig1 <- subset(six_split_df, IDsplit == ind & Migration == 1)
    mig1sp <- SpatialPoints(mig1[,c("Longitude", "Latitude")])
    proj4string(mig1sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    mig2 <- subset(six_split_df, IDsplit == ind & Migration == 2)
    mig2sp <- SpatialPoints(mig2[,c("Longitude", "Latitude")])
    proj4string(mig2sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    mig3 <- subset(six_split_df, IDsplit == ind & Migration == 3)
    mig3sp <- SpatialPoints(mig3[,c("Longitude", "Latitude")])
    proj4string(mig3sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    
    emd_val_split <- emd(mig1sp, mig2sp, gc=T)
    
    results_6split[counter,1] <- ind
    results_6split[counter,2] <- 'Mig 1'
    results_6split[counter,3] <- 'Mig 2'
    results_6split[counter,4] <- emd_val_split[1]
    
    counter <- counter+1
    
    emd_val_split <- emd(mig1sp, mig3sp, gc=T)
    
    results_6split[counter,1] <- ind
    results_6split[counter,2] <- 'Mig 1'
    results_6split[counter,3] <- 'Mig 3'
    results_6split[counter,4] <- emd_val_split[1]
    
    counter <- counter+1
    
    emd_val_split <- emd(mig2sp, mig3sp, gc=T)
    
    results_6split[counter,1] <- ind
    results_6split[counter,2] <- 'Mig 2'
    results_6split[counter,3] <- 'Mig 3'
    results_6split[counter,4] <- emd_val_split[1]
    
    counter <- counter+1
    
  }
  
  if(length(mig_num) ==4){
    
    mig1 <- subset(six_split_df, IDsplit == ind & Migration == 1)
    mig1sp <- SpatialPoints(mig1[,c("Longitude", "Latitude")])
    proj4string(mig1sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    mig2 <- subset(six_split_df, IDsplit == ind & Migration == 2)
    mig2sp <- SpatialPoints(mig2[,c("Longitude", "Latitude")])
    proj4string(mig2sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    mig3 <- subset(six_split_df, IDsplit == ind & Migration == 3)
    mig3sp <- SpatialPoints(mig3[,c("Longitude", "Latitude")])
    proj4string(mig3sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    mig4 <- subset(six_split_df, IDsplit == ind & Migration == 4)
    mig4sp <- SpatialPoints(mig4[,c("Longitude", "Latitude")])
    proj4string(mig4sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    
    
    emd_val_split <- emd(mig1sp, mig2sp, gc=T)
    
    results_6split[counter,1] <- ind
    results_6split[counter,2] <- 'Mig 1'
    results_6split[counter,3] <- 'Mig 2'
    results_6split[counter,4] <- emd_val_split[1]
    
    counter <- counter+1
    
    emd_val_split <- emd(mig1sp, mig3sp, gc=T)
    
    results_6split[counter,1] <- ind
    results_6split[counter,2] <- 'Mig 1'
    results_6split[counter,3] <- 'Mig 3'
    results_6split[counter,4] <- emd_val_split[1]
    
    counter <- counter+1
    
    emd_val_split <- emd(mig1sp, mig4sp, gc=T)
    
    results_6split[counter,1] <- ind
    results_6split[counter,2] <- 'Mig 1'
    results_6split[counter,3] <- 'Mig 4'
    results_6split[counter,4] <- emd_val_split[1]
    
    counter <- counter+1
    
    emd_val_split <- emd(mig2sp, mig3sp, gc=T)
    
    results_6split[counter,1] <- ind
    results_6split[counter,2] <- 'Mig 2'
    results_6split[counter,3] <- 'Mig 3'
    results_6split[counter,4] <- emd_val_split[1]
    
    counter <- counter+1
    
    emd_val_split <- emd(mig2sp, mig4sp, gc=T)
    
    results_6split[counter,1] <- ind
    results_6split[counter,2] <- 'Mig 2'
    results_6split[counter,3] <- 'Mig 4'
    results_6split[counter,4] <- emd_val_split[1]
    
    counter <- counter+1
    
    emd_val_split <- emd(mig3sp, mig4sp, gc=T)
    
    results_6split[counter,1] <- ind
    results_6split[counter,2] <- 'Mig 3'
    results_6split[counter,3] <- 'Mig 4'
    results_6split[counter,4] <- emd_val_split[1]
    
    counter <- counter+1
    
  }
  
  print(paste0(round((i/372)*100,1),'%'))
  
}


write.csv(results_6split, "data/emd_6_stages.csv", row.names=F)


# Analyse six stage EMD data --------------------------
results_6split <- read.csv("data/emd_6_stages.csv")

head(results_6split)

results_6split2 <- results_6split %>% separate(ID, c("ID", "split", "no_split"), "_")
results_6split2 <- results_6split2 %>% separate(Mig_A, c("MigA", "Mig_numA"), " ")
results_6split2 <- results_6split2 %>% separate(Mig_B, c("MigB", "Mig_numB"), " ")
results_6split2$FocalID <- paste0(results_6split2$ID, "_", results_6split2$Mig_numA)
results_6split2$CompID <- paste0(results_6split2$ID, "_", results_6split2$Mig_numB)
results_6split2 <- results_6split2[,c(9,10,8,3)]

# Want EMD values and diff.start2 for same individuals from first EMD analysis which compares the whole of mig1 v mig2
emd_results2_within <- subset(emd_results2, Within == 1)

# add difference in departure dates
results_6split3 <- plyr::join(results_6split2, emd_results2_within[,c("FocalID", "CompID", "diff.julian2")], type= "full")
results_6split3$EMD <- as.numeric(results_6split3$EMD)

emd_results2_within2 <- emd_results2_within[, c(1:3,5)] # remove 'within' column
emd_results2_within2$no_split <- 0 #and replace with no_split column

results_6split4 <- rbind(results_6split3, emd_results2_within2) # join two df's together so have whole mig and stage EMDs in one
results_6split4$no_split <- as.factor(results_6split4$no_split)

str(results_6split4)
summary(results_6split4$no_split)

# GLM
model3 <- glm(EMD ~ no_split + diff.julian2 + diff.julian2:no_split, data=results_6split4, family=Gamma(identity), na.action=na.pass)
model3.1 <- glm(EMD ~ no_split + diff.julian2, data=results_6split4, family=Gamma(identity), na.action=na.omit)
model3.2 <- glm(EMD ~ diff.julian2, data=results_6split4, family=Gamma(identity), na.action=na.omit)
model3.3 <- glm(EMD ~ no_split, data=results_6split4, family=Gamma(identity), na.action=na.omit)

anova(model3.1, model3, test="F")
#EMD ~ start date + same individual  vs. EMD ~ start date
#EMD ~ start date + same individual  vs. EMD ~ same individual  

anova(model3, model3.1, test="F") #test interaction
anova(model3.2, model3.1, test="F") #test split
anova(model3.1, model3.3, test="F") #test start date

# Summary of model for Table 2
summary(model3.1)

# get pairwise comparisons for Table S4
emm <- emmeans (model3.1, pairwise ~ no_split) # pairwise comparisons not created
emm

# Calculating EMD for migrations split into months (30 days) --------------------
# Using same dataframe as above (data/267_RIP_migrations_for_SeabirdTrackingDatabase.csv)
# uniqueIDmig <- levels(df$TrackId)

month_splitdf <- data.frame()

for (h in 1:length(uniqueIDmig)){
  
  ind <- uniqueIDmig[h]
  petrel <- subset(df, TrackId == ind)
  month_df1 <- split(petrel, (seq(nrow(petrel))-1) %/% 60) #split dataframe into multiple parts containing 60 rows
  month_df2 <- bind_rows(month_df1, .id = "Month") #to convert list into dataframe
  month_splitdf <- rbind(month_splitdf, month_df2)
}  

month_splitdf$Month <- as.numeric(month_splitdf$Month)+1 # add 1 as month splits start at 0
# Make a variable that says which migration it is
month_splitdf <- month_splitdf %>% tidyr::separate(TrackId, c("ID","Migration"), "_", remove=FALSE)
# Make a variable that is ID & split combined
month_splitdf$IDmonth  <- as.factor(paste0(month_splitdf$ID, "_month_", month_splitdf$Month))

# Calculating EMD 
# Run on own laptop

# Filter out so that each bird has same number of IDmonth's
month_splitdf %>% group_by(BirdId, IDmonth)

temp_dat <- month_splitdf %>% group_by(BirdId, Migration) %>% summarise(months = n_distinct(IDmonth)) %>% group_by(BirdId) %>% summarise(min_month = min(months))
dat <- data.frame()
for(k in unique(month_splitdf$BirdId)) {
  temp <- month_splitdf %>% filter(BirdId == k)
  temp <- temp %>% filter(Month <= temp_dat[temp_dat$BirdId == k,]$min_month)
  
  dat <- rbind(dat,temp)
}

uniqueIDmonth <- levels(dat$IDmonth) # Levels of IDsplit

results_month <- data.frame(matrix(ncol=4, nrow=length(uniqueIDmonth))) 
colnames(results_month) <- c('ID','Mig_A','Mig_B', 'EMD')

counter <- 1 # Make sure counter is 1 before running

for (i in 1:length(uniqueIDmonth)){
  
  ind <- uniqueIDmonth[i]
  mig <- subset(dat, IDmonth == ind)
  
  mig_num <- levels(as.factor(mig$Migration))
  
  if(length(mig_num) ==2){
    
    mig1 <- subset(dat, IDmonth == ind & Migration == 1)
    mig1sp <- SpatialPoints(mig1[,c("Longitude", "Latitude")])
    proj4string(mig1sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    mig2 <- subset(dat, IDmonth == ind & Migration == 2)
    mig2sp <- SpatialPoints(mig2[,c("Longitude", "Latitude")])
    proj4string(mig2sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    
    emd_val_month <- emd(mig1sp, mig2sp, gc=T)
    
    results_month[counter,1] <- ind
    results_month[counter,2] <- 'Mig 1'
    results_month[counter,3] <- 'Mig 2'
    results_month[counter,4] <- emd_val_month[1]
    counter <- counter + 1
  }
  
  if(length(mig_num) ==3){
    
    mig1 <- subset(dat, IDmonth == ind & Migration == 1)
    mig1sp <- SpatialPoints(mig1[,c("Longitude", "Latitude")])
    proj4string(mig1sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    mig2 <- subset(dat, IDmonth == ind & Migration == 2)
    mig2sp <- SpatialPoints(mig2[,c("Longitude", "Latitude")])
    proj4string(mig2sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    mig3 <- subset(dat, IDmonth == ind & Migration == 3)
    mig3sp <- SpatialPoints(mig3[,c("Longitude", "Latitude")])
    proj4string(mig3sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    
    emd_val_month <- emd(mig1sp, mig2sp, gc=T)
    
    results_month[counter,1] <- ind
    results_month[counter,2] <- 'Mig 1'
    results_month[counter,3] <- 'Mig 2'
    results_month[counter,4] <- emd_val_month[1]
    
    counter <- counter+1
    
    emd_val_month <- emd(mig1sp, mig3sp, gc=T)
    
    results_month[counter,1] <- ind
    results_month[counter,2] <- 'Mig 1'
    results_month[counter,3] <- 'Mig 3'
    results_month[counter,4] <- emd_val_month[1]
    
    counter <- counter+1
    
    emd_val_month <- emd(mig2sp, mig3sp, gc=T)
    
    results_month[counter,1] <- ind
    results_month[counter,2] <- 'Mig 2'
    results_month[counter,3] <- 'Mig 3'
    results_month[counter,4] <- emd_val_month[1]
    
    counter <- counter+1
    
  }
  
  if(length(mig_num) ==4){
    
    mig1 <- subset(dat, IDmonth == ind & Migration == 1)
    mig1sp <- SpatialPoints(mig1[,c("Longitude", "Latitude")])
    proj4string(mig1sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    mig2 <- subset(dat, IDmonth == ind & Migration == 2)
    mig2sp <- SpatialPoints(mig2[,c("Longitude", "Latitude")])
    proj4string(mig2sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    mig3 <- subset(dat, IDmonth == ind & Migration == 3)
    mig3sp <- SpatialPoints(mig3[,c("Longitude", "Latitude")])
    proj4string(mig3sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    mig4 <- subset(dat, IDmonth == ind & Migration == 4)
    mig4sp <- SpatialPoints(mig4[,c("Longitude", "Latitude")])
    proj4string(mig4sp) <- CRS("+proj=longlat +ellps=WGS84
                        +datum=WGS84 +no_defs")
    
    
    emd_val_month <- emd(mig1sp, mig2sp, gc=T)
    
    results_month[counter,1] <- ind
    results_month[counter,2] <- 'Mig 1'
    results_month[counter,3] <- 'Mig 2'
    results_month[counter,4] <- emd_val_month[1]
    
    counter <- counter+1
    
    emd_val_month <- emd(mig1sp, mig3sp, gc=T)
    
    results_month[counter,1] <- ind
    results_month[counter,2] <- 'Mig 1'
    results_month[counter,3] <- 'Mig 3'
    results_month[counter,4] <- emd_val_month[1]
    
    counter <- counter+1
    
    emd_val_month <- emd(mig1sp, mig4sp, gc=T)
    
    results_month[counter,1] <- ind
    results_month[counter,2] <- 'Mig 1'
    results_month[counter,3] <- 'Mig 4'
    results_month[counter,4] <- emd_val_month[1]
    
    counter <- counter+1
    
    emd_val_month <- emd(mig2sp, mig3sp, gc=T)
    
    results_month[counter,1] <- ind
    results_month[counter,2] <- 'Mig 2'
    results_month[counter,3] <- 'Mig 3'
    results_month[counter,4] <- emd_val_month[1]
    
    counter <- counter+1
    
    emd_val_month <- emd(mig2sp, mig4sp, gc=T)
    
    results_month[counter,1] <- ind
    results_month[counter,2] <- 'Mig 2'
    results_month[counter,3] <- 'Mig 4'
    results_month[counter,4] <- emd_val_month[1]
    
    counter <- counter+1
    
    emd_val_month<- emd(mig3sp, mig4sp, gc=T)
    
    results_month[counter,1] <- ind
    results_month[counter,2] <- 'Mig 3'
    results_month[counter,3] <- 'Mig 4'
    results_month[counter,4] <- emd_val_month[1]
    
    counter <- counter+1
    
  }
  
  print(paste0(round((i/length(uniqueIDmonth))*100,1),'%'))
  
}

write.csv(results_month, "data/emd_30day_period.csv", row.names=F)

# Analyse 30-day split EMD data --------------
results_month <- read.csv("data/emd_30day_period.csv")

head(results_month)

results_month2 <- results_month %>% separate(ID, c("ID", "split", "no_split"), "_")
results_month2 <- results_month2 %>% separate(Mig_A, c("MigA", "Mig_numA"), " ")
results_month2 <- results_month2 %>% separate(Mig_B, c("MigB", "Mig_numB"), " ")
results_month2$FocalID <- paste0(results_month2$ID, "_", results_month2$Mig_numA)
results_month2$CompID <- paste0(results_month2$ID, "_", results_month2$Mig_numB)
results_month2 <- results_month2[,c(9,10,8,3)]

# Want EMD values and diff.start2 for same individuals from first EMD analysis which compares the whole of mig1 v mig2
# emd_results2_within <- subset(emd_results2, Within == 1) # done this above for stage analysis

# add difference in departure dates
results_month3 <- plyr::join(results_month2, emd_results2_within[,c("FocalID", "CompID", "diff.julian2")], type= "full")
results_month3$EMD <- as.numeric(results_month3$EMD)

# emd_results2_within2 <- emd_results2_within[, c(1:3,5)] # remove 'within' column
# emd_results2_within2$no_split <- 0 #and replace with no_split column

results_month4 <- rbind(results_month3, emd_results2_within2) # join two df's together so have whole mig and stage EMDs in one
results_month4$no_split <- as.factor(results_month4$no_split)

str(results_month4)
summary(results_month4$no_split)

# GLM
model4 <- glm(EMD ~ no_split + diff.julian2 + diff.julian2:no_split, data=results_month4 %>% filter(no_split != 8), family=Gamma(identity), na.action=na.pass)
model4.1 <- glm(EMD ~ no_split + diff.julian2, data=results_month4 %>% filter(no_split != 8), family=Gamma(identity), na.action=na.omit)
model4.2 <- glm(EMD ~ diff.julian2, data=results_month4 %>% filter(no_split != 8), family=Gamma(identity), na.action=na.omit)
model4.3 <- glm(EMD ~ no_split, data=results_month4 %>% filter(no_split != 8), family=Gamma(identity), na.action=na.omit)

anova(model4.1, model4, test="F")
#EMD ~ start date + same individual  vs. EMD ~ start date
#EMD ~ start date + same individual  vs. EMD ~ same individual  

anova(model4, model4.1, test="F") #test interaction
anova(model4.2, model4.1, test="F") #test split
anova(model4.1, model4.3, test="F") #test start date

# Summary of model for Table S5
summary(model4.1)

# get pairwise comparisons for Table S6
emm2 <- emmeans (model4.1, pairwise ~ no_split) # pairwise comparisons not created
emm2



# Calculating BA for whole migrations -------------------------------------------
# Using same dataframe as above (data/267_RIP_migrations_for_SeabirdTrackingDatabase.csv)

## Create raster layer

# set and check extent to make own raster
# used https://projectionwizard.org/# to check projection for Indian Ocean
box <- extent(-5,125,-40,30) %>%
  as(., "SpatialPolygons") %>% 
  st_as_sf %>% st_set_crs(4326) %>%
  st_transform("+proj=laea +lon_0=77.34375") %>% extent()
box

box %>% as(., "SpatialPolygons") %>% st_as_sf() %>% st_set_crs("+proj=laea +lon_0=77.34375") %>% st_transform(4326) %>%  ggplot() + geom_sf() 

# create raster from scratch
E = extent(-7338354, 4592632, -5495682, 4246296) # the extent, in meters in the projection (or potentially in degrees of lat/lon); the order is: xmin, xmax, ymin, ymax
r = raster(ext = E, resolution = 200000, crs = st_crs("+proj=laea +lon_0=77.34375")$proj4string, vals = 1) # resolution is in meters if you are using a projection, in degrees if using lat/long; crs is the projection.
plot(r) # just to visualise - a raster equal to 1 everywhere, but its resolution and extent will be used later on.
my.pix = as(r, "SpatialPixels")

## Function to calculate UD

udCalc <- function(pred_pts_sub) {
  
  dat = pred_pts_sub[,c("TrackId", "Longitude", "Latitude")] %>% 
    st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326), remove = TRUE) %>% 
    st_transform("+proj=laea +lon_0=77.34375") %>% 
    as_Spatial()
  my.ud = kernelUD(xy=dat, h = 200000, grid = my.pix) # A fixed smoothing parameter (h) of 200 km for GLS data

  return(my.ud)
}

BA_df <- udCalc(df)

BAoverlap <- kerneloverlaphr(BA_df, meth="BA")

BAoverlap <- as.matrix(BAoverlap) # make into a matrix
BAoverlap[upper.tri(BAoverlap)] <- NA # make all upper triangle values of matrix NA (otherwise have duplicate comparisons)

BAoverlap2 <- as.data.frame(BAoverlap) # turn into a dataframe
BAoverlap2 <- BAoverlap2 %>% rownames_to_column("Petrel_1")
BAoverlap3 <- melt(BAoverlap2, id = "Petrel_1")
names(BAoverlap3)[1] <- "CompID"
names(BAoverlap3)[2] <- "FocalID"
names(BAoverlap3)[3] <- "BA"

BAoverlap3 <- subset(BAoverlap3, FocalID != CompID) # subset data to remove same petrel/year comparisons (EMD = 0)
BAoverlap3 <- na.omit(BAoverlap3)

# Add whether or not EMD comparisons are within (1) or between (0) individuals
BAoverlap3 <- BAoverlap3 %>% separate(FocalID, c("ID","Migration"), "_", remove=F)
BAoverlap3 <- BAoverlap3 %>% separate(CompID, c("ID2","Migration2"), "_", remove=F)

BAoverlap3 <- transform(BAoverlap3, Within= ifelse(ID==ID2, 1, 0))
BAoverlap3$Within <- as.factor(BAoverlap3$Within)

BAoverlap4 <- BAoverlap3[,c("FocalID","CompID","BA", "Within")] #remove unnecessary columns
BAoverlap4$FocalID <- as.factor(BAoverlap4$FocalID); BAoverlap4$CompID <- as.factor(BAoverlap4$CompID)
emd_results2$FocalID <- as.factor(emd_results2$FocalID); emd_results2$CompID <- as.factor(emd_results2$CompID)

# Join two df together so can compare results 
BAoverlap5 <- plyr::join(emd_results2, BAoverlap4)

# Run GLM on BA data
model5 <- glm(BA ~  Within + diff.julian2 + diff.julian2:Within,
            family = binomial(link=log), na.action=na.pass,
            data = BAoverlap5)

AIC(model5)
check_overdispersion(model5)
plot(model5) # Check the 4th graph for any outliers beyond the Cook's distance lines

devresid <- resid(model5, type = "deviance")  # calculate the deviance residuals
hist(devresid)       # deviance residuals >2 may indicate a poorly fitting model

summary(model5)
drop1(model5, test="Chi")

model5.1 <- glm(BA ~  Within + diff.julian2,
              family = binomial(link=log), na.action=na.pass,
              data = BAoverlap5)

drop1(model5.1, test="Chi")

# Summary of model for Table S3
summary(model5.1)
