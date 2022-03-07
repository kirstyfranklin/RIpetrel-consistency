## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Individual consistency in migration strategies of a tropical seabird, the Round Island petrel. Movement Ecology
## Kirsty A. Franklin
## 04: Making maps and figures of petrel migration
## 01-03-2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load libraries --------------------
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(forestmangr)
library(cowplot)
library(viridis)
library(lubridate)

# Load dataframes --------------------
df <- read.csv("data/all_repeat_migs_for_EMD.csv") # Data from BirdLife Seabird Tracking Database (but only those with repeat migs)

world <- ne_countries(scale = "medium", returnclass = "sf") # load in world map data
RI <- data.frame(Long= 57.30, Lat = -20.11, stringsAsFactors = FALSE) #make point for RI

## Save custom theme as a function ##
theme_custom <- function() {
    theme_void() + 
        theme(panel.border = element_rect(colour = "black", fill=NA, size=0.3), legend.position = "none") 
    }

# Figure 2 --------------------------------------------------
# Map showing more example tracks of petrels with repeat migrations
# Reviewer 1 wants to see the birds which we have three and four years worth of tracking data for

df %>% group_by(BirdId) %>% summarise(n = n_distinct(TrackId)) %>% filter(n > 2)
#	5H00584	4
#	5H33506	4
#	5H14280	3
#	5H30273	3
#	5H33239	3 

(f2a <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + #, expand = FALSE) +
        geom_point(data=df %>% filter(BirdId == "5H00584"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#29bf12", "#f04358", "#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#29bf12", "#f04358", "#F8BB09", "#28B0CE")) +
        theme_custom())

(f2b <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + #, expand = FALSE) +
        geom_point(data=df %>% filter(BirdId == "5H33506"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#29bf12", "#f04358", "#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#29bf12", "#f04358", "#F8BB09", "#28B0CE")) +
        theme_custom())

(f2c <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + #, expand = FALSE) +
        geom_point(data=df %>% filter(BirdId == "5H14280"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE", "#f04358")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE", "#f04358")) +
        theme_custom())

(f2d <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + #, expand = FALSE) +
        geom_point(data=df %>% filter(BirdId == "5H30273"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE", "#f04358")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE", "#f04358")) +
        theme_custom())

(f2e <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) +
        geom_point(data=df %>% filter(BirdId == "5H33239"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE", "#f04358")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE", "#f04358")) +
        theme_custom())

(f2f <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) +
        geom_point(data=df %>% filter(BirdId == "5H33414"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) +
        theme_custom())

(f2g <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) +
        geom_point(data=df %>% filter(BirdId == "5H14506"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) +
        theme_custom())

(f2h <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) +
        geom_point(data=df %>% filter(BirdId == "5H41489"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) +
        theme_custom())

(f2i <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) +
        geom_point(data=df %>% filter(BirdId == "5H41205"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) +
        theme_custom())

(f2j <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) +
        geom_point(data=df %>% filter(BirdId == "5H30529"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) +
        theme_custom())

(f2k <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) +
        geom_point(data=df %>% filter(BirdId == "5H09392"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) +
        theme_custom())

(f2l <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) +
        geom_point(data=df %>% filter(BirdId == "5H09142"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) +
        theme_custom())

# Join plots together - 3 wide, 4 down
(Figure2 <- ggdraw() +
        draw_plot(f2a, x = 0.007, y = 0.752, width = 0.32, height = 0.24) +
        draw_plot(f2b, x = 0.337, y = 0.752, width = 0.32, height = 0.24) +
        draw_plot(f2c, x = 0.667, y = 0.752, width = 0.32, height = 0.24) +
        draw_plot(f2d, x = 0.007, y = 0.502, width = 0.32, height = 0.24) +
        draw_plot(f2e, x = 0.337, y = 0.502, width = 0.32, height = 0.24) +
        draw_plot(f2f, x = 0.667, y = 0.502, width = 0.32, height = 0.24) +
        draw_plot(f2g, x = 0.007, y = 0.252, width = 0.32, height = 0.24) +
        draw_plot(f2h, x = 0.337, y = 0.252, width = 0.32, height = 0.24) +
        draw_plot(f2i, x = 0.667, y = 0.252, width = 0.32, height = 0.24) +
        draw_plot(f2j, x = 0.007, y = 0.002, width = 0.32, height = 0.24) +
        draw_plot(f2k, x = 0.337, y = 0.002, width = 0.32, height = 0.24) +
        draw_plot(f2l, x = 0.667, y = 0.002, width = 0.32, height = 0.24) +
        draw_plot_label(label = c("A","B","C","D","E","F","G","H","I","J","K","L"), size = 10, 
                        x = c(0.009, 0.339, 0.669, 0.009,0.339, 0.669, 0.009, 0.339,0.675, 0.009, 0.339, 0.669), 
                        y = c(0.99, 0.99, 0.99, 0.74, 0.74, 0.74, 0.49, 0.49, 0.49, 0.24, 0.24, 0.24)))

ggsave(filename="figs/Fig2.pdf", plot=last_plot(), width=170, height=190, units="mm", dpi=600)


# Figure 3 -------------------------------------------------------------
# Make maps to show low-moderate-high examples of individual consistency
# Using extreme values from EMD analysis
# Use emd_results2 file from script 02 to figure out which birds to plot, but use df to plot locations
emd_results2 <- read.csv("data/emd_whole_migs_df.csv")

# Within-individual comparison
# Lowest EMD value
emd_results2 %>% filter(Within == 1) %>% filter(EMD == min(EMD)) # 5H33239_1 5H33239_2 156.0081

(a <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + #, expand = FALSE) +
        geom_point(data=df %>% filter(TrackId == "5H33239_1" | TrackId == "5H33239_2"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) + theme_custom() +
        geom_text(aes(label="EMD=156.0"), x=92, y=-41, vjust=0, hjust=0, size=2.5))

# Median EMD value
# Round values in emd_results2 dataframe so it's easier to pull out the exact values and thus ID's
emd_results3 <- forestmangr::round_df(emd_results2, digits=2)
# Find the median value (note minus one because there are an even number of EMD values in this subset, n=78)
median(sort(subset(emd_results3, Within == 1)$EMD)[-1])
# Find which petrel comparison the median EMD value is for
emd_results3 %>% filter(Within == 1) %>% filter(EMD == 620.5) #5H33414_1 5H33414_2 620.5

(b <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + #, expand = FALSE) +
        geom_point(data=df %>% filter(TrackId == "5H33414_1" | TrackId == "5H33414_2"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) +
        theme_custom() +
        geom_text(aes(label="EMD=620.5"), x=92, y=-41, vjust=0, hjust=0, size=2.5))

# Highest EMD value
emd_results2 %>% filter(Within == 1) %>% filter(EMD == max(EMD)) # 5H33506_3 5H33506_4 1618.72

(c <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + #, expand = FALSE) +
        geom_point(data=df %>% filter(TrackId == "5H33506_3" | TrackId == "5H33506_4"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) +
        theme_custom() +
        geom_text(aes(label="EMD=1618.7"), x=91, y=-41, vjust=0, hjust=0, size=2.5))

# Between-individual comparison
# Lowest EMD value
emd_results2 %>% filter(Within == 0) %>% filter(EMD == min(EMD)) # 5H00598_1	5H33389_2	226.35
# Or do this one which goes to Arabian Sea = 5H41065_2	5H41396_1	226.8709

(d <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + #, expand = FALSE) +
        geom_point(data=df %>% filter(TrackId == "5H41065_2" | TrackId == "5H41396_1"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) +
        theme_custom() +
        geom_text(aes(label="EMD=226.4"), x=92, y=-41, vjust=0, hjust=0, size=2.5))

# Median EMD value
median(subset(emd_results3, Within == 0)$EMD) #odd number of EMD values, n=8437
emd_results3 %>% filter(Within == 0) %>% filter(EMD == 1932.61) #5H30273_2 5H41397_2 1932.61

(e <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + #, expand = FALSE) +
        geom_point(data=df %>% filter(TrackId == "5H30273_2" | TrackId == "5H41397_2"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) +
        theme_custom() +
        geom_text(aes(label="EMD=1932.6"), x=91, y=-41, vjust=0, hjust=0, size=2.5))

# Highest EMD value
emd_results2 %>% filter(Within == 0) %>% filter(EMD == max(EMD)) # 5H14305_1 5H14386_1 5418.977

(f <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + #, expand = FALSE) +
        geom_point(data=df %>% filter(TrackId == "5H14305_1" | TrackId == "5H14386_1"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")) +
        theme_custom() +
        geom_text(aes(label="EMD=5419.0"), x=91, y=-41, vjust=0, hjust=0, size=2.5))

# Join plots together
(EMD_plots_v3 <- ggdraw() +
        draw_plot(a, x = 0.04, y = 0.5, width = 0.311, height = 0.5) +
        draw_plot(b, x = 0.36, y = 0.5, width = 0.311, height = 0.5) +
        draw_plot(c, x = 0.68, y = 0.5, width = 0.311, height = 0.5) +
        draw_plot(d, x = 0.04, y = 0.0, width = 0.311, height = 0.5) +
        draw_plot(e, x = 0.36, y = 0.0, width = 0.311, height = 0.5) +
        draw_plot(f, x = 0.68, y = 0.0, width = 0.311, height = 0.5) +
        draw_plot_label(label = c("A","B","C","D","E","F"), size = 10, x = c(0.042, 0.362, 0.682, 0.042,0.362,.682), y = c(0.98, 0.98, 0.98, 0.48, 0.48, 0.48)))

ggsave(filename="figs/Figure3_v3.pdf", plot=last_plot(), width=170, height=108, units="mm", dpi=600)


# Figure S2 -------------------------------------------------------------
# Make a map of all petrel migrations

df$DateGMT2 <- dmy(df$DateGMT) # Make 'DateGMT' into Date so can have different colours for different years

# Plot showing tracks of all petrel migrations
(all_path <- ggplot() +  
    geom_sf(data=world, colour=NA) +
    coord_sf(xlim = c(30,115), ylim= c(-40,30)) + #, expand = FALSE) +
    geom_path(data=df, mapping=aes(y=Latitude, x=Longitude, colour=year(DateGMT2)), size=0.15) + #, colour="#28B0CE") +
    geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
    scale_colour_viridis(discrete=F, name="Year") +
    theme_void() + 
        theme(panel.border = element_rect(colour = "black", fill=NA, size=0.3), legend.position = c(0.92,0.84), legend.key.size = unit(3, 'mm'), legend.text = element_text(size=5),
              legend.title = element_text(size=6)))

# Plots showing geographical coordinates of all petrel migrations
(all_points <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + #, expand = FALSE) +
        geom_point(data=df, mapping=aes(y=Latitude, x=Longitude, alpha=0.5, fill=year(DateGMT2), colour=year(DateGMT2)), size=0.5, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill="black", size = 1.5, inherit.aes = FALSE) + 
        scale_fill_viridis(discrete=F) + scale_colour_viridis(discrete=F) +
        theme_custom())

# Join plots together
(Fig_all2 <- ggdraw() +
        draw_plot(all_points, x = 0.01, y = 0.01, width = 0.48, height = 0.98) +
        draw_plot(all_path, x = 0.5, y = 0.01, width = 0.48, height = 0.98) +
        draw_plot_label(label = c("A","B"), size = 10, x = c(0.01, 0.501), y = c(0.98, 0.98)))

ggsave(filename="figs/Figure_all_migs_both.pdf", plot=last_plot(), width=170, height=70, units="mm", dpi=600)


# Figure S3 --------------------------------------------------
# Maps to demonstrate how much EMD can vary when the BA value is the same

# Highest EMD value when BA = 0.06
# 5H14305_1	5H14386_1  EMD=5418.977  BA=0.05666673 
(S3a <- ggplot() +  
    geom_sf(data=world, colour=NA) +
    coord_sf(xlim = c(30,115), ylim= c(-40,30)) +
    geom_point(data=df %>% filter(TrackId == "5H14305_1" | TrackId == "5H14386_1"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
    geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill= "black", size = 1.5, inherit.aes = FALSE) + 
    theme_custom() +
    geom_text(aes(label=paste0("BA=0.06   EMD=5419.0")), x=71, y=-41, vjust=0, hjust=0, size=2.5) +
    scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
    scale_colour_manual(values=c("#F8BB09", "#28B0CE")))

# Mid EMD valuewhen BA = 0.06
# 5H41369_2	5H41512_1  EMD=3508.855

(S3b <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + 
        geom_point(data=df %>% filter(TrackId == "5H41369_2" | TrackId == "5H41512_1"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill= "black", size = 1.5, inherit.aes = FALSE) +
        theme_custom() +
        geom_text(aes(label=paste0("BA=0.06   EMD=3508.9")), x=71, y=-41, vjust=0, hjust=0, size=2.5) +
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")))

# Lowest EMD value when BA = 0.06
# 5H33414_1	5H33458_1  EMD=2090.752  BA=0.06143034
(S3c <- ggplot() +  
        geom_sf(data=world, colour=NA) +
        coord_sf(xlim = c(30,115), ylim= c(-40,30)) + 
        geom_point(data=df %>% filter(TrackId == "5H33414_1" | TrackId == "5H33458_1"), mapping=aes(y=Latitude, x=Longitude, fill=TrackId, colour=TrackId, alpha=0.5), size=0.8, pch=21, stroke=0.01) +
        geom_point(data=RI, mapping=aes(x=Long, y=Lat), shape=23, fill= "black", size = 1.5, inherit.aes = FALSE) +
        theme_custom() +
        geom_text(aes(label=paste0("BA=0.06   EMD=2090.8")), x=71, y=-41, vjust=0, hjust=0, size=2.5) +
        scale_fill_manual(values=c("#F8BB09", "#28B0CE")) +
        scale_colour_manual(values=c("#F8BB09", "#28B0CE")))

# Join plots together
(Figure_EMDvBA <- ggdraw() +
        draw_plot(S3a, x = 0.005, y = 0, width = 0.32, height = 1.0) +
        draw_plot(S3b, x = 0.335, y = 0, width = 0.32, height = 1.0) +
        draw_plot(S3c, x = 0.665, y = 0, width = 0.32, height = 1.0) +
        draw_plot_label(label = c("A","B","C"), size = 10, x = c(0.009, 0.339, 0.669), y = c(0.94, 0.94, 0.94)))

ggsave(filename="figs/Figure_EMDvBA.pdf", plot=last_plot(), width=170, height=50, units="mm", dpi=600)
