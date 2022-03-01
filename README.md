# RIpetrel-consistency
### Individual consistency in migration strategies of a tropical seabird, the Round Island petrel

Archive of analysis code used in Franklin, K. A. et al. (2022) **Individual consistency in migration strategies of a tropical seabird, the Round Island petrel**, *Movement Ecology*, which is available at xxx. 

This is a guide for how to do the analyses presented in the manuscript. The raw geolocator files and tracking data used in this paper are not available here, but the latter is available for request from the Seabird Tracking Database (http://seabirdtracking.org/) using ID: 1810. Therefore, you will not be able to run the analysis using the Round Island petrel data, but the idea is that you should be able to edit the scripts provided here in order to run your own analyses.

## Steps:
1. **Calculate petrel migration dates using hidden Markov model (HMM).** This step uses the raw light and immersion geolocator files, and processed geographic coordinates, to classify each day of GLS tracking as one of two behavioural states (*ashore on Round Island* or *at-sea*).
   - 01_HMM_dates
2. **Earth mover's distance (EMD).** Using the Round Island petrel tracking data (from the Seabird Tracking Database), we 
   - 02_script
3. **Temporal repeatability**
   - 03_script
4. **Making figures/maps of petrel migrations**
   - 04_script
