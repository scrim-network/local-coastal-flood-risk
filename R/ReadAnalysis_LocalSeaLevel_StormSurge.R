##==============================================================================
## ReadAnalysis_LocalSeaLevel_StormSurge.R
##
## Script reads in sea level and storm surge data from various studies/ datasets
## and modifies/ converts the data to be comparible.
##
## Contact Kelsey Ruckert (klr324@psu.edu) regarding questions.
##==============================================================================
## Copyright 2018 Kelsey Ruckert
## This file is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## This file is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this file.  If not, see <http://www.gnu.org/licenses/>.
##==============================================================================
# setwd('/Users/klr324/Documents/Data_LSL')


##=========================== READ KOPP ET AL. 2017 DATA ===================================
# Kopp et al. 2017 Local sea-level rise data at Sewell's point tide gaugelibrary(ncdf4)
library(ncdf4)
library(extRemes)
library(RColorBrewer)
library(plotrix)
library(ash)
library(fields)

# Read in conversion functions
source("Helper_scripts/conversion_functions.R")

################################### SEA-LEVEL DATA ##################################
##=========================== READ KOPP ET AL. 2014 DATA ===================================
# Kopp et al. 2014 Local sea-level rise data at Sewell's point tide gauge
# Data is cm above 2000, so 0cm is 2000. Data is projected with RCP26, 45, 60, and 85.
kopp14_rcp26_dat = read.csv("../Data/LSLproj_MC_299_rcp26.csv") 
kopp14_rcp26 = convert_cm_to_ft(data.frame(t_2030 = kopp14_rcp26_dat$X2030, t_2050 = kopp14_rcp26_dat$X2050, 
                                           t_2070 = kopp14_rcp26_dat$X2070, t_2100 = kopp14_rcp26_dat$X2100))

kopp14_rcp45_dat = read.csv("../Data/LSLproj_MC_299_rcp45.csv")
kopp14_rcp45 = convert_cm_to_ft(data.frame(t_2030 = kopp14_rcp45_dat$X2030, t_2050 = kopp14_rcp45_dat$X2050, 
                                           t_2070 = kopp14_rcp45_dat$X2070, t_2100 = kopp14_rcp45_dat$X2100))

kopp14_rcp60_dat = read.csv("../Data/LSLproj_MC_299_rcp60.csv")
kopp14_rcp60 = convert_cm_to_ft(data.frame(t_2030 = kopp14_rcp60_dat$X2030, t_2050 = kopp14_rcp60_dat$X2050, 
                                           t_2070 = kopp14_rcp60_dat$X2070, t_2100 = kopp14_rcp60_dat$X2100))

kopp14_rcp85_dat = read.csv("../Data/LSLproj_MC_299_rcp85.csv")
kopp14_rcp85 = convert_cm_to_ft(data.frame(t_2030 = kopp14_rcp85_dat$X2030, t_2050 = kopp14_rcp85_dat$X2050, 
                                           t_2070 = kopp14_rcp85_dat$X2070, t_2100 = kopp14_rcp85_dat$X2100))
k14_years = seq(2010, 2200, 10)

# Kopp et al. 2014 Local subsidence data at Sewell's point tide gauge
# Data is cm above 2000, so 0cm is 2000. This will be used
# to account for subsidence in the Wong and Keller 2017 data.
kopp14_subsid_dat = read.csv("../Data/LSLProj_bkgd_299_rcp26.csv")
kopp14_subsid = convert_cm_to_ft(data.frame(t_2030 = kopp14_subsid_dat$X2030, t_2050 = kopp14_subsid_dat$X2050, 
                                            t_2070 = kopp14_subsid_dat$X2070, t_2100 = kopp14_subsid_dat$X2100))

# Data is cm above 2000, so 0cm is 2000. Data is projected with RCP26, 45, 60, and 85.
kopp17_DP16_SEW_rcp26_dat = read.csv("../Data/LSLproj_MC_DP16_SEW_299_rcp26.csv") 
kopp17_DP16_SEW_rcp26 = convert_cm_to_ft(data.frame(t_2030 = kopp17_DP16_SEW_rcp26_dat$X2030, t_2050 = kopp17_DP16_SEW_rcp26_dat$X2050, 
                                           t_2070 = kopp17_DP16_SEW_rcp26_dat$X2070, t_2100 = kopp17_DP16_SEW_rcp26_dat$X2100))

kopp17_DP16_SEW_rcp45_dat = read.csv("../Data/LSLproj_MC_DP16_SEW_299_rcp45.csv")
kopp17_DP16_SEW_rcp45 = convert_cm_to_ft(data.frame(t_2030 = kopp17_DP16_SEW_rcp45_dat$X2030, t_2050 = kopp17_DP16_SEW_rcp45_dat$X2050, 
                                           t_2070 = kopp17_DP16_SEW_rcp45_dat$X2070, t_2100 = kopp17_DP16_SEW_rcp45_dat$X2100))

kopp17_DP16_SEW_rcp60_dat = read.csv("../Data/LSLproj_MC_DP16_SEW_299_rcp60.csv")
kopp17_DP16_SEW_rcp60 = convert_cm_to_ft(data.frame(t_2030 = kopp17_DP16_SEW_rcp60_dat$X2030, t_2050 = kopp17_DP16_SEW_rcp60_dat$X2050, 
                                           t_2070 = kopp17_DP16_SEW_rcp60_dat$X2070, t_2100 = kopp17_DP16_SEW_rcp60_dat$X2100))

kopp17_DP16_SEW_rcp85_dat = read.csv("../Data/LSLproj_MC_DP16_SEW_299_rcp85.csv")
kopp17_DP16_SEW_rcp85 = convert_cm_to_ft(data.frame(t_2030 = kopp17_DP16_SEW_rcp85_dat$X2030, t_2050 = kopp17_DP16_SEW_rcp85_dat$X2050, 
                                           t_2070 = kopp17_DP16_SEW_rcp85_dat$X2070, t_2100 = kopp17_DP16_SEW_rcp85_dat$X2100))
k17_DP16_SEW_years = seq(2010, 2300, 10)

##=========================== READ RASMUSSEN ET AL. 2018 DATA ===================================
# Rasmussen et al. 2018 Local sea-level rise data at sewell's point tide gauge
# Data is cm above 2000, so 0cm is 2000. Data is projected with global mean surface temperatures that hit stabilization targets of
# 1.5°C, 2.0°C, and 2.5°C above pre-industrial levels by 2100.
Ras18_SEW_1p5deg_dat = read.csv("../Data/LSLproj_MC_Ras18_SEW_299_1p5degree.csv") 
Ras18_SEW_1p5deg = convert_cm_to_ft(data.frame(t_2030 = Ras18_SEW_1p5deg_dat$X2030, t_2050 = Ras18_SEW_1p5deg_dat$X2050, 
                                                    t_2070 = Ras18_SEW_1p5deg_dat$X2070, t_2100 = Ras18_SEW_1p5deg_dat$X2100))

Ras18_SEW_2p0deg_dat = read.csv("../Data/LSLproj_MC_Ras18_SEW_299_2p0degree.csv") 
Ras18_SEW_2p0deg = convert_cm_to_ft(data.frame(t_2030 = Ras18_SEW_2p0deg_dat$X2030, t_2050 = Ras18_SEW_2p0deg_dat$X2050, 
                                                    t_2070 = Ras18_SEW_2p0deg_dat$X2070, t_2100 = Ras18_SEW_2p0deg_dat$X2100))

Ras18_SEW_2p5deg_dat = read.csv("../Data/LSLproj_MC_Ras18_SEW_299_2p5degree.csv") 
Ras18_SEW_2p5deg = convert_cm_to_ft(data.frame(t_2030 = Ras18_SEW_2p5deg_dat$X2030, t_2050 = Ras18_SEW_2p5deg_dat$X2050, 
                                                    t_2070 = Ras18_SEW_2p5deg_dat$X2070, t_2100 = Ras18_SEW_2p5deg_dat$X2100))
Ras18_SEW_years = seq(2010, 2300, 10)

##=========================== READ SWEET ET AL. 2017 DATA ===================================
# # Data is mm above 2000, so 0mm is 2000. Data is projected with RCP26, 45, 60, and 85.
values = rep(NA, length(seq(2000,2200,10)))
values[1] = 0

# Run a forward euler to estimate sea-level over time
for(i in 2:length(values)){
  values[i] = values[i-1] + 0.00810*10
}

sweet17_03dat = read.csv("../Data/sweet_etal_2017_SEW_0_3.csv", header=TRUE)
sweet17_03 = convert_mm_to_ft(sweet17_03dat)

sweet17_05dat = read.csv("../Data/sweet_etal_2017_SEW_0_5.csv", header=TRUE)
sweet17_05 = convert_mm_to_ft(sweet17_05dat)

sweet17_10dat = read.csv("../Data/sweet_etal_2017_SEW_1_0.csv", header=TRUE)
sweet17_10 = convert_mm_to_ft(sweet17_10dat)

sweet17_15dat = read.csv("../Data/sweet_etal_2017_SEW_1_5.csv", header=TRUE)
sweet17_15 = convert_mm_to_ft(sweet17_15dat)

sweet17_20dat = read.csv("../Data/sweet_etal_2017_SEW_2_0.csv", header=TRUE)
sweet17_20 = convert_mm_to_ft(sweet17_20dat)

sweet17_25dat = read.csv("../Data/sweet_etal_2017_SEW_2_5.csv", header=TRUE)
sweet17_25 = convert_mm_to_ft(sweet17_25dat)

##=========================== READ WONG & KELLER 2017 DATA ===================================
# Wong and Keller 2017 Local sea-level rise data with Fast dynamics at sewell's point tide gauge
# Data is m above 2000, so 0 m is 2000. Data is projected with RCP26, 45, 60, and 85.
# The model used is called BRICK, so any reference to BRICK refers to Wong and Keller 2017. LSL (sewell's point) with fast dynamics. 
fid1 <- nc_open("../Data/BRICK_sewellsPoint_FastDynamics_20Nov2018.nc")
lsl_fdyn_rcp26 <- ncvar_get(fid1,"LocalSeaLevel_RCP26")
lsl_fdyn_rcp45 <- ncvar_get(fid1,"LocalSeaLevel_RCP45")
lsl_fdyn_rcp60 <- ncvar_get(fid1,"LocalSeaLevel_RCP60")
lsl_fdyn_rcp85 <- ncvar_get(fid1,"LocalSeaLevel_RCP85")
year_proj <- ncvar_get(fid1,"time_proj")
nc_close(fid1)

# Wong and Keller 2017 Local sea-level rise data with NO Fast dynamics
fid1 <- nc_open("../Data/BRICK_NOfastDynamics_SP_20Nov2018.nc")
NO_fdyn_rcp26 <- ncvar_get(fid1,"LocalSeaLevel_RCP26")
NO_fdyn_rcp45 <- ncvar_get(fid1,"LocalSeaLevel_RCP45")
NO_fdyn_rcp60 <- ncvar_get(fid1,"LocalSeaLevel_RCP60")
NO_fdyn_rcp85 <- ncvar_get(fid1,"LocalSeaLevel_RCP85")
NO_fdyn_proj <- ncvar_get(fid1,"time_proj")
nc_close(fid1)

# Wong and Keller 2017 Greenland ice sheet and clacier and small ice caps projections
fid2 <- nc_open("../Data/BRICK_physical_fd-gamma_20Nov2018.nc")
GSIC_rcp26 <- ncvar_get(fid2,"GSIC_RCP26")
GIS_rcp26 <- ncvar_get(fid2,"GIS_RCP26")
GSIC_rcp45 <- ncvar_get(fid2,"GSIC_RCP45")
GIS_rcp45 <- ncvar_get(fid2,"GIS_RCP45")
GSIC_rcp85 <- ncvar_get(fid2,"GSIC_RCP85")
GIS_rcp85 <- ncvar_get(fid2,"GIS_RCP85")
ice_proj <- ncvar_get(fid2,"time_proj")
nc_close(fid1)

#--------------------- Generate subsidence data matching Wong & Keller 2017 ensemble size --------------------------
# Kopp et al. 2014 and Wong & Keller 2017 have different numbers of samples. Randomly generate subsidence
# data using Kopp et al. 2014 data. This new ensemble will match the sample size of Wong & Keller 2017
states_w = dim(lsl_fdyn_rcp26)[2]
BKkopp_subsid = data.frame(t_2030 = 1:states_w, t_2050 = 1:states_w, t_2070 = 1:states_w, t_2100 = 1:states_w)
for(i in 1:4){
  meanlog <- log(mean(kopp14_subsid[ ,i])) - 0.5 * log(1 + (sd(kopp14_subsid[ ,i])^2)/(mean(kopp14_subsid[ ,i])^2))
  sdlog <- sqrt(log(1 + (sd(kopp14_subsid[ ,i])^2)/(mean(kopp14_subsid[ ,i])^2)))
  BKkopp_subsid[ ,i] = rlnorm(states_w, meanlog=meanlog, sdlog=sdlog)
}

BKkopp_subsid_dat = mat.or.vec(states_w, length(k14_years))
for(i in 1:length(k14_years)){
  meanlog <- log(mean(kopp14_subsid_dat[ ,i])) - 0.5 * log(1 + (sd(kopp14_subsid_dat[ ,i])^2)/(mean(kopp14_subsid_dat[ ,i])^2))
  sdlog <- sqrt(log(1 + (sd(kopp14_subsid_dat[ ,i])^2)/(mean(kopp14_subsid_dat[ ,i])^2)))
  BKkopp_subsid_dat[ ,i] = convert_cm_to_ft(rlnorm(states_w, meanlog=meanlog, sdlog=sdlog))
}

# Linear regression of subsidence to add to BRICK data
t.time = 2010:2200
subsid.fit = mat.or.vec(states_w, length(t.time))
for(i in 1:states_w){
  fit = lm(BKkopp_subsid_dat[i,1:20] ~ k14_years[1:20])
  subsid.fit[i,] = fit$coefficients[1] + t.time*fit$coefficients[2]
}

# Add subsidence linear fit to BRICK data
lsl_fdyn_rcp26_sub = 
  lsl_fdyn_rcp45_sub = 
  lsl_fdyn_rcp60_sub = 
  lsl_fdyn_rcp85_sub = 
  NO_fdyn_rcp26_sub = 
  NO_fdyn_rcp45_sub = 
  NO_fdyn_rcp60_sub = 
  NO_fdyn_rcp85_sub = mat.or.vec(length(t.time), states_w)
for(i in 1:states_w){
  lsl_fdyn_rcp26_sub[ ,i] = convert_m_to_ft(lsl_fdyn_rcp26[match(2010:2200, year_proj),i]) + subsid.fit[i, ]
  lsl_fdyn_rcp45_sub[ ,i] = convert_m_to_ft(lsl_fdyn_rcp45[match(2010:2200, year_proj),i]) + subsid.fit[i, ]
  lsl_fdyn_rcp60_sub[ ,i] = convert_m_to_ft(lsl_fdyn_rcp60[match(2010:2200, year_proj),i]) + subsid.fit[i, ]
  lsl_fdyn_rcp85_sub[ ,i] = convert_m_to_ft(lsl_fdyn_rcp85[match(2010:2200, year_proj),i]) + subsid.fit[i, ]
  
  NO_fdyn_rcp26_sub[ ,i] = convert_m_to_ft(NO_fdyn_rcp26[match(2010:2200, year_proj),i]) + subsid.fit[i, ]
  NO_fdyn_rcp45_sub[ ,i] = convert_m_to_ft(NO_fdyn_rcp45[match(2010:2200, year_proj),i]) + subsid.fit[i, ]
  NO_fdyn_rcp60_sub[ ,i] = convert_m_to_ft(NO_fdyn_rcp60[match(2010:2200, year_proj),i]) + subsid.fit[i, ]
  NO_fdyn_rcp85_sub[ ,i] = convert_m_to_ft(NO_fdyn_rcp85[match(2010:2200, year_proj),i]) + subsid.fit[i, ]
}

# use for SLR + storm surge analysis
fd_r26_sub_2065 = lsl_fdyn_rcp26_sub[match(2065, t.time), ]
fd_r45_sub_2065 = lsl_fdyn_rcp45_sub[match(2065, t.time), ]
fd_r85_sub_2065 = lsl_fdyn_rcp85_sub[match(2065, t.time), ]

Nfd_r26_sub_2065 = NO_fdyn_rcp26_sub[match(2065, t.time), ]
Nfd_r45_sub_2065 = NO_fdyn_rcp45_sub[match(2065, t.time), ]
Nfd_r85_sub_2065 = NO_fdyn_rcp85_sub[match(2065, t.time), ]

#------------------------- Add subsidence to Wong & Keller 2017 data --------------------------------
# Function changing BRICK from m to feet and adding subsidence based on Kopp et al. 2014
combine_subsid_to_brick = function(out_name, brick_input, y_projection, subsidence){
  years = c(2030, 2050, 2070, 2100)
  for(i in 1:4){
    out_name[ ,i] = convert_m_to_ft(brick_input[match(years[i], y_projection), ]) + subsidence[ ,i]
  }
  out_name
}

# Local sea-level rise with Fast dynamics
brickfd_rcp26 = 
  brickfd_rcp45 = 
  brickfd_rcp60 = 
  brickfd_rcp85 = data.frame(t_2030 = 1:states_w, t_2050 = 1:states_w, t_2070 = 1:states_w, t_2100 = 1:states_w)

brickfd_rcp26 = combine_subsid_to_brick(brickfd_rcp26, lsl_fdyn_rcp26, year_proj, BKkopp_subsid)
brickfd_rcp45 = combine_subsid_to_brick(brickfd_rcp45, lsl_fdyn_rcp45, year_proj, BKkopp_subsid)
brickfd_rcp60 = combine_subsid_to_brick(brickfd_rcp60, lsl_fdyn_rcp60, year_proj, BKkopp_subsid)
brickfd_rcp85 = combine_subsid_to_brick(brickfd_rcp85, lsl_fdyn_rcp85, year_proj, BKkopp_subsid)

# Local sea-level rise with NO Fast dynamics
NO_fdft_rcp26 = 
  NO_fdft_rcp45 = 
  NO_fdft_rcp60 = 
  NO_fdft_rcp85 = data.frame(t_2030 = 1:states_w, t_2050 = 1:states_w, t_2070 = 1:states_w, t_2100 = 1:states_w)

NO_fdft_rcp26 = combine_subsid_to_brick(NO_fdft_rcp26, NO_fdyn_rcp26, NO_fdyn_proj, BKkopp_subsid)
NO_fdft_rcp45 = combine_subsid_to_brick(NO_fdft_rcp45, NO_fdyn_rcp45, NO_fdyn_proj, BKkopp_subsid)
NO_fdft_rcp60 = combine_subsid_to_brick(NO_fdft_rcp60, NO_fdyn_rcp60, NO_fdyn_proj, BKkopp_subsid)
NO_fdft_rcp85 = combine_subsid_to_brick(NO_fdft_rcp85, NO_fdyn_rcp85, NO_fdyn_proj, BKkopp_subsid)

##===================== READ PARRIS ET AL 2012; USACE 2014; HALL ET AL 2016 DATA ===========================
# Read in data from the USACE Sea level calculator for sewell's Point: http://www.corpsclimate.us/ccaceslcurves.cfm
# Data is ft above 1992, so 0 ft is 1992
SL_calculator = read.csv("../Data/SewellsPoint_Parris12_USACE14_Hall16_SL_data.csv", skip=1, header=TRUE)
SL_calculator = as.matrix(SL_calculator)

#----------------------------- Convert baseline to 2000 -------------------------------------
SL_calculator_ref2000 = SL_calculator
for(i in 2:13){
  SL_calculator_ref2000[,i] = SL_calculator[,i] - SL_calculator[match(2000, SL_calculator[,1]),i]
}

#--------------------------- Extract specific years for each study --------------------------
match_SLC = match(c(2030,2050,2070,2100), SL_calculator[,1])
# Parris et al. 2012
parris_etal_2012 = data.frame(t_2030 = c(SL_calculator_ref2000[match_SLC[1], 2:5]), 
                      t_2050 = c(SL_calculator_ref2000[match_SLC[2], 2:5]),
                      t_2070 = c(SL_calculator_ref2000[match_SLC[3], 2:5]), 
                      t_2100 = c(SL_calculator_ref2000[match_SLC[4], 2:5]), row.names = NULL)
# USACE 2014
usace2014 = data.frame(t_2030 = c(SL_calculator_ref2000[match_SLC[1], 6:8]), 
                       t_2050 = c(SL_calculator_ref2000[match_SLC[2], 6:8]),
                       t_2070 = c(SL_calculator_ref2000[match_SLC[3], 6:8]), 
                       t_2100 = c(SL_calculator_ref2000[match_SLC[4], 6:8]), row.names = NULL)
# Hall et al. 2016
hall_etal_2016 = data.frame(t_2030 = c(SL_calculator_ref2000[match_SLC[1], 9:13]), 
                        t_2050 = c(SL_calculator_ref2000[match_SLC[2], 9:13]),
                        t_2070 = c(SL_calculator_ref2000[match_SLC[3], 9:13]), 
                        t_2100 = c(SL_calculator_ref2000[match_SLC[4], 9:13]), row.names = NULL)

################################### STORM SURGE DATA ##################################
##=========================== READ USACE 2014 EXTREME WATER LEVEL DATA ===================================
# Data is presented in feet and meters above mean sea level
USACE_EWL = read.csv("../Data/USACE_ExtremeWaterLevels_SewellsPoint.csv", skip=2, header=TRUE) 
USACE_rp = as.numeric(as.character(USACE_EWL$Datum_EWL[8:14]))

##=========================== READ TEBALDI ET AL 2012 DATA ===================================
# Data is presented in meters above mean high water
tebaldi12 = read.csv("../Data/SewellsPoint_allrpsGPD_Tebaldi_etal_2012.csv", col.names=c("rp", "rl_50", "rp.1", "rl_025", "rl_975"))
tebaldi12[,c(2,4,5)] = convert_mhw_to_msl(convert_m_to_ft(tebaldi12[,c(2,4,5)]))

##==================== READ GEV ANALYSIS OF HISTORIC TIDE GAUGE OBSERVATIONS ============================
# Data is presented in meters above mean sea level; tide gauge observations are from sewell's Point.
NOAA_methodGEV = read.csv("../Data/NOAA_method_stormsurge_sewellspoint.csv")
NOAA_methodGEV[,c(3,4,5,6,9)] = convert_m_to_ft(NOAA_methodGEV[,c(3,4,5,6,9)])

# Read in a function that returns a vector of the median return periods for an input vector of block maxima observations
source("Helper_scripts/Empirical_probability_calculator.R")

# Snag the annual block maxima observations and remove the NAs from the vector
blockMax = as.numeric(na.omit(NOAA_methodGEV$obs))

# Run the numerical median probability return period formula
rp_ABM = median.rt(blockMax)

# Approximate the return period for the Norfolk-Long Island Hurricane of 1821, which has a return period far exceeding the historical 
# record. The storm surge of of 1821 may have been exceeded by a storm in 1825 or storms prior to 1806, so use those values to 
# estimate an uncertainty bar.
NLIH_1821 = 10 # 10 feet of surge in some areas of VA
returnperiod_l = 2018 - 1825 # low bound
returnperiod_m = 2018 - 1821
returnperiod_h = 2018 - 1806 # upper bound

rp_l = max(median.rt(vector(mode = 'numeric', length = returnperiod_l)))
rp_h = max(median.rt(vector(mode = 'numeric', length = returnperiod_h)))
rp_m = max(median.rt(vector(mode = 'numeric', length = returnperiod_m)))

##=========================== READ ZERVAS 2013 (NOAA) DATA ===================================
# NOAA MLE (Zervas data just with more points) from the USACE Sea level calculator for Sewell's Point: 
# http://www.corpsclimate.us/ccaceslcurves.cfm. Feet above MSL;
# And MLE with 95% Confidence interval from Table C in Appendix III of Zervas 2013
# Meters above MHHW; Loc = 0.678 ± 0.041, scale = 0.170 ± 0.031, shape = 0.120 ± 0.163
zervas_2013 = read.csv("../Data/Zervas_2013_ExtremeWaterLevels_SewellsPoint.csv", skip=2, header=TRUE)

# Convert the data collected from Table C to ft msl. Other data is already in ft above msl.
zervas_2013[,7:9] = convert_mhhw_to_msl(convert_m_to_ft(zervas_2013[,7:9]))

##=========================== READ WONG 2018 DATA ===================================
# Millimeters above mean sea level
wong18_stormsurge = readRDS("../Data/Wong_2018_norfolk_MCMC_stormsurge.rds")
wong18_stationary = wong18_stormsurge$time$gpd3
wong18_rl_years = c(2, 3, 5, 10, 20, 30, 50, 80, 100, 150, 200, 250, 300, 350, 400, 450, 500, 1000)

# Estimate the 97.5 and 2.5% confidence intervals 
wong18_stat_025 = wong18_stat_50 = wong18_stat_975 = vector(mode = "numeric", length = length(wong18_rl_years))
for(i in 1:length(wong18_rl_years)){
  wong18_stat_025[i] = quantile(wong18_stationary[ ,i],0.025)
  wong18_stat_50[i] = quantile(wong18_stationary[ ,i],0.5)
  wong18_stat_975[i] = quantile(wong18_stationary[ ,i],0.975)
}

# Convert values to feet
wong18_stat_025 = convert_mm_to_ft(wong18_stat_025)
wong18_stat_50 = convert_mm_to_ft(wong18_stat_50)
wong18_stat_975 = convert_mm_to_ft(wong18_stat_975)

# Create dataframe of median 100-yr return levels of stationary and non stationary values in 2065
wong_18 = data.frame(rl = c(quantile(wong18_stationary[ ,match(100, wong18_rl_years)], 0.5),
                            quantile(wong18_stormsurge$nao$bma[ ,match(100, wong18_rl_years)], 0.5),
                            quantile(wong18_stormsurge$time$bma[ ,match(100, wong18_rl_years)], 0.5),
                            quantile(wong18_stormsurge$temp$bma[ ,match(100, wong18_rl_years)], 0.5),
                            quantile(wong18_stormsurge$bma[ ,match(100, wong18_rl_years)], 0.5),
                            quantile(wong18_stormsurge$sealevel$bma[ ,match(100, wong18_rl_years)], 0.5)),
                     rp = rep(100, 6), year = rep(2065, 6), 
                     covariate_model = c("Stationary", "NAO", "Time", "Temperature", "BMA", "Sea level"), 
                     color = brewer.pal(9,"Blues")[4:9])
# Convert values to feet
wong_18$rl = convert_mm_to_ft(wong_18$rl)

##=========================== READ SRIKRISHNAN ET AL. IN PREP. DATA ===================================
# Millimeters above mean sea level
stationary = readRDS("../Data/Srikrishnan_norfolk_MCMC-stationary.rds")

#------------------- Remove a burnin and extract the 95% parameter estimates ------------------- 
burnin = 1:50000
burn_stationary = stationary[[1]]$samples[-burnin, ]

# Generate GEV functions for each state of the world via the parameter distributions
# Figure 3 plots just past the 500-yr return period so the 1000-yr return period is sufficient to reduce compute time 
# and still estimate the 100-yr and 500-yr return period
probs = seq(1/1000, c(1 - 1/1000), length.out = 1000)
stat_sotw = mat.or.vec(length(probs), nrow(burn_stationary))
for(i in 1:nrow(burn_stationary)){
stat_sotw[ ,i] = qevd(probs, loc = burn_stationary[i,1], scale = burn_stationary[i,2], shape = burn_stationary[i,3], type='GEV')
}

# Estimate the 97.5 and 2.5% confidence intervals 
gev_stat_025 = gev_stat_50 = gev_stat_975 = vector(mode = "numeric", length = nrow(stat_sotw))
for(i in 1:nrow(stat_sotw)){
  gev_stat_025[i] = quantile(stat_sotw[i, ],0.025)
  gev_stat_50[i] = quantile(stat_sotw[i, ],0.5)
  gev_stat_975[i] = quantile(stat_sotw[i, ],0.975)
}
# Convert values to feet
gev_stat_025 = convert_mm_to_ft(gev_stat_025)
gev_stat_50 = convert_mm_to_ft(gev_stat_50)
gev_stat_975 = convert_mm_to_ft(gev_stat_975)

################################### COMBINED SLR and STORM SURGE DATA ##################################
# ADD description of revd
#------------------- Generate GEV distribution using Srikrishnan et al. in prep. -----------------
# Determine Wong and Keller 2017 ensemble size
subbrick_length = length(brickfd_rcp26$t_2030)
# Generate a subset of GEV parameter sets matching the ensemble size of Wong and Keller 2017.
subbrick_stat2 = stat_sotw[ , sample(ncol(stat_sotw), size=subbrick_length, replace=FALSE)]
stationary_brick = convert_mm_to_ft(subbrick_stat2)

# Generate storm surge GEV distribution with an ensemble size matching Kopp et al. 2014, Kopp et al. 2017, and Rasmussen et al. 2018
subkopp_length = length(kopp14_rcp26$t_2030)
# Generate a subset of GEV parameter sets
subkopp_stat2 = stat_sotw[ , sample(ncol(stat_sotw), size=subkopp_length, replace=FALSE)]
stationary_kopp = convert_mm_to_ft(subkopp_stat2)

# Generate storm surge GEV distribution with an ensemble size matching Sweet et al. 2017
subsweet17_SEW_length = length(sweet17_03$X2030)
# Generate a subset of GEV parameter sets
subsweet17_SEW_stat2 = stat_sotw[ , sample(ncol(stat_sotw), size=subsweet17_SEW_length, replace=FALSE)]
stationary_sweet17_SEW = convert_mm_to_ft(subsweet17_SEW_stat2)

# Generate ensemble sizes of Wong and Keller 2017 SLR to match Wong 2018 storm surge ensembles
# non-stationary
bma_subset_fd_r26 = sample(fd_r26_sub_2065, size=nrow(wong18_stormsurge$bma), replace=FALSE)
bma_subset_fd_r45 = sample(fd_r45_sub_2065, size=nrow(wong18_stormsurge$bma), replace=FALSE)
bma_subset_fd_r85 = sample(fd_r85_sub_2065, size=nrow(wong18_stormsurge$bma), replace=FALSE)

bma_subset_Nfd_r26 = sample(Nfd_r26_sub_2065, size=nrow(wong18_stormsurge$bma), replace=FALSE)
bma_subset_Nfd_r45 = sample(Nfd_r45_sub_2065, size=nrow(wong18_stormsurge$bma), replace=FALSE)
bma_subset_Nfd_r85 = sample(Nfd_r85_sub_2065, size=nrow(wong18_stormsurge$bma), replace=FALSE)

# stationary
stat_subset_fd_r26 = sample(fd_r26_sub_2065, size=nrow(wong18_stationary), replace=FALSE)
stat_subset_fd_r45 = sample(fd_r45_sub_2065, size=nrow(wong18_stationary), replace=FALSE)
stat_subset_fd_r85 = sample(fd_r85_sub_2065, size=nrow(wong18_stationary), replace=FALSE)

stat_subset_Nfd_r26 = sample(Nfd_r26_sub_2065, size=nrow(wong18_stationary), replace=FALSE)
stat_subset_Nfd_r45 = sample(Nfd_r45_sub_2065, size=nrow(wong18_stationary), replace=FALSE)
stat_subset_Nfd_r85 = sample(Nfd_r85_sub_2065, size=nrow(wong18_stationary), replace=FALSE)
  
#--------------------------- Add distribution of storm surge data to SLR data --------------------------
# Add stationary and non-stationary storm surge (in 2065) from Wong 2018 to Wong and Keller 2017
bma_SLR_fd_r26 = bma_SLR_fd_r45 = bma_SLR_fd_r85 = bma_SLR_NOfd_r26 = bma_SLR_NOfd_r45 = bma_SLR_NOfd_r85 = 
  stat_SLR_fd_r26 = stat_SLR_fd_r45 = stat_SLR_fd_r85 = stat_SLR_NOfd_r26 = stat_SLR_NOfd_r45 = 
  stat_SLR_NOfd_r85 = mat.or.vec(ncol(wong18_stormsurge$bma), nrow(wong18_stormsurge$bma))
for(i in 1:nrow(wong18_stormsurge$bma)){
  bma_SLR_fd_r26[,i] = bma_subset_fd_r26[i] + convert_mm_to_ft(wong18_stormsurge$bma[i, ])
  bma_SLR_fd_r45[,i] = bma_subset_fd_r45[i] + convert_mm_to_ft(wong18_stormsurge$bma[i, ])
  bma_SLR_fd_r85[,i] = bma_subset_fd_r85[i] + convert_mm_to_ft(wong18_stormsurge$bma[i, ])
  bma_SLR_NOfd_r26[,i] = bma_subset_Nfd_r26[i] + convert_mm_to_ft(wong18_stormsurge$bma[i, ])
  bma_SLR_NOfd_r45[,i] = bma_subset_Nfd_r45[i] + convert_mm_to_ft(wong18_stormsurge$bma[i, ])
  bma_SLR_NOfd_r85[,i] = bma_subset_Nfd_r85[i] + convert_mm_to_ft(wong18_stormsurge$bma[i, ])
  stat_SLR_fd_r26[,i] = stat_subset_fd_r26[i] + convert_mm_to_ft(wong18_stationary[i, ])
  stat_SLR_fd_r45[,i] = stat_subset_fd_r45[i] + convert_mm_to_ft(wong18_stationary[i, ])
  stat_SLR_fd_r85[,i] = stat_subset_fd_r85[i] + convert_mm_to_ft(wong18_stationary[i, ])
  stat_SLR_NOfd_r26[,i] = stat_subset_Nfd_r26[i] + convert_mm_to_ft(wong18_stationary[i, ])
  stat_SLR_NOfd_r45[,i] = stat_subset_Nfd_r45[i] + convert_mm_to_ft(wong18_stationary[i, ])
  stat_SLR_NOfd_r85[,i] = stat_subset_Nfd_r85[i] + convert_mm_to_ft(wong18_stationary[i, ])
}

# Add stationary storm surge to Kopp et al. 2014; 2017 and Rasmussen et al. 2018
k14_r26_SS_t_2030 = k14_r26_SS_t_2050 = k14_r26_SS_t_2070 = k14_r26_SS_t_2100 = 
  k14_r45_SS_t_2030 = k14_r45_SS_t_2050 = k14_r45_SS_t_2070 = k14_r45_SS_t_2100 = 
  k14_r85_SS_t_2030 = k14_r85_SS_t_2050 = k14_r85_SS_t_2070 = k14_r85_SS_t_2100 = 
  k17_DP16_SEW_r26_SS_t_2030 = k17_DP16_SEW_r26_SS_t_2050 = k17_DP16_SEW_r26_SS_t_2070 = k17_DP16_SEW_r26_SS_t_2100 = 
  k17_DP16_SEW_r45_SS_t_2030 = k17_DP16_SEW_r45_SS_t_2050 = k17_DP16_SEW_r45_SS_t_2070 = k17_DP16_SEW_r45_SS_t_2100 =   
  k17_DP16_SEW_r85_SS_t_2030 = k17_DP16_SEW_r85_SS_t_2050 = k17_DP16_SEW_r85_SS_t_2070 = k17_DP16_SEW_r85_SS_t_2100 = 
  Ras18_SEW_1p5deg_SS_t_2030 = Ras18_SEW_1p5deg_SS_t_2050 = Ras18_SEW_1p5deg_SS_t_2070 = Ras18_SEW_1p5deg_SS_t_2100 = 
  Ras18_SEW_2p0deg_SS_t_2030 = Ras18_SEW_2p0deg_SS_t_2050 = Ras18_SEW_2p0deg_SS_t_2070 = Ras18_SEW_2p0deg_SS_t_2100 = 
  Ras18_SEW_2p5deg_SS_t_2030 = Ras18_SEW_2p5deg_SS_t_2050 = Ras18_SEW_2p5deg_SS_t_2070 = Ras18_SEW_2p5deg_SS_t_2100 = mat.or.vec(nrow(stationary_kopp), ncol(stationary_kopp))
for(i in 1:ncol(stationary_kopp)){
  # Add stationary storm surge to Kopp et al. 2014
  k14_r26_SS_t_2030[ ,i] = kopp14_rcp26$t_2030[i] + stationary_kopp[ ,i]
  k14_r26_SS_t_2050[ ,i] = kopp14_rcp26$t_2050[i] + stationary_kopp[ ,i]
  k14_r26_SS_t_2070[ ,i] = kopp14_rcp26$t_2070[i] + stationary_kopp[ ,i]
  k14_r26_SS_t_2100[ ,i] = kopp14_rcp26$t_2100[i] + stationary_kopp[ ,i]
  k14_r45_SS_t_2030[ ,i] = kopp14_rcp45$t_2030[i] + stationary_kopp[ ,i]
  k14_r45_SS_t_2050[ ,i] = kopp14_rcp45$t_2050[i] + stationary_kopp[ ,i]
  k14_r45_SS_t_2070[ ,i] = kopp14_rcp45$t_2070[i] + stationary_kopp[ ,i]
  k14_r45_SS_t_2100[ ,i] = kopp14_rcp45$t_2100[i] + stationary_kopp[ ,i]
  k14_r85_SS_t_2030[ ,i] = kopp14_rcp85$t_2030[i] + stationary_kopp[ ,i]
  k14_r85_SS_t_2050[ ,i] = kopp14_rcp85$t_2050[i] + stationary_kopp[ ,i]
  k14_r85_SS_t_2070[ ,i] = kopp14_rcp85$t_2070[i] + stationary_kopp[ ,i]
  k14_r85_SS_t_2100[ ,i] = kopp14_rcp85$t_2100[i] + stationary_kopp[ ,i]
  # Add stationary storm surge to Kopp et al. 2017
  k17_DP16_SEW_r26_SS_t_2030[ ,i] = kopp17_DP16_SEW_rcp26$t_2030[i] + stationary_kopp[ ,i]
  k17_DP16_SEW_r26_SS_t_2050[ ,i] = kopp17_DP16_SEW_rcp26$t_2050[i] + stationary_kopp[ ,i]
  k17_DP16_SEW_r26_SS_t_2070[ ,i] = kopp17_DP16_SEW_rcp26$t_2070[i] + stationary_kopp[ ,i]
  k17_DP16_SEW_r26_SS_t_2100[ ,i] = kopp17_DP16_SEW_rcp26$t_2100[i] + stationary_kopp[ ,i]
  k17_DP16_SEW_r45_SS_t_2030[ ,i] = kopp17_DP16_SEW_rcp45$t_2030[i] + stationary_kopp[ ,i]
  k17_DP16_SEW_r45_SS_t_2050[ ,i] = kopp17_DP16_SEW_rcp45$t_2050[i] + stationary_kopp[ ,i]
  k17_DP16_SEW_r45_SS_t_2070[ ,i] = kopp17_DP16_SEW_rcp45$t_2070[i] + stationary_kopp[ ,i]
  k17_DP16_SEW_r45_SS_t_2100[ ,i] = kopp17_DP16_SEW_rcp45$t_2100[i] + stationary_kopp[ ,i]
  k17_DP16_SEW_r85_SS_t_2030[ ,i] = kopp17_DP16_SEW_rcp85$t_2030[i] + stationary_kopp[ ,i]
  k17_DP16_SEW_r85_SS_t_2050[ ,i] = kopp17_DP16_SEW_rcp85$t_2050[i] + stationary_kopp[ ,i]
  k17_DP16_SEW_r85_SS_t_2070[ ,i] = kopp17_DP16_SEW_rcp85$t_2070[i] + stationary_kopp[ ,i]
  k17_DP16_SEW_r85_SS_t_2100[ ,i] = kopp17_DP16_SEW_rcp85$t_2100[i] + stationary_kopp[ ,i]
  # Add stationary storm surge to Rasmussen et al. 2018
  Ras18_SEW_1p5deg_SS_t_2030[ ,i] = Ras18_SEW_1p5deg$t_2030[i] + stationary_kopp[ ,i]
  Ras18_SEW_1p5deg_SS_t_2050[ ,i] = Ras18_SEW_1p5deg$t_2050[i] + stationary_kopp[ ,i]
  Ras18_SEW_1p5deg_SS_t_2070[ ,i] = Ras18_SEW_1p5deg$t_2070[i] + stationary_kopp[ ,i]
  Ras18_SEW_1p5deg_SS_t_2100[ ,i] = Ras18_SEW_1p5deg$t_2100[i] + stationary_kopp[ ,i]
  Ras18_SEW_2p0deg_SS_t_2030[ ,i] = Ras18_SEW_2p0deg$t_2030[i] + stationary_kopp[ ,i]
  Ras18_SEW_2p0deg_SS_t_2050[ ,i] = Ras18_SEW_2p0deg$t_2050[i] + stationary_kopp[ ,i]
  Ras18_SEW_2p0deg_SS_t_2070[ ,i] = Ras18_SEW_2p0deg$t_2070[i] + stationary_kopp[ ,i]
  Ras18_SEW_2p0deg_SS_t_2100[ ,i] = Ras18_SEW_2p0deg$t_2100[i] + stationary_kopp[ ,i]
  Ras18_SEW_2p5deg_SS_t_2030[ ,i] = Ras18_SEW_2p5deg$t_2030[i] + stationary_kopp[ ,i]
  Ras18_SEW_2p5deg_SS_t_2050[ ,i] = Ras18_SEW_2p5deg$t_2050[i] + stationary_kopp[ ,i]
  Ras18_SEW_2p5deg_SS_t_2070[ ,i] = Ras18_SEW_2p5deg$t_2070[i] + stationary_kopp[ ,i]
  Ras18_SEW_2p5deg_SS_t_2100[ ,i] = Ras18_SEW_2p5deg$t_2100[i] + stationary_kopp[ ,i]
}

k14_r26_SS_025 = k14_r26_SS_975 = k14_r45_SS_025 = k14_r45_SS_975 = k14_r85_SS_025 = k14_r85_SS_975 = 
  k17_DP16_SEW_r26_SS_025 = k17_DP16_SEW_r26_SS_975 = k17_DP16_SEW_r45_SS_025 = k17_DP16_SEW_r45_SS_975 = k17_DP16_SEW_r85_SS_025 = k17_DP16_SEW_r85_SS_975 = 
  Ras18_SEW_1p5deg_SS_025 = Ras18_SEW_1p5deg_SS_975 = Ras18_SEW_2p0deg_SS_025 = Ras18_SEW_2p0deg_SS_975 = Ras18_SEW_2p5deg_SS_025 = Ras18_SEW_2p5deg_SS_975 =
  data.frame(t_2030 = 1:nrow(k14_r26_SS_t_2030), t_2050 = 1:nrow(k14_r26_SS_t_2030), t_2070 = 1:nrow(k14_r26_SS_t_2030), t_2100 = 1:nrow(k14_r26_SS_t_2030))

# Estimate the 97.5 and 2.5% confidence intervals 
for(i in 1:nrow(k14_r26_SS_t_2030)){
  # Kopp et al. 2014
  k14_r26_SS_025$t_2030[i] = quantile(k14_r26_SS_t_2030[i, ],0.025)
  k14_r26_SS_025$t_2050[i] = quantile(k14_r26_SS_t_2050[i, ],0.025)
  k14_r26_SS_025$t_2070[i] = quantile(k14_r26_SS_t_2070[i, ],0.025)
  k14_r26_SS_025$t_2100[i] = quantile(k14_r26_SS_t_2100[i, ],0.025)
  k14_r26_SS_975$t_2030[i] = quantile(k14_r26_SS_t_2030[i, ],0.975)
  k14_r26_SS_975$t_2050[i] = quantile(k14_r26_SS_t_2050[i, ],0.975)
  k14_r26_SS_975$t_2070[i] = quantile(k14_r26_SS_t_2070[i, ],0.975)
  k14_r26_SS_975$t_2100[i] = quantile(k14_r26_SS_t_2100[i, ],0.975)
  k14_r45_SS_025$t_2030[i] = quantile(k14_r45_SS_t_2030[i, ],0.025)
  k14_r45_SS_025$t_2050[i] = quantile(k14_r45_SS_t_2050[i, ],0.025)
  k14_r45_SS_025$t_2070[i] = quantile(k14_r45_SS_t_2070[i, ],0.025)
  k14_r45_SS_025$t_2100[i] = quantile(k14_r45_SS_t_2100[i, ],0.025)
  k14_r45_SS_975$t_2030[i] = quantile(k14_r45_SS_t_2030[i, ],0.975)
  k14_r45_SS_975$t_2050[i] = quantile(k14_r45_SS_t_2050[i, ],0.975)
  k14_r45_SS_975$t_2070[i] = quantile(k14_r45_SS_t_2070[i, ],0.975)
  k14_r45_SS_975$t_2100[i] = quantile(k14_r45_SS_t_2100[i, ],0.975)
  k14_r85_SS_025$t_2030[i] = quantile(k14_r85_SS_t_2030[i, ],0.025)
  k14_r85_SS_025$t_2050[i] = quantile(k14_r85_SS_t_2050[i, ],0.025)
  k14_r85_SS_025$t_2070[i] = quantile(k14_r85_SS_t_2070[i, ],0.025)
  k14_r85_SS_025$t_2100[i] = quantile(k14_r85_SS_t_2100[i, ],0.025)
  k14_r85_SS_975$t_2030[i] = quantile(k14_r85_SS_t_2030[i, ],0.975)
  k14_r85_SS_975$t_2050[i] = quantile(k14_r85_SS_t_2050[i, ],0.975)
  k14_r85_SS_975$t_2070[i] = quantile(k14_r85_SS_t_2070[i, ],0.975)
  k14_r85_SS_975$t_2100[i] = quantile(k14_r85_SS_t_2100[i, ],0.975)
  # Kopp et al. 2017
  k17_DP16_SEW_r26_SS_025$t_2030[i] = quantile(k17_DP16_SEW_r26_SS_t_2030[i, ],0.025)
  k17_DP16_SEW_r26_SS_025$t_2050[i] = quantile(k17_DP16_SEW_r26_SS_t_2050[i, ],0.025)
  k17_DP16_SEW_r26_SS_025$t_2070[i] = quantile(k17_DP16_SEW_r26_SS_t_2070[i, ],0.025)
  k17_DP16_SEW_r26_SS_025$t_2100[i] = quantile(k17_DP16_SEW_r26_SS_t_2100[i, ],0.025)
  k17_DP16_SEW_r26_SS_975$t_2030[i] = quantile(k17_DP16_SEW_r26_SS_t_2030[i, ],0.975)
  k17_DP16_SEW_r26_SS_975$t_2050[i] = quantile(k17_DP16_SEW_r26_SS_t_2050[i, ],0.975)
  k17_DP16_SEW_r26_SS_975$t_2070[i] = quantile(k17_DP16_SEW_r26_SS_t_2070[i, ],0.975)
  k17_DP16_SEW_r26_SS_975$t_2100[i] = quantile(k17_DP16_SEW_r26_SS_t_2100[i, ],0.975)
  k17_DP16_SEW_r45_SS_025$t_2030[i] = quantile(k17_DP16_SEW_r45_SS_t_2030[i, ],0.025)
  k17_DP16_SEW_r45_SS_025$t_2050[i] = quantile(k17_DP16_SEW_r45_SS_t_2050[i, ],0.025)
  k17_DP16_SEW_r45_SS_025$t_2070[i] = quantile(k17_DP16_SEW_r45_SS_t_2070[i, ],0.025)
  k17_DP16_SEW_r45_SS_025$t_2100[i] = quantile(k17_DP16_SEW_r45_SS_t_2100[i, ],0.025)
  k17_DP16_SEW_r45_SS_975$t_2030[i] = quantile(k17_DP16_SEW_r45_SS_t_2030[i, ],0.975)
  k17_DP16_SEW_r45_SS_975$t_2050[i] = quantile(k17_DP16_SEW_r45_SS_t_2050[i, ],0.975)
  k17_DP16_SEW_r45_SS_975$t_2070[i] = quantile(k17_DP16_SEW_r45_SS_t_2070[i, ],0.975)
  k17_DP16_SEW_r45_SS_975$t_2100[i] = quantile(k17_DP16_SEW_r45_SS_t_2100[i, ],0.975)
  k17_DP16_SEW_r85_SS_025$t_2030[i] = quantile(k17_DP16_SEW_r85_SS_t_2030[i, ],0.025)
  k17_DP16_SEW_r85_SS_025$t_2050[i] = quantile(k17_DP16_SEW_r85_SS_t_2050[i, ],0.025)
  k17_DP16_SEW_r85_SS_025$t_2070[i] = quantile(k17_DP16_SEW_r85_SS_t_2070[i, ],0.025)
  k17_DP16_SEW_r85_SS_025$t_2100[i] = quantile(k17_DP16_SEW_r85_SS_t_2100[i, ],0.025)
  k17_DP16_SEW_r85_SS_975$t_2030[i] = quantile(k17_DP16_SEW_r85_SS_t_2030[i, ],0.975)
  k17_DP16_SEW_r85_SS_975$t_2050[i] = quantile(k17_DP16_SEW_r85_SS_t_2050[i, ],0.975)
  k17_DP16_SEW_r85_SS_975$t_2070[i] = quantile(k17_DP16_SEW_r85_SS_t_2070[i, ],0.975)
  k17_DP16_SEW_r85_SS_975$t_2100[i] = quantile(k17_DP16_SEW_r85_SS_t_2100[i, ],0.975)
  # Rasmussen et al. 2018
  Ras18_SEW_1p5deg_SS_025$t_2030[i] = quantile(Ras18_SEW_1p5deg_SS_t_2030[i, ],0.025)
  Ras18_SEW_1p5deg_SS_025$t_2050[i] = quantile(Ras18_SEW_1p5deg_SS_t_2050[i, ],0.025)
  Ras18_SEW_1p5deg_SS_025$t_2070[i] = quantile(Ras18_SEW_1p5deg_SS_t_2070[i, ],0.025)
  Ras18_SEW_1p5deg_SS_025$t_2100[i] = quantile(Ras18_SEW_1p5deg_SS_t_2100[i, ],0.025)
  Ras18_SEW_1p5deg_SS_975$t_2030[i] = quantile(Ras18_SEW_1p5deg_SS_t_2030[i, ],0.975)
  Ras18_SEW_1p5deg_SS_975$t_2050[i] = quantile(Ras18_SEW_1p5deg_SS_t_2050[i, ],0.975)
  Ras18_SEW_1p5deg_SS_975$t_2070[i] = quantile(Ras18_SEW_1p5deg_SS_t_2070[i, ],0.975)
  Ras18_SEW_1p5deg_SS_975$t_2100[i] = quantile(Ras18_SEW_1p5deg_SS_t_2100[i, ],0.975)
  Ras18_SEW_2p0deg_SS_025$t_2030[i] = quantile(Ras18_SEW_2p0deg_SS_t_2030[i, ],0.025)
  Ras18_SEW_2p0deg_SS_025$t_2050[i] = quantile(Ras18_SEW_2p0deg_SS_t_2050[i, ],0.025)
  Ras18_SEW_2p0deg_SS_025$t_2070[i] = quantile(Ras18_SEW_2p0deg_SS_t_2070[i, ],0.025)
  Ras18_SEW_2p0deg_SS_025$t_2100[i] = quantile(Ras18_SEW_2p0deg_SS_t_2100[i, ],0.025)
  Ras18_SEW_2p0deg_SS_975$t_2030[i] = quantile(Ras18_SEW_2p0deg_SS_t_2030[i, ],0.975)
  Ras18_SEW_2p0deg_SS_975$t_2050[i] = quantile(Ras18_SEW_2p0deg_SS_t_2050[i, ],0.975)
  Ras18_SEW_2p0deg_SS_975$t_2070[i] = quantile(Ras18_SEW_2p0deg_SS_t_2070[i, ],0.975)
  Ras18_SEW_2p0deg_SS_975$t_2100[i] = quantile(Ras18_SEW_2p0deg_SS_t_2100[i, ],0.975)
  Ras18_SEW_2p5deg_SS_025$t_2030[i] = quantile(Ras18_SEW_2p5deg_SS_t_2030[i, ],0.025)
  Ras18_SEW_2p5deg_SS_025$t_2050[i] = quantile(Ras18_SEW_2p5deg_SS_t_2050[i, ],0.025)
  Ras18_SEW_2p5deg_SS_025$t_2070[i] = quantile(Ras18_SEW_2p5deg_SS_t_2070[i, ],0.025)
  Ras18_SEW_2p5deg_SS_025$t_2100[i] = quantile(Ras18_SEW_2p5deg_SS_t_2100[i, ],0.025)
  Ras18_SEW_2p5deg_SS_975$t_2030[i] = quantile(Ras18_SEW_2p5deg_SS_t_2030[i, ],0.975)
  Ras18_SEW_2p5deg_SS_975$t_2050[i] = quantile(Ras18_SEW_2p5deg_SS_t_2050[i, ],0.975)
  Ras18_SEW_2p5deg_SS_975$t_2070[i] = quantile(Ras18_SEW_2p5deg_SS_t_2070[i, ],0.975)
  Ras18_SEW_2p5deg_SS_975$t_2100[i] = quantile(Ras18_SEW_2p5deg_SS_t_2100[i, ],0.975)
}

# Add stationary storm surge to Sweet et al. 2017
sweet17_03_SS_X2030 = sweet17_03_SS_X2050 = sweet17_03_SS_X2070 = sweet17_03_SS_X2100 = 
  sweet17_05_SS_X2030 = sweet17_05_SS_X2050 = sweet17_05_SS_X2070 = sweet17_05_SS_X2100 =
  sweet17_10_SS_X2030 = sweet17_10_SS_X2050 = sweet17_10_SS_X2070 = sweet17_10_SS_X2100 =
  sweet17_15_SS_X2030 = sweet17_15_SS_X2050 = sweet17_15_SS_X2070 = sweet17_15_SS_X2100 =
  sweet17_20_SS_X2030 = sweet17_20_SS_X2050 = sweet17_20_SS_X2070 = sweet17_20_SS_X2100 =
  sweet17_25_SS_X2030 = sweet17_25_SS_X2050 = sweet17_25_SS_X2070 = sweet17_25_SS_X2100 = mat.or.vec(nrow(stationary_sweet17_SEW), ncol(stationary_sweet17_SEW))
for(i in 1:ncol(stationary_sweet17_SEW)){
  sweet17_03_SS_X2030[ ,i] = sweet17_03$X2030[i] + stationary_sweet17_SEW[ ,i]
  sweet17_03_SS_X2050[ ,i] = sweet17_03$X2050[i] + stationary_sweet17_SEW[ ,i]
  sweet17_03_SS_X2070[ ,i] = sweet17_03$X2070[i] + stationary_sweet17_SEW[ ,i]
  sweet17_03_SS_X2100[ ,i] = sweet17_03$X2100[i] + stationary_sweet17_SEW[ ,i]
  sweet17_05_SS_X2030[ ,i] = sweet17_05$X2030[i] + stationary_sweet17_SEW[ ,i]
  sweet17_05_SS_X2050[ ,i] = sweet17_05$X2050[i] + stationary_sweet17_SEW[ ,i]
  sweet17_05_SS_X2070[ ,i] = sweet17_05$X2070[i] + stationary_sweet17_SEW[ ,i]
  sweet17_05_SS_X2100[ ,i] = sweet17_05$X2100[i] + stationary_sweet17_SEW[ ,i]
  sweet17_10_SS_X2030[ ,i] = sweet17_10$X2030[i] + stationary_sweet17_SEW[ ,i]
  sweet17_10_SS_X2050[ ,i] = sweet17_10$X2050[i] + stationary_sweet17_SEW[ ,i]
  sweet17_10_SS_X2070[ ,i] = sweet17_10$X2070[i] + stationary_sweet17_SEW[ ,i]
  sweet17_10_SS_X2100[ ,i] = sweet17_10$X2100[i] + stationary_sweet17_SEW[ ,i]
  sweet17_15_SS_X2030[ ,i] = sweet17_15$X2030[i] + stationary_sweet17_SEW[ ,i]
  sweet17_15_SS_X2050[ ,i] = sweet17_15$X2050[i] + stationary_sweet17_SEW[ ,i]
  sweet17_15_SS_X2070[ ,i] = sweet17_15$X2070[i] + stationary_sweet17_SEW[ ,i]
  sweet17_15_SS_X2100[ ,i] = sweet17_15$X2100[i] + stationary_sweet17_SEW[ ,i]
  sweet17_20_SS_X2030[ ,i] = sweet17_20$X2030[i] + stationary_sweet17_SEW[ ,i]
  sweet17_20_SS_X2050[ ,i] = sweet17_20$X2050[i] + stationary_sweet17_SEW[ ,i]
  sweet17_20_SS_X2070[ ,i] = sweet17_20$X2070[i] + stationary_sweet17_SEW[ ,i]
  sweet17_20_SS_X2100[ ,i] = sweet17_20$X2100[i] + stationary_sweet17_SEW[ ,i]
  sweet17_25_SS_X2030[ ,i] = sweet17_25$X2030[i] + stationary_sweet17_SEW[ ,i]
  sweet17_25_SS_X2050[ ,i] = sweet17_25$X2050[i] + stationary_sweet17_SEW[ ,i]
  sweet17_25_SS_X2070[ ,i] = sweet17_25$X2070[i] + stationary_sweet17_SEW[ ,i]
  sweet17_25_SS_X2100[ ,i] = sweet17_25$X2100[i] + stationary_sweet17_SEW[ ,i]
}

sweet17_03_SS_025 = sweet17_03_SS_975 = sweet17_05_SS_025 = sweet17_05_SS_975 = sweet17_10_SS_025 = sweet17_10_SS_975 = 
  sweet17_15_SS_025 = sweet17_15_SS_975 = sweet17_20_SS_025 = sweet17_20_SS_975 = sweet17_25_SS_025 = sweet17_25_SS_975 =
  data.frame(X2030 = 1:nrow(sweet17_25_SS_X2100), X2050 = 1:nrow(sweet17_25_SS_X2100), X2070 = 1:nrow(sweet17_25_SS_X2100), X2100 = 1:nrow(sweet17_25_SS_X2100))
# Estimate the 97.5 and 2.5% confidence intervals 
for(i in 1:nrow(sweet17_25_SS_X2100)){
  sweet17_03_SS_025$X2030[i] = quantile(sweet17_03_SS_X2030[i, ],0.025, na.rm = TRUE)
  sweet17_03_SS_025$X2050[i] = quantile(sweet17_03_SS_X2050[i, ],0.025, na.rm = TRUE)
  sweet17_03_SS_025$X2070[i] = quantile(sweet17_03_SS_X2070[i, ],0.025, na.rm = TRUE)
  sweet17_03_SS_025$X2100[i] = quantile(sweet17_03_SS_X2100[i, ],0.025, na.rm = TRUE)
  sweet17_03_SS_975$X2030[i] = quantile(sweet17_03_SS_X2030[i, ],0.975, na.rm = TRUE)
  sweet17_03_SS_975$X2050[i] = quantile(sweet17_03_SS_X2050[i, ],0.975, na.rm = TRUE)
  sweet17_03_SS_975$X2070[i] = quantile(sweet17_03_SS_X2070[i, ],0.975, na.rm = TRUE)
  sweet17_03_SS_975$X2100[i] = quantile(sweet17_03_SS_X2100[i, ],0.975, na.rm = TRUE)
  sweet17_05_SS_025$X2030[i] = quantile(sweet17_05_SS_X2030[i, ],0.025, na.rm = TRUE)
  sweet17_05_SS_025$X2050[i] = quantile(sweet17_05_SS_X2050[i, ],0.025, na.rm = TRUE)
  sweet17_05_SS_025$X2070[i] = quantile(sweet17_05_SS_X2070[i, ],0.025, na.rm = TRUE)
  sweet17_05_SS_025$X2100[i] = quantile(sweet17_05_SS_X2100[i, ],0.025, na.rm = TRUE)
  sweet17_05_SS_975$X2030[i] = quantile(sweet17_05_SS_X2030[i, ],0.975, na.rm = TRUE)
  sweet17_05_SS_975$X2050[i] = quantile(sweet17_05_SS_X2050[i, ],0.975, na.rm = TRUE)
  sweet17_05_SS_975$X2070[i] = quantile(sweet17_05_SS_X2070[i, ],0.975, na.rm = TRUE)
  sweet17_05_SS_975$X2100[i] = quantile(sweet17_05_SS_X2100[i, ],0.975, na.rm = TRUE)
  sweet17_10_SS_025$X2030[i] = quantile(sweet17_10_SS_X2030[i, ],0.025, na.rm = TRUE)
  sweet17_10_SS_025$X2050[i] = quantile(sweet17_10_SS_X2050[i, ],0.025, na.rm = TRUE)
  sweet17_10_SS_025$X2070[i] = quantile(sweet17_10_SS_X2070[i, ],0.025, na.rm = TRUE)
  sweet17_10_SS_025$X2100[i] = quantile(sweet17_10_SS_X2100[i, ],0.025, na.rm = TRUE)
  sweet17_10_SS_975$X2030[i] = quantile(sweet17_10_SS_X2030[i, ],0.975, na.rm = TRUE)
  sweet17_10_SS_975$X2050[i] = quantile(sweet17_10_SS_X2050[i, ],0.975, na.rm = TRUE)
  sweet17_10_SS_975$X2070[i] = quantile(sweet17_10_SS_X2070[i, ],0.975, na.rm = TRUE)
  sweet17_10_SS_975$X2100[i] = quantile(sweet17_10_SS_X2100[i, ],0.975, na.rm = TRUE)
  sweet17_15_SS_025$X2030[i] = quantile(sweet17_15_SS_X2030[i, ],0.025, na.rm = TRUE)
  sweet17_15_SS_025$X2050[i] = quantile(sweet17_15_SS_X2050[i, ],0.025, na.rm = TRUE)
  sweet17_15_SS_025$X2070[i] = quantile(sweet17_15_SS_X2070[i, ],0.025, na.rm = TRUE)
  sweet17_15_SS_025$X2100[i] = quantile(sweet17_15_SS_X2100[i, ],0.025, na.rm = TRUE)
  sweet17_15_SS_975$X2030[i] = quantile(sweet17_15_SS_X2030[i, ],0.975, na.rm = TRUE)
  sweet17_15_SS_975$X2050[i] = quantile(sweet17_15_SS_X2050[i, ],0.975, na.rm = TRUE)
  sweet17_15_SS_975$X2070[i] = quantile(sweet17_15_SS_X2070[i, ],0.975, na.rm = TRUE)
  sweet17_15_SS_975$X2100[i] = quantile(sweet17_15_SS_X2100[i, ],0.975, na.rm = TRUE)
  sweet17_20_SS_025$X2030[i] = quantile(sweet17_20_SS_X2030[i, ],0.025, na.rm = TRUE)
  sweet17_20_SS_025$X2050[i] = quantile(sweet17_20_SS_X2050[i, ],0.025, na.rm = TRUE)
  sweet17_20_SS_025$X2070[i] = quantile(sweet17_20_SS_X2070[i, ],0.025, na.rm = TRUE)
  sweet17_20_SS_025$X2100[i] = quantile(sweet17_20_SS_X2100[i, ],0.025, na.rm = TRUE)
  sweet17_20_SS_975$X2030[i] = quantile(sweet17_20_SS_X2030[i, ],0.975, na.rm = TRUE)
  sweet17_20_SS_975$X2050[i] = quantile(sweet17_20_SS_X2050[i, ],0.975, na.rm = TRUE)
  sweet17_20_SS_975$X2070[i] = quantile(sweet17_20_SS_X2070[i, ],0.975, na.rm = TRUE)
  sweet17_20_SS_975$X2100[i] = quantile(sweet17_20_SS_X2100[i, ],0.975, na.rm = TRUE)
  sweet17_25_SS_025$X2030[i] = quantile(sweet17_25_SS_X2030[i, ],0.025, na.rm = TRUE)
  sweet17_25_SS_025$X2050[i] = quantile(sweet17_25_SS_X2050[i, ],0.025, na.rm = TRUE)
  sweet17_25_SS_025$X2070[i] = quantile(sweet17_25_SS_X2070[i, ],0.025, na.rm = TRUE)
  sweet17_25_SS_025$X2100[i] = quantile(sweet17_25_SS_X2100[i, ],0.025, na.rm = TRUE)
  sweet17_25_SS_975$X2030[i] = quantile(sweet17_25_SS_X2030[i, ],0.975, na.rm = TRUE)
  sweet17_25_SS_975$X2050[i] = quantile(sweet17_25_SS_X2050[i, ],0.975, na.rm = TRUE)
  sweet17_25_SS_975$X2070[i] = quantile(sweet17_25_SS_X2070[i, ],0.975, na.rm = TRUE)
  sweet17_25_SS_975$X2100[i] = quantile(sweet17_25_SS_X2100[i, ],0.975, na.rm = TRUE)
}

# Add stationary storm surge to Wong and Keller 2017
bfd_r26_SS_t_2030 = bfd_r26_SS_t_2050 = bfd_r26_SS_t_2070 = bfd_r26_SS_t_2100 = 
  bfd_r45_SS_t_2030 = bfd_r45_SS_t_2050 = bfd_r45_SS_t_2070 = bfd_r45_SS_t_2100 = 
  bfd_r85_SS_t_2030 = bfd_r85_SS_t_2050 = bfd_r85_SS_t_2070 = bfd_r85_SS_t_2100 = 
  NOfd_r26_SS_t_2030 = NOfd_r26_SS_t_2050 = NOfd_r26_SS_t_2070 = NOfd_r26_SS_t_2100 = 
  NOfd_r45_SS_t_2030 = NOfd_r45_SS_t_2050 = NOfd_r45_SS_t_2070 = NOfd_r45_SS_t_2100 = 
  NOfd_r85_SS_t_2030 = NOfd_r85_SS_t_2050 = NOfd_r85_SS_t_2070 = NOfd_r85_SS_t_2100 = 
  bfd_r26_SS_2065 = bfd_r45_SS_2065 = bfd_r85_SS_2065 = NOfd_r26_SS_2065 = 
  NOfd_r45_SS_2065 = NOfd_r85_SS_2065 = mat.or.vec(nrow(stationary_brick), ncol(stationary_brick))
for(i in 1:ncol(stationary_brick)){
  # Add stationary storm surge to Wong and Keller 2017 fast dynamics
  bfd_r26_SS_t_2030[ ,i] = brickfd_rcp26$t_2030[i] + stationary_brick[ ,i]
  bfd_r26_SS_t_2050[ ,i] = brickfd_rcp26$t_2050[i] + stationary_brick[ ,i]
  bfd_r26_SS_t_2070[ ,i] = brickfd_rcp26$t_2070[i] + stationary_brick[ ,i]
  bfd_r26_SS_t_2100[ ,i] = brickfd_rcp26$t_2100[i] + stationary_brick[ ,i]
  bfd_r45_SS_t_2030[ ,i] = brickfd_rcp45$t_2030[i] + stationary_brick[ ,i]
  bfd_r45_SS_t_2050[ ,i] = brickfd_rcp45$t_2050[i] + stationary_brick[ ,i]
  bfd_r45_SS_t_2070[ ,i] = brickfd_rcp45$t_2070[i] + stationary_brick[ ,i]
  bfd_r45_SS_t_2100[ ,i] = brickfd_rcp45$t_2100[i] + stationary_brick[ ,i]
  bfd_r85_SS_t_2030[ ,i] = brickfd_rcp85$t_2030[i] + stationary_brick[ ,i]
  bfd_r85_SS_t_2050[ ,i] = brickfd_rcp85$t_2050[i] + stationary_brick[ ,i]
  bfd_r85_SS_t_2070[ ,i] = brickfd_rcp85$t_2070[i] + stationary_brick[ ,i]
  bfd_r85_SS_t_2100[ ,i] = brickfd_rcp85$t_2100[i] + stationary_brick[ ,i]
  bfd_r26_SS_2065[ ,i] = fd_r26_sub_2065[i] + stationary_brick[ ,i]
  bfd_r45_SS_2065[ ,i] = fd_r45_sub_2065[i] + stationary_brick[ ,i]
  bfd_r85_SS_2065[ ,i] = fd_r85_sub_2065[i] + stationary_brick[ ,i]
  # Add stationary storm surge to Wong and Keller 2017 NO fast dynamics
  NOfd_r26_SS_t_2030[ ,i] = NO_fdft_rcp26$t_2030[i] + stationary_brick[ ,i]
  NOfd_r26_SS_t_2050[ ,i] = NO_fdft_rcp26$t_2050[i] + stationary_brick[ ,i]
  NOfd_r26_SS_t_2070[ ,i] = NO_fdft_rcp26$t_2070[i] + stationary_brick[ ,i]
  NOfd_r26_SS_t_2100[ ,i] = NO_fdft_rcp26$t_2100[i] + stationary_brick[ ,i]
  NOfd_r45_SS_t_2030[ ,i] = NO_fdft_rcp45$t_2030[i] + stationary_brick[ ,i]
  NOfd_r45_SS_t_2050[ ,i] = NO_fdft_rcp45$t_2050[i] + stationary_brick[ ,i]
  NOfd_r45_SS_t_2070[ ,i] = NO_fdft_rcp45$t_2070[i] + stationary_brick[ ,i]
  NOfd_r45_SS_t_2100[ ,i] = NO_fdft_rcp45$t_2100[i] + stationary_brick[ ,i]
  NOfd_r85_SS_t_2030[ ,i] = NO_fdft_rcp85$t_2030[i] + stationary_brick[ ,i]
  NOfd_r85_SS_t_2050[ ,i] = NO_fdft_rcp85$t_2050[i] + stationary_brick[ ,i]
  NOfd_r85_SS_t_2070[ ,i] = NO_fdft_rcp85$t_2070[i] + stationary_brick[ ,i]
  NOfd_r85_SS_t_2100[ ,i] = NO_fdft_rcp85$t_2100[i] + stationary_brick[ ,i]
  NOfd_r26_SS_2065[ ,i] = Nfd_r26_sub_2065[i] + stationary_brick[ ,i]
  NOfd_r45_SS_2065[ ,i] = Nfd_r45_sub_2065[i] + stationary_brick[ ,i]
  NOfd_r85_SS_2065[ ,i] = Nfd_r85_sub_2065[i] + stationary_brick[ ,i]
}

bfd_r26_SS_025 = bfd_r26_SS_975 = bfd_r45_SS_025 = bfd_r45_SS_975 = bfd_r85_SS_025 = bfd_r85_SS_975 = 
  NOfd_r26_SS_025 = NOfd_r26_SS_975 = NOfd_r45_SS_025 = NOfd_r45_SS_975 = NOfd_r85_SS_025 = NOfd_r85_SS_975 =
  data.frame(t_2030 = 1:nrow(bfd_r26_SS_t_2030), t_2050 = 1:nrow(bfd_r26_SS_t_2030), t_2070 = 1:nrow(bfd_r26_SS_t_2030), t_2100 = 1:nrow(bfd_r26_SS_t_2030))
# Estimate the 97.5 and 2.5% confidence intervals 
for(i in 1:nrow(bfd_r26_SS_t_2030)){
  # Wong and Keller 2017 fast dynamics
  bfd_r26_SS_025$t_2030[i] = quantile(bfd_r26_SS_t_2030[i, ],0.025)
  bfd_r26_SS_025$t_2050[i] = quantile(bfd_r26_SS_t_2050[i, ],0.025)
  bfd_r26_SS_025$t_2070[i] = quantile(bfd_r26_SS_t_2070[i, ],0.025)
  bfd_r26_SS_025$t_2100[i] = quantile(bfd_r26_SS_t_2100[i, ],0.025)
  bfd_r26_SS_975$t_2030[i] = quantile(bfd_r26_SS_t_2030[i, ],0.975)
  bfd_r26_SS_975$t_2050[i] = quantile(bfd_r26_SS_t_2050[i, ],0.975)
  bfd_r26_SS_975$t_2070[i] = quantile(bfd_r26_SS_t_2070[i, ],0.975)
  bfd_r26_SS_975$t_2100[i] = quantile(bfd_r26_SS_t_2100[i, ],0.975)
  bfd_r45_SS_025$t_2030[i] = quantile(bfd_r45_SS_t_2030[i, ],0.025)
  bfd_r45_SS_025$t_2050[i] = quantile(bfd_r45_SS_t_2050[i, ],0.025)
  bfd_r45_SS_025$t_2070[i] = quantile(bfd_r45_SS_t_2070[i, ],0.025)
  bfd_r45_SS_025$t_2100[i] = quantile(bfd_r45_SS_t_2100[i, ],0.025)
  bfd_r45_SS_975$t_2030[i] = quantile(bfd_r45_SS_t_2030[i, ],0.975)
  bfd_r45_SS_975$t_2050[i] = quantile(bfd_r45_SS_t_2050[i, ],0.975)
  bfd_r45_SS_975$t_2070[i] = quantile(bfd_r45_SS_t_2070[i, ],0.975)
  bfd_r45_SS_975$t_2100[i] = quantile(bfd_r45_SS_t_2100[i, ],0.975)
  bfd_r85_SS_025$t_2030[i] = quantile(bfd_r85_SS_t_2030[i, ],0.025)
  bfd_r85_SS_025$t_2050[i] = quantile(bfd_r85_SS_t_2050[i, ],0.025)
  bfd_r85_SS_025$t_2070[i] = quantile(bfd_r85_SS_t_2070[i, ],0.025)
  bfd_r85_SS_025$t_2100[i] = quantile(bfd_r85_SS_t_2100[i, ],0.025)
  bfd_r85_SS_975$t_2030[i] = quantile(bfd_r85_SS_t_2030[i, ],0.975)
  bfd_r85_SS_975$t_2050[i] = quantile(bfd_r85_SS_t_2050[i, ],0.975)
  bfd_r85_SS_975$t_2070[i] = quantile(bfd_r85_SS_t_2070[i, ],0.975)
  bfd_r85_SS_975$t_2100[i] = quantile(bfd_r85_SS_t_2100[i, ],0.975)
  # Wong and Keller 2017 NO fast dynamics
  NOfd_r26_SS_025$t_2030[i] = quantile(NOfd_r26_SS_t_2030[i, ],0.025)
  NOfd_r26_SS_025$t_2050[i] = quantile(NOfd_r26_SS_t_2050[i, ],0.025)
  NOfd_r26_SS_025$t_2070[i] = quantile(NOfd_r26_SS_t_2070[i, ],0.025)
  NOfd_r26_SS_025$t_2100[i] = quantile(NOfd_r26_SS_t_2100[i, ],0.025)
  NOfd_r26_SS_975$t_2030[i] = quantile(NOfd_r26_SS_t_2030[i, ],0.975)
  NOfd_r26_SS_975$t_2050[i] = quantile(NOfd_r26_SS_t_2050[i, ],0.975)
  NOfd_r26_SS_975$t_2070[i] = quantile(NOfd_r26_SS_t_2070[i, ],0.975)
  NOfd_r26_SS_975$t_2100[i] = quantile(NOfd_r26_SS_t_2100[i, ],0.975)
  NOfd_r45_SS_025$t_2030[i] = quantile(NOfd_r45_SS_t_2030[i, ],0.025)
  NOfd_r45_SS_025$t_2050[i] = quantile(NOfd_r45_SS_t_2050[i, ],0.025)
  NOfd_r45_SS_025$t_2070[i] = quantile(NOfd_r45_SS_t_2070[i, ],0.025)
  NOfd_r45_SS_025$t_2100[i] = quantile(NOfd_r45_SS_t_2100[i, ],0.025)
  NOfd_r45_SS_975$t_2030[i] = quantile(NOfd_r45_SS_t_2030[i, ],0.975)
  NOfd_r45_SS_975$t_2050[i] = quantile(NOfd_r45_SS_t_2050[i, ],0.975)
  NOfd_r45_SS_975$t_2070[i] = quantile(NOfd_r45_SS_t_2070[i, ],0.975)
  NOfd_r45_SS_975$t_2100[i] = quantile(NOfd_r45_SS_t_2100[i, ],0.975)
  NOfd_r85_SS_025$t_2030[i] = quantile(NOfd_r85_SS_t_2030[i, ],0.025)
  NOfd_r85_SS_025$t_2050[i] = quantile(NOfd_r85_SS_t_2050[i, ],0.025)
  NOfd_r85_SS_025$t_2070[i] = quantile(NOfd_r85_SS_t_2070[i, ],0.025)
  NOfd_r85_SS_025$t_2100[i] = quantile(NOfd_r85_SS_t_2100[i, ],0.025)
  NOfd_r85_SS_975$t_2030[i] = quantile(NOfd_r85_SS_t_2030[i, ],0.975)
  NOfd_r85_SS_975$t_2050[i] = quantile(NOfd_r85_SS_t_2050[i, ],0.975)
  NOfd_r85_SS_975$t_2070[i] = quantile(NOfd_r85_SS_t_2070[i, ],0.975)
  NOfd_r85_SS_975$t_2100[i] = quantile(NOfd_r85_SS_t_2100[i, ],0.975)
}
#--------------------------- Calculate new 100-yr return levels from combined storm surge and SLR --------------------------
source("Helper_scripts/inv_sf_prob.r")

run.srikrishnan.stat = FALSE # Change to true if to run the analysis

if(run.srikrishnan.stat){
# Data from Vivek's stationary storm surge values
# BRICK fast dynamics r26
small.num = round(max(bfd_r26_SS_2065[1,])+0.01, 2)
end.num = round(mean(fd_r26_sub_2065) + gev_stat_50[which.min(abs(1/probs[order(1/probs)] - 100))], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > bfd_r26_SS_2065[1,]))
print(all(num.range[100] < bfd_r26_SS_2065[1000,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(length(bfd_r26_SS_2065[1,]), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(bfd_r26_SS_2065, 2, inv.sf, val= num.range[i])
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# max.returnL_fd26 <- which.max(new.returnLevel)
max.returnL_fd26 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[max.returnL_fd26]) #The probability should be 0.01
bfd_r26_SS_2065_100 = num.range[max.returnL_fd26]

# BRICK fast dynamics r45
small.num = round(max(bfd_r45_SS_2065[1,])+0.01, 2)
end.num = round(mean(fd_r45_sub_2065) + gev_stat_50[which.min(abs(1/probs[order(1/probs)] - 100))], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > bfd_r45_SS_2065[1,]))
print(all(num.range[100] < bfd_r45_SS_2065[1000,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(length(bfd_r45_SS_2065[1,]), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(bfd_r45_SS_2065, 2, inv.sf, val= num.range[i])
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# max.returnL_fd45 <- which.max(new.returnLevel)
max.returnL_fd45 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[max.returnL_fd45]) #The probability should be 0.01
bfd_r45_SS_2065_100 = num.range[max.returnL_fd45]

# BRICK fast dynamics r85
small.num = round(max(bfd_r85_SS_2065[1,])+0.01, 2)
end.num = round(mean(fd_r85_sub_2065) + gev_stat_50[which.min(abs(1/probs[order(1/probs)] - 100))], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > bfd_r85_SS_2065[1,]))
print(all(num.range[100] < bfd_r85_SS_2065[1000,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(length(bfd_r85_SS_2065[1,]), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(bfd_r85_SS_2065, 2, inv.sf, val= num.range[i])
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# max.returnL_fd85 <- which.max(new.returnLevel)
max.returnL_fd85 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[max.returnL_fd85]) #The probability should be 0.01
bfd_r85_SS_2065_100 = num.range[max.returnL_fd85]

# BRICK NO fast dynamics r26
small.num = round(max(NOfd_r26_SS_2065[1,])+0.01, 2)
end.num = round(mean(Nfd_r26_sub_2065) + gev_stat_50[which.min(abs(1/probs[order(1/probs)] - 100))], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > NOfd_r26_SS_2065[1,]))
print(all(num.range[100] < NOfd_r26_SS_2065[1000,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(length(NOfd_r26_SS_2065[1,]), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(NOfd_r26_SS_2065, 2, inv.sf, val= num.range[i])
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# max.returnL_NOfd26 <- which.max(new.returnLevel)
max.returnL_NOfd26 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[max.returnL_NOfd26]) #The probability should be 0.01
NOfd_r26_SS_2065_100 = num.range[max.returnL_NOfd26]

# BRICK NO fast dynamics r45
small.num = round(max(NOfd_r45_SS_2065[1,])+0.01, 2)
end.num = round(mean(Nfd_r45_sub_2065) + gev_stat_50[which.min(abs(1/probs[order(1/probs)] - 100))], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > NOfd_r45_SS_2065[1,]))
print(all(num.range[100] < NOfd_r45_SS_2065[1000,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(length(NOfd_r45_SS_2065[1,]), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(NOfd_r45_SS_2065, 2, inv.sf, val= num.range[i])
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# max.returnL_NOfd45 <- which.max(new.returnLevel)
max.returnL_NOfd45 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[max.returnL_NOfd45]) #The probability should be 0.01
NOfd_r45_SS_2065_100 = num.range[max.returnL_NOfd45]

# BRICK NO fast dynamics r85
small.num = round(max(NOfd_r85_SS_2065[1,])+0.01, 2)
end.num = round(mean(Nfd_r85_sub_2065) + gev_stat_50[which.min(abs(1/probs[order(1/probs)] - 100))], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > NOfd_r85_SS_2065[1,]))
print(all(num.range[100] < NOfd_r85_SS_2065[1000,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(length(NOfd_r85_SS_2065[1,]), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(NOfd_r85_SS_2065, 2, inv.sf, val= num.range[i])
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# max.returnL_NOfd85 <- which.max(new.returnLevel)
max.returnL_NOfd85 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[max.returnL_NOfd85]) #The probability should be 0.01
NOfd_r85_SS_2065_100 = num.range[max.returnL_NOfd85]
}
# -------------------------------------------------------------------------
# Data from Wong 2018 nonstationary BMA storm surge values rcp26 fast dynamics
small.num = round(max(bma_SLR_fd_r26[1,])+0.01, 2)
end.num = round(mean(bma_subset_fd_r26) + wong_18$rl[5], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > bma_SLR_fd_r26[1,]))
print(all(num.range[100] < bma_SLR_fd_r26[18,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(ncol(bma_SLR_fd_r26), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(bma_SLR_fd_r26, 2, inv.sf, val= num.range[i], prob = 1/wong18_rl_years, use.prob = TRUE) # basically apply is doing a for loop of bma_SLR_fd_r26[,i]
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# bma_max.returnL_fd26 <- which.max(new.returnLevel)
bma_max.returnL_fd26 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[bma_max.returnL_fd26]) #The probability should be 0.01
bma_fd_r26_SS_2065_100 = num.range[bma_max.returnL_fd26]

# Data from Wong 2018 nonstationary BMA storm surge values rcp45 fast dynamics
small.num = round(max(bma_SLR_fd_r45[1,])+0.01, 2)
end.num = round(mean(bma_subset_fd_r45) + wong_18$rl[5], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > bma_SLR_fd_r45[1,]))
print(all(num.range[100] < bma_SLR_fd_r45[18,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(ncol(bma_SLR_fd_r45), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(bma_SLR_fd_r45, 2, inv.sf, val= num.range[i], prob = 1/wong18_rl_years, use.prob = TRUE) # basically apply is doing a for loop of bma_SLR_fd_r26[,i]
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# bma_max.returnL_fd45 <- which.max(new.returnLevel)
bma_max.returnL_fd45 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[bma_max.returnL_fd45]) #The probability should be 0.01
bma_fd_r45_SS_2065_100 = num.range[bma_max.returnL_fd45]

# Data from Wong 2018 nonstationary BMA storm surge values rcp85 fast dynamics
small.num = round(max(bma_SLR_fd_r85[1,])+0.01, 2)
end.num = round(mean(bma_subset_fd_r85) + wong_18$rl[5], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > bma_SLR_fd_r85[1,]))
print(all(num.range[100] < bma_SLR_fd_r85[18,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(ncol(bma_SLR_fd_r85), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(bma_SLR_fd_r85, 2, inv.sf, val= num.range[i], prob = 1/wong18_rl_years, use.prob = TRUE) # basically apply is doing a for loop of bma_SLR_fd_r26[,i]
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# bma_max.returnL_fd85 <- which.max(new.returnLevel)
bma_max.returnL_fd85 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[bma_max.returnL_fd85]) #The probability should be 0.01
bma_fd_r85_SS_2065_100 = num.range[bma_max.returnL_fd85]

# Data from Wong 2018 nonstationary BMA storm surge values rcp26 NO fast dynamics
small.num = round(max(bma_SLR_NOfd_r26[1,])+0.01, 2)
end.num = round(mean(bma_subset_Nfd_r26) + wong_18$rl[5], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > bma_SLR_NOfd_r26[1,]))
print(all(num.range[100] < bma_SLR_NOfd_r26[18,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(ncol(bma_SLR_NOfd_r26), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(bma_SLR_NOfd_r26, 2, inv.sf, val= num.range[i], prob = 1/wong18_rl_years, use.prob = TRUE) # basically apply is doing a for loop of bma_SLR_fd_r26[,i]
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# bma_max.returnL_NOfd26 <- which.max(new.returnLevel)
bma_max.returnL_NOfd26 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[bma_max.returnL_NOfd26]) #The probability should be 0.01
bma_NOfd_r26_SS_2065_100 = num.range[bma_max.returnL_NOfd26]

# Data from Wong 2018 nonstationary BMA storm surge values rcp45 NO fast dynamics
small.num = round(max(bma_SLR_NOfd_r45[1,])+0.01, 2)
end.num = round(mean(bma_subset_Nfd_r45) + wong_18$rl[5], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > bma_SLR_NOfd_r45[1,]))
print(all(num.range[100] < bma_SLR_NOfd_r45[18,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(ncol(bma_SLR_NOfd_r45), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(bma_SLR_NOfd_r45, 2, inv.sf, val= num.range[i], prob = 1/wong18_rl_years, use.prob = TRUE) # basically apply is doing a for loop of bma_SLR_fd_r26[,i]
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# bma_max.returnL_NOfd45 <- which.max(new.returnLevel)
bma_max.returnL_NOfd45 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[bma_max.returnL_NOfd45]) #The probability should be 0.01
bma_NOfd_r45_SS_2065_100 = num.range[bma_max.returnL_NOfd45]

# Data from Wong 2018 nonstationary BMA storm surge values rcp85 NO fast dynamics
small.num = round(max(bma_SLR_NOfd_r85[1,])+0.01, 2)
end.num = round(mean(bma_subset_Nfd_r85) + wong_18$rl[5], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > bma_SLR_NOfd_r85[1,]))
print(all(num.range[100] < bma_SLR_NOfd_r85[18,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(ncol(bma_SLR_NOfd_r85), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(bma_SLR_NOfd_r85, 2, inv.sf, val= num.range[i], prob = 1/wong18_rl_years, use.prob = TRUE) # basically apply is doing a for loop of bma_SLR_fd_r26[,i]
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# bma_max.returnL_NOfd85 <- which.max(new.returnLevel)
bma_max.returnL_NOfd85 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[bma_max.returnL_NOfd85]) #The probability should be 0.01
bma_NOfd_r85_SS_2065_100 = num.range[bma_max.returnL_NOfd85]

# -------------------------------------------------------------------------
# Data from Wong 2018 stationary storm surge values rcp26 fast dynamics
small.num = round(max(stat_SLR_fd_r26[1,])+0.01, 2)
end.num = round(mean(stat_subset_fd_r26) + wong_18$rl[1], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > stat_SLR_fd_r26[1,]))
print(all(num.range[100] < stat_SLR_fd_r26[18,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(ncol(stat_SLR_fd_r26), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(stat_SLR_fd_r26, 2, inv.sf, val= num.range[i], prob = 1/wong18_rl_years, use.prob = TRUE) # basically apply is doing a for loop of bma_SLR_fd_r26[,i]
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# stat_max.returnL_fd26 <- which.max(new.returnLevel)
stat_max.returnL_fd26 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[stat_max.returnL_fd26]) #The probability should be 0.01
stat_fd_r26_SS_2065_100 = num.range[stat_max.returnL_fd26]

# Data from Wong 2018 stationary storm surge values rcp45 fast dynamics
small.num = round(max(stat_SLR_fd_r45[1,])+0.01, 2)
end.num = round(mean(stat_subset_fd_r45) + wong_18$rl[1], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > stat_SLR_fd_r45[1,]))
print(all(num.range[100] < stat_SLR_fd_r45[18,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(ncol(stat_SLR_fd_r45), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(stat_SLR_fd_r45, 2, inv.sf, val= num.range[i], prob = 1/wong18_rl_years, use.prob = TRUE) # basically apply is doing a for loop of bma_SLR_fd_r26[,i]
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# stat_max.returnL_fd45 <- which.max(new.returnLevel)
stat_max.returnL_fd45 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[stat_max.returnL_fd45]) #The probability should be 0.01
stat_fd_r45_SS_2065_100 = num.range[stat_max.returnL_fd45]

# Data from Wong 2018 stationary storm surge values rcp85 fast dynamics
small.num = round(max(stat_SLR_fd_r85[1,])+0.01, 2)
end.num = round(mean(stat_subset_fd_r85) + wong_18$rl[1], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > stat_SLR_fd_r85[1,]))
print(all(num.range[100] < stat_SLR_fd_r85[18,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(ncol(stat_SLR_fd_r85), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(stat_SLR_fd_r85, 2, inv.sf, val= num.range[i], prob = 1/wong18_rl_years, use.prob = TRUE) # basically apply is doing a for loop of bma_SLR_fd_r26[,i]
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# stat_max.returnL_fd85 <- which.max(new.returnLevel)
stat_max.returnL_fd85 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[stat_max.returnL_fd85]) #The probability should be 0.01
stat_fd_r85_SS_2065_100 = num.range[stat_max.returnL_fd85]

# Data from Wong 2018 stationary storm surge values rcp26 NO fast dynamics
small.num = round(max(stat_SLR_NOfd_r26[1,])+0.01, 2)
end.num = round(mean(stat_subset_Nfd_r26) + wong_18$rl[1], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > stat_SLR_NOfd_r26[1,]))
print(all(num.range[100] < stat_SLR_NOfd_r26[18,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(ncol(stat_SLR_NOfd_r26), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(stat_SLR_NOfd_r26, 2, inv.sf, val= num.range[i], prob = 1/wong18_rl_years, use.prob = TRUE) # basically apply is doing a for loop of bma_SLR_fd_r26[,i]
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# stat_max.returnL_NOfd26 <- which.max(new.returnLevel)
stat_max.returnL_NOfd26 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[stat_max.returnL_NOfd26]) #The probability should be 0.01
stat_NOfd_r26_SS_2065_100 = num.range[stat_max.returnL_NOfd26]

# Data from Wong 2018 stationary storm surge values rcp45 NO fast dynamics
small.num = round(max(stat_SLR_NOfd_r45[1,])+0.01, 2)
end.num = round(mean(stat_subset_Nfd_r45) + wong_18$rl[1], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > stat_SLR_NOfd_r45[1,]))
print(all(num.range[100] < stat_SLR_NOfd_r45[18,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(ncol(stat_SLR_NOfd_r45), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(stat_SLR_NOfd_r45, 2, inv.sf, val= num.range[i], prob = 1/wong18_rl_years, use.prob = TRUE) # basically apply is doing a for loop of bma_SLR_fd_r26[,i]
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# stat_max.returnL_NOfd45 <- which.max(new.returnLevel)
stat_max.returnL_NOfd45 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[stat_max.returnL_NOfd45]) #The probability should be 0.01
stat_NOfd_r45_SS_2065_100 = num.range[stat_max.returnL_NOfd45]

# Data from Wong 2018 stationary storm surge values rcp85 NO fast dynamics
small.num = round(max(stat_SLR_NOfd_r85[1,])+0.01, 2)
end.num = round(mean(stat_subset_Nfd_r85) + wong_18$rl[1], 2)
num.range1 = seq(small.num, end.num, length.out=20)
num.range2 = seq(end.num+0.1, round(end.num + end.num/2, 2), length.out=80)
num.range = c(num.range1, num.range2)

# Test: The first number in num.range must be larger than the minimum value in each flood frequency curve
# That way all NAs produced have a frequency smaller than 1:1000.
print(all(num.range[1] > stat_SLR_NOfd_r85[1,]))
print(all(num.range[100] < stat_SLR_NOfd_r85[18,]))

# Find the probabilities of the values in the range using all the potential storm surge plus SLR anomalies
new.probs <- mat.or.vec(ncol(stat_SLR_NOfd_r85), length(num.range))
for(i in 1:length(num.range)){
  new.probs[,i] <- apply(stat_SLR_NOfd_r85, 2, inv.sf, val= num.range[i], prob = 1/wong18_rl_years, use.prob = TRUE) # basically apply is doing a for loop of bma_SLR_fd_r26[,i]
}

any(is.na(new.probs[,1])) # should be FALSE
# Set all NAs to 0 since they are smaller than 1:1,000. This gives us a conservative estimate.
new.probs[is.na(new.probs)] <- 0
# Calculate the average
average.uncertainty.probs <- rep(NA, length(num.range))
for(i in 1:length(num.range)){
  average.uncertainty.probs[i] <- mean(new.probs[,i])
}

#Find the new 100-yr probability
# new.returnLevel <- which(average.uncertainty.probs >= 0.01)
# stat_max.returnL_NOfd85 <- which.max(new.returnLevel)
stat_max.returnL_NOfd85 <- which.min(abs(0.01 - average.uncertainty.probs))
print(average.uncertainty.probs[stat_max.returnL_NOfd85]) #The probability should be 0.01
stat_NOfd_r85_SS_2065_100 = num.range[stat_max.returnL_NOfd85]

##==============================================================================
## End
##==============================================================================

