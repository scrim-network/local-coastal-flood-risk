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
# Kopp et al. 2014 Local sea-level rise data at Sewells point tide gauge
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

# Kopp et al. 2014 Local subsidence data at Sewells point tide gauge
# Data is cm above 2000, so 0cm is 2000. This will be used
# to account for subsidence in the Wong and Keller 2017 data.
kopp14_subsid_dat = read.csv("../Data/LSLProj_bkgd_299_rcp26.csv")
kopp14_subsid = convert_cm_to_ft(data.frame(t_2030 = kopp14_subsid_dat$X2030, t_2050 = kopp14_subsid_dat$X2050, 
                                            t_2070 = kopp14_subsid_dat$X2070, t_2100 = kopp14_subsid_dat$X2100))

##=========================== READ KOPP ET AL. 2017 DATA ===================================
# Kopp et al. 2017 Local sea-level rise data at Sewells point tide gauge
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
# Rasmussen et al. 2018 Local sea-level rise data at Sewells point tide gauge
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
# Wong and Keller 2017 Local sea-level rise data with Fast dynamics at Sewells point tide gauge
# Data is m above 2000, so 0 m is 2000. Data is projected with RCP26, 45, 60, and 85.
# The model used is called BRICK, so any reference to BRICK refers to Wong and Keller 2017. LSL (sewells point) with fast dynamics. 
fid1 <- nc_open("../Data/BRICK_SewellsPoint_FastDynamics_08May2017.nc")
lsl_fdyn_rcp26 <- ncvar_get(fid1,"LocalSeaLevel_RCP26")
lsl_fdyn_rcp45 <- ncvar_get(fid1,"LocalSeaLevel_RCP45")
lsl_fdyn_rcp60 <- ncvar_get(fid1,"LocalSeaLevel_RCP60")
lsl_fdyn_rcp85 <- ncvar_get(fid1,"LocalSeaLevel_RCP85")
year_proj <- ncvar_get(fid1,"time_proj")
nc_close(fid1)

# Wong and Keller 2017 Local sea-level rise data with NO Fast dynamics
fid1 <- nc_open("../Data/BRICK_NOfastDynamics_SP_08May2017.nc")
NO_fdyn_rcp26 <- ncvar_get(fid1,"LocalSeaLevel_RCP26")
NO_fdyn_rcp45 <- ncvar_get(fid1,"LocalSeaLevel_RCP45")
NO_fdyn_rcp60 <- ncvar_get(fid1,"LocalSeaLevel_RCP60")
NO_fdyn_rcp85 <- ncvar_get(fid1,"LocalSeaLevel_RCP85")
NO_fdyn_proj <- ncvar_get(fid1,"time_proj")
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
t.time = 2010:2100
subsid.fit = mat.or.vec(states_w, length(t.time))
for(i in 1:states_w){
  fit = lm(BKkopp_subsid_dat[i,1:10] ~ k14_years[1:10])
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
  lsl_fdyn_rcp26_sub[ ,i] = convert_m_to_ft(lsl_fdyn_rcp26[match(2010:2100, year_proj),i]) + subsid.fit[i, ]
  lsl_fdyn_rcp45_sub[ ,i] = convert_m_to_ft(lsl_fdyn_rcp45[match(2010:2100, year_proj),i]) + subsid.fit[i, ]
  lsl_fdyn_rcp60_sub[ ,i] = convert_m_to_ft(lsl_fdyn_rcp60[match(2010:2100, year_proj),i]) + subsid.fit[i, ]
  lsl_fdyn_rcp85_sub[ ,i] = convert_m_to_ft(lsl_fdyn_rcp85[match(2010:2100, year_proj),i]) + subsid.fit[i, ]
  
  NO_fdyn_rcp26_sub[ ,i] = convert_m_to_ft(NO_fdyn_rcp26[match(2010:2100, year_proj),i]) + subsid.fit[i, ]
  NO_fdyn_rcp45_sub[ ,i] = convert_m_to_ft(NO_fdyn_rcp45[match(2010:2100, year_proj),i]) + subsid.fit[i, ]
  NO_fdyn_rcp60_sub[ ,i] = convert_m_to_ft(NO_fdyn_rcp60[match(2010:2100, year_proj),i]) + subsid.fit[i, ]
  NO_fdyn_rcp85_sub[ ,i] = convert_m_to_ft(NO_fdyn_rcp85[match(2010:2100, year_proj),i]) + subsid.fit[i, ]
}

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
# Read in data from the USACE Sea level calculator for Sewells Point: http://www.corpsclimate.us/ccaceslcurves.cfm
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
# Data is presented in meters above mean sea level; tide gauge observations are from Sewells Point.
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
# NOAA MLE (Zervas data just with more points) from the USACE Sea level calculator for Sewells Point: 
# http://www.corpsclimate.us/ccaceslcurves.cfm. Feet above MSL;
# And MLE with 95% Confidence interval from Table C in Appendix III of Zervas 2013
# Meters above MHHW; Loc = 0.678 ± 0.041, scale = 0.170 ± 0.031, shape = 0.120 ± 0.163
zervas_2013 = read.csv("../Data/Zervas_2013_ExtremeWaterLevels_SewellsPoint.csv", skip=2, header=TRUE)

# Convert the data collected from Table C to ft msl. Other data is already in ft above msl.
zervas_2013[,7:9] = convert_mhhw_to_msl(convert_m_to_ft(zervas_2013[,7:9]))

##=========================== READ SRIKRISHNAN ET AL. IN PREP. DATA ===================================
# Millimeters above mean sea level
stationary = readRDS("../Data/Srikrishnan_norfolk_MCMC-stationary.rds")
# processed_data = readRDS("../Data/Srikrishnan_processed_norfolk_data.rds")

#------------------- Remove a burnin and extraxt the 95% parameter estimates ------------------- 
burnin = 1:50000
burn_stationary = stationary[[1]]$samples[-burnin, ]
post_stat_25 = rep(NA,3)
post_stat_975 = rep(NA,3)
for(i in 1:3){
  post_stat_25[i] = quantile(burn_stationary[ ,i],0.025) 
  post_stat_975[i] = quantile(burn_stationary[ ,i],0.975) 
}

#------------------------- Generate MLE and 95% GEV distribution -----------------------
stat_gev = convert_mm_to_ft(revd(nrow(burn_stationary), loc = burn_stationary[,1], scale = burn_stationary[,2], 
                shape = burn_stationary[,3], type='GEV'))
stat_gev25 = convert_mm_to_ft(revd(1e5, loc = post_stat_25[1], scale = post_stat_25[2], shape = post_stat_25[3], type='GEV'))
stat_gev975 = convert_mm_to_ft(revd(1e5, loc = post_stat_975[1], scale = post_stat_975[2], shape = post_stat_975[3], type='GEV'))

################################### COMBINED SLR and STORM SURGE DATA ##################################
#------------------- Generate GEV distribution using Srikrishnan et al. in prep. -----------------
# Generate storm surge GEV distribution with an ensemble size matching Wong and Keller 2017
subbrick_length = length(brickfd_rcp26$t_2030)
subbrick_stat = burn_stationary[sample(nrow(burn_stationary), size=subbrick_length, replace=FALSE), ]
stationary_brick = convert_mm_to_ft(revd(nrow(subbrick_stat), loc = subbrick_stat[,1], scale = subbrick_stat[,2], 
                      shape = subbrick_stat[,3], type='GEV'))

# Generate storm surge GEV distribution with an ensemble size matching Kopp et al. 2014, Kopp et al. 2017, and Rasmussen et al. 2018
subkopp_length = length(kopp14_rcp26$t_2030)
subkopp_stat = burn_stationary[sample(nrow(burn_stationary), size=subkopp_length, replace=FALSE), ]
stationary_kopp = convert_mm_to_ft(revd(nrow(subkopp_stat), loc = subkopp_stat[,1], scale = subkopp_stat[,2], 
                      shape = subkopp_stat[,3], type='GEV'))

# Generate storm surge GEV distribution with an ensemble size matching Sweet et al. 2017
subsweet17_SEW_length = length(sweet17_03$X2030)
subsweet17_SEW_stat = burn_stationary[sample(nrow(burn_stationary), size=subsweet17_SEW_length, replace=FALSE), ]
stationary_sweet17_SEW = convert_mm_to_ft(revd(nrow(subsweet17_SEW_stat), loc = subsweet17_SEW_stat[,1], 
                                                   scale = subsweet17_SEW_stat[,2], shape = subsweet17_SEW_stat[,3], type='GEV'))

#--------------------------- Add distribution of storm surge data to SLR data --------------------------
# Add stationary storm surge to Kopp et al. 2014
k14_r26_SS = kopp14_rcp26 + stationary_kopp
k14_r45_SS = kopp14_rcp45 + stationary_kopp
k14_r60_SS = kopp14_rcp60 + stationary_kopp
k14_r85_SS = kopp14_rcp85 + stationary_kopp

# Add stationary storm surge to Kopp et al. 2017
k17_DP16_SEW_r26_SS = kopp17_DP16_SEW_rcp26 + stationary_kopp
k17_DP16_SEW_r45_SS = kopp17_DP16_SEW_rcp45 + stationary_kopp
k17_DP16_SEW_r60_SS = kopp17_DP16_SEW_rcp60 + stationary_kopp
k17_DP16_SEW_r85_SS = kopp17_DP16_SEW_rcp85 + stationary_kopp

# Add stationary storm surge to Rasmussen et al. 2018
Ras18_SEW_1p5deg_SS = Ras18_SEW_1p5deg + stationary_kopp
Ras18_SEW_2p0deg_SS = Ras18_SEW_2p0deg + stationary_kopp
Ras18_SEW_2p5deg_SS = Ras18_SEW_2p5deg + stationary_kopp

# Add stationary storm surge to Sweet et al. 2017
sweet17_03_SS = sweet17_03 + stationary_sweet17_SEW
sweet17_05_SS = sweet17_05 + stationary_sweet17_SEW
sweet17_10_SS = sweet17_10 + stationary_sweet17_SEW
sweet17_15_SS = sweet17_15 + stationary_sweet17_SEW
sweet17_20_SS = sweet17_20 + stationary_sweet17_SEW
sweet17_25_SS = sweet17_25 + stationary_sweet17_SEW

# Add stationary storm surge to Wong and Keller 2017 fast dynamics
bfd_r26_SS = brickfd_rcp26 + stationary_brick
bfd_r45_SS = brickfd_rcp45 + stationary_brick
bfd_r60_SS = brickfd_rcp60 + stationary_brick
bfd_r85_SS = brickfd_rcp85 + stationary_brick

# Add stationary storm surge to Wong and Keller 2017 NO fast dynamics
NOfd_r26_SS = NO_fdft_rcp26 + stationary_brick
NOfd_r45_SS = NO_fdft_rcp45 + stationary_brick
NOfd_r60_SS = NO_fdft_rcp60 + stationary_brick
NOfd_r85_SS = NO_fdft_rcp85 + stationary_brick
##==============================================================================
## End
##==============================================================================

