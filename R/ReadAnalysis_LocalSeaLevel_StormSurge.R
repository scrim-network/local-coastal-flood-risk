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
source("local-costal-flood-risk/R/Helper_scripts/conversion_functions.R")

################################### SEA-LEVEL DATA ##################################
##=========================== READ KOPP ET AL. 2014 DATA ===================================
# Kopp et al. 2014 Local sea-level rise data at Sewells point tide gauge
# Data is cm above 2000, so 0cm is 2000. Data is projected with RCP26, 45, 60, and 85.
kopp14_rcp26 = read.csv("LSLproj_MC_299_rcp26.tsv.csv") 
kopp14_rcp26 = convert_cm_to_ft(data.frame(t_2030 = kopp14_rcp26$X2030, t_2050 = kopp14_rcp26$X2050, 
                                           t_2070 = kopp14_rcp26$X2070, t_2100 = kopp14_rcp26$X2100))

kopp14_rcp45 = read.csv("LSLproj_MC_299_rcp45.tsv.csv")
kopp14_rcp45 = convert_cm_to_ft(data.frame(t_2030 = kopp14_rcp45$X2030, t_2050 = kopp14_rcp45$X2050, 
                                           t_2070 = kopp14_rcp45$X2070, t_2100 = kopp14_rcp45$X2100))

kopp14_rcp60 = read.csv("LSLproj_MC_299_rcp60.tsv.csv")
kopp14_rcp60 = convert_cm_to_ft(data.frame(t_2030 = kopp14_rcp60$X2030, t_2050 = kopp14_rcp60$X2050, 
                                           t_2070 = kopp14_rcp60$X2070, t_2100 = kopp14_rcp60$X2100))

kopp14_rcp85 = read.csv("LSLproj_MC_299_rcp85.tsv.csv")
kopp14_rcp85 = convert_cm_to_ft(data.frame(t_2030 = kopp14_rcp85$X2030, t_2050 = kopp14_rcp85$X2050, 
                                           t_2070 = kopp14_rcp85$X2070, t_2100 = kopp14_rcp85$X2100))

# Kopp et al. 2014 Local subsidence data at Sewells point tide gauge
# Data is cm above 2000, so 0cm is 2000. This will be used
# to account for subsidence in the Wong and Keller 2017 data.
kopp14_subsid = read.csv("LSLProj_bkgd_299_rcp26.csv")
kopp14_subsid = convert_cm_to_ft(data.frame(t_2030 = kopp14_subsid$X2030, t_2050 = kopp14_subsid$X2050, 
                                            t_2070 = kopp14_subsid$X2070, t_2100 = kopp14_subsid$X2100))

##=========================== READ WONG & KELLER 2017 DATA ===================================
# Wong and Keller 2017 Local sea-level rise data with Fast dynamics at Sewells point tide gauge
# Data is m above 2000, so 0 m is 2000. Data is projected with RCP26, 45, 60, and 85.
# The model used is called BRICK, so any reference to BRICK refers to Wong and Keller 2017. LSL (sewells point) with fast dynamics. 
fid1 <- nc_open("BRICK_SewellsPoint_FastDynamics_08May2017.nc")
lsl_fdyn_rcp26 <- ncvar_get(fid1,"LocalSeaLevel_RCP26")
lsl_fdyn_rcp45 <- ncvar_get(fid1,"LocalSeaLevel_RCP45")
lsl_fdyn_rcp60 <- ncvar_get(fid1,"LocalSeaLevel_RCP60")
lsl_fdyn_rcp85 <- ncvar_get(fid1,"LocalSeaLevel_RCP85")
year_proj <- ncvar_get(fid1,"time_proj")
nc_close(fid1)

# Wong and Keller 2017 Local sea-level rise data with NO Fast dynamics
fid1 <- nc_open("BRICK_NOfastDynamics_SP_08May2017.nc")
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
  BKkopp_subsid[ ,i] = rlnorm(states_w, meanlog=mean(log(kopp14_subsid[ ,i])), sdlog=sd(log(kopp14_subsid[ ,i])))
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
SL_calculator = read.csv("USACE_SL_Calculator_SewellsPoint.csv", skip=1, header=TRUE)
SL_calculator = as.matrix(SL_calculator)

#----------------------------- Convert baseline to 2000 -------------------------------------
SL_calculator_ref2000 = SL_calculator
for(i in 2:13){
  SL_calculator_ref2000[,i] = SL_calculator[,i] - SL_calculator[match(2000, SL_calculator[,1]),i]
}

#--------------------------- Extract specific years for each study --------------------------
match_SLC = match(c(2030,2050,2070,2100), SL_calculator[,1])
# Parris et al. 2012
noaa2012 = data.frame(t_2030 = c(SL_calculator_ref2000[match_SLC[1], 2:5]), 
                      t_2050 = c(SL_calculator_ref2000[match_SLC[2], 2:5]),
                      t_2070 = c(SL_calculator_ref2000[match_SLC[3], 2:5]), 
                      t_2100 = c(SL_calculator_ref2000[match_SLC[4], 2:5]), row.names = NULL)
# USACE 2014
usace2013 = data.frame(t_2030 = c(SL_calculator_ref2000[match_SLC[1], 6:8]), 
                       t_2050 = c(SL_calculator_ref2000[match_SLC[2], 6:8]),
                       t_2070 = c(SL_calculator_ref2000[match_SLC[3], 6:8]), 
                       t_2100 = c(SL_calculator_ref2000[match_SLC[4], 6:8]), row.names = NULL)
# Hall et al. 2016
carswg2016 = data.frame(t_2030 = c(SL_calculator_ref2000[match_SLC[1], 9:13]), 
                        t_2050 = c(SL_calculator_ref2000[match_SLC[2], 9:13]),
                        t_2070 = c(SL_calculator_ref2000[match_SLC[3], 9:13]), 
                        t_2100 = c(SL_calculator_ref2000[match_SLC[4], 9:13]), row.names = NULL)

##=========================== READ SWEET ET AL 2017 DATA ===================================
# Read in data from the USACE Sea level calculator for Sewells Point: http://www.corpsclimate.us/ccaceslcurves.cfm
# Data is ft above 1992, so 0 ft is 1992
NOAA_etal_2017 = read.csv("NOAA_etal_2017_SewellsPoint.csv", skip=1, header=TRUE) 
NOAA_etal_2017 = as.matrix(NOAA_etal_2017)

#----------------------------- Convert baseline to 2000 -------------------------------------
NOAA_etal_2017_ref2000 = NOAA_etal_2017
for(i in 2:22){
  NOAA_etal_2017_ref2000[,i] = NOAA_etal_2017[,i] - NOAA_etal_2017[match(2000, NOAA_etal_2017[,1]),i]
}

#--------------------------- Extract specific years --------------------------
match_NOAA17 = match(c(2030,2050,2070,2100), NOAA_etal_2017[,1])

# Medians are in columns 3,6,9,12,15,18, and 21; hence sequence from 3 to 21 by 3.
noaa2017 = data.frame(t_2030 = c(NOAA_etal_2017_ref2000[match_NOAA17[1], seq(3,21,3)]), 
                      t_2050 = c(NOAA_etal_2017_ref2000[match_NOAA17[2], seq(3,21,3)]),
                      t_2070 = c(NOAA_etal_2017_ref2000[match_NOAA17[3], seq(3,21,3)]), 
                      t_2100 = c(NOAA_etal_2017_ref2000[match_NOAA17[4], seq(3,21,3)]), row.names = NULL)

################################### STORM SURGE DATA ##################################
##=========================== READ USACE 2014 EXTREME WATER LEVEL DATA ===================================
# Data is presented in feet and meters above mean sea level
USACE_EWL = read.csv("USACE_ExtremeWaterLevels_SewellsPoint.csv", skip=2, header=TRUE) 
USACE_rp = as.numeric(as.character(USACE_EWL$Datum_EWL[8:14]))

##=========================== READ TEBALDI ET AL 2012 DATA ===================================
# Data is presented in meters above mean high water
tebaldi12 = read.csv("SewellsPoint_allrpsGPD_Tebaldi_etal_2012.csv", col.names=c("rp", "rl_50", "rp.1", "rl_025", "rl_975"))
tebaldi12[,c(2,4,5)] = convert_mhw_to_msl(convert_m_to_ft(tebaldi12[,c(2,4,5)]))

##==================== READ GEV ANALYSIS OF HISTORIC TIDE GAUGE OBSERVATIONS ============================
# Data is presented in meters above mean sea level; tide gauge observations are from Sewells Point.
NOAA_methodGEV = read.csv("NOAA_method_stormsurge_sewellspoint.csv")
NOAA_methodGEV[,c(3,4,5,6,9)] = convert_m_to_ft(NOAA_methodGEV[,c(3,4,5,6,9)])

##=========================== READ ZERVAS 2013 DATA ===================================
# Meters above MHHW; Loc = 0.678 ± 0.041, scale = 0.170 ± 0.031, 
# shape = 0.120 ± 0.163 (in meters, with 95% Confidence interval)
zervas_2013 = data.frame(mle = c(0.441, 0.742, 1.117, 1.722), min_95 = c(0.381, 0.697, 1.018, 1.422),
                         max_95 = c(0.480, 0.792, 1.279, 2.387), aep = c(0.99, 0.5, 0.1, 0.01))
zervas_2013[,1:3] = convert_mhhw_to_msl(convert_m_to_ft(zervas_2013[,1:3]))

# NOAA MLE (Zervas datajust with more points) from the USACE Sea level calculator for Sewells Point: 
# http://www.corpsclimate.us/ccaceslcurves.cfm. Feet above MSL
NOAA_rp = c(1,2,5,10,20,50,100)
NOAA_rl = c(2.85,3.84,4.55,5.07,5.62,6.40,7.05)

##=========================== READ SRIKRISHNAN ET AL. IN PREP. DATA ===================================
# Millimeters above mean sea level
stationary = readRDS("norfolk_MCMC-stationary.rds")
# processed_data = readRDS("processed_norfolk_data.rds")

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

# Generate storm surge GEV distribution with an ensemble size matching Kopp et al. 2014
subkopp_length = length(kopp14_rcp26$t_2030)
subkopp_stat = burn_stationary[sample(nrow(burn_stationary), size=subkopp_length, replace=FALSE), ]
stationary_kopp = convert_mm_to_ft(revd(nrow(subkopp_stat), loc = subkopp_stat[,1], scale = subkopp_stat[,2], 
                      shape = subkopp_stat[,3], type='GEV'))

#--------------------------- Add distribution of storm surge data to SLR data --------------------------
# Add stationary storm surge to Kopp et al. 2014
k14_r26_SS = kopp14_rcp26 + stationary_kopp
k14_r45_SS = kopp14_rcp45 + stationary_kopp
k14_r60_SS = kopp14_rcp60 + stationary_kopp
k14_r85_SS = kopp14_rcp85 + stationary_kopp

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

# Add Tebadli et al 2012 to 100-yr return period to Kopp et al. 2014
k14_r26_Teb = kopp14_rcp26 + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r45_Teb = kopp14_rcp45 + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r60_Teb = kopp14_rcp60 + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r85_Teb = kopp14_rcp85 + tebaldi12$rl_50[which(tebaldi12$rp == 100)]

##==============================================================================
## End
##==============================================================================

