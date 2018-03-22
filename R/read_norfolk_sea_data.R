# setwd('/Users/klr324/Documents/Data_LSL')

library(ncdf4)
library(extRemes)
library(RColorBrewer)
library(plotrix)
library(ash)
library(fields)

# Functions ---------------------------------------------------------------
# Conversion of metric to ft
convert_mm_to_ft = function(mm){
  mm * 0.00328084
} 

convert_cm_to_ft = function(cm){
  cm * 0.0328084
} 

convert_m_to_ft = function(m){
  m * 3.28084
}

# Conversion of datums
convert_mhw_to_msl = function(mhw_levels){
  MHW = 6.94
  MSL = 5.74
  mhw_levels + (MHW - MSL)
}

convert_mhhw_to_msl = function(mhhw_levels){
  MHHW = 7.14
  MSL = 5.74
  mhhw_levels + (MHHW - MSL)
}
#   -----------------------------------------------------------------------


# Read in data ------------------------------------------------------------
# Read in Kopp et al. 2014 MCMC LSLR data for sewells point
# Data is cm above 2000, so 0cm is 2000
kopp14_rcp26 = read.csv("LSLproj_MC_299_rcp26.tsv.csv") 
kopp14_rcp45 = read.csv("LSLproj_MC_299_rcp45.tsv.csv")
kopp14_rcp60 = read.csv("LSLproj_MC_299_rcp60.tsv.csv")
kopp14_rcp85 = read.csv("LSLproj_MC_299_rcp85.tsv.csv")

# Read in subsidence data for sewells point. This will be used
# to acoount for subsidence in the BRICK data.
kopp14_subsid = read.csv("LSLProj_bkgd_299_rcp26.csv")

# BRICK LSL (sewells point) with fast dynamics. Corresponds to the data from Tony and Keller 2017 (NOLA)
# Data is m above 2000, so 0m is 2000
fid1 <- nc_open("BRICK_SewellsPoint_FastDynamics_08May2017.nc")
lsl_fdyn_rcp26 <- ncvar_get(fid1,"LocalSeaLevel_RCP26")
lsl_fdyn_rcp45 <- ncvar_get(fid1,"LocalSeaLevel_RCP45")
lsl_fdyn_rcp60 <- ncvar_get(fid1,"LocalSeaLevel_RCP60")
lsl_fdyn_rcp85 <- ncvar_get(fid1,"LocalSeaLevel_RCP85")
year_proj <- ncvar_get(fid1,"time_proj")
nc_close(fid1)

fid1 <- nc_open("BRICK_NOfastDynamics_SP_08May2017.nc")
NO_fdyn_rcp26 <- ncvar_get(fid1,"LocalSeaLevel_RCP26")
NO_fdyn_rcp45 <- ncvar_get(fid1,"LocalSeaLevel_RCP45")
NO_fdyn_rcp60 <- ncvar_get(fid1,"LocalSeaLevel_RCP60")
NO_fdyn_rcp85 <- ncvar_get(fid1,"LocalSeaLevel_RCP85")
NO_fdyn_proj <- ncvar_get(fid1,"time_proj")
nc_close(fid1)

# Read in data from the USACE Sea level calculator for Sewells Point
# This data includes: NOAA et al. 2012, USACE 2013, CARSWG 2016, and NOAA et al. 2017
# Data is ft above 1992, so 0cm is 1992
NOAA_etal_2017 = read.csv("NOAA_etal_2017_SewellsPoint.csv", skip=1, header=TRUE) 
SL_calculator = read.csv("USACE_SL_Calculator_SewellsPoint.csv", skip=1, header=TRUE)
NOAA_etal_2017 = as.matrix(NOAA_etal_2017)
SL_calculator = as.matrix(SL_calculator)

# Convert to feet ---------------------------------------------------------
# Kopp et al. 2014 change from cm to feet
# RCP26
kopp14_rcp26_2030ft = convert_cm_to_ft(kopp14_rcp26$X2030)
kopp14_rcp26_2050ft = convert_cm_to_ft(kopp14_rcp26$X2050) 
kopp14_rcp26_2060ft = convert_cm_to_ft(kopp14_rcp26$X2060)
kopp14_rcp26_2100ft = convert_cm_to_ft(kopp14_rcp26$X2100) 

# RCP45
kopp14_rcp45_2030ft = convert_cm_to_ft(kopp14_rcp45$X2030)
kopp14_rcp45_2050ft = convert_cm_to_ft(kopp14_rcp45$X2050) 
kopp14_rcp45_2060ft = convert_cm_to_ft(kopp14_rcp45$X2060)
kopp14_rcp45_2100ft = convert_cm_to_ft(kopp14_rcp45$X2100) 

# RCP60
kopp14_rcp60_2030ft = convert_cm_to_ft(kopp14_rcp60$X2030)
kopp14_rcp60_2050ft = convert_cm_to_ft(kopp14_rcp60$X2050) 
kopp14_rcp60_2060ft = convert_cm_to_ft(kopp14_rcp60$X2060)
kopp14_rcp60_2100ft = convert_cm_to_ft(kopp14_rcp60$X2100) 

# RCP85
kopp14_rcp85_2030ft = convert_cm_to_ft(kopp14_rcp85$X2030)
kopp14_rcp85_2050ft = convert_cm_to_ft(kopp14_rcp85$X2050) 
kopp14_rcp85_2060ft = convert_cm_to_ft(kopp14_rcp85$X2060)
kopp14_rcp85_2100ft = convert_cm_to_ft(kopp14_rcp85$X2100)

# Subsidence
kopp14_subsid_2030ft = convert_cm_to_ft(kopp14_subsid$X2030)
kopp14_subsid_2050ft = convert_cm_to_ft(kopp14_subsid$X2050) 
kopp14_subsid_2060ft = convert_cm_to_ft(kopp14_subsid$X2060)
kopp14_subsid_2100ft = convert_cm_to_ft(kopp14_subsid$X2100)

# Kopp and Wong & Keller have different numbers of samples. Using the Kopp data randomly 
# generate subsidence data to increase the sample size to match that of Wong & Keller
states_w = dim(lsl_fdyn_rcp26)[2]
BKkopp_subsid_2030 = rlnorm(states_w, meanlog=mean(log(kopp14_subsid_2030ft)), sdlog=sd(log(kopp14_subsid_2030ft)))
BKkopp_subsid_2050 = rlnorm(states_w, meanlog=mean(log(kopp14_subsid_2050ft)), sdlog=sd(log(kopp14_subsid_2050ft)))
BKkopp_subsid_2060 = rlnorm(states_w, meanlog=mean(log(kopp14_subsid_2060ft)), sdlog=sd(log(kopp14_subsid_2060ft)))
BKkopp_subsid_2100 = rlnorm(states_w, meanlog=mean(log(kopp14_subsid_2100ft)), sdlog=sd(log(kopp14_subsid_2100ft)))

# BRICK change from m to feet and add subsidence based on Kopp et al. 2014
# RCP26
brickfd_rcp26_2030ft = convert_m_to_ft(lsl_fdyn_rcp26[match(2030, year_proj), ]) + BKkopp_subsid_2030
brickfd_rcp26_2050ft = convert_m_to_ft(lsl_fdyn_rcp26[match(2050, year_proj), ]) + BKkopp_subsid_2050
brickfd_rcp26_2060ft = convert_m_to_ft(lsl_fdyn_rcp26[match(2060, year_proj), ]) + BKkopp_subsid_2060
brickfd_rcp26_2100ft = convert_m_to_ft(lsl_fdyn_rcp26[match(2100, year_proj), ]) + BKkopp_subsid_2100

# RCP45
brickfd_rcp45_2030ft = convert_m_to_ft(lsl_fdyn_rcp45[match(2030, year_proj), ]) + BKkopp_subsid_2030
brickfd_rcp45_2050ft = convert_m_to_ft(lsl_fdyn_rcp45[match(2050, year_proj), ]) + BKkopp_subsid_2050
brickfd_rcp45_2060ft = convert_m_to_ft(lsl_fdyn_rcp45[match(2060, year_proj), ]) + BKkopp_subsid_2060
brickfd_rcp45_2100ft = convert_m_to_ft(lsl_fdyn_rcp45[match(2100, year_proj), ]) + BKkopp_subsid_2100

# RCP60
brickfd_rcp60_2030ft = convert_m_to_ft(lsl_fdyn_rcp60[match(2030, year_proj), ]) + BKkopp_subsid_2030
brickfd_rcp60_2050ft = convert_m_to_ft(lsl_fdyn_rcp60[match(2050, year_proj), ]) + BKkopp_subsid_2050
brickfd_rcp60_2060ft = convert_m_to_ft(lsl_fdyn_rcp60[match(2060, year_proj), ]) + BKkopp_subsid_2060
brickfd_rcp60_2100ft = convert_m_to_ft(lsl_fdyn_rcp60[match(2100, year_proj), ]) + BKkopp_subsid_2100

# RCP85
brickfd_rcp85_2030ft = convert_m_to_ft(lsl_fdyn_rcp85[match(2030, year_proj), ]) + BKkopp_subsid_2030
brickfd_rcp85_2050ft = convert_m_to_ft(lsl_fdyn_rcp85[match(2050, year_proj), ]) + BKkopp_subsid_2050
brickfd_rcp85_2060ft = convert_m_to_ft(lsl_fdyn_rcp85[match(2060, year_proj), ]) + BKkopp_subsid_2060
brickfd_rcp85_2100ft = convert_m_to_ft(lsl_fdyn_rcp85[match(2100, year_proj), ]) + BKkopp_subsid_2100

# BRICK change from m to feet and add subsidence based on Kopp et al. 2014
# RCP26
NO_fd_rcp26_2030ft = convert_m_to_ft(NO_fdyn_rcp26[match(2030, NO_fdyn_proj), ]) + BKkopp_subsid_2030
NO_fd_rcp26_2050ft = convert_m_to_ft(NO_fdyn_rcp26[match(2050, NO_fdyn_proj), ]) + BKkopp_subsid_2050
NO_fd_rcp26_2060ft = convert_m_to_ft(NO_fdyn_rcp26[match(2060, NO_fdyn_proj), ]) + BKkopp_subsid_2060
NO_fd_rcp26_2100ft = convert_m_to_ft(NO_fdyn_rcp26[match(2100, NO_fdyn_proj), ]) + BKkopp_subsid_2100

# RCP45
NO_fd_rcp45_2030ft = convert_m_to_ft(NO_fdyn_rcp45[match(2030, NO_fdyn_proj), ]) + BKkopp_subsid_2030
NO_fd_rcp45_2050ft = convert_m_to_ft(NO_fdyn_rcp45[match(2050, NO_fdyn_proj), ]) + BKkopp_subsid_2050
NO_fd_rcp45_2060ft = convert_m_to_ft(NO_fdyn_rcp45[match(2060, NO_fdyn_proj), ]) + BKkopp_subsid_2060
NO_fd_rcp45_2100ft = convert_m_to_ft(NO_fdyn_rcp45[match(2100, NO_fdyn_proj), ]) + BKkopp_subsid_2100

# RCP60
NO_fd_rcp60_2030ft = convert_m_to_ft(NO_fdyn_rcp60[match(2030, NO_fdyn_proj), ]) + BKkopp_subsid_2030
NO_fd_rcp60_2050ft = convert_m_to_ft(NO_fdyn_rcp60[match(2050, NO_fdyn_proj), ]) + BKkopp_subsid_2050
NO_fd_rcp60_2060ft = convert_m_to_ft(NO_fdyn_rcp60[match(2060, NO_fdyn_proj), ]) + BKkopp_subsid_2060
NO_fd_rcp60_2100ft = convert_m_to_ft(NO_fdyn_rcp60[match(2100, NO_fdyn_proj), ]) + BKkopp_subsid_2100

# RCP85
NO_fd_rcp85_2030ft = convert_m_to_ft(NO_fdyn_rcp85[match(2030, NO_fdyn_proj), ]) + BKkopp_subsid_2030
NO_fd_rcp85_2050ft = convert_m_to_ft(NO_fdyn_rcp85[match(2050, NO_fdyn_proj), ]) + BKkopp_subsid_2050
NO_fd_rcp85_2060ft = convert_m_to_ft(NO_fdyn_rcp85[match(2060, NO_fdyn_proj), ]) + BKkopp_subsid_2060
NO_fd_rcp85_2100ft = convert_m_to_ft(NO_fdyn_rcp85[match(2100, NO_fdyn_proj), ]) + BKkopp_subsid_2100

# Convert baseline to 2000 ------------------------------------------------
NOAA_etal_2017_ref2000 = NOAA_etal_2017[,2:22] - NOAA_etal_2017[match(2000, NOAA_etal_2017[,1]),2:22]
SL_calculator_ref2000 = SL_calculator[,2:13] - SL_calculator[match(2000, SL_calculator[,1]),2:13]

# Extract specific years --------------------------------------------------
match2030 = match(2030, SL_calculator[,1])
match2050 = match(2050, SL_calculator[,1])
match2060 = match(2060, SL_calculator[,1])
match2100 = match(2100, SL_calculator[,1])

N_match2030 = match(2030, NOAA_etal_2017[,1])
N_match2050 = match(2050, NOAA_etal_2017[,1])
N_match2060 = match(2060, NOAA_etal_2017[,1])
N_match2100 = match(2100, NOAA_etal_2017[,1])

noaa2012_2030 = c(SL_calculator[match2030, 2], SL_calculator[match2030, 3], SL_calculator[match2030, 4], 
                  SL_calculator[match2030, 5])
noaa2012_2050 = c(SL_calculator[match2050, 2], SL_calculator[match2050, 3], SL_calculator[match2050, 4], 
                  SL_calculator[match2050, 5])
noaa2012_2060 = c(SL_calculator[match2060, 2], SL_calculator[match2060, 3], SL_calculator[match2060, 4], 
                  SL_calculator[match2060, 5])
noaa2012_2100 = c(SL_calculator[match2100, 2], SL_calculator[match2100, 3], SL_calculator[match2100, 4], 
                  SL_calculator[match2100, 5])

usace2013_2030 = c(SL_calculator[match2030, 6], SL_calculator[match2030, 7], SL_calculator[match2030, 8])
usace2013_2050 = c(SL_calculator[match2050, 6], SL_calculator[match2050, 7], SL_calculator[match2050, 8])
usace2013_2060 = c(SL_calculator[match2060, 6], SL_calculator[match2060, 7], SL_calculator[match2060, 8])
usace2013_2100 = c(SL_calculator[match2100, 6], SL_calculator[match2100, 7], SL_calculator[match2100, 8])

carswg2016_2030 = c(SL_calculator[match2030, 9], SL_calculator[match2030, 10], SL_calculator[match2030, 11], 
                    SL_calculator[match2030, 12], SL_calculator[match2030, 13])
carswg2016_2050 = c(SL_calculator[match2050, 9], SL_calculator[match2050, 10], SL_calculator[match2050, 11], 
                    SL_calculator[match2050, 12], SL_calculator[match2050, 13])
carswg2016_2060 = c(SL_calculator[match2060, 9], SL_calculator[match2060, 10], SL_calculator[match2060, 11], 
                    SL_calculator[match2060, 12], SL_calculator[match2060, 13])
carswg2016_2100 = c(SL_calculator[match2100, 9], SL_calculator[match2100, 10], SL_calculator[match2100, 11], 
                    SL_calculator[match2100, 12], SL_calculator[match2100, 13])

noaa2017_2030_17 = c(NOAA_etal_2017[N_match2030, 2], NOAA_etal_2017[N_match2030, 5], NOAA_etal_2017[N_match2030, 8],
                     NOAA_etal_2017[N_match2030, 11], NOAA_etal_2017[N_match2030, 14], NOAA_etal_2017[N_match2030, 17],
                     NOAA_etal_2017[N_match2030, 20])
noaa2017_2030_50 = c(NOAA_etal_2017[N_match2030, 3], NOAA_etal_2017[N_match2030, 6], NOAA_etal_2017[N_match2030, 9],
                     NOAA_etal_2017[N_match2030, 12], NOAA_etal_2017[N_match2030, 15], NOAA_etal_2017[N_match2030, 18],
                     NOAA_etal_2017[N_match2030, 21])
noaa2017_2030_83 = c(NOAA_etal_2017[N_match2030, 4], NOAA_etal_2017[N_match2030, 7], NOAA_etal_2017[N_match2030, 10],
                     NOAA_etal_2017[N_match2030, 13], NOAA_etal_2017[N_match2030, 16], NOAA_etal_2017[N_match2030, 19],
                     NOAA_etal_2017[N_match2030, 22])

noaa2017_2050_17 = c(NOAA_etal_2017[N_match2050, 2], NOAA_etal_2017[N_match2050, 5], NOAA_etal_2017[N_match2050, 8],
                     NOAA_etal_2017[N_match2050, 11], NOAA_etal_2017[N_match2050, 14], NOAA_etal_2017[N_match2050, 17],
                     NOAA_etal_2017[N_match2050, 20])
noaa2017_2050_50 = c(NOAA_etal_2017[N_match2050, 3], NOAA_etal_2017[N_match2050, 6], NOAA_etal_2017[N_match2050, 9],
                     NOAA_etal_2017[N_match2050, 12], NOAA_etal_2017[N_match2050, 15], NOAA_etal_2017[N_match2050, 18],
                     NOAA_etal_2017[N_match2050, 21])
noaa2017_2050_83 = c(NOAA_etal_2017[N_match2050, 4], NOAA_etal_2017[N_match2050, 7], NOAA_etal_2017[N_match2050, 10],
                     NOAA_etal_2017[N_match2050, 13], NOAA_etal_2017[N_match2050, 16], NOAA_etal_2017[N_match2050, 19],
                     NOAA_etal_2017[N_match2050, 22])

noaa2017_2060_17 = c(NOAA_etal_2017[N_match2060, 2], NOAA_etal_2017[N_match2060, 5], NOAA_etal_2017[N_match2060, 8],
                     NOAA_etal_2017[N_match2060, 11], NOAA_etal_2017[N_match2060, 14], NOAA_etal_2017[N_match2060, 17],
                     NOAA_etal_2017[N_match2060, 20])
noaa2017_2060_50 = c(NOAA_etal_2017[N_match2060, 3], NOAA_etal_2017[N_match2060, 6], NOAA_etal_2017[N_match2060, 9],
                     NOAA_etal_2017[N_match2060, 12], NOAA_etal_2017[N_match2060, 15], NOAA_etal_2017[N_match2060, 18],
                     NOAA_etal_2017[N_match2060, 21])
noaa2017_2060_83 = c(NOAA_etal_2017[N_match2060, 4], NOAA_etal_2017[N_match2060, 7], NOAA_etal_2017[N_match2060, 10],
                     NOAA_etal_2017[N_match2060, 13], NOAA_etal_2017[N_match2060, 16], NOAA_etal_2017[N_match2060, 19],
                     NOAA_etal_2017[N_match2060, 22])

noaa2017_2100_17 = c(NOAA_etal_2017[N_match2100, 2], NOAA_etal_2017[N_match2100, 5], NOAA_etal_2017[N_match2100, 8],
                     NOAA_etal_2017[N_match2100, 11], NOAA_etal_2017[N_match2100, 14], NOAA_etal_2017[N_match2100, 17],
                     NOAA_etal_2017[N_match2100, 20])
noaa2017_2100_50 = c(NOAA_etal_2017[N_match2100, 3], NOAA_etal_2017[N_match2100, 6], NOAA_etal_2017[N_match2100, 9],
                     NOAA_etal_2017[N_match2100, 12], NOAA_etal_2017[N_match2100, 15], NOAA_etal_2017[N_match2100, 18],
                     NOAA_etal_2017[N_match2100, 21])
noaa2017_2100_83 = c(NOAA_etal_2017[N_match2100, 4], NOAA_etal_2017[N_match2100, 7], NOAA_etal_2017[N_match2100, 10],
                     NOAA_etal_2017[N_match2100, 13], NOAA_etal_2017[N_match2100, 16], NOAA_etal_2017[N_match2100, 19],
                     NOAA_etal_2017[N_match2100, 22])

# Storm Surge -------------------------------------------------------------
USACE_EWL = read.csv("USACE_ExtremeWaterLevels_SewellsPoint.csv", skip=2, header=TRUE) 
USACE_rp = as.numeric(as.character(USACE_EWL$Datum_EWL[8:14]))

tebaldi12 = read.csv("SewellsPoint_allrpsGPD_Tebaldi_etal_2012.csv", col.names=c("rp", "rl_50", "rp.1", "rl_025", "rl_975"))
tebaldi12[,c(2,4,5)] = convert_mhw_to_msl(convert_m_to_ft(tebaldi12[,c(2,4,5)]))

NOAA_methodGEV = read.csv("NOAA_method_stormsurge_sewellspoint.csv")
NOAA_methodGEV[,c(3,4,5,6,9)] = convert_m_to_ft(NOAA_methodGEV[,c(3,4,5,6,9)])

# USACE sea-level calculator
NOAA_rp = c(1,2,5,10,20,50,100)
NOAA_rl = c(2.85,3.84,4.55,5.07,5.62,6.40,7.05)

# Meters above MHHW; Loc = 0.678 ± 0.041, scale = 0.170 ± 0.031, 
# shape = 0.120 ± 0.163 (in meters, with 95% Confidence interval)
zervas_2013 = data.frame(mle = c(0.441, 0.742, 1.117, 1.722), min_95 = c(0.381, 0.697, 1.018, 1.422),
                         max_95 = c(0.480, 0.792, 1.279, 2.387), aep = c(0.99, 0.5, 0.1, 0.01))
zervas_2013[,1:3] = convert_mhhw_to_msl(convert_m_to_ft(zervas_2013[,1:3]))

# Load storm surge data from Vivek et al. in prep.
stationary = readRDS("norfolk_MCMC-stationary.rds")
non_stationary = readRDS("norfolk_MCMC-nonstationary.rds")
processed_data = readRDS("processed_norfolk_data.rds")

burnin = 1:50000
burn_stationary = stationary[[1]]$samples[-burnin, ]
mean_stat = rep(NA,3)
post_stat_25 = rep(NA,3)
post_stat_975 = rep(NA,3)
for(i in 1:3){
  mean_stat[i] = mean(burn_stationary[ ,i])
  post_stat_25[i] = quantile(burn_stationary[ ,i],0.025) 
  post_stat_975[i] = quantile(burn_stationary[ ,i],0.975) 
}

burn_nonstat = non_stationary[[1]]$samples[-burnin, ]
mean_nonstat = rep(NA,4)
post_nonstat_25 = rep(NA,4)
post_nonstat_975 = rep(NA,4)
for(i in 1:4){
  mean_nonstat[i] = mean(burn_nonstat[ ,i])
  post_nonstat_25[i] = quantile(burn_nonstat[ ,i],0.025) 
  post_nonstat_975[i] = quantile(burn_nonstat[ ,i],0.975) 
}

temp_2030_yr = match(2030, processed_data$temps$forcing$year)
temp_2050_yr = match(2050, processed_data$temps$forcing$year)
temp_2060_yr = match(2060, processed_data$temps$forcing$year)
temp_2100_yr = match(2100, processed_data$temps$forcing$year)
lambda_2030 = burn_nonstat[,1] + (processed_data$temps$forcing$temp[temp_2030_yr])*burn_nonstat[,2]
lambda_2030_25 = post_nonstat_25[1] + (processed_data$temps$forcing$temp[temp_2030_yr])*post_nonstat_25[2]
lambda_2030_975 = post_nonstat_975[1] + (processed_data$temps$forcing$temp[temp_2030_yr])*post_nonstat_975[2]
lambda_2050 = burn_nonstat[,1] + (processed_data$temps$forcing$temp[temp_2050_yr])*burn_nonstat[,2]
lambda_2050_25 = post_nonstat_25[1] + (processed_data$temps$forcing$temp[temp_2050_yr])*post_nonstat_25[2]
lambda_2050_975 = post_nonstat_975[1] + (processed_data$temps$forcing$temp[temp_2050_yr])*post_nonstat_975[2]
lambda_2060 = burn_nonstat[,1] + (processed_data$temps$forcing$temp[temp_2060_yr])*burn_nonstat[,2]
lambda_2060_25 = post_nonstat_25[1] + (processed_data$temps$forcing$temp[temp_2060_yr])*post_nonstat_25[2]
lambda_2060_975 = post_nonstat_975[1] + (processed_data$temps$forcing$temp[temp_2060_yr])*post_nonstat_975[2]
lambda_2100 = burn_nonstat[,1] + (processed_data$temps$forcing$temp[temp_2100_yr])*burn_nonstat[,2]
lambda_2100_25 = post_nonstat_25[1] + (processed_data$temps$forcing$temp[temp_2100_yr])*post_nonstat_25[2]
lambda_2100_975 = post_nonstat_975[1] + (processed_data$temps$forcing$temp[temp_2100_yr])*post_nonstat_975[2]

# Random generation data in millimeters
stat_gev = revd(nrow(burn_stationary), loc = burn_stationary[,1], scale = burn_stationary[,2], 
                shape = burn_stationary[,3], type='GEV')
stat_gev25 = revd(1e5, loc = post_stat_25[1], scale = post_stat_25[2], shape = post_stat_25[3], type='GEV')
stat_gev975 = revd(1e5, loc = post_stat_975[1], scale = post_stat_975[2], shape = post_stat_975[3], type='GEV')

nonstat_gev2030 = revd(nrow(burn_nonstat), loc = lambda_2030, scale = burn_nonstat[,3], 
                       shape = burn_nonstat[,4], type='GEV')
nonstat_gev203025 = revd(1e5, loc = lambda_2030_25, scale = post_nonstat_25[3], shape = post_nonstat_25[4], type='GEV')
nonstat_gev2030975 = revd(1e5, loc = lambda_2030_975, scale = post_nonstat_975[3], shape = post_nonstat_975[4], type='GEV')
nonstat_gev2050 = revd(nrow(burn_nonstat), loc = lambda_2050, scale = burn_nonstat[,3], 
                       shape = burn_nonstat[,4], type='GEV')
nonstat_gev205025 = revd(1e5, loc = lambda_2050_25, scale = post_nonstat_25[3], shape = post_nonstat_25[4], type='GEV')
nonstat_gev2050975 = revd(1e5, loc = lambda_2050_975, scale = post_nonstat_975[3], shape = post_nonstat_975[4], type='GEV')
nonstat_gev2060 = revd(nrow(burn_nonstat), loc = lambda_2060, scale = burn_nonstat[,3], 
                       shape = burn_nonstat[,4], type='GEV')
nonstat_gev206025 = revd(1e5, loc = lambda_2060_25, scale = post_nonstat_25[3], shape = post_nonstat_25[4], type='GEV')
nonstat_gev2060975 = revd(1e5, loc = lambda_2060_975, scale = post_nonstat_975[3], shape = post_nonstat_975[4], type='GEV')
nonstat_gev2100 = revd(nrow(burn_nonstat), loc = lambda_2100, scale = burn_nonstat[,3], 
                       shape = burn_nonstat[,4], type='GEV')
nonstat_gev210025 = revd(1e5, loc = lambda_2100_25, scale = post_nonstat_25[3], shape = post_nonstat_25[4], type='GEV')
nonstat_gev2100975 = revd(1e5, loc = lambda_2100_975, scale = post_nonstat_975[3], shape = post_nonstat_975[4], type='GEV')

stat_gev = convert_mm_to_ft(stat_gev)
stat_gev25 = convert_mm_to_ft(stat_gev25)
stat_gev975 = convert_mm_to_ft(stat_gev975)

nonstat_gev2030 = convert_mm_to_ft(nonstat_gev2030)
nonstat_gev203025 = convert_mm_to_ft(nonstat_gev203025)
nonstat_gev2030975 = convert_mm_to_ft(nonstat_gev2030975)
nonstat_gev2050 = convert_mm_to_ft(nonstat_gev2050)
nonstat_gev205025 = convert_mm_to_ft(nonstat_gev205025)
nonstat_gev2050975 = convert_mm_to_ft(nonstat_gev2050975)
nonstat_gev2060 = convert_mm_to_ft(nonstat_gev2060)
nonstat_gev206025 = convert_mm_to_ft(nonstat_gev206025)
nonstat_gev2060975 = convert_mm_to_ft(nonstat_gev2060975)
nonstat_gev2100 = convert_mm_to_ft(nonstat_gev2100)
nonstat_gev210025 = convert_mm_to_ft(nonstat_gev210025)
nonstat_gev2100975 = convert_mm_to_ft(nonstat_gev2100975)

#--------- Combined SLR + Storm surge
# BRICK length stationary
subbrick_length = length(brickfd_rcp26_2030ft)
subbrick_stat = burn_stationary[sample(nrow(burn_stationary), size=subbrick_length, replace=FALSE), ]
stationary_brick = convert_mm_to_ft(revd(nrow(subbrick_stat), loc = subbrick_stat[,1], scale = subbrick_stat[,2], 
                      shape = subbrick_stat[,3], type='GEV'))
#KOPP length stationary
subkopp_length = length(kopp14_rcp26_2030ft)
subkopp_stat = burn_stationary[sample(nrow(burn_stationary), size=subkopp_length, replace=FALSE), ]
stationary_kopp = convert_mm_to_ft(revd(nrow(subkopp_stat), loc = subkopp_stat[,1], scale = subkopp_stat[,2], 
                      shape = subkopp_stat[,3], type='GEV'))
# BRICK length non stationary
subbrick_nonstat = burn_nonstat[sample(nrow(burn_nonstat), size=subbrick_length, replace=FALSE), ]
lambda_brick2030 = subbrick_nonstat[,1] + (processed_data$temps$forcing$temp[temp_2030_yr])*subbrick_nonstat[,2]
lambda_brick2050 = subbrick_nonstat[,1] + (processed_data$temps$forcing$temp[temp_2050_yr])*subbrick_nonstat[,2]
lambda_brick2060 = subbrick_nonstat[,1] + (processed_data$temps$forcing$temp[temp_2060_yr])*subbrick_nonstat[,2]
lambda_brick2100 = subbrick_nonstat[,1] + (processed_data$temps$forcing$temp[temp_2100_yr])*subbrick_nonstat[,2]
nonstat_brick2030 = convert_mm_to_ft(revd(nrow(subbrick_nonstat), loc = lambda_brick2030, scale = subbrick_nonstat[,3], 
                                         shape = subbrick_nonstat[,4], type='GEV'))
nonstat_brick2050 = convert_mm_to_ft(revd(nrow(subbrick_nonstat), loc = lambda_brick2050, scale = subbrick_nonstat[,3], 
                                         shape = subbrick_nonstat[,4], type='GEV'))
nonstat_brick2060 = convert_mm_to_ft(revd(nrow(subbrick_nonstat), loc = lambda_brick2060, scale = subbrick_nonstat[,3], 
                                         shape = subbrick_nonstat[,4], type='GEV'))
nonstat_brick2100 = convert_mm_to_ft(revd(nrow(subbrick_nonstat), loc = lambda_brick2100, scale = subbrick_nonstat[,3], 
                                         shape = subbrick_nonstat[,4], type='GEV'))
# KOPP length non stationary
subkopp_nonstat = burn_nonstat[sample(nrow(burn_nonstat), size=subkopp_length, replace=FALSE), ]
lambda_kopp2030 = subkopp_nonstat[,1] + (processed_data$temps$forcing$temp[temp_2030_yr])*subkopp_nonstat[,2]
lambda_kopp2050 = subkopp_nonstat[,1] + (processed_data$temps$forcing$temp[temp_2050_yr])*subkopp_nonstat[,2]
lambda_kopp2060 = subkopp_nonstat[,1] + (processed_data$temps$forcing$temp[temp_2060_yr])*subkopp_nonstat[,2]
lambda_kopp2100 = subkopp_nonstat[,1] + (processed_data$temps$forcing$temp[temp_2100_yr])*subkopp_nonstat[,2]
nonstat_kopp2030 = convert_mm_to_ft(revd(nrow(subkopp_nonstat), loc = lambda_kopp2030, scale = subkopp_nonstat[,3], 
                                       shape = subkopp_nonstat[,4], type='GEV'))
nonstat_kopp2050 = convert_mm_to_ft(revd(nrow(subkopp_nonstat), loc = lambda_kopp2050, scale = subkopp_nonstat[,3], 
                                         shape = subkopp_nonstat[,4], type='GEV'))
nonstat_kopp2060 = convert_mm_to_ft(revd(nrow(subkopp_nonstat), loc = lambda_kopp2060, scale = subkopp_nonstat[,3], 
                                         shape = subkopp_nonstat[,4], type='GEV'))
nonstat_kopp2100 = convert_mm_to_ft(revd(nrow(subkopp_nonstat), loc = lambda_kopp2100, scale = subkopp_nonstat[,3], 
                                         shape = subkopp_nonstat[,4], type='GEV'))
#add storm surge to kopp
# RCP26
k14_r26_2030_SS = kopp14_rcp45_2030ft + stationary_kopp
k14_r26_2050_SS = kopp14_rcp45_2050ft + stationary_kopp
k14_r26_2060_SS = kopp14_rcp45_2060ft + stationary_kopp
k14_r26_2100_SS = kopp14_rcp45_2100ft + stationary_kopp
# RCP45
k14_r45_2030_SS = kopp14_rcp45_2030ft + stationary_kopp
k14_r45_2050_SS = kopp14_rcp45_2050ft + stationary_kopp 
k14_r45_2060_SS = kopp14_rcp45_2060ft + stationary_kopp
k14_r45_2100_SS = kopp14_rcp45_2100ft + stationary_kopp
# RCP60
k14_r60_2030_SS = kopp14_rcp60_2030ft + stationary_kopp
k14_r60_2050_SS = kopp14_rcp60_2050ft + stationary_kopp 
k14_r60_2060_SS = kopp14_rcp60_2060ft + stationary_kopp
k14_r60_2100_SS = kopp14_rcp60_2100ft + stationary_kopp 
# RCP85
k14_r85_2030_SS = kopp14_rcp85_2030ft + stationary_kopp
k14_r85_2050_SS = kopp14_rcp85_2050ft + stationary_kopp 
k14_r85_2060_SS = kopp14_rcp85_2060ft + stationary_kopp
k14_r85_2100_SS = kopp14_rcp85_2100ft + stationary_kopp
k14_r85_2030_SSN = kopp14_rcp85_2030ft + nonstat_kopp2030
k14_r85_2050_SSN = kopp14_rcp85_2050ft + nonstat_kopp2050 
k14_r85_2060_SSN = kopp14_rcp85_2060ft + nonstat_kopp2060
k14_r85_2100_SSN = kopp14_rcp85_2100ft + nonstat_kopp2100

#add storm surge to brick fast dynamics
# RCP26
bfd_r26_2030_SS = brickfd_rcp26_2030ft + stationary_brick
bfd_r26_2050_SS = brickfd_rcp26_2050ft + stationary_brick
bfd_r26_2060_SS = brickfd_rcp26_2060ft + stationary_brick
bfd_r26_2100_SS = brickfd_rcp26_2100ft + stationary_brick
# RCP45
bfd_r45_2030_SS = brickfd_rcp45_2030ft + stationary_brick
bfd_r45_2050_SS = brickfd_rcp45_2050ft + stationary_brick
bfd_r45_2060_SS = brickfd_rcp45_2060ft + stationary_brick
bfd_r45_2100_SS = brickfd_rcp45_2100ft + stationary_brick
# RCP60
bfd_r60_2030_SS = brickfd_rcp60_2030ft + stationary_brick
bfd_r60_2050_SS = brickfd_rcp60_2050ft + stationary_brick
bfd_r60_2060_SS = brickfd_rcp60_2060ft + stationary_brick
bfd_r60_2100_SS = brickfd_rcp60_2100ft + stationary_brick
# RCP85
bfd_r85_2030_SS = brickfd_rcp85_2030ft + stationary_brick
bfd_r85_2050_SS = brickfd_rcp85_2050ft + stationary_brick
bfd_r85_2060_SS = brickfd_rcp85_2060ft + stationary_brick
bfd_r85_2100_SS = brickfd_rcp85_2100ft + stationary_brick
bfd_r85_2030_SSN = brickfd_rcp85_2030ft + nonstat_brick2030
bfd_r85_2050_SSN = brickfd_rcp85_2050ft + nonstat_brick2050
bfd_r85_2060_SSN = brickfd_rcp85_2060ft + nonstat_brick2060
bfd_r85_2100_SSN = brickfd_rcp85_2100ft + nonstat_brick2100

#add storm surge to brick non fast dynamics
# RCP26
NOfd_r26_2030_SS = NO_fd_rcp26_2030ft + stationary_brick
NOfd_r26_2050_SS = NO_fd_rcp26_2050ft + stationary_brick
NOfd_r26_2060_SS = NO_fd_rcp26_2060ft + stationary_brick
NOfd_r26_2100_SS = NO_fd_rcp26_2100ft + stationary_brick
# RCP45
NOfd_r45_2030_SS = NO_fd_rcp45_2030ft + stationary_brick
NOfd_r45_2050_SS = NO_fd_rcp45_2050ft + stationary_brick
NOfd_r45_2060_SS = NO_fd_rcp45_2060ft + stationary_brick
NOfd_r45_2100_SS = NO_fd_rcp45_2100ft + stationary_brick
# RCP60
NOfd_r60_2030_SS = NO_fd_rcp60_2030ft + stationary_brick
NOfd_r60_2050_SS = NO_fd_rcp60_2050ft + stationary_brick
NOfd_r60_2060_SS = NO_fd_rcp60_2060ft + stationary_brick
NOfd_r60_2100_SS = NO_fd_rcp60_2100ft + stationary_brick
# RCP85
NOfd_r85_2030_SS = NO_fd_rcp85_2030ft + stationary_brick
NOfd_r85_2050_SS = NO_fd_rcp85_2050ft + stationary_brick
NOfd_r85_2060_SS = NO_fd_rcp85_2060ft + stationary_brick
NOfd_r85_2100_SS = NO_fd_rcp85_2100ft + stationary_brick
NOfd_r85_2030_SSN = NO_fd_rcp85_2030ft + nonstat_brick2030
NOfd_r85_2050_SSN = NO_fd_rcp85_2050ft + nonstat_brick2050
NOfd_r85_2060_SSN = NO_fd_rcp85_2060ft + nonstat_brick2060
NOfd_r85_2100_SSN = NO_fd_rcp85_2100ft + nonstat_brick2100

#add Tebadli et al 100-yr return period to kopp
# RCP26
k14_r26_2030_Teb = kopp14_rcp45_2030ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r26_2050_Teb = kopp14_rcp45_2050ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r26_2060_Teb = kopp14_rcp45_2060ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r26_2100_Teb = kopp14_rcp45_2100ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
# RCP45
k14_r45_2030_Teb = kopp14_rcp45_2030ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r45_2050_Teb = kopp14_rcp45_2050ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)] 
k14_r45_2060_Teb = kopp14_rcp45_2060ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r45_2100_Teb = kopp14_rcp45_2100ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
# RCP60
k14_r60_2030_Teb = kopp14_rcp60_2030ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r60_2050_Teb = kopp14_rcp60_2050ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)] 
k14_r60_2060_Teb = kopp14_rcp60_2060ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r60_2100_Teb = kopp14_rcp60_2100ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)] 
# RCP85
k14_r85_2030_Teb = kopp14_rcp85_2030ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r85_2050_Teb = kopp14_rcp85_2050ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)] 
k14_r85_2060_Teb = kopp14_rcp85_2060ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r85_2100_Teb = kopp14_rcp85_2100ft + tebaldi12$rl_50[which(tebaldi12$rp == 100)]


