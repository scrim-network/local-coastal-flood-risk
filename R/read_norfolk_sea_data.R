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
kopp14_rcp26 = convert_cm_to_ft(data.frame(t_2030 = kopp14_rcp26$X2030, t_2050 = kopp14_rcp26$X2050, t_2060 = kopp14_rcp26$X2060, t_2100 = kopp14_rcp26$X2100))

kopp14_rcp45 = read.csv("LSLproj_MC_299_rcp45.tsv.csv")
kopp14_rcp45 = convert_cm_to_ft(data.frame(t_2030 = kopp14_rcp45$X2030, t_2050 = kopp14_rcp45$X2050, t_2060 = kopp14_rcp45$X2060, t_2100 = kopp14_rcp45$X2100))

kopp14_rcp60 = read.csv("LSLproj_MC_299_rcp60.tsv.csv")
kopp14_rcp60 = convert_cm_to_ft(data.frame(t_2030 = kopp14_rcp60$X2030, t_2050 = kopp14_rcp60$X2050, t_2060 = kopp14_rcp60$X2060, t_2100 = kopp14_rcp60$X2100))

kopp14_rcp85 = read.csv("LSLproj_MC_299_rcp85.tsv.csv")
kopp14_rcp85 = convert_cm_to_ft(data.frame(t_2030 = kopp14_rcp85$X2030, t_2050 = kopp14_rcp85$X2050, t_2060 = kopp14_rcp85$X2060, t_2100 = kopp14_rcp85$X2100))

# Read in subsidence data for sewells point. This will be used
# to acoount for subsidence in the BRICK data.
kopp14_subsid = read.csv("LSLProj_bkgd_299_rcp26.csv")
kopp14_subsid = convert_cm_to_ft(data.frame(t_2030 = kopp14_subsid$X2030, t_2050 = kopp14_subsid$X2050, t_2060 = kopp14_subsid$X2060, t_2100 = kopp14_subsid$X2100))

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

# Convert to feet ---------------------------------------------------------
# Kopp and Wong & Keller have different numbers of samples. Using the Kopp data randomly 
# generate subsidence data to increase the sample size to match that of Wong & Keller
states_w = dim(lsl_fdyn_rcp26)[2]
BKkopp_subsid_2030 = rlnorm(states_w, meanlog=mean(log(kopp14_subsid$t_2030)), sdlog=sd(log(kopp14_subsid$t_2030)))
BKkopp_subsid_2050 = rlnorm(states_w, meanlog=mean(log(kopp14_subsid$t_2050)), sdlog=sd(log(kopp14_subsid$t_2050)))
BKkopp_subsid_2060 = rlnorm(states_w, meanlog=mean(log(kopp14_subsid$t_2060)), sdlog=sd(log(kopp14_subsid$t_2060)))
BKkopp_subsid_2100 = rlnorm(states_w, meanlog=mean(log(kopp14_subsid$t_2100)), sdlog=sd(log(kopp14_subsid$t_2100)))

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
# Read in data from the USACE Sea level calculator for Sewells Point
# This data includes: NOAA et al. 2012, USACE 2013, CARSWG 2016, and NOAA et al. 2017
# Data is ft above 1992, so 0cm is 1992
SL_calculator = read.csv("USACE_SL_Calculator_SewellsPoint.csv", skip=1, header=TRUE)
SL_calculator = as.matrix(SL_calculator)
SL_calculator_ref2000 = SL_calculator
for(i in 2:13){
  SL_calculator_ref2000[,i] = SL_calculator[,i] - SL_calculator[match(2000, SL_calculator[,1]),i]
}

NOAA_etal_2017 = read.csv("NOAA_etal_2017_SewellsPoint.csv", skip=1, header=TRUE) 
NOAA_etal_2017 = as.matrix(NOAA_etal_2017)
NOAA_etal_2017_ref2000 = NOAA_etal_2017
for(i in 2:22){
  NOAA_etal_2017_ref2000[,i] = NOAA_etal_2017[,i] - NOAA_etal_2017[match(2000, NOAA_etal_2017[,1]),i]
}

# Extract specific years --------------------------------------------------
match_SLC = match(c(2030,2050,2060,2100), SL_calculator[,1])
match_NOAA17 = match(c(2030,2050,2060,2100), NOAA_etal_2017[,1])

noaa2012 = data.frame(t_2030 = c(SL_calculator_ref2000[match_SLC[1], 2:5]), t_2050 = c(SL_calculator_ref2000[match_SLC[2], 2:5]),
                      t_2060 = c(SL_calculator_ref2000[match_SLC[3], 2:5]), t_2100 = c(SL_calculator_ref2000[match_SLC[4], 2:5]), row.names = NULL)

usace2013 = data.frame(t_2030 = c(SL_calculator_ref2000[match_SLC[1], 6:8]), t_2050 = c(SL_calculator_ref2000[match_SLC[2], 6:8]),
                      t_2060 = c(SL_calculator_ref2000[match_SLC[3], 6:8]), t_2100 = c(SL_calculator_ref2000[match_SLC[4], 6:8]), row.names = NULL)

carswg2016 = data.frame(t_2030 = c(SL_calculator_ref2000[match_SLC[1], 9:13]), t_2050 = c(SL_calculator_ref2000[match_SLC[2], 9:13]),
                       t_2060 = c(SL_calculator_ref2000[match_SLC[3], 9:13]), t_2100 = c(SL_calculator_ref2000[match_SLC[4], 9:13]), row.names = NULL)

# Medians are in columns 3,6,9,12,15,18, and 21; hence sequence from 3 to 21 by 3.
noaa2017 = data.frame(t_2030 = c(NOAA_etal_2017_ref2000[match_NOAA17[1], seq(3,21,3)]), t_2050 = c(NOAA_etal_2017_ref2000[match_NOAA17[2], seq(3,21,3)]),
                        t_2060 = c(NOAA_etal_2017_ref2000[match_NOAA17[3], seq(3,21,3)]), t_2100 = c(NOAA_etal_2017_ref2000[match_NOAA17[4], seq(3,21,3)]), row.names = NULL)

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
# processed_data = readRDS("processed_norfolk_data.rds")

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

# Random generation data in millimeters
stat_gev = revd(nrow(burn_stationary), loc = burn_stationary[,1], scale = burn_stationary[,2], 
                shape = burn_stationary[,3], type='GEV')
stat_gev25 = revd(1e5, loc = post_stat_25[1], scale = post_stat_25[2], shape = post_stat_25[3], type='GEV')
stat_gev975 = revd(1e5, loc = post_stat_975[1], scale = post_stat_975[2], shape = post_stat_975[3], type='GEV')

stat_gev = convert_mm_to_ft(stat_gev)
stat_gev25 = convert_mm_to_ft(stat_gev25)
stat_gev975 = convert_mm_to_ft(stat_gev975)

#--------- Combined SLR + Storm surge
# BRICK length stationary
subbrick_length = length(brickfd_rcp26_2030ft)
subbrick_stat = burn_stationary[sample(nrow(burn_stationary), size=subbrick_length, replace=FALSE), ]
stationary_brick = convert_mm_to_ft(revd(nrow(subbrick_stat), loc = subbrick_stat[,1], scale = subbrick_stat[,2], 
                      shape = subbrick_stat[,3], type='GEV'))
#KOPP length stationary
subkopp_length = length(kopp14_rcp26$t_2030)
subkopp_stat = burn_stationary[sample(nrow(burn_stationary), size=subkopp_length, replace=FALSE), ]
stationary_kopp = convert_mm_to_ft(revd(nrow(subkopp_stat), loc = subkopp_stat[,1], scale = subkopp_stat[,2], 
                      shape = subkopp_stat[,3], type='GEV'))

# add stationary storm surge to kopp
k14_r26_SS = kopp14_rcp26 + stationary_kopp
k14_r45_SS = kopp14_rcp45 + stationary_kopp
k14_r60_SS = kopp14_rcp60 + stationary_kopp
k14_r85_SS = kopp14_rcp85 + stationary_kopp

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

# add Tebadli et al 100-yr return period to kopp
k14_r26_Teb = kopp14_rcp26 + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r45_Teb = kopp14_rcp45 + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r60_Teb = kopp14_rcp60 + tebaldi12$rl_50[which(tebaldi12$rp == 100)]
k14_r85_Teb = kopp14_rcp85 + tebaldi12$rl_50[which(tebaldi12$rp == 100)]


