##==============================================================================
## Edits by Kelsey Ruckert (klr324@psu.edu) Feb. 2018
## with the purpose of extracting probabilistic SLR data
## for Norfolk (Sewells Point).
##
## Original file can be found at:
## https://github.com/scrim-network/BRICK/blob/master/LocalizedProjections.ipynb
##
##==============================================================================
## Copyright 2016 Tony Wong, Alexander Bakker
## This file is part of BRICK (Building blocks for Relevant Ice and Climate
## Knowledge). BRICK is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## BRICK is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with BRICK.  If not, see <http://www.gnu.org/licenses/>.
##
##==============================================================================
## BRICK: Localized projections
## Make localized projections using results from Wong and Keller (2017, DOI: 10.1002/2017EF000607)
## Uses fingerprints of Slangen et al. (2014, DOI: TODO)
## User-defined latitude/longitude
## Result is a netCDF file of localized annual mean sea level
## Include or not include sudden, rapid sea-level contribution from the Antarctic ice sheet (AIS fast dynamics)
## Include any of RCP2.6, 4.5, 6.0 and 8.5
## Question? Tony Wong (anthony.e.wong@colorado.edu)
##==============================================================================
# Clone the BRICK repository
# git clone https://github.com/scrim-network/BRICK.git
# cd BRICK
# Make sure to snag the file: (Projections only to 2100)
# https://download.scrim.psu.edu/Wong_etal_BRICK/NOLA_Wong_etal_2017/output_model/BRICK_physical_fd-gamma_08May2017.nc

library(ncdf4)

# Set location
# Where would you like localized projections? Enter latitude and longitude as degrees north and degrees east.
lat <- 36+(56.8/60)          # Sewall Point tide gauge at 36° 56.8' N
lon <- -(76+(19.8/60))       # 76° 19.8' W (NOAA Tides and Currents website)

# AIS fast dynamics
# With or without Antarctic ice sheet fast dynamics? (As in Wong et al. (2017, DOI: 10.1007/s10584-017-2039-4))
use_fastdynamics <- TRUE     # Set to FALSE to get the data without fast dynamics

# BRICK output file
# Which BRICK output file would you like? (Defaults to Wong and Keller (2017)) This path is assumed relative to the BRICK home directory.
# The Nov. 20th analysis is from rerunning the Wong and Keller 2017 analysis extending projections to 2200.
# filename.brick <- 'output_model/BRICK_physical_fd-gamma_08May2017.nc'
filename.brick <- 'output_model/BRICK_physical_fd-gamma_20Nov2018.nc'

# Do a check to see if the requested file is local. If not, fetch from download server.
# If this doesn't work, then you make need to either (1) try different method for the download.file, or (2) just navigate to 
# that URL in your browser and find the relevant BRICK output file to download.
files <- list.files(recursive=TRUE)
ifile <- which(files==filename.brick)

# if(length(ifile)==0) {
#   download.file(paste('https://download.scrim.psu.edu/Wong_etal_BRICK/NOLA_Wong_etal_2017/',filename.brick, sep=''),
#                 filename.brick, method='curl')
# }

# Read the results file
# Edit here the rcps variable if you only want a particular subset of the RCP scenarios.
# make sure ncdf4 package is installed; install it, if not
if('ncdf4' %in% rownames(installed.packages()) == FALSE) {install.packages('ncdf4')}
library(ncdf4)

rcps <- c('RCP26','RCP45','RCP60','RCP85'); n.rcp <- length(rcps)
gmsl <- lsl <- ais <- gis <- gsic <- 
  te <- lws <- temp <- ocheat <- vector('list', n.rcp)
names(gmsl) <- names(lsl) <- names(ais) <- names(gis) <- names(gsic) <- 
  names(te) <- names(lws) <- names(temp) <- names(ocheat) <- rcps

ncdata <- nc_open(filename.brick)

for (rcp in rcps) {
  gmsl[[rcp]]   <- ncvar_get(ncdata, paste('GlobalSeaLevel_',rcp, sep=''))
  ais[[rcp]]    <- ncvar_get(ncdata, paste('AIS_',rcp, sep=''))
  gis[[rcp]]    <- ncvar_get(ncdata, paste('GIS_',rcp, sep=''))
  gsic[[rcp]]   <- ncvar_get(ncdata, paste('GSIC_',rcp, sep=''))
  te[[rcp]]     <- ncvar_get(ncdata, paste('TE_',rcp, sep=''))
  lws[[rcp]]    <- ncvar_get(ncdata, paste('LWS_',rcp, sep=''))
  temp[[rcp]]   <- ncvar_get(ncdata, paste('temp_',rcp, sep=''))
  ocheat[[rcp]] <- ncvar_get(ncdata, paste('ocheat_',rcp, sep=''))
  if(!use_fastdynamics) {
    gmsl_nofd   <- ncvar_get(ncdata, paste('GlobalSeaLevel_nofd_',rcp, sep=''))
    ais[[rcp]]  <- ais[[rcp]] - (gmsl[[rcp]] - gmsl_nofd)
    gmsl[[rcp]] <- gmsl_nofd
  }
}

t.proj     <- ncvar_get(ncdata, 'time_proj')
n.ensemble <- length(ncvar_get(ncdata, 'ens'))

nc_close(ncdata)

# Local fingerprinting
# Load the global fingerprinting data set (Slangen et al 2014)
filename.fingerprints = './fingerprints/FINGERPRINTS_SLANGEN_Bakker.nc'

ncdata  <- nc_open(filename.fingerprints)
lat.fp  <- ncvar_get(ncdata, 'lat')
lon.fp  <- ncvar_get(ncdata, 'lon')
fp.gsic <- ncvar_get(ncdata, 'GLAC')
fp.gis  <- ncvar_get(ncdata, 'GIS')
fp.ais  <- ncvar_get(ncdata, 'AIS')
nc_close(ncdata)

# Get local fingerprints
# Convert the given latitude and longitude to degrees East/North, and find the 
# fingerprinting data set location closest to the local sea level lat/lon given.
if(lon < 0) {lon <- lon + 360}  # convert longitude to degrees East
ilat <- which( abs(lat.fp-lat)==min(abs(lat.fp-lat)) )
ilon <- which( abs(lon.fp-lon)==min(abs(lon.fp-lon)) )

# It is possible there were multiple lats/lons 'closest' to your given point. In this case, 
# take the average of the ones that are actually numbers.
fp.ais.loc <- mean(fp.ais[ilon,ilat],na.rm=TRUE)
fp.gsic.loc <- mean(fp.gsic[ilon,ilat],na.rm=TRUE)
fp.gis.loc <- mean(fp.gis[ilon,ilat],na.rm=TRUE)
fp.te.loc <- 1.0       # TE response is to global mean temperature, so global mean sea level response is same everywhere
fp.lws.loc <- 1.0      # assume LWS changes uniformly (likely not quite true, but a small contribution anyway)

# Check if the nearest spot ended up on land. If it did, take the average everywhere around the location.
if(is.na(fp.ais.loc) | is.na(fp.gsic.loc) | is.na(fp.gis.loc) | is.na(fp.te.loc)) {
  fp.ais.loc <- mean(fp.ais[(ilon-1):(ilon+1),(ilat-1):(ilat+1)], na.rm=TRUE)
  fp.gsic.loc <- mean(fp.gsic[(ilon-1):(ilon+1),(ilat-1):(ilat+1)], na.rm=TRUE)
  fp.gis.loc <- mean(fp.gis[(ilon-1):(ilon+1),(ilat-1):(ilat+1)], na.rm=TRUE)
  fp.te.loc <- 1.0
  fp.lws.loc <- 1.0
}

# If there is still something wrong, probably the desired lat/lon location is accidentally not near the coast.
if(is.na(fp.ais.loc) | is.na(fp.gsic.loc) | is.na(fp.gis.loc) | is.na(fp.te.loc)) {
  print('WARNING -- local sea level fingerprints are NaN. Is the lat/lon point possibly over land?')
}

# Calculate local sea level
for (rcp in rcps) {
  lsl[[rcp]] <- mat.or.vec(nr=nrow(gmsl[[rcp]]), nc=ncol(gmsl[[rcp]]))
  for (sow in 1:n.ensemble) {
    lsl[[rcp]][,sow] <- fp.gis.loc  * gis[[rcp]][,sow]  +
      fp.ais.loc  * ais[[rcp]][,sow]  +
      fp.gsic.loc * gsic[[rcp]][,sow] +
      fp.te.loc   * te[[rcp]][,sow]   +
      fp.lws.loc  * lws[[rcp]][,sow]
  }
}

# Reference period
# Check that all sea-level projections and temperatures are relative to 1986-2005 means. Or change this to 
# whatever you want the reference period to be. Set reference_period to the beginning and ending years of 
# whatever reference period you would like! Must be within the 1850-2100 simulation years.
# reference.period <- c(1986, 2005)
# 
# ind.norm <- which(t.proj==reference.period[1]):which(t.proj==reference.period[2])

## NOTE: Kopp et al. 2014 results are relative to 2000.
## Therefore use that period to be commensurate with their results
reference.period <- c(2000, 2000)
ind.norm <- rep(which(t.proj==reference.period[1]), 2)

for (rcp in rcps) {
  lsl[[rcp]] <- lsl[[rcp]] - matrix(apply(X=lsl[[rcp]][ind.norm,], MARGIN=2, FUN=mean), 
                                    nrow=length(t.proj), ncol=n.ensemble, byrow=TRUE)
}

# Write output file.
# Set to output name based on whether the model incorporates fast dynamics.
# if(!use_fastdynamics) {
#   filename.output <- './output_model/BRICK_NOfastDynamics_SP_08May2017.nc'
# } else{
#   filename.output <- './output_model/BRICK_SewellsPoint_FastDynamics_08May2017.nc'
# }

if(!use_fastdynamics) {
  filename.output <- './output_model/BRICK_NOfastDynamics_SP_20Nov2018.nc'
} else{
  filename.output <- './output_model/BRICK_SewellsPoint_FastDynamics_20Nov2018.nc'
}

# Now create the actual file:
dim.tproj    <- ncdim_def('time_proj', 'years', as.double(t.proj))
dim.lat      <- ncdim_def('lat', 'deg N', as.double(length(lat)))
dim.lon      <- ncdim_def('lon', 'deg E', as.double(length(lon)))
dim.ensemble <- ncdim_def('ens', 'ensemble member', as.double(1:ncol(lsl[[1]])), unlim=TRUE)

def.lat <- ncvar_def('lat.lsl', 'deg N', list(dim.lat), -999, longname = 'latitude of local sea level point')
def.lon <- ncvar_def('lon.lsl', 'deg E', list(dim.lon), -999, longname = 'longitude of local sea level point')

def.lsl <- vector('list', n.rcp); names(def.lsl) <- rcps
for (rcp in rcps) {
  def.lsl[[rcp]] <- ncvar_def(paste('LocalSeaLevel_',rcp, sep=''), 'meters', 
                              list(dim.tproj, dim.ensemble), -999, longname = paste('Local sea level ',rcp, sep=''))
}

outlist = list()
for (i in 1:n.rcp) {
  outlist[[i]] <- def.lsl[[i]]
}
outlist[[i+1]] <- def.lat
outlist[[i+2]] <- def.lon

outnc <- nc_create(filename.output, outlist, force_v4=TRUE)

ncvar_put(outnc, def.lat, lat)
ncvar_put(outnc, def.lon, lon)
for (rcp in rcps) {
  ncvar_put(outnc, def.lsl[[rcp]], lsl[[rcp]])
}

nc_close(outnc)

##==============================================================================
## End
##==============================================================================
