##==============================================================================
## Calculate_USACE_Parris_Hall_SLR_data.R
##
## Script uses the Nation Research Council 1987 model to create plausible 
## sea-level rise scenarios for Norfolk based on information listed in 
## USACE 2014, Parris et al. 2012, and Hall et al. 2016.
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
setwd('/Users/klr324/Documents/Data_LSL')

# Source Conversion functions.
source("local-costal-flood-risk/R/Helper_scripts/conversion_functions.R")

##================= Estimate scenarios with the NRC 1987 model =================
# National Resource council 1987 model 
nrc_model = function(alpha, beta, time){
  t_year = time-1992
  slr = alpha*t_year + beta*t_year^2
  return(slr)
}

time = 1992:2100

# Set local rate of relative sea-level change
rslr_alpha = 4.44/1000

# Set constant b (m/yr^2)
USACE14_beta = c(0, 2.71e-5, 1.13e-4)
Parris12_beta = c(0, 2.71e-5, 8.71e-5, 1.56e-4)
Hall16_beta = c(0, 2.71e-5, 7.00e-5, 1.13e-4, 1.56e-4)

# Estimate USACE 2014 scenarios
USACE14 = data.frame(low = time, int = time, high = time)
for(i in 1:length(USACE14_beta)){
  USACE14[ ,i] = nrc_model(rslr_alpha, USACE14_beta[i], time)
}
USACE14 = round(convert_m_to_ft(USACE14), 2)

# Estimate Parris et al. 2012 scenarios
Parris12 = data.frame(low = time, int_low = time, int_high = time, highest = time)
for(i in 1:length(Parris12_beta)){
  Parris12[ ,i] = nrc_model(rslr_alpha, Parris12_beta[i], time)
}
Parris12 = round(convert_m_to_ft(Parris12), 2)

# Estimate Hall et al. 2016 scenarios
Hall16 = data.frame(lowest = time, low = time, medium = time, high = time, highest = time)
for(i in 1:length(Hall16_beta)){
  Hall16[ ,i] = nrc_model(rslr_alpha, Hall16_beta[i], time)
}
Hall16 = round(convert_m_to_ft(Hall16), 2)
##==============================================================================
## End
##==============================================================================

