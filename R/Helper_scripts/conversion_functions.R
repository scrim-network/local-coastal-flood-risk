#######################################################################
#
# conversion_functions.R    26 March 2017
#
# Author: Kelsey Ruckert (klr324@psu.edu)
#
# Functions that convert value units, datums, or etc.
#
# To use this function, simply source this file:
#   source("conversion_functions.R")
#
# Version History:
#   1.0 - 26 March 2017 - Initial coding (K.L.R)
#
# Note: I wrote this code because data are not always directly comparible
# and in order to compare them some type of conversion is needed.
# Rather than having to look up the conversion or write a function in every,
# I wrote this script to contain any conversion function I need.
#
# THIS CODE IS PROVIDED AS-IS WITH NO WARRANTY (NEITHER EXPLICIT
# NOR IMPLICIT).  I SHARE THIS CODE IN HOPES THAT IT IS USEFUL, 
# BUT I AM NOT LIABLE FOR THE BEHAVIOR OF THIS CODE IN YOUR OWN
# APPLICATION.  YOU ARE FREE TO SHARE THIS CODE SO LONG AS THE
# AUTHOR(S) AND VERSION HISTORY REMAIN INTACT.
#
# Function Name: conversion_functions.R
# Input: The original values
# Output: Converted values
#
#######################################################################

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

# Conversion of water level datums (in ft)
# At Sewells Point Tide gauge (Norfolk, VA)
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

convert_navd88_to_msl = function(navd88_levels){
  NAVD88 = 5.99
  MSL = 5.74
  navd88_levels + (NAVD88 - MSL)
}
#   -----------------------------------------------------------------------
