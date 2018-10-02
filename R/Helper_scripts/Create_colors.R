#######################################################################
#
# create_colors.R    18 April 2018
#
# Author: Kelsey Ruckert (klr324@psu.edu)
#
# Script creating and setting colors to use for plotting.
#
# To use this function, simply source this file:
#   source("create_colors.R")
#
# Version History:
#   1.0 - 18 April 2018 - Initial coding (K.L.R)
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
#######################################################################
library(RColorBrewer)

##=========================== CREATE COLORS ===================================
# Transparent Color Function 
makeTransparent<- function(someColor, alpha=100){
  #someColor = someColor
  newColor<-col2rgb(someColor)
  apply(newColor,2 ,
        function(curcoldata)
        {rgb(red=curcoldata[1],
             green=curcoldata[2],
             blue=curcoldata[3], alpha=alpha,
             maxColorValue=255)})
}
# Create sequence with black and white function.
seq_color = function(num, maincol){
  col_fun <- colorRampPalette(c("white", maincol, "black"))
  col_fun(num)
}

color_scheme = brewer.pal(12, "Set3")
PiYG = brewer.pal(11, "PiYG")
RdGy = brewer.pal(11, "RdGy")
col_grad = seq_color(150, RdGy[7:11])

# Use http://www.color-hex.com to create shades and tints
turquoise = c("#56beac", color_scheme[1], "#c4e8e2")
paleyellow = c("#ffff67", color_scheme[2], "#ffffe6")
lilac = c("#8f88bf", color_scheme[3], "#dddbec")  # 1
coral = c("#af594f", color_scheme[4], "#fca69c")  # 2
skyblue = c("#4c6a7e", color_scheme[5], "#b2d0e4") # 3
burntorange = c("#fc9016", color_scheme[6], "#fed8ae") # 4
grass = c("#7d9b49", color_scheme[7], "#c9e796") #
pinks = seq_color(9, PiYG[1:5])[3:8] # Colors for Sweet et al. (2017)
greys = c("#2b2b2b", "#565656", "#828282", "#adadad", color_scheme[9])
purples = c("#4b334b", "#835984", color_scheme[10], "#d0a6d0")
seafoam = c("#7a8d76", color_scheme[11], "#e0f3dc")
sunyellow = c("#ffe323", color_scheme[12], "#fff3a2") #5
browns = c("#382004", "#623807", "#ae8553", "#d1b99d")

# #   -----------------------------------------------------------------------
