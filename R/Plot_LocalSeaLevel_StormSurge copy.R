##==============================================================================
## Plot_LocalSeaLevel_StormSurge.R
##
## Script reads in sea level and storm surge data from various studies/ datasets
## and plots them for comparison.
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
# setwd('/Users/klr324/Documents/Data_LSL/local-costal-flood-risk/R')

library(ncdf4)
library(extRemes)
library(RColorBrewer)
library(plotrix)
library(ash)
library(fields)

# Source survival function, function.
source("Helper_scripts/plot_sf.r")
source("Helper_scripts/plot_SLRcompare_PDF.R")
source("Helper_scripts/plot_SLRandStormSurge_SF.R")

# Source Conversion functions.
source("Helper_scripts/conversion_functions.R")

# Read in modified sea level and storm surge data.
source("ReadAnalysis_LocalSeaLevel_StormSurge.R")

##=========================== CREATE COLORS ===================================
source("Helper_scripts/Create_colors.R")

kopp14_col = skyblue
kopp17_DP16_col = lilac
brickfd_col = coral
NO_fd_col = sunyellow
Ras18_col = burntorange 

sweet17_col = pinks
hall16_col = greys
parris12_col = purples
usace14_col = turquoise

tebaldi12_col = browns
srikrishnan_col = greys
zervas13_col = paleyellow
obs_col = "black"

# Transparent colors with transparent color function
trans_kopp14_col = makeTransparent(kopp14_col, 150)
trans_kopp17_DP16_col = makeTransparent(kopp17_DP16_col, 150)
trans_brickfd_col = makeTransparent(brickfd_col, 150)
trans_NO_fd_col = makeTransparent(NO_fd_col, 150)
trans_Ras18_col = makeTransparent(Ras18_col, 150)

trans_sweet17_col = makeTransparent(sweet17_col, 150)
trans_hall16_col = makeTransparent(hall16_col, 150)
trans_parris12_col = makeTransparent(parris12_col, 150)
trans_usace14_col = makeTransparent(usace14_col, 150)

trans_tebaldi12_col = makeTransparent(tebaldi12_col, 150)
trans_srikrishnan_col = makeTransparent(srikrishnan_col, 150)
trans_zervas13_col = makeTransparent(zervas13_col, 150)

##=========================== PUBLICATION FIGURE SIZES ===================================
inches_to_dpi = function(inch){ inch * 300 }

text_column_width   = 5.2
minimum_width       = 2.63
full_page_width     = 7.5
full_page_height    = 8.75
single_panel_height = 4

##=========================== SLR PDF PLOTS ===================================
#---------------------------- 2030 -----------------------------------
pdf(file="../Figures/SLR_2030_2100.pdf", family="Times", width=full_page_width, height=full_page_height, pointsize=15)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4,
                5,6,7,
                5,6,7,
                8,9,10,
                8,9,10,
                11,12,13,
                11,12,13), 9, 3, byrow = TRUE))
# layout(matrix(c(1,2,3,
#                 4,5,6,
#                 7,8,9,
#                 10,11,12), 4, 3, byrow = TRUE))
# Add legend
par(mgp=c(1.5,.5,0), mar=c(0,3,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")

legend("topleft", legend=c("Wong & Keller 2017 FD", "Wong & Keller 2017 no FD", "Kopp et al. 2014", "Kopp et al. 2017", "Sweet et al. 2017",
                           "Rasmussen et al. 2018", "Parris et al. 2012", "USACE 2014", "Hall et al. 2016"),
       lty=c(1,1,1,1,NA,1,NA,NA,NA), lwd=c(2,2,2,2,NA,2,NA,NA,NA), pch=c(NA,NA,NA,NA,15,NA,19,19,19),
       pt.cex=c(NA,NA,NA,NA,2,NA,1,1,1), bty='n', ncol=3, 
       col=c(brickfd_col[1], NO_fd_col[1], kopp14_col[1], kopp17_DP16_col[1], sweet17_col[1],
             Ras18_col[1], parris12_col[2], usace14_col[1], hall16_col[1]))
 
gradient.rect(8.1,7.5,10.1,9, col=col_grad, gradient="x")
arrows(9.85, 6.3, 10.1, 6.3, length=0.075)
text(9.1,6.3, "Higher scenario")

#   -----------------------------------------------------------------------
# a) Sea-level rise probability density function low scenarios
par(mgp=c(1.5,.5,0), mar=c(3.5,3,1,0.5))
plot_SLRcompare_PDF(year = 2030, scen = "rcp26", deg = "1p5deg", sweet.scen = c("05", "03"), panel = "a.")

points(parris_etal_2012$t_2030[1], 6.5, col=parris12_col[4], pch=19)
points(usace2014$t_2030[1], 6, col=usace14_col[1], pch=19)
points(hall_etal_2016$t_2030[1], 5.5, col=hall16_col[5], pch=19)

#   -----------------------------------------------------------------------
# b) Sea-level rise probability density function medium scenarios
plot_SLRcompare_PDF(year = 2030, scen = "rcp45", deg = "2p0deg", sweet.scen = c("15", "10"), panel = "b.")

lines(hall_etal_2016$t_2030[2:3], rep(5.5, 2), col=hall16_col[4:3], lwd=1.5, lty=3)

points(parris_etal_2012$t_2030[2], 6.5, col=parris12_col[4], pch=19)
points(usace2014$t_2030[2], 6, col=usace14_col[2], pch=19)
points(hall_etal_2016$t_2030[2:3], rep(5.5, 2), col=hall16_col[4:3], pch=19)

#   -----------------------------------------------------------------------
# c) Sea-level rise probability density function high scenarios
par(mgp=c(1.5,0.5,0), mar=c(3.5,2.5,1,1))
plot_SLRcompare_PDF(year = 2030, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), panel = "c.")

lines(parris_etal_2012$t_2030[3:4], rep(6.5, 2), col=parris12_col[2:1], lwd=1.5, lty=3)
lines(hall_etal_2016$t_2030[4:5], rep(5.5, 2), col=hall16_col[2:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2030[3:4], rep(6.5, 2), col=parris12_col[2:1], pch=19)
points(usace2014$t_2030[3], 6, col=usace14_col[1], pch=19)
points(hall_etal_2016$t_2030[4:5], rep(5.5, 2), col=hall16_col[2:1], pch=19)

#---------------------------- 2050 -----------------------------------
# d) Sea-level rise probability density function low scenarios
par(mgp=c(1.5,.5,0), mar=c(3.5,3,1,0.5))
plot_SLRcompare_PDF(year = 2050, scen = "rcp26", deg = "1p5deg", sweet.scen = c("05", "03"), panel = "d.")

points(parris_etal_2012$t_2050[1], 3.25, col=parris12_col[4], pch=19)
points(usace2014$t_2050[1], 3, col=usace14_col[1], pch=19)
points(hall_etal_2016$t_2050[1], 2.75, col=hall16_col[5], pch=19)

#   -----------------------------------------------------------------------
# e) Sea-level rise probability density function medium scenarios
plot_SLRcompare_PDF(year = 2050, scen = "rcp45", deg = "2p0deg", sweet.scen = c("15", "10"), panel = "e.")

lines(hall_etal_2016$t_2050[2:3], rep(2.75, 2), col=hall16_col[4:3], lwd=1.5, lty=3)

points(parris_etal_2012$t_2050[2], 3.25, col=parris12_col[4], pch=19)
points(usace2014$t_2050[2], 3, col=usace14_col[2], pch=19)
points(hall_etal_2016$t_2050[2:3], rep(2.75, 2), col=hall16_col[4:3], pch=19)

#   -----------------------------------------------------------------------
# f) Sea-level rise probability density function high scenarios
par(mgp=c(1.5,0.5,0), mar=c(3.5,2.5,1,1))
plot_SLRcompare_PDF(year = 2050, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), panel = "f.")

lines(parris_etal_2012$t_2050[3:4], rep(3.25, 2), col=parris12_col[2:1], lwd=1.5, lty=3)
lines(hall_etal_2016$t_2050[4:5], rep(2.75, 2), col=hall16_col[2:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2050[3:4], rep(3.25, 2), col=parris12_col[2:1], pch=19)
points(usace2014$t_2050[3], 3, col=usace14_col[1], pch=19)
points(hall_etal_2016$t_2050[4:5], rep(2.75, 2), col=hall16_col[2:1], pch=19)

#---------------------------- 2070 -----------------------------------
# g) Sea-level rise probability density function low scenarios
par(mgp=c(1.5,.5,0), mar=c(3.5,3,1,0.5))
plot_SLRcompare_PDF(year = 2070, scen = "rcp26", deg = "1p5deg", sweet.scen = c("05", "03"), panel = "g.")

points(parris_etal_2012$t_2070[1], 2.75, col=parris12_col[4], pch=19)
points(usace2014$t_2070[1], 2.5, col=usace14_col[1], pch=19)
points(hall_etal_2016$t_2070[1], 2.25, col=hall16_col[5], pch=19)

#   -----------------------------------------------------------------------
# h) Sea-level rise probability density function medium scenarios
plot_SLRcompare_PDF(year = 2070, scen = "rcp45", deg = "2p0deg", sweet.scen = c("15", "10"), panel = "h.")

lines(hall_etal_2016$t_2070[2:3], rep(2.25, 2), col=hall16_col[4:3], lwd=1.5, lty=3)

points(parris_etal_2012$t_2070[2], 2.75, col=parris12_col[4], pch=19)
points(usace2014$t_2070[2], 2.5, col=usace14_col[2], pch=19)
points(hall_etal_2016$t_2070[2:3], rep(2.25, 2), col=hall16_col[4:3], pch=19)

#   -----------------------------------------------------------------------
# i) Sea-level rise probability density function high scenarios
par(mgp=c(1.5,0.5,0), mar=c(3.5,2.5,1,1))
plot_SLRcompare_PDF(year = 2070, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), panel = "i.")

lines(parris_etal_2012$t_2070[3:4], rep(2.75, 2), col=parris12_col[2:1], lwd=1.5, lty=3)
lines(hall_etal_2016$t_2070[4:5], rep(2.25, 2), col=hall16_col[2:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2070[3:4], rep(2.75, 2), col=parris12_col[2:1], pch=19)
points(usace2014$t_2070[3], 2.5, col=usace14_col[1], pch=19)
points(hall_etal_2016$t_2070[4:5], rep(2.25, 2), col=hall16_col[2:1], pch=19)

#---------------------------- 2100 -----------------------------------
# j) Sea-level rise probability density function low scenarios
par(mgp=c(1.5,.5,0), mar=c(3.5,3,1,0.5))
plot_SLRcompare_PDF(year = 2100, scen = "rcp26", deg = "1p5deg", sweet.scen = c("05", "03"), panel = "j.")

points(parris_etal_2012$t_2100[1], 1.45, col=parris12_col[4], pch=19)
points(usace2014$t_2100[1], 1.3, col=usace14_col[1], pch=19)
points(hall_etal_2016$t_2100[1], 1.15, col=hall16_col[5], pch=19)

#   -----------------------------------------------------------------------
# k) Sea-level rise probability density function medium scenarios
plot_SLRcompare_PDF(year = 2100, scen = "rcp45", deg = "2p0deg", sweet.scen = c("15", "10"), panel = "k.")

lines(hall_etal_2016$t_2100[2:3], rep(1.15, 2), col=hall16_col[4:3], lwd=1.5, lty=3)

points(parris_etal_2012$t_2100[2], 1.45, col=parris12_col[4], pch=19)
points(usace2014$t_2100[2], 1.3, col=usace14_col[2], pch=19)
points(hall_etal_2016$t_2100[2:3], rep(1.15, 2), col=hall16_col[4:3], pch=19)

#   -----------------------------------------------------------------------
# l) Sea-level rise probability density function high scenarios
par(mgp=c(1.5,0.5,0), mar=c(3.5,2.5,1,1))
plot_SLRcompare_PDF(year = 2100, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), panel = "l.")

lines(parris_etal_2012$t_2100[3:4], rep(1.45, 2), col=parris12_col[2:1], lwd=1.5, lty=3)
lines(hall_etal_2016$t_2100[4:5], rep(1.15, 2), col=hall16_col[2:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2100[3:4], rep(1.45, 2), col=parris12_col[2:1], pch=19)
points(usace2014$t_2100[3], 1.3, col=usace14_col[1], pch=19)
points(hall_etal_2016$t_2100[4:5], rep(1.15, 2), col=hall16_col[2:1], pch=19)

#   -----------------------------------------------------------------------
dev.off()
#   -----------------------------------------------------------------------

##=========================== RETURN PERIOD PLOTS OF COMBINED STORM SURGE AND SLR ===================================
pdf(file="../Figures/StormSurge_SLR_2030_2100.pdf", family="Times", width=full_page_width, height=single_panel_height*2, pointsize=12)

par(mfrow=c(2, 2), mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot_SLRandStormSurge_SF(year = 2030, panel = "a.")

legend("topleft", legend=c("Wong & Keller 2017 no FD","Wong & Keller 2017 FD", 
                           "Kopp et al. 2017", "Kopp et al. 2014"),
       lty=1, lwd=2, col=c(NO_fd_col[2], brickfd_col[2], kopp17_DP16_col[2], kopp14_col[2]), bty='n')

plot_SLRandStormSurge_SF(year = 2050, panel = "b.")
plot_SLRandStormSurge_SF(year = 2070, panel = "c.")
plot_SLRandStormSurge_SF(year = 2100, panel = "d.")

#   -----------------------------------------------------------------------
dev.off()
#   -----------------------------------------------------------------------

##=========================== MULTIPLE YEAR SLR DENSITY & PROJECTION PLOT ===================================
# Create functions to estimate projection or hindcast value associated with a quantile for each year
# This can be used to create credible intervals. There a two functions dependents on wheather to 
# loop through rows or columns.
percentile_projection_col = function(years, data, percentile){ # loop through the columns
  percent_proj = rep(NA, length(years))
  for(i in 1:length(years)){
    percent_proj[i] <- quantile(data[,i], percentile, na.rm=TRUE)
  }
  return(percent_proj)
}

percentile_projection_row = function(years, data, percentile){ # loop through the rows
  percent_proj = rep(NA, length(years))
  for(i in 1:length(years)){
    percent_proj[i] <- quantile(data[i, ], percentile, na.rm=TRUE)
  }
  return(percent_proj)
}

# Extract 90% CI
k14_26_5 = percentile_projection_col(k14_years, convert_cm_to_ft(kopp14_rcp26_dat), 0.05)
k14_45_5 = percentile_projection_col(k14_years, convert_cm_to_ft(kopp14_rcp45_dat), 0.05)
k14_85_5 = percentile_projection_col(k14_years, convert_cm_to_ft(kopp14_rcp85_dat), 0.05)
k14_26_95 = percentile_projection_col(k14_years, convert_cm_to_ft(kopp14_rcp26_dat), 0.95)
k14_45_95 = percentile_projection_col(k14_years, convert_cm_to_ft(kopp14_rcp45_dat), 0.95)
k14_85_95 = percentile_projection_col(k14_years, convert_cm_to_ft(kopp14_rcp85_dat), 0.95)

k17_DP16_SEW_26_5 = percentile_projection_col(k17_DP16_SEW_years, convert_cm_to_ft(kopp17_DP16_SEW_rcp26_dat), 0.05)
k17_DP16_SEW_45_5 = percentile_projection_col(k17_DP16_SEW_years, convert_cm_to_ft(kopp17_DP16_SEW_rcp45_dat), 0.05)
k17_DP16_SEW_85_5 = percentile_projection_col(k17_DP16_SEW_years, convert_cm_to_ft(kopp17_DP16_SEW_rcp85_dat), 0.05)
k17_DP16_SEW_26_95 = percentile_projection_col(k17_DP16_SEW_years, convert_cm_to_ft(kopp17_DP16_SEW_rcp26_dat), 0.95)
k17_DP16_SEW_45_95 = percentile_projection_col(k17_DP16_SEW_years, convert_cm_to_ft(kopp17_DP16_SEW_rcp45_dat), 0.95)
k17_DP16_SEW_85_95 = percentile_projection_col(k17_DP16_SEW_years, convert_cm_to_ft(kopp17_DP16_SEW_rcp85_dat), 0.95)

lsl_fdyn_26_5 = percentile_projection_row(t.time, lsl_fdyn_rcp26_sub, 0.05)
lsl_fdyn_45_5 = percentile_projection_row(t.time, lsl_fdyn_rcp45_sub, 0.05)
lsl_fdyn_85_5 = percentile_projection_row(t.time, lsl_fdyn_rcp85_sub, 0.05)
lsl_fdyn_26_95 = percentile_projection_row(t.time, lsl_fdyn_rcp26_sub, 0.95)
lsl_fdyn_45_95 = percentile_projection_row(t.time, lsl_fdyn_rcp45_sub, 0.95)
lsl_fdyn_85_95 = percentile_projection_row(t.time, lsl_fdyn_rcp85_sub, 0.95)

NO_fdyn_26_5 = percentile_projection_row(t.time, NO_fdyn_rcp26_sub, 0.05)
NO_fdyn_45_5 = percentile_projection_row(t.time, NO_fdyn_rcp45_sub, 0.05)
NO_fdyn_85_5 = percentile_projection_row(t.time, NO_fdyn_rcp85_sub, 0.05)
NO_fdyn_26_95 = percentile_projection_row(t.time, NO_fdyn_rcp26_sub, 0.95)
NO_fdyn_45_95 = percentile_projection_row(t.time, NO_fdyn_rcp45_sub, 0.95)
NO_fdyn_85_95 = percentile_projection_row(t.time, NO_fdyn_rcp85_sub, 0.95)

Ras18_SEW_2p5deg_5 = percentile_projection_col(Ras18_SEW_years, convert_cm_to_ft(Ras18_SEW_2p5deg_dat), 0.05)
Ras18_SEW_2p0deg_5 = percentile_projection_col(Ras18_SEW_years, convert_cm_to_ft(Ras18_SEW_2p0deg_dat), 0.05)
Ras18_SEW_1p5deg_5 = percentile_projection_col(Ras18_SEW_years, convert_cm_to_ft(Ras18_SEW_1p5deg_dat), 0.05)
Ras18_SEW_2p5deg_95 = percentile_projection_col(Ras18_SEW_years, convert_cm_to_ft(Ras18_SEW_2p5deg_dat), 0.95) 
Ras18_SEW_2p0deg_95 = percentile_projection_col(Ras18_SEW_years, convert_cm_to_ft(Ras18_SEW_2p0deg_dat), 0.95)
Ras18_SEW_1p5deg_95 = percentile_projection_col(Ras18_SEW_years, convert_cm_to_ft(Ras18_SEW_1p5deg_dat), 0.95)

sweet17_03_5 = percentile_projection_col(sweet17_10[1,], sweet17_03, 0.05)
sweet17_05_5 = percentile_projection_col(sweet17_10[1,], sweet17_05, 0.05)
sweet17_10_5 = percentile_projection_col(sweet17_10[1,], sweet17_10, 0.05)
sweet17_15_5 = percentile_projection_col(sweet17_10[1,], sweet17_15, 0.05)
sweet17_20_5 = percentile_projection_col(sweet17_10[1,], sweet17_20, 0.05)
sweet17_25_5 = percentile_projection_col(sweet17_10[1,], sweet17_25, 0.05)
sweet17_03_95 = percentile_projection_col(sweet17_10[1,], sweet17_03, 0.95)
sweet17_05_95 = percentile_projection_col(sweet17_10[1,], sweet17_05, 0.95)
sweet17_10_95 = percentile_projection_col(sweet17_10[1,], sweet17_10, 0.95)
sweet17_15_95 = percentile_projection_col(sweet17_10[1,], sweet17_15, 0.95)
sweet17_20_95 = percentile_projection_col(sweet17_10[1,], sweet17_20, 0.95)
sweet17_25_95 = percentile_projection_col(sweet17_10[1,], sweet17_25, 0.95)

#   -----------------------------------------------------------------------
pdf(file="../Figures/CI_stormsurge_slr.pdf", family="Times", width=full_page_width, height=single_panel_height*2, pointsize=12)
par(mfrow=c(2, 1), mgp=c(1.5,.5,0), mar=c(3.5,3,1,0.5))

# a) Sea-level rise projections
plot(0, type="n",xlab="Year", ylab="Projected sea level (ft)", ylim=c(0,13), xlim=c(2010, 2098), xaxt="n")
title(main="a.", adj=0)
axis(1, lwd = 1, at=seq(2010,2100, 10), label=seq(2010,2100, 10))

# Only plot RCP 8.5
polygon(y = c(sweet17_25_5, rev(sweet17_25_95)), x = c(seq(2000, 2200, 10), rev(seq(2000, 2200, 10))), col = trans_sweet17_col[1], border = NA)
polygon(y = c(sweet17_20_5, rev(sweet17_20_95)), x = c(seq(2000, 2200, 10), rev(seq(2000, 2200, 10))), col = trans_sweet17_col[2], border = NA)
polygon(y = c(k14_85_5, rev(k14_85_95)), x = c(k14_years, rev(k14_years)), col = trans_kopp14_col[1], border = NA)
polygon(y = c(k17_DP16_SEW_85_5, rev(k17_DP16_SEW_85_95)), x = c(k17_DP16_SEW_years, rev(k17_DP16_SEW_years)), col = trans_kopp17_DP16_col[1], border = NA)
polygon(y = c(Ras18_SEW_2p5deg_5, rev(Ras18_SEW_2p5deg_95)), x = c(Ras18_SEW_years, rev(Ras18_SEW_years)), col = trans_Ras18_col[1], border = NA)
polygon(y = c(lsl_fdyn_85_5, rev(lsl_fdyn_85_95)), x = c(t.time, rev(t.time)), col = trans_brickfd_col[1], border = NA)
polygon(y = c(NO_fdyn_85_5, rev(NO_fdyn_85_95)), x = c(t.time, rev(t.time)), col = trans_NO_fd_col[1], border = NA)

legend("topleft", legend = c("Sweet et al. 2.0 90% CI", "Sweet et al. 2.5 90% CI", "Kopp et al. 2014 RCP85 90% CI", 
                           "Kopp et al. 2017 RCP85 90% CI", "Rasmussen et al. 2018 2.5 90% CI", 
                           "Wong & Keller 2017 FD RCP85 90% CI", "Wong & Keller 2017 no FD RCP85 90% CI"), pch = 22, 
       bty='n', pt.bg = c(trans_sweet17_col[1], trans_sweet17_col[2], trans_kopp14_col[1], trans_kopp17_DP16_col[1], 
                          trans_Ras18_col[1], trans_brickfd_col[1], trans_NO_fd_col[1]), pt.cex = 2)
#   -----------------------------------------------------------------------
# b) Storm surge return period 
plot(1/NOAA_methodGEV$aep, NOAA_methodGEV$return_level, log = "x", type = "n", xlim = c(1, 500),
     ylim = c(2.85, 18), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Storm surge (ft MSL)")
title(main="b.", adj=0)
axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))

SF_Srikrishnan_stationary25 = plot.sf(stat_gev25, make.plot=FALSE)
SF_Srikrishnan_stationary975 = plot.sf(stat_gev975, make.plot=FALSE)
polygon(y = c(SF_Srikrishnan_stationary25$sf.num, rev(SF_Srikrishnan_stationary975$sf.num)), 
        x = c(1/SF_Srikrishnan_stationary25$sf, rev(1/SF_Srikrishnan_stationary975$sf)), col = trans_srikrishnan_col[5], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi12_col[2], border = NA)

polygon(y = c(zervas_2013$min_95[1:4], rev(zervas_2013$max_95[1:4])), 
        x = c(1/zervas_2013$aep[1:4], rev(1/zervas_2013$aep[1:4])), col = trans_zervas13_col[2], border = NA)
points(NOAA_methodGEV$return_obs, NOAA_methodGEV$obs, pch = 19, col=obs_col)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=srikrishnan_col[2], lwd=2)

lines(zervas_2013$NOAA_rp, zervas_2013$NOAA_rl_feet, lwd=2, col=zervas13_col[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi12_col[2])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=usace14_col[2])

legend("topleft", legend=c("Srikrishnan et al. in prep. 95% CI", "Srikrishnan et al. in prep. MLE",
                           "Tebaldi et al. 2012 95% CI", "Tebaldi et al. 2012 MLE",
                           "Zervas 2013 95% CI", "Zervas 2013 MLE", 
                           "USACE 2014", "Observations"),
       lty=c(NA,1,NA,1,NA,1,NA,NA), lwd=c(NA,2,NA,2,NA,2,NA,NA), pch=c(22,NA,22,NA,22,NA,19,19), 
       col=c("black", srikrishnan_col[2], "black", tebaldi12_col[2], "black", zervas13_col[2], usace14_col[2], obs_col),
       bty='n', pt.bg=c(trans_srikrishnan_col[5],NA,trans_tebaldi12_col[2],NA,trans_zervas13_col[2],NA,NA,NA), pt.cex = c(2,NA,2,NA,2,NA,1,1))

#   -----------------------------------------------------------------------
dev.off()
#   -----------------------------------------------------------------------

##==============================================================================
## End
##==============================================================================
