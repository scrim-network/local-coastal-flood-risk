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
# setwd('/Users/klr324/Documents/Data_LSL')

library(ncdf4)
library(extRemes)
library(RColorBrewer)
library(plotrix)
library(ash)
library(fields)

# Source survival function, function.
source("Helper_scripts/plot_sf.r")

# Source Conversion functions.
source("Helper_scripts/conversion_functions.R")

# Read in modified sea level and storm surge data.
source("ReadAnalysis_LocalSeaLevel_StormSurge.R")

##=========================== CREATE COLORS ===================================
# # Transparent Color Function 
# makeTransparent<- function(someColor, alpha=100){
#   #someColor = someColor
#   newColor<-col2rgb(someColor)
#   apply(newColor,2 ,
#         function(curcoldata)
#         {rgb(red=curcoldata[1],
#              green=curcoldata[2],
#              blue=curcoldata[3], alpha=alpha,
#              maxColorValue=255)})
# }
# # Create sequence with black and white function.
# seq_color = function(num, maincol){
#   col_fun <- colorRampPalette(c("white", maincol, "black"))
#   col_fun(num)
# }
# 
# RdGy = brewer.pal(11, "RdGy")
# BrBG = brewer.pal(11, "BrBG")
# RdBu = brewer.pal(11, "RdBu")
# PRGn = brewer.pal(11, "PRGn")
# PiYG = brewer.pal(11, "PiYG")
# Greens = brewer.pal(9, "Greens")
# tebaldi_gold = c("#f0cf0c", "#fcf5ce")
# 
# sweet_cols = seq_color(9, PiYG[1:5])
# col_grad = seq_color(150, RdGy[7:11])
# 
# trans_RdGy = makeTransparent(RdGy, 100)
# trans_BrBG = makeTransparent(BrBG, 100)
# trans_RdBu = makeTransparent(RdBu, 100)
# trans_PRGn = makeTransparent(PRGn, 100)
# trans_PiYG = makeTransparent(PiYG, 100)
# trans_Greens = makeTransparent(Greens, 100)
# trans_sweet_cols = makeTransparent(sweet_cols, 100)
# trans_tebaldi_gold = makeTransparent(tebaldi_gold, 200)

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

##=========================== SLR / STORM SURGE / COMBINED PLOTS ===================================
#---------------------------- 2030 -----------------------------------
pdf(file="../Figures/SLR_2030a.pdf", family="Times", width=full_page_width, height=single_panel_height, pointsize=14)
# layout(matrix(c(1,1,1,
#                 2,3,4,
#                 2,3,4,
#                 2,3,4), 4, 3, byrow = TRUE))
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4), 3, 3, byrow = TRUE))
# Add legend
par(mgp=c(1.5,.5,0), mar=c(0,4,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Tebaldi et al. 2012\nexpected value", "Sweet et al. 2017", 
                           "Parris et al. 2012","Tebaldi et al. 2012\n95% CI", "USACE 2014", 
                           "Srikrishnan et al.\nin prep. 95% CI", "Zervas 2013\nexpected value", "Hall et al. 2016", "Srikrishnan et al.\nin prep.",
                           "Zervas 2013\n 95% CI", "Observations"),
       lty=c(1,1,1,1,NA,NA,NA,NA,NA,1,NA,1,NA, NA), lwd=c(2,2,2,2,NA,NA,NA,NA,NA,2,NA,2,NA, NA), pch=c(NA,NA,NA,NA,19,19,22,19,22,NA,19,NA,22,19), 
       col=c(brickfd_col[2], kopp14_col[2], NO_fd_col[2], tebaldi12_col[2], sweet17_col[2], parris12_col[2], "black", usace14_col[2], "black", zervas13_col[2], hall16_col[3], srikrishnan_col[2], "black", obs_col),
       bty='n', ncol=5, pt.bg=c(NA,NA,NA,NA,NA,NA,trans_tebaldi12_col[2],NA,trans_srikrishnan_col[5],NA,NA,NA, trans_zervas13_col[2],NA), pt.cex = c(NA,NA,NA,NA,1,1,2,1,2,NA,1,NA,2, 1))
gradient.rect(7,2.5,9,4, col=col_grad, gradient="x")
arrows(8.75, 1.5, 9, 1.5, length=0.075)
text(8,1.5, "Higher scenario")

#   -----------------------------------------------------------------------
# a) Sea-level rise probability density function
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85$t_2030), xlab="Projected sea level in 2030 (ft)", ylab="Probability density", yaxt="n", 
     main="",col=kopp14_col[1], lwd=2, xlim=c(-0.3,2), ylim = c(0, 6.75), bty="l")
title(main="a.", adj=0)
# lines(density(kopp14_rcp60$t_2030), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2030), col=kopp14_col[2], lwd=2)
lines(density(kopp14_rcp26$t_2030), col=kopp14_col[3], lwd=2)

lines(density(kopp17_DP16_SEW_rcp85$t_2030), col=kopp17_DP16_col[1], lwd=2)
# lines(density(kopp17_DP16_SEW_rcp60$t_2030), col=Greens[6], lwd=2)
lines(density(kopp17_DP16_SEW_rcp45$t_2030), col=kopp17_DP16_col[2], lwd=2)
lines(density(kopp17_DP16_SEW_rcp26$t_2030), col=kopp17_DP16_col[3], lwd=2)

lines(density(brickfd_rcp85$t_2030), col=brickfd_col[1], lwd=2)
# lines(density(brickfd_rcp60$t_2030), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2030), col=brickfd_col[2], lwd=2)
lines(density(brickfd_rcp26$t_2030), col=brickfd_col[3], lwd=2)

lines(density(NO_fdft_rcp85$t_2030), col=NO_fd_col[1], lwd=2)
# lines(density(NO_fdft_rcp60$t_2030), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2030), col=NO_fd_col[2], lwd=2)
lines(density(NO_fdft_rcp26$t_2030), col=NO_fd_col[3], lwd=2)

lines(density(Ras18_SEW_2p5deg$t_2030), col=Ras18_col[1], lwd=2)
lines(density(Ras18_SEW_2p0deg$t_2030), col=Ras18_col[2], lwd=2)
lines(density(Ras18_SEW_1p5deg$t_2030), col=Ras18_col[3], lwd=2)

lines(density(sweet17_03$X2030), col=sweet17_col[6], lwd=2)
lines(density(sweet17_05$X2030), col=sweet17_col[5], lwd=2)
lines(density(sweet17_10$X2030), col=sweet17_col[4], lwd=2)
lines(density(sweet17_15$X2030, na.rm=TRUE), col=sweet17_col[3], lwd=2)
lines(density(sweet17_20$X2030, na.rm=TRUE), col=sweet17_col[2], lwd=2)
lines(density(sweet17_25$X2030, na.rm=TRUE), col=sweet17_col[1], lwd=2)

lines(parris_etal_2012$t_2030, rep(6.5, 4), col=parris12_col[4:1], lwd=1.5, lty=3)
lines(usace2014$t_2030, rep(6, 3), col=usace14_col[3:1], lwd=1.5, lty=3)
lines(hall_etal_2016$t_2030, rep(5.5, 5), col=hall16_col[5:1], lwd=1.5, lty=3)
lines(sweet2017$t_2030, rep(5, length(sweet2017$t_2030)), col=sweet17_col[6:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2030, rep(6.5, 4), col=parris12_col[4:1], pch=19)
points(usace2014$t_2030, rep(6, 3), col=usace14_col[3:1], pch=19)
points(hall_etal_2016$t_2030, rep(5.5, 5), col=hall16_col[5:1], pch=19)
points(sweet2017$t_2030, rep(5, length(sweet2017$t_2030)), col=sweet17_col[6:1], pch=19)

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

polygon(y = c(Ras18_CBBT_SS_q025$height, rev(Ras18_CBBT_SS_q975$height)), 
        x = c(1/Ras18_CBBT_SS_q025$freq, rev(1/Ras18_CBBT_SS_q975$freq)), col = trans_Ras18_col[2], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi12_col[2], border = NA)

polygon(y = c(zervas_2013$min_95[1:4], rev(zervas_2013$max_95[1:4])), 
        x = c(1/zervas_2013$aep[1:4], rev(1/zervas_2013$aep[1:4])), col = trans_zervas13_col[2], border = NA)
points(NOAA_methodGEV$return_obs, NOAA_methodGEV$obs, pch = 19, col=obs_col)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=srikrishnan_col[2], lwd=2)

lines(1/Ras18_CBBT_SS_q50$freq, Ras18_CBBT_SS_q50$height, lwd=2, col=Ras18_col[2])
lines(1/Ras18_CBBT_SS_parun$freq, Ras18_CBBT_SS_parun$height, lwd=2, col=Ras18_col[1])

lines(zervas_2013$NOAA_rp, zervas_2013$NOAA_rl_feet, lwd=2, col=zervas13_col[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi12_col[2])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=usace14_col[2])

#   -----------------------------------------------------------------------
# c) Combined sea level and storm surge return period 
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(1/NOAA_methodGEV$aep, NOAA_methodGEV$return_level, log = "x", type = "n", xlim = c(1, 500),
     ylim = c(2.85, 18), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Projected sea+surge level (ft MSL)")
title(main="c.", adj=0)
axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))

SF_k14_r85_2030_SS = plot.sf(k14_r85_SS$t_2030, make.plot=FALSE)
# SF_k14_r60_2030_SS = plot.sf(k14_r60_SS$t_2030, make.plot=FALSE)
SF_k14_r45_2030_SS = plot.sf(k14_r45_SS$t_2030, make.plot=FALSE)
SF_k14_r26_2030_SS = plot.sf(k14_r26_SS$t_2030, make.plot=FALSE)
lines(1/SF_k14_r85_2030_SS$sf, SF_k14_r85_2030_SS$sf.num, col=kopp14_col[1], lwd=1.5)
# lines(1/SF_k14_r60_2030_SS$sf, SF_k14_r60_2030_SS$sf.num, col=RdGy[2], lwd=1.5)
lines(1/SF_k14_r45_2030_SS$sf, SF_k14_r45_2030_SS$sf.num, col=kopp14_col[2], lwd=1.5)
lines(1/SF_k14_r26_2030_SS$sf, SF_k14_r26_2030_SS$sf.num, col=kopp14_col[3], lwd=1.5)

SF_k17_DP16_SEW_r85_2030_SS = plot.sf(k17_DP16_SEW_r85_SS$t_2030, make.plot=FALSE)
# SF_k17_DP16_SEW_r60_2030_SS = plot.sf(k17_DP16_SEW_r60_SS$t_2030, make.plot=FALSE)
SF_k17_DP16_SEW_r45_2030_SS = plot.sf(k17_DP16_SEW_r45_SS$t_2030, make.plot=FALSE)
SF_k17_DP16_SEW_r26_2030_SS = plot.sf(k17_DP16_SEW_r26_SS$t_2030, make.plot=FALSE)
lines(1/SF_k17_DP16_SEW_r85_2030_SS$sf, SF_k17_DP16_SEW_r85_2030_SS$sf.num, col=kopp17_DP16_col[1], lwd=1.5)
# lines(1/SF_k17_DP16_SEW_r60_2030_SS$sf, SF_k17_DP16_SEW_r60_2030_SS$sf.num, col=Greens[6], lwd=1.5)
lines(1/SF_k17_DP16_SEW_r45_2030_SS$sf, SF_k17_DP16_SEW_r45_2030_SS$sf.num, col=kopp17_DP16_col[2], lwd=1.5)
lines(1/SF_k17_DP16_SEW_r26_2030_SS$sf, SF_k17_DP16_SEW_r26_2030_SS$sf.num, col=kopp17_DP16_col[3], lwd=1.5)

SF_bfd_r85_2030_SS = plot.sf(bfd_r85_SS$t_2030, make.plot=FALSE)
# SF_bfd_r60_2030_SS = plot.sf(bfd_r60_SS$t_2030, make.plot=FALSE)
SF_bfd_r45_2030_SS = plot.sf(bfd_r45_SS$t_2030, make.plot=FALSE)
SF_bfd_r26_2030_SS = plot.sf(bfd_r26_SS$t_2030, make.plot=FALSE)
lines(1/SF_bfd_r85_2030_SS$sf, SF_bfd_r85_2030_SS$sf.num, col=brickfd_col[1], lwd=1.5)
# lines(1/SF_bfd_r60_2030_SS$sf, SF_bfd_r60_2030_SS$sf.num, col=RdBu[10], lwd=1.5)
lines(1/SF_bfd_r45_2030_SS$sf, SF_bfd_r45_2030_SS$sf.num, col=brickfd_col[2], lwd=1.5)
lines(1/SF_bfd_r26_2030_SS$sf, SF_bfd_r26_2030_SS$sf.num, col=brickfd_col[3], lwd=1.5)

SF_NOfd_r85_2030_SS = plot.sf(NOfd_r85_SS$t_2030, make.plot=FALSE)
# SF_NOfd_r60_2030_SS = plot.sf(NOfd_r60_SS$t_2030, make.plot=FALSE)
SF_NOfd_r45_2030_SS = plot.sf(NOfd_r45_SS$t_2030, make.plot=FALSE)
SF_NOfd_r26_2030_SS = plot.sf(NOfd_r26_SS$t_2030, make.plot=FALSE)
lines(1/SF_NOfd_r85_2030_SS$sf, SF_NOfd_r85_2030_SS$sf.num, col=NO_fd_col[1], lwd=1.5)
# lines(1/SF_NOfd_r60_2030_SS$sf, SF_NOfd_r60_2030_SS$sf.num, col=PRGn[3], lwd=1.5)
lines(1/SF_NOfd_r45_2030_SS$sf, SF_NOfd_r45_2030_SS$sf.num, col=NO_fd_col[2], lwd=1.5)
lines(1/SF_NOfd_r26_2030_SS$sf, SF_NOfd_r26_2030_SS$sf.num, col=NO_fd_col[3], lwd=1.5)

dev.off()

#---------------------------- 2050 -----------------------------------
pdf(file="../Figures/SLR_2050a.pdf", family="Times", width=full_page_width, height=single_panel_height, pointsize=14)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4), 3, 3, byrow = TRUE))
# Add legend
par(mgp=c(1.5,.5,0), mar=c(0,4,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Tebaldi et al. 2012\nexpected value", "Sweet et al. 2017", 
                           "Parris et al. 2012","Tebaldi et al. 2012\n95% CI", "USACE 2014", 
                           "Srikrishnan et al.\nin prep. 95% CI", "Zervas 2013\nexpected value", "Hall et al. 2016", "Srikrishnan et al.\nin prep.",
                           "Zervas 2013\n 95% CI", "Observations"),
       lty=c(1,1,1,1,NA,NA,NA,NA,NA,1,NA,1,NA, NA), lwd=c(2,2,2,2,NA,NA,NA,NA,NA,2,NA,2,NA, NA), pch=c(NA,NA,NA,NA,19,19,22,19,22,NA,19,NA,22,19), 
       col=c(RdBu[9], sunyellow[2], PRGn[3], tebaldi_gold[1], sweet_cols[5], BrBG[2],"black", BrBG[9], "black", BrBG[2], RdGy[9], RdBu[11], "black", "black"),
       bty='n', ncol=5, pt.bg=c(NA,NA,NA,NA,NA,NA,tebaldi_gold[2],NA,trans_RdBu[9],NA,NA,NA, trans_BrBG[2],NA), pt.cex = c(NA,NA,NA,NA,1,1,2,1,2,NA,1,NA,2, 1))
gradient.rect(7,2.5,9,4, col=col_grad, gradient="x")
arrows(8.75, 1.5, 9, 1.5, length=0.075)
text(8,1.5, "Higher scenario")

#   -----------------------------------------------------------------------
# a) Sea-level rise probability density function
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85$t_2050), xlab="Projected sea level in 2050 (ft)", ylab="Probability density", yaxt="n", 
     main="", col=kopp14_col[1], lwd=2, xlim=c(-0.2,4), ylim = c(0, 3.5), bty="l")
title(main="a.", adj=0)
# lines(density(kopp14_rcp60$t_2050), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2050), col=kopp14_col[2], lwd=2)
lines(density(kopp14_rcp26$t_2050), col=kopp14_col[3], lwd=2)

lines(density(kopp17_DP16_SEW_rcp85$t_2050), col=kopp17_DP16_col[1], lwd=2)
# lines(density(kopp17_DP16_SEW_rcp60$t_2050), col=Greens[6], lwd=2)
lines(density(kopp17_DP16_SEW_rcp45$t_2050), col=kopp17_DP16_col[2], lwd=2)
lines(density(kopp17_DP16_SEW_rcp26$t_2050), col=kopp17_DP16_col[3], lwd=2)

lines(density(brickfd_rcp85$t_2050), col=brickfd_col[1], lwd=2)
# lines(density(brickfd_rcp60$t_2050), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2050), col=brickfd_col[2], lwd=2)
lines(density(brickfd_rcp26$t_2050), col=brickfd_col[3], lwd=2)

lines(density(NO_fdft_rcp85$t_2050), col=NO_fd_col[1], lwd=2)
# lines(density(NO_fdft_rcp60$t_2050), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2050), col=NO_fd_col[2], lwd=2)
lines(density(NO_fdft_rcp26$t_2050), col=NO_fd_col[3], lwd=2)

lines(density(Ras18_SEW_2p5deg$t_2050), col=Ras18_col[1], lwd=2)
lines(density(Ras18_SEW_2p0deg$t_2050), col=Ras18_col[2], lwd=2)
lines(density(Ras18_SEW_1p5deg$t_2050), col=Ras18_col[3], lwd=2)

lines(density(sweet17_03$X2050), col=sweet17_col[6], lwd=2)
lines(density(sweet17_05$X2050), col=sweet17_col[5], lwd=2)
lines(density(sweet17_10$X2050), col=sweet17_col[4], lwd=2)
lines(density(sweet17_15$X2050, na.rm=TRUE), col=sweet17_col[3], lwd=2)
lines(density(sweet17_20$X2050, na.rm=TRUE), col=sweet17_col[2], lwd=2)
lines(density(sweet17_25$X2050, na.rm=TRUE), col=sweet17_col[1], lwd=2)

lines(parris_etal_2012$t_2050, rep(3.25, 4), col=parris12_col[4:1], lwd=2, lty=3)
lines(usace2014$t_2050, rep(3, 3), col=usace14_col[3:1], lwd=2, lty=3)
lines(hall_etal_2016$t_2050, rep(2.75, 5), col=hall16_col[5:1], lwd=2, lty=3)
lines(sweet2017$t_2050, rep(2.5, length(sweet2017$t_2050)), col=sweet17_col[6:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2050, rep(3.25, 4), col=parris12_col[4:1], pch=19)
points(usace2014$t_2050, rep(3, 3), col=usace14_col[3:1], pch=19)
points(hall_etal_2016$t_2050, rep(2.75, 5), col=hall16_col[5:1], pch=19)
points(sweet2017$t_2050, rep(2.5, length(sweet2017$t_2050)), col=sweet17_col[6:1], pch=19)

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

polygon(y = c(Ras18_CBBT_SS_q025$height, rev(Ras18_CBBT_SS_q975$height)), 
        x = c(1/Ras18_CBBT_SS_q025$freq, rev(1/Ras18_CBBT_SS_q975$freq)), col = trans_Ras18_col[2], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi12_col[2], border = NA)

polygon(y = c(zervas_2013$min_95[1:4], rev(zervas_2013$max_95[1:4])), 
        x = c(1/zervas_2013$aep[1:4], rev(1/zervas_2013$aep[1:4])), col = trans_zervas13_col[2], border = NA)
points(NOAA_methodGEV$return_obs, NOAA_methodGEV$obs, pch = 19, col=obs_col)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=srikrishnan_col[2], lwd=2)

lines(1/Ras18_CBBT_SS_q50$freq, Ras18_CBBT_SS_q50$height, lwd=2, col=Ras18_col[2])
lines(1/Ras18_CBBT_SS_parun$freq, Ras18_CBBT_SS_parun$height, lwd=2, col=Ras18_col[1])

lines(zervas_2013$NOAA_rp, zervas_2013$NOAA_rl_feet, lwd=2, col=zervas13_col[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi12_col[2])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=usace14_col[2])

#   -----------------------------------------------------------------------
# c) Combined sea level and storm surge return period 
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(1/NOAA_methodGEV$aep, NOAA_methodGEV$return_level, log = "x", type = "n", xlim = c(1, 500),
     ylim = c(2.85, 18), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Projected sea+surge level (ft MSL)")
title(main="c.", adj=0)
axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))

SF_k14_r85_2050_SS = plot.sf(k14_r85_SS$t_2050, make.plot=FALSE)
# SF_k14_r60_2050_SS = plot.sf(k14_r60_SS$t_2050, make.plot=FALSE)
SF_k14_r45_2050_SS = plot.sf(k14_r45_SS$t_2050, make.plot=FALSE)
SF_k14_r26_2050_SS = plot.sf(k14_r26_SS$t_2050, make.plot=FALSE)
lines(1/SF_k14_r85_2050_SS$sf, SF_k14_r85_2050_SS$sf.num, col=kopp14_col[1], lwd=1.5)
# lines(1/SF_k14_r60_2050_SS$sf, SF_k14_r60_2050_SS$sf.num, col=RdGy[2], lwd=1.5)
lines(1/SF_k14_r45_2050_SS$sf, SF_k14_r45_2050_SS$sf.num, col=kopp14_col[2], lwd=1.5)
lines(1/SF_k14_r26_2050_SS$sf, SF_k14_r26_2050_SS$sf.num, col=kopp14_col[3], lwd=1.5)

SF_k17_DP16_SEW_r85_2050_SS = plot.sf(k17_DP16_SEW_r85_SS$t_2050, make.plot=FALSE)
# SF_k17_DP16_SEW_r60_2050_SS = plot.sf(k17_DP16_SEW_r60_SS$t_2050, make.plot=FALSE)
SF_k17_DP16_SEW_r45_2050_SS = plot.sf(k17_DP16_SEW_r45_SS$t_2050, make.plot=FALSE)
SF_k17_DP16_SEW_r26_2050_SS = plot.sf(k17_DP16_SEW_r26_SS$t_2050, make.plot=FALSE)
lines(1/SF_k17_DP16_SEW_r85_2050_SS$sf, SF_k17_DP16_SEW_r85_2050_SS$sf.num, col=kopp17_DP16_col[1], lwd=1.5)
# lines(1/SF_k17_DP16_SEW_r60_2050_SS$sf, SF_k17_DP16_SEW_r60_2050_SS$sf.num, col=Greens[6], lwd=1.5)
lines(1/SF_k17_DP16_SEW_r45_2050_SS$sf, SF_k17_DP16_SEW_r45_2050_SS$sf.num, col=kopp17_DP16_col[2], lwd=1.5)
lines(1/SF_k17_DP16_SEW_r26_2050_SS$sf, SF_k17_DP16_SEW_r26_2050_SS$sf.num, col=kopp17_DP16_col[3], lwd=1.5)

SF_bfd_r85_2050_SS = plot.sf(bfd_r85_SS$t_2050, make.plot=FALSE)
# SF_bfd_r60_2050_SS = plot.sf(bfd_r60_SS$t_2050, make.plot=FALSE)
SF_bfd_r45_2050_SS = plot.sf(bfd_r45_SS$t_2050, make.plot=FALSE)
SF_bfd_r26_2050_SS = plot.sf(bfd_r26_SS$t_2050, make.plot=FALSE)
lines(1/SF_bfd_r85_2050_SS$sf, SF_bfd_r85_2050_SS$sf.num, col=brickfd_col[1], lwd=1.5)
# lines(1/SF_bfd_r60_2050_SS$sf, SF_bfd_r60_2050_SS$sf.num, col=RdBu[10], lwd=1.5)
lines(1/SF_bfd_r45_2050_SS$sf, SF_bfd_r45_2050_SS$sf.num, col=brickfd_col[2], lwd=1.5)
lines(1/SF_bfd_r26_2050_SS$sf, SF_bfd_r26_2050_SS$sf.num, col=brickfd_col[3], lwd=1.5)

SF_NOfd_r85_2050_SS = plot.sf(NOfd_r85_SS$t_2050, make.plot=FALSE)
# SF_NOfd_r60_2050_SS = plot.sf(NOfd_r60_SS$t_2050, make.plot=FALSE)
SF_NOfd_r45_2050_SS = plot.sf(NOfd_r45_SS$t_2050, make.plot=FALSE)
SF_NOfd_r26_2050_SS = plot.sf(NOfd_r26_SS$t_2050, make.plot=FALSE)
lines(1/SF_NOfd_r85_2050_SS$sf, SF_NOfd_r85_2050_SS$sf.num, col=NO_fd_col[1], lwd=1.5)
# lines(1/SF_NOfd_r60_2050_SS$sf, SF_NOfd_r60_2050_SS$sf.num, col=PRGn[3], lwd=1.5)
lines(1/SF_NOfd_r45_2050_SS$sf, SF_NOfd_r45_2050_SS$sf.num, col=NO_fd_col[2], lwd=1.5)
lines(1/SF_NOfd_r26_2050_SS$sf, SF_NOfd_r26_2050_SS$sf.num, col=NO_fd_col[3], lwd=1.5)

dev.off()

#---------------------------- 2070 -----------------------------------
pdf(file="../Figures/SLR_2070a.pdf", family="Times", width=full_page_width, height=single_panel_height, pointsize=14)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4), 3, 3, byrow = TRUE))
# Add legend
par(mgp=c(1.5,.5,0), mar=c(0,4,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Tebaldi et al. 2012\nexpected value", "Sweet et al. 2017", 
                           "Parris et al. 2012","Tebaldi et al. 2012\n95% CI", "USACE 2014", 
                           "Srikrishnan et al.\nin prep. 95% CI", "Zervas 2013\nexpected value", "Hall et al. 2016", "Srikrishnan et al.\nin prep.",
                           "Zervas 2013\n 95% CI", "Observations"),
       lty=c(1,1,1,1,NA,NA,NA,NA,NA,1,NA,1,NA, NA), lwd=c(2,2,2,2,NA,NA,NA,NA,NA,2,NA,2,NA, NA), pch=c(NA,NA,NA,NA,19,19,22,19,22,NA,19,NA,22,19), 
       col=c(RdBu[9], sunyellow[2], PRGn[3], tebaldi_gold[1], sweet_cols[5], BrBG[2],"black", BrBG[9], "black", BrBG[2], RdGy[9], RdBu[11], "black", "black"),
       bty='n', ncol=5, pt.bg=c(NA,NA,NA,NA,NA,NA,tebaldi_gold[2],NA,trans_RdBu[9],NA,NA,NA, trans_BrBG[2],NA), pt.cex = c(NA,NA,NA,NA,1,1,2,1,2,NA,1,NA,2, 1))
gradient.rect(7,2.5,9,4, col=col_grad, gradient="x")
arrows(8.75, 1.5, 9, 1.5, length=0.075)
text(8,1.5, "Higher scenario")

#   -----------------------------------------------------------------------
# a) Sea-level rise probability density function
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85$t_2070), xlab="Projected sea level in 2070 (ft)", ylab="Probability density", yaxt="n",
     main="", col=kopp14_col[1], lwd=2, xlim=c(-0.3,8), ylim = c(0, 3), bty="l")
title(main="a.", adj=0)
# lines(density(kopp14_rcp60$t_2070), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2070), col=kopp14_col[2], lwd=2)
lines(density(kopp14_rcp26$t_2070), col=kopp14_col[3], lwd=2)

lines(density(kopp17_DP16_SEW_rcp85$t_2070), col=kopp17_DP16_col[1], lwd=2)
# lines(density(kopp17_DP16_SEW_rcp60$t_2070), col=Greens[6], lwd=2)
lines(density(kopp17_DP16_SEW_rcp45$t_2070), col=kopp17_DP16_col[2], lwd=2)
lines(density(kopp17_DP16_SEW_rcp26$t_2070), col=kopp17_DP16_col[3], lwd=2)

lines(density(brickfd_rcp85$t_2070), col=brickfd_col[1], lwd=2)
# lines(density(brickfd_rcp60$t_2070), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2070), col=brickfd_col[2], lwd=2)
lines(density(brickfd_rcp26$t_2070), col=brickfd_col[3], lwd=2)

lines(density(NO_fdft_rcp85$t_2070), col=NO_fd_col[1], lwd=2)
# lines(density(NO_fdft_rcp60$t_2070), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2070), col=NO_fd_col[2], lwd=2)
lines(density(NO_fdft_rcp26$t_2070), col=NO_fd_col[3], lwd=2)

lines(density(Ras18_SEW_2p5deg$t_2070), col=Ras18_col[1], lwd=2)
lines(density(Ras18_SEW_2p0deg$t_2070), col=Ras18_col[2], lwd=2)
lines(density(Ras18_SEW_1p5deg$t_2070), col=Ras18_col[3], lwd=2)

lines(density(sweet17_03$X2070), col=sweet17_col[6], lwd=2)
lines(density(sweet17_05$X2070), col=sweet17_col[5], lwd=2)
lines(density(sweet17_10$X2070), col=sweet17_col[4], lwd=2)
lines(density(sweet17_15$X2070, na.rm=TRUE), col=sweet17_col[3], lwd=2)
lines(density(sweet17_20$X2070, na.rm=TRUE), col=sweet17_col[2], lwd=2)
lines(density(sweet17_25$X2070, na.rm=TRUE), col=sweet17_col[1], lwd=2)

lines(parris_etal_2012$t_2070, rep(2.75, 4), col=parris12_col[4:1], lwd=2, lty=3)
lines(usace2014$t_2070, rep(2.5, 3), col=usace14_col[3:1], lwd=2, lty=3)
lines(hall_etal_2016$t_2070, rep(2.25, 5), col=hall16_col[5:1], lwd=2, lty=3)
lines(sweet2017$t_2070, rep(2, length(sweet2017$t_2070)), col=sweet17_col[6:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2070, rep(2.75, 4), col=parris12_col[4:1], pch=19)
points(usace2014$t_2070, rep(2.5, 3), col=usace14_col[3:1], pch=19)
points(hall_etal_2016$t_2070, rep(2.25, 5), col=hall16_col[5:1], pch=19)
points(sweet2017$t_2070, rep(2, length(sweet2017$t_2070)), col=sweet17_col[6:1], pch=19)

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

polygon(y = c(Ras18_CBBT_SS_q025$height, rev(Ras18_CBBT_SS_q975$height)), 
        x = c(1/Ras18_CBBT_SS_q025$freq, rev(1/Ras18_CBBT_SS_q975$freq)), col = trans_Ras18_col[2], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi12_col[2], border = NA)

polygon(y = c(zervas_2013$min_95[1:4], rev(zervas_2013$max_95[1:4])), 
        x = c(1/zervas_2013$aep[1:4], rev(1/zervas_2013$aep[1:4])), col = trans_zervas13_col[2], border = NA)
points(NOAA_methodGEV$return_obs, NOAA_methodGEV$obs, pch = 19, col=obs_col)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=srikrishnan_col[2], lwd=2)

lines(1/Ras18_CBBT_SS_q50$freq, Ras18_CBBT_SS_q50$height, lwd=2, col=Ras18_col[2])
lines(1/Ras18_CBBT_SS_parun$freq, Ras18_CBBT_SS_parun$height, lwd=2, col=Ras18_col[1])

lines(zervas_2013$NOAA_rp, zervas_2013$NOAA_rl_feet, lwd=2, col=zervas13_col[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi12_col[2])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=usace14_col[2])

#   -----------------------------------------------------------------------
# c) Combined sea level and storm surge return period 
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(1/NOAA_methodGEV$aep, NOAA_methodGEV$return_level, log = "x", type = "n", xlim = c(1, 500),
     ylim = c(2.85, 18), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Projected sea+surge level (ft MSL)")
title(main="c.", adj=0)
axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))

SF_k14_r85_2070_SS = plot.sf(k14_r85_SS$t_2070, make.plot=FALSE)
# SF_k14_r60_2070_SS = plot.sf(k14_r60_SS$t_2070, make.plot=FALSE)
SF_k14_r45_2070_SS = plot.sf(k14_r45_SS$t_2070, make.plot=FALSE)
SF_k14_r26_2070_SS = plot.sf(k14_r26_SS$t_2070, make.plot=FALSE)
lines(1/SF_k14_r85_2070_SS$sf, SF_k14_r85_2070_SS$sf.num, col=kopp14_col[1], lwd=1.5)
# lines(1/SF_k14_r60_2070_SS$sf, SF_k14_r60_2070_SS$sf.num, col=RdGy[2], lwd=1.5)
lines(1/SF_k14_r45_2070_SS$sf, SF_k14_r45_2070_SS$sf.num, col=kopp14_col[2], lwd=1.5)
lines(1/SF_k14_r26_2070_SS$sf, SF_k14_r26_2070_SS$sf.num, col=kopp14_col[3], lwd=1.5)

SF_k17_DP16_SEW_r85_2070_SS = plot.sf(k17_DP16_SEW_r85_SS$t_2070, make.plot=FALSE)
# SF_k17_DP16_SEW_r60_2070_SS = plot.sf(k17_DP16_SEW_r60_SS$t_2070, make.plot=FALSE)
SF_k17_DP16_SEW_r45_2070_SS = plot.sf(k17_DP16_SEW_r45_SS$t_2070, make.plot=FALSE)
SF_k17_DP16_SEW_r26_2070_SS = plot.sf(k17_DP16_SEW_r26_SS$t_2070, make.plot=FALSE)
lines(1/SF_k17_DP16_SEW_r85_2070_SS$sf, SF_k17_DP16_SEW_r85_2070_SS$sf.num, col=kopp17_DP16_col[1], lwd=1.5)
# lines(1/SF_k17_DP16_SEW_r60_2070_SS$sf, SF_k17_DP16_SEW_r60_2070_SS$sf.num, col=Greens[6], lwd=1.5)
lines(1/SF_k17_DP16_SEW_r45_2070_SS$sf, SF_k17_DP16_SEW_r45_2070_SS$sf.num, col=kopp17_DP16_col[2], lwd=1.5)
lines(1/SF_k17_DP16_SEW_r26_2070_SS$sf, SF_k17_DP16_SEW_r26_2070_SS$sf.num, col=kopp17_DP16_col[3], lwd=1.5)

SF_bfd_r85_2070_SS = plot.sf(bfd_r85_SS$t_2070, make.plot=FALSE)
# SF_bfd_r60_2070_SS = plot.sf(bfd_r60_SS$t_2070, make.plot=FALSE)
SF_bfd_r45_2070_SS = plot.sf(bfd_r45_SS$t_2070, make.plot=FALSE)
SF_bfd_r26_2070_SS = plot.sf(bfd_r26_SS$t_2070, make.plot=FALSE)
lines(1/SF_bfd_r85_2070_SS$sf, SF_bfd_r85_2070_SS$sf.num, col=brickfd_col[1], lwd=1.5)
# lines(1/SF_bfd_r60_2070_SS$sf, SF_bfd_r60_2070_SS$sf.num, col=RdBu[10], lwd=1.5)
lines(1/SF_bfd_r45_2070_SS$sf, SF_bfd_r45_2070_SS$sf.num, col=brickfd_col[2], lwd=1.5)
lines(1/SF_bfd_r26_2070_SS$sf, SF_bfd_r26_2070_SS$sf.num, col=brickfd_col[3], lwd=1.5)

SF_NOfd_r85_2070_SS = plot.sf(NOfd_r85_SS$t_2070, make.plot=FALSE)
# SF_NOfd_r60_2070_SS = plot.sf(NOfd_r60_SS$t_2070, make.plot=FALSE)
SF_NOfd_r45_2070_SS = plot.sf(NOfd_r45_SS$t_2070, make.plot=FALSE)
SF_NOfd_r26_2070_SS = plot.sf(NOfd_r26_SS$t_2070, make.plot=FALSE)
lines(1/SF_NOfd_r85_2070_SS$sf, SF_NOfd_r85_2070_SS$sf.num, col=NO_fd_col[1], lwd=1.5)
# lines(1/SF_NOfd_r60_2070_SS$sf, SF_NOfd_r60_2070_SS$sf.num, col=PRGn[3], lwd=1.5)
lines(1/SF_NOfd_r45_2070_SS$sf, SF_NOfd_r45_2070_SS$sf.num, col=NO_fd_col[2], lwd=1.5)
lines(1/SF_NOfd_r26_2070_SS$sf, SF_NOfd_r26_2070_SS$sf.num, col=NO_fd_col[3], lwd=1.5)

dev.off()

#---------------------------- 2100 -----------------------------------
pdf(file="../Figures/SLR_2100a.pdf", family="Times", width=full_page_width, height=single_panel_height, pointsize=14)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4), 3, 3, byrow = TRUE))
# Add legend
par(mgp=c(1.5,.5,0), mar=c(0,4,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Tebaldi et al. 2012\nexpected value", "Sweet et al. 2017", 
                           "Parris et al. 2012","Tebaldi et al. 2012\n95% CI", "USACE 2014", 
                           "Srikrishnan et al.\nin prep. 95% CI", "Zervas 2013\nexpected value", "Hall et al. 2016", "Srikrishnan et al.\nin prep.",
                           "Zervas 2013\n 95% CI", "Observations"),
       lty=c(1,1,1,1,NA,NA,NA,NA,NA,1,NA,1,NA, NA), lwd=c(2,2,2,2,NA,NA,NA,NA,NA,2,NA,2,NA, NA), pch=c(NA,NA,NA,NA,19,19,22,19,22,NA,19,NA,22,19), 
       col=c(RdBu[9], sunyellow[2], PRGn[3], tebaldi_gold[1], sweet_cols[5], BrBG[2],"black", BrBG[9], "black", BrBG[2], RdGy[9], RdBu[11], "black", "black"),
       bty='n', ncol=5, pt.bg=c(NA,NA,NA,NA,NA,NA,tebaldi_gold[2],NA,trans_RdBu[9],NA,NA,NA, trans_BrBG[2],NA), pt.cex = c(NA,NA,NA,NA,1,1,2,1,2,NA,1,NA,2, 1))
gradient.rect(7,2.5,9,4, col=col_grad, gradient="x")
arrows(8.75, 1.5, 9, 1.5, length=0.075)
text(8,1.5, "Higher scenario")

#   -----------------------------------------------------------------------
# a) Sea-level rise probability density function
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85$t_2100), xlab="Projected sea level in 2100 (ft)", ylab="Probability density", yaxt="n", 
     main="", col=kopp14_col[1], lwd=2, xlim=c(-0.5,15), ylim = c(0, 1.5), bty="l")
title(main="a.", adj=0)
# lines(density(kopp14_rcp60$t_2100), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2100), col=kopp14_col[2], lwd=2)
lines(density(kopp14_rcp26$t_2100), col=kopp14_col[3], lwd=2)

lines(density(kopp17_DP16_SEW_rcp85$t_2100), col=kopp17_DP16_col[1], lwd=2)
# lines(density(kopp17_DP16_SEW_rcp60$t_2100), col=Greens[6], lwd=2)
lines(density(kopp17_DP16_SEW_rcp45$t_2100), col=kopp17_DP16_col[2], lwd=2)
lines(density(kopp17_DP16_SEW_rcp26$t_2100), col=kopp17_DP16_col[3], lwd=2)

lines(density(brickfd_rcp85$t_2100), col=brickfd_col[1], lwd=2)
# lines(density(brickfd_rcp60$t_2100), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2100), col=brickfd_col[2], lwd=2)
lines(density(brickfd_rcp26$t_2100), col=brickfd_col[3], lwd=2)

lines(density(NO_fdft_rcp85$t_2100), col=NO_fd_col[1], lwd=2)
# lines(density(NO_fdft_rcp60$t_2100), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2100), col=NO_fd_col[2], lwd=2)
lines(density(NO_fdft_rcp26$t_2100), col=NO_fd_col[3], lwd=2)

lines(density(Ras18_SEW_2p5deg$t_2100), col=Ras18_col[1], lwd=2)
lines(density(Ras18_SEW_2p0deg$t_2100), col=Ras18_col[2], lwd=2)
lines(density(Ras18_SEW_1p5deg$t_2100), col=Ras18_col[3], lwd=2)

lines(density(sweet17_03$X2100), col=sweet17_col[6], lwd=2)
lines(density(sweet17_05$X2100), col=sweet17_col[5], lwd=2)
lines(density(sweet17_10$X2100), col=sweet17_col[4], lwd=2)
lines(density(sweet17_15$X2100, na.rm=TRUE), col=sweet17_col[3], lwd=2)
lines(density(sweet17_20$X2100, na.rm=TRUE), col=sweet17_col[2], lwd=2)
lines(density(sweet17_25$X2100, na.rm=TRUE), col=sweet17_col[1], lwd=2)

lines(parris_etal_2012$t_2100, rep(1.45, 4), col=parris12_col[4:1], lwd=2, lty=3)
lines(usace2014$t_2100, rep(1.3, 3), col=usace14_col[3:1], lwd=2, lty=3)
lines(hall_etal_2016$t_2100, rep(1.15, 5), col=hall16_col[5:1], lwd=2, lty=3)
lines(sweet2017$t_2100, rep(1, length(sweet2017$t_2100)), col=sweet17_col[6:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2100, rep(1.45, 4), col=parris12_col[4:1], pch=19)
points(usace2014$t_2100, rep(1.3, 3), col=usace14_col[3:1], pch=19)
points(hall_etal_2016$t_2100, rep(1.15, 5), col=hall16_col[5:1], pch=19)
points(sweet2017$t_2100, rep(1, length(sweet2017$t_2100)), col=sweet17_col[6:1], pch=19)

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

polygon(y = c(Ras18_CBBT_SS_q025$height, rev(Ras18_CBBT_SS_q975$height)), 
        x = c(1/Ras18_CBBT_SS_q025$freq, rev(1/Ras18_CBBT_SS_q975$freq)), col = trans_Ras18_col[2], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi12_col[2], border = NA)

polygon(y = c(zervas_2013$min_95[1:4], rev(zervas_2013$max_95[1:4])), 
        x = c(1/zervas_2013$aep[1:4], rev(1/zervas_2013$aep[1:4])), col = trans_zervas13_col[2], border = NA)
points(NOAA_methodGEV$return_obs, NOAA_methodGEV$obs, pch = 19, col=obs_col)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=srikrishnan_col[2], lwd=2)

lines(1/Ras18_CBBT_SS_q50$freq, Ras18_CBBT_SS_q50$height, lwd=2, col=Ras18_col[2])
lines(1/Ras18_CBBT_SS_parun$freq, Ras18_CBBT_SS_parun$height, lwd=2, col=Ras18_col[1])

lines(zervas_2013$NOAA_rp, zervas_2013$NOAA_rl_feet, lwd=2, col=zervas13_col[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi12_col[2])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=usace14_col[2])

#   -----------------------------------------------------------------------
# c) Combined sea level and storm surge return period 
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(1/NOAA_methodGEV$aep, NOAA_methodGEV$return_level, log = "x", type = "n", xlim = c(1, 500),
     ylim = c(2.85, 18), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Projected sea+surge level (ft MSL)")
title(main="c.", adj=0)
axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))

SF_k14_r85_2100_SS = plot.sf(k14_r85_SS$t_2100, make.plot=FALSE)
# SF_k14_r60_2100_SS = plot.sf(k14_r60_SS$t_2100, make.plot=FALSE)
SF_k14_r45_2100_SS = plot.sf(k14_r45_SS$t_2100, make.plot=FALSE)
SF_k14_r26_2100_SS = plot.sf(k14_r26_SS$t_2100, make.plot=FALSE)
lines(1/SF_k14_r85_2100_SS$sf, SF_k14_r85_2100_SS$sf.num, col=kopp14_col[1], lwd=1.5)
# lines(1/SF_k14_r60_2100_SS$sf, SF_k14_r60_2100_SS$sf.num, col=RdGy[2], lwd=1.5)
lines(1/SF_k14_r45_2100_SS$sf, SF_k14_r45_2100_SS$sf.num, col=kopp14_col[2], lwd=1.5)
lines(1/SF_k14_r26_2100_SS$sf, SF_k14_r26_2100_SS$sf.num, col=kopp14_col[3], lwd=1.5)

SF_k17_DP16_SEW_r85_2100_SS = plot.sf(k17_DP16_SEW_r85_SS$t_2100, make.plot=FALSE)
# SF_k17_DP16_SEW_r60_2100_SS = plot.sf(k17_DP16_SEW_r60_SS$t_2100, make.plot=FALSE)
SF_k17_DP16_SEW_r45_2100_SS = plot.sf(k17_DP16_SEW_r45_SS$t_2100, make.plot=FALSE)
SF_k17_DP16_SEW_r26_2100_SS = plot.sf(k17_DP16_SEW_r26_SS$t_2100, make.plot=FALSE)
lines(1/SF_k17_DP16_SEW_r85_2100_SS$sf, SF_k17_DP16_SEW_r85_2100_SS$sf.num, col=kopp17_DP16_col[1], lwd=1.5)
# lines(1/SF_k17_DP16_SEW_r60_2100_SS$sf, SF_k17_DP16_SEW_r60_2100_SS$sf.num, col=Greens[6], lwd=1.5)
lines(1/SF_k17_DP16_SEW_r45_2100_SS$sf, SF_k17_DP16_SEW_r45_2100_SS$sf.num, col=kopp17_DP16_col[2], lwd=1.5)
lines(1/SF_k17_DP16_SEW_r26_2100_SS$sf, SF_k17_DP16_SEW_r26_2100_SS$sf.num, col=kopp17_DP16_col[3], lwd=1.5)

SF_bfd_r85_2100_SS = plot.sf(bfd_r85_SS$t_2100, make.plot=FALSE)
# SF_bfd_r60_2100_SS = plot.sf(bfd_r60_SS$t_2100, make.plot=FALSE)
SF_bfd_r45_2100_SS = plot.sf(bfd_r45_SS$t_2100, make.plot=FALSE)
SF_bfd_r26_2100_SS = plot.sf(bfd_r26_SS$t_2100, make.plot=FALSE)
lines(1/SF_bfd_r85_2100_SS$sf, SF_bfd_r85_2100_SS$sf.num, col=brickfd_col[1], lwd=1.5)
# lines(1/SF_bfd_r60_2100_SS$sf, SF_bfd_r60_2100_SS$sf.num, col=RdBu[10], lwd=1.5)
lines(1/SF_bfd_r45_2100_SS$sf, SF_bfd_r45_2100_SS$sf.num, col=brickfd_col[2], lwd=1.5)
lines(1/SF_bfd_r26_2100_SS$sf, SF_bfd_r26_2100_SS$sf.num, col=brickfd_col[3], lwd=1.5)

SF_NOfd_r85_2100_SS = plot.sf(NOfd_r85_SS$t_2100, make.plot=FALSE)
# SF_NOfd_r60_2100_SS = plot.sf(NOfd_r60_SS$t_2100, make.plot=FALSE)
SF_NOfd_r45_2100_SS = plot.sf(NOfd_r45_SS$t_2100, make.plot=FALSE)
SF_NOfd_r26_2100_SS = plot.sf(NOfd_r26_SS$t_2100, make.plot=FALSE)
lines(1/SF_NOfd_r85_2100_SS$sf, SF_NOfd_r85_2100_SS$sf.num, col=NO_fd_col[1], lwd=1.5)
# lines(1/SF_NOfd_r60_2100_SS$sf, SF_NOfd_r60_2100_SS$sf.num, col=PRGn[3], lwd=1.5)
lines(1/SF_NOfd_r45_2100_SS$sf, SF_NOfd_r45_2100_SS$sf.num, col=NO_fd_col[2], lwd=1.5)
lines(1/SF_NOfd_r26_2100_SS$sf, SF_NOfd_r26_2100_SS$sf.num, col=NO_fd_col[3], lwd=1.5)

dev.off()

##=========================== MULTIPLE YEAR SLR DENSITY & PROJECTION PLOT ===================================
# Extract 90% CI
k14_26_5 =
k14_45_5 =
# k14_60_5 =
k14_85_5 =
k14_26_95 =
k14_45_95 =
# k14_60_95 =
k14_85_95 = rep(NA, length(k14_years))
for(i in 1:length(k14_years)){
  k14_26_5[i] <- quantile(convert_cm_to_ft(kopp14_rcp26_dat[,i]), 0.05)
  k14_45_5[i] <- quantile(convert_cm_to_ft(kopp14_rcp45_dat[,i]), 0.05)
  # k14_60_5[i] <- quantile(convert_cm_to_ft(kopp14_rcp60_dat[,i]), 0.05, na.rm=TRUE) #goes to 2100
  k14_85_5[i] <- quantile(convert_cm_to_ft(kopp14_rcp85_dat[,i]), 0.05)
  
  k14_26_95[i] <- quantile(convert_cm_to_ft(kopp14_rcp26_dat[,i]), 0.95)
  k14_45_95[i] <- quantile(convert_cm_to_ft(kopp14_rcp45_dat[,i]), 0.95)
  # k14_60_95[i] <- quantile(convert_cm_to_ft(kopp14_rcp60_dat[,i]), 0.95, na.rm=TRUE) #goes to 2100
  k14_85_95[i] <- quantile(convert_cm_to_ft(kopp14_rcp85_dat[,i]), 0.95)
}
k17_DP16_SEW_26_5 =
  k17_DP16_SEW_45_5 =
  # k17_DP16_SEW_60_5 =
  k17_DP16_SEW_85_5 =
  k17_DP16_SEW_26_95 =
  k17_DP16_SEW_45_95 =
  # k17_DP16_SEW_60_95 =
  k17_DP16_SEW_85_95 = rep(NA, length(k17_DP16_SEW_years))
for(i in 1:length(k17_DP16_SEW_years)){
  k17_DP16_SEW_26_5[i] <- quantile(convert_cm_to_ft(kopp17_DP16_SEW_rcp26_dat[,i]), 0.05)
  k17_DP16_SEW_45_5[i] <- quantile(convert_cm_to_ft(kopp17_DP16_SEW_rcp45_dat[,i]), 0.05)
  # k17_DP16_SEW_60_5[i] <- quantile(convert_cm_to_ft(kopp17_DP16_SEW_rcp60_dat[,i]), 0.05, na.rm=TRUE) #goes to 2100
  k17_DP16_SEW_85_5[i] <- quantile(convert_cm_to_ft(kopp17_DP16_SEW_rcp85_dat[,i]), 0.05)
  
  k17_DP16_SEW_26_95[i] <- quantile(convert_cm_to_ft(kopp17_DP16_SEW_rcp26_dat[,i]), 0.95)
  k17_DP16_SEW_45_95[i] <- quantile(convert_cm_to_ft(kopp17_DP16_SEW_rcp45_dat[,i]), 0.95)
  # k17_DP16_SEW_60_95[i] <- quantile(convert_cm_to_ft(kopp17_DP16_SEW_rcp60_dat[,i]), 0.95, na.rm=TRUE) #goes to 2100
  k17_DP16_SEW_85_95[i] <- quantile(convert_cm_to_ft(kopp17_DP16_SEW_rcp85_dat[,i]), 0.95)
}
  lsl_fdyn_26_5 =  
  lsl_fdyn_45_5 =
  # lsl_fdyn_60_5 =
  lsl_fdyn_85_5 =
  lsl_fdyn_26_95 =
  lsl_fdyn_45_95 =
  # lsl_fdyn_60_95 =
  lsl_fdyn_85_95 = 
    NO_fdyn_26_5 =
    NO_fdyn_45_5 =
    # NO_fdyn_60_5 =
    NO_fdyn_85_5 =
    NO_fdyn_26_95 =
    NO_fdyn_45_95 =
    # NO_fdyn_60_95 =
    NO_fdyn_85_95 = rep(NA, length(t.time))
for(i in 1:length(t.time)){
  lsl_fdyn_26_5[i] <- quantile(lsl_fdyn_rcp26_sub[i,], 0.05)
  lsl_fdyn_45_5[i] <- quantile(lsl_fdyn_rcp45_sub[i,], 0.05)
  # lsl_fdyn_60_5[i] <- quantile(lsl_fdyn_rcp60_sub[i,], 0.05) 
  lsl_fdyn_85_5[i] <- quantile(lsl_fdyn_rcp85_sub[i,], 0.05)
  
  lsl_fdyn_26_95[i] <- quantile(lsl_fdyn_rcp26_sub[i,], 0.95)
  lsl_fdyn_45_95[i] <- quantile(lsl_fdyn_rcp45_sub[i,], 0.95)
  # lsl_fdyn_60_95[i] <- quantile(lsl_fdyn_rcp60_sub[i,], 0.95) 
  lsl_fdyn_85_95[i] <- quantile(lsl_fdyn_rcp85_sub[i,], 0.95)
  
  NO_fdyn_26_5[i] <- quantile(NO_fdyn_rcp26_sub[i,], 0.05)
  NO_fdyn_45_5[i] <- quantile(NO_fdyn_rcp45_sub[i,], 0.05)
  # NO_fdyn_60_5[i] <- quantile(NO_fdyn_rcp60_sub[i,], 0.05) 
  NO_fdyn_85_5[i] <- quantile(NO_fdyn_rcp85_sub[i,], 0.05)
  
  NO_fdyn_26_95[i] <- quantile(NO_fdyn_rcp26_sub[i,], 0.95)
  NO_fdyn_45_95[i] <- quantile(NO_fdyn_rcp45_sub[i,], 0.95)
  # NO_fdyn_60_95[i] <- quantile(NO_fdyn_rcp60_sub[i,], 0.95) 
  NO_fdyn_85_95[i] <- quantile(NO_fdyn_rcp85_sub[i,], 0.95)
}
  Ras18_SEW_2p5deg_5 =  
    Ras18_SEW_2p0deg_5 =
    Ras18_SEW_1p5deg_5 =
    Ras18_SEW_2p5deg_95 =  
    Ras18_SEW_2p0deg_95 =
    Ras18_SEW_1p5deg_95 = rep(NA, length(Ras18_SEW_years))
  for(i in 1:length(Ras18_SEW_years)){
    Ras18_SEW_2p5deg_5[i] <- quantile(convert_cm_to_ft(Ras18_SEW_2p5deg_dat[,i]), 0.05)
    Ras18_SEW_2p0deg_5[i] <- quantile(convert_cm_to_ft(Ras18_SEW_2p0deg_dat[,i]), 0.05)
    Ras18_SEW_1p5deg_5[i] <- quantile(convert_cm_to_ft(Ras18_SEW_1p5deg_dat[,i]), 0.05)
    
    Ras18_SEW_2p5deg_95[i] <- quantile(convert_cm_to_ft(Ras18_SEW_2p5deg_dat[,i]), 0.95)
    Ras18_SEW_2p0deg_95[i] <- quantile(convert_cm_to_ft(Ras18_SEW_2p0deg_dat[,i]), 0.95)
    Ras18_SEW_1p5deg_95[i] <- quantile(convert_cm_to_ft(Ras18_SEW_1p5deg_dat[,i]), 0.95)
  }
  sweet17_03_5 =  
    sweet17_05_5 =
    sweet17_10_5 =
    sweet17_15_5 =  
    sweet17_20_5 =
    sweet17_25_5 =
    sweet17_03_95 =  
    sweet17_05_95 =
    sweet17_10_95 =
    sweet17_15_95 =  
    sweet17_20_95 =
    sweet17_25_95 =rep(NA, ncol(sweet17_10))
  for(i in 1:ncol(sweet17_10)){
    sweet17_03_5[i] <- quantile(sweet17_03[,i], 0.05, na.rm=TRUE)
    sweet17_05_5[i] <- quantile(sweet17_05[,i], 0.05, na.rm=TRUE)
    sweet17_10_5[i] <- quantile(sweet17_10[,i], 0.05, na.rm=TRUE)
    sweet17_15_5[i] <- quantile(sweet17_15[,i], 0.05, na.rm=TRUE)
    sweet17_20_5[i] <- quantile(sweet17_20[,i], 0.05, na.rm=TRUE)
    sweet17_25_5[i] <- quantile(sweet17_25[,i], 0.05, na.rm=TRUE)
    
    sweet17_03_95[i] <- quantile(sweet17_03[,i], 0.95, na.rm=TRUE)
    sweet17_05_95[i] <- quantile(sweet17_05[,i], 0.95, na.rm=TRUE)
    sweet17_10_95[i] <- quantile(sweet17_10[,i], 0.95, na.rm=TRUE)
    sweet17_15_95[i] <- quantile(sweet17_15[,i], 0.95, na.rm=TRUE)
    sweet17_20_95[i] <- quantile(sweet17_20[,i], 0.95, na.rm=TRUE)
    sweet17_25_95[i] <- quantile(sweet17_25[,i], 0.95, na.rm=TRUE)
  }

pdf(file="../Figures/SLR_proj_pdfa.pdf", family="Times", width=text_column_width, height=full_page_height, pointsize=12)
layout(matrix(c(1,1,
                2,3,
                4,5), 3, 2, byrow = TRUE))
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(k14_years, k14_85_95, type="n",xlab="Year", ylab="Projected sea level (ft)",
     ylim=c(0,13), xlim=c(2010, 2098), xaxt="n")
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

legend("topleft", legend=c("Wong & Keller 2017 FD RCP85 90% CI", "Wong & Keller 2017 no FD RCP85 90% CI", "Kopp et al. 2014 RCP85 90% CI",
                           "Wong & Keller 2017 FD", "Wong & Keller 2017 no FD", "Kopp et al. 2014", "Sweet et al. 2017",
                           "Parris et al. 2012",
                           "USACE 2014", "Hall et al. 2016"),
       lty=c(NA,NA,NA,1,1,1,NA,NA,NA,NA), lwd=c(NA,NA,NA,2,2,2,NA,NA,NA,NA), pch=c(22,22,22,NA,NA,NA,19,19,19,19),
       col=c("black", "black", "black", RdBu[9], PRGn[3], sunyellow[2], sweet_cols[5], BrBG[2], BrBG[9], RdGy[9]),
       bty='n', pt.bg=c(trans_RdBu[10], trans_PRGn[2], trans_sunyellow[1],NA,NA,NA,NA,NA,NA,NA), pt.cex = c(2,2,2,NA,NA,NA,1,1,1,1))
gradient.rect(2008,2,2012,2.25, col=col_grad, gradient="x")
arrows(2028.5, 2.12, 2030.5, 2.12, length=0.075)
text(2020.5,2.12, "Higher scenario")

#--------------------------
# b) Sea-level rise probability density function 2030
plot(density(kopp14_rcp85$t_2030), xlab="Projected sea level in 2030 (ft)", ylab="Probability density", yaxt="n", 
     main="",col=kopp14_col[1], lwd=2, xlim=c(-0.2,2), ylim = c(0, 6.75), bty="l")
title(main="b.", adj=0)
# lines(density(kopp14_rcp60$t_2030), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2030), col=kopp14_col[2], lwd=2)
lines(density(kopp14_rcp26$t_2030), col=kopp14_col[3], lwd=2)

lines(density(kopp17_DP16_SEW_rcp85$t_2030), col=kopp17_DP16_col[1], lwd=2)
# lines(density(kopp17_DP16_SEW_rcp60$t_2030), col=Greens[6], lwd=2)
lines(density(kopp17_DP16_SEW_rcp45$t_2030), col=kopp17_DP16_col[2], lwd=2)
lines(density(kopp17_DP16_SEW_rcp26$t_2030), col=kopp17_DP16_col[3], lwd=2)

lines(density(brickfd_rcp85$t_2030), col=brickfd_col[1], lwd=2)
# lines(density(brickfd_rcp60$t_2030), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2030), col=brickfd_col[2], lwd=2)
lines(density(brickfd_rcp26$t_2030), col=brickfd_col[3], lwd=2)

lines(density(NO_fdft_rcp85$t_2030), col=NO_fd_col[1], lwd=2)
# lines(density(NO_fdft_rcp60$t_2030), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2030), col=NO_fd_col[2], lwd=2)
lines(density(NO_fdft_rcp26$t_2030), col=NO_fd_col[3], lwd=2)

lines(density(Ras18_SEW_2p5deg$t_2030), col=Ras18_col[1], lwd=2)
lines(density(Ras18_SEW_2p0deg$t_2030), col=Ras18_col[2], lwd=2)
lines(density(Ras18_SEW_1p5deg$t_2030), col=Ras18_col[3], lwd=2)

lines(density(sweet17_03$X2030), col=sweet17_col[6], lwd=2)
lines(density(sweet17_05$X2030), col=sweet17_col[5], lwd=2)
lines(density(sweet17_10$X2030), col=sweet17_col[4], lwd=2)
lines(density(sweet17_15$X2030, na.rm=TRUE), col=sweet17_col[3], lwd=2)
lines(density(sweet17_20$X2030, na.rm=TRUE), col=sweet17_col[2], lwd=2)
lines(density(sweet17_25$X2030, na.rm=TRUE), col=sweet17_col[1], lwd=2)

lines(parris_etal_2012$t_2030, rep(6.5, 4), col=parris12_col[4:1], lwd=1.5, lty=3)
lines(usace2014$t_2030, rep(6, 3), col=usace14_col[3:1], lwd=1.5, lty=3)
lines(hall_etal_2016$t_2030, rep(5.5, 5), col=hall16_col[5:1], lwd=1.5, lty=3)
lines(sweet2017$t_2030, rep(5, length(sweet2017$t_2030)), col=sweet17_col[6:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2030, rep(6.5, 4), col=parris12_col[4:1], pch=19)
points(usace2014$t_2030, rep(6, 3), col=usace14_col[3:1], pch=19)
points(hall_etal_2016$t_2030, rep(5.5, 5), col=hall16_col[5:1], pch=19)
points(sweet2017$t_2030, rep(5, length(sweet2017$t_2030)), col=sweet17_col[6:1], pch=19)

#   -----------------------------------------------------------------------
# c) Sea-level rise probability density function 2050
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(density(kopp14_rcp85$t_2050), xlab="Projected sea level in 2050 (ft)", ylab="Probability density", yaxt="n", 
     main="", col=kopp14_col[1], lwd=2, xlim=c(-0.2,4), ylim = c(0, 3.5), bty="l")
title(main="c.", adj=0)
# lines(density(kopp14_rcp60$t_2050), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2050), col=kopp14_col[2], lwd=2)
lines(density(kopp14_rcp26$t_2050), col=kopp14_col[3], lwd=2)

lines(density(kopp17_DP16_SEW_rcp85$t_2050), col=kopp17_DP16_col[1], lwd=2)
# lines(density(kopp17_DP16_SEW_rcp60$t_2050), col=Greens[6], lwd=2)
lines(density(kopp17_DP16_SEW_rcp45$t_2050), col=kopp17_DP16_col[2], lwd=2)
lines(density(kopp17_DP16_SEW_rcp26$t_2050), col=kopp17_DP16_col[3], lwd=2)

lines(density(brickfd_rcp85$t_2050), col=brickfd_col[1], lwd=2)
# lines(density(brickfd_rcp60$t_2050), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2050), col=brickfd_col[2], lwd=2)
lines(density(brickfd_rcp26$t_2050), col=brickfd_col[3], lwd=2)

lines(density(NO_fdft_rcp85$t_2050), col=NO_fd_col[1], lwd=2)
# lines(density(NO_fdft_rcp60$t_2050), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2050), col=NO_fd_col[2], lwd=2)
lines(density(NO_fdft_rcp26$t_2050), col=NO_fd_col[3], lwd=2)

lines(density(Ras18_SEW_2p5deg$t_2050), col=Ras18_col[1], lwd=2)
lines(density(Ras18_SEW_2p0deg$t_2050), col=Ras18_col[2], lwd=2)
lines(density(Ras18_SEW_1p5deg$t_2050), col=Ras18_col[3], lwd=2)

lines(density(sweet17_03$X2050), col=sweet17_col[6], lwd=2)
lines(density(sweet17_05$X2050), col=sweet17_col[5], lwd=2)
lines(density(sweet17_10$X2050), col=sweet17_col[4], lwd=2)
lines(density(sweet17_15$X2050, na.rm=TRUE), col=sweet17_col[3], lwd=2)
lines(density(sweet17_20$X2050, na.rm=TRUE), col=sweet17_col[2], lwd=2)
lines(density(sweet17_25$X2050, na.rm=TRUE), col=sweet17_col[1], lwd=2)

lines(parris_etal_2012$t_2050, rep(3.25, 4), col=parris12_col[4:1], lwd=2, lty=3)
lines(usace2014$t_2050, rep(3, 3), col=usace14_col[3:1], lwd=2, lty=3)
lines(hall_etal_2016$t_2050, rep(2.75, 5), col=hall16_col[5:1], lwd=2, lty=3)
lines(sweet2017$t_2050, rep(2.5, length(sweet2017$t_2050)), col=sweet17_col[6:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2050, rep(3.25, 4), col=parris12_col[4:1], pch=19)
points(usace2014$t_2050, rep(3, 3), col=usace14_col[3:1], pch=19)
points(hall_etal_2016$t_2050, rep(2.75, 5), col=hall16_col[5:1], pch=19)
points(sweet2017$t_2050, rep(2.5, length(sweet2017$t_2050)), col=sweet17_col[6:1], pch=19)

#   -----------------------------------------------------------------------
# d) Sea-level rise probability density function 2070
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85$t_2070), xlab="Projected sea level in 2070 (ft)", ylab="Probability density", yaxt="n",
     main="", col=kopp14_col[1], lwd=2, xlim=c(-0.3,8), ylim = c(0, 3), bty="l")
title(main="d.", adj=0)
# lines(density(kopp14_rcp60$t_2070), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2070), col=kopp14_col[2], lwd=2)
lines(density(kopp14_rcp26$t_2070), col=kopp14_col[3], lwd=2)

lines(density(kopp17_DP16_SEW_rcp85$t_2070), col=kopp17_DP16_col[1], lwd=2)
# lines(density(kopp17_DP16_SEW_rcp60$t_2070), col=Greens[6], lwd=2)
lines(density(kopp17_DP16_SEW_rcp45$t_2070), col=kopp17_DP16_col[2], lwd=2)
lines(density(kopp17_DP16_SEW_rcp26$t_2070), col=kopp17_DP16_col[3], lwd=2)

lines(density(brickfd_rcp85$t_2070), col=brickfd_col[1], lwd=2)
# lines(density(brickfd_rcp60$t_2070), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2070), col=brickfd_col[2], lwd=2)
lines(density(brickfd_rcp26$t_2070), col=brickfd_col[3], lwd=2)

lines(density(NO_fdft_rcp85$t_2070), col=NO_fd_col[1], lwd=2)
# lines(density(NO_fdft_rcp60$t_2070), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2070), col=NO_fd_col[2], lwd=2)
lines(density(NO_fdft_rcp26$t_2070), col=NO_fd_col[3], lwd=2)

lines(density(Ras18_SEW_2p5deg$t_2070), col=Ras18_col[1], lwd=2)
lines(density(Ras18_SEW_2p0deg$t_2070), col=Ras18_col[2], lwd=2)
lines(density(Ras18_SEW_1p5deg$t_2070), col=Ras18_col[3], lwd=2)

lines(density(sweet17_03$X2070), col=sweet17_col[6], lwd=2)
lines(density(sweet17_05$X2070), col=sweet17_col[5], lwd=2)
lines(density(sweet17_10$X2070), col=sweet17_col[4], lwd=2)
lines(density(sweet17_15$X2070, na.rm=TRUE), col=sweet17_col[3], lwd=2)
lines(density(sweet17_20$X2070, na.rm=TRUE), col=sweet17_col[2], lwd=2)
lines(density(sweet17_25$X2070, na.rm=TRUE), col=sweet17_col[1], lwd=2)

lines(parris_etal_2012$t_2070, rep(2.75, 4), col=parris12_col[4:1], lwd=2, lty=3)
lines(usace2014$t_2070, rep(2.5, 3), col=usace14_col[3:1], lwd=2, lty=3)
lines(hall_etal_2016$t_2070, rep(2.25, 5), col=hall16_col[5:1], lwd=2, lty=3)
lines(sweet2017$t_2070, rep(2, length(sweet2017$t_2070)), col=sweet17_col[6:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2070, rep(2.75, 4), col=parris12_col[4:1], pch=19)
points(usace2014$t_2070, rep(2.5, 3), col=usace14_col[3:1], pch=19)
points(hall_etal_2016$t_2070, rep(2.25, 5), col=hall16_col[5:1], pch=19)
points(sweet2017$t_2070, rep(2, length(sweet2017$t_2070)), col=sweet17_col[6:1], pch=19)

#   -----------------------------------------------------------------------
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
# e) Sea-level rise probability density function 2100
plot(density(kopp14_rcp85$t_2100), xlab="Projected sea level in 2100 (ft)", ylab="Probability density", yaxt="n", 
     main="", col=kopp14_col[1], lwd=2, xlim=c(-0.5,15), ylim = c(0, 1.5), bty="l")
title(main="e.", adj=0)
# lines(density(kopp14_rcp60$t_2100), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2100), col=kopp14_col[2], lwd=2)
lines(density(kopp14_rcp26$t_2100), col=kopp14_col[3], lwd=2)

lines(density(kopp17_DP16_SEW_rcp85$t_2100), col=kopp17_DP16_col[1], lwd=2)
# lines(density(kopp17_DP16_SEW_rcp60$t_2100), col=Greens[6], lwd=2)
lines(density(kopp17_DP16_SEW_rcp45$t_2100), col=kopp17_DP16_col[2], lwd=2)
lines(density(kopp17_DP16_SEW_rcp26$t_2100), col=kopp17_DP16_col[3], lwd=2)

lines(density(brickfd_rcp85$t_2100), col=brickfd_col[1], lwd=2)
# lines(density(brickfd_rcp60$t_2100), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2100), col=brickfd_col[2], lwd=2)
lines(density(brickfd_rcp26$t_2100), col=brickfd_col[3], lwd=2)

lines(density(NO_fdft_rcp85$t_2100), col=NO_fd_col[1], lwd=2)
# lines(density(NO_fdft_rcp60$t_2100), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2100), col=NO_fd_col[2], lwd=2)
lines(density(NO_fdft_rcp26$t_2100), col=NO_fd_col[3], lwd=2)

lines(density(Ras18_SEW_2p5deg$t_2100), col=Ras18_col[1], lwd=2)
lines(density(Ras18_SEW_2p0deg$t_2100), col=Ras18_col[2], lwd=2)
lines(density(Ras18_SEW_1p5deg$t_2100), col=Ras18_col[3], lwd=2)

lines(density(sweet17_03$X2100), col=sweet17_col[6], lwd=2)
lines(density(sweet17_05$X2100), col=sweet17_col[5], lwd=2)
lines(density(sweet17_10$X2100), col=sweet17_col[4], lwd=2)
lines(density(sweet17_15$X2100, na.rm=TRUE), col=sweet17_col[3], lwd=2)
lines(density(sweet17_20$X2100, na.rm=TRUE), col=sweet17_col[2], lwd=2)
lines(density(sweet17_25$X2100, na.rm=TRUE), col=sweet17_col[1], lwd=2)

lines(parris_etal_2012$t_2100, rep(1.45, 4), col=parris12_col[4:1], lwd=2, lty=3)
lines(usace2014$t_2100, rep(1.3, 3), col=usace14_col[3:1], lwd=2, lty=3)
lines(hall_etal_2016$t_2100, rep(1.15, 5), col=hall16_col[5:1], lwd=2, lty=3)
lines(sweet2017$t_2100, rep(1, length(sweet2017$t_2100)), col=sweet17_col[6:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2100, rep(1.45, 4), col=parris12_col[4:1], pch=19)
points(usace2014$t_2100, rep(1.3, 3), col=usace14_col[3:1], pch=19)
points(hall_etal_2016$t_2100, rep(1.15, 5), col=hall16_col[5:1], pch=19)
points(sweet2017$t_2100, rep(1, length(sweet2017$t_2100)), col=sweet17_col[6:1], pch=19)

dev.off()

##==============================================================================
## End
##==============================================================================
