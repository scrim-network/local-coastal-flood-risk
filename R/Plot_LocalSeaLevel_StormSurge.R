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
setwd('/Users/klr324/Documents/Data_LSL')

library(ncdf4)
library(extRemes)
library(RColorBrewer)
library(plotrix)
library(ash)
library(fields)

# Source survival function, function.
source("local-costal-flood-risk/R/Helper_scripts/plot_sf.r")

# Source Conversion functions.
source("local-costal-flood-risk/R/Helper_scripts/conversion_functions.R")

# Read in modified sea level and storm surge data.
source("local-costal-flood-risk/R/ReadAnalysis_LocalSeaLevel_StormSurge.R")

##=========================== CREATE COLORS ===================================
RdGy = brewer.pal(11, "RdGy")
BrBG = brewer.pal(11, "BrBG")
RdBu = brewer.pal(11, "RdBu")
PRGn = brewer.pal(11, "PRGn")
PiYG = brewer.pal(11, "PiYG")
tebaldi_gold = c("#f0cf0c", "#fcf5ce")

# Create sequence with black and white function.
seq_color = function(num, maincol){
  col_fun <- colorRampPalette(c("white", maincol, "black"))
  col_fun(num)
}
noaa_cols = seq_color(9, PiYG[1:5])
col_grad = seq_color(150, RdGy[7:11])

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
trans_RdGy = makeTransparent(RdGy, 75)
trans_BrBG = makeTransparent(BrBG, 75)
trans_RdBu = makeTransparent(RdBu, 75)
trans_PRGn = makeTransparent(PRGn, 75)
trans_PiYG = makeTransparent(PiYG, 75)
trans_noaa_cols = makeTransparent(noaa_cols, 75)
trans_tebaldi_gold = makeTransparent(tebaldi_gold, 200)

##=========================== PUBLICATION FIGURE SIZES ===================================
inches_to_dpi = function(inch){ inch * 300 }

text_column_width   = 5.2
minimum_width       = 2.63
full_page_width     = 7.5
full_page_height    = 8.75
single_panel_height = 4

##=========================== SLR / STORM SURGE / COMBINED PLOTS ===================================
#---------------------------- 2030 -----------------------------------
pdf(file="SLR_2030.pdf", family="Times", width=full_page_width, height=single_panel_height, pointsize=12)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4,
                2,3,4), 4, 3, byrow = TRUE))
# Add legend
par(mgp=c(1.5,.5,0), mar=c(0,4,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Tebaldi et al. 2012\nexpected value", "Sweet et al. 2017", 
                           "Parris et al. 2012","Tebaldi et al. 2012\n95% CI", "USACE 2014", 
                           "Srikrishnan et al.\nin prep. 95% CI", "Zervas 2013\nexpected value", "Hall et al. 2016", "Srikrishnan et al.\nin prep.",
                           "Zervas 2013\n 95% CI", "Observations"),
       lty=c(1,1,1,1,NA,NA,NA,NA,NA,1,NA,1,NA, NA), lwd=c(2,2,2,2,NA,NA,NA,NA,NA,2,NA,2,NA, NA), pch=c(NA,NA,NA,NA,19,19,22,19,22,NA,19,NA,22,19), 
       col=c(BrBG[9], RdGy[3], PRGn[3], tebaldi_gold[1], noaa_cols[5], BrBG[2],"black", RdBu[10], "black", BrBG[2], RdGy[9], RdBu[11], "black", "black"),
       bty='n', ncol=5, pt.bg=c(NA,NA,NA,NA,NA,NA,tebaldi_gold[2],NA,trans_RdBu[9],NA,NA,NA, trans_BrBG[2],NA), pt.cex = c(NA,NA,NA,NA,1,1,2,1,2,NA,1,NA,2, 1))
gradient.rect(7,2.5,9,4, col=col_grad, gradient="x")
arrows(8.75, 1.5, 9, 1.5, length=0.075)
text(8,1.5, "Higher scenario")

#   -----------------------------------------------------------------------
# a) Sea-level rise probability density function
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85$t_2030), xlab="Projected sea level in 2030 (ft)", ylab="Probability density", yaxt="n", 
     main="",col=RdGy[1], lwd=2, xlim=c(-0.2,2), ylim = c(0, 6.75), bty="l")
title(main="a.", adj=0)
lines(density(kopp14_rcp60$t_2030), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2030), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26$t_2030), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85$t_2030), col=BrBG[11], lwd=2)
lines(density(brickfd_rcp60$t_2030), col=BrBG[10], lwd=2)
lines(density(brickfd_rcp45$t_2030), col=BrBG[9], lwd=2)
lines(density(brickfd_rcp26$t_2030), col=BrBG[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2030), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2030), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2030), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2030), col=PRGn[5], lwd=2)

lines(noaa2012$t_2030, rep(6.5, 4), col=BrBG[4:1], lwd=1.5, lty=3)
lines(usace2013$t_2030, rep(6, 3), col=RdBu[9:11], lwd=1.5, lty=3)
lines(carswg2016$t_2030, rep(5.5, 5), col=RdGy[7:11], lwd=1.5, lty=3)
lines(noaa2017$t_2030, rep(5, length(noaa2017$t_2030)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012$t_2030, rep(6.5, 4), col=BrBG[4:1], pch=19)
points(usace2013$t_2030, rep(6, 3), col=RdBu[9:11], pch=19)
points(carswg2016$t_2030, rep(5.5, 5), col=RdGy[7:11], pch=19)
points(noaa2017$t_2030, rep(5, length(noaa2017$t_2030)), col=noaa_cols[8:2], pch=19)

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
        x = c(1/SF_Srikrishnan_stationary25$sf, rev(1/SF_Srikrishnan_stationary975$sf)), col = trans_RdBu[9], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi_gold[2], border = NA)

polygon(y = c(zervas_2013$min_95, rev(zervas_2013$max_95)), 
        x = c(1/zervas_2013$aep, rev(1/zervas_2013$aep)), col = trans_BrBG[2], border = NA)
points(NOAA_methodGEV$return_obs, NOAA_methodGEV$obs, pch = 19)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=RdBu[11], lwd=2)

lines(NOAA_rp, NOAA_rl, lwd=2, col=BrBG[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi_gold[1])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=RdBu[10])

#   -----------------------------------------------------------------------
# c) Combined sea level and storm surge return period 
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(1/NOAA_methodGEV$aep, NOAA_methodGEV$return_level, log = "x", type = "n", xlim = c(1, 500),
     ylim = c(2.85, 12), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Projected sea+surge level (ft MSL)")
title(main="c.", adj=0)
axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))

SF_k14_r85_2030_SS = plot.sf(k14_r85_SS$t_2030, make.plot=FALSE)
SF_k14_r60_2030_SS = plot.sf(k14_r60_SS$t_2030, make.plot=FALSE)
SF_k14_r45_2030_SS = plot.sf(k14_r45_SS$t_2030, make.plot=FALSE)
SF_k14_r26_2030_SS = plot.sf(k14_r26_SS$t_2030, make.plot=FALSE)
lines(1/SF_k14_r85_2030_SS$sf, SF_k14_r85_2030_SS$sf.num, col=RdGy[1], lwd=1.5)
lines(1/SF_k14_r60_2030_SS$sf, SF_k14_r60_2030_SS$sf.num, col=RdGy[2], lwd=1.5)
lines(1/SF_k14_r45_2030_SS$sf, SF_k14_r45_2030_SS$sf.num, col=RdGy[3], lwd=1.5)
lines(1/SF_k14_r26_2030_SS$sf, SF_k14_r26_2030_SS$sf.num, col=RdGy[4], lwd=1.5)

SF_bfd_r85_2030_SS = plot.sf(bfd_r85_SS$t_2030, make.plot=FALSE)
SF_bfd_r60_2030_SS = plot.sf(bfd_r60_SS$t_2030, make.plot=FALSE)
SF_bfd_r45_2030_SS = plot.sf(bfd_r45_SS$t_2030, make.plot=FALSE)
SF_bfd_r26_2030_SS = plot.sf(bfd_r26_SS$t_2030, make.plot=FALSE)
lines(1/SF_bfd_r85_2030_SS$sf, SF_bfd_r85_2030_SS$sf.num, col=BrBG[11], lwd=1.5)
lines(1/SF_bfd_r60_2030_SS$sf, SF_bfd_r60_2030_SS$sf.num, col=BrBG[10], lwd=1.5)
lines(1/SF_bfd_r45_2030_SS$sf, SF_bfd_r45_2030_SS$sf.num, col=BrBG[9], lwd=1.5)
lines(1/SF_bfd_r26_2030_SS$sf, SF_bfd_r26_2030_SS$sf.num, col=BrBG[8], lwd=1.5)

SF_NOfd_r85_2030_SS = plot.sf(NOfd_r85_SS$t_2030, make.plot=FALSE)
SF_NOfd_r60_2030_SS = plot.sf(NOfd_r60_SS$t_2030, make.plot=FALSE)
SF_NOfd_r45_2030_SS = plot.sf(NOfd_r45_SS$t_2030, make.plot=FALSE)
SF_NOfd_r26_2030_SS = plot.sf(NOfd_r26_SS$t_2030, make.plot=FALSE)
lines(1/SF_NOfd_r85_2030_SS$sf, SF_NOfd_r85_2030_SS$sf.num, col=PRGn[2], lwd=1.5)
lines(1/SF_NOfd_r60_2030_SS$sf, SF_NOfd_r60_2030_SS$sf.num, col=PRGn[3], lwd=1.5)
lines(1/SF_NOfd_r45_2030_SS$sf, SF_NOfd_r45_2030_SS$sf.num, col=PRGn[4], lwd=1.5)
lines(1/SF_NOfd_r26_2030_SS$sf, SF_NOfd_r26_2030_SS$sf.num, col=PRGn[5], lwd=1.5)

dev.off()

#---------------------------- 2050 -----------------------------------
pdf(file="SLR_2050.pdf", family="Times", width=full_page_width, height=single_panel_height, pointsize=12)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4,
                2,3,4), 4, 3, byrow = TRUE))
# Add legend
par(mgp=c(1.5,.5,0), mar=c(0,4,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Tebaldi et al. 2012\nexpected value", "Sweet et al. 2017", 
                           "Parris et al. 2012","Tebaldi et al. 2012\n95% CI", "USACE 2014", 
                           "Srikrishnan et al.\nin prep. 95% CI", "Zervas 2013\nexpected value", "Hall et al. 2016", "Srikrishnan et al.\nin prep.",
                           "Zervas 2013\n 95% CI", "Observations"),
       lty=c(1,1,1,1,NA,NA,NA,NA,NA,1,NA,1,NA, NA), lwd=c(2,2,2,2,NA,NA,NA,NA,NA,2,NA,2,NA, NA), pch=c(NA,NA,NA,NA,19,19,22,19,22,NA,19,NA,22,19), 
       col=c(BrBG[9], RdGy[3], PRGn[3], tebaldi_gold[1], noaa_cols[5], BrBG[2],"black", RdBu[10], "black", BrBG[2], RdGy[9], RdBu[11], "black", "black"),
       bty='n', ncol=5, pt.bg=c(NA,NA,NA,NA,NA,NA,tebaldi_gold[2],NA,trans_RdBu[9],NA,NA,NA, trans_BrBG[2],NA), pt.cex = c(NA,NA,NA,NA,1,1,2,1,2,NA,1,NA,2, 1))
gradient.rect(7,2.5,9,4, col=col_grad, gradient="x")
arrows(8.75, 1.5, 9, 1.5, length=0.075)
text(8,1.5, "Higher scenario")

#   -----------------------------------------------------------------------
# a) Sea-level rise probability density function
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85$t_2050), xlab="Projected sea level in 2050 (ft)", ylab="Probability density", yaxt="n", 
     main="", col=RdGy[1], lwd=2, xlim=c(-0.2,4), ylim = c(0, 3.5), bty="l")
title(main="a.", adj=0)
lines(density(kopp14_rcp60$t_2050), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2050), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26$t_2050), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85$t_2050), col=BrBG[11], lwd=2)
lines(density(brickfd_rcp60$t_2050), col=BrBG[10], lwd=2)
lines(density(brickfd_rcp45$t_2050), col=BrBG[9], lwd=2)
lines(density(brickfd_rcp26$t_2050), col=BrBG[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2050), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2050), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2050), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2050), col=PRGn[5], lwd=2)

lines(noaa2012$t_2050, rep(3.25, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2013$t_2050, rep(3, 3), col=RdBu[9:11], lwd=2, lty=3)
lines(carswg2016$t_2050, rep(2.75, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(noaa2017$t_2050, rep(2.5, length(noaa2017$t_2050)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012$t_2050, rep(3.25, 4), col=BrBG[4:1], pch=19)
points(usace2013$t_2050, rep(3, 3), col=RdBu[9:11], pch=19)
points(carswg2016$t_2050, rep(2.75, 5), col=RdGy[7:11], pch=19)
points(noaa2017$t_2050, rep(2.5, length(noaa2017$t_2050)), col=noaa_cols[8:2], pch=19)

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
        x = c(1/SF_Srikrishnan_stationary25$sf, rev(1/SF_Srikrishnan_stationary975$sf)), col = trans_RdBu[9], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi_gold[2], border = NA)

polygon(y = c(zervas_2013$min_95, rev(zervas_2013$max_95)), 
        x = c(1/zervas_2013$aep, rev(1/zervas_2013$aep)), col = trans_BrBG[2], border = NA)
points(NOAA_methodGEV$return_obs, NOAA_methodGEV$obs, pch = 19)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=RdBu[11], lwd=2)

lines(NOAA_rp, NOAA_rl, lwd=2, col=BrBG[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi_gold[1])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=RdBu[10])

#   -----------------------------------------------------------------------
# c) Combined sea level and storm surge return period 
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(1/NOAA_methodGEV$aep, NOAA_methodGEV$return_level, log = "x", type = "n", xlim = c(1, 500),
     ylim = c(2.85, 12.5), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Projected sea+surge level (ft MSL)")
title(main="c.", adj=0)
axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))

SF_k14_r85_2050_SS = plot.sf(k14_r85_SS$t_2050, make.plot=FALSE)
SF_k14_r60_2050_SS = plot.sf(k14_r60_SS$t_2050, make.plot=FALSE)
SF_k14_r45_2050_SS = plot.sf(k14_r45_SS$t_2050, make.plot=FALSE)
SF_k14_r26_2050_SS = plot.sf(k14_r26_SS$t_2050, make.plot=FALSE)
lines(1/SF_k14_r85_2050_SS$sf, SF_k14_r85_2050_SS$sf.num, col=RdGy[1], lwd=1.5)
lines(1/SF_k14_r60_2050_SS$sf, SF_k14_r60_2050_SS$sf.num, col=RdGy[2], lwd=1.5)
lines(1/SF_k14_r45_2050_SS$sf, SF_k14_r45_2050_SS$sf.num, col=RdGy[3], lwd=1.5)
lines(1/SF_k14_r26_2050_SS$sf, SF_k14_r26_2050_SS$sf.num, col=RdGy[4], lwd=1.5)

SF_bfd_r85_2050_SS = plot.sf(bfd_r85_SS$t_2050, make.plot=FALSE)
SF_bfd_r60_2050_SS = plot.sf(bfd_r60_SS$t_2050, make.plot=FALSE)
SF_bfd_r45_2050_SS = plot.sf(bfd_r45_SS$t_2050, make.plot=FALSE)
SF_bfd_r26_2050_SS = plot.sf(bfd_r26_SS$t_2050, make.plot=FALSE)
lines(1/SF_bfd_r85_2050_SS$sf, SF_bfd_r85_2050_SS$sf.num, col=BrBG[11], lwd=1.5)
lines(1/SF_bfd_r60_2050_SS$sf, SF_bfd_r60_2050_SS$sf.num, col=BrBG[10], lwd=1.5)
lines(1/SF_bfd_r45_2050_SS$sf, SF_bfd_r45_2050_SS$sf.num, col=BrBG[9], lwd=1.5)
lines(1/SF_bfd_r26_2050_SS$sf, SF_bfd_r26_2050_SS$sf.num, col=BrBG[8], lwd=1.5)

SF_NOfd_r85_2050_SS = plot.sf(NOfd_r85_SS$t_2050, make.plot=FALSE)
SF_NOfd_r60_2050_SS = plot.sf(NOfd_r60_SS$t_2050, make.plot=FALSE)
SF_NOfd_r45_2050_SS = plot.sf(NOfd_r45_SS$t_2050, make.plot=FALSE)
SF_NOfd_r26_2050_SS = plot.sf(NOfd_r26_SS$t_2050, make.plot=FALSE)
lines(1/SF_NOfd_r85_2050_SS$sf, SF_NOfd_r85_2050_SS$sf.num, col=PRGn[2], lwd=1.5)
lines(1/SF_NOfd_r60_2050_SS$sf, SF_NOfd_r60_2050_SS$sf.num, col=PRGn[3], lwd=1.5)
lines(1/SF_NOfd_r45_2050_SS$sf, SF_NOfd_r45_2050_SS$sf.num, col=PRGn[4], lwd=1.5)
lines(1/SF_NOfd_r26_2050_SS$sf, SF_NOfd_r26_2050_SS$sf.num, col=PRGn[5], lwd=1.5)

dev.off()

#---------------------------- 2070 -----------------------------------
pdf(file="SLR_2070.pdf", family="Times", width=full_page_width, height=single_panel_height, pointsize=12)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4,
                2,3,4), 4, 3, byrow = TRUE))
# Add legend
par(mgp=c(1.5,.5,0), mar=c(0,4,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Tebaldi et al. 2012\nexpected value", "Sweet et al. 2017", 
                           "Parris et al. 2012","Tebaldi et al. 2012\n95% CI", "USACE 2014", 
                           "Srikrishnan et al.\nin prep. 95% CI", "Zervas 2013\nexpected value", "Hall et al. 2016", "Srikrishnan et al.\nin prep.",
                           "Zervas 2013\n 95% CI", "Observations"),
       lty=c(1,1,1,1,NA,NA,NA,NA,NA,1,NA,1,NA, NA), lwd=c(2,2,2,2,NA,NA,NA,NA,NA,2,NA,2,NA, NA), pch=c(NA,NA,NA,NA,19,19,22,19,22,NA,19,NA,22,19), 
       col=c(BrBG[9], RdGy[3], PRGn[3], tebaldi_gold[1], noaa_cols[5], BrBG[2],"black", RdBu[10], "black", BrBG[2], RdGy[9], RdBu[11], "black", "black"),
       bty='n', ncol=5, pt.bg=c(NA,NA,NA,NA,NA,NA,tebaldi_gold[2],NA,trans_RdBu[9],NA,NA,NA, trans_BrBG[2],NA), pt.cex = c(NA,NA,NA,NA,1,1,2,1,2,NA,1,NA,2, 1))
gradient.rect(7,2.5,9,4, col=col_grad, gradient="x")
arrows(8.75, 1.5, 9, 1.5, length=0.075)
text(8,1.5, "Higher scenario")

#   -----------------------------------------------------------------------
# a) Sea-level rise probability density function
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85$t_2070), xlab="Projected sea level in 2070 (ft)", ylab="Probability density", yaxt="n",
     main="", col=RdGy[1], lwd=2, xlim=c(-0.3,8), ylim = c(0, 3), bty="l")
title(main="a.", adj=0)
lines(density(kopp14_rcp60$t_2070), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2070), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26$t_2070), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85$t_2070), col=BrBG[11], lwd=2)
lines(density(brickfd_rcp60$t_2070), col=BrBG[10], lwd=2)
lines(density(brickfd_rcp45$t_2070), col=BrBG[9], lwd=2)
lines(density(brickfd_rcp26$t_2070), col=BrBG[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2070), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2070), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2070), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2070), col=PRGn[5], lwd=2)

lines(noaa2012$t_2070, rep(2.75, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2013$t_2070, rep(2.5, 3), col=RdBu[9:11], lwd=2, lty=3)
lines(carswg2016$t_2070, rep(2.25, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(noaa2017$t_2070, rep(2, length(noaa2017$t_2070)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012$t_2070, rep(2.75, 4), col=BrBG[4:1], pch=19)
points(usace2013$t_2070, rep(2.5, 3), col=RdBu[9:11], pch=19)
points(carswg2016$t_2070, rep(2.25, 5), col=RdGy[7:11], pch=19)
points(noaa2017$t_2070, rep(2, length(noaa2017$t_2070)), col=noaa_cols[8:2], pch=19)

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
        x = c(1/SF_Srikrishnan_stationary25$sf, rev(1/SF_Srikrishnan_stationary975$sf)), col = trans_RdBu[9], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi_gold[2], border = NA)

polygon(y = c(zervas_2013$min_95, rev(zervas_2013$max_95)), 
        x = c(1/zervas_2013$aep, rev(1/zervas_2013$aep)), col = trans_BrBG[2], border = NA)
points(NOAA_methodGEV$return_obs, NOAA_methodGEV$obs, pch = 19)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=RdBu[11], lwd=2)

lines(NOAA_rp, NOAA_rl, lwd=2, col=BrBG[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi_gold[1])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=RdBu[10])

#   -----------------------------------------------------------------------
# c) Combined sea level and storm surge return period 
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(1/NOAA_methodGEV$aep, NOAA_methodGEV$return_level, log = "x", type = "n", xlim = c(1, 500),
     ylim = c(2.85, 14), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Projected sea+surge level (ft MSL)")
title(main="c.", adj=0)
axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))

SF_k14_r85_2070_SS = plot.sf(k14_r85_SS$t_2070, make.plot=FALSE)
SF_k14_r60_2070_SS = plot.sf(k14_r60_SS$t_2070, make.plot=FALSE)
SF_k14_r45_2070_SS = plot.sf(k14_r45_SS$t_2070, make.plot=FALSE)
SF_k14_r26_2070_SS = plot.sf(k14_r26_SS$t_2070, make.plot=FALSE)
lines(1/SF_k14_r85_2070_SS$sf, SF_k14_r85_2070_SS$sf.num, col=RdGy[1], lwd=1.5)
lines(1/SF_k14_r60_2070_SS$sf, SF_k14_r60_2070_SS$sf.num, col=RdGy[2], lwd=1.5)
lines(1/SF_k14_r45_2070_SS$sf, SF_k14_r45_2070_SS$sf.num, col=RdGy[3], lwd=1.5)
lines(1/SF_k14_r26_2070_SS$sf, SF_k14_r26_2070_SS$sf.num, col=RdGy[4], lwd=1.5)

SF_bfd_r85_2070_SS = plot.sf(bfd_r85_SS$t_2070, make.plot=FALSE)
SF_bfd_r60_2070_SS = plot.sf(bfd_r60_SS$t_2070, make.plot=FALSE)
SF_bfd_r45_2070_SS = plot.sf(bfd_r45_SS$t_2070, make.plot=FALSE)
SF_bfd_r26_2070_SS = plot.sf(bfd_r26_SS$t_2070, make.plot=FALSE)
lines(1/SF_bfd_r85_2070_SS$sf, SF_bfd_r85_2070_SS$sf.num, col=BrBG[11], lwd=1.5)
lines(1/SF_bfd_r60_2070_SS$sf, SF_bfd_r60_2070_SS$sf.num, col=BrBG[10], lwd=1.5)
lines(1/SF_bfd_r45_2070_SS$sf, SF_bfd_r45_2070_SS$sf.num, col=BrBG[9], lwd=1.5)
lines(1/SF_bfd_r26_2070_SS$sf, SF_bfd_r26_2070_SS$sf.num, col=BrBG[8], lwd=1.5)

SF_NOfd_r85_2070_SS = plot.sf(NOfd_r85_SS$t_2070, make.plot=FALSE)
SF_NOfd_r60_2070_SS = plot.sf(NOfd_r60_SS$t_2070, make.plot=FALSE)
SF_NOfd_r45_2070_SS = plot.sf(NOfd_r45_SS$t_2070, make.plot=FALSE)
SF_NOfd_r26_2070_SS = plot.sf(NOfd_r26_SS$t_2070, make.plot=FALSE)
lines(1/SF_NOfd_r85_2070_SS$sf, SF_NOfd_r85_2070_SS$sf.num, col=PRGn[2], lwd=1.5)
lines(1/SF_NOfd_r60_2070_SS$sf, SF_NOfd_r60_2070_SS$sf.num, col=PRGn[3], lwd=1.5)
lines(1/SF_NOfd_r45_2070_SS$sf, SF_NOfd_r45_2070_SS$sf.num, col=PRGn[4], lwd=1.5)
lines(1/SF_NOfd_r26_2070_SS$sf, SF_NOfd_r26_2070_SS$sf.num, col=PRGn[5], lwd=1.5)

dev.off()

#---------------------------- 2100 -----------------------------------
pdf(file="SLR_2100.pdf", family="Times", width=full_page_width, height=single_panel_height, pointsize=12)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4,
                2,3,4), 4, 3, byrow = TRUE))
# Add legend
par(mgp=c(1.5,.5,0), mar=c(0,4,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Tebaldi et al. 2012\nexpected value", "Sweet et al. 2017", 
                           "Parris et al. 2012","Tebaldi et al. 2012\n95% CI", "USACE 2014", 
                           "Srikrishnan et al.\nin prep. 95% CI", "Zervas 2013\nexpected value", "Hall et al. 2016", "Srikrishnan et al.\nin prep.",
                           "Zervas 2013\n 95% CI", "Observations"),
       lty=c(1,1,1,1,NA,NA,NA,NA,NA,1,NA,1,NA, NA), lwd=c(2,2,2,2,NA,NA,NA,NA,NA,2,NA,2,NA, NA), pch=c(NA,NA,NA,NA,19,19,22,19,22,NA,19,NA,22,19), 
       col=c(BrBG[9], RdGy[3], PRGn[3], tebaldi_gold[1], noaa_cols[5], BrBG[2],"black", RdBu[10], "black", BrBG[2], RdGy[9], RdBu[11], "black", "black"),
       bty='n', ncol=5, pt.bg=c(NA,NA,NA,NA,NA,NA,tebaldi_gold[2],NA,trans_RdBu[9],NA,NA,NA, trans_BrBG[2],NA), pt.cex = c(NA,NA,NA,NA,1,1,2,1,2,NA,1,NA,2, 1))
gradient.rect(7,2.5,9,4, col=col_grad, gradient="x")
arrows(8.75, 1.5, 9, 1.5, length=0.075)
text(8,1.5, "Higher scenario")

#   -----------------------------------------------------------------------
# a) Sea-level rise probability density function
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85$t_2100), xlab="Projected sea level in 2100 (ft)", ylab="Probability density", yaxt="n", 
     main="", col=RdGy[1], lwd=2, xlim=c(-0.5,15), ylim = c(0, 1.5), bty="l")
title(main="a.", adj=0)
lines(density(kopp14_rcp60$t_2100), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2100), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26$t_2100), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85$t_2100), col=BrBG[11], lwd=2)
lines(density(brickfd_rcp60$t_2100), col=BrBG[10], lwd=2)
lines(density(brickfd_rcp45$t_2100), col=BrBG[9], lwd=2)
lines(density(brickfd_rcp26$t_2100), col=BrBG[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2100), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2100), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2100), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2100), col=PRGn[5], lwd=2)

lines(noaa2012$t_2100, rep(1.45, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2013$t_2100, rep(1.3, 3), col=RdBu[9:11], lwd=2, lty=3)
lines(carswg2016$t_2100, rep(1.15, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(noaa2017$t_2100, rep(1, length(noaa2017$t_2100)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012$t_2100, rep(1.45, 4), col=BrBG[4:1], pch=19)
points(usace2013$t_2100, rep(1.3, 3), col=RdBu[9:11], pch=19)
points(carswg2016$t_2100, rep(1.15, 5), col=RdGy[7:11], pch=19)
points(noaa2017$t_2100, rep(1, length(noaa2017$t_2100)), col=noaa_cols[8:2], pch=19)

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
        x = c(1/SF_Srikrishnan_stationary25$sf, rev(1/SF_Srikrishnan_stationary975$sf)), col = trans_RdBu[9], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi_gold[2], border = NA)

polygon(y = c(zervas_2013$min_95, rev(zervas_2013$max_95)), 
        x = c(1/zervas_2013$aep, rev(1/zervas_2013$aep)), col = trans_BrBG[2], border = NA)
points(NOAA_methodGEV$return_obs, NOAA_methodGEV$obs, pch = 19)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=RdBu[11], lwd=2)

lines(NOAA_rp, NOAA_rl, lwd=2, col=BrBG[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi_gold[1])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=RdBu[10])

#   -----------------------------------------------------------------------
# c) Combined sea level and storm surge return period 
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(1/NOAA_methodGEV$aep, NOAA_methodGEV$return_level, log = "x", type = "n", xlim = c(1, 500),
     ylim = c(2.85, 16), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Projected sea+surge level (ft MSL)")
title(main="c.", adj=0)
axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))

SF_k14_r85_2100_SS = plot.sf(k14_r85_SS$t_2100, make.plot=FALSE)
SF_k14_r60_2100_SS = plot.sf(k14_r60_SS$t_2100, make.plot=FALSE)
SF_k14_r45_2100_SS = plot.sf(k14_r45_SS$t_2100, make.plot=FALSE)
SF_k14_r26_2100_SS = plot.sf(k14_r26_SS$t_2100, make.plot=FALSE)
lines(1/SF_k14_r85_2100_SS$sf, SF_k14_r85_2100_SS$sf.num, col=RdGy[1], lwd=1.5)
lines(1/SF_k14_r60_2100_SS$sf, SF_k14_r60_2100_SS$sf.num, col=RdGy[2], lwd=1.5)
lines(1/SF_k14_r45_2100_SS$sf, SF_k14_r45_2100_SS$sf.num, col=RdGy[3], lwd=1.5)
lines(1/SF_k14_r26_2100_SS$sf, SF_k14_r26_2100_SS$sf.num, col=RdGy[4], lwd=1.5)

SF_bfd_r85_2100_SS = plot.sf(bfd_r85_SS$t_2100, make.plot=FALSE)
SF_bfd_r60_2100_SS = plot.sf(bfd_r60_SS$t_2100, make.plot=FALSE)
SF_bfd_r45_2100_SS = plot.sf(bfd_r45_SS$t_2100, make.plot=FALSE)
SF_bfd_r26_2100_SS = plot.sf(bfd_r26_SS$t_2100, make.plot=FALSE)
lines(1/SF_bfd_r85_2100_SS$sf, SF_bfd_r85_2100_SS$sf.num, col=BrBG[11], lwd=1.5)
lines(1/SF_bfd_r60_2100_SS$sf, SF_bfd_r60_2100_SS$sf.num, col=BrBG[10], lwd=1.5)
lines(1/SF_bfd_r45_2100_SS$sf, SF_bfd_r45_2100_SS$sf.num, col=BrBG[9], lwd=1.5)
lines(1/SF_bfd_r26_2100_SS$sf, SF_bfd_r26_2100_SS$sf.num, col=BrBG[8], lwd=1.5)

SF_NOfd_r85_2100_SS = plot.sf(NOfd_r85_SS$t_2100, make.plot=FALSE)
SF_NOfd_r60_2100_SS = plot.sf(NOfd_r60_SS$t_2100, make.plot=FALSE)
SF_NOfd_r45_2100_SS = plot.sf(NOfd_r45_SS$t_2100, make.plot=FALSE)
SF_NOfd_r26_2100_SS = plot.sf(NOfd_r26_SS$t_2100, make.plot=FALSE)
lines(1/SF_NOfd_r85_2100_SS$sf, SF_NOfd_r85_2100_SS$sf.num, col=PRGn[2], lwd=1.5)
lines(1/SF_NOfd_r60_2100_SS$sf, SF_NOfd_r60_2100_SS$sf.num, col=PRGn[3], lwd=1.5)
lines(1/SF_NOfd_r45_2100_SS$sf, SF_NOfd_r45_2100_SS$sf.num, col=PRGn[4], lwd=1.5)
lines(1/SF_NOfd_r26_2100_SS$sf, SF_NOfd_r26_2100_SS$sf.num, col=PRGn[5], lwd=1.5)

dev.off()

##=========================== MULTIPLE YEAR SLR DENSITY PLOT ===================================
# 2030
denskopp14_rcp85_2030 <- density(kopp14_rcp85$t_2030)
denskopp14_rcp60_2030 <- density(kopp14_rcp60$t_2030)
denskopp14_rcp45_2030 <- density(kopp14_rcp45$t_2030)
denskopp14_rcp26_2030 <- density(kopp14_rcp26$t_2030)
densbrickfd_rcp85_2030 <- density(brickfd_rcp85$t_2030)
densbrickfd_rcp60_2030 <- density(brickfd_rcp60$t_2030)
densbrickfd_rcp45_2030 <- density(brickfd_rcp45$t_2030)
densbrickfd_rcp26_2030 <- density(brickfd_rcp26$t_2030)
densNO_fd_rcp85_2030 <- density(NO_fdft_rcp85$t_2030)
densNO_fd_rcp60_2030 <- density(NO_fdft_rcp60$t_2030)
densNO_fd_rcp45_2030 <- density(NO_fdft_rcp45$t_2030)
densNO_fd_rcp26_2030 <- density(NO_fdft_rcp26$t_2030)
# 2050
denskopp14_rcp85_2050 <- density(kopp14_rcp85$t_2050)
denskopp14_rcp60_2050 <- density(kopp14_rcp60$t_2050)
denskopp14_rcp45_2050 <- density(kopp14_rcp45$t_2050)
denskopp14_rcp26_2050 <- density(kopp14_rcp26$t_2050)
densbrickfd_rcp85_2050 <- density(brickfd_rcp85$t_2050)
densbrickfd_rcp60_2050 <- density(brickfd_rcp60$t_2050)
densbrickfd_rcp45_2050 <- density(brickfd_rcp45$t_2050)
densbrickfd_rcp26_2050 <- density(brickfd_rcp26$t_2050)
densNO_fd_rcp85_2050 <- density(NO_fdft_rcp85$t_2050)
densNO_fd_rcp60_2050 <- density(NO_fdft_rcp60$t_2050)
densNO_fd_rcp45_2050 <- density(NO_fdft_rcp45$t_2050)
densNO_fd_rcp26_2050 <- density(NO_fdft_rcp26$t_2050)
# 2070
denskopp14_rcp85_2070 <- density(kopp14_rcp85$t_2070)
denskopp14_rcp60_2070 <- density(kopp14_rcp60$t_2070)
denskopp14_rcp45_2070 <- density(kopp14_rcp45$t_2070)
denskopp14_rcp26_2070 <- density(kopp14_rcp26$t_2070)
densbrickfd_rcp85_2070 <- density(brickfd_rcp85$t_2070)
densbrickfd_rcp60_2070 <- density(brickfd_rcp60$t_2070)
densbrickfd_rcp45_2070 <- density(brickfd_rcp45$t_2070)
densbrickfd_rcp26_2070 <- density(brickfd_rcp26$t_2070)
densNO_fd_rcp85_2070 <- density(NO_fdft_rcp85$t_2070)
densNO_fd_rcp60_2070 <- density(NO_fdft_rcp60$t_2070)
densNO_fd_rcp45_2070 <- density(NO_fdft_rcp45$t_2070)
densNO_fd_rcp26_2070 <- density(NO_fdft_rcp26$t_2070)
# 2100
denskopp14_rcp85_2100 <- density(kopp14_rcp85$t_2100)
denskopp14_rcp60_2100 <- density(kopp14_rcp60$t_2100)
denskopp14_rcp45_2100 <- density(kopp14_rcp45$t_2100)
denskopp14_rcp26_2100 <- density(kopp14_rcp26$t_2100)
densbrickfd_rcp85_2100 <- density(brickfd_rcp85$t_2100)
densbrickfd_rcp60_2100 <- density(brickfd_rcp60$t_2100)
densbrickfd_rcp45_2100 <- density(brickfd_rcp45$t_2100)
densbrickfd_rcp26_2100 <- density(brickfd_rcp26$t_2100)
densNO_fd_rcp85_2100 <- density(NO_fdft_rcp85$t_2100)
densNO_fd_rcp60_2100 <- density(NO_fdft_rcp60$t_2100)
densNO_fd_rcp45_2100 <- density(NO_fdft_rcp45$t_2100)
densNO_fd_rcp26_2100 <- density(NO_fdft_rcp26$t_2100)

###############

pdf(file="SLR_timerepresent_v2.pdf", family="Times", width=text_column_width, height=single_panel_height, pointsize=12)
layout(matrix(c(1,1,2), 1, 3, byrow = TRUE))
par(mgp=c(1.5,.5,0), mar=c(3,3,1,1))
plot(x=denskopp14_rcp85_2100$x, y=(denskopp14_rcp85_2100$y), xlab="Projected sea level: 2030-2100 (ft)", ylab="Probability density", yaxt="n", 
     main="",col=trans_RdGy[1], type="l", lwd=1.5, xlim = range(densNO_fd_rcp26_2050$x, denskopp14_rcp85_2100$x), ylim = c(0, 18), bty="l")
abline(h=0, col="lightgray")
text(15, 0.5, "2100", col="lightgray")
# title(main="Projected sea level: 2030-2100", adj=0)

lines(x=denskopp14_rcp60_2100$x, y=(denskopp14_rcp60_2100$y), col=trans_RdGy[2], lwd=1.5)
lines(x=denskopp14_rcp45_2100$x, y=(denskopp14_rcp45_2100$y), col=trans_RdGy[3], lwd=1.5)
lines(x=denskopp14_rcp26_2100$x, y=(denskopp14_rcp26_2100$y), col=trans_RdGy[4], lwd=1.5)

lines(x=densbrickfd_rcp85_2100$x, y=(densbrickfd_rcp85_2100$y), col=trans_BrBG[11], lwd=1.5)
lines(x=densrickfd_rcp60_2100$x, y=(densbrickfd_rcp60_2100$y), col=trans_BrBG[10], lwd=1.5)
lines(x=densbrickfd_rcp45_2100$x, y=(densbrickfd_rcp45_2100$y), col=trans_BrBG[9], lwd=1.5)
lines(x=densbrickfd_rcp26_2100$x, y=(densbrickfd_rcp26_2100$y), col=trans_BrBG[8], lwd=1.5)

lines(x=densNO_fd_rcp85_2100$x, y=(densNO_fd_rcp85_2100$y), col=trans_PRGn[2], lwd=1.5)
lines(x=densNO_fd_rcp60_2100$x, y=(densNO_fd_rcp60_2100$y), col=trans_PRGn[3], lwd=1.5)
lines(x=densNO_fd_rcp45_2100$x, y=(densNO_fd_rcp45_2100$y), col=trans_PRGn[4], lwd=1.5)
lines(x=densNO_fd_rcp26_2100$x, y=(densNO_fd_rcp26_2100$y), col=trans_PRGn[5], lwd=1.5)

lines(noaa2012$t_2100, rep(2.95, 4), col=trans_BrBG[4:1], lwd=1.5, lty=3)
lines(usace2013$t_2100, rep(2.45, 3), col=trans_RdBu[9:11], lwd=1.5, lty=3)
lines(carswg2016$t_2100, rep(1.95, 5), col=trans_RdGy[7:11], lwd=1.5, lty=3)
lines(noaa2017$t_2100, rep(1.45, length(noaa2017$t_2100)), col=trans_noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012$t_2100, rep(2.95, 4), col=trans_BrBG[4:1], pch=20)
points(usace2013$t_2100, rep(2.45, 3), col=trans_RdBu[9:11], pch=20)
points(carswg2016$t_2100, rep(1.95, 5), col=trans_RdGy[7:11], pch=20)
points(noaa2017$t_2100, rep(1.45, length(noaa2017$t_2100)), col=trans_noaa_cols[8:2], pch=20)

#2070
abline(h=3.2, col="lightgray")
text(15, 3.7, "2070", col="lightgray")

lines(x=denskopp14_rcp85_2070$x, y=(denskopp14_rcp85_2070$y+3.2), col=trans_RdGy[1], lwd=1.5)
lines(x=denskopp14_rcp60_2070$x, y=(denskopp14_rcp60_2070$y+3.2), col=trans_RdGy[2], lwd=1.5)
lines(x=denskopp14_rcp45_2070$x, y=(denskopp14_rcp45_2070$y+3.2), col=trans_RdGy[3], lwd=1.5)
lines(x=denskopp14_rcp26_2070$x, y=(denskopp14_rcp26_2070$y+3.2), col=trans_RdGy[4], lwd=1.5)

lines(x=densbrickfd_rcp85_2070$x, y=(densbrickfd_rcp85_2070$y+3.2), col=trans_BrBG[11], lwd=1.5)
lines(x=densbrickfd_rcp60_2070$x, y=(densbrickfd_rcp60_2070$y+3.2), col=trans_BrBG[10], lwd=1.5)
lines(x=densbrickfd_rcp45_2070$x, y=(densbrickfd_rcp45_2070$y+3.2), col=trans_BrBG[9], lwd=1.5)
lines(x=densbrickfd_rcp26_2070$x, y=(densbrickfd_rcp26_2070$y+3.2), col=trans_BrBG[8], lwd=1.5)

lines(x=densNO_fd_rcp85_2070$x, y=(densNO_fd_rcp85_2070$y+3.2), col=trans_PRGn[2], lwd=1.5)
lines(x=densNO_fd_rcp60_2070$x, y=(densNO_fd_rcp60_2070$y+3.2), col=trans_PRGn[3], lwd=1.5)
lines(x=densNO_fd_rcp45_2070$x, y=(densNO_fd_rcp45_2070$y+3.2), col=trans_PRGn[4], lwd=1.5)
lines(x=densNO_fd_rcp26_2070$x, y=(densNO_fd_rcp26_2070$y+3.2), col=trans_PRGn[5], lwd=1.5)

lines(noaa2012$t_2070, rep(7.2, 4), col=trans_BrBG[4:1], lwd=1.5, lty=3)
lines(usace2013$t_2070, rep(6.7, 3), col=trans_RdBu[9:11], lwd=1.5, lty=3)
lines(carswg2016$t_2070, rep(6.2, 5), col=trans_RdGy[7:11], lwd=1.5, lty=3)
lines(noaa2017$t_2070, rep(5.7, length(noaa2017$t_2070)), col=trans_noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012$t_2070, rep(7.2, 4), col=trans_BrBG[4:1], pch=20)
points(usace2013$t_2070, rep(6.7, 3), col=trans_RdBu[9:11], pch=20)
points(carswg2016$t_2070, rep(6.2, 5), col=trans_RdGy[7:11], pch=20)
points(noaa2017$t_2070, rep(5.7, length(noaa2017$t_2070)), col=trans_noaa_cols[8:2], pch=20)

#2050
abline(h=7.45, col="#5f4b3a")
text(15, 7.95, "2050", col="#5f4b3a")

lines(x=denskopp14_rcp85_2050$x, y=(denskopp14_rcp85_2050$y+7.45), col=RdGy[1], lwd=1.5)
lines(x=denskopp14_rcp60_2050$x, y=(denskopp14_rcp60_2050$y+7.45), col=RdGy[2], lwd=1.5)
lines(x=denskopp14_rcp45_2050$x, y=(denskopp14_rcp45_2050$y+7.45), col=RdGy[3], lwd=1.5)
lines(x=denskopp14_rcp26_2050$x, y=(denskopp14_rcp26_2050$y+7.45), col=RdGy[4], lwd=1.5)

lines(x=densbrickfd_rcp85_2050$x, y=(densbrickfd_rcp85_2050$y+7.45), col=BrBG[11], lwd=1.5)
lines(x=densbrickfd_rcp60_2050$x, y=(densbrickfd_rcp60_2050$y+7.45), col=BrBG[10], lwd=1.5)
lines(x=densbrickfd_rcp45_2050$x, y=(densbrickfd_rcp45_2050$y+7.45), col=BrBG[9], lwd=1.5)
lines(x=densbrickfd_rcp26_2050$x, y=(densbrickfd_rcp26_2050$y+7.45), col=BrBG[8], lwd=1.5)

lines(x=densNO_fd_rcp85_2050$x, y=(densNO_fd_rcp85_2050$y+7.45), col=PRGn[2], lwd=1.5)
lines(x=densNO_fd_rcp60_2050$x, y=(densNO_fd_rcp60_2050$y+7.45), col=PRGn[3], lwd=1.5)
lines(x=densNO_fd_rcp45_2050$x, y=(densNO_fd_rcp45_2050$y+7.45), col=PRGn[4], lwd=1.5)
lines(x=densNO_fd_rcp26_2050$x, y=(densNO_fd_rcp26_2050$y+7.45), col=PRGn[5], lwd=1.5)

lines(noaa2012$t_2050, rep(11.7, 4), col=BrBG[4:1], lwd=1.5, lty=3)
lines(usace2013$t_2050, rep(11.2, 3), col=RdBu[9:11], lwd=1.5, lty=3)
lines(carswg2016$t_2050, rep(10.7, 5), col=RdGy[7:11], lwd=1.5, lty=3)
lines(noaa2017$t_2050, rep(10.2, length(noaa2017$t_2050)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012$t_2050, rep(11.7, 4), col=BrBG[4:1], pch=20)
points(usace2013$t_2050, rep(11.2, 3), col=RdBu[9:11], pch=20)
points(carswg2016$t_2050, rep(10.7, 5), col=RdGy[7:11], pch=20)
points(noaa2017$t_2050, rep(10.2, length(noaa2017$t_2050)), col=noaa_cols[8:2], pch=20)

#2030
abline(h=11.95, col="lightgray")
text(15, 12.45, "2030", col="lightgray")

lines(x=denskopp14_rcp85_2030$x, y=(denskopp14_rcp85_2030$y+11.95), col=trans_RdGy[1], lwd=1.5)
lines(x=denskopp14_rcp60_2030$x, y=(denskopp14_rcp60_2030$y+11.95), col=trans_RdGy[2], lwd=1.5)
lines(x=denskopp14_rcp45_2030$x, y=(denskopp14_rcp45_2030$y+11.95), col=trans_RdGy[3], lwd=1.5)
lines(x=denskopp14_rcp26_2030$x, y=(denskopp14_rcp26_2030$y+11.95), col=trans_RdGy[4], lwd=1.5)

lines(x=densbrickfd_rcp85_2030$x, y=(densbrickfd_rcp85_2030$y+11.95), col=trans_BrBG[11], lwd=1.5)
lines(x=densbrickfd_rcp60_2030$x, y=(densbrickfd_rcp60_2030$y+11.95), col=trans_BrBG[10], lwd=1.5)
lines(x=densbrickfd_rcp45_2030$x, y=(densbrickfd_rcp45_2030$y+11.95), col=trans_BrBG[9], lwd=1.5)
lines(x=densbrickfd_rcp26_2030$x, y=(densbrickfd_rcp26_2030$y+11.95), col=trans_BrBG[8], lwd=1.5)

lines(x=densNO_fd_rcp85_2030$x, y=(densNO_fd_rcp85_2030$y+11.95), col=trans_PRGn[2], lwd=1.5)
lines(x=densNO_fd_rcp60_2030$x, y=(densNO_fd_rcp60_2030$y+11.95), col=trans_PRGn[3], lwd=1.5)
lines(x=densNO_fd_rcp45_2030$x, y=(densNO_fd_rcp45_2030$y+11.95), col=trans_PRGn[4], lwd=1.5)
lines(x=densNO_fd_rcp26_2030$x, y=(densNO_fd_rcp26_2030$y+11.95), col=trans_PRGn[5], lwd=1.5)

lines(noaa2012$t_2030, rep(18.45, 4), col=trans_BrBG[4:1], lwd=1.5, lty=3)
lines(usace2013$t_2030, rep(17.95, 3), col=trans_RdBu[9:11], lwd=1.5, lty=3)
lines(carswg2016$t_2030, rep(17.45, 5), col=trans_RdGy[7:11], lwd=1.5, lty=3)
lines(noaa2017$t_2030, rep(16.95, length(noaa2017$t_2030)), col=trans_noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012$t_2030, rep(18.45, 4), col=trans_BrBG[4:1], pch=20)
points(usace2013$t_2030, rep(17.95, 3), col=trans_RdBu[9:11], pch=20)
points(carswg2016$t_2030, rep(17.45, 5), col=trans_RdGy[7:11], pch=20)
points(noaa2017$t_2030, rep(16.95, length(noaa2017$t_2030)), col=trans_noaa_cols[8:2], pch=20)

par(mgp=c(2,0.5,0), mar=c(3,1,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Sweet et al. 2017", 
                           "Parris et al. 2012", 
                           "USACE 2014", "Hall et al. 2016"),
       lty=c(1,1,1,NA,NA,NA,NA), lwd=c(2,2,2,NA,NA,NA,NA), pch=c(NA,NA,NA,19,19,19,19), 
       col=c(BrBG[9], RdGy[3], PRGn[3], noaa_cols[5], BrBG[2], RdBu[10], RdGy[9]),
       bty='n')
gradient.rect(-0.35,5,8,5.25, col=col_grad, gradient="x")
arrows(6.75, 4.75, 8, 4.75, length=0.075)
text(3.5,4.75, "Higher scenario")
dev.off()

##==============================================================================
## End
##==============================================================================

k14_26_5 =
k14_45_5 =
k14_60_5 =
k14_85_5 =
k14_26_95 =
k14_45_95 =
k14_60_95 =
k14_85_95 = rep(NA, length(k14_years))
for(i in 1:length(k14_years)){
  k14_26_5[i] <- quantile(convert_cm_to_ft(kopp14_rcp26_dat[,i]), 0.05)
  k14_45_5[i] <- quantile(convert_cm_to_ft(kopp14_rcp45_dat[,i]), 0.05)
  k14_60_5[i] <- quantile(convert_cm_to_ft(kopp14_rcp60_dat[,i]), 0.05, na.rm=TRUE) #goes to 2100
  k14_85_5[i] <- quantile(convert_cm_to_ft(kopp14_rcp85_dat[,i]), 0.05)
  
  k14_26_95[i] <- quantile(convert_cm_to_ft(kopp14_rcp26_dat[,i]), 0.95)
  k14_45_95[i] <- quantile(convert_cm_to_ft(kopp14_rcp45_dat[,i]), 0.95)
  k14_60_95[i] <- quantile(convert_cm_to_ft(kopp14_rcp60_dat[,i]), 0.95, na.rm=TRUE) #goes to 2100
  k14_85_95[i] <- quantile(convert_cm_to_ft(kopp14_rcp85_dat[,i]), 0.95)
}

  lsl_fdyn_26_5 =  
  lsl_fdyn_45_5 =
  lsl_fdyn_60_5 =
  lsl_fdyn_85_5 =
  lsl_fdyn_26_95 =
  lsl_fdyn_45_95 =
  lsl_fdyn_60_95 =
  lsl_fdyn_85_95 = 
    NO_fdyn_26_5 =
    NO_fdyn_45_5 =
    NO_fdyn_60_5 =
    NO_fdyn_85_5 =
    NO_fdyn_26_95 =
    NO_fdyn_45_95 =
    NO_fdyn_60_95 =
    NO_fdyn_85_95 = rep(NA, length(t.time))
for(i in 1:length(t.time)){
  lsl_fdyn_26_5[i] <- quantile(lsl_fdyn_rcp26_sub[i,], 0.05)
  lsl_fdyn_45_5[i] <- quantile(lsl_fdyn_rcp45_sub[i,], 0.05)
  lsl_fdyn_60_5[i] <- quantile(lsl_fdyn_rcp60_sub[i,], 0.05) 
  lsl_fdyn_85_5[i] <- quantile(lsl_fdyn_rcp85_sub[i,], 0.05)
  
  lsl_fdyn_26_95[i] <- quantile(lsl_fdyn_rcp26_sub[i,], 0.95)
  lsl_fdyn_45_95[i] <- quantile(lsl_fdyn_rcp45_sub[i,], 0.95)
  lsl_fdyn_60_95[i] <- quantile(lsl_fdyn_rcp60_sub[i,], 0.95) 
  lsl_fdyn_85_95[i] <- quantile(lsl_fdyn_rcp85_sub[i,], 0.95)
  
  NO_fdyn_26_5[i] <- quantile(NO_fdyn_rcp26_sub[i,], 0.05)
  NO_fdyn_45_5[i] <- quantile(NO_fdyn_rcp45_sub[i,], 0.05)
  NO_fdyn_60_5[i] <- quantile(NO_fdyn_rcp60_sub[i,], 0.05) 
  NO_fdyn_85_5[i] <- quantile(NO_fdyn_rcp85_sub[i,], 0.05)
  
  NO_fdyn_26_95[i] <- quantile(NO_fdyn_rcp26_sub[i,], 0.95)
  NO_fdyn_45_95[i] <- quantile(NO_fdyn_rcp45_sub[i,], 0.95)
  NO_fdyn_60_95[i] <- quantile(NO_fdyn_rcp60_sub[i,], 0.95) 
  NO_fdyn_85_95[i] <- quantile(NO_fdyn_rcp85_sub[i,], 0.95)
}

pdf(file="SLR_proj_pdf.pdf", family="Times", width=text_column_width, height=full_page_height, pointsize=12)
layout(matrix(c(1,1,
                2,3,
                4,5), 3, 2, byrow = TRUE))
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(k14_years, k14_85_95, type="n",xlab="Year", ylab="Projected sea level (ft)",
     ylim=c(0,11.5), xlim=c(2010, 2098), xaxt="n")
title(main="a.", adj=0)
axis(1, lwd = 1, at=seq(2010,2100, 10), label=seq(2010,2100, 10))

polygon(y = c(k14_85_5, rev(k14_85_95)), x = c(k14_years, rev(k14_years)), col = trans_RdGy[1], border = NA)
polygon(y = c(k14_60_5[1:10], rev(k14_60_95[1:10])), x = c(k14_years[1:10], rev(k14_years[1:10])), 
        col = trans_RdGy[2], border = NA)
polygon(y = c(k14_45_5, rev(k14_45_95)), x = c(k14_years, rev(k14_years)), col = trans_RdGy[3], border = NA)
polygon(y = c(k14_26_5, rev(k14_26_95)), x = c(k14_years, rev(k14_years)), col = trans_RdGy[4], border = NA)

polygon(y = c(lsl_fdyn_85_5, rev(lsl_fdyn_85_95)), x = c(t.time, rev(t.time)), col = trans_BrBG[11], border = NA)
polygon(y = c(lsl_fdyn_60_5, rev(lsl_fdyn_60_95)), x = c(t.time, rev(t.time)), col = trans_BrBG[10], border = NA)
polygon(y = c(lsl_fdyn_45_5, rev(lsl_fdyn_45_95)), x = c(t.time, rev(t.time)), col = trans_BrBG[9], border = NA)
polygon(y = c(lsl_fdyn_26_5, rev(lsl_fdyn_26_95)), x = c(t.time, rev(t.time)), col = trans_BrBG[8], border = NA)

polygon(y = c(NO_fdyn_85_5, rev(NO_fdyn_85_95)), x = c(t.time, rev(t.time)), col = trans_PRGn[2], border = NA)
polygon(y = c(NO_fdyn_60_5, rev(NO_fdyn_60_95)), x = c(t.time, rev(t.time)), col = trans_PRGn[3], border = NA)
polygon(y = c(NO_fdyn_45_5, rev(NO_fdyn_45_95)), x = c(t.time, rev(t.time)), col = trans_PRGn[4], border = NA)
polygon(y = c(NO_fdyn_26_5, rev(NO_fdyn_26_95)), x = c(t.time, rev(t.time)), col = trans_PRGn[5], border = NA)

# # NOAA et al 2012
# lines(SL_calculator_ref2000[ ,1], SL_calculator_ref2000[ ,2], col=BrBG[4])
# lines(SL_calculator_ref2000[ ,1], SL_calculator_ref2000[ ,3], col=BrBG[3])
# lines(SL_calculator_ref2000[ ,1], SL_calculator_ref2000[ ,4], col=BrBG[2])
# lines(SL_calculator_ref2000[ ,1], SL_calculator_ref2000[ ,5], col=BrBG[1])
# 
# # USACE 2013
# lines(SL_calculator_ref2000[ ,1], SL_calculator_ref2000[ ,6], col=RdBu[9])
# lines(SL_calculator_ref2000[ ,1], SL_calculator_ref2000[ ,7], col=RdBu[10])
# lines(SL_calculator_ref2000[ ,1], SL_calculator_ref2000[ ,8], col=RdBu[11])
# 
# # Hall et al 2016
# lines(SL_calculator_ref2000[ ,1], SL_calculator_ref2000[ ,9], col=RdGy[7])
# lines(SL_calculator_ref2000[ ,1], SL_calculator_ref2000[ ,10], col=RdGy[8])
# lines(SL_calculator_ref2000[ ,1], SL_calculator_ref2000[ ,11], col=RdGy[9])
# lines(SL_calculator_ref2000[ ,1], SL_calculator_ref2000[ ,12], col=RdGy[10])
# lines(SL_calculator_ref2000[ ,1], SL_calculator_ref2000[ ,13], col=RdGy[11])
# 
# # Sweet et al 2017
# lines(NOAA_etal_2017_ref2000[ ,1], NOAA_etal_2017_ref2000[ ,6], col=noaa_cols[8])
# lines(NOAA_etal_2017_ref2000[ ,1], NOAA_etal_2017_ref2000[ ,9], col=noaa_cols[7])
# lines(NOAA_etal_2017_ref2000[ ,1], NOAA_etal_2017_ref2000[ ,12], col=noaa_cols[6])
# lines(NOAA_etal_2017_ref2000[ ,1], NOAA_etal_2017_ref2000[ ,15], col=noaa_cols[5])
# lines(NOAA_etal_2017_ref2000[ ,1], NOAA_etal_2017_ref2000[ ,18], col=noaa_cols[4])
# lines(NOAA_etal_2017_ref2000[ ,1], NOAA_etal_2017_ref2000[ ,21], col=noaa_cols[3])

points(rep(2030, 4), noaa2012$t_2030, col=BrBG[4:1], pch=19)
points(rep(2030, 3), usace2013$t_2030, col=RdBu[9:11], pch=19)
points(rep(2030, 5), carswg2016$t_2030, col=RdGy[7:11], pch=19)
points(rep(2030, length(noaa2017$t_2030)), noaa2017$t_2030, col=noaa_cols[8:2], pch=19)

points(rep(2050, 4), noaa2012$t_2050, col=BrBG[4:1], pch=19)
points(rep(2050, 3), usace2013$t_2050, col=RdBu[9:11], pch=19)
points(rep(2050, 5), carswg2016$t_2050, col=RdGy[7:11], pch=19)
points(rep(2050, length(noaa2017$t_2050)), noaa2017$t_2050, col=noaa_cols[8:3], pch=19)

points(rep(2070, 4), noaa2012$t_2070, col=BrBG[4:1], pch=19)
points(rep(2070, 3), usace2013$t_2070, col=RdBu[9:11], pch=19)
points(rep(2070, 5), carswg2016$t_2070, col=RdGy[7:11], pch=19)
points(rep(2070, length(noaa2017$t_2070)), noaa2017$t_2070, col=noaa_cols[8:3], pch=19)

points(rep(2100, 4), noaa2012$t_2100, col=BrBG[4:1], pch=19)
points(rep(2100, 3), usace2013$t_2100, col=RdBu[9:11], pch=19)
points(rep(2100, 5), carswg2016$t_2100, col=RdGy[7:11], pch=19)
points(rep(2100, length(noaa2017$t_2100)), noaa2017$t_2100, col=noaa_cols[8:3], pch=19)

legend("topleft", legend=c("Wong & Keller 2017 FD 90% CI", "Kopp et al. 2014 90% CI", 
                           "Wong & Keller 2017 no FD 90% CI", "Wong & Keller 2017 FD", 
                           "Kopp et al. 2014", "Wong & Keller 2017 no FD", "Sweet et al. 2017", 
                           "Parris et al. 2012", 
                           "USACE 2014", "Hall et al. 2016"),
       lty=c(NA,NA,NA,1,1,1,NA,NA,NA,NA), lwd=c(NA,NA,NA,2,2,2,NA,NA,NA,NA), pch=c(22,22,22,NA,NA,NA,19,19,19,19), 
       col=c("black", "black", "black", BrBG[9], RdGy[3], PRGn[3], noaa_cols[5], BrBG[2], RdBu[10], RdGy[9]),
       bty='n', pt.bg=c(BrBG[9], RdGy[3], PRGn[3],NA,NA,NA,NA,NA,NA,NA), pt.cex = c(2,2,2,NA,NA,NA,1,1,1,1))
gradient.rect(2008,4,2012,4.5, col=col_grad, gradient="x")
arrows(2028.5, 4.25, 2030.5, 4.25, length=0.075)
text(2020.5,4.25, "Higher scenario")

#--------------------------
# b) Sea-level rise probability density function 2030
plot(density(kopp14_rcp85$t_2030), xlab="Projected sea level in 2030 (ft)", ylab="Probability density", yaxt="n", 
     main="",col=RdGy[1], lwd=2, xlim=c(-0.2,2), ylim = c(0, 6.75), bty="l")
title(main="b.", adj=0)
lines(density(kopp14_rcp60$t_2030), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2030), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26$t_2030), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85$t_2030), col=BrBG[11], lwd=2)
lines(density(brickfd_rcp60$t_2030), col=BrBG[10], lwd=2)
lines(density(brickfd_rcp45$t_2030), col=BrBG[9], lwd=2)
lines(density(brickfd_rcp26$t_2030), col=BrBG[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2030), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2030), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2030), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2030), col=PRGn[5], lwd=2)

lines(noaa2012$t_2030, rep(6.5, 4), col=BrBG[4:1], lwd=1.5, lty=3)
lines(usace2013$t_2030, rep(6, 3), col=RdBu[9:11], lwd=1.5, lty=3)
lines(carswg2016$t_2030, rep(5.5, 5), col=RdGy[7:11], lwd=1.5, lty=3)
lines(noaa2017$t_2030, rep(5, length(noaa2017$t_2030)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012$t_2030, rep(6.5, 4), col=BrBG[4:1], pch=19)
points(usace2013$t_2030, rep(6, 3), col=RdBu[9:11], pch=19)
points(carswg2016$t_2030, rep(5.5, 5), col=RdGy[7:11], pch=19)
points(noaa2017$t_2030, rep(5, length(noaa2017$t_2030)), col=noaa_cols[8:2], pch=19)

#   -----------------------------------------------------------------------
# c) Sea-level rise probability density function 2050
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(density(kopp14_rcp85$t_2050), xlab="Projected sea level in 2050 (ft)", ylab="Probability density", yaxt="n", 
     main="", col=RdGy[1], lwd=2, xlim=c(-0.2,4), ylim = c(0, 3.5), bty="l")
title(main="c.", adj=0)
lines(density(kopp14_rcp60$t_2050), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2050), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26$t_2050), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85$t_2050), col=BrBG[11], lwd=2)
lines(density(brickfd_rcp60$t_2050), col=BrBG[10], lwd=2)
lines(density(brickfd_rcp45$t_2050), col=BrBG[9], lwd=2)
lines(density(brickfd_rcp26$t_2050), col=BrBG[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2050), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2050), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2050), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2050), col=PRGn[5], lwd=2)

lines(noaa2012$t_2050, rep(3.25, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2013$t_2050, rep(3, 3), col=RdBu[9:11], lwd=2, lty=3)
lines(carswg2016$t_2050, rep(2.75, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(noaa2017$t_2050, rep(2.5, length(noaa2017$t_2050)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012$t_2050, rep(3.25, 4), col=BrBG[4:1], pch=19)
points(usace2013$t_2050, rep(3, 3), col=RdBu[9:11], pch=19)
points(carswg2016$t_2050, rep(2.75, 5), col=RdGy[7:11], pch=19)
points(noaa2017$t_2050, rep(2.5, length(noaa2017$t_2050)), col=noaa_cols[8:2], pch=19)

#   -----------------------------------------------------------------------
# d) Sea-level rise probability density function 2070
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85$t_2070), xlab="Projected sea level in 2070 (ft)", ylab="Probability density", yaxt="n",
     main="", col=RdGy[1], lwd=2, xlim=c(-0.3,8), ylim = c(0, 3), bty="l")
title(main="d.", adj=0)
lines(density(kopp14_rcp60$t_2070), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2070), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26$t_2070), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85$t_2070), col=BrBG[11], lwd=2)
lines(density(brickfd_rcp60$t_2070), col=BrBG[10], lwd=2)
lines(density(brickfd_rcp45$t_2070), col=BrBG[9], lwd=2)
lines(density(brickfd_rcp26$t_2070), col=BrBG[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2070), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2070), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2070), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2070), col=PRGn[5], lwd=2)

lines(noaa2012$t_2070, rep(2.75, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2013$t_2070, rep(2.5, 3), col=RdBu[9:11], lwd=2, lty=3)
lines(carswg2016$t_2070, rep(2.25, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(noaa2017$t_2070, rep(2, length(noaa2017$t_2070)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012$t_2070, rep(2.75, 4), col=BrBG[4:1], pch=19)
points(usace2013$t_2070, rep(2.5, 3), col=RdBu[9:11], pch=19)
points(carswg2016$t_2070, rep(2.25, 5), col=RdGy[7:11], pch=19)
points(noaa2017$t_2070, rep(2, length(noaa2017$t_2070)), col=noaa_cols[8:2], pch=19)

#   -----------------------------------------------------------------------
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
# e) Sea-level rise probability density function 2100
plot(density(kopp14_rcp85$t_2100), xlab="Projected sea level in 2100 (ft)", ylab="Probability density", yaxt="n", 
     main="", col=RdGy[1], lwd=2, xlim=c(-0.5,15), ylim = c(0, 1.5), bty="l")
title(main="e.", adj=0)
lines(density(kopp14_rcp60$t_2100), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2100), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26$t_2100), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85$t_2100), col=BrBG[11], lwd=2)
lines(density(brickfd_rcp60$t_2100), col=BrBG[10], lwd=2)
lines(density(brickfd_rcp45$t_2100), col=BrBG[9], lwd=2)
lines(density(brickfd_rcp26$t_2100), col=BrBG[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2100), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2100), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2100), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2100), col=PRGn[5], lwd=2)

lines(noaa2012$t_2100, rep(1.45, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2013$t_2100, rep(1.3, 3), col=RdBu[9:11], lwd=2, lty=3)
lines(carswg2016$t_2100, rep(1.15, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(noaa2017$t_2100, rep(1, length(noaa2017$t_2100)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012$t_2100, rep(1.45, 4), col=BrBG[4:1], pch=19)
points(usace2013$t_2100, rep(1.3, 3), col=RdBu[9:11], pch=19)
points(carswg2016$t_2100, rep(1.15, 5), col=RdGy[7:11], pch=19)
points(noaa2017$t_2100, rep(1, length(noaa2017$t_2100)), col=noaa_cols[8:2], pch=19)

dev.off()

##==============================================================================
## End
##==============================================================================
