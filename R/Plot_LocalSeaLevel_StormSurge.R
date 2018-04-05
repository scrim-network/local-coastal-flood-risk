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

RdGy = brewer.pal(11, "RdGy")
BrBG = brewer.pal(11, "BrBG")
RdBu = brewer.pal(11, "RdBu")
PRGn = brewer.pal(11, "PRGn")
PiYG = brewer.pal(11, "PiYG")
tebaldi_gold = c("#f0cf0c", "#fcf5ce")

sweet_cols = seq_color(9, PiYG[1:5])
col_grad = seq_color(150, RdGy[7:11])

trans_RdGy = makeTransparent(RdGy, 100)
trans_BrBG = makeTransparent(BrBG, 100)
trans_RdBu = makeTransparent(RdBu, 100)
trans_PRGn = makeTransparent(PRGn, 100)
trans_PiYG = makeTransparent(PiYG, 100)
trans_sweet_cols = makeTransparent(sweet_cols, 100)
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
       col=c(RdBu[9], RdGy[3], PRGn[3], tebaldi_gold[1], sweet_cols[5], BrBG[2],"black", BrBG[9], "black", BrBG[2], RdGy[9], RdBu[11], "black", "black"),
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

lines(density(brickfd_rcp85$t_2030), col=RdBu[11], lwd=2)
lines(density(brickfd_rcp60$t_2030), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2030), col=RdBu[9], lwd=2)
lines(density(brickfd_rcp26$t_2030), col=RdBu[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2030), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2030), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2030), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2030), col=PRGn[5], lwd=2)

lines(parris_etal_2012$t_2030, rep(6.5, 4), col=BrBG[4:1], lwd=1.5, lty=3)
lines(usace2014$t_2030, rep(6, 3), col=BrBG[8:10], lwd=1.5, lty=3)
lines(hall_etal_2016$t_2030, rep(5.5, 5), col=RdGy[7:11], lwd=1.5, lty=3)
lines(sweet2017$t_2030, rep(5, length(sweet2017$t_2030)), col=sweet_cols[8:2], lwd=1.5, lty=3)

points(parris_etal_2012$t_2030, rep(6.5, 4), col=BrBG[4:1], pch=19)
points(usace2014$t_2030, rep(6, 3), col=BrBG[8:10], pch=19)
points(hall_etal_2016$t_2030, rep(5.5, 5), col=RdGy[7:11], pch=19)
points(sweet2017$t_2030, rep(5, length(sweet2017$t_2030)), col=sweet_cols[8:2], pch=19)

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

lines(zervas_2013$NOAA_rp, zervas_2013$NOAA_rl_feet, lwd=2, col=BrBG[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi_gold[1])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=BrBG[9])

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
lines(1/SF_bfd_r85_2030_SS$sf, SF_bfd_r85_2030_SS$sf.num, col=RdBu[11], lwd=1.5)
lines(1/SF_bfd_r60_2030_SS$sf, SF_bfd_r60_2030_SS$sf.num, col=RdBu[10], lwd=1.5)
lines(1/SF_bfd_r45_2030_SS$sf, SF_bfd_r45_2030_SS$sf.num, col=RdBu[9], lwd=1.5)
lines(1/SF_bfd_r26_2030_SS$sf, SF_bfd_r26_2030_SS$sf.num, col=RdBu[8], lwd=1.5)

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
       col=c(RdBu[9], RdGy[3], PRGn[3], tebaldi_gold[1], sweet_cols[5], BrBG[2],"black", BrBG[9], "black", BrBG[2], RdGy[9], RdBu[11], "black", "black"),
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

lines(density(brickfd_rcp85$t_2050), col=RdBu[11], lwd=2)
lines(density(brickfd_rcp60$t_2050), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2050), col=RdBu[9], lwd=2)
lines(density(brickfd_rcp26$t_2050), col=RdBu[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2050), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2050), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2050), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2050), col=PRGn[5], lwd=2)

lines(parris_etal_2012$t_2050, rep(3.25, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2014$t_2050, rep(3, 3), col=BrBG[8:10], lwd=2, lty=3)
lines(hall_etal_2016$t_2050, rep(2.75, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(sweet2017$t_2050, rep(2.5, length(sweet2017$t_2050)), col=sweet_cols[8:2], lwd=1.5, lty=3)

points(parris_etal_2012$t_2050, rep(3.25, 4), col=BrBG[4:1], pch=19)
points(usace2014$t_2050, rep(3, 3), col=BrBG[8:10], pch=19)
points(hall_etal_2016$t_2050, rep(2.75, 5), col=RdGy[7:11], pch=19)
points(sweet2017$t_2050, rep(2.5, length(sweet2017$t_2050)), col=sweet_cols[8:2], pch=19)

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

lines(zervas_2013$NOAA_rp, zervas_2013$NOAA_rl_feet, lwd=2, col=BrBG[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi_gold[1])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=BrBG[9])

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
lines(1/SF_bfd_r85_2050_SS$sf, SF_bfd_r85_2050_SS$sf.num, col=RdBu[11], lwd=1.5)
lines(1/SF_bfd_r60_2050_SS$sf, SF_bfd_r60_2050_SS$sf.num, col=RdBu[10], lwd=1.5)
lines(1/SF_bfd_r45_2050_SS$sf, SF_bfd_r45_2050_SS$sf.num, col=RdBu[9], lwd=1.5)
lines(1/SF_bfd_r26_2050_SS$sf, SF_bfd_r26_2050_SS$sf.num, col=RdBu[8], lwd=1.5)

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
       col=c(RdBu[9], RdGy[3], PRGn[3], tebaldi_gold[1], sweet_cols[5], BrBG[2],"black", BrBG[9], "black", BrBG[2], RdGy[9], RdBu[11], "black", "black"),
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

lines(density(brickfd_rcp85$t_2070), col=RdBu[11], lwd=2)
lines(density(brickfd_rcp60$t_2070), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2070), col=RdBu[9], lwd=2)
lines(density(brickfd_rcp26$t_2070), col=RdBu[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2070), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2070), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2070), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2070), col=PRGn[5], lwd=2)

lines(parris_etal_2012$t_2070, rep(2.75, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2014$t_2070, rep(2.5, 3), col=BrBG[8:10], lwd=2, lty=3)
lines(hall_etal_2016$t_2070, rep(2.25, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(sweet2017$t_2070, rep(2, length(sweet2017$t_2070)), col=sweet_cols[8:2], lwd=1.5, lty=3)

points(parris_etal_2012$t_2070, rep(2.75, 4), col=BrBG[4:1], pch=19)
points(usace2014$t_2070, rep(2.5, 3), col=BrBG[8:10], pch=19)
points(hall_etal_2016$t_2070, rep(2.25, 5), col=RdGy[7:11], pch=19)
points(sweet2017$t_2070, rep(2, length(sweet2017$t_2070)), col=sweet_cols[8:2], pch=19)

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

lines(zervas_2013$NOAA_rp, zervas_2013$NOAA_rl_feet, lwd=2, col=BrBG[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi_gold[1])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=BrBG[9])

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
lines(1/SF_bfd_r85_2070_SS$sf, SF_bfd_r85_2070_SS$sf.num, col=RdBu[11], lwd=1.5)
lines(1/SF_bfd_r60_2070_SS$sf, SF_bfd_r60_2070_SS$sf.num, col=RdBu[10], lwd=1.5)
lines(1/SF_bfd_r45_2070_SS$sf, SF_bfd_r45_2070_SS$sf.num, col=RdBu[9], lwd=1.5)
lines(1/SF_bfd_r26_2070_SS$sf, SF_bfd_r26_2070_SS$sf.num, col=RdBu[8], lwd=1.5)

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
       col=c(RdBu[9], RdGy[3], PRGn[3], tebaldi_gold[1], sweet_cols[5], BrBG[2],"black", BrBG[9], "black", BrBG[2], RdGy[9], RdBu[11], "black", "black"),
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

lines(density(brickfd_rcp85$t_2100), col=RdBu[11], lwd=2)
lines(density(brickfd_rcp60$t_2100), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2100), col=RdBu[9], lwd=2)
lines(density(brickfd_rcp26$t_2100), col=RdBu[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2100), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2100), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2100), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2100), col=PRGn[5], lwd=2)

lines(parris_etal_2012$t_2100, rep(1.45, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2014$t_2100, rep(1.3, 3), col=BrBG[8:10], lwd=2, lty=3)
lines(hall_etal_2016$t_2100, rep(1.15, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(sweet2017$t_2100, rep(1, length(sweet2017$t_2100)), col=sweet_cols[8:2], lwd=1.5, lty=3)

points(parris_etal_2012$t_2100, rep(1.45, 4), col=BrBG[4:1], pch=19)
points(usace2014$t_2100, rep(1.3, 3), col=BrBG[8:10], pch=19)
points(hall_etal_2016$t_2100, rep(1.15, 5), col=RdGy[7:11], pch=19)
points(sweet2017$t_2100, rep(1, length(sweet2017$t_2100)), col=sweet_cols[8:2], pch=19)

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

lines(zervas_2013$NOAA_rp, zervas_2013$NOAA_rl_feet, lwd=2, col=BrBG[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi_gold[1])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=BrBG[9])

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
lines(1/SF_bfd_r85_2100_SS$sf, SF_bfd_r85_2100_SS$sf.num, col=RdBu[11], lwd=1.5)
lines(1/SF_bfd_r60_2100_SS$sf, SF_bfd_r60_2100_SS$sf.num, col=RdBu[10], lwd=1.5)
lines(1/SF_bfd_r45_2100_SS$sf, SF_bfd_r45_2100_SS$sf.num, col=RdBu[9], lwd=1.5)
lines(1/SF_bfd_r26_2100_SS$sf, SF_bfd_r26_2100_SS$sf.num, col=RdBu[8], lwd=1.5)

SF_NOfd_r85_2100_SS = plot.sf(NOfd_r85_SS$t_2100, make.plot=FALSE)
SF_NOfd_r60_2100_SS = plot.sf(NOfd_r60_SS$t_2100, make.plot=FALSE)
SF_NOfd_r45_2100_SS = plot.sf(NOfd_r45_SS$t_2100, make.plot=FALSE)
SF_NOfd_r26_2100_SS = plot.sf(NOfd_r26_SS$t_2100, make.plot=FALSE)
lines(1/SF_NOfd_r85_2100_SS$sf, SF_NOfd_r85_2100_SS$sf.num, col=PRGn[2], lwd=1.5)
lines(1/SF_NOfd_r60_2100_SS$sf, SF_NOfd_r60_2100_SS$sf.num, col=PRGn[3], lwd=1.5)
lines(1/SF_NOfd_r45_2100_SS$sf, SF_NOfd_r45_2100_SS$sf.num, col=PRGn[4], lwd=1.5)
lines(1/SF_NOfd_r26_2100_SS$sf, SF_NOfd_r26_2100_SS$sf.num, col=PRGn[5], lwd=1.5)

dev.off()

##=========================== MULTIPLE YEAR SLR DENSITY & PROJECTION PLOT ===================================
# Extract 90% CI
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
     ylim=c(0,6), xlim=c(2010, 2098), xaxt="n")
title(main="a.", adj=0)
axis(1, lwd = 1, at=seq(2010,2100, 10), label=seq(2010,2100, 10))

# Only plot RCP 8.5
polygon(y = c(k14_85_5, rev(k14_85_95)), x = c(k14_years, rev(k14_years)), col = trans_RdGy[1], border = NA)
polygon(y = c(lsl_fdyn_85_5, rev(lsl_fdyn_85_95)), x = c(t.time, rev(t.time)), col = trans_RdBu[10], border = NA)
polygon(y = c(NO_fdyn_85_5, rev(NO_fdyn_85_95)), x = c(t.time, rev(t.time)), col = trans_PRGn[2], border = NA)

legend("topleft", legend=c("Wong & Keller 2017 FD RCP85 90% CI", "Wong & Keller 2017 no FD RCP85 90% CI", "Kopp et al. 2014 RCP85 90% CI",
                           "Wong & Keller 2017 FD", "Wong & Keller 2017 no FD", "Kopp et al. 2014", "Sweet et al. 2017",
                           "Parris et al. 2012",
                           "USACE 2014", "Hall et al. 2016"),
       lty=c(NA,NA,NA,1,1,1,NA,NA,NA,NA), lwd=c(NA,NA,NA,2,2,2,NA,NA,NA,NA), pch=c(22,22,22,NA,NA,NA,19,19,19,19),
       col=c("black", "black", "black", RdBu[9], PRGn[3], RdGy[3], sweet_cols[5], BrBG[2], BrBG[9], RdGy[9]),
       bty='n', pt.bg=c(trans_RdBu[10], trans_PRGn[2], trans_RdGy[1],NA,NA,NA,NA,NA,NA,NA), pt.cex = c(2,2,2,NA,NA,NA,1,1,1,1))
gradient.rect(2008,2,2012,2.25, col=col_grad, gradient="x")
arrows(2028.5, 2.12, 2030.5, 2.12, length=0.075)
text(2020.5,2.12, "Higher scenario")

#--------------------------
# b) Sea-level rise probability density function 2030
plot(density(kopp14_rcp85$t_2030), xlab="Projected sea level in 2030 (ft)", ylab="Probability density", yaxt="n", 
     main="",col=RdGy[1], lwd=2, xlim=c(-0.2,2), ylim = c(0, 6.75), bty="l")
title(main="b.", adj=0)
lines(density(kopp14_rcp60$t_2030), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2030), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26$t_2030), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85$t_2030), col=RdBu[11], lwd=2)
lines(density(brickfd_rcp60$t_2030), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2030), col=RdBu[9], lwd=2)
lines(density(brickfd_rcp26$t_2030), col=RdBu[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2030), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2030), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2030), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2030), col=PRGn[5], lwd=2)

lines(parris_etal_2012$t_2030, rep(6.5, 4), col=BrBG[4:1], lwd=1.5, lty=3)
lines(usace2014$t_2030, rep(6, 3), col=BrBG[8:10], lwd=1.5, lty=3)
lines(hall_etal_2016$t_2030, rep(5.5, 5), col=RdGy[7:11], lwd=1.5, lty=3)
lines(sweet2017$t_2030, rep(5, length(sweet2017$t_2030)), col=sweet_cols[8:2], lwd=1.5, lty=3)

points(parris_etal_2012$t_2030, rep(6.5, 4), col=BrBG[4:1], pch=19)
points(usace2014$t_2030, rep(6, 3), col=BrBG[8:10], pch=19)
points(hall_etal_2016$t_2030, rep(5.5, 5), col=RdGy[7:11], pch=19)
points(sweet2017$t_2030, rep(5, length(sweet2017$t_2030)), col=sweet_cols[8:2], pch=19)

#   -----------------------------------------------------------------------
# c) Sea-level rise probability density function 2050
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(density(kopp14_rcp85$t_2050), xlab="Projected sea level in 2050 (ft)", ylab="Probability density", yaxt="n", 
     main="", col=RdGy[1], lwd=2, xlim=c(-0.2,4), ylim = c(0, 3.5), bty="l")
title(main="c.", adj=0)
lines(density(kopp14_rcp60$t_2050), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2050), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26$t_2050), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85$t_2050), col=RdBu[11], lwd=2)
lines(density(brickfd_rcp60$t_2050), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2050), col=RdBu[9], lwd=2)
lines(density(brickfd_rcp26$t_2050), col=RdBu[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2050), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2050), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2050), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2050), col=PRGn[5], lwd=2)

lines(parris_etal_2012$t_2050, rep(3.25, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2014$t_2050, rep(3, 3), col=BrBG[8:10], lwd=2, lty=3)
lines(hall_etal_2016$t_2050, rep(2.75, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(sweet2017$t_2050, rep(2.5, length(sweet2017$t_2050)), col=sweet_cols[8:2], lwd=1.5, lty=3)

points(parris_etal_2012$t_2050, rep(3.25, 4), col=BrBG[4:1], pch=19)
points(usace2014$t_2050, rep(3, 3), col=BrBG[8:10], pch=19)
points(hall_etal_2016$t_2050, rep(2.75, 5), col=RdGy[7:11], pch=19)
points(sweet2017$t_2050, rep(2.5, length(sweet2017$t_2050)), col=sweet_cols[8:2], pch=19)

#   -----------------------------------------------------------------------
# d) Sea-level rise probability density function 2070
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85$t_2070), xlab="Projected sea level in 2070 (ft)", ylab="Probability density", yaxt="n",
     main="", col=RdGy[1], lwd=2, xlim=c(-0.3,8), ylim = c(0, 3), bty="l")
title(main="d.", adj=0)
lines(density(kopp14_rcp60$t_2070), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2070), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26$t_2070), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85$t_2070), col=RdBu[11], lwd=2)
lines(density(brickfd_rcp60$t_2070), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2070), col=RdBu[9], lwd=2)
lines(density(brickfd_rcp26$t_2070), col=RdBu[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2070), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2070), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2070), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2070), col=PRGn[5], lwd=2)

lines(parris_etal_2012$t_2070, rep(2.75, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2014$t_2070, rep(2.5, 3), col=BrBG[8:10], lwd=2, lty=3)
lines(hall_etal_2016$t_2070, rep(2.25, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(sweet2017$t_2070, rep(2, length(sweet2017$t_2070)), col=sweet_cols[8:2], lwd=1.5, lty=3)

points(parris_etal_2012$t_2070, rep(2.75, 4), col=BrBG[4:1], pch=19)
points(usace2014$t_2070, rep(2.5, 3), col=BrBG[8:10], pch=19)
points(hall_etal_2016$t_2070, rep(2.25, 5), col=RdGy[7:11], pch=19)
points(sweet2017$t_2070, rep(2, length(sweet2017$t_2070)), col=sweet_cols[8:2], pch=19)

#   -----------------------------------------------------------------------
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
# e) Sea-level rise probability density function 2100
plot(density(kopp14_rcp85$t_2100), xlab="Projected sea level in 2100 (ft)", ylab="Probability density", yaxt="n", 
     main="", col=RdGy[1], lwd=2, xlim=c(-0.5,15), ylim = c(0, 1.5), bty="l")
title(main="e.", adj=0)
lines(density(kopp14_rcp60$t_2100), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45$t_2100), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26$t_2100), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85$t_2100), col=RdBu[11], lwd=2)
lines(density(brickfd_rcp60$t_2100), col=RdBu[10], lwd=2)
lines(density(brickfd_rcp45$t_2100), col=RdBu[9], lwd=2)
lines(density(brickfd_rcp26$t_2100), col=RdBu[8], lwd=2)

lines(density(NO_fdft_rcp85$t_2100), col=PRGn[2], lwd=2)
lines(density(NO_fdft_rcp60$t_2100), col=PRGn[3], lwd=2)
lines(density(NO_fdft_rcp45$t_2100), col=PRGn[4], lwd=2)
lines(density(NO_fdft_rcp26$t_2100), col=PRGn[5], lwd=2)

lines(parris_etal_2012$t_2100, rep(1.45, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2014$t_2100, rep(1.3, 3), col=BrBG[8:10], lwd=2, lty=3)
lines(hall_etal_2016$t_2100, rep(1.15, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(sweet2017$t_2100, rep(1, length(sweet2017$t_2100)), col=sweet_cols[8:2], lwd=1.5, lty=3)

points(parris_etal_2012$t_2100, rep(1.45, 4), col=BrBG[4:1], pch=19)
points(usace2014$t_2100, rep(1.3, 3), col=BrBG[8:10], pch=19)
points(hall_etal_2016$t_2100, rep(1.15, 5), col=RdGy[7:11], pch=19)
points(sweet2017$t_2100, rep(1, length(sweet2017$t_2100)), col=sweet_cols[8:2], pch=19)

dev.off()

##==============================================================================
## End
##==============================================================================
