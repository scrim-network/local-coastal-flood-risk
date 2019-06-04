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
# setwd('/Users/klr324/Documents/GitHub/local-coastal-flood-risk/R')
# install.packages("svglite")

library(ncdf4)
library(extRemes)
library(RColorBrewer)
library(plotrix)
library(ash)
library(fields)
library(diagram)
library(DEoptim)
library(stringr)
library(svglite) # XQuartz is required to use Cairo

# Source survival function, function.
source("Helper_scripts/plot_SLRcompare_PDF.R")
source("Helper_scripts/plot_SLRandStormSurge_CI.R")

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

# Flow chart colors
Lmoss_green = "#C5E0B4" 
pale_yellow = "#FFF2CC"

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
trans_wong18_col = makeTransparent(wong_18$color, 150)
trans_zervas13_col = makeTransparent(zervas13_col, 150)

##=========================== PUBLICATION FIGURE SIZES ===================================
# Figures should be sized between:
# 1/4 page figure = 95 mm x 115 mm (3.74016 x 4.52756 in)
# Full page = 190 mm x 230 mm (7.48031 x 9.05512 in)
inches_to_dpi = function(inch){ inch * 300 }
mm_to_inches = function(mm){ mm * 0.0393701 }

# For most journals the figures should be 39 mm, 84 mm, 129 mm, or 174 mm wide and not higher than 234 mm.
# For books and book-sized journals, the figures should be 80 mm or 122 mm wide and not higher than 198 mm.

quart_width = mm_to_inches(95)
quart_height = mm_to_inches(115)
full_width = mm_to_inches(190)
full_height = mm_to_inches(230)

text_column_width   = 5.2
minimum_width       = 2.63
full_page_width     = 7.5
full_page_height    = 8.75
single_panel_height = 4

##=========================== FIGURE LABELING ===================================
# https://www.r-bloggers.com/adding-figure-labels-a-b-c-in-the-top-left-corner-of-the-plotting-region/
fig_label <- function(text, region="figure", pos="topleft", cex=NULL, ...) {
  
  region <- match.arg(region, c("figure", "plot", "device"))
  pos <- match.arg(pos, c("topleft", "top", "topright", 
                          "left", "center", "right", 
                          "bottomleft", "bottom", "bottomright"))
  
  if(region %in% c("figure", "device")) {
    ds <- dev.size("in")
    # xy coordinates of device corners in user coordinates
    x <- grconvertX(c(0, ds[1]), from="in", to="user")
    y <- grconvertY(c(0, ds[2]), from="in", to="user")
    
    # fragment of the device we use to plot
    if(region == "figure") {
      # account for the fragment of the device that 
      # the figure is using
      fig <- par("fig")
      dx <- (x[2] - x[1])
      dy <- (y[2] - y[1])
      x <- x[1] + dx * fig[1:2]
      y <- y[1] + dy * fig[3:4]
    } 
  }
  
  # much simpler if in plotting region
  if(region == "plot") {
    u <- par("usr")
    x <- u[1:2]
    y <- u[3:4]
  }
  
  sw <- strwidth(text, cex=cex) * 60/100
  sh <- strheight(text, cex=cex) * 60/100
  
  x1 <- switch(pos,
               topleft     =x[1] + sw, 
               left        =x[1] + sw,
               bottomleft  =x[1] + sw,
               top         =(x[1] + x[2])/2,
               center      =(x[1] + x[2])/2,
               bottom      =(x[1] + x[2])/2,
               topright    =x[2] - sw,
               right       =x[2] - sw,
               bottomright =x[2] - sw)
  
  y1 <- switch(pos,
               topleft     =y[2] - sh,
               top         =y[2] - sh,
               topright    =y[2] - sh,
               left        =(y[1] + y[2])/2,
               center      =(y[1] + y[2])/2,
               right       =(y[1] + y[2])/2,
               bottomleft  =y[1] + sh,
               bottom      =y[1] + sh,
               bottomright =y[1] + sh)
  
  old.par <- par(xpd=NA)
  on.exit(par(old.par))
  
  text(x1, y1, text, cex=cex, ...)
  return(invisible(c(x,y)))
}

##=========================== FLOW DIAGRAM ===================================
# Designate coordinates for box midpoints
elpos  <- coordinates (rep(6,18))

# Create matrix for straight arrows
fromto_straight <- matrix(ncol = 2, byrow = TRUE, data = c(16, 17, 
                                                           26, 27, 
                                                           35, 36, 
                                                           47, 48, 
                                                           63, 64,
                                                           64, 65, 
                                                           65, 66,
                                                           77, 78, 
                                                           87, 88,
                                                           88, 89,
                                                           89, 90,
                                                           101, 102))
# Create matrix for arrows that split
fromto_split <- matrix(ncol = 3, byrow = TRUE, data = c(17, 12, 24, 
                                                        27, 16, 40,
                                                        40, 35, 47,
                                                        49, 26, 74,
                                                        74, 63, 87, 
                                                        88, 77, 101))
# Create label vectors for circle and rectangle boxes
lab_circ = c("Choice\nof sea-\nlevel\ndata",
             "Kopp et\nal.\n[2017]",
             "Wong\nand\nKeller\n[2017]",
             "Rasmu-\nssen et\nal.\n[2018]",
             "Kopp et\nal.\n[2014]",
             "Sweet\net al.\n[2017]",
             "Hall et\nal.\n[2016]",
             "Parris\net al.\n[2012]",
             "USACE\n[2011,\n2013,\n2014]")
lab_rect = c("Accounts\nfor ice\nsheet\nfeedback\nprocesses",
             "Based on\nRCP\nscenarios",
             "Probabil-\nistic",
             "Projected\nindividual\ncomp-\nonents",
             "Based on\nstabilizat-\nion target\nscenarios",
             "Neglects\nice sheet\nfeedback\nprocesses",
             "Based on\nRCP\nscenarios",
             "Late 20th-\n21st\ncentury\nhistorical\nlinear\ntrend",
             "Accounts\nfor ice\nsheet\nfeedback\nprocesses",
             "6\nscenarios",
             "Plausible\nrange",
             "5\nscenarios",
             "20th\ncentury\nhistorical\nlinear\ntrend",
             "Neglects\nice sheet\nfeedback\nprocesses",
             "4\nscenarios",
             "3\nscenarios")

# pdf(file="../Figures/2018-f01.pdf", family="Helvetica", width=text_column_width, height=single_panel_height*2, pointsize=10)
cairo_ps(filename = "../Figures/2019-f01.eps", family="Helvetica", fallback_resolution = 300,
         width=text_column_width, height=single_panel_height*2, pointsize=10)
par(mar = c(0, 0, 0, 0))
openplotmat()

# Plot arrows
for(i in 1:nrow(fromto_straight)){
  straightarrow(to = elpos[fromto_straight[i, 2], ],
                from = elpos[fromto_straight[i, 1], ], lwd = 1, arr.type="triangle", arr.length = 0.2)
}
splitarrow(from = elpos[fromto_split[1, 1], ], to = elpos[fromto_split[1, 2:3], ], lwd = 1, arr.type="triangle", arr.length = 0.2, arr.pos = 0.3)
splitarrow(from = elpos[fromto_split[2, 1], ], to = elpos[fromto_split[2, 2:3], ], lwd = 1, arr.type="triangle", arr.length = 0.2, arr.pos = 0.45)
splitarrow(from = elpos[fromto_split[3, 1], ], to = elpos[fromto_split[3, 2:3], ], lwd = 1, arr.type="triangle", arr.length = 0.2, arr.pos = 0.15)
splitarrow(from = elpos[fromto_split[4, 1], ], to = elpos[fromto_split[4, 2:3], ], lwd = 1, arr.type="triangle", arr.length = 0.2, arr.pos = 0.775)
splitarrow(from = elpos[fromto_split[5, 1], ], to = elpos[fromto_split[5, 2:3], ], lwd = 1, arr.type="triangle", arr.length = 0.2, arr.pos = 0.35)
splitarrow(from = elpos[fromto_split[6, 1], ], to = elpos[fromto_split[6, 2:3], ], lwd = 1, arr.type="triangle", arr.length = 0.2, arr.pos = 0.575)
                      
# Add boxes to diagram plot
textellipse(elpos[49,], 0.075, 0.05, lab = lab_circ[1], box.col = Lmoss_green, shadow.col = NA)
textellipse(elpos[12,], 0.075, 0.05, lab = lab_circ[2], box.col = pale_yellow, shadow.col = NA)
textellipse(elpos[24,], 0.075, 0.05, lab = lab_circ[3], box.col = pale_yellow, shadow.col = NA)
textellipse(elpos[36,], 0.075, 0.05, lab = lab_circ[4], box.col = pale_yellow, shadow.col = NA)
textellipse(elpos[48,], 0.075, 0.05, lab = lab_circ[5], box.col = pale_yellow, shadow.col = NA)
textellipse(elpos[66,], 0.075, 0.05, lab = lab_circ[6], box.col = pale_yellow, shadow.col = NA)
textellipse(elpos[78,], 0.075, 0.05, lab = lab_circ[7], box.col = pale_yellow, shadow.col = NA)
textellipse(elpos[90,], 0.075, 0.05, lab = lab_circ[8], box.col = pale_yellow, shadow.col = NA)
textellipse(elpos[102,], 0.075, 0.05, lab = lab_circ[9], box.col = pale_yellow, shadow.col = NA)
textrect(elpos[16,], 0.065, 0.055, lab = lab_rect[1], box.col = pale_yellow, shadow.col = NA, lcol=pale_yellow)
textrect(elpos[17,], 0.065, 0.045, lab = lab_rect[2], box.col = Lmoss_green, shadow.col = NA, lcol=Lmoss_green)
textrect(elpos[26,], 0.065, 0.042, lab = lab_rect[3], box.col = pale_yellow, shadow.col = NA, lcol=pale_yellow)
textrect(elpos[27,], 0.065, 0.05, lab = lab_rect[4], box.col = Lmoss_green, shadow.col = NA, lcol=Lmoss_green)
textrect(elpos[35,], 0.065, 0.055, lab = lab_rect[5], box.col = pale_yellow, shadow.col = NA, lcol=pale_yellow)
textrect(elpos[40,], 0.065, 0.05, lab = lab_rect[6], box.col = Lmoss_green, shadow.col = NA, lcol=Lmoss_green)
textrect(elpos[47,], 0.065, 0.045, lab = lab_rect[7], box.col = pale_yellow, shadow.col = NA, lcol=pale_yellow)
textrect(elpos[63,], 0.065, 0.065, lab = lab_rect[8], box.col = pale_yellow, shadow.col = NA, lcol=pale_yellow)
textrect(elpos[64,], 0.065, 0.065, lab = lab_rect[9], box.col = pale_yellow, shadow.col = NA, lcol=pale_yellow)
textrect(elpos[65,], 0.065, 0.04, lab = lab_rect[10], box.col = pale_yellow, shadow.col = NA, lcol=pale_yellow)
textrect(elpos[74,], 0.065, 0.04, lab = lab_rect[11], box.col = Lmoss_green, shadow.col = NA, lcol=Lmoss_green)
textrect(elpos[77,], 0.065, 0.04, lab = lab_rect[12], box.col = pale_yellow, shadow.col = NA, lcol=pale_yellow)
textrect(elpos[87,], 0.065, 0.065, lab = lab_rect[13], box.col = pale_yellow, shadow.col = NA, lcol=pale_yellow)
textrect(elpos[88,], 0.065, 0.05, lab = lab_rect[14], box.col = Lmoss_green, shadow.col = NA, lcol=Lmoss_green)
textrect(elpos[89,], 0.065, 0.04, lab = lab_rect[15], box.col = pale_yellow, shadow.col = NA, lcol=pale_yellow)
textrect(elpos[101,], 0.065, 0.04, lab = lab_rect[16], box.col = pale_yellow, shadow.col = NA, lcol=pale_yellow)

dev.off()
#   -----------------------------------------------------------------------

##=========================== SLR PDF PLOTS ===================================
#---------------------------- 2030 -----------------------------------
# pdf(file="../Figures/2018-f02.pdf", family="Helvetica", width=text_column_width, height=single_panel_height*2, pointsize=10)
cairo_ps(filename = "../Figures/2019-f02.eps", family="Helvetica", fallback_resolution = 300,
         width=text_column_width, height=single_panel_height*2, pointsize=10)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4,
                5,6,7,
                5,6,7,
                8,9,10,
                8,9,10,
                11,12,13,
                11,12,13), 9, 3, byrow = TRUE))

# Add legend
par(oma=c(1.5,1.5,0,0), mgp=c(1.5,.5,0), mar=c(0,2,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")

legend("topleft", legend=c("Wong & Keller [2017] FD", "Wong & Keller [2017] no FD", "Kopp et al. [2014]", "Kopp et al. [2017]", "Sweet et al. [2017]",
                           "Rasmussen et al. [2018]", "Parris et al. [2012]", "USACE [2014]", "Hall et al. [2016]"),
       lty=c(1,1,1,1,NA,1,NA,NA,NA), lwd=c(2,2,2,2,NA,2,NA,NA,NA), pch=c(NA,NA,NA,NA,15,NA,19,19,19),
       pt.cex=c(NA,NA,NA,NA,2,NA,1,1,1), bty='n', ncol=3, cex=1.2,
       col=c(brickfd_col[1], NO_fd_col[1], kopp14_col[1], kopp17_DP16_col[1], sweet17_col[1],
             Ras18_col[1], parris12_col[2], usace14_col[1], hall16_col[1]))

gradient.rect(7.35,1.5,9.1,3, col=col_grad, gradient="x")
arrows(9.2, 0.3, 9.45, 0.3, length=0.075)
text(8.1,0.3, "Higher scenario", cex=1.25)

#   -----------------------------------------------------------------------
# a) Sea-level rise probability density function low scenarios
par(mgp=c(1.5,.5,0), mar=c(3.5,1.5,1,0.5))
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
par(mgp=c(1.5,0.5,0), mar=c(3.5,1.5,1,1))
plot_SLRcompare_PDF(year = 2030, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), panel = "c.")

lines(parris_etal_2012$t_2030[3:4], rep(6.5, 2), col=parris12_col[2:1], lwd=1.5, lty=3)
lines(hall_etal_2016$t_2030[4:5], rep(5.5, 2), col=hall16_col[2:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2030[3:4], rep(6.5, 2), col=parris12_col[2:1], pch=19)
points(usace2014$t_2030[3], 6, col=usace14_col[1], pch=19)
points(hall_etal_2016$t_2030[4:5], rep(5.5, 2), col=hall16_col[2:1], pch=19)

#---------------------------- 2050 -----------------------------------
# d) Sea-level rise probability density function low scenarios
par(mgp=c(1.5,.5,0), mar=c(3.5,1.5,1,0.5))
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
par(mgp=c(1.5,0.5,0), mar=c(3.5,1.5,1,1))
plot_SLRcompare_PDF(year = 2050, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), panel = "f.")

lines(parris_etal_2012$t_2050[3:4], rep(3.25, 2), col=parris12_col[2:1], lwd=1.5, lty=3)
lines(hall_etal_2016$t_2050[4:5], rep(2.75, 2), col=hall16_col[2:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2050[3:4], rep(3.25, 2), col=parris12_col[2:1], pch=19)
points(usace2014$t_2050[3], 3, col=usace14_col[1], pch=19)
points(hall_etal_2016$t_2050[4:5], rep(2.75, 2), col=hall16_col[2:1], pch=19)

#---------------------------- 2070 -----------------------------------
# g) Sea-level rise probability density function low scenarios
par(mgp=c(1.5,.5,0), mar=c(3.5,1.5,1,0.5))
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
par(mgp=c(1.5,0.5,0), mar=c(3.5,1.5,1,1))
plot_SLRcompare_PDF(year = 2070, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), panel = "i.")

lines(parris_etal_2012$t_2070[3:4], rep(2.75, 2), col=parris12_col[2:1], lwd=1.5, lty=3)
lines(hall_etal_2016$t_2070[4:5], rep(2.25, 2), col=hall16_col[2:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2070[3:4], rep(2.75, 2), col=parris12_col[2:1], pch=19)
points(usace2014$t_2070[3], 2.5, col=usace14_col[1], pch=19)
points(hall_etal_2016$t_2070[4:5], rep(2.25, 2), col=hall16_col[2:1], pch=19)

#---------------------------- 2100 -----------------------------------
# j) Sea-level rise probability density function low scenarios
par(mgp=c(1.5,.5,0), mar=c(3.5,1.5,1,0.5))
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
par(mgp=c(1.5,0.5,0), mar=c(3.5,1.5,1,1))
plot_SLRcompare_PDF(year = 2100, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), panel = "l.")

lines(parris_etal_2012$t_2100[3:4], rep(1.45, 2), col=parris12_col[2:1], lwd=1.5, lty=3)
lines(hall_etal_2016$t_2100[4:5], rep(1.15, 2), col=hall16_col[2:1], lwd=1.5, lty=3)

points(parris_etal_2012$t_2100[3:4], rep(1.45, 2), col=parris12_col[2:1], pch=19)
points(usace2014$t_2100[3], 1.3, col=usace14_col[1], pch=19)
points(hall_etal_2016$t_2100[4:5], rep(1.15, 2), col=hall16_col[2:1], pch=19)

#   -----------------------------------------------------------------------
mtext(text="Probability density",side=2,line=0,outer=TRUE)
mtext(text="Projected sea level (ft)",side=1,line=0,outer=TRUE)

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

GSIC_rcp26_5 = percentile_projection_row(ice_proj, convert_m_to_ft(GSIC_rcp26), 0.05)
GIS_rcp26_5 = percentile_projection_row(ice_proj, convert_m_to_ft(GIS_rcp26), 0.05)
GSIC_rcp45_5 = percentile_projection_row(ice_proj, convert_m_to_ft(GSIC_rcp45), 0.05)
GIS_rcp45_5 = percentile_projection_row(ice_proj, convert_m_to_ft(GIS_rcp45), 0.05)
GSIC_rcp85_5 = percentile_projection_row(ice_proj, convert_m_to_ft(GSIC_rcp85), 0.05)
GIS_rcp85_5 = percentile_projection_row(ice_proj, convert_m_to_ft(GIS_rcp85), 0.05)
GSIC_rcp26_95 = percentile_projection_row(ice_proj, convert_m_to_ft(GSIC_rcp26), 0.95)
GIS_rcp26_95 = percentile_projection_row(ice_proj, convert_m_to_ft(GIS_rcp26), 0.95)
GSIC_rcp45_95 = percentile_projection_row(ice_proj, convert_m_to_ft(GSIC_rcp45), 0.95)
GIS_rcp45_95 = percentile_projection_row(ice_proj, convert_m_to_ft(GIS_rcp45), 0.95)
GSIC_rcp85_95 = percentile_projection_row(ice_proj, convert_m_to_ft(GSIC_rcp85), 0.95)
GIS_rcp85_95 = percentile_projection_row(ice_proj, convert_m_to_ft(GIS_rcp85), 0.95)

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
# pdf(file="../Figures/2018-f03.pdf", family="Helvetica", width=text_column_width, height=single_panel_height*1.5, pointsize=10)
cairo_ps(filename = "../Figures/2019-f03.eps", family="Helvetica", fallback_resolution = 300,
         width=text_column_width, height=single_panel_height*1.5, pointsize=10)
par(oma=c(0,0,0,0), mfrow=c(2, 1), mgp=c(1.5,.5,0), mar=c(3,3,0.5,1.5))

# a) Sea-level rise projections
plot(0, type="n",xlab="Year", ylab="Projected sea level (ft)", ylim=c(0,13), xlim=c(2010, 2098), xaxt="n")
fig_label("a.", region="figure", pos="topleft", cex=1.5)
axis(1, lwd = 1, at=seq(2010,2100, 10), label=seq(2010,2100, 10))

# Only plot RCP 8.5; those with Fast dynamic plot solid with border
polygon(y = c(sweet17_25_5, rev(sweet17_25_95)), x = c(seq(2000, 2200, 10), rev(seq(2000, 2200, 10))), col = sweet17_col[1], border = "black", lty=2, lwd=2)
polygon(y = c(sweet17_20_5, rev(sweet17_20_95)), x = c(seq(2000, 2200, 10), rev(seq(2000, 2200, 10))), col = trans_sweet17_col[5], border = NA)
polygon(y = c(k17_DP16_SEW_85_5, rev(k17_DP16_SEW_85_95)), x = c(k17_DP16_SEW_years, rev(k17_DP16_SEW_years)), col = kopp17_DP16_col[1], border = "black", lty=2, lwd=2)

polygon(y = c(k14_85_5, rev(k14_85_95)), x = c(k14_years, rev(k14_years)), col = trans_kopp14_col[3], border = NA)
polygon(y = c(lsl_fdyn_85_5, rev(lsl_fdyn_85_95)), x = c(t.time, rev(t.time)), col = brickfd_col[1], border = "black", lty=2, lwd=2)
polygon(y = c(NO_fdyn_85_5, rev(NO_fdyn_85_95)), x = c(t.time, rev(t.time)), col = trans_NO_fd_col[3], border = NA)

arrows(2102.5, sweet17_20_95[11]+0.3, 2102.5, sweet17_25_95[11]+0.5, xpd = TRUE, lwd=1.5, length = 0.05, col = sweet17_col[1])
arrows(2102.5, k14_85_95[which(k14_years==2100)], 2102.5, k17_DP16_SEW_85_95[which(k14_years==2100)]+0.25, xpd = TRUE, lwd=1.5, 
       length = 0.05, col = kopp17_DP16_col[1])
arrows(2103, NO_fdyn_85_95[which(t.time == 2100)], 2103, lsl_fdyn_85_95[which(t.time == 2100)], xpd = TRUE, lwd=1.5, length = 0.05, col = brickfd_col[1])

legend("topleft", legend = c("Incorporates ice sheet feedback proccesses", 
                             "Wong & Keller [2017] no FD RCP85 90% CI", "Wong & Keller [2017] FD RCP85 90% CI", 
                             #"Rasmussen et al. [2018] 2.5 90% CI", 
                             "Kopp et al. [2017] RCP85 90% CI", 
                             "Kopp et al. [2014] RCP85 90% CI", "Sweet et al. [2017] 2.5 90% CI", 
                             "Sweet et al. [2017] 2.0 90% CI"), pch = c(NA, rep(22, 7)), 
       bty='n', pt.bg = c(NA, trans_NO_fd_col[3], brickfd_col[1], 
                          #trans_Ras18_col[3], 
                          kopp17_DP16_col[1], trans_kopp14_col[3], 
                          sweet17_col[1], trans_sweet17_col[5]), pt.cex = 2, lty=c(2, rep(0, 6)), lwd=c(1.5, rep(1, 6)))
#   -----------------------------------------------------------------------
# b) Projection timescale version project design life
sweet_years = seq(2000, 2200, 10)
project_years = data.frame(kopp14 = c(min(k14_years), max(k14_years)), kopp17 = c(min(k17_DP16_SEW_years), max(k17_DP16_SEW_years)), 
                           wong17 = c(min(t.time), max(t.time)), rasumussen18 = c(min(Ras18_SEW_years), max(Ras18_SEW_years)), 
                           usace = c(min(SL_calculator_ref2000[,"Year"]), max(SL_calculator_ref2000[,"Year"])), 
                           parris12 = c(min(SL_calculator_ref2000[,"Year"]), max(SL_calculator_ref2000[,"Year"])), 
                           hall16 = c(min(SL_calculator_ref2000[,"Year"]), max(SL_calculator_ref2000[,"Year"])), 
                           sweet17 = c(min(sweet_years), max(sweet_years)))

par(mgp=c(1.5,.5,0), mar=c(3,10,0.5,0.5))
plot(c(min(project_years), max(project_years)), c(1,ncol(project_years)+2), type = "n", yaxt="n", 
     xlab="Sea-level rise projection year", ylab="")
fig_label("b.", region="figure", pos="topleft", cex=1.5)

rect(2020, 0, 2400, 20, col="gray99", border=NA)
rect(2020, 0, 2120, 20, col="gray85", border=NA)
text(2070, 9.75, "Design life of 100 yrs", cex=0.8)
text(2210, 9.75, "Continued operation & maintenance", cex=0.8)
arrows(2297, 9.75, 2307, 9.75, length=0.075)

rect(project_years$kopp14[1], 0.75, project_years$kopp14[2], 1.25, col=kopp14_col[2], border=NA)
rect(project_years$kopp17[1], 1.75, project_years$kopp17[2], 2.25, col=kopp17_DP16_col[2], border=NA)
rect(project_years$wong17[1], 2.75, project_years$wong17[2], 3.25, col=NO_fd_col[2], border=NA)
rect(project_years$wong17[1], 3.75, project_years$wong17[2], 4.25, col=brickfd_col[2], border=NA)
rect(project_years$rasumussen18[1], 4.75, project_years$rasumussen18[2], 5.25, col=Ras18_col[2], border=NA)
rect(project_years$usace[1], 5.75, project_years$usace[2], 6.25, col=usace14_col[2], border=NA)
rect(project_years$parris12[1], 6.75, project_years$parris12[2], 7.25, col=parris12_col[2], border=NA)
rect(project_years$hall16[1], 7.75, project_years$hall16[2], 8.25, col=hall16_col[2], border=NA)
rect(project_years$sweet17[1], 8.75, project_years$sweet17[2], 9.25, col=sweet17_col[2], border=NA)

axis(2, lwd = 1, at=1:9, label=c("Kopp et al. [2014]", "Kopp et al. [2017]", "Wong & Keller\n[2017] no FD", 
                                 "Wong & Keller [2017] FD", "Rasmussen et al.\n[2018]", "USACE [2014]", 
                                 "Parris et al. [2012]", "Hall et al. [2016]", "Sweet et al. [2017]"), las=1)
box()
#   -----------------------------------------------------------------------
dev.off()

##=========================== STORM SURGE PLOT ===================================
storm_df = data.frame(studies = c("Tebaldi et al.\n[2012]", "Wong [2018]", "Zervas [2013]", "Wong [2018]\nNAO", "This study MLE",  
                                  "Wong [2018]\nTime", "Wong [2018]\nTemp.", "Wong [2018]\nBMA", "Wong [2018]\nSea level"),
                      values = c(tebaldi12$rl_50[which(tebaldi12$rp==100)], wong_18$rl[1], zervas_2013$NOAA_rl_feet[which(zervas_2013$NOAA_rp==100)], 
                                 wong_18$rl[2], gev_stat_50[which.min(abs(1/probs[order(1/probs)] - 100))], wong_18$rl[-1:-2]))

# pdf(file="../Figures/2018-f04.pdf", family="Helvetica", width=text_column_width, height=single_panel_height*1.5, pointsize=10)
cairo_ps(filename = "../Figures/2019-f04.eps", family="Helvetica", fallback_resolution = 300,
         width=text_column_width, height=single_panel_height*1.5, pointsize=10)
# c) Storm surge return period 
par(mfrow=c(2, 1), mgp=c(1.5,.5,0), mar=c(3,3,0.5,0.5))
plot(1/NOAA_methodGEV$aep, NOAA_methodGEV$return_level, log = "x", type = "n", xlim = c(1, 500),
     ylim = c(2.85, 17), 
     xaxt = 'n', cex=1, #bty="l",
     xlab = "Return period (years)", 
     ylab = "Storm surge (ft MSL)")
axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))
fig_label("a.", region="figure", pos="topleft", cex=1.5)

polygon(y = c(gev_stat_025, rev(gev_stat_975)), 
        x = c(1/probs[order(1/probs)], 1/probs), col = trans_srikrishnan_col[5], border = NA)

polygon(y = c(wong18_stat_025, rev(wong18_stat_975)), 
        x = c(wong18_rl_years, rev(wong18_rl_years)), col = trans_wong18_col[3], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi12_col[2], border = NA)

polygon(y = c(zervas_2013$min_95[1:4], rev(zervas_2013$max_95[1:4])), 
        x = c(1/zervas_2013$aep[1:4], rev(1/zervas_2013$aep[1:4])), col = trans_zervas13_col[2], border = NA)

lines(1/probs[order(1/probs)], gev_stat_50, col=srikrishnan_col[2], lwd=2)
lines(wong18_rl_years, wong18_stat_50, col=wong_18$color[3], lwd=2)
lines(zervas_2013$NOAA_rp, zervas_2013$NOAA_rl_feet, lwd=2, col=zervas13_col[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi12_col[2])
points(rp_ABM, blockMax, pch = 19, col=obs_col)
points(USACE_rp, USACE_EWL$feet[8:14], pch = 19, col=usace14_col[2])

points(c(rp_h, rp_l), rep(NLIH_1821, 2), pch="|", col=burntorange[1])
lines(c(rp_h, rp_l), rep(NLIH_1821, 2), lty=2, col=burntorange[1])
points(rp_m, NLIH_1821, pch=19, col=burntorange[1], cex=1)

points(rp_1749, Storm_of_1749, pch=19, col="darkred", cex=1)

legend("topleft", legend=c("This study 95% CI", "This study MLE",
                           "Tebaldi et al. [2012] 95% CI", "Tebaldi et al. [2012] MLE",
                           "Zervas [2013] 95% CI", "Zervas [2013] MLE", 
                           "Wong [2018] 95% CI", "Wong [2018] MLE",
                           "1821 Norfolk-Long Island Hurricane", "The Storm of 1749", "USACE [2014]", "Observations"), cex=0.95,
       lty=c(NA,1,NA,1,NA,1,NA,1,NA,NA,NA,NA), lwd=c(NA,2,NA,2,NA,2,NA,2,NA,NA,NA,NA), pch=c(22,NA,22,NA,22,NA,22,NA,19,19,19,19), 
       col=c("black", srikrishnan_col[2], "black", tebaldi12_col[2], "black", zervas13_col[2], "black", wong_18$color[3], burntorange[1], "darkred", usace14_col[2], obs_col),
       bty='n', pt.bg=c(trans_srikrishnan_col[5],NA,trans_tebaldi12_col[2],NA,trans_zervas13_col[2],NA,trans_wong18_col[3],NA,NA,NA,NA, NA), pt.cex = c(2,NA,2,NA,2,NA,2,NA,1, 1,1,1))

# timelineS plot
# par(oma=c(0,0,0,0), mgp=c(1.5,.5,0), mar=c(3,0.5,0.5,0.5))
par(mgp=c(0.5,.5,0), mar=c(2,1,0.5,0.5))
plot(NA, xlim = c(-1, 1), ylim = c(6.5,8), ann = FALSE, axes = FALSE)
fig_label("b.", region="figure", pos="topleft", cex=1.5)

d = 1
v = -0.75

points <- rep_len(d * c(0.4, 0.7), length.out = nrow(storm_df))
segments(y0 = storm_df[[2]], x0 = v, y1 = storm_df[[2]], x1 = points + v, col = "gray44")

axis(2, at = seq(from = 6, to = 8, by = 0.5), cex.axis = 1, pos = v, lwd.tick = 2, col = "gray44", font = 2, las = 1)
abline(v = v, lwd = 5, col = "gray44")

points(y = storm_df[[2]], x = points + v, pch = 21, cex = 1.75, lwd=2,
       bg = c("white","white","white", "darkred", "white", "darkred", "darkred", "darkred", "darkred"))

text(x = points +v+0.1, y = storm_df[[2]], labels = rev(c("a", "b", "c", "d", "e", "f", "g", "h", "i")), font=2)

text(x = v-0.15, y = 7.25, labels = "100-yr median storm surge in 2065\n(ft MSL)", srt=90)
legend("topright", legend = c("Stationary", "Non-stationary"), pch = 21, pt.bg = c(NA, "darkred"), pt.cex=1.75, bty="n")
legend("bottomright", legend = rev(str_replace_all(storm_df[[1]], "([\n])", " ")), pch = c("a", "b", "c", "d", "e", "f", "g", "h", "i"), bty="n")

dev.off()
##=========================== 100-YR COMBINED STORM SURGE AND SLR ===================================
wongstormSLR_r26 = data.frame(studies = c("Wong [2018]\nBMA + FD SLR", "Wong [2018]\nBMA + SLR", 
                                          "Wong [2018] + FD SLR", "Wong [2018]\n + SLR"), values = c(bma_fd_r26_SS_2065_100, bma_NOfd_r26_SS_2065_100, 
                                                                                                     stat_fd_r26_SS_2065_100, stat_NOfd_r26_SS_2065_100))

wongstormSLR_r45 = data.frame(studies = c("Wong [2018]\nBMA + FD SLR", "Wong [2018]\nBMA + SLR", 
                                          "Wong [2018] + FD SLR", "Wong [2018]\n + SLR"), values = c(bma_fd_r45_SS_2065_100, bma_NOfd_r45_SS_2065_100,  
                                                                                                     stat_fd_r45_SS_2065_100, stat_NOfd_r45_SS_2065_100))

wongstormSLR_r85 = data.frame(studies = c("Wong [2018]\nBMA + FD SLR", "Wong [2018]\nBMA + SLR", 
                                          "Wong [2018] + FD SLR", "Wong [2018]\n + SLR"), values = c(bma_fd_r85_SS_2065_100, bma_NOfd_r85_SS_2065_100,
                                                                                                     stat_fd_r85_SS_2065_100, stat_NOfd_r85_SS_2065_100))

# pdf(file="../Figures/2018-f05.pdf", family="Helvetica", width=text_column_width, height=(single_panel_height*1.5)/2, pointsize=10)
cairo_ps(filename = "../Figures/2019-f05.eps", family="Helvetica", fallback_resolution = 300,
         width=text_column_width, height=(single_panel_height*1.5)/2, pointsize=10)
par(mfrow=c(1,1), mgp=c(1.5,0.75,0), mar=c(0.5,0,0.5,0.5))
plot(NA, xlim = c(-0.9, 0.7), ylim = c(8.5,10), ann = FALSE, axes = FALSE)

d = 0
v = -0.7

points <- rep_len(d + c(0.4, 0.7), length.out = nrow(wongstormSLR_r26))
segments(y0 = wongstormSLR_r26[[2]], x0 = d+v, y1 = wongstormSLR_r26[[2]], x1 = points + v, col = "gray44")

axis(2, at = c(8.5, 9, 9.5, 10), cex.axis = 1, pos = d+v, lwd.tick = 2, col = "gray44", font = 2, las = 1,
     labels = c("8.5", "9", "9.5", "10"))
abline(v = d+v, lwd = 5, col = "gray44")

points(y = wongstormSLR_r26[[2]], x = points + v, pch = 21, cex = 1.75, lwd=2,
       bg = c("darkred", "#2c7bb6", "white", "white"),
       col = c("darkred", "#2c7bb6", "darkred", "#2c7bb6"))
text(x = d+v-0.15, y = 9.25, labels = "100-yr storm surge + SLR in 2065\n(ft MSL)", srt=90, font=1.5)

#-----
segments(y0 = wongstormSLR_r45[[2]], x0 = d+v, y1 = wongstormSLR_r45[[2]], x1 = points + v, col = "gray44")
points(y = wongstormSLR_r45[[2]], x = points + v, pch = 22, cex = 1.75, lwd=2,
       bg = c("darkred", "#2c7bb6", "white", "white"),
       col = c("darkred", "#2c7bb6", "darkred", "#2c7bb6"))

#-----
segments(y0 = wongstormSLR_r85[[2]], x0 = d+v, y1 = wongstormSLR_r85[[2]], x1 = points + v, col = "gray44")
points(y = wongstormSLR_r85[[2]], x = points + v, pch = 24, cex = 1.75, lwd=2,
       bg = c("darkred", "#2c7bb6", "white", "white"),
       col = c("darkred", "#2c7bb6", "darkred", "#2c7bb6"))

text(x = -0.3, y = c(wongstormSLR_r85[[2]][1]+0.1, wongstormSLR_r85[[2]][3]+0.1),
     labels = c("Wong [2018] BMA + SLR", "Wong [2018] + SLR"), font=1)

legend("topright", legend = c("Non-stationary", "Stationary", NA, "Fast dynamics", "No fast dynamics", NA, "RCP8.5", "RCP4.5", "RCP2.6"), 
       pch = c(21, 21, NA, 21, 21, NA, 24, 22, 21), pt.bg = c("black", "white", NA, "darkred", "#2c7bb6", NA, "white", "white", "white"), 
       col=c("black", "black", NA, "darkred", "#2c7bb6", NA, "black", "black", "black"), pt.cex=1.75, pt.lwd=1.75)
dev.off()

##=========================== ICE MASS VOLUME PLOT ===================================
# Median and 90% credible intervals for initial ice mass volume from the BRICK model 
# using the gamma priors for the fast dynamics. 
# See: https://static-content.springer.com/esm/art%3A10.1007%2Fs10584-017-2039-4/MediaObjects/10584_2017_2039_MOESM2_ESM.pdf
median.gsic = convert_m_to_ft(0.3958)
lower.5.gsic = convert_m_to_ft(0.3097)
upper.95.gsic = convert_m_to_ft(0.4908)

median.gis = convert_m_to_ft(7.352)
lower.5.gis = convert_m_to_ft(7.177)
upper.95.gis = convert_m_to_ft(7.539)

# pdf(file="../Figures/2019-sf01.pdf", family="Helvetica", width=text_column_width, height=single_panel_height*1.5, pointsize=10)
cairo_ps(filename = "../Figures/2019-sf01.eps", family="Helvetica", fallback_resolution = 1200,
         width=text_column_width, height=single_panel_height*1.5, pointsize=10)
par(oma=c(0,0,0,0), mfrow=c(2, 1), mgp=c(1.5,.5,0), mar=c(3,3,0.5,1.5))
plot(0, type="n",xlab="Year", ylab="Projected GSIC melt (ft SLE)", ylim=c(0,1.7), xlim=c(2010, 2193), xaxt="n")
axis(1, lwd = 1, at=seq(2010,2200, 10), label=seq(2010,2200, 10))
polygon(y = c(GSIC_rcp85_5, rev(GSIC_rcp85_95)), x = c(ice_proj, rev(ice_proj)), col = trans_Ras18_col[1], border = NA)
polygon(y = c(GSIC_rcp45_5, rev(GSIC_rcp45_95)), x = c(ice_proj, rev(ice_proj)), col = trans_brickfd_col[1], border = NA)
polygon(y = c(GSIC_rcp26_5, rev(GSIC_rcp26_95)), x = c(ice_proj, rev(ice_proj)), col = trans_kopp17_DP16_col[1], border = NA)
fig_label("a.", region="figure", pos="topleft", cex=1.5)
abline(h=c(lower.5.gsic, upper.95.gsic), lty=3)

plot(0, type="n",xlab="Year", ylab="Projected GIS melt (ft SLE)", ylim=c(0,25), xlim=c(2010, 2193), xaxt="n")
axis(1, lwd = 1, at=seq(2010,2200, 10), label=seq(2010,2200, 10))
polygon(y = c(GIS_rcp26_5, rev(GIS_rcp26_95)), x = c(ice_proj, rev(ice_proj)), col = trans_kopp17_DP16_col[1], border = NA)
polygon(y = c(GIS_rcp45_5, rev(GIS_rcp45_95)), x = c(ice_proj, rev(ice_proj)), col = trans_brickfd_col[1], border = NA)
polygon(y = c(GIS_rcp85_5, rev(GIS_rcp85_95)), x = c(ice_proj, rev(ice_proj)), col = trans_Ras18_col[1], border = NA)
fig_label("b.", region="figure", pos="topleft", cex=1.5)
abline(h=c(lower.5.gis, upper.95.gis), lty=3)

legend("left", legend = c("RCP85 90% CI", "RCP45 90% CI", 
                             "RCP26 90% CI", "Initial ice mass\nvolume 90% CI"), pch = c(rep(22, 3), NA), 
       bty='n', pt.bg = c(trans_Ras18_col[1], trans_brickfd_col[1], trans_kopp17_DP16_col[1], NA), lty = c(NA,NA,NA,3),
       pt.cex = 2)

dev.off()

##=========================== STORM SURGE OBSERVATION PLOT ===================================
# pdf(file="../Figures/2018-sf02.pdf", family="Helvetica", width=text_column_width, height=single_panel_height*1.5, pointsize=10)
cairo_ps(filename = "../Figures/2019-sf02.eps", family="Helvetica", fallback_resolution = 300,
         width=text_column_width, height=single_panel_height*1.5, pointsize=10)
par(mfrow=c(2, 1), mgp=c(1.5,.5,0), mar=c(3,3,0.5,0.5))
plot(ABM_with_years$index_yr, convert_m_to_ft(ABM_with_years$abm), pch=20, type = "b", col = "black",#, xaxs = 'i',
     ylab = "Annual block maxima (ft MSL)", xlab = "Year", lwd=1.5)
fig_label("a.", region="figure", pos="topleft", cex=1.5)

plot(rp_ABM, blockMax, log = "x", pch = 19, col=obs_col, xaxt = 'n', cex=1, xlab = "Return period (years)", 
     ylab = "Storm surge (ft MSL)")
axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))
fig_label("b.", region="figure", pos="topleft", cex=1.5)

dev.off()
##==============================================================================
## End
##==============================================================================
