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
library(diagram)

# Source survival function, function.
source("Helper_scripts/plot_sf.r")
source("Helper_scripts/plot_SLRcompare_PDF.R")
source("Helper_scripts/plot_SLRandStormSurge_SF_scen.R")

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
trans_zervas13_col = makeTransparent(zervas13_col, 150)

##=========================== PUBLICATION FIGURE SIZES ===================================
inches_to_dpi = function(inch){ inch * 300 }

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
             "Kopp et\nal.\n(2017)",
             "Wong\nand\nKeller\n(2017)",
             "Rasmu-\nssen et\nal.\n(2018)",
             "Kopp et\nal.\n(2014)",
             "Sweet\net al.\n(2017)",
             "Hall et\nal.\n(2016)",
             "Parris\net al.\n(2012)",
             "USACE\n(2011,\n2013,\n2014)")
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

pdf(file="../Figures/Fig1.pdf", family="Times", width=text_column_width, height=single_panel_height*2, pointsize=10)
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
pdf(file="../Figures/Fig2.pdf", family="Times", width=text_column_width, height=single_panel_height*2, pointsize=12)
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

legend("topleft", legend=c("Wong & Keller 2017 FD", "Wong & Keller 2017 no FD", "Kopp et al. 2014", "Kopp et al. 2017", "Sweet et al. 2017",
                           "Rasmussen et al. 2018", "Parris et al. 2012", "USACE 2014", "Hall et al. 2016"),
       lty=c(1,1,1,1,NA,1,NA,NA,NA), lwd=c(2,2,2,2,NA,2,NA,NA,NA), pch=c(NA,NA,NA,NA,15,NA,19,19,19),
       pt.cex=c(NA,NA,NA,NA,2,NA,1,1,1), bty='n', ncol=3, 
       col=c(brickfd_col[1], NO_fd_col[1], kopp14_col[1], kopp17_DP16_col[1], sweet17_col[1],
             Ras18_col[1], parris12_col[2], usace14_col[1], hall16_col[1]))

gradient.rect(7.35,1.5,9.1,3, col=col_grad, gradient="x")
arrows(8.95, 0.3, 9.2, 0.3, length=0.075)
text(8.1,0.3, "Higher scenario")

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
pdf(file="../Figures/Fig3.pdf", family="Times", width=text_column_width, height=single_panel_height*2, pointsize=12)
par(oma=c(0,0,0,0), mfrow=c(3, 1), mgp=c(1.5,.5,0), mar=c(3,3,0.5,1.5))

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

arrows(2103, sweet17_20_95[11]+0.3, 2103, sweet17_25_95[11]+0.5, xpd = TRUE, lwd=1.5, length = 0.05, col = sweet17_col[1])
arrows(2103, k14_85_95[which(k14_years==2100)], 2103, k17_DP16_SEW_85_95[which(k14_years==2100)]+0.25, xpd = TRUE, lwd=1.5, 
       length = 0.05, col = kopp17_DP16_col[1])
arrows(2104.5, NO_fdyn_85_95[which(t.time == 2100)], 2104.5, lsl_fdyn_85_95[which(t.time == 2100)], xpd = TRUE, lwd=1.5, length = 0.05, col = brickfd_col[1])

legend("topleft", legend = c("Incorporates ice sheet feedback proccesses", 
                             "Wong & Keller 2017 no FD RCP85 90% CI", "Wong & Keller 2017 FD RCP85 90% CI", 
                             #"Rasmussen et al. 2018 2.5 90% CI", 
                             "Kopp et al. 2017 RCP85 90% CI", 
                             "Kopp et al. 2014 RCP85 90% CI", "Sweet et al. 2017 2.5 90% CI", 
                             "Sweet et al. 2017 2.0 90% CI"), pch = c(NA, rep(22, 7)), 
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
text(2070, 9.75, "Design life of 100 yrs")
text(2200, 9.75, "Continued operation and maintenance")
arrows(2275, 9.75, 2285, 9.75, length=0.075)

rect(project_years$kopp14[1], 0.75, project_years$kopp14[2], 1.25, col=kopp14_col[2], border=NA)
rect(project_years$kopp17[1], 1.75, project_years$kopp17[2], 2.25, col=kopp17_DP16_col[2], border=NA)
rect(project_years$wong17[1], 2.75, project_years$wong17[2], 3.25, col=NO_fd_col[2], border=NA)
rect(project_years$wong17[1], 3.75, project_years$wong17[2], 4.25, col=brickfd_col[2], border=NA)
rect(project_years$rasumussen18[1], 4.75, project_years$rasumussen18[2], 5.25, col=Ras18_col[2], border=NA)
rect(project_years$usace[1], 5.75, project_years$usace[2], 6.25, col=usace14_col[2], border=NA)
rect(project_years$parris12[1], 6.75, project_years$parris12[2], 7.25, col=parris12_col[2], border=NA)
rect(project_years$hall16[1], 7.75, project_years$hall16[2], 8.25, col=hall16_col[2], border=NA)
rect(project_years$sweet17[1], 8.75, project_years$sweet17[2], 9.25, col=sweet17_col[2], border=NA)

axis(2, lwd = 1, at=1:9, label=c("Kopp et al. 2014", "Kopp et al. 2017", "Wong & Keller\n2017 no FD", 
                                 "Wong & Keller 2017 FD", "Rasmussen et al.\n2018", "USACE 2014", 
                                 "Parris et al. 2012", "Hall et al. 2016", "Sweet et al. 2017"), las=1)

box()
#   -----------------------------------------------------------------------
# c) Storm surge return period 
# pdf(file="../Figures/Figtest.pdf", family="Times", width=text_column_width, height=single_panel_height, pointsize=12)
par(mgp=c(1.5,.5,0), mar=c(3,3,0.5,0.5))
plot(1/NOAA_methodGEV$aep, NOAA_methodGEV$return_level, log = "x", type = "n", xlim = c(1, 500),
     ylim = c(2.85, 17), 
     xaxt = 'n', cex=1, #bty="l",
     xlab = "Return period (years)", 
     ylab = "Storm surge (ft MSL)")
axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))
fig_label("c.", region="figure", pos="topleft", cex=1.5)

# SF_Srikrishnan_stationary025 = plot.sf(stat_gev025, make.plot=FALSE)
# SF_Srikrishnan_stationary975 = plot.sf(stat_gev975, make.plot=FALSE)
# polygon(y = c(SF_Srikrishnan_stationary025$sf.num, rev(SF_Srikrishnan_stationary975$sf.num)), 
#         x = c(1/SF_Srikrishnan_stationary025$sf, rev(1/SF_Srikrishnan_stationary975$sf)), col = trans_srikrishnan_col[5], border = NA)
polygon(y = c(gev_stat_025, rev(gev_stat_975)), 
        x = c(1/probs[order(1/probs)], 1/probs), col = trans_srikrishnan_col[5], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi12_col[2], border = NA)

polygon(y = c(zervas_2013$min_95[1:4], rev(zervas_2013$max_95[1:4])), 
        x = c(1/zervas_2013$aep[1:4], rev(1/zervas_2013$aep[1:4])), col = trans_zervas13_col[2], border = NA)
# points(NOAA_methodGEV$return_obs, NOAA_methodGEV$obs, pch = 19, col=obs_col)

# SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
# lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=srikrishnan_col[2], lwd=2)

# SF_Srikrishnan_stationary50 = plot.sf(stat_gev50, make.plot=FALSE)
# lines(1/SF_Srikrishnan_stationary50$sf, SF_Srikrishnan_stationary50$sf.num, col="blue", lwd=2)

lines(1/probs[order(1/probs)], gev_stat_50, col=srikrishnan_col[2], lwd=2)
lines(zervas_2013$NOAA_rp, zervas_2013$NOAA_rl_feet, lwd=2, col=zervas13_col[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi12_col[2])
points(rp_ABM, blockMax, pch = 19, col=obs_col)
points(USACE_rp, USACE_EWL$feet[8:14], pch = 19, col=usace14_col[2])

# returnperiod_l = 2017 - 1825
# returnperiod_h = 2017 - 1806
# 
# points(c(returnperiod_h, returnperiod_l), rep(10, 2), pch="|", col=burntorange[1])
# lines(c(returnperiod_h, returnperiod_l), rep(10, 2), lty=2, col=burntorange[1])
# points(mean(c(returnperiod_h, returnperiod_l)), 10, pch=19, col=burntorange[1])

points(c(rp_h, rp_l), rep(NLIH_1821, 2), pch="|", col=burntorange[1])
lines(c(rp_h, rp_l), rep(NLIH_1821, 2), lty=2, col=burntorange[1])
points(rp_m, NLIH_1821, pch=19, col=burntorange[1])

legend("topleft", legend=c("Our model 95% CI", "Our model MLE",
                           "Tebaldi et al. 2012 95% CI", "Tebaldi et al. 2012 MLE",
                           "Zervas 2013 95% CI", "Zervas 2013 MLE", 
                           "USACE 2014", "Observations", "1821 Norfolk-Long Island Hurricane"),
       lty=c(NA,1,NA,1,NA,1,NA,NA,NA), lwd=c(NA,2,NA,2,NA,2,NA,NA,NA), pch=c(22,NA,22,NA,22,NA,19,19,19), 
       col=c("black", srikrishnan_col[2], "black", tebaldi12_col[2], "black", zervas13_col[2], usace14_col[2], obs_col, burntorange[1]),
       bty='n', pt.bg=c(trans_srikrishnan_col[5],NA,trans_tebaldi12_col[2],NA,trans_zervas13_col[2],NA,NA,NA,NA), pt.cex = c(2,NA,2,NA,2,NA,1,1,1))

dev.off()
#   -----------------------------------------------------------------------

##=========================== RETURN PERIOD PLOTS OF COMBINED STORM SURGE AND SLR ===================================
# pdf(file="../Figures/Fig4.pdf", family="Times", width=text_column_width, height=single_panel_height*2, pointsize=12)
pdf(file="../Figures/Fig4CI.pdf", family="Times", width=text_column_width, height=single_panel_height*2, pointsize=12)
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
par(oma=c(1.5,1.5,0,0), mgp=c(1.5,.5,0), mar=c(0,3,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")

# legend("topleft", legend=c("Wong & Keller 2017 no FD","Wong & Keller 2017 FD", 
#                            "Kopp et al. 2017", "Kopp et al. 2014", "Rasmussen et al. 2018", "Sweet et al. 2017"), ncol=2, 
#        lty=1, lwd=2, col=c(NO_fd_col[2], brickfd_col[2], kopp17_DP16_col[2], kopp14_col[2], Ras18_col[2], sweet17_col[3]), bty='n')

legend("topleft", legend=c("95% CI Wong & Keller 2017 no FD","95% CI Wong & Keller 2017 FD", 
                           "95% CI Kopp et al. 2017", "95% CI Kopp et al. 2014", "95% CI Rasmussen et al. 2018", "95% CI Sweet et al. 2017"), ncol=2, 
       pch=15, pt.cex = 2, col=c(NO_fd_col[2], brickfd_col[2], kopp17_DP16_col[2], kopp14_col[2], Ras18_col[2], sweet17_col[3]), bty='n')

gradient.rect(8.35,7.5,10.1,9, col=col_grad, gradient="x")
arrows(9.95, 6.3, 10.2, 6.3, length=0.075)
text(9.1,6.3, "Higher scenario")

# gradient.rect(7.35,7.5,9.1,9, col=col_grad, gradient="x")
# arrows(8.95, 6.3, 9.2, 6.3, length=0.075)
# text(8.1,6.3, "Higher scenario")

#   -----------------------------------------------------------------------
# a) low scenarios
par(mgp=c(1.5,.5,0), mar=c(3.5,3,1,0.5))
plot_SLRandStormSurge_CI(year = 2030, scen = "rcp26", deg = "1p5deg", sweet.scen = c("05", "03"), probs, panel = "a.")
# plot_SLRandStormSurge_SF_scen(year = 2030, scen = "rcp26", deg = "1p5deg", sweet.scen = c("05", "03"), panel = "a.")

#   -----------------------------------------------------------------------
# b) medium scenarios
plot_SLRandStormSurge_CI(year = 2030, scen = "rcp45", deg = "2p0deg", sweet.scen = c("15", "10"), probs, panel = "b.")
# plot_SLRandStormSurge_SF_scen(year = 2030, scen = "rcp45", deg = "2p0deg", sweet.scen = c("15", "10"), panel = "b.")

#   -----------------------------------------------------------------------
# c) high scenarios
par(mgp=c(1.5,0.5,0), mar=c(3.5,3,1,1))
plot_SLRandStormSurge_CI(year = 2030, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), probs, panel = "c.")
# plot_SLRandStormSurge_SF_scen(year = 2030, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), panel = "c.")

#---------------------------- 2050 -----------------------------------
# d) low scenarios
par(mgp=c(1.5,.5,0), mar=c(3.5,3,1,0.5))
plot_SLRandStormSurge_CI(year = 2050, scen = "rcp26", deg = "1p5deg", sweet.scen = c("05", "03"), probs, panel = "d.")
# plot_SLRandStormSurge_SF_scen(year = 2050, scen = "rcp26", deg = "1p5deg", sweet.scen = c("05", "03"), panel = "d.")

#   -----------------------------------------------------------------------
# e) medium scenarios
plot_SLRandStormSurge_CI(year = 2050, scen = "rcp45", deg = "2p0deg", sweet.scen = c("15", "10"), probs, panel = "e.")
# plot_SLRandStormSurge_SF_scen(year = 2050, scen = "rcp45", deg = "2p0deg", sweet.scen = c("15", "10"), panel = "e.")

#   -----------------------------------------------------------------------
# f) high scenarios
par(mgp=c(1.5,0.5,0), mar=c(3.5,3,1,1))
plot_SLRandStormSurge_CI(year = 2050, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), probs, panel = "f.")
# plot_SLRandStormSurge_SF_scen(year = 2050, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), panel = "f.")

#---------------------------- 2070 -----------------------------------
# g) low scenarios
par(mgp=c(1.5,.5,0), mar=c(3.5,3,1,0.5))
plot_SLRandStormSurge_CI(year = 2070, scen = "rcp26", deg = "1p5deg", sweet.scen = c("05", "03"), probs, panel = "g.")
# plot_SLRandStormSurge_SF_scen(year = 2070, scen = "rcp26", deg = "1p5deg", sweet.scen = c("05", "03"), panel = "g.")

#   -----------------------------------------------------------------------
# h) medium scenarios
plot_SLRandStormSurge_CI(year = 2070, scen = "rcp45", deg = "2p0deg", sweet.scen = c("15", "10"), probs, panel = "h.")
# plot_SLRandStormSurge_SF_scen(year = 2070, scen = "rcp45", deg = "2p0deg", sweet.scen = c("15", "10"), panel = "h.")

#   -----------------------------------------------------------------------
# i) high scenarios
par(mgp=c(1.5,0.5,0), mar=c(3.5,3,1,1))
plot_SLRandStormSurge_CI(year = 2070, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), probs, panel = "i.")
# plot_SLRandStormSurge_SF_scen(year = 2070, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), panel = "i.")

#---------------------------- 2100 -----------------------------------
# j) low scenarios
par(mgp=c(1.5,.5,0), mar=c(3.5,3,1,0.5))
plot_SLRandStormSurge_CI(year = 2100, scen = "rcp26", deg = "1p5deg", sweet.scen = c("05", "03"), probs, panel = "j.")
# plot_SLRandStormSurge_SF_scen(year = 2100, scen = "rcp26", deg = "1p5deg", sweet.scen = c("05", "03"), panel = "j.")

#   -----------------------------------------------------------------------
# k) medium scenarios
plot_SLRandStormSurge_CI(year = 2100, scen = "rcp45", deg = "2p0deg", sweet.scen = c("15", "10"), probs, panel = "k.")
# plot_SLRandStormSurge_SF_scen(year = 2100, scen = "rcp45", deg = "2p0deg", sweet.scen = c("15", "10"), panel = "k.")

#   -----------------------------------------------------------------------
# l) high scenarios
par(mgp=c(1.5,0.5,0), mar=c(3.5,3,1,1))
plot_SLRandStormSurge_CI(year = 2100, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), probs, panel = "l.")
# plot_SLRandStormSurge_SF_scen(year = 2100, scen = "rcp85", deg = "2p5deg", sweet.scen = c("25", "20"), panel = "l.")

#   -----------------------------------------------------------------------
mtext(text="Projected sea+surge level (ft MSL)",side=2,line=0,outer=TRUE)
mtext(text="Return period (years)",side=1,line=0,outer=TRUE)

dev.off()
#   -----------------------------------------------------------------------

##==============================================================================
## End
##==============================================================================

