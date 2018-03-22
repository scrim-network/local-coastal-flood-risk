setwd('/Users/klr324/Documents/Data_LSL')

library(ncdf4)
library(extRemes)
library(RColorBrewer)
library(plotrix)
library(ash)
library(fields)

source("local-costal-flood-risk/R/plot_sf.r")
# Read in sea level and storm surge data 
source("local-costal-flood-risk/R/read_norfolk_sea_data.R")

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

# Conversion of datums
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

# colors ------------------------------------------------------------------
seq_color = function(num, maincol){
  col_fun <- colorRampPalette(c("white", maincol, "black"))
  col_fun(num)
}

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

RdGy = brewer.pal(11, "RdGy")
BrBG = brewer.pal(11, "BrBG")
RdBu = brewer.pal(11, "RdBu")
PRGn = brewer.pal(11, "PRGn")
PiYG = brewer.pal(11, "PiYG")

noaa_cols = seq_color(9, PiYG[1:5])
col_grad = seq_color(150, RdGy[7:11])
tebaldi_gold = c("#f0cf0c", "#fcf5ce")

trans_RdGy = makeTransparent(RdGy, 75)
trans_BrBG = makeTransparent(BrBG, 75)
trans_RdBu = makeTransparent(RdBu, 75)
trans_PRGn = makeTransparent(PRGn, 75)
trans_PiYG = makeTransparent(PiYG, 75)
trans_noaa_cols = makeTransparent(noaa_cols, 75)
trans_tebaldi_gold = makeTransparent(tebaldi_gold, 200)

#-------------------------- Set widths and heights ------------------------------
inches_to_dpi = function(inch){ inch * 300 }

text_column_width   = 5.2
minimum_width       = 2.63
full_page_width     = 7.5
full_page_height    = 8.75
single_panel_height = 4

#---------------------------- Plot PDFS -----------------------------------
################################ 2030 #####################################
pdf(file="SLR_2030.pdf", family="Times", width=full_page_width, height=1.5*single_panel_height, pointsize=12)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4,
                2,3,4,
                5,6,7,
                5,6,7,
                5,6,7), 7, 3, byrow = TRUE))
par(mgp=c(1.5,.5,0), mar=c(0,4,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Sweet et al. 2017", "Tebaldi et al. 2012\nexpected value", 
                           "Parris et al. 2012","Tebaldi et al. 2012\n95% CI", "USACE 2014", 
                           "Srikrishnan et al.\nin prep. S 95% CI", "Hall et al. 2016", "Srikrishnan et al.\nin prep. S",
                           "Observations"),
       lty=c(1,1,1,NA,1,NA,NA,NA,NA,NA,1,NA), lwd=c(2,2,2,NA,2,NA,NA,NA,NA,NA,2,NA), pch=c(NA,NA,NA,19,NA,19,22,19,22,19,NA,19), 
       col=c(BrBG[9], RdGy[3], PRGn[3], noaa_cols[5], tebaldi_gold[1],BrBG[2],"black", RdBu[10], "black", RdGy[9], RdBu[11], "black"),
       bty='n', ncol=6, pt.bg=c(NA,NA,NA,NA,NA,NA,tebaldi_gold[2],NA,trans_RdBu[9],NA,NA,NA), pt.cex = c(NA,NA,NA,1,NA,1,2,1,2,1,NA,1))
gradient.rect(7.5,1.5,9.5,3, col=col_grad, gradient="x")
arrows(9.25, 0.5, 9.5, 0.5, length=0.075)
text(8.5,0.5, "Higher scenario")

legend("bottomleft", legend=c("Srikrishnan et al.\nin prep. NS", "Srikrishnan et al.\nin prep. NS 95% CI",
                              "Zervas 2013\nexpected value", "Zervas 2013\n 95% CI"),
       lty=c(3,NA,1,NA), lwd=c(2,NA,2,NA), pch=c(NA,22,NA,22), 
       col=c(noaa_cols[2], "black", BrBG[2], "black"), bty='n', horiz=TRUE, 
       pt.bg=c(NA,trans_noaa_cols[7],NA,trans_BrBG[2]), pt.cex = c(NA,2,NA,2))

par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85_2030ft), xlab="Projected sea level in 2030 (ft)", ylab="Probability density", yaxt="n", 
     main="",col=RdGy[1], lwd=2, xlim=c(-0.2,2), ylim = c(0, 6.75), bty="l")
title(main="a.", adj=0)
lines(density(kopp14_rcp60_2030ft), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45_2030ft), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26_2030ft), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85_2030ft), col=BrBG[11], lwd=2)
lines(density(brickfd_rcp60_2030ft), col=BrBG[10], lwd=2)
lines(density(brickfd_rcp45_2030ft), col=BrBG[9], lwd=2)
lines(density(brickfd_rcp26_2030ft), col=BrBG[8], lwd=2)

lines(density(NO_fd_rcp85_2030ft), col=PRGn[2], lwd=2)
lines(density(NO_fd_rcp60_2030ft), col=PRGn[3], lwd=2)
lines(density(NO_fd_rcp45_2030ft), col=PRGn[4], lwd=2)
lines(density(NO_fd_rcp26_2030ft), col=PRGn[5], lwd=2)

lines(noaa2012_2030, rep(6.5, 4), col=BrBG[4:1], lwd=1.5, lty=3)
lines(usace2013_2030, rep(6, 3), col=RdBu[9:11], lwd=1.5, lty=3)
lines(carswg2016_2030, rep(5.5, 5), col=RdGy[7:11], lwd=1.5, lty=3)
lines(noaa2017_2030_50, rep(5, length(noaa2017_2030_50)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012_2030, rep(6.5, 4), col=BrBG[4:1], pch=19)
points(usace2013_2030, rep(6, 3), col=RdBu[9:11], pch=19)
points(carswg2016_2030, rep(5.5, 5), col=RdGy[7:11], pch=19)
points(noaa2017_2030_50, rep(5, length(noaa2017_2030_50)), col=noaa_cols[8:2], pch=19)

#   -----------------------------------------------------------------------
plot(1/annual_exceed, return_level, log = "x", type = "n", xlim = c(1, 90),
     ylim = c(2.85, 12), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Storm surge (ft MSL)")
title(main="b.", adj=0)
axis(1, lwd = 1, at=10^(seq(-1,log10(10^2), by = 1)), label=c(0.1, 1, 10, 100))

SF_Srikrishnan_nonstationary25 = plot.sf(nonstat_gev203025, make.plot=FALSE)
SF_Srikrishnan_nonstationary975 = plot.sf(nonstat_gev2030975, make.plot=FALSE)
polygon(y = c(SF_Srikrishnan_nonstationary25$sf.num, rev(SF_Srikrishnan_nonstationary975$sf.num)), 
        x = c(1/SF_Srikrishnan_nonstationary25$sf, rev(1/SF_Srikrishnan_nonstationary975$sf)), col = trans_noaa_cols[7], 
        border = trans_noaa_cols[7], lty=3)

SF_Srikrishnan_stationary25 = plot.sf(stat_gev25, make.plot=FALSE)
SF_Srikrishnan_stationary975 = plot.sf(stat_gev975, make.plot=FALSE)
polygon(y = c(SF_Srikrishnan_stationary25$sf.num, rev(SF_Srikrishnan_stationary975$sf.num)), 
        x = c(1/SF_Srikrishnan_stationary25$sf, rev(1/SF_Srikrishnan_stationary975$sf)), col = trans_RdBu[9], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi_gold[2], border = NA)

polygon(y = c(zervas_2013$min_95, rev(zervas_2013$max_95)), 
        x = c(1/zervas_2013$aep, rev(1/zervas_2013$aep)), col = trans_BrBG[2], border = NA)
points(NOAA_methodGEV$return_obs, obs, pch = 19)

SF_Srikrishnan_nonstationary = plot.sf(nonstat_gev2030, make.plot=FALSE)
lines(1/SF_Srikrishnan_nonstationary$sf, SF_Srikrishnan_nonstationary$sf.num, col=noaa_cols[2], lwd=2, lty=3)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=RdBu[11], lwd=2)

lines(NOAA_rp, NOAA_rl, lwd=2, col=BrBG[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi_gold[1])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=RdBu[10])

# ----------------------------------------------------------------------
# PLACE HOLDER
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(density(k14_r85_2030_SSN), xlab="Projected sea+surge level (ft)", ylab="Probability density", yaxt="n", type = "l", 
     main="", col=RdGy[1], lwd=2, lty=3, xlim=c(min(k14_r85_2030_SSN),12), ylim=c(0, 0.75), bty="l")
title(main="c.", adj=0)

lines(density(k14_r85_2030_SS), col=RdGy[1], lwd=2)
lines(density(k14_r60_2030_SS), col=RdGy[2], lwd=2)
lines(density(k14_r45_2030_SS), col=RdGy[3], lwd=2)
lines(density(k14_r26_2030_SS), col=RdGy[4], lwd=2)

lines(density(bfd_r85_2030_SSN), col=BrBG[11], lwd=2, lty=3)
lines(density(bfd_r85_2030_SS), col=BrBG[11], lwd=2)
lines(density(bfd_r60_2030_SS), col=BrBG[10], lwd=2)
lines(density(bfd_r45_2030_SS), col=BrBG[9], lwd=2)
lines(density(bfd_r26_2030_SS), col=BrBG[8], lwd=2)

lines(density(NOfd_r85_2030_SSN), col=PRGn[2], lwd=2, lty=3)
lines(density(NOfd_r85_2030_SS), col=PRGn[2], lwd=2)
lines(density(NOfd_r60_2030_SS), col=PRGn[3], lwd=2)
lines(density(NOfd_r45_2030_SS), col=PRGn[4], lwd=2)
lines(density(NOfd_r26_2030_SS), col=PRGn[5], lwd=2)

# ----------------------------------------------------------------------
par(mgp=c(2,.5,0), mar=c(3.5,4,1,1))
plot.sf(kopp14_rcp85_2030ft, ylab = "Probability of exceedance", xlab = "Projected sea level in 2030 (ft)",
        yaxt = "n", yaxs = 'i', typ="l", lwd=2, lty=1, bty="l",
        ylim = c(10^-4, 10^0+0.25), xlim=c(0,1.8), col=RdGy[1])
title(main="d.", adj=0)

SF_kopp14_rcp60_2030ft = plot.sf(kopp14_rcp60_2030ft, make.plot=FALSE)
lines(SF_kopp14_rcp60_2030ft$sf.num, SF_kopp14_rcp60_2030ft$sf, col=RdGy[2], lwd=2)
SF_kopp14_rcp45_2030ft = plot.sf(kopp14_rcp45_2030ft, make.plot=FALSE)
lines(SF_kopp14_rcp45_2030ft$sf.num, SF_kopp14_rcp45_2030ft$sf, col=RdGy[3], lwd=2)
SF_kopp14_rcp26_2030ft = plot.sf(kopp14_rcp26_2030ft, make.plot=FALSE)
lines(SF_kopp14_rcp26_2030ft$sf.num, SF_kopp14_rcp26_2030ft$sf, col=RdGy[4], lwd=2)

SF_brickfd_rcp85_2030ft = plot.sf(brickfd_rcp85_2030ft, make.plot=FALSE)
lines(SF_brickfd_rcp85_2030ft$sf.num, SF_brickfd_rcp85_2030ft$sf, col=BrBG[11], lwd=2)
SF_brickfd_rcp60_2030ft = plot.sf(brickfd_rcp60_2030ft, make.plot=FALSE)
lines(SF_brickfd_rcp60_2030ft$sf.num, SF_brickfd_rcp60_2030ft$sf, col=BrBG[10], lwd=2)
SF_brickfd_rcp45_2030ft = plot.sf(brickfd_rcp45_2030ft, make.plot=FALSE)
lines(SF_brickfd_rcp45_2030ft$sf.num, SF_brickfd_rcp45_2030ft$sf, col=BrBG[9], lwd=2)
SF_brickfd_rcp26_2030ft = plot.sf(brickfd_rcp26_2030ft, make.plot=FALSE)
lines(SF_brickfd_rcp26_2030ft$sf.num, SF_brickfd_rcp26_2030ft$sf, col=BrBG[8], lwd=2)

SF_NO_fd_rcp85_2030ft = plot.sf(NO_fd_rcp85_2030ft, make.plot=FALSE)
lines(SF_NO_fd_rcp85_2030ft$sf.num, SF_NO_fd_rcp85_2030ft$sf, col=PRGn[2], lwd=2)
SF_NO_fd_rcp60_2030ft = plot.sf(NO_fd_rcp60_2030ft, make.plot=FALSE)
lines(SF_NO_fd_rcp60_2030ft$sf.num, SF_NO_fd_rcp60_2030ft$sf, col=PRGn[3], lwd=2)
SF_NO_fd_rcp45_2030ft = plot.sf(NO_fd_rcp45_2030ft, make.plot=FALSE)
lines(SF_NO_fd_rcp45_2030ft$sf.num, SF_NO_fd_rcp45_2030ft$sf, col=PRGn[4], lwd=2)
SF_NO_fd_rcp26_2030ft = plot.sf(NO_fd_rcp26_2030ft, make.plot=FALSE)
lines(SF_NO_fd_rcp26_2030ft$sf.num, SF_NO_fd_rcp26_2030ft$sf, col=PRGn[5], lwd=2)

#   -----------------------------------------------------------------------
plot(return_level, annual_exceed, log = "y", type = "n",bty="l",
     yaxt = 'n', cex=1, xlim = c(2.85, 12), ylim=c(0.011,1),
     ylab = "Probability of exceedance", xlab = "Storm surge (ft MSL)")
axis(2, at=10^(-4:0), label=parse(text=paste("10^", -4:0, sep="")), las=1)
title(main="e.", adj=0)

polygon(x = c(SF_Srikrishnan_nonstationary25$sf.num, rev(SF_Srikrishnan_nonstationary975$sf.num)), 
        y = c(SF_Srikrishnan_nonstationary25$sf, rev(SF_Srikrishnan_nonstationary975$sf)), col = trans_noaa_cols[7], 
        border = trans_noaa_cols[7], lty=3)

polygon(x = c(SF_Srikrishnan_stationary25$sf.num, rev(SF_Srikrishnan_stationary975$sf.num)), 
        y = c(SF_Srikrishnan_stationary25$sf, rev(SF_Srikrishnan_stationary975$sf)), col = trans_RdBu[9], border = NA)

polygon(x = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        y = c(1/tebaldi12$rp, rev(1/tebaldi12$rp)), col = trans_tebaldi_gold[2], border = NA)

polygon(x = c(zervas_2013$min_95, rev(zervas_2013$max_95)), 
        y = c(zervas_2013$aep, rev(zervas_2013$aep)), col = trans_BrBG[2], border = NA)
points(obs, 1/NOAA_methodGEV$return_obs, pch = 19)

lines(SF_Srikrishnan_nonstationary$sf.num, SF_Srikrishnan_nonstationary$sf, col=noaa_cols[2], lwd=2, lty=3)
lines(SF_Srikrishnan_stationary$sf.num, SF_Srikrishnan_stationary$sf, col=RdBu[11], lwd=2)
lines(NOAA_rl, 1/NOAA_rp, lwd=2, col=BrBG[2])
lines(tebaldi12$rl_50, 1/tebaldi12$rp, lty = 1, lwd = 2, col=tebaldi_gold[1])
points(USACE_EWL$feet[8:14], 1/USACE_rp, pch = 20, col=RdBu[10])

# ----------------------------------------------------------------------
# PLACE HOLDER
par(mgp=c(2,0.5,0), mar=c(3.5,3.5,1,1))
plot(return_level, annual_exceed, log = "y", type = "n", ylim = c(0.011, 1),
     xlim = c(2.85, 12), 
     yaxt = 'n', cex=1, bty="l",
     ylab = "Probability of exceedance", 
     xlab = "Projected sea+surge level (ft)")
title(main="f.", adj=0)
axis(2, at=10^(-4:0), label=parse(text=paste("10^", -4:0, sep="")), las=1)
# axis(1, lwd = 1, at=10^(seq(-1,log10(10^2), by = 1)), label=c(0.1, 1, 10, 100))

SF_k14_r85_2030_SSN = plot.sf(k14_r85_2030_SSN, make.plot=FALSE)
lines(SF_k14_r85_2030_SSN$sf.num, SF_k14_r85_2030_SSN$sf, col=RdGy[1], lwd=1.5, lty=3)

SF_k14_r85_2030_SS = plot.sf(k14_r85_2030_SS, make.plot=FALSE)
lines(SF_k14_r85_2030_SS$sf.num, SF_k14_r85_2030_SS$sf, col=RdGy[1], lwd=1.5)
SF_k14_r60_2030_SS = plot.sf(k14_r60_2030_SS, make.plot=FALSE)
lines(SF_k14_r60_2030_SS$sf.num, SF_k14_r60_2030_SS$sf, col=RdGy[2], lwd=1.5)
SF_k14_r45_2030_SS = plot.sf(k14_r45_2030_SS, make.plot=FALSE)
lines(SF_k14_r45_2030_SS$sf.num, SF_k14_r45_2030_SS$sf, col=RdGy[3], lwd=1.5)
SF_k14_r26_2030_SS = plot.sf(k14_r26_2030_SS, make.plot=FALSE)
lines(SF_k14_r26_2030_SS$sf.num, SF_k14_r26_2030_SS$sf, col=RdGy[4], lwd=1.5)

SF_bfd_r85_2030_SSN = plot.sf(bfd_r85_2030_SSN, make.plot=FALSE)
lines(SF_bfd_r85_2030_SSN$sf.num, SF_bfd_r85_2030_SSN$sf, col=BrBG[11], lwd=1.5, lty=3)

SF_bfd_r85_2030_SS = plot.sf(bfd_r85_2030_SS, make.plot=FALSE)
lines(SF_bfd_r85_2030_SS$sf.num, SF_bfd_r85_2030_SS$sf, col=BrBG[11], lwd=1.5)
SF_bfd_r60_2030_SS = plot.sf(bfd_r60_2030_SS, make.plot=FALSE)
lines(SF_bfd_r60_2030_SS$sf.num, SF_bfd_r60_2030_SS$sf, col=BrBG[10], lwd=1.5)
SF_bfd_r45_2030_SS = plot.sf(bfd_r45_2030_SS, make.plot=FALSE)
lines(SF_bfd_r45_2030_SS$sf.num, SF_bfd_r45_2030_SS$sf, col=BrBG[9], lwd=1.5)
SF_bfd_r26_2030_SS = plot.sf(bfd_r26_2030_SS, make.plot=FALSE)
lines(SF_bfd_r26_2030_SS$sf.num, SF_bfd_r26_2030_SS$sf, col=BrBG[8], lwd=1.5)

SF_NOfd_r85_2030_SSN = plot.sf(NOfd_r85_2030_SSN, make.plot=FALSE)
lines(SF_NOfd_r85_2030_SSN$sf.num, SF_NOfd_r85_2030_SSN$sf, col=PRGn[2], lwd=1.5, lty=3)

SF_NOfd_r85_2030_SS = plot.sf(NOfd_r85_2030_SS, make.plot=FALSE)
lines(SF_NOfd_r85_2030_SS$sf.num, SF_NOfd_r85_2030_SS$sf, col=PRGn[2], lwd=1.5)
SF_NOfd_r60_2030_SS = plot.sf(NOfd_r60_2030_SS, make.plot=FALSE)
lines(SF_NOfd_r60_2030_SS$sf.num, SF_NOfd_r60_2030_SS$sf, col=PRGn[3], lwd=1.5)
SF_NOfd_r45_2030_SS = plot.sf(NOfd_r45_2030_SS, make.plot=FALSE)
lines(SF_NOfd_r45_2030_SS$sf.num, SF_NOfd_r45_2030_SS$sf, col=PRGn[4], lwd=1.5)
SF_NOfd_r26_2030_SS = plot.sf(NOfd_r26_2030_SS, make.plot=FALSE)
lines(SF_NOfd_r26_2030_SS$sf.num, SF_NOfd_r26_2030_SS$sf, col=PRGn[5], lwd=1.5)

dev.off()

################################ 2050 #####################################
pdf(file="SLR_2050.pdf", family="Times", width=full_page_width, height=1.5*single_panel_height, pointsize=12)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4,
                2,3,4,
                5,6,7,
                5,6,7,
                5,6,7), 7, 3, byrow = TRUE))
par(mgp=c(1.5,.5,0), mar=c(0,4,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Sweet et al. 2017", "Tebaldi et al. 2012\nexpected value", 
                           "Parris et al. 2012","Tebaldi et al. 2012\n95% CI", "USACE 2014", 
                           "Srikrishnan et al.\nin prep. S 95% CI", "Hall et al. 2016", "Srikrishnan et al.\nin prep. S",
                           "Observations"),
       lty=c(1,1,1,NA,1,NA,NA,NA,NA,NA,1,NA), lwd=c(2,2,2,NA,2,NA,NA,NA,NA,NA,2,NA), pch=c(NA,NA,NA,19,NA,19,22,19,22,19,NA,19), 
       col=c(BrBG[9], RdGy[3], PRGn[3], noaa_cols[5], tebaldi_gold[1],BrBG[2],"black", RdBu[10], "black", RdGy[9], RdBu[11], "black"),
       bty='n', ncol=6, pt.bg=c(NA,NA,NA,NA,NA,NA,tebaldi_gold[2],NA,trans_RdBu[9],NA,NA,NA), pt.cex = c(NA,NA,NA,1,NA,1,2,1,2,1,NA,1))
gradient.rect(7.5,1.5,9.5,3, col=col_grad, gradient="x")
arrows(9.25, 0.5, 9.5, 0.5, length=0.075)
text(8.5,0.5, "Higher scenario")

legend("bottomleft", legend=c("Srikrishnan et al.\nin prep. NS", "Srikrishnan et al.\nin prep. NS 95% CI",
                              "Zervas 2013\nexpected value", "Zervas 2013\n 95% CI"),
       lty=c(3,NA,1,NA), lwd=c(2,NA,2,NA), pch=c(NA,22,NA,22), 
       col=c(noaa_cols[2], "black", BrBG[2], "black"), bty='n', horiz=TRUE, 
       pt.bg=c(NA,trans_noaa_cols[7],NA,trans_BrBG[2]), pt.cex = c(NA,2,NA,2))

# Plot LSLR in 2050
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85_2050ft), xlab="Projected sea level in 2050 (ft)", ylab="Probability density", yaxt="n", 
     main="", col=RdGy[1], lwd=2, xlim=c(-0.2,4), ylim = c(0, 3.5), bty="l")
title(main="a.", adj=0)
lines(density(kopp14_rcp60_2050ft), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45_2050ft), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26_2050ft), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85_2050ft), col=BrBG[11], lwd=2)
lines(density(brickfd_rcp60_2050ft), col=BrBG[10], lwd=2)
lines(density(brickfd_rcp45_2050ft), col=BrBG[9], lwd=2)
lines(density(brickfd_rcp26_2050ft), col=BrBG[8], lwd=2)

lines(density(NO_fd_rcp85_2050ft), col=PRGn[2], lwd=2)
lines(density(NO_fd_rcp60_2050ft), col=PRGn[3], lwd=2)
lines(density(NO_fd_rcp45_2050ft), col=PRGn[4], lwd=2)
lines(density(NO_fd_rcp26_2050ft), col=PRGn[5], lwd=2)

lines(noaa2012_2050, rep(3.25, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2013_2050, rep(3, 3), col=RdBu[9:11], lwd=2, lty=3)
lines(carswg2016_2050, rep(2.75, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(noaa2017_2050_50, rep(2.5, length(noaa2017_2050_50)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012_2050, rep(3.25, 4), col=BrBG[4:1], pch=19)
points(usace2013_2050, rep(3, 3), col=RdBu[9:11], pch=19)
points(carswg2016_2050, rep(2.75, 5), col=RdGy[7:11], pch=19)
points(noaa2017_2050_50, rep(2.5, length(noaa2017_2050_50)), col=noaa_cols[8:2], pch=19)

#   -----------------------------------------------------------------------
plot(1/annual_exceed, return_level, log = "x", type = "n", xlim = c(1, 90),
     ylim = c(2.85, 12), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Storm surge (ft MSL)")
title(main="b.", adj=0)
axis(1, lwd = 1, at=10^(seq(-1,log10(10^2), by = 1)), label=c(0.1, 1, 10, 100))

SF_Srikrishnan_nonstationary25 = plot.sf(nonstat_gev205025, make.plot=FALSE)
SF_Srikrishnan_nonstationary975 = plot.sf(nonstat_gev2050975, make.plot=FALSE)
polygon(y = c(SF_Srikrishnan_nonstationary25$sf.num, rev(SF_Srikrishnan_nonstationary975$sf.num)), 
        x = c(1/SF_Srikrishnan_nonstationary25$sf, rev(1/SF_Srikrishnan_nonstationary975$sf)), col = trans_noaa_cols[7], 
        border = trans_noaa_cols[7], lty=3)

SF_Srikrishnan_stationary25 = plot.sf(stat_gev25, make.plot=FALSE)
SF_Srikrishnan_stationary975 = plot.sf(stat_gev975, make.plot=FALSE)
polygon(y = c(SF_Srikrishnan_stationary25$sf.num, rev(SF_Srikrishnan_stationary975$sf.num)), 
        x = c(1/SF_Srikrishnan_stationary25$sf, rev(1/SF_Srikrishnan_stationary975$sf)), col = trans_RdBu[9], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi_gold[2], border = NA)

polygon(y = c(zervas_2013$min_95, rev(zervas_2013$max_95)), 
        x = c(1/zervas_2013$aep, rev(1/zervas_2013$aep)), col = trans_BrBG[2], border = NA)
points(NOAA_methodGEV$return_obs, obs, pch = 19)

SF_Srikrishnan_nonstationary = plot.sf(nonstat_gev2050, make.plot=FALSE)
lines(1/SF_Srikrishnan_nonstationary$sf, SF_Srikrishnan_nonstationary$sf.num, col=noaa_cols[2], lwd=2, lty=3)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=RdBu[11], lwd=2)

lines(NOAA_rp, NOAA_rl, lwd=2, col=BrBG[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi_gold[1])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=RdBu[10])

# ----------------------------------------------------------------------
# PLACE HOLDER
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(density(k14_r85_2050_SSN), xlab="Projected sea+surge level (ft)", ylab="Probability density", yaxt="n", type = "l", 
     main="", col=RdGy[1], lwd=2, lty=3, xlim=c(min(k14_r85_2050_SSN), 15), ylim=c(0, 0.75), bty="l")
title(main="c.", adj=0)

lines(density(k14_r85_2050_SS), col=RdGy[1], lwd=2)
lines(density(k14_r60_2050_SS), col=RdGy[2], lwd=2)
lines(density(k14_r45_2050_SS), col=RdGy[3], lwd=2)
lines(density(k14_r26_2050_SS), col=RdGy[4], lwd=2)

lines(density(bfd_r85_2050_SSN), col=BrBG[11], lwd=2, lty=3)
lines(density(bfd_r85_2050_SS), col=BrBG[11], lwd=2)
lines(density(bfd_r60_2050_SS), col=BrBG[10], lwd=2)
lines(density(bfd_r45_2050_SS), col=BrBG[9], lwd=2)
lines(density(bfd_r26_2050_SS), col=BrBG[8], lwd=2)

lines(density(NOfd_r85_2050_SSN), col=PRGn[2], lwd=2, lty=3)
lines(density(NOfd_r85_2050_SS), col=PRGn[2], lwd=2)
lines(density(NOfd_r60_2050_SS), col=PRGn[3], lwd=2)
lines(density(NOfd_r45_2050_SS), col=PRGn[4], lwd=2)
lines(density(NOfd_r26_2050_SS), col=PRGn[5], lwd=2)

# SF ----------------------------------------------------------------------
par(mgp=c(2,.5,0), mar=c(3.5,4,1,1))
plot.sf(kopp14_rcp85_2050ft, ylab = "Probability of exceedance", xlab = "Projected sea level in 2050 (ft)",
        yaxt = "n", yaxs = 'i', typ="l", lwd=2, lty=1, bty="l",
        ylim = c(10^-4, 10^0+0.25), col=RdGy[1])
title(main="d.", adj=0)

SF_kopp14_rcp60_2050ft = plot.sf(kopp14_rcp60_2050ft, make.plot=FALSE)
lines(SF_kopp14_rcp60_2050ft$sf.num, SF_kopp14_rcp60_2050ft$sf, col=RdGy[2], lwd=2)
SF_kopp14_rcp45_2050ft = plot.sf(kopp14_rcp45_2050ft, make.plot=FALSE)
lines(SF_kopp14_rcp45_2050ft$sf.num, SF_kopp14_rcp45_2050ft$sf, col=RdGy[3], lwd=2)
SF_kopp14_rcp26_2050ft = plot.sf(kopp14_rcp26_2050ft, make.plot=FALSE)
lines(SF_kopp14_rcp26_2050ft$sf.num, SF_kopp14_rcp26_2050ft$sf, col=RdGy[4], lwd=2)

SF_brickfd_rcp85_2050ft = plot.sf(brickfd_rcp85_2050ft, make.plot=FALSE)
lines(SF_brickfd_rcp85_2050ft$sf.num, SF_brickfd_rcp85_2050ft$sf, col=BrBG[11], lwd=2)
SF_brickfd_rcp60_2050ft = plot.sf(brickfd_rcp60_2050ft, make.plot=FALSE)
lines(SF_brickfd_rcp60_2050ft$sf.num, SF_brickfd_rcp60_2050ft$sf, col=BrBG[10], lwd=2)
SF_brickfd_rcp45_2050ft = plot.sf(brickfd_rcp45_2050ft, make.plot=FALSE)
lines(SF_brickfd_rcp45_2050ft$sf.num, SF_brickfd_rcp45_2050ft$sf, col=BrBG[9], lwd=2)
SF_brickfd_rcp26_2050ft = plot.sf(brickfd_rcp26_2050ft, make.plot=FALSE)
lines(SF_brickfd_rcp26_2050ft$sf.num, SF_brickfd_rcp26_2050ft$sf, col=BrBG[8], lwd=2)

SF_NO_fd_rcp85_2050ft = plot.sf(NO_fd_rcp85_2050ft, make.plot=FALSE)
lines(SF_NO_fd_rcp85_2050ft$sf.num, SF_NO_fd_rcp85_2050ft$sf, col=PRGn[2], lwd=2)
SF_NO_fd_rcp60_2050ft = plot.sf(NO_fd_rcp60_2050ft, make.plot=FALSE)
lines(SF_NO_fd_rcp60_2050ft$sf.num, SF_NO_fd_rcp60_2050ft$sf, col=PRGn[3], lwd=2)
SF_NO_fd_rcp45_2050ft = plot.sf(NO_fd_rcp45_2050ft, make.plot=FALSE)
lines(SF_NO_fd_rcp45_2050ft$sf.num, SF_NO_fd_rcp45_2050ft$sf, col=PRGn[4], lwd=2)
SF_NO_fd_rcp26_2050ft = plot.sf(NO_fd_rcp26_2050ft, make.plot=FALSE)
lines(SF_NO_fd_rcp26_2050ft$sf.num, SF_NO_fd_rcp26_2050ft$sf, col=PRGn[5], lwd=2)

#   -----------------------------------------------------------------------
plot(return_level, annual_exceed, log = "y", type = "n",bty="l",
     yaxt = 'n', cex=1, xlim = c(2.85, 12), ylim=c(0.011,1),
     ylab = "Probability of exceedance", xlab = "Storm surge (ft MSL)")
axis(2, at=10^(-4:0), label=parse(text=paste("10^", -4:0, sep="")), las=1)
title(main="e.", adj=0)

polygon(x = c(SF_Srikrishnan_nonstationary25$sf.num, rev(SF_Srikrishnan_nonstationary975$sf.num)), 
        y = c(SF_Srikrishnan_nonstationary25$sf, rev(SF_Srikrishnan_nonstationary975$sf)), col = trans_noaa_cols[7], 
        border = trans_noaa_cols[7], lty=3)

polygon(x = c(SF_Srikrishnan_stationary25$sf.num, rev(SF_Srikrishnan_stationary975$sf.num)), 
        y = c(SF_Srikrishnan_stationary25$sf, rev(SF_Srikrishnan_stationary975$sf)), col = trans_RdBu[9], border = NA)

polygon(x = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        y = c(1/tebaldi12$rp, rev(1/tebaldi12$rp)), col = trans_tebaldi_gold[2], border = NA)

polygon(x = c(zervas_2013$min_95, rev(zervas_2013$max_95)), 
        y = c(zervas_2013$aep, rev(zervas_2013$aep)), col = trans_BrBG[2], border = NA)
points(obs, 1/NOAA_methodGEV$return_obs, pch = 19)

lines(SF_Srikrishnan_nonstationary$sf.num, SF_Srikrishnan_nonstationary$sf, col=noaa_cols[2], lwd=2, lty=3)
lines(SF_Srikrishnan_stationary$sf.num, SF_Srikrishnan_stationary$sf, col=RdBu[11], lwd=2)
lines(NOAA_rl, 1/NOAA_rp, lwd=2, col=BrBG[2])
lines(tebaldi12$rl_50, 1/tebaldi12$rp, lty = 1, lwd = 2, col=tebaldi_gold[1])
points(USACE_EWL$feet[8:14], 1/USACE_rp, pch = 20, col=RdBu[10])

# ----------------------------------------------------------------------
# PLACE HOLDER
par(mgp=c(2,0.5,0), mar=c(3.5,3.5,1,1))
plot(return_level, annual_exceed, log = "y", type = "n",bty="l",
     yaxt = 'n', cex=1, xlim = c(2.85, 15), ylim=c(0.011,1),
     ylab = "Probability of exceedance", xlab = "Projected sea+surge level (ft)")
axis(2, at=10^(-4:0), label=parse(text=paste("10^", -4:0, sep="")), las=1)
title(main="f.", adj=0)

SF_k14_r85_2050_SSN = plot.sf(k14_r85_2050_SSN, make.plot=FALSE)
lines(SF_k14_r85_2050_SSN$sf.num, SF_k14_r85_2050_SSN$sf, col=RdGy[1], lwd=1.5, lty=3)

SF_k14_r85_2050_SS = plot.sf(k14_r85_2050_SS, make.plot=FALSE)
lines(SF_k14_r85_2050_SS$sf.num, SF_k14_r85_2050_SS$sf, col=RdGy[1], lwd=1.5)
SF_k14_r60_2050_SS = plot.sf(k14_r60_2050_SS, make.plot=FALSE)
lines(SF_k14_r60_2050_SS$sf.num, SF_k14_r60_2050_SS$sf, col=RdGy[2], lwd=1.5)
SF_k14_r45_2050_SS = plot.sf(k14_r45_2050_SS, make.plot=FALSE)
lines(SF_k14_r45_2050_SS$sf.num, SF_k14_r45_2050_SS$sf, col=RdGy[3], lwd=1.5)
SF_k14_r26_2050_SS = plot.sf(k14_r26_2050_SS, make.plot=FALSE)
lines(SF_k14_r26_2050_SS$sf.num, SF_k14_r26_2050_SS$sf, col=RdGy[4], lwd=1.5)

SF_bfd_r85_2050_SSN = plot.sf(bfd_r85_2050_SSN, make.plot=FALSE)
lines(SF_bfd_r85_2050_SSN$sf.num, SF_bfd_r85_2050_SSN$sf, col=BrBG[11], lwd=1.5, lty=3)

SF_bfd_r85_2050_SS = plot.sf(bfd_r85_2050_SS, make.plot=FALSE)
lines(SF_bfd_r85_2050_SS$sf.num, SF_bfd_r85_2050_SS$sf, col=BrBG[11], lwd=1.5)
SF_bfd_r60_2050_SS = plot.sf(bfd_r60_2050_SS, make.plot=FALSE)
lines(SF_bfd_r60_2050_SS$sf.num, SF_bfd_r60_2050_SS$sf, col=BrBG[10], lwd=1.5)
SF_bfd_r45_2050_SS = plot.sf(bfd_r45_2050_SS, make.plot=FALSE)
lines(SF_bfd_r45_2050_SS$sf.num, SF_bfd_r45_2050_SS$sf, col=BrBG[9], lwd=1.5)
SF_bfd_r26_2050_SS = plot.sf(bfd_r26_2050_SS, make.plot=FALSE)
lines(SF_bfd_r26_2050_SS$sf.num, SF_bfd_r26_2050_SS$sf, col=BrBG[8], lwd=1.5)

SF_NOfd_r85_2050_SSN = plot.sf(NOfd_r85_2050_SSN, make.plot=FALSE)
lines(SF_NOfd_r85_2050_SSN$sf.num, SF_NOfd_r85_2050_SSN$sf, col=PRGn[2], lwd=1.5, lty=3)

SF_NOfd_r85_2050_SS = plot.sf(NOfd_r85_2050_SS, make.plot=FALSE)
lines(SF_NOfd_r85_2050_SS$sf.num, SF_NOfd_r85_2050_SS$sf, col=PRGn[2], lwd=1.5)
SF_NOfd_r60_2050_SS = plot.sf(NOfd_r60_2050_SS, make.plot=FALSE)
lines(SF_NOfd_r60_2050_SS$sf.num, SF_NOfd_r60_2050_SS$sf, col=PRGn[3], lwd=1.5)
SF_NOfd_r45_2050_SS = plot.sf(NOfd_r45_2050_SS, make.plot=FALSE)
lines(SF_NOfd_r45_2050_SS$sf.num, SF_NOfd_r45_2050_SS$sf, col=PRGn[4], lwd=1.5)
SF_NOfd_r26_2050_SS = plot.sf(NOfd_r26_2050_SS, make.plot=FALSE)
lines(SF_NOfd_r26_2050_SS$sf.num, SF_NOfd_r26_2050_SS$sf, col=PRGn[5], lwd=1.5)

dev.off()

################################ 2060 #####################################
pdf(file="SLR_2060.pdf", family="Times", width=full_page_width, height=1.5*single_panel_height, pointsize=12)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4,
                2,3,4,
                5,6,7,
                5,6,7,
                5,6,7), 7, 3, byrow = TRUE))
par(mgp=c(1.5,.5,0), mar=c(0,4,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Sweet et al. 2017", "Tebaldi et al. 2012\nexpected value", 
                           "Parris et al. 2012","Tebaldi et al. 2012\n95% CI", "USACE 2014", 
                           "Srikrishnan et al.\nin prep. S 95% CI", "Hall et al. 2016", "Srikrishnan et al.\nin prep. S",
                           "Observations"),
       lty=c(1,1,1,NA,1,NA,NA,NA,NA,NA,1,NA), lwd=c(2,2,2,NA,2,NA,NA,NA,NA,NA,2,NA), pch=c(NA,NA,NA,19,NA,19,22,19,22,19,NA,19), 
       col=c(BrBG[9], RdGy[3], PRGn[3], noaa_cols[5], tebaldi_gold[1],BrBG[2],"black", RdBu[10], "black", RdGy[9], RdBu[11], "black"),
       bty='n', ncol=6, pt.bg=c(NA,NA,NA,NA,NA,NA,tebaldi_gold[2],NA,trans_RdBu[9],NA,NA,NA), pt.cex = c(NA,NA,NA,1,NA,1,2,1,2,1,NA,1))
gradient.rect(7.5,1.5,9.5,3, col=col_grad, gradient="x")
arrows(9.25, 0.5, 9.5, 0.5, length=0.075)
text(8.5,0.5, "Higher scenario")

legend("bottomleft", legend=c("Srikrishnan et al.\nin prep. NS", "Srikrishnan et al.\nin prep. NS 95% CI",
                              "Zervas 2013\nexpected value", "Zervas 2013\n 95% CI"),
       lty=c(3,NA,1,NA), lwd=c(2,NA,2,NA), pch=c(NA,22,NA,22), 
       col=c(noaa_cols[2], "black", BrBG[2], "black"), bty='n', horiz=TRUE, 
       pt.bg=c(NA,trans_noaa_cols[7],NA,trans_BrBG[2]), pt.cex = c(NA,2,NA,2))

# Plot LSLR in 2060
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85_2060ft), xlab="Projected sea level in 2060 (ft)", ylab="Probability density", yaxt="n",
     main="", col=RdGy[1], lwd=2, xlim=c(-0.2,6), ylim = c(0, 3), bty="l")
title(main="a.", adj=0)
lines(density(kopp14_rcp60_2060ft), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45_2060ft), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26_2060ft), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85_2060ft), col=BrBG[11], lwd=2)
lines(density(brickfd_rcp60_2060ft), col=BrBG[10], lwd=2)
lines(density(brickfd_rcp45_2060ft), col=BrBG[9], lwd=2)
lines(density(brickfd_rcp26_2060ft), col=BrBG[8], lwd=2)

lines(density(NO_fd_rcp85_2060ft), col=PRGn[2], lwd=2)
lines(density(NO_fd_rcp60_2060ft), col=PRGn[3], lwd=2)
lines(density(NO_fd_rcp45_2060ft), col=PRGn[4], lwd=2)
lines(density(NO_fd_rcp26_2060ft), col=PRGn[5], lwd=2)

lines(noaa2012_2060, rep(2.75, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2013_2060, rep(2.5, 3), col=RdBu[9:11], lwd=2, lty=3)
lines(carswg2016_2060, rep(2.25, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(noaa2017_2060_50, rep(2, length(noaa2017_2060_50)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012_2060, rep(2.75, 4), col=BrBG[4:1], pch=19)
points(usace2013_2060, rep(2.5, 3), col=RdBu[9:11], pch=19)
points(carswg2016_2060, rep(2.25, 5), col=RdGy[7:11], pch=19)
points(noaa2017_2060_50, rep(2, length(noaa2017_2060_50)), col=noaa_cols[8:2], pch=19)

#   -----------------------------------------------------------------------
plot(1/annual_exceed, return_level, log = "x", type = "n", xlim = c(1, 90),
     ylim = c(2.25, 12), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Storm surge (ft MSL)")
title(main="b.", adj=0)
axis(1, lwd = 1, at=10^(seq(-1,log10(10^2), by = 1)), label=c(0.1, 1, 10, 100))

SF_Srikrishnan_nonstationary25 = plot.sf(nonstat_gev206025, make.plot=FALSE)
SF_Srikrishnan_nonstationary975 = plot.sf(nonstat_gev2060975, make.plot=FALSE)
polygon(y = c(SF_Srikrishnan_nonstationary25$sf.num, rev(SF_Srikrishnan_nonstationary975$sf.num)), 
        x = c(1/SF_Srikrishnan_nonstationary25$sf, rev(1/SF_Srikrishnan_nonstationary975$sf)), col = trans_noaa_cols[7], 
        border = trans_noaa_cols[7], lty=3)

SF_Srikrishnan_stationary25 = plot.sf(stat_gev25, make.plot=FALSE)
SF_Srikrishnan_stationary975 = plot.sf(stat_gev975, make.plot=FALSE)
polygon(y = c(SF_Srikrishnan_stationary25$sf.num, rev(SF_Srikrishnan_stationary975$sf.num)), 
        x = c(1/SF_Srikrishnan_stationary25$sf, rev(1/SF_Srikrishnan_stationary975$sf)), col = trans_RdBu[9], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi_gold[2], border = NA)

polygon(y = c(zervas_2013$min_95, rev(zervas_2013$max_95)), 
        x = c(1/zervas_2013$aep, rev(1/zervas_2013$aep)), col = trans_BrBG[2], border = NA)
points(NOAA_methodGEV$return_obs, obs, pch = 19)

SF_Srikrishnan_nonstationary = plot.sf(nonstat_gev2060, make.plot=FALSE)
lines(1/SF_Srikrishnan_nonstationary$sf, SF_Srikrishnan_nonstationary$sf.num, col=noaa_cols[2], lwd=2, lty=3)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=RdBu[11], lwd=2)

lines(NOAA_rp, NOAA_rl, lwd=2, col=BrBG[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi_gold[1])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=RdBu[10])

# ----------------------------------------------------------------------
# PLACE HOLDER
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(density(k14_r85_2060_SSN), xlab="Projected sea+surge level (ft)", ylab="Probability density", yaxt="n", type = "l", 
     main="", col=RdGy[1], lwd=2, lty=3, xlim=c(min(k14_r85_2060_SSN),max(bfd_r85_2060_SSN)), ylim=c(0, 0.75), bty="l")
title(main="c.", adj=0)

lines(density(k14_r85_2060_SS), col=RdGy[1], lwd=2)
lines(density(k14_r60_2060_SS), col=RdGy[2], lwd=2)
lines(density(k14_r45_2060_SS), col=RdGy[3], lwd=2)
lines(density(k14_r26_2060_SS), col=RdGy[4], lwd=2)

lines(density(bfd_r85_2060_SSN), col=BrBG[11], lwd=2, lty=3)
lines(density(bfd_r85_2060_SS), col=BrBG[11], lwd=2)
lines(density(bfd_r60_2060_SS), col=BrBG[10], lwd=2)
lines(density(bfd_r45_2060_SS), col=BrBG[9], lwd=2)
lines(density(bfd_r26_2060_SS), col=BrBG[8], lwd=2)

lines(density(NOfd_r85_2060_SSN), col=PRGn[2], lwd=2, lty=3)
lines(density(NOfd_r85_2060_SS), col=PRGn[2], lwd=2)
lines(density(NOfd_r60_2060_SS), col=PRGn[3], lwd=2)
lines(density(NOfd_r45_2060_SS), col=PRGn[4], lwd=2)
lines(density(NOfd_r26_2060_SS), col=PRGn[5], lwd=2)

# SF ----------------------------------------------------------------------
par(mgp=c(2,.5,0), mar=c(3.5,4,1,1))
plot.sf(kopp14_rcp85_2060ft, ylab = "Probability of exceedance", xlab = "Projected sea level in 2060 (ft)",
        yaxt = "n", yaxs = 'i', typ="l", lwd=2, lty=1, bty="l",
        ylim = c(10^-4, 10^0+0.25), col=RdGy[1])
title(main="d.", adj=0)

SF_kopp14_rcp60_2060ft = plot.sf(kopp14_rcp60_2060ft, make.plot=FALSE)
lines(SF_kopp14_rcp60_2060ft$sf.num, SF_kopp14_rcp60_2060ft$sf, col=RdGy[2], lwd=2)
SF_kopp14_rcp45_2060ft = plot.sf(kopp14_rcp45_2060ft, make.plot=FALSE)
lines(SF_kopp14_rcp45_2060ft$sf.num, SF_kopp14_rcp45_2060ft$sf, col=RdGy[3], lwd=2)
SF_kopp14_rcp26_2060ft = plot.sf(kopp14_rcp26_2060ft, make.plot=FALSE)
lines(SF_kopp14_rcp26_2060ft$sf.num, SF_kopp14_rcp26_2060ft$sf, col=RdGy[4], lwd=2)

SF_brickfd_rcp85_2060ft = plot.sf(brickfd_rcp85_2060ft, make.plot=FALSE)
lines(SF_brickfd_rcp85_2060ft$sf.num, SF_brickfd_rcp85_2060ft$sf, col=BrBG[11], lwd=2)
SF_brickfd_rcp60_2060ft = plot.sf(brickfd_rcp60_2060ft, make.plot=FALSE)
lines(SF_brickfd_rcp60_2060ft$sf.num, SF_brickfd_rcp60_2060ft$sf, col=BrBG[10], lwd=2)
SF_brickfd_rcp45_2060ft = plot.sf(brickfd_rcp45_2060ft, make.plot=FALSE)
lines(SF_brickfd_rcp45_2060ft$sf.num, SF_brickfd_rcp45_2060ft$sf, col=BrBG[9], lwd=2)
SF_brickfd_rcp26_2060ft = plot.sf(brickfd_rcp26_2060ft, make.plot=FALSE)
lines(SF_brickfd_rcp26_2060ft$sf.num, SF_brickfd_rcp26_2060ft$sf, col=BrBG[8], lwd=2)

SF_NO_fd_rcp85_2060ft = plot.sf(NO_fd_rcp85_2060ft, make.plot=FALSE)
lines(SF_NO_fd_rcp85_2060ft$sf.num, SF_NO_fd_rcp85_2060ft$sf, col=PRGn[2], lwd=2)
SF_NO_fd_rcp60_2060ft = plot.sf(NO_fd_rcp60_2060ft, make.plot=FALSE)
lines(SF_NO_fd_rcp60_2060ft$sf.num, SF_NO_fd_rcp60_2060ft$sf, col=PRGn[3], lwd=2)
SF_NO_fd_rcp45_2060ft = plot.sf(NO_fd_rcp45_2060ft, make.plot=FALSE)
lines(SF_NO_fd_rcp45_2060ft$sf.num, SF_NO_fd_rcp45_2060ft$sf, col=PRGn[4], lwd=2)
SF_NO_fd_rcp26_2060ft = plot.sf(NO_fd_rcp26_2060ft, make.plot=FALSE)
lines(SF_NO_fd_rcp26_2060ft$sf.num, SF_NO_fd_rcp26_2060ft$sf, col=PRGn[5], lwd=2)

#   -----------------------------------------------------------------------
plot(return_level, annual_exceed, log = "y", type = "n",bty="l",
     yaxt = 'n', cex=1, xlim = c(2.25, 12), ylim=c(0.011,1),
     ylab = "Probability of exceedance", xlab = "Storm surge (ft MSL)")
axis(2, at=10^(-4:0), label=parse(text=paste("10^", -4:0, sep="")), las=1)
title(main="e.", adj=0)

polygon(x = c(SF_Srikrishnan_nonstationary25$sf.num, rev(SF_Srikrishnan_nonstationary975$sf.num)), 
        y = c(SF_Srikrishnan_nonstationary25$sf, rev(SF_Srikrishnan_nonstationary975$sf)), col = trans_noaa_cols[7], 
        border = trans_noaa_cols[7], lty=3)

polygon(x = c(SF_Srikrishnan_stationary25$sf.num, rev(SF_Srikrishnan_stationary975$sf.num)), 
        y = c(SF_Srikrishnan_stationary25$sf, rev(SF_Srikrishnan_stationary975$sf)), col = trans_RdBu[9], border = NA)

polygon(x = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        y = c(1/tebaldi12$rp, rev(1/tebaldi12$rp)), col = trans_tebaldi_gold[2], border = NA)

polygon(x = c(zervas_2013$min_95, rev(zervas_2013$max_95)), 
        y = c(zervas_2013$aep, rev(zervas_2013$aep)), col = trans_BrBG[2], border = NA)
points(obs, 1/NOAA_methodGEV$return_obs, pch = 19)

lines(SF_Srikrishnan_nonstationary$sf.num, SF_Srikrishnan_nonstationary$sf, col=noaa_cols[2], lwd=2, lty=3)
lines(SF_Srikrishnan_stationary$sf.num, SF_Srikrishnan_stationary$sf, col=RdBu[11], lwd=2)
lines(NOAA_rl, 1/NOAA_rp, lwd=2, col=BrBG[2])
lines(tebaldi12$rl_50, 1/tebaldi12$rp, lty = 1, lwd = 2, col=tebaldi_gold[1])
points(USACE_EWL$feet[8:14], 1/USACE_rp, pch = 20, col=RdBu[10])

# ----------------------------------------------------------------------
# PLACE HOLDER
par(mgp=c(2,0.5,0), mar=c(3.5,3.5,1,1))
plot(return_level, annual_exceed, log = "y", type = "n",bty="l",
     yaxt = 'n', cex=1, xlim = c(2.25, 20), ylim=c(0.011,1),
     ylab = "Probability of exceedance", xlab = "Projected sea+surge level (ft)")
axis(2, at=10^(-4:0), label=parse(text=paste("10^", -4:0, sep="")), las=1)
title(main="f.", adj=0)

SF_k14_r85_2060_SSN = plot.sf(k14_r85_2060_SSN, make.plot=FALSE)
lines(SF_k14_r85_2060_SSN$sf.num, SF_k14_r85_2060_SSN$sf, col=RdGy[1], lwd=1.5, lty=3)

SF_k14_r85_2060_SS = plot.sf(k14_r85_2060_SS, make.plot=FALSE)
lines(SF_k14_r85_2060_SS$sf.num, SF_k14_r85_2060_SS$sf, col=RdGy[1], lwd=1.5)
SF_k14_r60_2060_SS = plot.sf(k14_r60_2060_SS, make.plot=FALSE)
lines(SF_k14_r60_2060_SS$sf.num, SF_k14_r60_2060_SS$sf, col=RdGy[2], lwd=1.5)
SF_k14_r45_2060_SS = plot.sf(k14_r45_2060_SS, make.plot=FALSE)
lines(SF_k14_r45_2060_SS$sf.num, SF_k14_r45_2060_SS$sf, col=RdGy[3], lwd=1.5)
SF_k14_r26_2060_SS = plot.sf(k14_r26_2060_SS, make.plot=FALSE)
lines(SF_k14_r26_2060_SS$sf.num, SF_k14_r26_2060_SS$sf, col=RdGy[4], lwd=1.5)

SF_bfd_r85_2060_SSN = plot.sf(bfd_r85_2060_SSN, make.plot=FALSE)
lines(SF_bfd_r85_2060_SSN$sf.num, SF_bfd_r85_2060_SSN$sf, col=BrBG[11], lwd=1.5, lty=3)

SF_bfd_r85_2060_SS = plot.sf(bfd_r85_2060_SS, make.plot=FALSE)
lines(SF_bfd_r85_2060_SS$sf.num, SF_bfd_r85_2060_SS$sf, col=BrBG[11], lwd=1.5)
SF_bfd_r60_2060_SS = plot.sf(bfd_r60_2060_SS, make.plot=FALSE)
lines(SF_bfd_r60_2060_SS$sf.num, SF_bfd_r60_2060_SS$sf, col=BrBG[10], lwd=1.5)
SF_bfd_r45_2060_SS = plot.sf(bfd_r45_2060_SS, make.plot=FALSE)
lines(SF_bfd_r45_2060_SS$sf.num, SF_bfd_r45_2060_SS$sf, col=BrBG[9], lwd=1.5)
SF_bfd_r26_2060_SS = plot.sf(bfd_r26_2060_SS, make.plot=FALSE)
lines(SF_bfd_r26_2060_SS$sf.num, SF_bfd_r26_2060_SS$sf, col=BrBG[8], lwd=1.5)

SF_NOfd_r85_2060_SSN = plot.sf(NOfd_r85_2060_SSN, make.plot=FALSE)
lines(SF_NOfd_r85_2060_SSN$sf.num, SF_NOfd_r85_2060_SSN$sf, col=PRGn[2], lwd=1.5, lty=3)

SF_NOfd_r85_2060_SS = plot.sf(NOfd_r85_2060_SS, make.plot=FALSE)
lines(SF_NOfd_r85_2060_SS$sf.num, SF_NOfd_r85_2060_SS$sf, col=PRGn[2], lwd=1.5)
SF_NOfd_r60_2060_SS = plot.sf(NOfd_r60_2060_SS, make.plot=FALSE)
lines(SF_NOfd_r60_2060_SS$sf.num, SF_NOfd_r60_2060_SS$sf, col=PRGn[3], lwd=1.5)
SF_NOfd_r45_2060_SS = plot.sf(NOfd_r45_2060_SS, make.plot=FALSE)
lines(SF_NOfd_r45_2060_SS$sf.num, SF_NOfd_r45_2060_SS$sf, col=PRGn[4], lwd=1.5)
SF_NOfd_r26_2060_SS = plot.sf(NOfd_r26_2060_SS, make.plot=FALSE)
lines(SF_NOfd_r26_2060_SS$sf.num, SF_NOfd_r26_2060_SS$sf, col=PRGn[5], lwd=1.5)

dev.off()

################################ 2100 #####################################
pdf(file="SLR_2100.pdf", family="Times", width=full_page_width, height=1.5*single_panel_height, pointsize=12)
layout(matrix(c(1,1,1,
                2,3,4,
                2,3,4,
                2,3,4,
                5,6,7,
                5,6,7,
                5,6,7), 7, 3, byrow = TRUE))
par(mgp=c(1.5,.5,0), mar=c(0,4,1,1))
plot(1, type="n", xlab="", ylab="", xlim=c(0, 10), ylim=c(0, 10), yaxt="n", xaxt="n", bty="n")
legend("topleft", legend=c("Wong & Keller\n2017 FD", "Kopp et al. 2014", 
                           "Wong & Keller\n2017 no FD", "Sweet et al. 2017", "Tebaldi et al. 2012\nexpected value", 
                           "Parris et al. 2012","Tebaldi et al. 2012\n95% CI", "USACE 2014", 
                           "Srikrishnan et al.\nin prep. S 95% CI", "Hall et al. 2016", "Srikrishnan et al.\nin prep. S",
                           "Observations"),
       lty=c(1,1,1,NA,1,NA,NA,NA,NA,NA,1,NA), lwd=c(2,2,2,NA,2,NA,NA,NA,NA,NA,2,NA), pch=c(NA,NA,NA,19,NA,19,22,19,22,19,NA,19), 
       col=c(BrBG[9], RdGy[3], PRGn[3], noaa_cols[5], tebaldi_gold[1],BrBG[2],"black", RdBu[10], "black", RdGy[9], RdBu[11], "black"),
       bty='n', ncol=6, pt.bg=c(NA,NA,NA,NA,NA,NA,tebaldi_gold[2],NA,trans_RdBu[9],NA,NA,NA), pt.cex = c(NA,NA,NA,1,NA,1,2,1,2,1,NA,1))
gradient.rect(7.5,1.5,9.5,3, col=col_grad, gradient="x")
arrows(9.25, 0.5, 9.5, 0.5, length=0.075)
text(8.5,0.5, "Higher scenario")

legend("bottomleft", legend=c("Srikrishnan et al.\nin prep. NS", "Srikrishnan et al.\nin prep. NS 95% CI",
                              "Zervas 2013\nexpected value", "Zervas 2013\n 95% CI"),
       lty=c(3,NA,1,NA), lwd=c(2,NA,2,NA), pch=c(NA,22,NA,22), 
       col=c(noaa_cols[2], "black", BrBG[2], "black"), bty='n', horiz=TRUE, 
       pt.bg=c(NA,trans_noaa_cols[7],NA,trans_BrBG[2]), pt.cex = c(NA,2,NA,2))

# Plot LSLR in 2100
par(mgp=c(1.5,.5,0), mar=c(3.5,4,1,1))
plot(density(kopp14_rcp85_2100ft), xlab="Projected sea level in 2100 (ft)", ylab="Probability density", yaxt="n", 
     main="", col=RdGy[1], lwd=2, xlim=c(-0.5,15), ylim = c(0, 1.5), bty="l")
title(main="a.", adj=0)
lines(density(kopp14_rcp60_2100ft), col=RdGy[2], lwd=2)
lines(density(kopp14_rcp45_2100ft), col=RdGy[3], lwd=2)
lines(density(kopp14_rcp26_2100ft), col=RdGy[4], lwd=2)

lines(density(brickfd_rcp85_2100ft), col=BrBG[11], lwd=2)
lines(density(brickfd_rcp60_2100ft), col=BrBG[10], lwd=2)
lines(density(brickfd_rcp45_2100ft), col=BrBG[9], lwd=2)
lines(density(brickfd_rcp26_2100ft), col=BrBG[8], lwd=2)

lines(density(NO_fd_rcp85_2100ft), col=PRGn[2], lwd=2)
lines(density(NO_fd_rcp60_2100ft), col=PRGn[3], lwd=2)
lines(density(NO_fd_rcp45_2100ft), col=PRGn[4], lwd=2)
lines(density(NO_fd_rcp26_2100ft), col=PRGn[5], lwd=2)

lines(noaa2012_2100, rep(1.45, 4), col=BrBG[4:1], lwd=2, lty=3)
lines(usace2013_2100, rep(1.3, 3), col=RdBu[9:11], lwd=2, lty=3)
lines(carswg2016_2100, rep(1.15, 5), col=RdGy[7:11], lwd=2, lty=3)
lines(noaa2017_2100_50, rep(1, length(noaa2017_2100_50)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012_2100, rep(1.45, 4), col=BrBG[4:1], pch=19)
points(usace2013_2100, rep(1.3, 3), col=RdBu[9:11], pch=19)
points(carswg2016_2100, rep(1.15, 5), col=RdGy[7:11], pch=19)
points(noaa2017_2100_50, rep(1, length(noaa2017_2100_50)), col=noaa_cols[8:2], pch=19)

#   -----------------------------------------------------------------------
plot(1/annual_exceed, return_level, log = "x", type = "n", xlim = c(1, 90),
     ylim = c(2, 13), 
     xaxt = 'n', cex=1, bty="l",
     xlab = "Return period (years)", 
     ylab = "Storm surge (ft MSL)")
title(main="b.", adj=0)
axis(1, lwd = 1, at=10^(seq(-1,log10(10^2), by = 1)), label=c(0.1, 1, 10, 100))

SF_Srikrishnan_nonstationary25 = plot.sf(nonstat_gev210025, make.plot=FALSE)
SF_Srikrishnan_nonstationary975 = plot.sf(nonstat_gev2100975, make.plot=FALSE)
polygon(y = c(SF_Srikrishnan_nonstationary25$sf.num, rev(SF_Srikrishnan_nonstationary975$sf.num)), 
        x = c(1/SF_Srikrishnan_nonstationary25$sf, rev(1/SF_Srikrishnan_nonstationary975$sf)), col = trans_noaa_cols[7], 
        border = trans_noaa_cols[7], lty=3)

SF_Srikrishnan_stationary25 = plot.sf(stat_gev25, make.plot=FALSE)
SF_Srikrishnan_stationary975 = plot.sf(stat_gev975, make.plot=FALSE)
polygon(y = c(SF_Srikrishnan_stationary25$sf.num, rev(SF_Srikrishnan_stationary975$sf.num)), 
        x = c(1/SF_Srikrishnan_stationary25$sf, rev(1/SF_Srikrishnan_stationary975$sf)), col = trans_RdBu[9], border = NA)

polygon(y = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        x = c(tebaldi12$rp, rev(tebaldi12$rp)), col = trans_tebaldi_gold[2], border = NA)

polygon(y = c(zervas_2013$min_95, rev(zervas_2013$max_95)), 
        x = c(1/zervas_2013$aep, rev(1/zervas_2013$aep)), col = trans_BrBG[2], border = NA)
points(NOAA_methodGEV$return_obs, obs, pch = 19)

SF_Srikrishnan_nonstationary = plot.sf(nonstat_gev2100, make.plot=FALSE)
lines(1/SF_Srikrishnan_nonstationary$sf, SF_Srikrishnan_nonstationary$sf.num, col=noaa_cols[2], lwd=2, lty=3)

SF_Srikrishnan_stationary = plot.sf(stat_gev, make.plot=FALSE)
lines(1/SF_Srikrishnan_stationary$sf, SF_Srikrishnan_stationary$sf.num, col=RdBu[11], lwd=2)

lines(NOAA_rp, NOAA_rl, lwd=2, col=BrBG[2])
lines(tebaldi12$rp, tebaldi12$rl_50, lty = 1, lwd = 2, col= tebaldi_gold[1])
points(USACE_rp, USACE_EWL$feet[8:14], pch = 20, col=RdBu[10])

# ----------------------------------------------------------------------
# PLACE HOLDER
par(mgp=c(1.5,0.5,0), mar=c(3.5,3.5,1,1))
plot(density(k14_r85_2100_SSN), xlab="Projected sea+surge level (ft)", ylab="Probability density", yaxt="n", type = "l", 
     main="", col=RdGy[1], lwd=2, lty=3, xlim=c(min(k14_r85_2100_SSN),max(bfd_r85_2100_SSN)), ylim=c(0, 0.6), bty="l")
title(main="c.", adj=0)

lines(density(k14_r85_2100_SS), col=RdGy[1], lwd=2)
lines(density(k14_r60_2100_SS), col=RdGy[2], lwd=2)
lines(density(k14_r45_2100_SS), col=RdGy[3], lwd=2)
lines(density(k14_r26_2100_SS), col=RdGy[4], lwd=2)

lines(density(bfd_r85_2100_SSN), col=BrBG[11], lwd=2, lty=3)
lines(density(bfd_r85_2100_SS), col=BrBG[11], lwd=2)
lines(density(bfd_r60_2100_SS), col=BrBG[10], lwd=2)
lines(density(bfd_r45_2100_SS), col=BrBG[9], lwd=2)
lines(density(bfd_r26_2100_SS), col=BrBG[8], lwd=2)

lines(density(NOfd_r85_2100_SSN), col=PRGn[2], lwd=2, lty=3)
lines(density(NOfd_r85_2100_SS), col=PRGn[2], lwd=2)
lines(density(NOfd_r60_2100_SS), col=PRGn[3], lwd=2)
lines(density(NOfd_r45_2100_SS), col=PRGn[4], lwd=2)
lines(density(NOfd_r26_2100_SS), col=PRGn[5], lwd=2)

# SF ----------------------------------------------------------------------
par(mgp=c(2,0.5,0), mar=c(3.5,4,1,1))
plot.sf(kopp14_rcp85_2100ft, ylab = "Probability of exceedance", xlab = "Projected sea level in 2100 (ft)",
        yaxt = "n", yaxs = 'i', typ="l", lwd=2, lty=1, bty="l",
        ylim = c(10^-4, 10^0+0.25), col=RdGy[1])
title(main="d.", adj=0)

SF_kopp14_rcp60_2100ft = plot.sf(kopp14_rcp60_2100ft, make.plot=FALSE)
lines(SF_kopp14_rcp60_2100ft$sf.num, SF_kopp14_rcp60_2100ft$sf, col=RdGy[2], lwd=2)
SF_kopp14_rcp45_2100ft = plot.sf(kopp14_rcp45_2100ft, make.plot=FALSE)
lines(SF_kopp14_rcp45_2100ft$sf.num, SF_kopp14_rcp45_2100ft$sf, col=RdGy[3], lwd=2)
SF_kopp14_rcp26_2100ft = plot.sf(kopp14_rcp26_2100ft, make.plot=FALSE)
lines(SF_kopp14_rcp26_2100ft$sf.num, SF_kopp14_rcp26_2100ft$sf, col=RdGy[4], lwd=2)

SF_brickfd_rcp85_2100ft = plot.sf(brickfd_rcp85_2100ft, make.plot=FALSE)
lines(SF_brickfd_rcp85_2100ft$sf.num, SF_brickfd_rcp85_2100ft$sf, col=BrBG[11], lwd=2)
SF_brickfd_rcp60_2100ft = plot.sf(brickfd_rcp60_2100ft, make.plot=FALSE)
lines(SF_brickfd_rcp60_2100ft$sf.num, SF_brickfd_rcp60_2100ft$sf, col=BrBG[10], lwd=2)
SF_brickfd_rcp45_2100ft = plot.sf(brickfd_rcp45_2100ft, make.plot=FALSE)
lines(SF_brickfd_rcp45_2100ft$sf.num, SF_brickfd_rcp45_2100ft$sf, col=BrBG[9], lwd=2)
SF_brickfd_rcp26_2100ft = plot.sf(brickfd_rcp26_2100ft, make.plot=FALSE)
lines(SF_brickfd_rcp26_2100ft$sf.num, SF_brickfd_rcp26_2100ft$sf, col=BrBG[8], lwd=2)

SF_NO_fd_rcp85_2100ft = plot.sf(NO_fd_rcp85_2100ft, make.plot=FALSE)
lines(SF_NO_fd_rcp85_2100ft$sf.num, SF_NO_fd_rcp85_2100ft$sf, col=PRGn[2], lwd=2)
SF_NO_fd_rcp60_2100ft = plot.sf(NO_fd_rcp60_2100ft, make.plot=FALSE)
lines(SF_NO_fd_rcp60_2100ft$sf.num, SF_NO_fd_rcp60_2100ft$sf, col=PRGn[3], lwd=2)
SF_NO_fd_rcp45_2100ft = plot.sf(NO_fd_rcp45_2100ft, make.plot=FALSE)
lines(SF_NO_fd_rcp45_2100ft$sf.num, SF_NO_fd_rcp45_2100ft$sf, col=PRGn[4], lwd=2)
SF_NO_fd_rcp26_2100ft = plot.sf(NO_fd_rcp26_2100ft, make.plot=FALSE)
lines(SF_NO_fd_rcp26_2100ft$sf.num, SF_NO_fd_rcp26_2100ft$sf, col=PRGn[5], lwd=2)

#   -----------------------------------------------------------------------
plot(return_level, annual_exceed, log = "y", type = "n",bty="l",
     yaxt = 'n', cex=1, xlim = c(2, 13), ylim=c(0.011,1),
     ylab = "Probability of exceedance", xlab = "Storm surge (ft MSL)")
axis(2, at=10^(-4:0), label=parse(text=paste("10^", -4:0, sep="")), las=1)
title(main="e.", adj=0)

polygon(x = c(SF_Srikrishnan_nonstationary25$sf.num, rev(SF_Srikrishnan_nonstationary975$sf.num)), 
        y = c(SF_Srikrishnan_nonstationary25$sf, rev(SF_Srikrishnan_nonstationary975$sf)), col = trans_noaa_cols[7], 
        border = trans_noaa_cols[7], lty=3)

polygon(x = c(SF_Srikrishnan_stationary25$sf.num, rev(SF_Srikrishnan_stationary975$sf.num)), 
        y = c(SF_Srikrishnan_stationary25$sf, rev(SF_Srikrishnan_stationary975$sf)), col = trans_RdBu[9], border = NA)

polygon(x = c(tebaldi12$rl_025, rev(tebaldi12$rl_975)), 
        y = c(1/tebaldi12$rp, rev(1/tebaldi12$rp)), col = trans_tebaldi_gold[2], border = NA)

polygon(x = c(zervas_2013$min_95, rev(zervas_2013$max_95)), 
        y = c(zervas_2013$aep, rev(zervas_2013$aep)), col = trans_BrBG[2], border = NA)
points(obs, 1/NOAA_methodGEV$return_obs, pch = 19)

lines(SF_Srikrishnan_nonstationary$sf.num, SF_Srikrishnan_nonstationary$sf, col=noaa_cols[2], lwd=2, lty=3)
lines(SF_Srikrishnan_stationary$sf.num, SF_Srikrishnan_stationary$sf, col=RdBu[11], lwd=2)
lines(NOAA_rl, 1/NOAA_rp, lwd=2, col=BrBG[2])
lines(tebaldi12$rl_50, 1/tebaldi12$rp, lty = 1, lwd = 2, col=tebaldi_gold[1])
points(USACE_EWL$feet[8:14], 1/USACE_rp, pch = 20, col=RdBu[10])

# ----------------------------------------------------------------------
# PLACE HOLDER
par(mgp=c(2,0.5,0), mar=c(3.5,3.5,1,1))
plot(return_level, annual_exceed, log = "y", type = "n",bty="l",
     yaxt = 'n', cex=1, xlim = c(2, 25), ylim=c(0.011,1),
     ylab = "Probability of exceedance", xlab = "Projected sea+surge level (ft)")
axis(2, at=10^(-4:0), label=parse(text=paste("10^", -4:0, sep="")), las=1)
title(main="f.", adj=0)

SF_k14_r85_2100_SSN = plot.sf(k14_r85_2100_SSN, make.plot=FALSE)
lines(SF_k14_r85_2100_SSN$sf.num, SF_k14_r85_2100_SSN$sf, col=RdGy[1], lwd=1.5, lty=3)

SF_k14_r85_2100_SS = plot.sf(k14_r85_2100_SS, make.plot=FALSE)
lines(SF_k14_r85_2100_SS$sf.num, SF_k14_r85_2100_SS$sf, col=RdGy[1], lwd=1.5)
SF_k14_r60_2100_SS = plot.sf(k14_r60_2100_SS, make.plot=FALSE)
lines(SF_k14_r60_2100_SS$sf.num, SF_k14_r60_2100_SS$sf, col=RdGy[2], lwd=1.5)
SF_k14_r45_2100_SS = plot.sf(k14_r45_2100_SS, make.plot=FALSE)
lines(SF_k14_r45_2100_SS$sf.num, SF_k14_r45_2100_SS$sf, col=RdGy[3], lwd=1.5)
SF_k14_r26_2100_SS = plot.sf(k14_r26_2100_SS, make.plot=FALSE)
lines(SF_k14_r26_2100_SS$sf.num, SF_k14_r26_2100_SS$sf, col=RdGy[4], lwd=1.5)

SF_bfd_r85_2100_SSN = plot.sf(bfd_r85_2100_SSN, make.plot=FALSE)
lines(SF_bfd_r85_2100_SSN$sf.num, SF_bfd_r85_2100_SSN$sf, col=BrBG[11], lwd=1.5, lty=3)

SF_bfd_r85_2100_SS = plot.sf(bfd_r85_2100_SS, make.plot=FALSE)
lines(SF_bfd_r85_2100_SS$sf.num, SF_bfd_r85_2100_SS$sf, col=BrBG[11], lwd=1.5)
SF_bfd_r60_2100_SS = plot.sf(bfd_r60_2100_SS, make.plot=FALSE)
lines(SF_bfd_r60_2100_SS$sf.num, SF_bfd_r60_2100_SS$sf, col=BrBG[10], lwd=1.5)
SF_bfd_r45_2100_SS = plot.sf(bfd_r45_2100_SS, make.plot=FALSE)
lines(SF_bfd_r45_2100_SS$sf.num, SF_bfd_r45_2100_SS$sf, col=BrBG[9], lwd=1.5)
SF_bfd_r26_2100_SS = plot.sf(bfd_r26_2100_SS, make.plot=FALSE)
lines(SF_bfd_r26_2100_SS$sf.num, SF_bfd_r26_2100_SS$sf, col=BrBG[8], lwd=1.5)

SF_NOfd_r85_2100_SSN = plot.sf(NOfd_r85_2100_SSN, make.plot=FALSE)
lines(SF_NOfd_r85_2100_SSN$sf.num, SF_NOfd_r85_2100_SSN$sf, col=PRGn[2], lwd=1.5, lty=3)

SF_NOfd_r85_2100_SS = plot.sf(NOfd_r85_2100_SS, make.plot=FALSE)
lines(SF_NOfd_r85_2100_SS$sf.num, SF_NOfd_r85_2100_SS$sf, col=PRGn[2], lwd=1.5)
SF_NOfd_r60_2100_SS = plot.sf(NOfd_r60_2100_SS, make.plot=FALSE)
lines(SF_NOfd_r60_2100_SS$sf.num, SF_NOfd_r60_2100_SS$sf, col=PRGn[3], lwd=1.5)
SF_NOfd_r45_2100_SS = plot.sf(NOfd_r45_2100_SS, make.plot=FALSE)
lines(SF_NOfd_r45_2100_SS$sf.num, SF_NOfd_r45_2100_SS$sf, col=PRGn[4], lwd=1.5)
SF_NOfd_r26_2100_SS = plot.sf(NOfd_r26_2100_SS, make.plot=FALSE)
lines(SF_NOfd_r26_2100_SS$sf.num, SF_NOfd_r26_2100_SS$sf, col=PRGn[5], lwd=1.5)

dev.off()

################################ 2100 #####################################
# 2030
denskopp14_rcp85_2030 <- density(kopp14_rcp85_2030ft)
denskopp14_rcp60_2030 <- density(kopp14_rcp60_2030ft)
denskopp14_rcp45_2030 <- density(kopp14_rcp45_2030ft)
denskopp14_rcp26_2030 <- density(kopp14_rcp26_2030ft)
densbrickfd_rcp85_2030 <- density(brickfd_rcp85_2030ft)
densbrickfd_rcp60_2030 <- density(brickfd_rcp60_2030ft)
densbrickfd_rcp45_2030 <- density(brickfd_rcp45_2030ft)
densbrickfd_rcp26_2030 <- density(brickfd_rcp26_2030ft)
densNO_fd_rcp85_2030 <- density(NO_fd_rcp85_2030ft)
densNO_fd_rcp60_2030 <- density(NO_fd_rcp60_2030ft)
densNO_fd_rcp45_2030 <- density(NO_fd_rcp45_2030ft)
densNO_fd_rcp26_2030 <- density(NO_fd_rcp26_2030ft)
# 2050
denskopp14_rcp85_2050 <- density(kopp14_rcp85_2050ft)
denskopp14_rcp60_2050 <- density(kopp14_rcp60_2050ft)
denskopp14_rcp45_2050 <- density(kopp14_rcp45_2050ft)
denskopp14_rcp26_2050 <- density(kopp14_rcp26_2050ft)
densbrickfd_rcp85_2050 <- density(brickfd_rcp85_2050ft)
densbrickfd_rcp60_2050 <- density(brickfd_rcp60_2050ft)
densbrickfd_rcp45_2050 <- density(brickfd_rcp45_2050ft)
densbrickfd_rcp26_2050 <- density(brickfd_rcp26_2050ft)
densNO_fd_rcp85_2050 <- density(NO_fd_rcp85_2050ft)
densNO_fd_rcp60_2050 <- density(NO_fd_rcp60_2050ft)
densNO_fd_rcp45_2050 <- density(NO_fd_rcp45_2050ft)
densNO_fd_rcp26_2050 <- density(NO_fd_rcp26_2050ft)
# 2060
denskopp14_rcp85_2060 <- density(kopp14_rcp85_2060ft)
denskopp14_rcp60_2060 <- density(kopp14_rcp60_2060ft)
denskopp14_rcp45_2060 <- density(kopp14_rcp45_2060ft)
denskopp14_rcp26_2060 <- density(kopp14_rcp26_2060ft)
densbrickfd_rcp85_2060 <- density(brickfd_rcp85_2060ft)
densbrickfd_rcp60_2060 <- density(brickfd_rcp60_2060ft)
densbrickfd_rcp45_2060 <- density(brickfd_rcp45_2060ft)
densbrickfd_rcp26_2060 <- density(brickfd_rcp26_2060ft)
densNO_fd_rcp85_2060 <- density(NO_fd_rcp85_2060ft)
densNO_fd_rcp60_2060 <- density(NO_fd_rcp60_2060ft)
densNO_fd_rcp45_2060 <- density(NO_fd_rcp45_2060ft)
densNO_fd_rcp26_2060 <- density(NO_fd_rcp26_2060ft)
# 2100
denskopp14_rcp85_2100 <- density(kopp14_rcp85_2100ft)
denskopp14_rcp60_2100 <- density(kopp14_rcp60_2100ft)
denskopp14_rcp45_2100 <- density(kopp14_rcp45_2100ft)
denskopp14_rcp26_2100 <- density(kopp14_rcp26_2100ft)
densbrickfd_rcp85_2100 <- density(brickfd_rcp85_2100ft)
densbrickfd_rcp60_2100 <- density(brickfd_rcp60_2100ft)
densbrickfd_rcp45_2100 <- density(brickfd_rcp45_2100ft)
densbrickfd_rcp26_2100 <- density(brickfd_rcp26_2100ft)
densNO_fd_rcp85_2100 <- density(NO_fd_rcp85_2100ft)
densNO_fd_rcp60_2100 <- density(NO_fd_rcp60_2100ft)
densNO_fd_rcp45_2100 <- density(NO_fd_rcp45_2100ft)
densNO_fd_rcp26_2100 <- density(NO_fd_rcp26_2100ft)

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

lines(noaa2012_2100, rep(2.95, 4), col=trans_BrBG[4:1], lwd=1.5, lty=3)
lines(usace2013_2100, rep(2.45, 3), col=trans_RdBu[9:11], lwd=1.5, lty=3)
lines(carswg2016_2100, rep(1.95, 5), col=trans_RdGy[7:11], lwd=1.5, lty=3)
lines(noaa2017_2100_50, rep(1.45, length(noaa2017_2100_50)), col=trans_noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012_2100, rep(2.95, 4), col=trans_BrBG[4:1], pch=20)
points(usace2013_2100, rep(2.45, 3), col=trans_RdBu[9:11], pch=20)
points(carswg2016_2100, rep(1.95, 5), col=trans_RdGy[7:11], pch=20)
points(noaa2017_2100_50, rep(1.45, length(noaa2017_2100_50)), col=trans_noaa_cols[8:2], pch=20)

# lines(noaa2012_2100, rep(4.75, 4), col=trans_BrBG[4:1], lwd=1.5, lty=3)
# lines(usace2013_2100, rep(4.25, 3), col=trans_RdBu[9:11], lwd=1.5, lty=3)
# lines(carswg2016_2100, rep(3.75, 5), col=trans_RdGy[7:11], lwd=1.5, lty=3)
# lines(noaa2017_2100_50, rep(3.25, length(noaa2017_2100_50)), col=trans_noaa_cols[8:2], lwd=1.5, lty=3)
# 
# points(noaa2012_2100, rep(4.75, 4), col=trans_BrBG[4:1], pch=20)
# points(usace2013_2100, rep(4.25, 3), col=trans_RdBu[9:11], pch=20)
# points(carswg2016_2100, rep(3.75, 5), col=trans_RdGy[7:11], pch=20)
# points(noaa2017_2100_50, rep(3.25, length(noaa2017_2100_50)), col=trans_noaa_cols[8:2], pch=20)
# 
# #2060
# abline(h=5, col="lightgray")
# text(15, 5.5, "2060", col="lightgray")
#2060
abline(h=3.2, col="lightgray")
text(15, 3.7, "2060", col="lightgray")

lines(x=denskopp14_rcp85_2060$x, y=(denskopp14_rcp85_2060$y+3.2), col=trans_RdGy[1], lwd=1.5)
lines(x=denskopp14_rcp60_2060$x, y=(denskopp14_rcp60_2060$y+3.2), col=trans_RdGy[2], lwd=1.5)
lines(x=denskopp14_rcp45_2060$x, y=(denskopp14_rcp45_2060$y+3.2), col=trans_RdGy[3], lwd=1.5)
lines(x=denskopp14_rcp26_2060$x, y=(denskopp14_rcp26_2060$y+3.2), col=trans_RdGy[4], lwd=1.5)

lines(x=densbrickfd_rcp85_2060$x, y=(densbrickfd_rcp85_2060$y+3.2), col=trans_BrBG[11], lwd=1.5)
lines(x=densbrickfd_rcp60_2060$x, y=(densbrickfd_rcp60_2060$y+3.2), col=trans_BrBG[10], lwd=1.5)
lines(x=densbrickfd_rcp45_2060$x, y=(densbrickfd_rcp45_2060$y+3.2), col=trans_BrBG[9], lwd=1.5)
lines(x=densbrickfd_rcp26_2060$x, y=(densbrickfd_rcp26_2060$y+3.2), col=trans_BrBG[8], lwd=1.5)

lines(x=densNO_fd_rcp85_2060$x, y=(densNO_fd_rcp85_2060$y+3.2), col=trans_PRGn[2], lwd=1.5)
lines(x=densNO_fd_rcp60_2060$x, y=(densNO_fd_rcp60_2060$y+3.2), col=trans_PRGn[3], lwd=1.5)
lines(x=densNO_fd_rcp45_2060$x, y=(densNO_fd_rcp45_2060$y+3.2), col=trans_PRGn[4], lwd=1.5)
lines(x=densNO_fd_rcp26_2060$x, y=(densNO_fd_rcp26_2060$y+3.2), col=trans_PRGn[5], lwd=1.5)

lines(noaa2012_2060, rep(7.2, 4), col=trans_BrBG[4:1], lwd=1.5, lty=3)
lines(usace2013_2060, rep(6.7, 3), col=trans_RdBu[9:11], lwd=1.5, lty=3)
lines(carswg2016_2060, rep(6.2, 5), col=trans_RdGy[7:11], lwd=1.5, lty=3)
lines(noaa2017_2060_50, rep(5.7, length(noaa2017_2060_50)), col=trans_noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012_2060, rep(7.2, 4), col=trans_BrBG[4:1], pch=20)
points(usace2013_2060, rep(6.7, 3), col=trans_RdBu[9:11], pch=20)
points(carswg2016_2060, rep(6.2, 5), col=trans_RdGy[7:11], pch=20)
points(noaa2017_2060_50, rep(5.7, length(noaa2017_2060_50)), col=trans_noaa_cols[8:2], pch=20)

# lines(noaa2012_2060, rep(10, 4), col=trans_BrBG[4:1], lwd=1.5, lty=3)
# lines(usace2013_2060, rep(9.5, 3), col=trans_RdBu[9:11], lwd=1.5, lty=3)
# lines(carswg2016_2060, rep(9, 5), col=trans_RdGy[7:11], lwd=1.5, lty=3)
# lines(noaa2017_2060_50, rep(8.5, length(noaa2017_2060_50)), col=trans_noaa_cols[8:2], lwd=1.5, lty=3)
# 
# points(noaa2012_2060, rep(10, 4), col=trans_BrBG[4:1], pch=20)
# points(usace2013_2060, rep(9.5, 3), col=trans_RdBu[9:11], pch=20)
# points(carswg2016_2060, rep(9, 5), col=trans_RdGy[7:11], pch=20)
# points(noaa2017_2060_50, rep(8.5, length(noaa2017_2060_50)), col=trans_noaa_cols[8:2], pch=20)
# 
# #2050
# abline(h=10.25, col="#5f4b3a")
# text(15, 10.75, "2050", col="#5f4b3a")
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

lines(noaa2012_2050, rep(11.7, 4), col=BrBG[4:1], lwd=1.5, lty=3)
lines(usace2013_2050, rep(11.2, 3), col=RdBu[9:11], lwd=1.5, lty=3)
lines(carswg2016_2050, rep(10.7, 5), col=RdGy[7:11], lwd=1.5, lty=3)
lines(noaa2017_2050_50, rep(10.2, length(noaa2017_2050_50)), col=noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012_2050, rep(11.7, 4), col=BrBG[4:1], pch=20)
points(usace2013_2050, rep(11.2, 3), col=RdBu[9:11], pch=20)
points(carswg2016_2050, rep(10.7, 5), col=RdGy[7:11], pch=20)
points(noaa2017_2050_50, rep(10.2, length(noaa2017_2050_50)), col=noaa_cols[8:2], pch=20)

# lines(noaa2012_2050, rep(14.75, 4), col=BrBG[4:1], lwd=1.5, lty=3)
# lines(usace2013_2050, rep(14.25, 3), col=RdBu[9:11], lwd=1.5, lty=3)
# lines(carswg2016_2050, rep(13.75, 5), col=RdGy[7:11], lwd=1.5, lty=3)
# lines(noaa2017_2050_50, rep(13.25, length(noaa2017_2050_50)), col=noaa_cols[8:2], lwd=1.5, lty=3)
# 
# points(noaa2012_2050, rep(14.75, 4), col=BrBG[4:1], pch=20)
# points(usace2013_2050, rep(14.25, 3), col=RdBu[9:11], pch=20)
# points(carswg2016_2050, rep(13.75, 5), col=RdGy[7:11], pch=20)
# points(noaa2017_2050_50, rep(13.25, length(noaa2017_2050_50)), col=noaa_cols[8:2], pch=20)
# 
# #2030
# abline(h=15, col="lightgray")
# text(15, 15.5, "2030", col="lightgray")
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

lines(noaa2012_2030, rep(18.45, 4), col=trans_BrBG[4:1], lwd=1.5, lty=3)
lines(usace2013_2030, rep(17.95, 3), col=trans_RdBu[9:11], lwd=1.5, lty=3)
lines(carswg2016_2030, rep(17.45, 5), col=trans_RdGy[7:11], lwd=1.5, lty=3)
lines(noaa2017_2030_50, rep(16.95, length(noaa2017_2030_50)), col=trans_noaa_cols[8:2], lwd=1.5, lty=3)

points(noaa2012_2030, rep(18.45, 4), col=trans_BrBG[4:1], pch=20)
points(usace2013_2030, rep(17.95, 3), col=trans_RdBu[9:11], pch=20)
points(carswg2016_2030, rep(17.45, 5), col=trans_RdGy[7:11], pch=20)
points(noaa2017_2030_50, rep(16.95, length(noaa2017_2030_50)), col=trans_noaa_cols[8:2], pch=20)

# lines(noaa2012_2030, rep(18, 4), col=trans_BrBG[4:1], lwd=1.5, lty=3)
# lines(usace2013_2030, rep(17.5, 3), col=trans_RdBu[9:11], lwd=1.5, lty=3)
# lines(carswg2016_2030, rep(17, 5), col=trans_RdGy[7:11], lwd=1.5, lty=3)
# lines(noaa2017_2030_50, rep(16.5, length(noaa2017_2030_50)), col=trans_noaa_cols[8:2], lwd=1.5, lty=3)
# 
# points(noaa2012_2030, rep(18, 4), col=trans_BrBG[4:1], pch=20)
# points(usace2013_2030, rep(17.5, 3), col=trans_RdBu[9:11], pch=20)
# points(carswg2016_2030, rep(17, 5), col=trans_RdGy[7:11], pch=20)
# points(noaa2017_2030_50, rep(16.5, length(noaa2017_2030_50)), col=trans_noaa_cols[8:2], pch=20)

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
