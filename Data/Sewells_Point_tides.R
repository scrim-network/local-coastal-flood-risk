##==============================================================================
## file: Sewells_Point_tides.R
##
## Edits by Kelsey Ruckert (klr324@psu.edu) Nov. 2 2016
## to do a GEV Analysis of tide gauge data from Norfolk, VA (Sewells Point).
##
## Contact Kelsey Ruckert (klr324@psu.edu) regarding questions.
##
## Original code files include:
## https://github.com/pcoddo/VanDantzig/blob/master/Model_Versions/Uncertainty_SLR_GEV/Storm_Surge_Module/tide_gauge.R
## https://github.com/scrim-network/Ruckertetal_SFB_SLR_2016/blob/master/San_francisco_tides.R
##==============================================================================
## Copyright 2015 Kelsey Ruckert, Perry Oddo
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

# Clear environment and graphics
rm(list = ls())
#graphics.off()

# Install and load required libraries
# install.packages("extRemes")
# install.packages("fExtremes")
# install.packages("ismev")
# install.packages("lubridate")
# install.packages("zoo")
library(extRemes)
library(fExtremes)
library(ismev)
library(lubridate)
library(zoo)

# Read in tide gauge data
data_1 = read.table("1928-01-01_SewellsPoint_1942-08-25.txt", header = TRUE, sep = '\t')
data_2 = read.table("1943-09-15_SewellsPoint_2015-12-31.txt", header = TRUE, sep = '\t')
data = rbind(data_1,data_2)
data$sl <- data$sl * 100

# Define function to aggregate data by years
as.year <- function(x) {floor(as.numeric(as.yearmon(x)))}

# Format data - create Datetime column and reference columns for each year in dataset
# date.time column used to create unique index for each entry 
data$date2 <- as.Date(as.character(data$date), format="%Y%m%d")
data$date.time <- as.POSIXct(paste(data$date, data$time), format = "%Y%m%d %H:%M", "GMT")
all(!duplicated(data$date.time))             # Are there duplicates? TRUE means no duplicates
# which(duplicated(data$date.time) == TRUE)  # if False uncomment

data$year.id <- as.numeric(as.factor(with(data, as.year(paste(date2)))))

# Create zoo object for tide gauge data for entire time series, ordered by date.time
sl <- zoo(data$sl, order.by=data$date.time)

# Aggregate annual block maximas and mean values
year.max <- aggregate(sl, as.year, max)
year.mean <- aggregate(sl, as.year, mean)

# Create data frame for block maxima, means, and index years
annual <- data.frame(index(year.max), coredata(year.mean), coredata(year.max))
annual$year.id <- index(annual[,1])

# Find residuals of annual means
# Create data frame to index and match annual means based on year.id
annual.res <- merge(data[, c("year.id", "sl")], annual[, c("year.id", "coredata.year.mean.")])
annual.res$date <- as.year(data$date2)
annual.res$residual <- annual.res$sl - annual.res$coredata.year.mean.   # Residuals of Annual Means
annual.res$date.time <- data$date.time   # Unique index for residuals

# Create zoo obhect for residuals and aggregate block maximas
# year.res.zoo are 'detrended' sea levels
year.res.zoo <- zoo(annual.res$residual, order.by = annual.res$date.time)
year.res.max <- aggregate(year.res.zoo, as.year, max)

# Save object for MCMC analysis
MCMC_coredata <- coredata(year.res.max)
# save(MCMC_coredata, file = "Sewells_coredata.RData")

### Fit GEV of residuals ###
year.res.max.fit <- fevd(coredata(year.res.max))   # extRemes package
year.res.max.fit2 <- gevFit(coredata(year.res.max))   # fExtremes package
year.res.max.fit3 <- gev.fit(coredata(year.res.max), show = FALSE)   # ismev package

# Print GEV estimates
print(year.res.max.fit2@fit$par.ests)
# xi          mu        beta 
# 0.2031878 110.4028132  15.3330898

# Determine return levels using maximum likelihood estimate (95% confidence interval):
year.return <- return.level(year.res.max.fit, return.period = 10000, do.ci = TRUE)

# Print estimates of 2:100000-year flood, with confidence intervals
print(year.return)

# save.image(file = "Sewells_stormSurge.RData")

#------------------- Estimate current storm surge -------------------------------------
storm_surgeL <- 1e5 # desired storm surge level
q = seq(0,1,length.out= storm_surgeL +1)  # quantile array

# Find closed-form solution of GEV fit
fit_q_year = qgev(q, year.res.max.fit2@fit$par.ests[1], year.res.max.fit2@fit$par.ests[2],
year.res.max.fit2@fit$par.ests[3])
fit_q_year = fit_q_year[fit_q_year< max(fit_q_year)]

# Find which q is the 100-yr value
num = which(q <= 0.99)
num.max = which.max(num)
year100prob <- num.max +1
# check to make sure the year100prob value is the 100-yr value (10^-2)
print(1-q[year100prob])

return_level = fit_q_year/100
aep = 1-q[1:storm_surgeL]

## EXPORT DATA TO CSV FILE ##
# ------------------------------
# Data (block_maxima) should be a numeric vector of block maxima tide observations
# max_return_period should be the maximum number of years returned 
# e.g. 10^4 for a 1/10,000 year return level)
block_maxima = coredata(year.res.max)/100
max_return_period = 10^4
  
fit.obj <- gev.fit(block_maxima, show = FALSE)
  
a <- fit.obj$mle
mat <- fit.obj$cov
dat <- fit.obj$data
  
eps <- 1e-06; a1 <- a; a2 <- a; a3 <- a
a1[1] <- a[1] + eps; a2[2] <- a[2] + eps; a3[3] <- a[3] + eps
f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5,
       0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999, 0.9999, 0.99999, 0.999999)
quant <- gevq(a, 1 - f[which((1/(1-f)) <= (max_return_period+0.01))])
d <- t(gev.rl.gradient(a = a, p = 1 - f))
v <- apply(d, 1, q.form, m = mat)

return_p = 1/(1-(1:length(dat)/(length(dat) + 1)))
obs = sort(dat)
return_pf = 1/(1-(f[1:length(quant)]))
min_f = quant - 1.96 * sqrt(v[1:length(quant)])
max_f = quant + 1.96 * sqrt(v[1:length(quant)])
mle_f = quant
  
# Append NAs to make each vector the same length
return_p = c(return_p, rep(NA, length(aep) - length(return_p)))
obs = c(obs, rep(NA, length(aep) - length(obs)))
return_pf = c(return_pf, rep(NA, length(aep) - length(return_pf)))
min_f = c(min_f, rep(NA, length(aep) - length(min_f)))
max_f = c(max_f, rep(NA, length(aep) - length(max_f)))
mle_f = c(mle_f, rep(NA, length(aep) - length(mle_f)))

# Create data frame and same to a csv file.
framed = data.frame(return_period = return_pf, min_level = min_f, mle_level = mle_f, max_level = max_f, 
                    obs = obs, return_obs = return_p, aep = aep, return_level = return_level)

write.csv(framed, file="NOAA_method_stormsurge_sewellspoint.csv")

## QUICK TEST PLOTS ##
# ------------------------------
library(RColorBrewer)
myheatcolors <- brewer.pal(9,"YlOrRd")

source("../R/Helper_scripts/plot_sf.r")

# Figure dimensions
text_column_width   = 5.2
minimum_width       = 2.63
full_page_width     = 7.5
full_page_height    = 8.75
single_panel_height = 4
# -----------------------------

# Plot current storm surge frequency with observations
pdf(file="../Figures/SewellsPoint_Survival.pdf", family="Times", width=text_column_width, height=single_panel_height, pointsize=11)
par(mfrow=c(1,1), mgp=c(1.5,.5,0),mar=c(4, 3, 1, 2))
plot.sf(coredata(year.res.max)/100, pch = 21, bg = "black",
        ylab = "Survival function [1 - cdf]",
        xlab = "Return level (m)",sub="Sewells Point, VA",
        yaxt = "n", yaxs = 'i',
        ylim = c(10^-2.5, 10^0+0.25),
        xlim = c((fit_q_year[1]/100), 2.75))

lines(fit_q_year/100, 1-q[1:storm_surgeL], type="l",lwd=2, col = myheatcolors[3])
legend("topright",
       c("Annual block maxima\nobservations", "Best estimate"),
       col = c("black", myheatcolors[3]),
       pt.bg = c("black", NA),
       pch = c(21, NA),
       lty = c(NA, 1), cex=1,
       lwd = c(1.5, 1.5),
       bty = 'n',
       inset = c(0.01, -0.01))
dev.off()

pdf(file="../Figures/SewellsPoint_StormSurge.pdf", family="Times", width=text_column_width, height=single_panel_height*2, pointsize=11)
par(mfrow=c(2,1), mgp=c(1.5,.5,0),mar=c(4, 3, 1, 2))
plot(index(year.res.max), coredata(year.res.max)/100, pch=20, type = "b", col = "black",#, xaxs = 'i',
     ylab = "Annual block maxima (m)", xlab = "Year", lwd=1.5)
box(lwd = 1)

return_level_plot <- function(block_maxima, max_return_period, legend)
{
  # Data (block_maxima) should be a numeric vector of block maxima tide observations
  # max_return_period should be the maximum number of years returned on plot
  # e.g. 10^4 for a 1/10,000 year return level)
  # legend = TRUE to plot legend
  
  require(ismev)
  fit.obj <- gev.fit(block_maxima, show = FALSE)
  
  a <- fit.obj$mle
  mat <- fit.obj$cov
  dat <- fit.obj$data
  
  eps <- 1e-06; a1 <- a; a2 <- a; a3 <- a
  a1[1] <- a[1] + eps; a2[2] <- a[2] + eps; a3[3] <- a[3] + eps
  f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5,
         0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999, 0.9999, 0.99999, 0.999999)
  q <- gevq(a, 1 - f[which((1/(1-f)) <= (max_return_period+0.01))])
  d <- t(gev.rl.gradient(a = a, p = 1 - f))
  v <- apply(d, 1, q.form, m = mat)
  plot(1/(1-(f[1:length(q)])), q, log = "x", type = "n", xlim = c(0.8, 10^3),
       ylim = c(min(dat, q), 4), #c(min(dat, q), max(dat, q + 1.96 * sqrt(v[1:length(q)]))), 
       xaxt = 'n', cex=1,
       xlab = "Return period (years)", 
       ylab = "Return level (m)")
  axis(1, lwd = 1, at=10^(seq(-1,log10(10^3), by = 1)), label=c(0.1, 1, 10, 100, 1000))
  axis(2, lwd = 1)
  lines(1/(1-(f[1:length(q)])), q, lty = 1, lwd = 2)
  lines(1/(1-(f[1:length(q)])), q + 1.96 * sqrt(v[1:length(q)]), col = "#0080FFFF", lwd = 1.5)
  lines(1/(1-(f[1:length(q)])), q - 1.96 * sqrt(v[1:length(q)]), col = "#0080FFFF", lwd = 1.5)
  points(1/(1-(1:length(dat)/(length(dat) + 1))), sort(dat), lwd = 1, cex = 0.75, pch = 21, bg = "white")
  box(lwd = 1)
  
  if(legend == TRUE | legend == T)
  {
    legend("topleft",
           c("Annual block maxima observations", "Best estimate"),
           col = c("black", "black"),
           pt.bg = c("white", NA),
           pch = c(21, NA),
           lty = c(NA, 1), cex=1,
           lwd = c(1.5, 1.5),
           bty = 'n',
           inset = c(0.01, -0.01))
  }
}

par(mgp=c(1.5,.5,0),mar=c(4, 3, 1, 2))
return_level_plot(coredata(year.res.max)/100, 10^4, legend = T)
dev.off()

################################## END #############################################
