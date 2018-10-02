##==============================================================================
## plot_SLRcompare_PDF.R
##
## This function plots a PDF plot of probabilistic sea-level rise projections from:
##   - Kopp et al. 2014
##   - Kopp et al. 2017
##   - Wong et al. 2017 accounting for fast dynamics
##   - Wong et al. 2017 neglecting fast dynamics
##   - Sweet et al. 2017
##   - Rasmussen et al. 2018
##---------------------------------------------------------------------------------
## The function takes input of the:
##   - Projection year (year = 2070)
##   - RCP scenario for Kopp et al. 2014 and 2017 and also Wong et al. 2017 
##          (scen = "rcp85") 
##   - Degree of temperature warming for Rasmussen et al. 2018 (deg = "2p5deg")
##   - Scenario based on the amount of global sea-level rise in 2100 for Sweet et al. 
##          2017 (sweet.scen = c("25", "20"))
##   - Character to name a panel (panel = "a.")
##
## The function was written in this fashion, so plots comparing different projection 
## years and scenarios can be regenerated quickly and also in a multiple panel plot.
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

plot_SLRcompare_PDF = function(  year = 2070,
                                 scen = "rcp85",
                                 deg = "2p5deg",
                                 sweet.scen = c("25", "20"),
                                 panel = "a."){
  
  # Set up data.frames to call the plotting color based on the study and projection scenario
  s.col = data.frame(col = 1:3, name = c("rcp85", "rcp45", "rcp26"))
  d.col = data.frame(col = 1:3, name = c("2p5deg", "2p0deg", "1p5deg"))
  sweet.col = data.frame(col = 1:6, name = c("25", "20", "15", "10", "05", "03"))
  
  s.col = s.col$col[which(s.col$name == scen)]
  d.col = d.col$col[which(d.col$name == deg)]
  sweet.col = sweet.col$col[which(sweet.col$name == sweet.scen)]
  
  # Set x and y axis limits and also the y coordinates for where to plot the Sweet et al. 
  # polygon based on the projection year  
  if(year == "2030"){
    xl = c(-0.5,2); yl = c(0, 6.75); ypol = c(yl[2]-1.75, yl[2]-2.25); dif = 0.5*2
  } else if (year == "2050"){
    xl = c(-0.5,4); yl = c(0, 3.5); ypol = c(yl[2]-1, yl[2]-1.25); dif = 0.25*2
  } else if (year == "2070"){
    xl = c(-0.5,8); yl = c(0, 3); ypol = c(yl[2]-1, yl[2]-1.25); dif = 0.25*2
  } else if (year == "2100"){
    xl = c(-0.5,15); yl = c(0, 1.5); ypol = c(yl[2]-0.4, yl[2]-0.55); dif = 0.15*2
  } else {
    xl = c(-0.5,2); yl = c(0, 6.75); ypol = c(yl[2]-1.75, yl[2]-2.25); dif = 0.5*2
  }
  xl = c(-0.5,15)
  # Create a density plot  
  plot(density(c(1,1)), type = "n", xlab= as.character(year), ylab="", cex.lab=1.25,
       yaxt="n", main="", xlim=xl, ylim=yl, bty="l")
  title(main=panel, adj=0)

  # Plot the projections based on the scenario and projection year from the function input
  var = paste('kopp14_', scen, sep="")
  coln_year = which(colnames(get(var)) == paste("t_", year, sep=""))
  lines(density(get(var)[ ,coln_year]), col=kopp14_col[s.col], lwd=2)

  var = paste('kopp17_DP16_SEW_', scen, sep="")
  coln_year = which(colnames(get(var)) == paste("t_", year, sep=""))
  lines(density(get(var)[ ,coln_year]), col=kopp17_DP16_col[s.col], lwd=2)

  var = paste('brickfd_', scen, sep="")
  coln_year = which(colnames(get(var)) == paste("t_", year, sep=""))
  lines(density(get(var)[ ,coln_year]), col=brickfd_col[s.col], lwd=2)

  var = paste('NO_fdft_', scen, sep="")
  coln_year = which(colnames(get(var)) == paste("t_", year, sep=""))
  lines(density(get(var)[ ,coln_year]), col=NO_fd_col[s.col], lwd=2)

  var = paste('Ras18_SEW_', deg, sep="")
  coln_year = which(colnames(get(var)) == paste("t_", year, sep=""))
  lines(density(get(var)[ ,coln_year]), col=Ras18_col[d.col], lwd=2)

  # Plot the Sweet et al. 2017 as multiple polygons to resemble box and wisker like plots
  var = paste('sweet17_', sweet.scen[1], sep="")
  coln_year = which(colnames(get(var)) == paste("X", year, sep=""))
  #lines(density(get(var)[ ,coln_year], na.rm=TRUE), col=sweet17_col[sweet.col[1]], lwd=2)
  # add the lighter 5-95% range polygon
  polygon(x = c(quantile(get(var)[ ,coln_year], 0.05, na.rm=TRUE), 
                quantile(get(var)[ ,coln_year], 0.05, na.rm=TRUE),
                quantile(get(var)[ ,coln_year], 0.95, na.rm=TRUE),
                quantile(get(var)[ ,coln_year], 0.95, na.rm=TRUE)), 
          y = c(rev(ypol), ypol), col=trans_sweet17_col[sweet.col[2]], border=NA)
  # now add the darker 25-75% range polygon
  polygon(x = c(quantile(get(var)[ ,coln_year], 0.25, na.rm=TRUE), 
                quantile(get(var)[ ,coln_year], 0.25, na.rm=TRUE),
                quantile(get(var)[ ,coln_year], 0.75, na.rm=TRUE),
                quantile(get(var)[ ,coln_year], 0.75, na.rm=TRUE)), 
          y = c(rev(ypol), ypol), col=sweet17_col[sweet.col[1]], border=NA)
  lines(rep(quantile(get(var)[ ,coln_year], 0.50, na.rm=TRUE),2), ypol,
        lwd=2, col='black')

  var = paste('sweet17_', sweet.scen[2], sep="")
  coln_year = which(colnames(get(var)) == paste("X", year, sep=""))
  #lines(density(get(var)[ ,coln_year], na.rm=TRUE), col=sweet17_col[sweet.col[2]], lwd=2)
  # add the lighter 5-95% range polygon
  polygon(x = c(quantile(get(var)[ ,coln_year], 0.05, na.rm=TRUE), 
                quantile(get(var)[ ,coln_year], 0.05, na.rm=TRUE),
                quantile(get(var)[ ,coln_year], 0.95, na.rm=TRUE),
                quantile(get(var)[ ,coln_year], 0.95, na.rm=TRUE)), 
          y = c(rev(ypol), ypol)-dif, col=trans_sweet17_col[sweet.col[2]], border=NA)
  # now add the darker 25-75% range polygon
  polygon(x = c(quantile(get(var)[ ,coln_year], 0.25, na.rm=TRUE), 
                quantile(get(var)[ ,coln_year], 0.25, na.rm=TRUE),
                quantile(get(var)[ ,coln_year], 0.75, na.rm=TRUE),
                quantile(get(var)[ ,coln_year], 0.75, na.rm=TRUE)), 
          y = c(rev(ypol), ypol)-dif, col=sweet17_col[sweet.col[2]], border=NA)
  lines(rep(quantile(get(var)[ ,coln_year], 0.50, na.rm=TRUE),2), ypol-dif,
        lwd=2, col='black')
}
##==============================================================================
## End
##==============================================================================
