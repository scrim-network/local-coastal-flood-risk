##==============================================================================
## plot_SLRandStormSurge_SF.R
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

plot_SLRandStormSurge_SF = function(  year = 2070, 
                            panel = "a."){
  
  # Create a combined sea level and storm surge return period plot
  plot(0, log = "x", type = "n", xlim = c(1, 500),
       ylim = c(2.85, 18), 
       xaxt = 'n', cex=1, bty="l",
       xlab = "Return period (years)", 
       ylab = paste("Projected sea+surge level in ", year, " (ft MSL)", sep=""))
  title(main=panel, adj=0)
  axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))

  # Plot return periods based on projection year from the function input 
  #   RCP2.6-----------------------------------------------------------------------
  coln_year = which(colnames(k14_r26_SS) == paste("t_", year, sep=""))
  SF_k14_r26_SS = plot.sf(k14_r26_SS[ ,coln_year], make.plot=FALSE)
  lines(1/SF_k14_r26_SS$sf, SF_k14_r26_SS$sf.num, col=kopp14_col[3], lwd=1.5)
  
  coln_year = which(colnames(k17_DP16_SEW_r26_SS) == paste("t_", year, sep=""))
  SF_k17_DP16_SEW_r26_SS = plot.sf(k17_DP16_SEW_r26_SS[ ,coln_year], make.plot=FALSE)
  lines(1/SF_k17_DP16_SEW_r26_SS$sf, SF_k17_DP16_SEW_r26_SS$sf.num, col=kopp17_DP16_col[3], lwd=1.5)
  
  coln_year = which(colnames(bfd_r26_SS) == paste("t_", year, sep=""))
  SF_bfd_r26_SS = plot.sf(bfd_r26_SS[ ,coln_year], make.plot=FALSE)
  lines(1/SF_bfd_r26_SS$sf, SF_bfd_r26_SS$sf.num, col=brickfd_col[3], lwd=1.5)
  
  coln_year = which(colnames(NOfd_r26_SS) == paste("t_", year, sep=""))
  SF_NOfd_r26_SS = plot.sf(NOfd_r26_SS[ ,coln_year], make.plot=FALSE)
  lines(1/SF_NOfd_r26_SS$sf, SF_NOfd_r26_SS$sf.num, col=NO_fd_col[3], lwd=1.5)
  
  #   RCP4.5-----------------------------------------------------------------------  
  coln_year = which(colnames(k14_r45_SS) == paste("t_", year, sep=""))
  SF_k14_r45_SS = plot.sf(k14_r45_SS[ ,coln_year], make.plot=FALSE)
  lines(1/SF_k14_r45_SS$sf, SF_k14_r45_SS$sf.num, col=kopp14_col[2], lwd=1.5)
  
  coln_year = which(colnames(k17_DP16_SEW_r45_SS) == paste("t_", year, sep=""))
  SF_k17_DP16_SEW_r45_SS = plot.sf(k17_DP16_SEW_r45_SS[ ,coln_year], make.plot=FALSE)
  lines(1/SF_k17_DP16_SEW_r45_SS$sf, SF_k17_DP16_SEW_r45_SS$sf.num, col=kopp17_DP16_col[2], lwd=1.5)
  
  coln_year = which(colnames(bfd_r45_SS) == paste("t_", year, sep=""))
  SF_bfd_r45_SS = plot.sf(bfd_r45_SS[ ,coln_year], make.plot=FALSE)
  lines(1/SF_bfd_r45_SS$sf, SF_bfd_r45_SS$sf.num, col=brickfd_col[2], lwd=1.5)
  
  coln_year = which(colnames(NOfd_r45_SS) == paste("t_", year, sep=""))
  SF_NOfd_r45_SS = plot.sf(NOfd_r45_SS[ ,coln_year], make.plot=FALSE)
  lines(1/SF_NOfd_r45_SS$sf, SF_NOfd_r45_SS$sf.num, col=NO_fd_col[2], lwd=1.5)
  
  #   RCP8.5-----------------------------------------------------------------------  
  coln_year = which(colnames(k14_r85_SS) == paste("t_", year, sep=""))
  SF_k14_r85_SS = plot.sf(k14_r85_SS[ ,coln_year], make.plot=FALSE)
  lines(1/SF_k14_r85_SS$sf, SF_k14_r85_SS$sf.num, col=kopp14_col[1], lwd=1.5)
  
  coln_year = which(colnames(k17_DP16_SEW_r85_SS) == paste("t_", year, sep=""))
  SF_k17_DP16_SEW_r85_SS = plot.sf(k17_DP16_SEW_r85_SS[ ,coln_year], make.plot=FALSE)
  lines(1/SF_k17_DP16_SEW_r85_SS$sf, SF_k17_DP16_SEW_r85_SS$sf.num, col=kopp17_DP16_col[1], lwd=1.5)
  
  coln_year = which(colnames(bfd_r85_SS) == paste("t_", year, sep=""))
  SF_bfd_r85_SS = plot.sf(bfd_r85_SS[ ,coln_year], make.plot=FALSE)
  lines(1/SF_bfd_r85_SS$sf, SF_bfd_r85_SS$sf.num, col=brickfd_col[1], lwd=1.5)
  
  coln_year = which(colnames(NOfd_r85_SS) == paste("t_", year, sep=""))
  SF_NOfd_r85_SS = plot.sf(NOfd_r85_SS[ ,coln_year], make.plot=FALSE)
  lines(1/SF_NOfd_r85_SS$sf, SF_NOfd_r85_SS$sf.num, col=NO_fd_col[1], lwd=1.5)  
  
}
##==============================================================================
## End
##==============================================================================
