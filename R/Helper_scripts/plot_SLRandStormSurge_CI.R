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

plot_SLRandStormSurge_CI = function(  year = 2070, 
                                      scen = "rcp85",
                                      deg = "2p5deg",
                                      sweet.scen = c("25", "20"),
                                      probs, 
                                      panel = "a."){
  
  # Set up data.frames to call the plotting color based on the study and projection scenario
  s.col = data.frame(col = 1:3, name = c("rcp85", "rcp45", "rcp26"))
  d.col = data.frame(col = 1:3, name = c("2p5deg", "2p0deg", "1p5deg"))
  sweet.col = data.frame(col = 1:6, name = c("25", "20", "15", "10", "05", "03"))
  
  s.col = s.col$col[which(s.col$name == scen)]
  d.col = d.col$col[which(d.col$name == deg)]
  sweet.col = sweet.col$col[which(sweet.col$name == sweet.scen)]
  
  # Create a combined sea level and storm surge return period plot
  plot(0, log = "x", type = "n", xlim = c(1, 500),
       ylim = c(2.85, 28), 
       xaxt = 'n', cex.lab=1.25, bty="l",
       xlab = "", 
       ylab = as.character(year))
  title(main=panel, adj=0)
  axis(1, lwd = 1, at=c(0.1, 1, 10, 100, 250, 500), label=c(0.1, 1, 10, 100, 250, 500))


  # Plot return periods based on projection year from the function input 
  if(scen=="rcp26"){
  #   RCP2.6-----------------------------------------------------------------------
    coln_year = which(colnames(k14_r26_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(k14_r26_SS_025[ ,coln_year], rev(k14_r26_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_kopp14_col[s.col], border = NA)
    
    coln_year = which(colnames(k17_DP16_SEW_r26_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(k17_DP16_SEW_r26_SS_025[ ,coln_year], rev(k17_DP16_SEW_r26_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_kopp17_DP16_col[s.col], border = NA)
    
    coln_year = which(colnames(bfd_r26_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(bfd_r26_SS_025[ ,coln_year], rev(bfd_r26_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_brickfd_col[s.col], border = NA)
    
    coln_year = which(colnames(NOfd_r26_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(NOfd_r26_SS_025[ ,coln_year], rev(NOfd_r26_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_NO_fd_col[s.col], border = NA)
    
    # Similar to 2.6
    coln_year = which(colnames(Ras18_SEW_1p5deg_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(Ras18_SEW_1p5deg_SS_025[ ,coln_year], rev(Ras18_SEW_1p5deg_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_Ras18_col[d.col], border = NA)
    
    coln_year = which(colnames(sweet17_03_SS_025) == paste("X", year, sep=""))
    polygon(y = c(sweet17_03_SS_025[ ,coln_year], rev(sweet17_03_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_sweet17_col[sweet.col[2]], border = NA)
    
    coln_year = which(colnames(sweet17_05_SS_025) == paste("X", year, sep=""))
    polygon(y = c(sweet17_05_SS_025[ ,coln_year], rev(sweet17_05_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_sweet17_col[sweet.col[1]], border = NA)
    
  } else if(scen=="rcp45"){

  #   RCP4.5-----------------------------------------------------------------------  
    coln_year = which(colnames(k14_r45_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(k14_r45_SS_025[ ,coln_year], rev(k14_r45_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_kopp14_col[s.col], border = NA)
    
    coln_year = which(colnames(k17_DP16_SEW_r45_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(k17_DP16_SEW_r45_SS_025[ ,coln_year], rev(k17_DP16_SEW_r45_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_kopp17_DP16_col[s.col], border = NA)
    
    coln_year = which(colnames(bfd_r45_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(bfd_r45_SS_025[ ,coln_year], rev(bfd_r45_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_brickfd_col[s.col], border = NA)
    
    coln_year = which(colnames(NOfd_r45_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(NOfd_r45_SS_025[ ,coln_year], rev(NOfd_r45_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_NO_fd_col[s.col], border = NA)    
    
  # coln_year = which(colnames(k14_r45_SS) == paste("t_", year, sep=""))
  # SF_k14_r45_SS = plot.sf(k14_r45_SS[ ,coln_year], make.plot=FALSE)
  # lines(1/SF_k14_r45_SS$sf, SF_k14_r45_SS$sf.num, col=kopp14_col[2], lwd=1.5)
  # 
  # coln_year = which(colnames(k17_DP16_SEW_r45_SS) == paste("t_", year, sep=""))
  # SF_k17_DP16_SEW_r45_SS = plot.sf(k17_DP16_SEW_r45_SS[ ,coln_year], make.plot=FALSE)
  # lines(1/SF_k17_DP16_SEW_r45_SS$sf, SF_k17_DP16_SEW_r45_SS$sf.num, col=kopp17_DP16_col[2], lwd=1.5)
  # 
  # coln_year = which(colnames(bfd_r45_SS) == paste("t_", year, sep=""))
  # SF_bfd_r45_SS = plot.sf(bfd_r45_SS[ ,coln_year], make.plot=FALSE)
  # lines(1/SF_bfd_r45_SS$sf, SF_bfd_r45_SS$sf.num, col=brickfd_col[2], lwd=1.5)
  # 
  # coln_year = which(colnames(NOfd_r45_SS) == paste("t_", year, sep=""))
  # SF_NOfd_r45_SS = plot.sf(NOfd_r45_SS[ ,coln_year], make.plot=FALSE)
  # lines(1/SF_NOfd_r45_SS$sf, SF_NOfd_r45_SS$sf.num, col=NO_fd_col[2], lwd=1.5)
  
  # Similar to 4.5
    coln_year = which(colnames(Ras18_SEW_1p5deg_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(Ras18_SEW_1p5deg_SS_025[ ,coln_year], rev(Ras18_SEW_1p5deg_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_Ras18_col[d.col], border = NA)
    
    coln_year = which(colnames(sweet17_03_SS_025) == paste("X", year, sep=""))
    polygon(y = c(sweet17_03_SS_025[ ,coln_year], rev(sweet17_03_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_sweet17_col[sweet.col[2]], border = NA)
    
    coln_year = which(colnames(sweet17_05_SS_025) == paste("X", year, sep=""))
    polygon(y = c(sweet17_05_SS_025[ ,coln_year], rev(sweet17_05_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_sweet17_col[sweet.col[1]], border = NA)
    
  # coln_year = which(colnames(Ras18_SEW_2p0deg_SS) == paste("t_", year, sep=""))
  # SF_Ras18_SEW_2p0deg_SS = plot.sf(Ras18_SEW_2p0deg_SS[ ,coln_year], make.plot=FALSE)
  # lines(1/SF_Ras18_SEW_2p0deg_SS$sf, SF_Ras18_SEW_2p0deg_SS$sf.num, col=Ras18_col[2], lwd=1.5)
  # 
  # coln_year = which(colnames(sweet17_10_SS) == paste("X", year, sep=""))
  # SF_sweet17_10_SS = plot.sf(sweet17_10_SS[ ,coln_year], make.plot=FALSE)
  # lines(1/SF_sweet17_10_SS$sf, SF_sweet17_10_SS$sf.num, col=sweet17_col[4], lwd=1.5)
  # 
  # coln_year = which(colnames(sweet17_15_SS) == paste("X", year, sep=""))
  # rm_na = sweet17_15_SS[!is.na(sweet17_15_SS[ ,coln_year]), coln_year] # remove NAs
  # SF_sweet17_15_SS = plot.sf(rm_na, make.plot=FALSE)
  # lines(1/SF_sweet17_15_SS$sf, SF_sweet17_15_SS$sf.num, col=sweet17_col[3], lwd=1.5)
  
  } else if(scen=="rcp85"){

  #   RCP8.5-----------------------------------------------------------------------  
    coln_year = which(colnames(k14_r85_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(k14_r85_SS_025[ ,coln_year], rev(k14_r85_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_kopp14_col[s.col], border = NA)
    
    coln_year = which(colnames(k17_DP16_SEW_r85_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(k17_DP16_SEW_r85_SS_025[ ,coln_year], rev(k17_DP16_SEW_r85_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_kopp17_DP16_col[s.col], border = NA)
    
    coln_year = which(colnames(bfd_r85_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(bfd_r85_SS_025[ ,coln_year], rev(bfd_r85_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_brickfd_col[s.col], border = NA)
    
    coln_year = which(colnames(NOfd_r85_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(NOfd_r85_SS_025[ ,coln_year], rev(NOfd_r85_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_NO_fd_col[s.col], border = NA)    
  #   
  #   
  # coln_year = which(colnames(k14_r85_SS) == paste("t_", year, sep=""))
  # SF_k14_r85_SS = plot.sf(k14_r85_SS[ ,coln_year], make.plot=FALSE)
  # lines(1/SF_k14_r85_SS$sf, SF_k14_r85_SS$sf.num, col=kopp14_col[1], lwd=1.5)
  # 
  # coln_year = which(colnames(k17_DP16_SEW_r85_SS) == paste("t_", year, sep=""))
  # SF_k17_DP16_SEW_r85_SS = plot.sf(k17_DP16_SEW_r85_SS[ ,coln_year], make.plot=FALSE)
  # lines(1/SF_k17_DP16_SEW_r85_SS$sf, SF_k17_DP16_SEW_r85_SS$sf.num, col=kopp17_DP16_col[1], lwd=1.5)
  # 
  # coln_year = which(colnames(bfd_r85_SS) == paste("t_", year, sep=""))
  # SF_bfd_r85_SS = plot.sf(bfd_r85_SS[ ,coln_year], make.plot=FALSE)
  # lines(1/SF_bfd_r85_SS$sf, SF_bfd_r85_SS$sf.num, col=brickfd_col[1], lwd=1.5)
  # 
  # coln_year = which(colnames(NOfd_r85_SS) == paste("t_", year, sep=""))
  # SF_NOfd_r85_SS = plot.sf(NOfd_r85_SS[ ,coln_year], make.plot=FALSE)
  # lines(1/SF_NOfd_r85_SS$sf, SF_NOfd_r85_SS$sf.num, col=NO_fd_col[1], lwd=1.5)  
  
  # Similar to 8.5
    coln_year = which(colnames(Ras18_SEW_2p5deg_SS_025) == paste("t_", year, sep=""))
    polygon(y = c(Ras18_SEW_2p5deg_SS_025[ ,coln_year], rev(Ras18_SEW_2p5deg_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_Ras18_col[d.col], border = NA)
    
    coln_year = which(colnames(sweet17_20_SS_025) == paste("X", year, sep=""))
    polygon(y = c(sweet17_20_SS_025[ ,coln_year], rev(sweet17_20_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_sweet17_col[sweet.col[2]], border = NA)
    
    coln_year = which(colnames(sweet17_05_SS_025) == paste("X", year, sep=""))
    polygon(y = c(sweet17_25_SS_025[ ,coln_year], rev(sweet17_25_SS_975[ ,coln_year])), 
            x = c(1/probs[order(1/probs)], 1/probs), col = trans_sweet17_col[sweet.col[1]], border = NA)
    
  # coln_year = which(colnames(Ras18_SEW_2p5deg_SS) == paste("t_", year, sep=""))
  # SF_Ras18_SEW_2p5deg_SS = plot.sf(Ras18_SEW_2p5deg_SS[ ,coln_year], make.plot=FALSE)
  # lines(1/SF_Ras18_SEW_2p5deg_SS$sf, SF_Ras18_SEW_2p5deg_SS$sf.num, col=Ras18_col[1], lwd=1.5)
  # 
  # coln_year = which(colnames(sweet17_20_SS) == paste("X", year, sep=""))
  # rm_na = sweet17_20_SS[!is.na(sweet17_20_SS[ ,coln_year]), coln_year] # remove NAs
  # SF_sweet17_20_SS = plot.sf(rm_na, make.plot=FALSE)
  # lines(1/SF_sweet17_20_SS$sf, SF_sweet17_20_SS$sf.num, col=sweet17_col[2], lwd=1.5)
  # 
  # coln_year = which(colnames(sweet17_25_SS) == paste("X", year, sep=""))
  # rm_na = sweet17_25_SS[!is.na(sweet17_25_SS[ ,coln_year]), coln_year] # remove NAs
  # SF_sweet17_25_SS = plot.sf(rm_na, make.plot=FALSE)
  # lines(1/SF_sweet17_25_SS$sf, SF_sweet17_25_SS$sf.num, col=sweet17_col[1], lwd=1.5)
  
  } else {
    stop("Unknown scenario")
    }
}
##==============================================================================
## End
##==============================================================================
