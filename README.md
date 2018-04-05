# Synthesis and review of key available coastal flood risk estimates: A case study for Norfolk, VA.

This document contains the code used to generate the analysis found in Ruckert et al. (in prep.). Analysis codes are written in R with the exception of one file written in Matlab (to extract Kopp et al. 2014 data).

Full Citation:
> TO ADD

## Analysis overview

In this analysis, we examine the differences in published flood risk projections with an emphasis in Norfolk, VA. Flood risk projections we examine include sea-level rise, storm surge, and combined sea-level rise and storm surge. Our analysis compares flood risk projections by 1) extracting code or datasets available online or through personal communication, 2) identify background conditions/ assumptions/ methodology (e.g., units of measurements, water level datum, baseline year(s), localization method), 3) convert data for comparibility, and 4) estimate annual block maxima from tide gauge observations.

## Obtaining the data
All of the data needed to run the analysis and create the plots are provided in the `/Data/` directory. However, we also include the scripts used to extract the data or state how we extract the data for reproducibility. If you do not wish to rerun the scripts, you can skip ahead to running the analysis and plotting the figures.

* Wong and Keller (2017)
	* RCP2.6, 4.5, 6.0, and 8.5 sea-level rise distributions WITH fast dynamics
	* RCP2.6, 4.5, 6.0, and 8.5 sea-level rise distributions WITHOUT fast dynamics  
	Dataset obtained from [download server](https://download.scrim.psu.edu/Wong_etal_BRICK/NOLA_Wong_etal_2017/output_model/) and code obtained from the [BRICK repository](https://github.com/scrim-network/BRICK). We modify this code to extract local projections at the Sewells Point tide gauge using: 

* Kopp et al. (2014)
	* RCP2.6, 4.5, 6.0, and 8.5 sea-level rise distributions.  
	Code and data obtained from the [LocalizeSL repository](https://github.com/bobkopp/LocalizeSL). Specficially, the [LcalizeSL-1.2.zip](https://github.com/bobkopp/LocalizeSL/releases/download/v1.2/LocalizeSL-1.2.zip)
	* 
* Parris et al. (2012)
	* 4 sea-level rise scenarios
* USACE (2011; 2013; 2014)
	* 3 sea-level rise scenarios
	* Maximum likelihood estimates of storm surge
* Hall et al. 2016
	* 5 sea-level rise scenarios
* Sweet et al. 2017
	* 6 sea-level rise scenarios
* Tebaldi et al. (2012)
	* Maximum likelihood estimates and 95% confidence interval estimates of storm surge
* Srikrishnan et al. (in prep.)
	* Distribution of GEV parameters
* Zervas (2013)
	* Maximum likelihood estimates and 95% confidence interval estimates of storm surge
* Storm surge observations
	* Annual block maxima 





