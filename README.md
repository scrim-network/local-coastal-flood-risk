# Synthesis and review of key available coastal flood risk estimates: A case study for Norfolk, VA.

This document contains the code used to generate the analysis found in Ruckert et al. (in prep.). Analysis codes are written in R with the exception of one file written in Matlab (to extract Kopp et al. 2014 data).

Full Citation:
> TO ADD

## Analysis overview

In this analysis, we examine the differences in published flood risk projections with an emphasis in Norfolk, VA. Flood risk projections we examine include sea-level rise, storm surge, and combined sea-level rise and storm surge. Our analysis compares flood risk projections by 1) extracting code or datasets available online or through personal communication, 2) identify background conditions/ assumptions/ methodology (e.g., units of measurements, water level datum, baseline year(s), localization method), and 3) convert data for comparibility.

## Running the analysis
This Github repository includes everything needed to reproduce the work described in Ruckert et al. (2018). To obtain the analysis data and codes:

```
git clone https://github.com/scrim-network/local-coastal-flood-risk.git
```

The data files that correspond to the paper are available in the `Data/` directory. However, we also include the code and details to obtain the data in the following section and therefore invite any user to reproduce our data collection methods. Some of the data files are zipped (i.e., [Data/BRICK\_NOfastDynamics\_SP\_08May2017.nc](https://github.com/scrim-network/local-coastal-flood-risk/blob/master/Data/BRICK_NOfastDynamics_SP_08May2017.nc.zip) and [Data/BRICK\_SewellsPoint\_FastDynamics\_08May2017.nc](https://github.com/scrim-network/local-coastal-flood-risk/blob/master/Data/BRICK_SewellsPoint_FastDynamics_08May2017.nc.zip), so they could be uploaded to Github. These files need to be unzipped before running the analysis, otherwise R will complain.

Once you have obtained all the code and data, you can open R and install the relevant R packages. Note that if these packages are already installed and/ or loaded in R, R will throw error messages with a request to restart R before proceeding with package updates. Then you can run the analysis and plot the figures.

```
R  

install.packages("ncdf4")
install.packages("extRemes")
install.packages("RColorBrewer")
install.packages("plotrix")
install.packages("ash")
install.packages("fields")
install.packages("fExtremes")
install.packages("ismev")
install.packages("lubridate")
install.packages("zoo")

setwd("local-coastal-flood-risk/R")  
source("Plot_LocalSeaLevel_StormSurge.R")
```

The script `R/Plot_LocalSeaLevel_StormSurge.R` calls the script that runs the analysis, calls additional helper scripts, and creates the figures. The script that runs the analysis is called `R/ReadAnalysis_LocalSeaLevel_StormSurge.R`. The file `R/ReadAnalysis_LocalSeaLevel_StormSurge.R` reads in all the data and converts all dataset to the same background conditions (i.e., feet, local mean sea-level, anomalies with respect to 2000, incorporating local subsidence). Additionally, the analysis combines sea-level rise and storm surge distributions. The other additional files included are `R/Helper_files/conversion_functions.R`, which contains functions to convert values from different units of measurement or water level datums and `R/Helper_files/plot_sf.r`, which generates and plots the survivival function of a given vector of data.

## Obtaining the data
As stated. above, all of the data needed to run the analysis and create the plots are provided in the `Data/` directory. However, we also include the scripts used to extract the data or state how we extract the data for reproducibility. If you do not wish to rerun the scripts, you can skip ahead to running the analysis and plotting the figures.

* Wong and Keller (2017; [view online](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1002/2017EF000607))
	* RCP2.6, 4.5, 6.0, and 8.5 sea-level rise distributions WITH fast dynamics
	* RCP2.6, 4.5, 6.0, and 8.5 sea-level rise distributions WITHOUT fast dynamics  
	We obtained the dataset used in Wong and Keller (2017) from their [download server](https://download.scrim.psu.edu/Wong_etal_BRICK/NOLA_Wong_etal_2017/output_model/) and the code from the [BRICK repository](https://github.com/scrim-network/BRICK). In their paper, they downscale projections locally to New Orleans. Following their steps, we modify their code to extract local projections at the Sewells Point tide gauge using the [Data/Calculate\_BRICK\_SewellsPoint\_LSL.R](https://github.com/scrim-network/local-coastal-flood-risk/blob/master/Data/Calculate_BRICK_SewellsPoint_LSL.R) script.

* Kopp et al. (2014; [view online](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1002/2014EF000239))
	* RCP2.6, 4.5, 6.0, and 8.5 sea-level rise distributions.  
	We obtained the code and data from Bob Kopp's [LocalizeSL Github repository](https://github.com/bobkopp/LocalizeSL). Specficially, the [LcalizeSL-1.2.zip](https://github.com/bobkopp/LocalizeSL/releases/download/v1.2/LocalizeSL-1.2.zip) file. Using the instructions he provides, we modify his code to extract local projections at the Sewells Point tide gauge using the [Data/Koppetal\_2014\_Generate\_SewellsPoint\_SLR.m](https://github.com/scrim-network/local-coastal-flood-risk/blob/master/Data/Koppetal_2014_Generate_SewellsPoint_SLR.m) script.
 
* Parris et al. (2012; [view online](https://cpo.noaa.gov/sites/cpo/Reports/2012/NOAA_SLR_r3.pdf))
	* 4 sea-level rise scenarios   
	We obtained this data from the [USACE Sea-level Change Curve Calculator](http://www.corpsclimate.us/ccaceslcurves.cfm) by setting the guage to "Sewells Point, VA", scenario source to "NOAA et al. 2012", output units to "Feet", SLC rate to "NOAA 2006 Rates", output datum to "LMSL", interval year to "1", and keeping all other options the same. Since all of the details to reproduce the scenarios are provided, we also include the [Data/Calculate\_USACE\_Parris\_Hall\_SLR\_data.R](https://github.com/scrim-network/local-coastal-flood-risk/blob/master/Data/Calculate_USACE_Parris_Hall_SLR_data.R) script (for easier reproducibility), which reproduces the scenarios using the coded model.
	
* USACE ([2011](http://www.corpsclimate.us/docs/EC_1165-2-212%20-Final_10_Nov_2011.pdf); [2013](http://www.publications.usace.army.mil/Portals/76/Publications/EngineerRegulations/ER_1100-2-8162.pdf); [2014](http://www.publications.usace.army.mil/Portals/76/Publications/EngineerTechnicalLetters/ETL_1100-2-1.pdf))
	* 3 sea-level rise scenarios
	* Maximum likelihood estimates of storm surge  
	We obtained this data from the [USACE Sea-level Change Curve Calculator](http://www.corpsclimate.us/ccaceslcurves.cfm) by setting the guage to "Sewells Point, VA", scenario source to "USACE 2013", output units to "Feet", SLC rate to "NOAA 2006 Rates", output datum to "LMSL", interval year to "1", EWL type to "Highs", EWL source to "USACE (Percentile)", and keeping all other options the same. Since all of the details to reproduce the scenarios are provided, we also include the [Data/Calculate\_USACE\_Parris\_Hall\_SLR\_data.R](https://github.com/scrim-network/local-coastal-flood-risk/blob/master/Data/Calculate_USACE_Parris_Hall_SLR_data.R) script (for easier reproducibility), which reproduces the scenarios using the coded model.
	
* Hall et al. (2016; [view online](https://www.hsdl.org/?abstract&did=792698))
	* 5 sea-level rise scenarios  
	We obtained this data from the [USACE Sea-level Change Curve Calculator](http://www.corpsclimate.us/ccaceslcurves.cfm) by setting the guage to "Sewells Point, VA", scenario source to "CARSWG 2016", output units to "Feet", SLC rate to "NOAA 2006 Rates", output datum to "LMSL", interval year to "1", and keeping all other options the same. Since all of the details to reproduce the scenarios are provided, we also include the [Data/Calculate\_USACE\_Parris\_Hall\_SLR\_data.R](https://github.com/scrim-network/local-coastal-flood-risk/blob/master/Data/Calculate_USACE_Parris_Hall_SLR_data.R) script (for easier reproducibility), which reproduces the scenarios using the coded model.
	
* Sweet et al. (2017; [view online](https://tidesandcurrents.noaa.gov/publications/techrpt83_Global_and_Regional_SLR_Scenarios_for_the_US_final.pdf))
	* 6 sea-level rise scenarios  
	We obtained this data from the [USACE Sea-level Change Curve Calculator](http://www.corpsclimate.us/ccaceslcurves.cfm) by setting the guage to "SEWELLS POINT", scenario source to "NOAA et al. 2017", and output units to "Feet". We also check the option to "Show 2100 to 2200" and "Adjust to MSL(83-01) Datum". Everything else is left as is. Although the scenarios are produced with the National Research Council (1987) sea-level model, we do not provide a script to reproduce the projections because we currently do not have the fingerprints or other non-climatic data in which Sweet et al. (2017) used to localize the scenarios.
	
* Tebaldi et al. (2012; [view online](http://iopscience.iop.org/article/10.1088/1748-9326/7/1/014032))
	* Maximum likelihood estimates and 95% confidence interval estimates of storm surge  
	 We obtained this data through personal communication with Claudia Tebaldi.
	 
* Srikrishnan et al. (in prep.)
	* Distribution of GEV parameters  
	 We obtained this data through personal communication with Vivek Srikrishnan.
	 
* Zervas (2013; [view online](https://tidesandcurrents.noaa.gov/publications/NOAA_Technical_Report_NOS_COOPS_067a.pdf))
	* Maximum likelihood estimates and 95% confidence interval estimates of storm surge  
	We collected the 95% confidence interval estimates from Table C in Appendix III of the paper. This table also contains the maximum likelihood estimates for Sewells Point, VA, but we instead use the estimates provided on the [USACE Sea-level Change Curve Calculator](http://www.corpsclimate.us/ccaceslcurves.cfm). We do so because the USACE Sea-level Change Curve Calculator provides several additional return period estimates that were not included in Table C and the differences between the estimates are negelable. Using the USACE Sea-level Change Curve Calculator, we obtain the estimates by setting the output units to "Feet", output datum to "LMSL", EWL type to "Highs", and EWL source to "NOAA (GEV)".
	
* Storm surge observations
	* Annual block maxima.  
	We downloaded verified hourly height water levels from NOAA's tides and currents data inventory for [Sewells Point](https://tidesandcurrents.noaa.gov/inventory.html?id=8638610). Data was collected from Jan. 1, 1928 to Dec. 31, 2015 with the units set to "Metric", timezone to "GMT", and datum set to "MSL". After all the data was collected and formatted for R, we estimated the annual block maxima using the [Data/Sewells\_Point\_tides.R](https://github.com/scrim-network/local-coastal-flood-risk/blob/master/Data/Sewells_Point_tides.R) script.
	
## Contacts
Kelsey Ruckert (klr324@psu.edu)  
Vivek Srikrishnan (vxs914@psu.edu)  
Klaus Keller (klaus@psu.edu)

## License
Copyright 2018 Kelsey Ruckert

These files are free software: you can redistribute them and/or modify them under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

These files are distributed in the hope that they will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with these files. If not, see http://www.gnu.org/licenses/.
