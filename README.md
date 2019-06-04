# Code for "Characterizing the deep uncertainties surrounding coastal flood hazard projections: A case study for Norfolk, VA"

This directory contains the code used to generate the analysis found in Ruckert et al. (in review). Analysis codes are predomintely written in R with a few files written in Matlab (which extract data from other studies).

Full Citation:

> Ruckert, K. L., Srikrishnan, V., and Keller, K. (in review). Characterizing the deep uncertainties surrounding coastal flood hazard projections: A case study for Norfolk, VA. *Scientific Reports*, ...

> Ruckert, K. L., Srikrishnan, V., and Keller, K. (2018). Characterizing the deep uncertainties surrounding coastal flood hazard projections: A case study for Norfolk, VA. arXiv preprint. arXiv: [1812.04673v2](https://arxiv.org/abs/1812.04673v2).

## Analysis overview

In this analysis, we examine the differences in published coastal flood hazard projections (sea-level rise and storm surge) with an emphasis on Norfolk, VA. Our analysis compares flood hazard projections by 1) extracting code or datasets available online or through personal communication, 2) identifying background conditions/ assumptions/ methodology (e.g., units of measurements, water level datum, baseline year(s), localization method), and 3) converting data for comparibility.

## Running the analysis
This directory includes everything needed to reproduce the work described in Ruckert et al. (in prep.). The data files that correspond to the paper are available in the `Data/` directory. However, we also include the code and details to obtain the data in the following section and therefore invite any user to reproduce our data collection methods. Some of the data files are zipped (i.e., `Data/BRICK_NOfastDynamics_SP_20Nov2018.nc` and `Data/BRICK_SewellsPoint_FastDynamics_20Nov2018.nc`) to reduce file size. These files need to be unzipped before running the analysis, otherwise R will complain.

Once you have obtained all the code and data, you can open R, install the relevant R packages, and run the analysis. Note that if these packages are already installed and/ or loaded in R, R will throw error messages with a request to restart R before proceeding with package updates. Additionally, the package `svglite` requires [XQuartz](https://www.xquartz.org) to save files with semi-transparent colors as .eps. To save files as pdfs without this function simply comment out `library(svglite)`, comment out all the `cairo()` code snippets, and uncomment the `pdf()` code snippets.

```R  
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
install.packages("diagram")
install.packages("DEoptim")
install.packages("stringr")
install.packages("svglite") # XQuartz is required to use Cairo

setwd("PATH_TO_DIRECTORY/local-coastal-flood-risk/R")  
source("Plot_LocalSeaLevel_StormSurge.R")
```

The script `R/Plot_LocalSeaLevel_StormSurge.R` calls the script that runs the analysis, calls additional helper scripts, and creates the figures. The script that runs the analysis is called `R/ReadAnalysis_LocalSeaLevel_StormSurge.R`. The file `R/ReadAnalysis_LocalSeaLevel_StormSurge.R` reads in all the data and converts all data to the same background conditions (i.e., feet, local mean sea-level, anomalies with respect to 2000, incorporating local subsidence). Additionally, the analysis combines sea-level rise and storm surge distributions accounting for uncertainty.

## Obtaining the data (optional)
As stated above, all of the data needed to run the analysis and create the plots are provided in the `Data/` directory. However, we also include the scripts used to extract the data or state how we extracted the data for reproducibility. If you do not wish to rerun the data collection scripts, you can skip this part and just run the analysis and plot the figures.

### Sea-level projections

* USACE ([2011](http://www.corpsclimate.us/docs/EC_1165-2-212%20-Final_10_Nov_2011.pdf); [2013](http://www.publications.usace.army.mil/Portals/76/Publications/EngineerRegulations/ER_1100-2-8162.pdf); [2014](http://www.publications.usace.army.mil/Portals/76/Publications/EngineerTechnicalLetters/ETL_1100-2-1.pdf))
	* 3 sea-level rise scenarios   
	We obtained this data from the [USACE Sea-level Change Curve Calculator](http://corpsmapu.usace.army.mil/rccinfo/slc/slcc_calc.html) on Apr. 4th, 2108. The data is obtained by setting the guage to "Sewells Point, VA", scenario source to "USACE 2013", output units to "Feet", SLC rate to "NOAA 2006 Rates", output datum to "LMSL", interval year to "1", and keeping all other options the same. Since all of the details to reproduce the scenarios are provided, we also include the `Data/Calculate_USACE_Parris_Hall_SLR_data.R` script (for easier reproducibility), which reproduces the scenarios using the coded model. The values are changed to be relative to the year 2000 in the script reading in the data for the analysis (`R/ReadAnalysis_LocalSeaLevel_StormSurge.R`).

* Parris et al. (2012; [view online](https://cpo.noaa.gov/sites/cpo/Reports/2012/NOAA_SLR_r3.pdf))
	* 4 sea-level rise scenarios   
	We obtained this data from the [USACE Sea-level Change Curve Calculator](http://corpsmapu.usace.army.mil/rccinfo/slc/slcc_calc.html) on Apr. 4th, 2108. The data is obtained by setting the guage to "Sewells Point, VA", scenario source to "NOAA et al. 2012", output units to "Feet", SLC rate to "NOAA 2006 Rates", output datum to "LMSL", interval year to "1", and keeping all other options the same. Since all of the details to reproduce the scenarios are provided, we also include the `Data/Calculate_USACE_Parris_Hall_SLR_data.R` script (for easier reproducibility), which reproduces the scenarios using the coded model. The values are changed to be relative to the year 2000 in the script reading in the data for the analysis (`R/ReadAnalysis_LocalSeaLevel_StormSurge.R`).

* Kopp et al. (2014; [view online](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1002/2014EF000239))
	* RCP2.6, 4.5, and 8.5 sea-level rise distributions.  
	We obtained the code and data from Bob Kopp's [LocalizeSL Github repository](https://github.com/bobkopp/LocalizeSL). Specficially, the [LocalizeSL-1.2.zip](https://github.com/bobkopp/LocalizeSL/releases/download/v1.2/LocalizeSL-1.2.zip) file. Using the instructions he provides, we modify his code to extract local projections at the Sewell's Point tide gauge using the `Data/Koppetal_2014_Generate_SewellsPoint_SLR.m` script.
	
* Hall et al. (2016; [view online](https://www.hsdl.org/?abstract&did=792698))
	* 5 sea-level rise scenarios  
	We obtained this data from the [USACE Sea-level Change Curve Calculator](http://corpsmapu.usace.army.mil/rccinfo/slc/slcc_calc.html) on Apr. 4th, 2108. The data is obtained by setting the guage to "Sewells Point, VA", scenario source to "CARSWG 2016", output units to "Feet", SLC rate to "NOAA 2006 Rates", output datum to "LMSL", interval year to "1", and keeping all other options the same. Since all of the details to reproduce the scenarios are provided, we also include the `Data/Calculate_USACE_Parris_Hall_SLR_data.R` script (for easier reproducibility), which reproduces the scenarios using the coded model. The values are changed to be relative to the year 2000 in the script reading in the data for the analysis (`R/ReadAnalysis_LocalSeaLevel_StormSurge.R`).

* Wong and Keller (2017; [view online](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1002/2017EF000607)) extended to 2200
	* RCP2.6, 4.5, and 8.5 sea-level rise distributions WITH fast dynamics
	* RCP2.6, 4.5, and 8.5 sea-level rise distributions WITHOUT fast dynamics  
	We reran the analysis in Wong and Keller (2017) using the code from the [BRICK repository](https://github.com/scrim-network/BRICK) and modify the code to project to 2200 rather than 2100. In their paper, they downscale projections locally to New Orleans. Following their steps, we modify their code to extract local projections at the Sewell's Point tide gauge using the `Data/Calculate_BRICK_SewellsPoint_LSL.R` script. The .nc file listed in the code is too large to place on Github, so contact us if you don't have this file. The file will be available on DataCommons.
	
* Kopp et al. (2017; [view online](https://agupubs.onlinelibrary.wiley.com/doi/10.1002/2017EF000663))
	* RCP2.6, 4.5, and 8.5 sea-level rise distributions.  
	We obtained the code and data from Bob Kopp's [LocalizeSL Github repository](https://github.com/bobkopp/LocalizeSL). Specficially, the [LocalizeSL-2.0.zip](https://github.com/bobkopp/LocalizeSL/releases/download/v2.0/LocalizeSL-2.0.zip) file. Using the instructions he provides, we modify his code to extract local projections at the Sewell's Point tide gauge using the `Data/Koppetal_2017_Generate_DP16_SEW_SLR.m` script.
	
* Sweet et al. (2017; [view online](https://tidesandcurrents.noaa.gov/publications/techrpt83_Global_and_Regional_SLR_Scenarios_for_the_US_final.pdf))
	* 6 sea-level rise scenarios  
	We obtained the code and data from Bob Kopp's [LocalizeSL Github repository](https://github.com/bobkopp/LocalizeSL). Download the github repository and within the `LocalizeSL/MFILES/scripts_Sweet2017` copy in and run the file `Data/Extract_SEW_LSL_Sweet2017.m`.
	
* Rasmussen et al. (2018; [view online](http://iopscience.iop.org/article/10.1088/1748-9326/aaac87))
	* Temp. target 1.5, 2.0, and 2.5 &deg;C sea-level rise distributions.  
	We obtained the code and data from Bob Kopp's [LocalizeSL Github repository](https://github.com/bobkopp/LocalizeSL). Specficially, the [SLRProjections180124GRIDDEDcore_Tscens.mat](https://github.com/bobkopp/LocalizeSL/blob/master/IFILES/SLRProjections180124GRIDDEDcore_Tscens.mat) file, which contains the Rasmussen et al. (2018) data. Using the instructions provided in `https://github.com/bobkopp/LocalizeSL/blob/master/notebooks/runLocalizeSL_Rasumssen2018.ipynb`, we modify the code to extract local projections at the Sewell's Point tide gauge using the `Data/Rasmussenetal_2018_Generate_SewellsPoint_SLR.m` script.
	
### Storm surge analysis	

* Tebaldi et al. (2012; [view online](http://iopscience.iop.org/article/10.1088/1748-9326/7/1/014032))
	* Maximum likelihood estimates and 95% confidence interval estimates of storm surge  
	 We obtained this data through personal communication with Claudia Tebaldi.
	 
* Zervas (2013; [view online](https://tidesandcurrents.noaa.gov/publications/NOAA_Technical_Report_NOS_COOPS_067a.pdf))
	* Maximum likelihood estimates and 95% confidence interval estimates of storm surge  
	We collected the 95% confidence interval estimates from Table C in Appendix III of the paper. This table also contains the maximum likelihood estimates for Sewell's Point, VA, but we instead use the estimates provided on the [USACE Sea-level Change Curve Calculator](http://corpsmapu.usace.army.mil/rccinfo/slc/slcc_calc.html) (downloaded on Apr. 5th, 2018). We do so because the USACE Sea-level Change Curve Calculator provides several additional return period estimates that were not included in Table C and the differences between the estimates are negligible. Using the USACE Sea-level Change Curve Calculator, we obtain the estimates by setting the output units to "Feet", output datum to "LMSL", EWL type to "Highs", and EWL source to "NOAA (GEV)".
	
* USACE (2014; [view online](http://www.publications.usace.army.mil/Portals/76/Publications/EngineerTechnicalLetters/ETL_1100-2-1.pdf))
	* Maximum likelihood estimates of storm surge  
	We obtained this data from the [USACE Sea-level Change Curve Calculator](http://corpsmapu.usace.army.mil/rccinfo/slc/slcc_calc.html) on Feb. 13th, 2018. The data was obtained by setting the gauge to "Sewells Point, VA", output units to "Feet", output datum to "LMSL", EWL type to "Highs", EWL source to "USACE (Percentile)", and keeping all other options the same.
	
* Wong (2018; [view online](https://www.adv-stat-clim-meteorol-oceanogr.net/4/53/2018/))
	* Distributions of stationary and non-stationary storm surge estimates in 2065 
	The maximum likelihood estimates, 95% confidence interval estimates, and median 100-yr return levels of stationary and non stationary values in 2065 are approximated in the `R/ReadAnalysis_LocalSeaLevel_StormSurge.R` file. The data and analysis codes are located at [https://github.com/tonyewong/covariates).
	 
* Our historical observation analysis
	* Annual block maxima  
	We downloaded verified hourly height water levels from NOAA's tides and currents data inventory for [Sewell's Point](https://tidesandcurrents.noaa.gov/inventory.html?id=8638610). Data was collected from Jan. 1, 1928 to Dec. 31, 2015 with the units set to "Metric", timezone to "GMT", and datum set to "MSL". After all the data was collected and formatted for R, we estimated the annual block maxima using the `Data/Sewells_Point_tides.R` script. Return periods associated with the annual block maxima are calculated using a numerical median probability return period function within the `R/ReadAnalysis_LocalSeaLevel_StormSurge.R` file. This is the `median.rt()` function in the `R/Helper_scripts/Empirical_probability_calculator.R` file.
	 
* Our storm surge analysis
	* Distribution of GEV parameters  
	The maximum likelihood estimates and 95% confidence interval estimates of storm surge are approximated in the `R/ReadAnalysis_LocalSeaLevel_StormSurge.R` file. The data and analysis codes to reproduce the distribution of the GEV parameters are located at [http://www.github.com/vsrikrish/SPSLAM](http://www.github.com/vsrikrish/SPSLAM).
	
## Contacts
Kelsey Ruckert (klr324@psu.edu)  
Vivek Srikrishnan (vxs914@psu.edu)  
Klaus Keller (klaus@psu.edu)

## License
Copyright 2018 Kelsey Ruckert

These files are free software: you can redistribute them and/or modify them under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

These files are distributed in the hope that they will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with these files. If not, see http://www.gnu.org/licenses/.
