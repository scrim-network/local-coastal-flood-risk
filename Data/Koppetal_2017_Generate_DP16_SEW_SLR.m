% Edits by Kelsey Ruckert (klr324@psu.edu) April 2018
% with the purpose of extracting probabilistic SLR data
% for Norfolk (Sewells Point).

% Original file: https://github.com/bobkopp/LocalizeSL/blob/master/notebooks/LocalizeSL%20Example.ipynb

% Copyright (C) 2017 by Robert E. Kopp

% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.

% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.

% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

% R. E. Kopp, R. M. DeConto, D. A. Bader, R. M. Horton, C. C. Hay, S. Kulp,
% M. Oppenheimer, D. Pollard, and B. H. Strauss (2017). Implications of
% Antarctic ice-cliff collapse and ice-shelf hydrofracturing mechanisms for
% sea-level projections. Earth?s Future. doi: 10.1002/2017EF000663. . 

% Download Kopp et al. 2017 code and files. 

url = 'https://github.com/bobkopp/LocalizeSL/releases/download/v2.0/LocalizeSL-2.0.zip';
websave('LocalizeSL-2.0.zip',url);
unzip('LocalizeSL-2.0.zip');

% set up path 
rootdir=[pwd, '/LocalizeSL'];
cd(rootdir)
addpath(fullfile(rootdir,'MFILES'));

selectedSite = 299; % PSMSL ID for Sewells Point, VA: http://www.psmsl.org/data/obtaining/stations/299.php

% Load the corefile for the data set of interest. In this case, we will use
% the corefile containing the DP16 projections from Kopp et al. (2017).
corefile=load(fullfile(rootdir,'IFILES/SLRProjections170113GRIDDEDcore-DP16-Pl5_15-BC.mat'));
ccclab='DP16';

% generate local samples

[sampslocrise,sampsloccomponents,siteids,sitenames,targyears,scens,cols] = LocalizeStoredProjections(selectedSite,corefile);
nameshort=sitenames{1}(1:3);

% output quantiles of projections

quantlevs=[.01 .05 .167 .5 .833 .95 .99 .995 .999];
WriteTableSLRProjection(sampslocrise,quantlevs,siteids,sitenames,targyears,scens,['LSLproj_quant' ccclab '_' nameshort '_']);

% output timing of height exceedances. I.E. survival functions
WriteTableSLRHeightExceedanceTiming(sampslocrise,[],siteids,sitenames,targyears,scens,1,['LSLheights_' ccclab '_' nameshort '_']);

% output Monte Carlo samples
% By components
WriteTableMC(sampsloccomponents,[],siteids,sitenames,targyears,scens,['LSLproj_MC_comp' ccclab '_' nameshort '_']);

% Total LSL projections
WriteTableMC(sampslocrise,[],siteids,sitenames,targyears,scens,['LSLproj_MC_' ccclab '_' nameshort '_']);

% Convert the output to a csv file that is column based for easier reading
% in R.
datarcp26 = dlmread('LSLproj_MC_DP16_SEW_299_rcp26.tsv','\t', 1, 0);
csvwrite('LSLproj_MC_DP16_SEW_299_rcp26.csv', transpose(datarcp26));

datarcp45 = dlmread('LSLproj_MC_DP16_SEW_299_rcp45.tsv','\t', 1, 0);
csvwrite('LSLproj_MC_DP16_SEW_299_rcp45.csv', transpose(datarcp45));

datarcp60 = dlmread('LSLproj_MC_DP16_SEW_299_rcp60.tsv','\t', 1, 0);
csvwrite('LSLproj_MC_DP16_SEW_299_rcp60.csv', transpose(datarcp60));

datarcp85 = dlmread('LSLproj_MC_DP16_SEW_299_rcp85.tsv','\t', 1, 0);
csvwrite('LSLproj_MC_DP16_SEW_299_rcp85.csv', transpose(datarcp85));

% output Monte Carlo samples without background trend,
% to allow incorporation of alternative estimates of background trend

% WriteTableMC(sampsloccomponents,1:23,siteids,sitenames,targyears,scens,'LSLProj_nobkgd_');
WriteTableMC(sampsloccomponents,setdiff(1:size(sampsloccomponents{1},2),cols.colGIA),siteids,sitenames,targyears,scens,['LSLproj_MC_nobkgd_' ccclab '_' nameshort '_']);

% output decomposition
WriteTableDecomposition(sampsloccomponents,quantlevs,siteids,sitenames,targyears,cols,scens,['LSLproj_decomp_' ccclab '_' nameshort '_']);

% pull GSL samples
[sampsGSLrise,sampsGSLcomponents,siteids,sitenames,targyears,scens,cols] = LocalizeStoredProjections(0,corefile);
WriteTableDecomposition(sampsGSLcomponents,quantlevs,siteids,sitenames,targyears,cols,scens,['GSLproj_' ccclab '_' nameshort '_']);