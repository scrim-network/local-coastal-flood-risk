% Edits by Kelsey Ruckert (klr324@psu.edu) April 2018
% with the purpose of extracting probabilistic SLR data
% for Norfolk (Sewells Point).

% Original file: https://github.com/bobkopp/LocalizeSL/blob/master/notebooks/runLocalizeSL_Rasumssen2018.ipynb

% Copyright (C) 2018 by Robert E. Kopp

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

% D. J. Rasmussen, K. Bittermann, M. K. Buchanan, S. Kulp, B. H. Strauss, R. E. 
% Kopp, and M. Oppenheimer (2018). Extreme sea level implications of 
% 1.5°C, 2.0?°C, and 2.5?°C temperature stabilization targets in the 21st and 
% 22nd centuries. _Environmental Research Letters_ 13, 034040. doi: 
% 10.1088/1748-9326/aaac87.

% Download Rasmussen et al. 2018 code and data. 
% git clone https://github.com/bobkopp/LocalizeSL.git

% set up path 
rootdir=[pwd, '/LocalizeSL'];
cd(rootdir)
addpath(fullfile(rootdir,'MFILES'));

selectedSite = 299; % PSMSL ID for Sewells Point, VA: http://www.psmsl.org/data/obtaining/stations/299.php

% Load the corefile for the data set of interest. In this case, we will use
% the corefile from Rasmussen et al. 2018.
corefile=load(fullfile(rootdir,'IFILES/SLRProjections180124GRIDDEDcore_Tscens.mat'));
ccclab='Ras18';

% specify scenario labels and scenarios to use
% important since we differ from defaults here
scenlabs={'tmp15','tmp20','tmp25'};
selscens=[1 2 3];

% generate local samples
[sampslocrise,sampsloccomponents,siteids,sitenames,targyears,scens,cols] = LocalizeStoredProjections(selectedSite,corefile,selscens);
nameshort=sitenames{1}(1:3);

% output quantiles of projections

quantlevs=[.01 .05 .167 .5 .833 .95 .99 .995 .999];
WriteTableSLRProjection(sampslocrise,quantlevs,siteids,sitenames,targyears,scens,['LSLproj_quant' ccclab '_' nameshort '_']);

% output timing of height exceedances. I.E. survival functions
WriteTableSLRHeightExceedanceTiming(sampslocrise,[],siteids,sitenames,targyears,scens,1,['LSLheights_' ccclab '_' nameshort '_']);

% output Monte Carlo samples
% Total LSL projections
WriteTableMC(sampslocrise,[],siteids,sitenames,targyears,scens,['LSLproj_MC_' ccclab '_' nameshort '_']);

% Convert the output to a csv file that is column based for easier reading
% in R.
data1p5degree = dlmread('LSLproj_MC_Ras18_SEW_299_1p5degree.tsv','\t', 1, 0);
csvwrite('LSLproj_MC_Ras18_SEW_299_1p5degree.csv', transpose(data1p5degree));

data2p0degree = dlmread('LSLproj_MC_Ras18_SEW_299_2p0degree.tsv','\t', 1, 0);
csvwrite('LSLproj_MC_Ras18_SEW_299_2p0degree.csv', transpose(data2p0degree));

data2p5degree = dlmread('LSLproj_MC_Ras18_SEW_299_2p5degree.tsv','\t', 1, 0);
csvwrite('LSLproj_MC_Ras18_SEW_299_2p5degree.csv', transpose(data2p5degree));

% output Monte Carlo samples without background trend,
% to allow incorporation of alternative estimates of background trend
WriteTableMC(sampsloccomponents,1:23,siteids,sitenames,targyears,scens,['LSLproj_MC_nobkgd_' ccclab '_' nameshort '_']);

% output decomposition
WriteTableDecomposition(sampsloccomponents,quantlevs,siteids,sitenames,targyears,cols,scens,['LSLproj_decomp_' ccclab '_' nameshort '_']);

% pull GSL samples
[sampsGSLrise,sampsGSLcomponents,GSLsiteids,GSLsitenames,GSLtargyears,GSLscens,GSLcols] = LocalizeStoredProjections(0,corefile,selscens);
WriteTableDecomposition(sampsGSLcomponents,quantlevs,GSLsiteids,GSLsitenames,GSLtargyears,GSLcols,GSLscens,['GSLproj_' ccclab '_' nameshort '_']);
