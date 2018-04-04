% Edits by Kelsey Ruckert (klr324@psu.edu) Feb. 2018
% with the purpose of extracting probabilistic SLR data
% for Norfolk (Sewells Point).

% Copyright (C) 2015 by Robert E. Kopp

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

% R. E. Kopp, R. M. Horton, C. M. Little, J. X. Mitrovica, M. Oppenheimer,
% D. J. Rasmussen, B. H. Strauss, and C. Tebaldi (2014). Probabilistic 21st
% and 22nd century sea-level projections at a global network of tide  gauge
% sites. Earth's Future 2: 287?306, doi:10.1002/2014EF000239. 

% Download Kopp et al. 2014 code and files. 

url = 'https://github.com/bobkopp/LocalizeSL/releases/download/v1.2/LocalizeSL-1.2.zip';
websave('LocalizeSL-1.2.zip',url);
unzip('LocalizeSL-1.2.zip');

selectedSite = 299; % PSMSL ID for Sewells Point, VA: http://www.psmsl.org/data/obtaining/stations/299.php

% set up path 
rootdir=[pwd, '/LocalizeSL'];
cd(rootdir)
corefile=fullfile(rootdir,'IFILES/SLRProjections140523core.mat');
addpath(fullfile(rootdir,'MFILES'));

% generate local samples

[sampslocrise,sampsloccomponents,siteids,sitenames,targyears,scens,cols] = LocalizeStoredProjections(selectedSite,corefile);

% output quantiles of projections

quantlevs=[.01 .05 .167 .5 .833 .95 .99 .995 .999];
WriteTableSLRProjection(sampslocrise,quantlevs,siteids,sitenames,targyears,scens);

% output Monte Carlo samples
WriteTableMC(sampslocrise,[],siteids,sitenames,targyears,scens);

% Convert the output to a csv file that is column based for easier reading
% in R.
datarcp26 = dlmread('LSLproj_MC_299_rcp26.tsv','\t', 1, 0);
csvwrite('LSLproj_MC_299_rcp26.csv', transpose(datarcp26));

datarcp45 = dlmread('LSLproj_MC_299_rcp45.tsv','\t', 1, 0);
csvwrite('LSLproj_MC_299_rcp45.csv', transpose(datarcp45));

datarcp60 = dlmread('LSLproj_MC_299_rcp60.tsv','\t', 1, 0);
csvwrite('LSLproj_MC_299_rcp60.csv', transpose(datarcp60));

datarcp85 = dlmread('LSLproj_MC_299_rcp85.tsv','\t', 1, 0);
csvwrite('LSLproj_MC_299_rcp85.csv', transpose(datarcp85));

% output Monte Carlo samples with only background trend,
% to allow incorporation of background trend to other alternative estimates

WriteTableMC(sampsloccomponents,24,siteids,sitenames,targyears,scens,'LSLProj_bkgd_');

% Convert the output to a csv file that is column based for easier reading
% in R.
datarcp26 = dlmread('LSLProj_bkgd_299_rcp26.tsv','\t', 1, 0);
csvwrite('LSLProj_bkgd_299_rcp26.csv', transpose(datarcp26));

datarcp45 = dlmread('LSLProj_bkgd_299_rcp45.tsv','\t', 1, 0);
csvwrite('LSLProj_bkgd_299_rcp45.csv', transpose(datarcp45));

datarcp60 = dlmread('LSLProj_bkgd_299_rcp60.tsv','\t', 1, 0);
csvwrite('LSLProj_bkgd_299_rcp60.csv', transpose(datarcp60));

datarcp85 = dlmread('LSLProj_bkgd_299_rcp85.tsv','\t', 1, 0);
csvwrite('LSLProj_bkgd_299_rcp85.csv', transpose(datarcp85));

% output Monte Carlo samples without background trend,
% to allow incorporation of alternative estimates of background trend

WriteTableMC(sampsloccomponents,1:23,siteids,sitenames,targyears,scens,'LSLProj_nobkgd_');

% output decomposition
WriteTableDecomposition(sampsloccomponents,quantlevs,siteids,sitenames,targyears,cols,scens);

% pull GSL samples
[sampsGSLrise,sampsGSLcomponents,siteids,sitenames,targyears,scens,cols] = LocalizeStoredProjections(0,corefile);
WriteTableDecomposition(sampsGSLcomponents,quantlevs,siteids,sitenames,targyears,cols,scens);