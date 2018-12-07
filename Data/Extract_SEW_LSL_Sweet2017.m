% Edits by Kelsey Ruckert (klr324@psu.edu) April 2018
% with the purpose of extracting SLR data
% for Norfolk (Sewells Point).

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

% Last updated by Robert Kopp, robert-dot-kopp-at-rutgers-dot-edu, Thu Jan 26 16:57:11 EST 2017

addpath(pwd);
selectedSite = 299; % PSMSL ID for Sewells Point, VA: http://www.psmsl.org/data/obtaining/stations/299.php

% runSeaLevelConditionalDistributions
% Last updated by Robert Kopp, robert-dot-kopp-at-rutgers-dot-edu, Tue Nov 01 19:05:24 EDT 2016

% Download Sweet et al. 2017 code and data. 
% git clone https://github.com/bobkopp/LocalizeSL.git

% set up path 
% rootdir=[pwd, '/LocalizeSL'];
c% d(rootdir)
% addpath(fullfile(rootdir,'MFILES'));

addpath(pwd)
workdir='workdir-161102';
if ~exist(workdir,'dir')
    mkdir(workdir)
end
cd(workdir);

rootdir=[pwd,'/../../..'];
%rootdir='~/Dropbox/Code/LocalizeSL';
addpath(fullfile(rootdir,'MFILES'));

% Load the corefile for the data set of interest. In this case, we will use
% the corefile from Sweet et al. 2017.
savefilecore=fullfile(rootdir,'IFILES/SLRProjections161027GRIDDEDcore.mat');
p=load(savefilecore);

condtargyrs=[2100 2050 2030];
condtargs=[30 50 100 150 200 250 ;
           15 NaN NaN NaN NaN NaN ;
           9 NaN NaN NaN NaN NaN] * 10;
condtargwins=[20 20 20 50 50 150 ;
              20 20 20 20 20 20 ;
              10 10 10 10 10 10];


disp('Conditionalizing GSL...');
[projGSL,condsubscen]=ConditionalDistributionsGSL(p,condtargyrs,condtargs,condtargwins);

disp('Conditionalizing LSL...');
% projLOC=ConditionalDistributionsLSL(p,condsubscen);

defval('difftimestep',20);
defval('Nslice',20);
defval('substitutep',[]);
defval('qvals',[.5 .025 .975]);
% defval('qvals',[.5 .167 .833]);
defval('separatebkgd',1);

Nbkgdsamps=17;
docomponents=1;

if isempty(substitutep)
    clear substitutep; substitutep.filler=0;
end

if separatebkgd~=0
    separatebkgd=1;
else
    separatebkgd=0;
end


fullindex=1:length(p.targregions);
slicesub{1}=1:min(Nslice,length(fullindex));
lastmax=max(slicesub{end});
while lastmax<max(fullindex)
    slicesub{end+1}=((lastmax+1):min(Nslice+lastmax,length(fullindex)));
    lastmax=max(slicesub{end});
end

targyears=p.targyears;
targregions=p.targregions;

colsCOMP={p.colGIC,p.colGIS,p.colAIS,p.colTE}; colsCOMPlab={'GIC','GIS','AIS','Oc'};

% turn off background rates
if separatebkgd
    substitutep.rateprojs=p.rateprojs*0;
    substitutep.rateprojssd=p.rateprojssd*0;
end

sss=2;
    slicedp=slicep(p,slicesub{sss});
    clear wwprojLOC wwprojLOChi wwprojLOClo wwprojLOCrate wwprojLOCratehi wwprojLOCratelo;
    clear wwprojLOC0 wwprojLOC0hi wwprojLOC0lo wwprojLOC0rate wwprojLOC0ratehi wwprojLOC0ratelo;
    clear wwprojLOCc wwprojLOCchi wwprojLOCclo;
    
    dtstep=difftimestep;
    www=20;
        selectedSite=targregions(slicesub{sss}(www)); % selectedsite equals 299
        if docomponents
            [wsamps,wsampscomp] = LocalizeStoredProjections(selectedSite,slicedp,[1 3 4],substitutep);
        else
            [wsamps] = LocalizeStoredProjections(selectedSite,slicedp,[1 3 4],substitutep);
        end
        
        wlocalsamps=[wsamps{1}; wsamps{2} ; wsamps{3}];
        [wsampsdrise]=SampleSLRates(wsamps,targyears,dtstep);
        wlocalsamprates=[wsampsdrise{1} ; wsampsdrise{2} ;wsampsdrise{3}];
        
        wprojLOC0 = zeros(length(condsubscen),length(targyears));
        wprojLOC0hi = wprojLOC0;
        wprojLOC0lo = wprojLOC0;
        
        wprojLOC = zeros(length(condsubscen),length(targyears));
        wprojLOChi = wprojLOC;
        wprojLOClo = wprojLOC;
                
        wprojLOCrate = zeros(length(condsubscen),length(targyears)-1);
        wprojLOCratehi = wprojLOCrate;
        wprojLOCratelo = wprojLOCrate;
        
        wprojLOC0rate = zeros(length(condsubscen),length(targyears)-1);
        wprojLOC0ratehi = wprojLOC0rate;
        wprojLOC0ratelo = wprojLOC0rate;

        u=norminv(linspace(0,1,Nbkgdsamps+2)); u=u(2:end-1);
        wbkgdratesamps=reshape(slicedp.rateprojs(www)+u*slicedp.rateprojssd(www),1,1,[]);
        wbkgdlevels=bsxfun(@times,slicedp.targyears-2000,wbkgdratesamps);
        if ~separatebkgd
            wbkgdlevels=zeros(size(wbkgdlevels));
        end

        if docomponents
            wlocalsampscomp=[wsampscomp{1}; wsampscomp{2} ; wsampscomp{3}];
            wprojLOCc = zeros(length(condsubscen),length(targyears),length(colsCOMP));
            wprojLOCchi = wprojLOCc;
            wprojLOCclo = wprojLOCc;
        end
        
        ccclab={'0_3', '0_5', '1_0', '1_5', '2_0', '2_5'};
        
        for qqq=1:length(condsubscen)
            wlsamps0 = wlocalsamps(condsubscen{qqq},:);
            wlsamps=bsxfun(@plus,repmat(wlsamps0,1,1,Nbkgdsamps),wbkgdlevels);
            wlsamps=reshape(permute(wlsamps,[3 1 2]),Nbkgdsamps*size(wlsamps0,1),[]);
            
            wlsamprates0 = wlocalsamprates(condsubscen{qqq},:);
            wlsamprates = bsxfun(@plus,repmat(wlsamprates0,1,1,Nbkgdsamps),repmat(wbkgdratesamps,size(wlsamprates0,1),size(wlsamprates0,2)));
            wlsamprates=reshape(permute(wlsamprates,[3 1 2]),Nbkgdsamps*size(wlsamprates0,1),[]);
            
            wprojLOC(qqq,:)=quantile(wlsamps,qvals(1));
            wprojLOChi(qqq,:)=quantile(wlsamps,qvals(3));
            wprojLOClo(qqq,:)=quantile(wlsamps,qvals(2));
            wprojLOCrate(qqq,:)=quantile(wlsamprates,qvals(1));
            wprojLOCratehi(qqq,:)=quantile(wlsamprates,qvals(3));
            wprojLOCratelo(qqq,:)=quantile(wlsamprates,qvals(2));
            
            wprojLOC0(qqq,:)=quantile(wlsamps0,qvals(1));
            wprojLOC0hi(qqq,:)=quantile(wlsamps0,qvals(3));
            wprojLOC0lo(qqq,:)=quantile(wlsamps0,qvals(2));
            wprojLOC0rate(qqq,:)=quantile(wlsamprates0,qvals(1));
            wprojLOC0ratehi(qqq,:)=quantile(wlsamprates0,qvals(3));
            wprojLOC0ratelo(qqq,:)=quantile(wlsamprates0,qvals(2));
            
            csvwrite(['sweet_etal_2017_SEW_' ccclab{qqq} '.csv'],wlsamps)

        end
        
        
