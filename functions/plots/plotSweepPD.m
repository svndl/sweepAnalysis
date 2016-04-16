function [figNum,plotNum,threshInfo] = plotSweepPD(plotType,pdDataMatrix,dataHdr,binLevels,freqNum,errType,plotThreshFit,plotOpt,figHandles,newfig)

% function [figNum,plotNum,threshInfo] = plotSweepPD(plotType,pdDataMatrix,dataHdr,binLevels,freqNum,errType,plotThreshFit,plotOpt,figHandles)
%   
% Create a plot that looks like the PowerDiva style plots, using 
% precomputed values from PowerDiva for the means and noise estimates. The
% additional property of this plot is that it also shows errorbars based on
% the desired type of error estimation process specified in errType
% (default 'SEM') for the plotType 'Ampl'
%
% INPUTS:
%       plotTypes: 'Ampl' (amplitude in muV) or 'SNR' for Snr (no errorbars)
%       plotOpt: structure holding options for plots (e.g., colors). Fields and defaults are
%           plotOpt.dataColor = 'k' --> color of data points of plot
%           plotOpt.bins2plot = 1:length(binLevels); 
%               --> vector of indexes of bins to plot (default is all bins)
%
% This function is only meant to be called once for a particular sweep. The
% logic is that you create a figure and then call this function each time
% you want to plot another sweep (i.e. from a different group or a
% different condition)

% find index of each field saved in data Matrix
% (i.e., what does each column in dataMatrix correspond to?)
for k = 1:length(dataHdr)
    switch dataHdr{k}
        case 'iTrial'
            trialIx = k;
        case 'iFr'
            freqIx = k;
        case 'iBin'
            binIx = k;
        case 'Signal'
            amplIx = k;
        case 'SNR'
            SNRIx = k;
        case 'Noise'
            noiseIx = k;
    end
end

switch plotType
    case {'Ampl','SNR'}
    otherwise
        error('PlotType (parameter 1) must be either ''Ampl'' or ''SNR''');
end
if nargin < 6 || isempty(errType)
    errType = 'SEM';
end

if nargin < 7 || isempty(plotThreshFit), plotThreshFit = 0; end

if plotThreshFit && ~strcmp(plotType,'Ampl')
    fprintf('You can only plot threshold fits if the plotType is set to ''Ampl''.');
    plotThreshFit = 0;
end

% set all plot options to default if none specified
if nargin < 8 || isempty(plotOpt)
    plotOpt.dataColor = 'k';
    plotOpt.bins2plot = 1:length(binLevels);
end

% This is an override so that multiPlotGroupComparison.m can plot subplots
if isempty(newfig)
    newfig = 1;
end

% set any missing plot options to default
if ~isfield(plotOpt,'dataColor'); plotOpt.dataColor='k'; end
if ~isfield(plotOpt,'bins2plot'); plotOpt.bins2plot=1:length(binLevels); end 

% check that indices in plotOpt.bins2plot doesn't exceed dimensions of
% binLevels
if max(plotOpt.bins2plot)>length(binLevels);
    disp(['The number of bins in binLevels is ' num2str(length(binLevels))])
    disp(['You have asked to plot the ' num2str(max(plotOpt.bins2plot)) 'th bin.'])
    error('Please make the indices in plotOpt.bins2plot no greater than the number of bins in binLevels.')
end

hexagArrag = false;
if nargin < 9 || isempty(figHandles)
    if newfig == 1
        figure;
        set(gcf,'Color','w');
        set(gca,'FontSize',20);
    end
    figInfo = gcf;
    if ~isnumeric(figInfo)
        figNum = figInfo.Number;        
    else
        figNum = figInfo;
    end
else
    figNum = figHandles(1);
    figure(figNum);
    if length(figHandles)==4
        subplot(figHandles(2),figHandles(3),figHandles(4));
    elseif length(figHandles)>4
        subplot('position',figHandles(2:end));
        hexagArrag = true;
    end
    if isLogSpaced(binLevels)
        set(gca,'XScale','log');
    end
end

if hexagArrag
    mrkrSz = 10;
else
    mrkrSz = 14;
end

% number of bins = number of bins specified by user 
nBins = length(plotOpt.bins2plot); 

threshFitted = 0;
threshVal = nan;
slopeVal = nan;
fitBinRange = nan(1,2);

% Get the mean trial data only (as computed by PowerDiva):
% note: gets mean trial data for ALL bins, not just plotOpt.bins2plot bins
meanTrialRows = pdDataMatrix(:,trialIx) == 0 & pdDataMatrix(:,binIx) ~= 0 & pdDataMatrix(:,freqIx) == freqNum;
meanTrialMat = pdDataMatrix(meanTrialRows,:);

switch plotType
    case 'Ampl'
        indexToPlot = amplIx;
        % Get the error estimates for confidence intervals on the means
        % (for ALL bins, not just bins in plotOpt.bins2plot)
        amplErrorRange = nan(2,max(pdDataMatrix(:,binIx)));
        for binNum = 1:max(pdDataMatrix(:,binIx));
            
            xyData = getXyData(pdDataMatrix,dataHdr,binNum,freqNum);            
            try
                amplErrorRange(:,binNum) = fitErrorEllipse(xyData,errType);
            catch
            end
        end
        if ~hexagArrag, ylabel('Amplitude (\muV)'), end
    case 'SNR'
        indexToPlot = SNRIx;
        if ~hexagArrag, ylabel('SNR'), end
end

% Compute threshold & slope (and associated variables) if desired:
% note: only uses bins specified by user
if plotThreshFit && strcmp(plotType,'Ampl')
    clear sweepMatSubjects;
    sweepMatSubjects = constructSweepMatSubjects(pdDataMatrix,dataHdr,freqNum);
    
    [threshVal,threshStdErr,slopeVal,slopeStdErr,tLSB,tRSB,~,saveY,saveXX] = getThreshScoringOutput(sweepMatSubjects, binLevels(plotOpt.bins2plot));
    fitBinRange = [tLSB,tRSB];
    if isnan(threshVal)
        fprintf('No scoring function could be fitted.\n');
    else
        % save line info to plot after everything else so it's "on top"
        fprintf('Thresh = %1.2f, Slope = %1.2f, Range=[%d,%d].\n',threshVal,slopeVal,fitBinRange)
        threshFitted = 1;
    end
    
    if threshFitted
        threshInfo.xx = saveXX;
        threshInfo.YY = saveY;
        threshInfo.threshVal = threshVal;
        threshInfo.slopeVal = slopeVal;
        threshInfo.threshStdErr = threshStdErr;
        threshInfo.slopeStdErr = slopeStdErr;
        threshInfo.fitBinRange = fitBinRange;
    else
        threshInfo.xx = nan;
        threshInfo.YY = nan;
        threshInfo.threshVal = nan;
        threshInfo.slopeVal = nan;
        threshInfo.threshStdErr = nan;
        threshInfo.slopeStdErr = nan;
        threshInfo.fitBinRange = nan;
    end    
end


figure(figNum);
% Plot mean data (filled circles):
if isLogSpaced(binLevels)
    hold on;
    plotNum = semilogx(binLevels(plotOpt.bins2plot),...
        meanTrialMat(plotOpt.bins2plot,indexToPlot),'ko-','Color',...
        plotOpt.dataColor,'MarkerFaceColor',plotOpt.dataColor,'LineWidth',2);   
    if strcmp(plotType,'Ampl')
        % with noise estimates (empty squares)
        semilogx(binLevels(plotOpt.bins2plot),...
            meanTrialMat(plotOpt.bins2plot,noiseIx),'ks','Color',...
            plotOpt.dataColor,'MarkerSize',mrkrSz);
    end
else
    hold on;
    plotNum = plot(binLevels(plotOpt.bins2plot),...
        meanTrialMat(plotOpt.bins2plot,indexToPlot),'ko-','Color',...
        plotOpt.dataColor,'MarkerFaceColor',plotOpt.dataColor,'LineWidth',2);
    if strcmp(plotType,'Ampl')
        plot(binLevels(plotOpt.bins2plot),...
            meanTrialMat(plotOpt.bins2plot,noiseIx),'ks','Color',...
            plotOpt.dataColor,'MarkerSize',mrkrSz);
    end
end

% Plot error bars on amplitude values if appropriate:
if strcmp(plotType,'Ampl')
    % don't use built-in Matlab errorbar function because it makes ugly 
    % "tees," the horizontal lines on top & bottom
    for binNum = plotOpt.bins2plot;
        try
            plot([binLevels(binNum) binLevels(binNum)],...
                [amplErrorRange(1,binNum) amplErrorRange(2,binNum)],...
                'k-','Color',plotOpt.dataColor,'LineWidth',2);
        catch            
            fprintf('Error bars could not be plotted on your data, probably your data do not contain >1 sample.');
        end
    end
end

% Plot linear fit used to extrapolate to the zero-crossing/threshold:
if plotThreshFit && threshFitted    
    
    % add shaded error region on threshold values:
    h = fill([threshVal-threshStdErr threshVal+threshStdErr threshVal+threshStdErr threshVal-threshStdErr],...
        [min(ylim) min(ylim) max(ylim) max(ylim)],'k');
    set(h,'LineStyle','none','FaceAlpha',0.2,'FaceColor',plotOpt.dataColor);
    
    if isLogSpaced(binLevels)
        set(gca,'XScale','log');
        semilogx(saveXX,saveY,'k-','LineWidth',3);
        semilogx(threshVal,0,'kd','MarkerSize',18,...
            'MarkerFaceColor',plotOpt.dataColor,'LineWidth',3);
    else
        plot(saveXX,saveY,'k-','LineWidth',3);
        plot(threshVal,0,'kd','MarkerSize',18,...
            'MarkerFaceColor',plotOpt.dataColor,'LineWidth',3);
    end

    text((threshVal-min(xlim))/2+min(xlim),0.3*max(ylim),...
        sprintf('thresh (slope): %2.3f+/-%2.3f (%2.3f+/-%2.3f)',threshVal,...
        threshStdErr,slopeVal,slopeStdErr),'Color',plotOpt.dataColor,'FontSize',12);

end

% Make some final plot settings:
set(gca,'XTick',binLevels([plotOpt.bins2plot(1) floor(nBins/2) plotOpt.bins2plot(end)]))
if ~hexagArrag
    xlabel('Bin Values')
    set(gca,'ticklength',1.5*get(gca,'ticklength'))
else
    axis square
    box off
    set(gca,'ticklength',2.5*get(gca,'ticklength'))
end
set(gca,'tickDir','out')
