% Generate plot of subplot
dataDir1 = '/Users/Nathan/Desktop/Research/CVIdata/CVI_NT_20150407_1127/Exp_TEXT_PD1010_5_Cz/';
dataDir2 = '/Users/Nathan/Desktop/Research/CVIdata/CVI3top34_20160224_1020/Exp_TEXT_PD1010_5_Cz';

newmap = containers.Map;
newmap('O1-Cz') = 21;
newmap('O2-Cz') = 22;
newmap('Oz-Cz') = 23;
newmap('PO7-Cz') = 24;
newmap('PO8-Cz') = 25;

condDesc = {'Cond1', 'Cond2', 'Cond3', 'Cond4', 'Cond5'};
dataType = 'RLS';
groupNames = {'CVI_NT', 'CVI'};

% Get indices that would otherwise be for user query. This allows user to
% make sure this script plots the correct channels/freqs.
channels = [];
currPdData = makeDataStructure(dataDir1, [], dataType, groupNames{1}, condDesc, newmap);
channels = [channels currPdData(1,:).channel];
disp(channels);
disp(condDesc);
disp(currPdData(1,1).freqsAnalyzed);

% Selecting indices for plot.
channelNames = keys(newmap);
valuesInChanMap = cell2mat(values(newmap));
channelIdxToPlot = 1:length(valuesInChanMap);
conditionsToPlot = [1];
freqsToPlot = [1,2];

% Do the plotting
totalNumPlots = length(conditionsToPlot)*length(freqsToPlot)*length(channelIdxToPlot);
m=1;
firstRow = 0;
figure;
set(gca,'FontSize',12.5);
dimY = 2; % Need to change this if you have more plots in the figure (i.e. choosing more frequencies)
dimX = 5;
assert((dimY*dimX) == totalNumPlots);
for i = 1:length(freqsToPlot)
    fprintf('Plotting channel: %d\n', channelIdxToPlot(i));
    for j = 1:length(conditionsToPlot)
        for k = 1:length(channelIdxToPlot)
            idxInput = [k,j,1,i];
            subplot(dimY,dimX,m)
            plotGroupComparison({dataDir1, dataDir2}, groupNames, [], ...
                                condDesc, 0, idxInput, dataType, newmap, 0);
            m = m+1;
            if i == 1
                firstRow = 1;
            else
                firstRow = 0;
            end
            if firstRow == 1
                title(channelNames{k});
            end
        end
    end
end
