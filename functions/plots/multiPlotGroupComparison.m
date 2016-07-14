%% Generate plot of subplot
clear
clc
%% Setting up data

% Change each data directory to the path to the data you want to compare
dataDir1 = '/Users/Nathan/Desktop/Research/CVIdata/CVI_NT_20150407_1127/Exp_TEXT_PD1010_5_Cz/';
dataDir2 = '/Users/Nathan/Desktop/Research/CVIdata/CVI3top34_20160224_1020/Exp_TEXT_PD1010_5_Cz';

% This "map" is only created because in the CVI data, the channel names are not of the
% format: hc%d.  The "map" is not needed if the channel names are of the format described.
% See getSweepDataFlex.m and changeChanNames.m to understand the usage of the maps.
% The values of the map are arbitrary (you can set them to whatever you
% like).
newmap = containers.Map;
newmap('Oz-Cz') = 26;
newmap('PO7-Cz') = 7;
newmap('O1-Cz') = 1;
newmap('O2-Cz') = 2;
newmap('PO8-Cz') = 8;

% Change the following as needed, corresponding to the type of data that is being dealt with
condDesc = {'Cond1', 'Cond2', 'Cond3', 'Cond4', 'Cond5'};
dataType = 'RLS';
groupNames = {'CVI_NT', 'CVI'};

% Get indices that would otherwise be for user query (try to use plotGroupComparison.m on the data
% first. This allows user to make sure this script plots the correct channels/freqs.

% Change the arguments to makeDataStructure() as necessary.  If "newmap" was not created above, 
% we do not need it here for makeDataStructure(). makeDataStructure.m has a call to getSweepDataFlex.m,
% which in turn, uses the map (if applicable) to reassign channel types.
currPdData = makeDataStructure(dataDir1, [], dataType, groupNames{1}, condDesc, newmap);

% If the dataset has 128 channels, then the length of this array should be
% 128. And to index into it for plotting, you would just change
% 'channelIdxToPlot' (below) to be the channels you want (i.e. [2,7,50,75]
% would mean you want channels 2, 7, 50, 75 for the 128 channel data).
channels = [];
channels = [channels currPdData(1,:).channel];
condDesc = {currPdData(:,1).conditionName};

% The point of these disp() is to show the user which indices are to be used when choosing which
% conditions or frequencies to look at. 
fprintf('All channels:');
disp(channels);
fprintf('All conditions:');
disp(condDesc);
fprintf('All frequencies:\n');
disp(currPdData(1,1).freqsAnalyzed);

% Selecting indices for plot. Refer to the output of the disp() from above
% to make sure that you are selecting the correct channel(s), condition(s)
% and frequencies.
valuesInChanMap = cell2mat(values(newmap));

% In this case, you are plotting channels 24 and 21, meaning that you are
% plotting O2-Cz and Oz-Cz (refer to the map above).
% channelIdxToPlot = 1:length(valuesInChanMap);
channelIdxToPlot = [4,5];

% Change this if you want to look at other conditions or frequencies
conditionsToPlot = [1]; % One condition at a time
freqsToPlot = [1,3];

%% Do the plotting
totalNumPlots = length(conditionsToPlot)*length(freqsToPlot)*length(channelIdxToPlot);
m=1;
firstRow = 0;
figure;
set(gca,'FontSize',12.5);

% Need to change this if you have more plots in the figure (i.e. choosing more frequencies)
% dimY * dimX is the number of subplots in the plot to be created.
dimY = 2;
dimX = 2;
assert((dimY*dimX) == totalNumPlots);

% Do the group comparison plots here
for i = 1:length(freqsToPlot)
    fprintf('Plotting frequency: %d\n', freqsToPlot(i));
    for j = 1:length(conditionsToPlot)
        fprintf('Plotting condition: %d, %s\n', conditionsToPlot(j), condDesc{conditionsToPlot(j)});
        for k = 1:length(channelIdxToPlot)
            fprintf('Plotting channel: %d\n', channels(channelIdxToPlot(k)));
            cond = conditionsToPlot(j);
            chan = channelIdxToPlot(k);
            freq = freqsToPlot(i);
            idxInput = [chan,cond,1,freq];
            subplot(dimY,dimX,m)
            plotGroupComparison({dataDir1, dataDir2}, groupNames, [], ...
                                condDesc, 0, idxInput, dataType, newmap, 0);
            m = m+1;
            if i == 1
                firstRow = 1;
            else
                firstRow = 0;
            end
            % The title of the plot corresponds to the map from above. i.e.
            % a title of '26' means that the channel is 'Oz-Cz'.
            if firstRow == 1
                title(channels(chan));
            end
            ylabel(currPdData(1,1).freqsAnalyzed(freq));
        end
    end
end
