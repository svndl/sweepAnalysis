function plotGroupComparison(dataDirs, groupNames, channels, condDesc, user, idxInput, sweepEstType, newChanMap, newfig)
% plotGroupComparison(dataDirs, groupNames, channels, condDesc, sweepEstType)
%
% Variables in square brackets,[var], indicate that they are optional.
%
% dataDirs       : Cell array of group data file directories
% groupNames     : Descriptive names of the groups being compared. The
%                  order of the group names must be consistent with the
%                  array of the data directory paths (cell array)
% channels       : Vector of channel numbers
% condDesc       : Cell array of condition description. Must be consistent
%                  with the array of condition numbers.
% [user]         : Set to 1 if want to generate a single plot. Set to 0 if
%                  want to generate plot with multiple subplots. If not
%                  specified, default is 1 (user input required).
% [idxInput]     : Set to the indicies of the channels, conditions, plot
%                  type, and frequencies wanted for the multi group 
%                  comparison plot.
% [sweepEstType] : 'RLS' or 'DFT'. If not specified, default is 'RLS'
% [newChanMap]   : Map that maps channel names to numbers
% [newfig]       : Tells us if we want a new figure to be created in
%                  plotSweepPD.m. Set to 1 if new figure wanted. Else, 0.
%
% This function assumes you only want to plot one channel, one frequency,
% one condition per group.

    if (nargin < 5) || (isempty(user))
        user = 1;
        fprintf('User input required.\n');
    end

    if (nargin < 6) || (isempty(idxInput))
        idxInput = [];
        fprintf('User input required.\n');
    end
    
    if (nargin < 7) || (isempty(sweepEstType))
        sweepEstType = 'RLS';
        fprintf('No data type specified. Default (RLS) is assumed.\n');
    end

    if (nargin < 8) || (isempty(newChanMap))
        newChanMap = containers.Map;
    end
    
    if (nargin < 9) || (isempty(newfig))
        newfig = 1;
    end
    
    % If there is no user input, we need the number if indices to be 4 to
    % match (in this specific order):
    %   1 - Channel index
    %   2 - Condition index
    %   3 - Plot type index
    %   4 - Frequency index
    if ~(isempty(idxInput))
        assert(length(idxInput) == 4);
    end
    
    % Get number of groups we are comparing
    numToCompare = length(dataDirs);
    
    colors = [1 0 0; 0 0 1; 0 1 0; 1 0 1; 0 1 1; 0 0 0];
    figNum = [];
    plotNum = nan(1, numToCompare);
    plotThresholdFits = true; % set to false if not desired
    oldChannels = channels;
    for i = 1:numToCompare
        fprintf('Plotting for group: %s\n', groupNames{i});
        
        % Get pd data
        currPdData = makeDataStructure(dataDirs{i}, channels, sweepEstType, groupNames{i}, condDesc, newChanMap);
        
        % Get channels to compare
        channels = [];
        for n = 1:size(currPdData, 2)
            % Can do this because channels same for all structs in pd data.
            channels = [channels currPdData(1,n).channel];
        end
        if user == 1
            fprintf('Select channel:\n');
            selectedChan = userQuery(channels);
            while length(selectedChan) ~= 1
                fprintf('Select only one channel.\n');
                selectedChan = userQuery(channels);
            end
            selectedChan = selectedChan(1); % Can do this since length == 1
        else
            selectedChan = idxInput(1);
        end
        
        % Get condition to compare
        if user == 1
            fprintf('Select condition:\n');
            selectedCond = userQuery(condDesc);
            while length(selectedCond) ~= 1
                fprintf('Select only one condition.\n');
                selectedCond = userQuery(condDesc);
            end
            selectedCond = selectedCond(1); % Can do this since length == 1
        else
            selectedCond = idxInput(2);
        end
        
        % Get plot type
        plotType = {'Ampl', 'SNR'};
        if user == 1
            fprintf('Select plot type:\n');
            selectedPlot = userQuery(plotType);
            while length(selectedPlot) ~= 1
                fprintf('Select only one plot type.\n');
                selectedPlot = userQuery(selectedPlot);
            end
            selectedPlot = plotType{selectedPlot(1)}; % Can do this since length == 1
        else
            selectedPlot = plotType{idxInput(3)};
        end
        
        % Get frequency to look at
        frequencies = currPdData(selectedCond,selectedChan).freqsAnalyzed;
        if user == 1
            fprintf('Select frequency:\n');
            selectedFreq = userQuery(frequencies);
            while length(selectedFreq) ~= 1
                fprintf('Select only one frequency.\n');
                selectedFreq = userQuery(currPdData(selectedCond,selectedChan).freqsAnalyzed);
            end
            selectedFreq = selectedFreq(1);
        else
            selectedFreq = idxInput(4);
        end
        
        % We are plotting each group (no longer each condition, like in the
        % example.m code)
        plotOpt.dataColor=colors(i,:); % set plot options
        [figNum, plotNum(i)] = plotSweepPD(selectedPlot, currPdData(selectedCond, selectedChan).dataMatrix, ...
                                                         currPdData(selectedCond, selectedChan).hdrFields,  ...
                                                         currPdData(selectedCond, selectedChan).binLevels,  ...
                                                         selectedFreq, 'SEM', plotThresholdFits, plotOpt, ...
                                                         figNum, newfig);
        % Reset channels because they get modified each loop
        channels = oldChannels;
    end
    legend(plotNum(~isnan(plotNum)), groupNames,'Location','NorthWest')
end

function selectedArr = userQuery(listOfValues)
% listOfValues is an array (could be cell or double array)

    % Just check type/class of array contents
    if isfloat(listOfValues(1))
        number = 1;
    else
        number = 0;
    end
    msg = 'Enter the index into this array of values: [ ';
    for i = 1:length(listOfValues)
        if number == 1
            currVal = num2str(listOfValues(i));
        else
            currVal = listOfValues{i};
        end
        msg = [msg, currVal];
        msg = [msg, ' '];
    end
    msg = [msg, '], separated by spaces: '];
    fprintf(msg);
    selected = input('', 's');
    selectedIndices = regexp(selected, ' ', 'split');
    selectedArr = zeros(1,length(selectedIndices));
    for i = 1:length(selectedIndices)
        selectedArr(i) = str2double(selectedIndices{i});
    end
end


