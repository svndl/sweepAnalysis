function [dataArray]=makeSweepDataArray(pdDataMatrix, dataHdr, freqNum, dataType, trialType)
% [dataArray]=makeSweepDataArray(pdDataMatrix, dataHdr, freqNum, dataType, trialType)
%
% Returns signal values (e.g., amplitudes) at the given frequencies 
% (freqNum) for the specified sweep. See the field "binLevels" in the 
% pdData structure for the corresponding bin values of the sweep.
%
% Requires the "dataMatrix" and "hdrFields" fields of a pdData structure.
%
% INPUTS:
%       pdDataMatrix: matrix of powerdiva data stored in field "dataMatrix"
%           in a pdData structure
%       dataHdr: cell array of column titles for pdDataMatrix; stored in 
%           field "hdrFields" in pdData 
%       freqNum: the index (or indices) of the desired frequence (e.g.,
%           1F1) in the field "freqsAnalyzed" in the pdData structure
%       dataType: a string giving the value (e.g., signal amplitude) to be 
%           extracted from the dataMatrix; must be a string that appears in
%           the "dataHdr" cell array. Default = 'Signal' (for signal
%           amplitude).
%       trialType: 
%             "average" --> returns matrix of mean values (default),
%                   where each row corresponds to a frequency in freqNum
%             "all" --> returns 3D array of values of each trial/subject 
%                   Each frequency = one row
%                   Each bin level = one column
%                   trial/subject = third dimension

% set default dataType
if nargin < 4 || isempty(dataType);
    dataType = 'Signal';
end

if isempty(find(strcmp(dataType,dataHdr), 1));
    error('dataType was not found in dataHdr; specify a different field')
end

% set default trialType
if nargin < 5 || isempty(trialType);
    trialType = 'average';
end

switch trialType
    case {'average','all'}
    otherwise
        error('trialType (parameter 1) must be either ''average'' or ''all''');
end


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

% find index of desired dataType
dataTypeIx = find(strcmp(dataType,dataHdr),1);

nBins=max(pdDataMatrix(:,binIx));   % number of bins
nFreq=length(freqNum);              % number of frequencies

switch trialType
    case 'average'    % get mean values
    
        % initialize matrix for holding mean values
        dataArray=zeros(nFreq,nBins);
        
        for i=1:nFreq;
                        
            % Get the mean trial rows corresponding to frequency i
            meanTrialRows = pdDataMatrix(:,trialIx) == 0 & pdDataMatrix(:,binIx) ~= 0 & pdDataMatrix(:,freqIx) == freqNum(i);

            % store mean trial values for all bins
            dataArray(i,:) = pdDataMatrix(meanTrialRows,dataTypeIx);
        end
    case 'all' % get values for each trial/subject
        nTrials=max(pdDataMatrix(:,trialIx)); % number of trials/subjects 
        
        % initialize matrix for holding values
        dataArray=zeros(nFreq,nBins,nTrials);
        
        for i=1:nFreq;
            for j=1:nTrials;

                % Get the rows corresponding to trial j and frequency i
                trialRows=pdDataMatrix(:,trialIx) == j & pdDataMatrix(:,binIx) ~= 0 & pdDataMatrix(:,freqIx) == freqNum(i);

                % store trial value for all bins
                dataArray(i,:,j) = pdDataMatrix(trialRows,dataTypeIx);
            end
        end
end
