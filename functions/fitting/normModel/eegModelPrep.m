function [eeg] = eegModelPrep(eeg,opt)
% [eeg] = eegModelPrep(eeg,opt)
%
% Filters data (optional), divides data into epochs (bins), and splits
% trials into train/test data in preparation for fitting model to data
%
% INPUTS:
%       eeg = data structure containing raw EEG signal; created by getEEG.m
%       opt = structure with fields 
%           opt.filt = "true","false" (default) -> whether to apply a
%               highpass filter to the data
%           opt.filtCutoff = cutoff frequency for highpass filter (required
%               if opt.filt = "true"); no default value
%           opt.filtN = filter order (default = 10)
%           opt.propTrain = proportion of trials to use for training model
%               (default = 0.8)
%           opt.seed = seed to use for random number generator when
%               dividing trials into train/test trials (default = 0.8)
%
% OUTPUTS: adds the following fields to the eeg structure:
%       eeg.epochSz = length of each epoch (i.e., number of samples/epoch)
%       eeg.filt = structure with info about the filter (if data filtered)
%       eeg.eegSignalFilt = filtered EEG signal (if data filtered)
%       eeg.goodTrials = 1 x nEpochs cell; each cell contains signal of
%           "good" trials for a given epoch; if data was filtered, then 
%           goodTrials will consist of the filtered signal
%       eeg.trainTrials = 1 x nEpochs cell; each cell is a boolean of the
%           good trials to use for training (1 = train, 0 = test)
%       eeg.trainR = mean of the "train" good trials 
%       eeg.testR = mean of the "test" good trials (note: will be NaN's if
%           opt.propTrain = 1 (i.e., if all of trials used for training)

% set defaults and check that all necessary opt fields are present

% default: don't filter
if ~isfield(opt,'filt'); opt.filt='false'; end 

% if opt.filt == true, make sure filter cutoff is specified
if strcmp(opt.filt,'true') && ~isfield(opt,'filtCutoff');
    error('Specify cutoff frequency for highpass filter using opt.filtCutoff');
end

% if opt.filt == true and filter order isn't specified, set to 10
if strcmp(opt.filt,'true') && ~isfield(opt,'filtN');
    opt.filtN=10;
end

% default % of trials to use for training = 80%
if ~isfield(opt,'propTrain'); opt.propTrain=0.8; end 

% default seed for random number generator
if ~isfield(opt,'seed'); opt.seed=1; end


% Get epoch (bin) divisions
% length of each epoch (assumes each epoch is the same size)
eeg.epochSz=size(eeg.eegSignal,2)/eeg.nEpochs; 
% start point of each epoch
epochStart=linspace(1,size(eeg.eegSignal,2)-(eeg.epochSz-1),eeg.nEpochs);
% end of each epoch
epochStop=epochStart+eeg.epochSz-1;

% filter data if opt.filt==true
if opt.filt;
    
    % create structure in eeg for holding filter info
    eeg.filt=struct(); 
    
    % filter settings
    eeg.filt.cutoff=opt.filtCutoff;
    eeg.filt.Wn=(eeg.filt.cutoff/(eeg.FreqHz/2));
    eeg.filt.N=opt.filtN;
    
    % create filter
    [eeg.filt.b,eeg.filt.a]=butter(eeg.filt.N,eeg.filt.Wn,'high');
    
    % initialize array for holding filtered time series
    eeg.eegSignalFilt = zeros(eeg.nTrials,size(eeg.eegSignal,2));
    
    % filter 
    for i=1:eeg.nTrials;
        eeg.eegSignalFilt(i,:)=filtfilt(eeg.filt.b,eeg.filt.a,eeg.eegSignal(i,:));
    end
end

% get good trials for each epoch 
eeg.goodTrials=cell(eeg.nEpochs,1);
for i=1:eeg.nEpochs;
    if opt.filt;    % if filtered, use filtered data
        eeg.goodTrials{i}=eeg.eegSignalFilt(eeg.IsEpochOK(:,i)==1,...
            epochStart(i):epochStop(i));
    else            % otherwise, use unfiltered data
        eeg.goodTrials{i}=eeg.eegSignal(eeg.IsEpochOK(:,i)==1,...
            epochStart(i):epochStop(i));
    end
end

% split into train and test trials
numGoodTrials=sum(eeg.IsEpochOK,1);     % number of trials that are good for each epoch
eeg.trainTrials=cell(eeg.nEpochs,1);    % initialize array for holding booleans of trials to use for training

numTrainTrials=ceil(numGoodTrials*opt.propTrain); % number of trials to use for training (rounds up)

% 1 = use trial for training, 0 = use trial for testing
for i=1:eeg.nEpochs;
    rng(i+opt.seed)
    eeg.trainTrials{i}=randsample([ones(1,numTrainTrials(i)) ...
        zeros(1,numGoodTrials(i)-numTrainTrials(i))],numGoodTrials(i));
end

% get mean trials for test/train
eeg.trainR=[];
eeg.testR=[];
for i=1:eeg.nEpochs;
    eeg.trainR=[eeg.trainR mean(eeg.goodTrials{i}(eeg.trainTrials{i}==1,:),1)]; % for training model
    eeg.testR=[eeg.testR mean(eeg.goodTrials{i}(eeg.trainTrials{i}==0,:),1)];   % for testing model
end

