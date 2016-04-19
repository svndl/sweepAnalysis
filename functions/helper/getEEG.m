function [eeg] = getEEG(dataDir,conditions,channels)
% function [eeg] = getEEG(dataDir,conditions,channels)
%
% Gets EEG signal for all trials of specified condition from power diva
% export and stores in structure along with associated metadata (including
% the number of epochs, epoch quality, sampling frequency, and signal
% units).
%
% INPUTS:
%       dataDir = string giving folder that contains matlab workspaces 
%           of EEG data; workspaces are in the format Raw_cXXX_tYYY.mat,
%           where XXX is a Condition number and YYY is a Trial number
%       conditions = vector (numeric) giving the numbers of the conditions
%           to import
%       channels = vector (numeric) giving the numbers of the channels to
%           import
%
% OUTPUTS:
%       eeg = structure containing the EEG signals along with metadata.
%           Each cell in the eeg structure contains the data for a given 
%           condition (each row = same condition) and channel (each column 
%           = same channel).
%           Fields in each cell:
%               nTrials = number of trials
%               DataUnitStr = string giving units of signal, one of 
%                   {"Volts", "microVolts", "AmpsPerSqMtr", "picoAmpsPerSqMtr"}
%               FreqHz = data acquisition rate in Hz
%               nEpochs = the number of epochs in the trials
%               nPreludeEpochs = the number of prelude/postlude epochs
%               eegSignal = matrix of the EEG signal, nTrials x signal length
%               IsEpochOK = epoch quality, nTrials x nEpochs; 1 = epoch is
%                   "good", 0 = epoch is "bad" (i.e., contains artifact)

eeg=struct();               % create structure for holding data
nConds=length(conditions);  % number of conditions to import
nChan=length(channels);     % number of channels to import

% get list of files in directory
allFiles=cellstr(ls(dataDir));

for i=1:nConds;
    cond=num2str(conditions(i),'%03d');              % condition # in format ###
    condFiles=allFiles(strncmp(['Raw_c' cond],...    % files for condition
        allFiles,8)); 
    nTrials=length(condFiles);                       % number of trials in condition
    
    if nTrials==0;
        error(['No trials found for condition ' cond])
    end
    
    % get trial EEG signals for each channel
    for k=1:nChan;
        
        ch=channels(k);             % channel number
        eeg(i,k).condition=cond;    % save condition number
        eeg(i,k).channel=ch;        % save channel number
        eeg(i,k).nTrials=nTrials;   % save number of trials
        
        for j=1:nTrials;
            
            load([dataDir,'/',condFiles{j}]); % load trial data
            
            % get metadata from first trial
            if j==1;
                eeg(i,k).DataUnitStr=DataUnitStr;         % signal units
                eeg(i,k).FreqHz=FreqHz;                   % data acquisition rate
                eeg(i,k).nEpochs=NmbEpochs;               % # of epochs
                eeg(i,k).nPreludeEpochs=NmbPreludeEpochs; % # of pre/postlude epochs
                
                % initialize array for holding signal for each trial
                eeg(i,k).eegSignal=zeros(nTrials,size(RawTrial,1));
                
                % initialze array for holding epoch quality info
                eeg(i,k).IsEpochOK=zeros(nTrials,size(IsEpochOK,1));
            end
            
            % get channel signal for trial
            eeg(i,k).eegSignal(j,:)=(double(RawTrial(:,ch)) + Shift(ch))*Ampl(ch);
            
            % get epoch quality info
            eeg(i,k).IsEpochOK(j,:)=IsEpochOK(:,ch);
            
            clearvars -except dataDir conditions channels eeg nConds nChan ...
                allFiles cond condFiles nTrials ch i j k 
        end
        
        disp(['Finished importing eeg signal for channel ' num2str(ch)...
            ', condition ' cond])
    end
end

