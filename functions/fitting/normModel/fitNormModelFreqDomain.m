function [f]=fitNormModelFreqDomain(theta,binLevels,Fi,Fs,binSz,AmplR,harm)
% [f]=fitNormModelFreqDomain(theta,binLevels,Fi,Fs,binSz,AmplR,harm)
%
% Loss function for fitting normalization model to sweep data in the 
% frequency domain.
%
% Computes loss function = the mean squared error of all contrast and
% frequency combinations (# bin levels x # harmonics)
%
% INPUTS
%       theta = parameters of the normalization model
%           theta(1) = Rm
%           theta(2) = p
%           theta(3) = q
%           theta(4) = sigma
%       binLevels = experimental contrast levels, as a vector
%       Fi = frequency of input in Hz
%       Fs = sampling frequency in Hz
%       binSz = number of samples/bin
%       AmplR = matrix giving actual responses at the frequencies in freqs
%           size is # of bins x # of frequencies
%           each column = response function at a given frequency
%           rows correspond to the bin levels in c
%       harm = vector of harmonics at which the contrast response
%           function will be fit 
%      

% create input for contrast sweep, c
[c]=makeSineInputByBin(binLevels,Fi,Fs,binSz);

% using input, simulate contrast sweep using normalization model
[~,As,f]=simulateSweep(theta,c,Fs);

nBin=length(binLevels);     % number of bins
nHarm=length(harm);         % number of harmonics at which to compare responses
AmplRhat=zeros(nBin,nHarm); % matrix for holding estimated amplitudes of contrast response functions

% find amplitude at harmonics of interest
freqs=harm*Fi;              % frequences corresponding to harmonics
for i=1:nBin;
    for k=1:length(freqs);
        AmplRhat(i,k)=As(i,round(f,4)==round(freqs(k),4));
    end
end

% function to minimize: mean squared error
f=sum(sum((AmplR-AmplRhat).^2))/numel(AmplR);

end