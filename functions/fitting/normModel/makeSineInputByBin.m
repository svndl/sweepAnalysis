function [c]=makeSineInputByBin(binLevels,Fi,Fs,binSz)
% [c]=makeSineInputByBin(binLevels,Fi,Fs,binSz)
%
% Creates set of sinusoidal inputs -- each row in c corresponds to input
% for one bin.
% Amplitude of each bin is equal to the corresponding (contrast) level in 
% binLevels; sine wave is shifted so that the minimum value is zero.
% 
% INPUT:
%       binLevels = experimental contrast levels, as a vector
%       Fi = frequency of input in Hz
%       Fs = sampling frequency in Hz
%       binSz = number of samples/bin
% OUTPUT:
%       c = matrix of sinusoidal input; each row corresponds to input at one
%           bin level; size is length(binLevels) x binSz

% initialize array for holding input at each bin level
c=zeros(length(binLevels),binSz);

% time vector for each input
t=linspace(0,(binSz/Fs),binSz+1); 
t=t(2:end);

% input at each bin level
for i=1:length(binLevels);
    c(i,:)=(sin(2*pi*Fi*t).*(binLevels(i)/2))+(binLevels(i)/2);

end

end