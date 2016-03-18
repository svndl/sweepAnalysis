function [c,t]=makeSineInput(binLevels,Fi,Fs,binSz)
% [c,t]=makeSineInput(binLevels,Fi,Fs,binSz)
%
% Creates sinusoidal input for entire sweep as a single vector.
%
% INPUT:
%       binLevels = experimental contrast levels, as a vector
%       Fi = frequency of input in Hz
%       Fs = sampling frequency in Hz
%       binSz = number of samples/bin
% OUTPUT:
%       c = sinusoidal input, with amplitude corresponding to binLevels
%       t = corresponding time vector 

binStart=linspace(1,binSz*length(binLevels)-(binSz-1),length(binLevels));   % start point of each bin
binStop=binStart+binSz-1;                                                   % stop point of each bin

t=linspace(0,length(binLevels)*(binSz/Fs),binSz*length(binLevels)+1);       % time vector
t=t(2:end);

con=zeros(1,length(t)); % initialize array for contrast presented at each time

% create vector of contrasts
for i=1:length(binLevels);
    con(binStart(i):binStop(i))=binLevels(i);
end

% create sine wave with amplitude corresponding to the contrast at each timepoint
c=(sin(2*pi*Fi*t).*(con/2))+(con/2); 

end
