function [f]=fitNormModelTimeDomain(theta,c,R,binSz)
% [f]=fitNormModelTimeDomain(theta,c,R,binSz)
%
% Loss function for optimizing normalization model in time domain
%
% Computes loss function = the sum of the squared differences between the
% simulated and experimental EEG signal
%
% INPUTS
%       theta = parameters of the normalization model
%           theta(1) = Rm
%           theta(2) = p
%           theta(3) = q
%           theta(4) = sigma
%       c = sinusoidal input corresponding to experimental bin levels;
%           created using makeSineInput.m
%       R = experimental response; must be same length as c
%       binSz = the number of samples (i.e., time points) in each 
%           experimental bin

% estimated response Rhat given current parameters
Rhat=theta(1)*c.^theta(2)./(c.^theta(3) + theta(4).^theta(3));

% for each bin, subtract mean response to center at zero
binStart=linspace(1,length(c)-(binSz-1),length(c)/binSz);
binStop=binStart+binSz-1;
for i=1:(length(c)/binSz);
    Rhat(binStart(i):binStop(i))=Rhat(binStart(i):binStop(i))-mean(Rhat(binStart(i):binStop(i)));
end

% for each bin, subtract mean response of experimental response
for i=1:(length(c)/binSz);
    R(binStart(i):binStop(i))=R(binStart(i):binStop(i))-mean(R(binStart(i):binStop(i)));
end

% function to minimize: sum of squared errors
f=sum((R-Rhat).^2);

end

