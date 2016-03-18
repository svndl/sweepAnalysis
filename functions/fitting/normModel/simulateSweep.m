function [Rhat,As,f]=simulateSweep(theta,c,Fs)
% [Rhat,As,f]=simulateSweep(theta,c,Fs)
%
% Given input c (created by makeSineInputByBin.m), simulate constrast sweep
% using normalization model
%
% INPUTS
%       theta = parameters of the normalization model
%           theta(1) = Rm
%           theta(2) = p
%           theta(3) = q
%           theta(4) = sigma
%       c = sinusoidal input created using makeSineInputByBin.m
%       Fs = sampling frequency in Hz
%
% OUTPUTS
%       Rhat: the simulated response; each row corresponds to the response
%           for a different bin (i.e., contrast level)
%       As: matrix of amplitude spectra; each row corresponds to the 
%           spectrum for a different bin
%       f: vector of frequencies correspond to the spectra in As

nBin=size(c,1);             % number of bins in sweep
Rhat=zeros(nBin,size(c,2)); % matrix for holding estimated responses

% matrix for holding one sided amplitude spectra
if mod(length(c),2)==0;
    As=zeros(nBin,length(c)/2+1);
else
    As=zeros(nBin,(length(c)-1)/2+1);
end

f=linspace(0,Fs/2,size(As,2)); % frequencies corresponding to amplitude spectra

% for each contrast level, determine estimated response and spectra
for i=1:nBin,
    
    % estimated response
    Rhat(i,:)=theta(1)*c(i,:).^theta(2)./(c(i,:).^theta(3) + theta(4).^theta(3));
    
    % amplitude spectrum
    AsTwo=abs(fft(Rhat(i,:)));
    if mod(length(Rhat(i,:)),2)==0;
        As(i,:)=AsTwo(1:length(Rhat(i,:))/2+1);
    else
        As(i,:)=AsTwo(1:(length(Rhat(i,:))-1)/2+1);
    end
end



