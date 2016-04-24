source("../functions/getSweepDataFlex.R");

# require("mclust");
require("MASS");

##################################################################
# Acquire Data
##################################################################

CVI_NT_Data <- getSweepDataFlex("/Users/Nathan/Desktop/Research/CVIdata/CVI_NT_20150407_1127/Exp_TEXT_PD1010_5_Cz/RLS_c001.txt", chanToKeep = c(), binToKeep = c(), colsToKeep = c(3, 4, 9, 11, 13, 14, 19, 20));
CVI_Data <- getSweepDataFlex("/Users/Nathan/Desktop/Research/CVIdata/CVI3top34_20160224_1020/Exp_TEXT_PD1010_5_Cz/RLS_c001.txt", chanToKeep = c(), binToKeep = c(), colsToKeep = c(3, 4, 9, 11, 13, 14, 19, 20));

# Only get specific channel and 1F1 and 2F1
channel = "Oz-Cz";

# Get 1F1 data
CVI_NT_1F1Data = CVI_NT_Data$selectedData[CVI_NT_Data$selectedData$Harm == "1F1",];
CVI_NT_1F1Data = CVI_NT_1F1Data[CVI_NT_1F1Data$iCh == channel,];
CVI_1F1Data = CVI_Data$selectedData[CVI_Data$selectedData$Harm == "1F1",];
CVI_1F1Data = CVI_1F1Data[CVI_1F1Data$iCh == channel,];

# Get 2F1 data
CVI_NT_2F1Data = CVI_NT_Data$selectedData[CVI_NT_Data$selectedData$Harm == "2F1",];
CVI_NT_2F1Data = CVI_NT_2F1Data[CVI_NT_2F1Data$iCh == channel,];
CVI_2F1Data = CVI_Data$selectedData[CVI_Data$selectedData$Harm == "2F1",];
CVI_2F1Data = CVI_2F1Data[CVI_2F1Data$iCh == channel,];

# Get max signal values for NT data
maximumBin = 0;
# Look at specific bin
specificBin = 1;
binToLook = 9;
# Look at the average bin (bin 0)
averageBin = 0;

# Signal/Phase
CVI_NT_1F1_signal = c();
CVI_NT_2F1_signal = c();
CVI_NT_1F1_phase = c();
CVI_NT_2F1_phase = c();

# Sin/Cos
CVI_NT_1F1_Sr = c();
CVI_NT_2F1_Sr = c();
CVI_NT_1F1_Si = c();
CVI_NT_2F1_Si = c();
for (i in 1:max(CVI_NT_1F1Data$iTrial))
{
	# Get index of bins
	indexRange = (11*i + 2):(11*i + 11);
	baseIndex = 11*i+1;
	
	# Max value
	if (maximumBin)
	{
		CVI_NT_1F1_signal = c(CVI_NT_1F1_signal, max(CVI_NT_1F1Data$Signal[indexRange]));
		idx = which(CVI_NT_1F1Data$Signal[indexRange] == max(CVI_NT_1F1Data$Signal[indexRange]))
		CVI_NT_1F1_phase = c(CVI_NT_1F1_phase, CVI_NT_1F1Data$Phase[baseIndex+idx]);
		CVI_NT_1F1_Sr = c(CVI_NT_1F1_Sr, CVI_NT_1F1Data$Sr[baseIndex+idx]);
		CVI_NT_1F1_Si = c(CVI_NT_1F1_Si, CVI_NT_1F1Data$Si[baseIndex+idx]);
		
		CVI_NT_2F1_signal = c(CVI_NT_2F1_signal, max(CVI_NT_2F1Data$Signal[indexRange]));
		idx = which(CVI_NT_2F1Data$Signal[indexRange] == max(CVI_NT_2F1Data$Signal[indexRange]))
		CVI_NT_2F1_phase = c(CVI_NT_2F1_phase, CVI_NT_2F1Data$Phase[baseIndex+idx]);
		CVI_NT_2F1_Sr = c(CVI_NT_2F1_Sr, CVI_NT_2F1Data$Sr[baseIndex+idx]);
		CVI_NT_2F1_Si = c(CVI_NT_2F1_Si, CVI_NT_2F1Data$Si[baseIndex+idx]);
	}
	
	# Specific bin
	else if (specificBin)
	{
		CVI_NT_1F1_signal = c(CVI_NT_1F1_signal, max(CVI_NT_1F1Data$Signal[baseIndex+binToLook]));
		CVI_NT_1F1_phase = c(CVI_NT_1F1_phase, CVI_NT_1F1Data$Phase[baseIndex+binToLook]);
		CVI_NT_1F1_Sr = c(CVI_NT_1F1_Sr, CVI_NT_1F1Data$Sr[baseIndex+binToLook]);
		CVI_NT_1F1_Si = c(CVI_NT_1F1_Si, CVI_NT_1F1Data$Si[baseIndex+binToLook]);
		
		CVI_NT_2F1_signal = c(CVI_NT_2F1_signal, max(CVI_NT_2F1Data$Signal[baseIndex+binToLook]));
		CVI_NT_2F1_phase = c(CVI_NT_2F1_phase, CVI_NT_2F1Data$Phase[baseIndex+binToLook]);
		CVI_NT_2F1_Sr = c(CVI_NT_2F1_Sr, CVI_NT_2F1Data$Sr[baseIndex+binToLook]);
		CVI_NT_2F1_Si = c(CVI_NT_2F1_Si, CVI_NT_2F1Data$Si[baseIndex+binToLook]);
	}
	
	# Average bin (i.e. Bin 0) Bin 0 isn't exactly scalar average. There is some other way
	# in which this average is calculated (could be using weights)
	else if (averageBin)
	{
		CVI_NT_1F1_signal = c(CVI_NT_1F1_signal, max(CVI_NT_1F1Data$Signal[baseIndex]));
		CVI_NT_1F1_phase = c(CVI_NT_1F1_phase, CVI_NT_1F1Data$Phase[baseIndex]);
		CVI_NT_1F1_Sr = c(CVI_NT_1F1_Sr, CVI_NT_1F1Data$Sr[baseIndex]);
		CVI_NT_1F1_Si = c(CVI_NT_1F1_Si, CVI_NT_1F1Data$Si[baseIndex]);
		
		CVI_NT_2F1_signal = c(CVI_NT_2F1_signal, max(CVI_NT_2F1Data$Signal[baseIndex]));
		CVI_NT_2F1_phase = c(CVI_NT_2F1_phase, CVI_NT_2F1Data$Phase[baseIndex]);	
		CVI_NT_2F1_Sr = c(CVI_NT_2F1_Sr, CVI_NT_2F1Data$Sr[baseIndex]);
		CVI_NT_2F1_Si = c(CVI_NT_2F1_Si, CVI_NT_2F1Data$Si[baseIndex]);
	}
}
CVI_NT_Sig = cbind(CVI_NT_1F1_signal, CVI_NT_2F1_signal);
CVI_NT_Phase = cbind(CVI_NT_1F1_phase, CVI_NT_2F1_phase);
CVI_NT_Sr = cbind(CVI_NT_1F1_Sr, CVI_NT_2F1_Sr);
CVI_NT_Si = cbind(CVI_NT_1F1_Si, CVI_NT_2F1_Si);

# Get max signal values for CVI data
CVI_1F1_signal = c();
CVI_2F1_signal = c();
CVI_1F1_phase = c();
CVI_2F1_phase = c();

# Sin/Cos
CVI_1F1_Sr = c();
CVI_2F1_Sr = c();
CVI_1F1_Si = c();
CVI_2F1_Si = c();
for (i in 1:max(CVI_1F1Data$iTrial))
{
	indexRange = (11*i + 2):(11*i + 11);
	baseIndex = 11*i+1;
	
	# Max value
	if (maximumBin)
	{
		CVI_1F1_signal = c(CVI_1F1_signal, max(CVI_1F1Data$Signal[indexRange]));
		idx = which(CVI_1F1Data$Signal[indexRange] == max(CVI_1F1Data$Signal[indexRange]))
		CVI_1F1_phase = c(CVI_1F1_phase, CVI_1F1Data$Phase[baseIndex+idx]);
		CVI_1F1_Sr = c(CVI_1F1_Sr, CVI_1F1Data$Sr[baseIndex+idx]);
		CVI_1F1_Si = c(CVI_1F1_Si, CVI_1F1Data$Si[baseIndex+idx]);
		
		CVI_2F1_signal = c(CVI_2F1_signal, max(CVI_2F1Data$Signal[indexRange]));
		idx = which(CVI_2F1Data$Signal[indexRange] == max(CVI_2F1Data$Signal[indexRange]))
		CVI_2F1_phase = c(CVI_2F1_phase, CVI_2F1Data$Phase[baseIndex+idx]);	
		CVI_2F1_Sr = c(CVI_2F1_Sr, CVI_2F1Data$Sr[baseIndex+idx]);
		CVI_2F1_Si = c(CVI_2F1_Si, CVI_2F1Data$Si[baseIndex+idx]);	
	}
	
	# Specific bin
	else if (specificBin)
	{
		CVI_1F1_signal = c(CVI_1F1_signal, CVI_1F1Data$Signal[baseIndex+binToLook]);
		CVI_1F1_phase = c(CVI_1F1_phase, CVI_1F1Data$Phase[baseIndex+binToLook]);
		CVI_1F1_Sr = c(CVI_1F1_Sr, CVI_1F1Data$Sr[baseIndex+binToLook]);
		CVI_1F1_Si = c(CVI_1F1_Si, CVI_1F1Data$Si[baseIndex+binToLook]);
		
		CVI_2F1_signal = c(CVI_2F1_signal, CVI_2F1Data$Signal[baseIndex+binToLook]);		
		CVI_2F1_phase = c(CVI_2F1_phase, CVI_2F1Data$Phase[baseIndex+binToLook])
		CVI_2F1_Sr = c(CVI_2F1_Sr, CVI_2F1Data$Sr[baseIndex+binToLook]);
		CVI_2F1_Si = c(CVI_2F1_Si, CVI_2F1Data$Si[baseIndex+binToLook]);
	}
	
	# Average bin (i.e. Bin 0)
	else if (averageBin)
	{
		CVI_1F1_signal = c(CVI_1F1_signal,CVI_1F1Data$Signal[baseIndex]);
		CVI_1F1_phase = c(CVI_1F1_phase, CVI_1F1Data$Phase[baseIndex]);
		CVI_1F1_Sr = c(CVI_1F1_Sr, CVI_1F1Data$Sr[baseIndex]);
		CVI_1F1_Si = c(CVI_1F1_Si, CVI_1F1Data$Si[baseIndex]);
		
		CVI_2F1_signal = c(CVI_2F1_signal, CVI_2F1Data$Signal[baseIndex]);
		CVI_2F1_phase = c(CVI_2F1_phase, CVI_2F1Data$Phase[baseIndex]);
		CVI_2F1_Sr = c(CVI_2F1_Sr, CVI_2F1Data$Sr[baseIndex]);
		CVI_2F1_Si = c(CVI_2F1_Si, CVI_2F1Data$Si[baseIndex]);
	}
}
CVI_Sig = cbind(CVI_1F1_signal, CVI_2F1_signal);
CVI_Phase = cbind(CVI_1F1_phase, CVI_2F1_phase);
CVI_Sr = cbind(CVI_1F1_Sr, CVI_2F1_Sr);
CVI_Si = cbind(CVI_1F1_Si, CVI_2F1_Si);

##################################################################
# Debuggers
##################################################################

# print("CVI_NT Means");
# print(mean(CVI_NT_1F1_signal));
# print(mean(CVI_NT_2F1_signal));
# print("CVI Means")
# print(mean(CVI_1F1_signal));
# print(mean(CVI_2F1_signal));

##################################################################
# Visualize
##################################################################

# CVIxlim=range(CVI_Sig[,2]);
# CVIylim=range(CVI_Sig[,1]);
# CVI_NTxlim=range(CVI_NT_Sig[,2]);
# CVI_NTylim=range(CVI_NT_Sig[,1]);
# dev.new();
# plot(CVI_Sig[,2], CVI_Sig[,1], col="red", xlab = "", ylab = "", xlim=range(CVIxlim, CVI_NTxlim), ylim=range(CVIylim, CVI_NTylim))
# par(new=TRUE);
# plot(CVI_NT_Sig[,2], CVI_NT_Sig[,1], col="blue", xlab = "2F1", ylab = "1F1", xlim=range(CVIxlim, CVI_NTxlim), ylim=range(CVIylim,CVI_NTylim), main="2F1 vs 1F1 for CVI and NT");
# legend(0.5, 17.6, c("CVI", "NT"), lty=c(1,1), lwd=c(2.5,2.5), col=c("red", "blue"));
# #legend(3, 23, c("CVI", "NT"), lty=c(1,1), lwd=c(2.5,2.5), col=c("red", "blue"));

##################################################################
# Combine both data together
##################################################################

# Sig/Phase
allData = rbind(CVI_NT_Sig, CVI_Sig);
allDataPhase = rbind(CVI_NT_Phase, CVI_Phase);
allDataSigPhase = cbind(allData, allDataPhase);

# Sin/Cos
allDataSr = rbind(CVI_NT_Sr, CVI_Sr);
allDataSi = rbind(CVI_NT_Si, CVI_Si);
allDataSrSi = cbind(allDataSr, allDataSi);

##################################################################
# Get Rows of CVI and Rows of NT
##################################################################
numCVI_NT = dim(CVI_NT_Sig)[1];
numCVI = dim(CVI_Sig)[1];
total = numCVI_NT + numCVI;

##################################################################
# Get Classes
##################################################################

Classes <- rep("NT", dim(CVI_NT_Sig)[1]);	# 1 = NT
Classes <- c(Classes, rep("CVI", dim(CVI_Sig)[1]));	# 2 = CVI

##################################################################
# Linear Discriminant Analysis on Signal (No PCA)
##################################################################

# ldaDat <- allData;
# ldaDat <- cbind(ldaDat, Classes)
# colnames(ldaDat) <- c("1F1", "2F1", "Class");
# ldaAnalysis <- lda(ldaDat[,1:2], ldaDat[,3]);
# dev.new();
# plot(ldaAnalysis);

##################################################################
# Work on Data with Phase
##################################################################

pcaOut <-prcomp(allDataSigPhase);
reducedDimensionData <- pcaOut$x[,1:2];
# reducedDimensionData <- cbind(reducedDimensionData, Classes)
xlim = range(as.numeric(reducedDimensionData[,1]));
ylim = range(as.numeric(reducedDimensionData[,2]));

dev.new()
plot(reducedDimensionData[1:numCVI_NT,1], reducedDimensionData[1:numCVI_NT,2], col="red", xlab = "", ylab = "", xlim=xlim, ylim=ylim)
par(new=TRUE);
plot(reducedDimensionData[(numCVI_NT+1):total,1], reducedDimensionData[(numCVI_NT+1):total,2], col="blue", xlab = "Component 1", ylab = "Component 2", xlim=xlim, ylim=ylim, main="CVI vs NT with 2 Principle Components, Sig/Phase")
legend(150, 100, c("CVI", "NT"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue", "red"));

##################################################################
# Work on Data with Sr/Si
##################################################################

pcaOutSrSi <- prcomp(allDataSrSi);
reducedSrSiData <- pcaOutSrSi$x[,1:2];
xlim = range(as.numeric(reducedSrSiData[,1]));
ylim = range(as.numeric(reducedSrSiData[,2]));

dev.new()
plot(reducedSrSiData[1:numCVI_NT,1], reducedSrSiData[1:numCVI_NT,2], col="red", xlab="", ylab="", xlim=xlim, ylim=ylim);
par(new=TRUE);
plot(reducedSrSiData[(numCVI_NT+1):total,1], reducedSrSiData[(numCVI_NT+1):total,2], col="blue", xlab="Component 1", ylab="Component 2", xlim=xlim, ylim=ylim, main="CVI vs NT with 2 PC, Sr/Si")
legend(-15, 10, c("CVI", "NT"), lty=c(1,1), lwd=c(2.5,2.5), col=c("blue", "red"));

##################################################################
# Linear Discriminant Analysis on Two Principle Components (Sig/Phase Data)
##################################################################

ldaPostPCAData <- reducedDimensionData;
stopifnot(dim(ldaPostPCAData)[2] == 2); # Make sure (for now) that we only have 2 features after PCA
colnames(ldaPostPCAData) <- c("Component 1", "Component 2");
ldaPostPCAAnalysis <- lda(ldaPostPCAData, Classes);
dev.new()
plot(ldaPostPCAAnalysis);

##################################################################
# Linear Discriminant Analysis on Two PCs (Sr/Si Data)
##################################################################

ldaSrSiReduced <- reducedSrSiData;
stopifnot(dim(ldaSrSiReduced[2]) == 2);
colnames(ldaSrSiReduced) <- c("Component 1", "Component 2");
ldaSrSiReducedAnalysis <- lda(ldaSrSiReduced, Classes);
dev.new()
plot(ldaSrSiReducedAnalysis);

##################################################################
# Predictions (On Own Data), Both Data
##################################################################

ldaPostPCAData.predict <- predict(ldaPostPCAAnalysis,ldaPostPCAData)$class;
table(ldaPostPCAData.predict, Classes);

ldaSrSiReduced.predict <- predict(ldaSrSiReducedAnalysis, ldaSrSiReduced)$class;
table(ldaSrSiReduced.predict, Classes);

##################################################################
# Leave one out Cross Validation, Both Data
##################################################################

# Signal/Phase Data
ldaPostPCAAnalysis.2 <- lda(ldaPostPCAData, Classes, CV=TRUE);
predTab <- table(ldaPostPCAAnalysis.2$class, Classes);
# Display prediction error percentages
cat("Signal/Phase Data\n");
for (i in 1:dim(predTab)[1])
{
	currGroup = colnames(predTab)[i];
	total <- sum(predTab[i,]);
	correct <- predTab[i,i];
	percentCorrect = correct/total;
	cat(paste(currGroup, ":", percentCorrect));
	cat("\n");
}

# Sr/Si Data
ldaSrSiReducedAnalysis.2 <- lda(ldaSrSiReduced, Classes, CV=TRUE);
predTab <- table(ldaSrSiReducedAnalysis.2$class, Classes);
cat("Sr/Si Data\n")
for (i in 1:dim(predTab)[1])
{
	currGroup = colnames(predTab)[i];
	total <- sum(predTab[i,]);
	correct <- predTab[i,i];
	percentCorrect = correct/total;
	cat(paste(currGroup, ":", percentCorrect));
	cat("\n");
}

##################################################################
# Apply EM Clustering (Typically For Unsupervised Learning)
##################################################################

# mixtureModel <- Mclust(allData, modelNames=mclust.options("emModelNames"))
# summary(mixtureModel);

# # Plot the clustered data
# classifiedData = cbind(allData, mixtureModel$classification);
# colnames(classifiedData) <- c("1F1", "2F1", "Class");
# # Class1xLim = range(classifiedData[classifiedData[,3]==1,1]);
# # Class2xLim = range(classifiedData[classifiedData[,3]==2,1]);
# # Class1yLim = range(classifiedData[classifiedData[,3]==1,2]);
# # Class2yLim = range(classifiedData[classifiedData[,3]==2,2]);

# dev.new();
# class1Data <- classifiedData[classifiedData[,3]==1,];
# class2Data <- classifiedData[classifiedData[,3]==2,];
# plot(class1Data[,2], class1Data[,1], col="green", xlab="", ylab="", xlim=range(CVIxlim, CVI_NTxlim), ylim=range(CVIylim, CVI_NTylim));
# par(new=TRUE);
# plot(class2Data[,2], class2Data[,1], col="black", xlab="2F1", ylab="1F1", xlim=range(CVIxlim, CVI_NTxlim), ylim=range(CVIylim, CVI_NTylim), main="EM Classification");
# legend(0.5, 17.6, c("Class 1", "Class 2"), lty=c(1,1), lwd=c(2.5,2.5), col=c("green", "black"));

