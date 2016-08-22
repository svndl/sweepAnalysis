# This function is used to convert a Matlab data structure into an R data structure.
# See Peter Kohler for information regarding the structure of the MATLAB data.
#
# The output data has a tree-like structure as follows:
# - Group Number (Only 2 in total)
#   - Condition Number
#     - Data Type (only going to use rcReal and rcImag for the time being
#       since there are less components -- smaller runtime. See line 38.)
#       - Subject Number
#         - Harmonics
#           - Trial Number
#             - Component Number
#               - Bin Number
#
# Sample usage of the function:
#   rFormatData <- readMatStruct("...\PATH\TO\MATLAB\STRUCTURE", 0)

readMatStruct <- function(matData, debugMode)
{
	# debugMode is 1 or 0 depending on whether or not user wants print statements
	# indicating progress of data processing. Default is 0.
	
	if (missing(debugMode))	{
		debugMode = 0;
	}
	
	require("R.matlab")
	cat("R.matlab is reading data...\n");
	data <- readMat(matData);
	cat("Processing data...\n");
	
	# Add entries for each group
	numGroups = length(data);
	allData = data.frame(matrix(ncol=numGroups));
	allDataSubjectInfo = data.frame(matrix(ncol=numGroups));
	
	# Not using sensorReal/sensorImag due to runtime (too much data from 128 components)
	dataTypes = c("rcReal", "rcImag");
	
	for (i in 1:numGroups)
	{
		if (debugMode) {
			cat("Processing Group:", i, '\n');
		}
		
		# Add entries for each condition in each group
		numConditions = length(data[[i]]);
		allData[[i]] = data.frame(matrix(ncol=numConditions));
		allDataSubjectInfo[[i]] = data.frame(matrix(ncol=numConditions));
		
		# Fill up data for each condition in each group
		out = processGroup(data, i, numConditions, length(dataTypes), debugMode);
		allData[[i]] = out$conditionData
		allDataSubjectInfo[[i]] = out$conditionDataInfo
	}
	
	cat("Done processing data.\n");
	data <- list("allData" = allData, "subjectData" = allDataSubjectInfo);
	return(data);
}

processGroup <- function(data, groupNumber, numConditions, numDataTypes, debugMode)
{
	# groupNumber: needed so that we know which data to use in this function
	# data: output from R.matlab function readMat()
	# numConditions: number of conditions in data
	# numDataTypes: number of data types in data
	# debugMode: whether or not user wants print statements indicating progress
	
	conditionData <- data.frame(matrix(ncol=numConditions));
	conditionDataInfo <- data.frame(matrix(ncol=numConditions));
	for (i in 1:numConditions)
	{
		if (debugMode) {
			cat("Processing Group:", groupNumber, ", Condition", i, '\n');
		}
		dataGroupCond <- as.vector(data[[groupNumber]][[i]]);
	
		conditionName = paste('Condition', i, sep="_");
		colnames(conditionData)[i] = conditionName;
		colnames(conditionDataInfo)[i] = conditionName;
		
		conditionData[[i]] = data.frame(matrix(ncol=numDataTypes));
		conditionDataInfo[[i]] = data.frame(matrix(ncol=numDataTypes));
		for (j in 1:numDataTypes)
		{
			dataGroupType = dataGroupCond[[j]]
			numSubjects = length(dataGroupType);
			conditionData[[i]][[j]] = data.frame(matrix(ncol=numSubjects));
			conditionDataInfo[[i]][[j]] = data.frame(matrix(ncol=numDataTypes));
			for (k in 1:numSubjects)
			{
				colnames(conditionData[[i]][[j]])[k] = "subject";
				dataGroupTypeSubj = dataGroupType[[k]][[1]];
				subjectDataDimensions = dim(dataGroupTypeSubj);
				subjectData <- fillUpDataTree(dataGroupTypeSubj, subjectDataDimensions);
				
				# Each subject will have the data and information about the data including:
				# number of harmonics, trials, components, bins
				conditionData[[i]][[j]][[k]] = subjectData$data;
				conditionDataInfo[[i]][[j]][[k]] = subjectData$dataInfo;
			}
		}
	}
	data <- list("conditionData" = conditionData, "conditionDataInfo" = conditionDataInfo)
	return(data)
}

fillUpDataTree <- function(subjectData, subjectDataDimensions)
{
	# Naming the columns helps with building the data matrix.
	
	numBins = subjectDataDimensions[1];
	numComponents = subjectDataDimensions[2];
	numTrials = subjectDataDimensions[3];				
	numHarmonics = subjectDataDimensions[4];
	
	stopifnot(length(subjectData) == numHarmonics * numTrials * numComponents * numBins);
	subjectDataTree <- data.frame(matrix(ncol = numHarmonics));
	for (k in 1:numHarmonics)
	{
		colnames(subjectDataTree)[k] = "harmonic";
		subjectDataTree[[k]] <- data.frame(matrix(ncol = numTrials));
		for (m in 1:numTrials)
		{
			colnames(subjectDataTree[[k]])[m] = "trial";
			subjectDataTree[[k]][[m]] <- data.frame(matrix(ncol = numComponents));
			for (n in 1:numComponents)
			{
				colnames(subjectDataTree[[k]][[m]])[n] = "component";
				subjectDataTree[[k]][[m]][[n]] <- data.frame(matrix(ncol = numBins));
				for (z in 1:numBins)
				{
					colnames(subjectDataTree[[k]][[m]][[n]])[z] = "bin";
					x = (z-1) + (n-1) * numBins + (m-1) * numComponents * numBins + (k-1) * numTrials * numComponents * numBins + 1;
					subjectDataTree[[k]][[m]][[n]][[z]] = subjectData[[x]];
				}
			}
		}
	}
	
	infoMatrix = data.frame(matrix(ncol=4));
	colnames(infoMatrix) = c("numBins", "numComponents", "numTrials", "numHarmonics");
	
	infoMatrix$numBins = numBins;
	infoMatrix$numComponents = numComponents;
	infoMatrix$numTrials = numTrials;
	infoMatrix$numHarmonics = numHarmonics;
	
	subjectData <- list("data" = subjectDataTree, "dataInfo" = infoMatrix);
	return(subjectData);
}
