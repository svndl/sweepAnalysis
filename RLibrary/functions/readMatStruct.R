# This function is used to convert a Matlab data structure to an R data structure.
# See Peter Kohler for information regarding the way the Matlab data is structured.

readMatStruct <- function(matData)
{	
	# We need numBins, numComponents, numTrials, numHarmonics as arguments because it
	# is not possible to get these numbers from matData alone (Matlab compatibility issue)
	
	require("R.matlab")
	cat("Reading data...\n");
	data <- readMat(matData);
	
	# Data for each condition
	numConditions = length(data$subFullASD);
	stopifnot(numConditions == length(data$subFullTYP));
	cat('Number of conditions: ', numConditions, '\n')
	
	dataTypes = c('rcReal', 'rcImag', 'sensorReal', 'sensorImag');
	dataTypes = c('rcReal', 'rcImag');
	
	allData = data.frame(matrix(ncol = 2));
	colnames(allData) = c("ASD", "TYP");
	
	# Add entries for each condition in each subject group
	allData[[1]] = data.frame(matrix(ncol=numConditions));
	allData[[2]] = data.frame(matrix(ncol=numConditions));
	for (cond in 1:numConditions)
	{
		conditionName = paste('Condition', cond, sep="-");
		cat(conditionName, "\n");
		
		# Index of ASD in data is 1. I don't use actual name for generality purposes.
		dataGroup1Cond <- as.vector(data[[1]][[cond]]);
		numDataTypes = length(dataGroup1Cond);
		# stopifnot(numDataTypes == length(dataTypes));
		colnames(allData[[1]])[cond] = conditionName;
		
		# Index of TYP in data is 2. I don't use actual name for generality purposes.
		dataGroup2Cond <- as.vector(data[[2]][[cond]]);
		numDataTypes = length(dataGroup2Cond);
		# stopifnot(numDataTypes == length(dataTypes));
		colnames(allData[[2]])[cond] = conditionName;
		
		# Initialize data frames for each condition
		allData[[1]][[conditionName]] = data.frame(matrix(ncol=length(dataTypes)));
		allData[[2]][[conditionName]] = data.frame(matrix(ncol=length(dataTypes)));
		for (i in 1:length(dataTypes))
		{
			dataType = dataTypes[i];
			cat(dataType, '\n');
			
			# Data - ASD
			dataGroup1Type = dataGroup1Cond[[i]];
			colnames(allData[[1]][[conditionName]])[i] = dataType;
			
			# Data - Typical
			dataGroup2Type = dataGroup2Cond[[i]];
			colnames(allData[[2]][[conditionName]])[i] = dataType;

			numSubjectsGroup1 = length(dataGroup1Type);
			numSubjectsGroup2 = length(dataGroup2Type);
			
			# Initialize data frames for each subject
			allData[[1]][[conditionName]][[i]] = data.frame(matrix(ncol=numSubjectsGroup1));
			allData[[2]][[conditionName]][[i]] = data.frame(matrix(ncol=numSubjectsGroup2));
			for (j in 1:numSubjectsGroup1)
			{
				# Indexed as an array of length 1x5000 (10x5x10x10)
				# Bins x Components x Trials x Harmonics

				cat("Group 1: Subject ", j, "\n");

				# [[1]] at the end is a weird compatibility issue for Matlab
				dataGroup1TypeSubj = dataGroup1Type[[j]][[1]];
				
				cat("Length: ", length(dataGroup1TypeSubj), "\n");
				
				subjectDataDimensions = dim(dataGroup1TypeSubj);
				subjectData <- fillUpDataTree(dataGroup1TypeSubj, subjectDataDimensions);
				allData[[1]][[conditionName]][[i]][[j]] = subjectData;
			}
			for (j in 1:numSubjectsGroup2)
			{
				# Indexed as an array of length 1x5000 (10x5x10x10)
				# Bins x Components x Trials x Harmonics

				cat("Group 2: Subject ", j, "\n");
				
				# [[1]] at the end is a weird compatibility issue for Matlab
				dataGroup2TypeSubj = dataGroup2Type[[j]][[1]];
				
				cat("Length: ", length(dataGroup2TypeSubj), "\n");
				
				subjectDataDimensions = dim(dataGroup2TypeSubj);
				subjectData <- fillUpDataTree(dataGroup2TypeSubj, subjectDataDimensions);
				allData[[2]][[conditionName]][[i]][[j]] = subjectData;
			}
		}
	}
	return(allData);
}

fillUpDataTree <- function(subjectData, subjectDataDimensions)
{
	numBins = subjectDataDimensions[1];
	numComponents = subjectDataDimensions[2];
	numTrials = subjectDataDimensions[3];				
	numHarmonics = subjectDataDimensions[4];
	
	stopifnot(length(subjectData) == numHarmonics * numTrials * numComponents * numBins);
	subjectDataTree <- data.frame(matrix(ncol = numHarmonics));
	for (k in 1:numHarmonics)
	{
		subjectDataTree[[k]] <- data.frame(matrix(ncol = numTrials));
		for (m in 1:numTrials)
		{
			subjectDataTree[[k]][[m]] <- data.frame(matrix(ncol = numComponents));
			for (n in 1:numComponents)
			{
				subjectDataTree[[k]][[m]][[n]] <- data.frame(matrix(ncol = numBins));
				for (z in 1:numBins)
				{
					x = (z-1) + (n-1) * numBins + (m-1) * numComponents * numBins + (k-1) * numTrials * numComponents * numBins + 1;
					subjectDataTree[[k]][[m]][[n]][[z]] = subjectData[[x]];
				}
			}
		}
	}
	return(subjectDataTree);
}
