# This function builds the data matrix of typical and atypical subjects for
# a specific condition and data type. Using all the conditions at once is 
# too time consuming. If you want to use all the data types, then you can
# call this function again with the new data type and then concatenate the
# two data matrices (column-wise concatenation, since you are concatenating
# new features and not new subjects).
#
# data: output from readMatStruct(). See readMatStruct.R for more details.
# condition: a number indicating which condition to obtain data from.
#
# A "feature" or "variable" is just the levels needed to be traversed in order
# to get the data point at the leaf of the tree. See dfsGetFeatures().

buildDataMatrix <- function(data, condition, dataType)
{
	# Initialize an empty data matrix
	selectedData = c();
	
	# Get the total number of features for the dataset.
	cat("Getting features...\n");
	featureInfo = getMaxNumberOfFeatures(data, condition, dataType);
	numFeatures = featureInfo$numFeatures;
	listOfFeatures = featureInfo$listOfFeatures;
	
	# Iterate through each group
	cat("Getting data...\n");
	numOmit = 0;
	for (i in 1:length(data$allData))
	{
		# We are only working with one condition and one data type
		numSubjects = length(data$allData[[i]][[condition]][[dataType]]);
		
		# Iterate through each subject, given a condition and a data type
		for (j in 1:numSubjects)
		{
			subjectName = paste("Group", i, "Subject", j, sep="_");
			subjectData = data$allData[[i]][[condition]][[dataType]][[j]];
			
			listOfValues = c();
			listOfValues = dfsSubjectDataTree(subjectData, listOfValues);
			
			# If a subject has less features, then omit subject.
			if (length(listOfValues) != numFeatures)
			{
				stopifnot(length(listOfValues) < numFeatures);
				numMissing = numFeatures - length(listOfValues);
				cat("**Warning: Group", i, "Subject", j, "has", numMissing, "missing features. Omitting subject.\n");
				numOmit = numOmit + 1;
			}
			else {
				selectedData = rbind(selectedData, listOfValues);
				rownames(selectedData)[(i-1)*numSubjects+j - numOmit] = subjectName;
			}
			cat("Subject", j, length(listOfValues), '\n');
		}
	}
	selectedData = data.frame(selectedData);
	stopifnot(length(selectedData) == length(listOfFeatures));
	colnames(selectedData) = listOfFeatures;
	return(selectedData);
}

getMaxNumberOfFeatures <- function(data, condition, dataType)
{
	# In order to get the maximum number of features, we must iterate through
	# each group and through each subject in each group in order to get the 
	# maximum number of features over every subject. This function is only
	# needed if we do not know if all the subjects (in both groups) have the
	# same number of features.
	
	listOfFeatures = c();
	maxFeatures = 0;
	
	# Iterate through each group
	for (i in 1:length(data$allData))
	{
		subjectDataInfo = data$subjectData[[i]][[condition]][[dataType]];
		numSubjects = length(data$allData[[i]][[condition]][[dataType]]);
		for (j in 1:numSubjects)
		{
			numBins = subjectDataInfo[[j]]$numBins;
			numComponents = subjectDataInfo[[j]]$numComponents;
			numTrials = subjectDataInfo[[j]]$numTrials;
			numHarmonics = subjectDataInfo[[j]]$numHarmonics;
			numFeatures = numBins * numComponents * numTrials * numHarmonics;
			
			# If the current subject in the current group has the largest number of
			# features so far, get the list of feature names.
			if (numFeatures > maxFeatures) {
				subjectData = data$allData[[i]][[condition]][[dataType]][[j]];
				cat("Getting feature names...\n");
				listOfFeatures = c();
				path = "";
				listOfFeatures = dfsGetFeatures(subjectData, listOfFeatures, path)
				maxFeatures = numFeatures;
			}
		}
	}
	featureInfo <- list("numFeatures" = maxFeatures, "listOfFeatures" = listOfFeatures)
	return(featureInfo);
}

# This function does a depth first traversal of the data tree at the
# "subject" level in the tree hierarchy. "listOfValues" is the list of
# data points for the subject. If a subject has 10 harmonics x 10 trials
# x 5 components x 10 bins, then the list should have 5000 points at the
# end of the recursion.
dfsSubjectDataTree <- function(data, listOfValues)
{
	# Base case: at the leaf of the tree
	if (class(data) == "numeric")
	{
		# Change all NaNs to NAs, so we have uniformity.
		if (!is.finite(data)) {
			data = NA;
		}
		listOfValues = c(listOfValues, data)
		return(listOfValues);
	}
	for (i in 1:length(data))
	{
		listOfValues = dfsSubjectDataTree(data[[i]], listOfValues);
	}
	return(listOfValues);
}

# "path" is a vector of indices for each level in the tree hierarchy in order
# to reach the leaf (i.e. the datapoint). As there are 5000 points for each
# subject (in the above example), there would be 5000 features as well.
dfsGetFeatures <- function(data, listOfFeatures, path)
{
	if (class(data) == "numeric")
	{
		listOfFeatures = c(listOfFeatures, path)
		return(listOfFeatures);
	}
	for (i in 1:length(data))
	{
		if (path == "") {
			newPath = i;
		}
		else {
			newPath = paste(path, i, sep="_");
		}
		listOfFeatures = dfsGetFeatures(data[[i]], listOfFeatures, newPath);
	}
	return(listOfFeatures);
}

