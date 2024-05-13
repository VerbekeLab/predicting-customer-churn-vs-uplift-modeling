## Original code by Floris Devriendt, cleaned and modified by Jordi Vandenhouwe & Maarten de Winter
## Loading packages ##
library(splitstackshape)
source("0 - runSettings.R") # use this file to setup run

## Loading CSV and Transform to RDS (first run only)
# df <- read.csv(paste("RDS/",datasetName,".csv",sep=""), header=TRUE,sep=";",dec=".") #load as csv
# saveRDS(df, paste("RDS/",datasetName,".RDS",sep=""))   #save as RDS

## Loading RDS (subsequent runs)
df <- readRDS(paste("RDS/",datasetName,".RDS",sep=""))

# Check if saveLocation folder exists and if not, create it
if (!file.exists(saveLocation)){
  dir.create(file.path(saveLocation))
}

targetVariable <- "visit"

# Taking a 50% control 50% treatment sample of 300000 observations
split <- stratified(df,treatmentVariable, size = 150000, bothSets = TRUE, keep.rownames = TRUE)
trainSampleIndices <- as.integer(as.data.frame(split[[1]])$rn)
train_sample <- df[trainSampleIndices,]
rm(trainSampleIndices)

# Taking a 50% control 50% treatment test sample of 200k observations
testSampleIndices <- as.integer(as.data.frame(split[[2]])$rn)
test_sample <- df[testSampleIndices,]
test_sample_split <- stratified(test_sample,treatmentVariable, size = 100000, bothSets = FALSE, keep.rownames = FALSE)
test_sample <- as.data.frame(test_sample_split)
rm(test_sample_split)
rm(testSampleIndices)
rm(split)
rm(df)

train_sample$churn <- 1 - train_sample$visit # visit = positive so transform into churn variable
test_sample$churn <- 1 - test_sample$visit # visit = positive so transform into churn variable
targetVariable <- "churn" # redefining target variable

# Removing unwanted columns
unwantedColumns <- c("visit","exposure","conversion")
train_sample <- train_sample[,-which(names(train_sample) %in% unwantedColumns)]
test_sample <- test_sample[,-which(names(test_sample) %in% unwantedColumns)]

# Testing probabilities to see whether training and testing set are similar.
prop.table(table(train_sample[,targetVariable],train_sample[,treatmentVariable]))
prop.table(table(test_sample[,targetVariable],test_sample[,treatmentVariable]))

################################
# Creating subsets             #
################################

## Split & Save Train Sample
splitted.train         <- split(train_sample, f = train_sample[,treatmentVariable])
train_sample_control   <- splitted.train[[1]]
train_sample_treatment <- splitted.train[[2]]
saveRDS(train_sample, paste(saveLocation,"/train_sample.RDS", sep = ""))
saveRDS(train_sample_control, paste(saveLocation,"/train_sample_control.RDS", sep = ""))
saveRDS(train_sample_treatment, paste(saveLocation,"/train_sample_treatment.RDS", sep = ""))

# Testing probabilities to see whether treatment and control set are similar to the above ratio's.
prop.table(table(train_sample_control[,targetVariable]))
prop.table(table(train_sample_treatment[,targetVariable]))

## Split & Save Test Sample
splitted.test       <- split(test_sample, f = test_sample[,treatmentVariable])
test_sample_control   <- splitted.test[[1]]
test_sample_treatment <- splitted.test[[2]]
saveRDS(test_sample, paste(saveLocation,"/test_sample.RDS", sep = ""))
saveRDS(test_sample_control, paste(saveLocation,"/test_sample_control.RDS", sep = ""))
saveRDS(test_sample_treatment, paste(saveLocation,"/test_sample_treatment.RDS", sep = ""))

# Testing probabilities to see whether treatment and control set are similar to the above ratio's.
churn_rate_control   <- prop.table(table(test_sample_control[,targetVariable]))[2]
churn_rate_treatment <- prop.table(table(test_sample_treatment[,targetVariable]))[2]

# Calculating retention rate (used for MP-measure)
retention_rate       <- (churn_rate_control - churn_rate_treatment) / churn_rate_control
