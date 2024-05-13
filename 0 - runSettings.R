## Setting setup variables

## ING
datasetName <- "ING"
saveLocation <- paste("RDS/",datasetName,sep="")
treatmentVariable <- "comm_after6"
targetVariable <- "churn_6mnths"

## Criteo
# datasetName <- "criteo"
# saveLocation <- paste("RDS/",datasetName,sep="")
# treatmentVariable <- "treatment"
# targetVariable <- "visit"

## MPU2 Variables:
runMPU <- "YES" # "YES" to run MPU measure, "NO" to load last run

incentiveCost <- 10
benefit <- 200 # average margin of 59 for 24 months
sendingCost <- 1
administrativeCosts <- 50

## Use this file to change the run settings used in the other scripts
## Don't forget to save and resource after making changes