# import merge function based on data_transpose_exports.R
# Nov 9
# JH and AR
# 
# This script:
# 1. reads the raw EV exports for each fish
# 2. exports a compiled csv file (allFishDat.csv) for each fish
# 3. merges all fish data together
# 4. filters data using the 6 dB compensation file
# 5. Saves local Rdata files of compiled data
# 6. Writes "ProcessedData/processed_AnalysisData.csv"
#
# Important: this script takes a long time to run
# Important: this script overwrites analysis data files
# Important: this script will result in multiple changes to git tracked files

# Load libraries ----
library(tidyverse)
library(janitor)

## Read exported data in to R ----
source("Analysis_Scripts/custom_functions.R")
fish <- dir("Data")
allfish <- lapply(fish, read_tidy_export_EVfiles)
masterDF <- bind_rows(allfish)

## reformat column order ----
colNames <- colnames(masterDF) #extract all column names
colNamesFirst <- colNames[1:57] #extract all non frequency TS response data (ie. just biological fish info)
colNamesSecond <- str_sort(colNames[58:483],numeric=TRUE) #extract only col names for freq resp data (eg. F45, F45.5, etc.)
colNamesOrdered <- c(colNamesFirst, colNamesSecond)
masterDF <- masterDF[colNamesOrdered] #proper order for column names

# apply TS filter ----
ts_filter <- read_csv("ExploratoryAnalysis/accepted_6dB_TS_compensation_singletargets.csv")
processed_data <- inner_join(masterDF, ts_filter, by=c("fishNum", "FishTrack"))
processed_data <- processed_data %>% relocate(MaxTSdiff, .after = FishTrack)

# save data ----
save(masterDF, file = "ProcessedData/processed_AllFishCombined_unfiltered.Rdata")
write_csv(masterDF, file = "ProcessedData/processed_AllFishCombined_unfiltered.csv")
save(processed_data, file = "ProcessedData/processed_AnalysisData.Rdata")
write_csv(processed_data, file = "ProcessedData/processed_AnalysisData.csv")

