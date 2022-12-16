# create shiny app Rdata file
load("ProcessedData/processed_AnalysisData.Rdata")
save(processed_data, file = "ExploratoryAnalysis/FishTrack-EDA-Tool/processed_data.Rdata")
