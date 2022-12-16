export_wbfr <- function(getFishID){
  library(RDCOMClient)
  EvApp <-  RDCOMClient::COMCreate('EchoviewCom.EvApplication')
  library(EchoviewR)
  library(here)
  evfile <- paste0(getFishID, ".EV")
  EVfolder <- here(file.path("Data", getFishID))
  EVFile2Open <- paste(EVfolder, evfile, sep = "/")
  EvFile<-EvApp$OpenFile(EVFile2Open)
  EvExport <- EvFile[["Properties"]][["Export"]]
  EvExport[["Mode"]] <- 1
  EVVar <- EvFile[["Variables"]]$FindByName("Single target detection - wideband 1")
  
  
  # Export Frequency Response - 70kHz
  EVVar$ExportSingleTargetWidebandFrequencyResponseByRegions(
    file.path(paste(getwd(), "Data",getFishID,"FreqResponse70.csv", sep="/")),
    FALSE, # show average
    FALSE, # show min/max
    0.4, # window size
    0, # window unit, 0=meters
    1, # max time window
    TRUE, # apply beam compensation
    EvFile[["Variables"]]$FindByName("Fileset 1: TS pulse compressed wideband pings T1"),
    EvFile[["RegionClasses"]]$FindByName("070kHz_FishTracks"))
  
  # Export Frequency Response - 120kHz
  EVVar$ExportSingleTargetWidebandFrequencyResponseByRegions(
    file.path(paste(getwd(), "Data",getFishID,"FreqResponse120.csv", sep="/")),
    FALSE, # show average
    FALSE, # show min/max
    0.4, # window size
    0, # window unit, 0=meters
    1, # max time window
    TRUE, # apply beam compensation
    EvFile[["Variables"]]$FindByName("Fileset 1: TS pulse compressed wideband pings T3"),
    EvFile[["RegionClasses"]]$FindByName("070kHz_FishTracks"))
  
  # Export Frequency Response - 200kHz
  EVVar$ExportSingleTargetWidebandFrequencyResponseByRegions(
    file.path(paste(getwd(), "Data",getFishID,"FreqResponse200.csv", sep="/")),
    FALSE, # show average
    FALSE, # show min/max
    0.4, # window size
    0, # window unit, 0=meters
    1, # max time window
    TRUE, # apply beam compensation
    EvFile[["Variables"]]$FindByName("Fileset 1: TS pulse compressed wideband pings T2"),
    EvFile[["RegionClasses"]]$FindByName("070kHz_FishTracks"))
  
  # Export Frequency Response - 70kHz
  EVVar$ExportSingleTargetWidebandFrequencyResponseByRegions(
    file.path(paste(getwd(), "Data",getFishID,"FreqResponse70_uncompTS.csv", sep="/")),
    FALSE, # show average
    FALSE, # show min/max
    0.4, # window size
    0, # window unit, 0=meters
    1, # max time window
    FALSE, # apply beam compensation
    EvFile[["Variables"]]$FindByName("Fileset 1: TS pulse compressed wideband pings T1"),
    EvFile[["RegionClasses"]]$FindByName("070kHz_FishTracks"))
  
  # Export Frequency Response - 120kHz
  EVVar$ExportSingleTargetWidebandFrequencyResponseByRegions(
    file.path(paste(getwd(), "Data",getFishID,"FreqResponse120_uncompTS.csv", sep="/")),
    FALSE, # show average
    FALSE, # show min/max
    0.4, # window size
    0, # window unit, 0=meters
    1, # max time window
    FALSE, # apply beam compensation
    EvFile[["Variables"]]$FindByName("Fileset 1: TS pulse compressed wideband pings T3"),
    EvFile[["RegionClasses"]]$FindByName("070kHz_FishTracks"))
  
  # Export Frequency Response - 200kHz
  EVVar$ExportSingleTargetWidebandFrequencyResponseByRegions(
    file.path(paste(getwd(), "Data",getFishID,"FreqResponse200_uncompTS.csv", sep="/")),
    FALSE, # show average
    FALSE, # show min/max
    0.4, # window size
    0, # window unit, 0=meters
    1, # max time window
    FALSE, # apply beam compensation
    EvFile[["Variables"]]$FindByName("Fileset 1: TS pulse compressed wideband pings T2"),
    EvFile[["RegionClasses"]]$FindByName("070kHz_FishTracks"))
  
  EvFile$Save()
  EvFile$Close()
  EvApp$Quit()
  
  usethis::ui_done(getFishID)
}

# export_wbfr("LT016")
allfish <- dir("Data/")

purrr::map(allfish, export_wbfr)
