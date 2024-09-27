setwd("C:/Users/jsontag/Documents/CHIR Project")
rm(list = ls())

library(stringr)
library(tidyverse)
library(purrr)
library(dplyr)

mtype="PIPO"
source("C:/Users/jsontag/Documents/CHIR Project/Scripts/datacleaningfunctions.R")

samp=read.csv("C:/Users/jsontag/Documents/CHIR Project/CHIR Data/CHIR_SampleEventReport.csv")
cover=read.csv("C:/Users/jsontag/Documents/CHIR Project/CHIR Data/PIPO/CHIR_PIPO_Cover-Species_Composition(metric)_XPT.csv", na.strings=c("","NA"))
fuel1000=read.csv("C:/Users/jsontag/Documents/CHIR Project/CHIR Data/PIPO/CHIR_PIPO_Surface_Fuels-1000Hr_XPT.csv", na.strings=c("","NA"))
duff=read.csv("C:/Users/jsontag/Documents/CHIR Project/CHIR Data/PIPO/CHIR_PIPO_Surface_Fuels-Duff_Litter_XPT.csv", na.strings=c("","NA"))
fine=read.csv("C:/Users/jsontag/Documents/CHIR Project/CHIR Data/PIPO/CHIR_PIPO_Surface Fuels-Fine_XPT.csv", na.strings=c("","NA"))
saps=read.csv("C:/Users/jsontag/Documents/CHIR Project/CHIR Data/PIPO/CHIR_PIPO_Trees-Saplings(Diameter_Class)(metric)_XPT.csv", na.strings=c("","NA"))
seeds=read.csv("C:/Users/jsontag/Documents/CHIR Project/CHIR Data/PIPO/CHIR_PIPO_Trees-Seedlings(Height_Class)(metric)_XPT.csv")
tree=read.csv("C:/Users/jsontag/Documents/CHIR Project/CHIR Data/PIPO/CHIR_PIPO_Trees-Individuals(metric)_XPT.csv")
pbsev=read.csv("C:/Users/jsontag/Documents/CHIR Project/CHIR Data/PIPO/PIPO_Post Burn Severity_XPT.csv")
covpts=read.csv("C:/Users/jsontag/Documents/CHIR Project/CHIR Data/PIPO/PIPO_Cover - Points (metric)_XPT.csv")
densbelts=read.csv("C:/Users/jsontag/Documents/CHIR Project/CHIR Data/PIPO/PIPO_Density - Belts (metric)_XPT.csv")

# what is this?
#these are some new functions! fun!
flags=c()
#new functions

#In the instruction document everything starting with fuels is the same
#as data cleaning functions for SAGU, sample events is going to be different


###SAMPLE EVENT CHECKS
chir_sample_event_qc <- function(samp, mtype) {

  samples <<- samp[which(samp$ProjectUnit_Name == mtype),] # filtering for vegetation type

  # checking for NAs in monitoring status
  nas_monitoring_status <- anyNA(samples$MonitoringStatus_Name)
  cat("Any NAs in monitoring status?\n")
  cat(nas_monitoring_status, "\n")

  # checking for NAs in protocols
  nas_protocols <- anyNA(samples$Protocols)
  cat("Any NAs in protocols?\n")
  cat(nas_protocols, "\n")

  # adding any NAs to flags
  if (nas_monitoring_status) {
    flags <- c(flags, "NAs exist in monitoring status column of sample event data")
  }
  if (nas_protocols) {
    flags <- c(flags, "NAs exist in protocol column of sample event data")
  }

  monitoring_status <- c("01Pre",    "01Post" ,  "01Year01" ,"01Year02" ,"01Year05" ,"02Year01" ,"02Year02" ,"02Year05",
                         "02Year10" , "01Year10" ,"01Year20", "00PR01","00PR02"  , "02Post"  , "02Pre", "02Year20",
                         "03Pre", "03Post" ,  "03Year01", "03Year02", "03Year05" ,"03Year10","03Year20",
                         "04Year01", "04Year02", "04Year05" ,"04Year10","04Year20")

  recorded_monitoring_status <- unique(samples$MonitoringStatus_Name)
  mislabeled_monitoring_status <- setdiff(recorded_monitoring_status, monitoring_status)
  # printing monitoring status checks
  cat("\nWhich monitoring statuses are included?\n")
  cat(paste(recorded_monitoring_status, sep="\n"))
  cat("\n")
  cat("\nWhich monitoring statuses are off-cycle?\n")
  if (length(mislabeled_monitoring_status) == 0) {
    cat("None\n")
  } else {
    cat(mislabeled_monitoring_status, sep = "\n")
    flags <- c(flags, paste("These monitoring statuses are off-cycle:", paste(mislabeled_monitoring_status, collapse = ", "), sep = " "))
  }

  #ms check with years

  # anything that says post should be the same years as a fire

  samples$SampleEvent_Date=as.Date(samples$SampleEvent_Date, "%m/%d/%Y")
  samples$year=str_split_fixed(samples$SampleEvent_Date, "-",3)[,1]


  samples=samples%>%
    mutate(fire="no_fire")



  post=samples[grep("Post", samples$MonitoringStatus_Name),]
  samples[grep("Post", samples$MonitoringStatus_Name),"fire"]="fire"



  postyears=unique(post$year)
  postplots=c()

  for(x in 1:length(postyears)){
    postplot=unique(post[which(post$year==postyears[x]), "MacroPlot_Name"], "\n")
    postplots=c(postplots, postplot)
    cat(paste(c("\nIn", postyears[x],"the following plots burned and were measured: ", postplot), collapse=" "))
  }

  cat("(Warning: some plots may have burned but were only measured in following years)")
  cat("\n")
  after=samples[grep("Year01", samples$MonitoringStatus_Name),]
  samples[grep("Year01", samples$MonitoringStatus_Name),"fire"]="fire+1"

  burnyears=as.numeric(unique(c(samples[which(samples$fire=="fire+1"), "year"])))-1



  afteryears=unique(after$year)
  afterplots=c()

  for(x in 1:length(afteryears)){
    afterplot=unique(after[which(after$year==afteryears[x]), "MacroPlot_Name"], "\n")
    afterplots=c(afterplots, afterplot)
  }
  samples[which(samples$year %in% burnyears & samples$MacroPlot_Name %in% afterplots),"fire"]="fire"

  cat("\n")

  after2=samples[grep("Year02", samples$MonitoringStatus_Name),]
  samples[grep("Year02", samples$MonitoringStatus_Name),"fire"]="fire+2"

  burnyears=as.numeric(unique(c(samples[which(samples$fire=="fire+2"), "year"])))-2


  after2years=unique(after2$year)
  after2plots=c()

  for(x in 1:length(after2years)){
    after2plot=unique(after2[which(after2$year==after2years[x]), "MacroPlot_Name"], "\n")
    after2plots=c(after2plots, after2plot)
  }
  samples[which(samples$year %in% burnyears & samples$MacroPlot_Name %in% after2plots),"fire"]="fire"


  after5=samples[grep("Year05", samples$MonitoringStatus_Name),]
  samples[grep("Year05", samples$MonitoringStatus_Name),"fire"]="fire+5"

  burnyears=as.numeric(unique(c(samples[which(samples$fire=="fire+5"), "year"])))-5


  after5years=unique(after5$year)
  after5plots=c()

  for(x in 1:length(after5years)){
    after5plot=unique(after5[which(after5$year==after5years[x]), "MacroPlot_Name"], "\n")
    after5plots=c(after5plots, after5plot)
  }
  samples[which(samples$year %in% burnyears & samples$MacroPlot_Name %in% after5plots),"fire"]="fire"


  after10=samples[grep("Year10", samples$MonitoringStatus_Name),]
  samples[grep("Year10", samples$MonitoringStatus_Name),"fire"]="fire+10"

  burnyears=as.numeric(unique(c(samples[which(samples$fire=="fire+10"), "year"])))-10


  after10years=unique(after10$year)
  after10plots=c()

  for(x in 1:length(after10years)){
    after10plot=unique(after10[which(after10$year==after10years[x]), "MacroPlot_Name"], "\n")
    after10plots=c(after10plots, after10plot)
  }


  samples=rbind(samples, after, after2, after5, after10)

  samples[which(samples$fire=="fire+1"), "year"]=as.numeric(samples[which(samples$fire=="fire+1"), "year"])-1
  samples[which(samples$fire=="fire+2"), "year"]=as.numeric(samples[which(samples$fire=="fire+2"), "year"])-2
  samples[which(samples$fire=="fire+5"), "year"]=as.numeric(samples[which(samples$fire=="fire+5"), "year"])-5
  samples[which(samples$fire=="fire+10"), "year"]=as.numeric(samples[which(samples$fire=="fire+10"), "year"])-10

  samples[which(samples$fire %in% c("fire+1","fire+2","fire+5","fire+10")), "MonitoringStatus_Name"]="01Post"
  samples[which(samples$fire %in% c("fire+1","fire+2","fire+5","fire+10")), "Visited"]="N"
  samples[which(samples$fire %in% c("fire+1","fire+2","fire+5","fire+10")), "Protocols"]="all"

  samples[which(samples$fire %in% c("fire+1","fire+2","fire+5","fire+10")), "fire"]="fire"

  #plotting stuff
  protocols <- c("Cover - Points (metric)", "Trees (metric)", "Surface Fuels",
                 "Density - Belts (metric)", "Cover - Species Composition (metric)",
                 "Post Burn Severity (metric)", "Post Burn Severity")


  a=samples[which(samples$Protocols=="all"),]
  a$Protocols="Cover - Points (metric)"
  b=samples[which(samples$Protocols=="all"),]
  b$Protocols="Trees (metric)"
  c=samples[which(samples$Protocols=="all"),]
  c$Protocols="Surface Fuels"
  d=samples[which(samples$Protocols=="all"),]
  d$Protocols="Density - Belts (metric)"
  e=samples[which(samples$Protocols=="all"),]
  e$Protocols="Cover - Species Composition (metric)"
  f=samples[which(samples$Protocols=="all"),]
  f$Protocols="Post Burn Severity (metric)"
  g=samples[which(samples$Protocols=="all"),]
  g$Protocols="Post Burn Severity"

  samples=rbind(samples, a,b,c,d,e,f,g)
  samples=samples[-which(samples$Protocols=="all"),]



  p=samples %>%
    ggplot(aes(x=year, y=MacroPlot_Name, shape=Visited))


  p=p+geom_point(aes(color=samples$MonitoringStatus_Name), size=5)+
    geom_point(aes(shape = samples$fire), size=3)+
    scale_shape_manual(values=c(16, 13,8, 1), breaks=c('Y', 'N', 'fire','no_fire'))+
    facet_wrap(~Protocols)+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust=1, size=5))

  p


  recorded_protocols <- unique(samples$Protocols)
  cat("\n")


  cat("\nValid protocols (used for Saguaro)", "\n")
  cat(protocols, sep = "\n")


  mislabeled_protocols <- setdiff(recorded_protocols, protocols)
  cat("\nDo any protocols have data that shouldn't?\n")
  if (length(mislabeled_protocols) == 0) {
    cat("None\n")
  } else {
    cat(paste(c(mislabeled_protocols,"in year", unique(samples[which(samples$Protocols %in% mislabeled_protocols),"year"]), "for plots", unique(samples[which(samples$Protocols %in% mislabeled_protocols),"MacroPlot_Name"])), collapse = ", "), sep = "\n")
    flags <- c(flags, paste("These protocols have data but do not fall under the list of valid protocols:", paste(c(mislabeled_protocols,"in year", unique(samples[which(samples$Protocols %in% mislabeled_protocols),"year"]), "for plots", unique(samples[which(samples$Protocols %in% mislabeled_protocols),"MacroPlot_Name"])), collapse = ", "), sep = " "))
  }



  output<-list(p, flags)
  return(output)
}


chir_sample_event_qc(samp, "ARPU")

chir_sample_event_qc(samp, "BOGR")

chir_sample_event_qc(samp, "PIPO")

chir_sample_event_qc(samp, "QUEM")


###COVER - POINTS (METRIC) - two separate functions for forest vs shrub

##FOREST PLOTS
#checks 1 thru 8  written from FFI queries:
  #Sampling Area Forest Check = Cvr_Pts_header_forest
  #Height Check = Cvr_Pts_Height 
  #Repeated Hits Check = Cvr_Pts_hits_repeat
  #Order Check = Cvr_Pts_hits_total
  #Status Check = Cvr_Pts_Status
  #Substrate Height Check = Cvr_Pts_SubHt 
  #Substrate Status Check = Cvr_Pts_SubStat 
  #Transect Forest Check = Cvr_Pts_Tran_forest 

covpts_qc <- function(covpts) {
  
  # Function to print and return details of flagged rows
  print_and_return_flagged <- function(data, check_name) {
    if (nrow(data) > 0) {
      print(paste(check_name, "- Rows flagged:", nrow(data)))
      print(data)
    } else {
      print(paste(check_name, "- Rows flagged: 0"))
    }
    return(data)
  }
  
  # Define a function to identify header rows
  is_header_row <- function(row) {
    !is.na(row["NumTran"]) && !is.na(row["TranLen"]) && !is.na(row["NumPtsTran"]) && !is.na(row["Offset"])
  }
  
  # 1. Sampling Area Forest Check: check sampling area information for "forest" plots (PIPO, QUEM)
  check_sampling_area_forest <- function(covpts) {
    invalid_rows <- suppressMessages(
      covpts %>%
        filter(grepl("PIPO|QUEM", Monitoring.Status)) %>%
        filter(is.na(NumTran) | NumTran != 2 | 
                 is.na(TranLen) | TranLen != 50 |
                 is.na(NumPtsTran) | NumPtsTran != 166) %>%
        select(MacroPlot.Name, Monitoring.Status, NumTran, TranLen, NumPtsTran)
    )
    return(print_and_return_flagged(invalid_rows, "Sampling Area Forest Check"))
  }
  
  # 2. Height Check: check height ≤ 2 meters
  check_height <- function(covpts) {
    invalid_rows <- suppressMessages(
      covpts %>%
        filter(!is.na(Height) & Height > 2 & !apply(covpts, 1, is_header_row)) %>%
        select(MacroPlot.Name, Monitoring.Status, Height)
    )
    return(print_and_return_flagged(invalid_rows, "Height Check"))
  }
  
  # 3. Repeated Hits Check: check for repeated herb line measurements
  check_repeated_hits <- function(covpts) {
    repeated_hits <- suppressMessages(
      covpts %>%
        group_by(MacroPlot.Name, Date, Transect, Point, Order, Species.Symbol) %>%
        filter(n() > 1) %>%
        select(MacroPlot.Name, Date, Transect, Point, Order, Species.Symbol)
    )
    return(print_and_return_flagged(repeated_hits, "Repeated Hits Check"))
  }
  
  # 4. Order Check: check sequential order
  check_order <- function(covpts) {
    invalid_order <- suppressMessages(
      covpts %>%
        group_by(MacroPlot.Name, Date, Transect) %>%
        arrange(Order) %>%
        mutate(SeqOrder = row_number()) %>%
        filter(Order != SeqOrder) %>%
        select(MacroPlot.Name, Date, Transect, Order, SeqOrder)
    )
    return(print_and_return_flagged(invalid_order, "Order Check"))
  }
  
  # 5. Status Check: check species status (Status = "L" or "D") excluding header rows
  check_status <- function(covpts) {
    invalid_status <- suppressMessages(
      covpts %>%
        filter(!(Status %in% c("L", "D")) & !apply(covpts, 1, is_header_row)) %>%
        select(MacroPlot.Name, Monitoring.Status, Status)
    )
    return(print_and_return_flagged(invalid_status, "Status Check"))
  }
  
  # 6. Substrate Height Check: check height for substrate (should be blank)
  check_substrate_height <- function(covpts) {
    invalid_substrate_height <- suppressMessages(
      covpts %>%
        filter(!is.na(Height) & CanopyLayer == "Substrate" & !apply(covpts, 1, is_header_row)) %>%
        select(MacroPlot.Name, Monitoring.Status, Height, CanopyLayer)
    )
    return(print_and_return_flagged(invalid_substrate_height, "Substrate Height Check"))
  }
  
  # 7. Substrate Status Check: check substrate status (Status = "D" or blank)
  check_substrate_status <- function(covpts) {
    invalid_substrate_status <- suppressMessages(
      covpts %>%
        filter(CanopyLayer == "Substrate" & !(Status %in% c("D", NA))) %>%
        select(MacroPlot.Name, Monitoring.Status, Status, CanopyLayer)
    )
    return(print_and_return_flagged(invalid_substrate_status, "Substrate Status Check"))
  }
  
  # 8. Transect Forest Check: check transect numbers for "forest" plots
  check_transect_forest <- function(covpts) {
    invalid_transect <- suppressMessages(
      covpts %>%
        filter(grepl("PIPO|QUEM", Monitoring.Status)) %>%
        filter(!(Transect %in% c(1, 2))) %>%
        select(MacroPlot.Name, Monitoring.Status, Transect)
    )
    return(print_and_return_flagged(invalid_transect, "Transect Forest Check"))
  }
  
  # Run all checks and store results
  results_list <- list(
    Sampling_Area_Forest_Check = check_sampling_area_forest(covpts),
    Height_Check = check_height(covpts),
    Repeated_Hits_Check = check_repeated_hits(covpts),
    Order_Check = check_order(covpts),
    Status_Check = check_status(covpts),
    Substrate_Height_Check = check_substrate_height(covpts),
    Substrate_Status_Check = check_substrate_status(covpts),
    Transect_Forest_Check = check_transect_forest(covpts)
  )
  
  # Filter out empty results (no issues found)
  results_list <- results_list[sapply(results_list, nrow) > 0]
  
  # If no issues were found, return a message
  if (length(results_list) == 0) {
    return("All checks passed without issues.")
  }
  
  # Return the list of issues with detailed flagged rows
  return(results_list)
}



###DENSITY - BELTS (METRIC) - two separate functions for forest vs shrub plots

##FOREST PLOTS
#checks 1 thru 7 written from FFI queries:
  #Age Class Check =  Dens_Belts_age
  #Subbelt Check = Dens_Belts_belt
  #Count Check = Dens_Belts_count
  #SubFrac Check = Dens_Belts_fract 
  #Forest Sampling Area Check = Dens_Belts_header_forest 
  #Status Check = Dens_Belts_status 
  #Forest Transect Check = Dens_Belts_tran_forest 
densbelts_qc <- function(densbelts) {
  
  # Function to suppress verbose output
  suppress_verbose_output <- function(expr) {
    suppressMessages(suppressWarnings(expr))
  }
  
  # Function to print and return details of flagged rows
  print_and_return_flagged <- function(data, check_name) {
    if (nrow(data) > 0) {
      cat(paste(check_name, "- Rows flagged:", nrow(data)), "\n")
      print(data)
    } else {
      cat(paste(check_name, "- Rows flagged: 0"), "\n")
    }
    return(data)
  }
  
  # Check if a row is a header row
  is_header_row <- function(row) {
    !is.na(row$NumTran) & !is.na(row$NumSubbelt) & !is.na(row$TranWid) & !is.na(row$TranLen)
  }
  
  # 1. Forest Sampling Area Check (should only check header rows)
  check_sampling_area_forest <- function(densbelts) {
    invalid_rows <- suppress_verbose_output(
      densbelts %>%
        filter(is_header_row(.)) %>%
        filter((TranLen * TranWid != 100) | is.na(TranLen) | is.na(TranWid)) %>%
        select(MacroPlot.Name, Monitoring.Status, TranLen, TranWid, Area)
    )
    return(print_and_return_flagged(invalid_rows, "Forest Sampling Area Check"))
  }
  
  # 2. Age Class Check
  check_age_class <- function(densbelts) {
    invalid_rows <- suppress_verbose_output(
      densbelts %>%
        filter(!is_header_row(.)) %>%
        filter(!AgeCl %in% c("I", "M", "R")) %>%
        select(MacroPlot.Name, Monitoring.Status, AgeCl)
    )
    return(print_and_return_flagged(invalid_rows, "Age Class Check"))
  }
  
  # 3. Subbelt Check (should be between 0-6 or equal to 10)
  check_subbelt <- function(densbelts) {
    invalid_sublbelt <- suppressMessages(
      densbelts %>%
        filter(!(Subbelt %in% c(0:6, 10))) %>%
        select(MacroPlot.Name, Monitoring.Status, Subbelt)
    )
    return(print_and_return_flagged(invalid_sublbelt, "Subbelt Check"))
  }
  
  # 4. Count Check
  check_count <- function(densbelts) {
    invalid_rows <- suppress_verbose_output(
      densbelts %>%
        filter(!is_header_row(.)) %>%
        filter(!(Count > 0 | (Species.Symbol == "XXXX" & Count == 0))) %>%
        select(MacroPlot.Name, Monitoring.Status, Species.Symbol, Count)
    )
    return(print_and_return_flagged(invalid_rows, "Count Check"))
  }
  
  # 5. SubFrac Check
  check_subfrac <- function(densbelts) {
    invalid_rows <- suppress_verbose_output(
      densbelts %>%
        filter(!is_header_row(.)) %>%
        filter(SubFrac != 1) %>%
        select(MacroPlot.Name, Monitoring.Status, Subbelt, SubFrac)
    )
    return(print_and_return_flagged(invalid_rows, "SubFrac Check"))
  }
  
  # 6. Status Check
  check_status <- function(densbelts) {
    invalid_rows <- suppress_verbose_output(
      densbelts %>%
        filter(!is_header_row(.)) %>%
        filter(!Status %in% c("L", "D")) %>%
        select(MacroPlot.Name, Monitoring.Status, Status)
    )
    return(print_and_return_flagged(invalid_rows, "Status Check"))
  }
  
  # 7. Forest Transect Check
  check_transect_forest <- function(densbelts) {
    invalid_rows <- suppress_verbose_output(
      densbelts %>%
        filter(!is_header_row(.)) %>%
        filter(Transect != 1 & Transect != 2) %>%
        select(MacroPlot.Name, Monitoring.Status, Transect)
    )
    return(print_and_return_flagged(invalid_rows, "Forest Transect Check"))
  }
  
  # Run all checks and store results
  results_list <- list(
    Sampling_Area_Forest_Check = check_sampling_area_forest(densbelts),
    Age_Class_Check = check_age_class(densbelts),
    Subbelt_Check = check_subbelt(densbelts),
    Count_Check = check_count(densbelts),
    SubFrac_Check = check_subfrac(densbelts),
    Status_Check = check_status(densbelts),
    Forest_Transect_Check = check_transect_forest(densbelts)
  )
  
  # Filter out empty results (no issues found)
  results_list <- results_list[sapply(results_list, nrow) > 0]
  
  # If no issues were found, return a message
  if (length(results_list) == 0) {
    return("All checks passed without issues.")
  }
  
  # Return the list of issues with detailed flagged rows
  return(results_list)
}









