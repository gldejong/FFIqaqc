setwd("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/")
rm(list = ls())
mtype="PSME"
source("C:/Users/edeegan/OneDrive - DOI/FFIqaqc/R/datacleaningfunctions.R")
samp=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/CHIR_data/CHIR_SampleEventReport.csv")
cover=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Cover - Species Composition (metric)_XPT.csv", na.strings=c("","NA"))
fuel1000=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Surface Fuels - 1000Hr_XPT.csv", na.strings=c("","NA"))
duff=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Surface Fuels - Duff_Litter_XPT.csv", na.strings=c("","NA"))
fine=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Surface Fuels - Fine_XPT.csv", na.strings=c("","NA"))
saps=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Saplings (Diameter Class) (metric)_XPT.csv", na.strings=c("","NA"))
seeds=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Seedlings (Height Class) (metric)_XPT.csv")
tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")
pbsev=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Post Burn Severity (metric)_XPT.csv")

# what is this?
#these are some new functions! fun!
flags=c()
#new functions

#In the instruction document everything starting with fuels is the same
#as data cleaning functions for SAGU, sample events is going to be different

