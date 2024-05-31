setwd("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/")
rm(list = ls())
mtype="PSME"
source("C:/Users/edeegan/OneDrive - DOI/FFIqaqc/R/datacleaningfunctions.R")
samp=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/SAGU_SampleEventReport.csv")
cover=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Cover - Species Composition (metric)_XPT.csv", na.strings=c("","NA"))
fuel1000=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Surface Fuels - 1000Hr_XPT.csv", na.strings=c("","NA"))
duff=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Surface Fuels - Duff_Litter_XPT.csv", na.strings=c("","NA"))
fine=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Surface Fuels - Fine_XPT.csv", na.strings=c("","NA"))
saps=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Saplings (Diameter Class) (metric)_XPT.csv", na.strings=c("","NA"))
seeds=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Seedlings (Height Class) (metric)_XPT.csv")
tree=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Trees - Individuals (metric)_XPT.csv")
pbsev=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Post Burn Severity (metric)_XPT.csv")

# what is this?
flags=c()
#new functions

#missing dbh

#every tree should have a dbh that isn't dead and down
tree=tree[-which(tree$CrwnCl=="DD" | tree$CrwnCl=="BBD"),]
tree=tree[-which(tree$Species.Symbol=="CANOPY"),]
no_dbh=tree[which(is.na(tree$DBH)),]
#also status of X - didn't find or measure

unique(no_dbh$Species.Symbol)

#species change - CHANGE TO PER PLOT
tags=unique(tree$TagNo)
for(x in 1:length(tags)){
  tree_i=tree[which(tree$TagNo==tags[x]),]
  if(length(unique(tree_i$Species.Symbol))==1){
    #all good
  }else{
    #flag
    flags<-c(flags, paste(c("Tag number ", tags[x],
                          " in tree data set switches species:", paste(tree_i$Species.Symbol, sep=", "),
                          "recorded for species in sample events",
                          paste(tree_i[which(tree_i$TagNo==tags[x]),"MacroPlot.Name"],
                          tree_i[which(tree_i$TagNo==tags[x]),"Monitoring.Status"], collapse=", ")), collapse=" "))
  }
}



#correct subfraction
poles=tree[which(tree$DBH<15.1),]
overstory=tree[which(tree$DBH>=15.1),]

poles[which(poles$SubFrac!=0.25),]
overstory[which(overstory$SubFrac!=1),]
#shrub functions

