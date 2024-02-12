library(readxl)
library(tidyverse)
library(xtable)
library(openxlsx)
library(fuzzyjoin)
setwd("C:/Users/edeegan/OneDrive - DOI/FFIqaqc/")

rm(list = ls())


#reading in historical tracker sheet
sheet_names_h <- excel_sheets("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/QAQC/PSME_Flags_tracking.xlsx")
historical <- lapply(sheet_names_h, function(x) {          # Read all sheets to list
  as.data.frame(read_excel("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/QAQC/PSME_Flags_tracking.xlsx", sheet = x)) } )

names(historical) <- sheet_names_h



#reading in yearly tracker sheet
sheet_names_y <- excel_sheets("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/QAQC/PSME 2024-02-08 flags.xlsx")
yearly <- lapply(sheet_names_y, function(x) {          # Read all sheets to list
  as.data.frame(read_excel("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/QAQC/PSME 2024-02-08 flags.xlsx", sheet = x)) } )

names(yearly) <- sheet_names_y

#comparing the two excel sheets
for(i in 1:length(yearly)){
  sheet_names_h[i]==sheet_names_y[i]

  h_df=historical[[i]]
  y_df=yearly[[i]]

  new=stringdist_right_join(h_df, y_df, by="Issue", method='dl', max_dist=20)

  new=new[!duplicated(new$Issue.y),]
  sum(is.na(new$Issue.x))
  new[which(is.na(new$Issue.x)),"Status"]="New"
  new[which(!is.na(new$Issue.x) & new$Resolved.x=="No" | new$Resolved.x=="no"),"Status"]="Previously flagged and unresolved"
  new[which(!is.na(new$Issue.x) & new$Resolved.x=="Yes" | new$Resolved.x=="yes"),"Status"]="Previously flagged and resolved"
  new[which(!is.na(new$Issue.x) & new$Resolved.x=="Yes" | new$Resolved.x=="yes"),"Resolved.y"]="Yes"
  y_df=new[,-which(grepl("x", colnames(new)))]


y_df=y_df %>% arrange(Status)


yearly[[i]]=y_df

}

todaysdate=Sys.Date()
mtype="PSME"
write.xlsx(
  x=yearly,
  file = paste(mtype, "flags_wStatus", todaysdate, ".xlsx"),
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)






