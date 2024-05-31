library(readxl)
library(tidyverse)
library(xtable)
library(openxlsx)
library(fuzzyjoin)
setwd("C:/Users/edeegan/OneDrive - DOI/FFIqaqc/")

rm(list = ls())
#need to work on this script! - still working on it!
#working on pulls

#reading in historical tracker sheet - 2023 for example
sheet_names_h <- excel_sheets("C:/Users/edeegan/OneDrive - DOI/FFIqaqc/PSME flags_newissues 2024-02-12 .xlsx")
historical <- lapply(sheet_names_h, function(x) {          # Read all sheets to list
  as.data.frame(read_excel("C:/Users/edeegan/OneDrive - DOI/FFIqaqc/PSME flags_newissues 2024-02-12 .xlsx", sheet = x)) } )

names(historical) <- sheet_names_h



#reading in yearly tracker sheet - 2024 for example
sheet_names_y <- excel_sheets("C:/Users/edeegan/OneDrive - DOI/FFIqaqc/PSME flags_wStatus 2024-03-26 .xlsx")
yearly <- lapply(sheet_names_y, function(x) {          # Read all sheets to list
  as.data.frame(read_excel("C:/Users/edeegan/OneDrive - DOI/FFIqaqc/PSME flags_wStatus 2024-03-26 .xlsx", sheet = x)) } )

names(yearly) <- sheet_names_y

#comparing the two excel sheets
for(i in 1:length(yearly)){

  h_df=historical[[i]]
  y_df=yearly[[i]]
  colnames(y_df)[1:5]=c("Issue","Resolved","Resolved_by","Action_need","Other_notes")
  colnames(h_df)=c("Issue","Resolved","Resolved_by","Action_need","Other_notes")

  y_df[which(!is.na(y_df$Issue) & y_df$Resolved=="No" | y_df$Resolved=="no"),"Status"]="Previously flagged and unresolved"
  y_df[which(!is.na(y_df$Issue) & y_df$Resolved=="Yes" | y_df$Resolved=="yes"),"Status"]="Previously flagged and resolved"
  setaside_y=y_df[which(y_df$Status=="Previously flagged and unresolved" | y_df$Status=="Previously flagged and resolved"),]

  if(nrow(setaside_y)>0){
    y_df=y_df[-which(y_df$Status=="Previously flagged and unresolved" | y_df$Status=="Previously flagged and resolved"),]
}
  y_df=y_df[,1:5]
  colnames(y_df)=c("Issue","Resolved","Resolved_by","Action_need","Other_notes")
  h_df=h_df[,1:5]
  new=stringdist_right_join(h_df, y_df, by="Issue", method='dl', max_dist=5)

  new=new[!duplicated(new$Issue.y),]
  if(nrow(new)>0){

  new[which(is.na(new$Issue.x)),"Status"]="New"
  new[which(!is.na(new$Issue.x) & new$Resolved.x=="No" | new$Resolved.x=="no"),"Status"]="Previously flagged and unresolved"
  new[which(!is.na(new$Issue.x) & new$Resolved.x=="No" | new$Resolved.x=="no"),"Resolved.y"]="No"
  new[which(!is.na(new$Issue.x) & new$Resolved.x=="Yes" | new$Resolved.x=="yes"),"Status"]="Previously flagged and resolved"
  new[which(!is.na(new$Issue.x) & new$Resolved.x=="Yes" | new$Resolved.x=="yes"),"Resolved.y"]="Yes"
  new[which(is.na(new$Status)), "Status"]="New"
  }else{
  new[1,]=NA
  new$Status=NA
}
  y_df=new[,-which(grepl("x", colnames(new)))]
  colnames(y_df)=c("Issue","Resolved","Resolved_by","Action_need","Other_notes", "Status")
  y_df=rbind(y_df, setaside_y)
  y_df=as.data.frame(y_df)


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






