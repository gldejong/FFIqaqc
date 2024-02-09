library(readxl)
library(tidyverse)
library(xtable)
library(openxlsx)
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

  #finds rows in yearly that are not in historical
  new=setdiff(y_df$Issue, h_df$Issue)
  #finds rows in historical that have been resolved that are in yearly
  resolved=intersect(h_df[which(h_df$Resolved=="Yes"), "Issue"], y_df$Issue)
  #finds rows in historical that have NOT been resolved that are in yearly
  unresolved=intersect(h_df[which(h_df$Resolved=="No"), "Issue"], y_df$Issue)


y_df$Status=NA
#can be Previously flagged and resolved, Previously flagged and unresolved, or New issue
y_df[which(y_df$Issue %in% new),"Status"]="New"
y_df[which(y_df$Issue %in% resolved),"Status"]="Previously flagged and resolved"
y_df[which(y_df$Issue %in% unresolved),"Status"]="Previously flagged and unresolved"

y_df=y_df %>% arrange(Status)


yearly[[i]]=y_df

}

todaysdate=Sys.Date()
mtype="PSME"
write.xlsx(
  x=yearly,
  file = paste(mtype, todaysdate, "flags.xlsx"),
  col_names = TRUE,
  format_headers = TRUE,
  use_zip64 = FALSE
)


library(fuzzyjoin)

stringdist_inner_join(h_df, y_df)

