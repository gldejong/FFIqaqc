#Post burn severity function

#clearing environment - fresh start!
rm(list = ls())

setwd("C:/Users/edeegan/OneDrive - DOI/FFIqaqc/")

pbsev=read.csv("C:/Users/edeegan/OneDrive - DOI/Fire_project/Fire_project/SAGU_data/PSME/PSME_Post Burn Severity (metric)_XPT.csv")

pbsev_qc=function(pbsev){

  # Helper function for validation checks
  validate <- function(column_name, valid_values, error_message) {
    # Extract non-missing values from the specified column
    values <- na.omit(pbsev[[column_name]])

    # Find indices of values that are not in the list of valid_values
    invalid_indices <- which(!values %in% valid_values)

    if (length(invalid_indices) > 0) {
      # Prepare the error message including invalid values and their row details
      error_msg <- paste(error_message, values[invalid_indices],
                         "in events/s", pbsev[which(pbsev[[column_name]] %in% values[invalid_indices]),"MacroPlot.Name"], pbsev[which(pbsev[[column_name]] %in% values[invalid_indices]), "Monitoring.Status"], collapse = "\n")
      # Print the error message and add it to the flags vector
      cat(error_msg, "\n")
      flags<- c(flags, paste("Error:", error_msg))
    } else {
      cat("All", column_name, "values are valid: TRUE\n")
    }
  }


  validate("NumTran", 4, "Invalid NumTran for post burn severity")
  validate("TranLen", 50, "Invalid TranLen for post burn severity")
  validate("NumPtsTran", 10, "Invalid NumPtsTran for post burn severity")
  validate("Sub", c(0,1,2,3,4,5), "Invalid substrate value for post burn severity")
  validate("Veg", c(0,1,2,3,4,5), "Invalid vegetation value for post burn severity")


  postburn=pbsev
  # Group by macro plot and sample event date (monitoring status)
  postburndates <- as.data.frame(str_split(unique(paste(postburn$MacroPlot.Name, postburn$Monitoring.Status, sep = ",")), ","))

  no_errors_t <- c()
  no_errors_s <- c()

  for (i in 1:ncol(postburndates)) {
    rows <- which((postburn[["MacroPlot.Name"]] == postburndates[1, i]) & (postburn[["Monitoring.Status"]] == postburndates[2, i]))

    # Check for errors in postburn transect values
    if (length(setdiff(na.omit(postburn$Transect[rows]), rep(c(1:4), each = 10))) != 0) {
      weird_rows <- which(postburn$Transect[rows] == setdiff(na.omit(postburn$Transect[rows]), rep(c(1:4), each = 10)))
      off_values <- postburn$Transect[weird_rows]

      cat("Error: Not all postburn transect values match the expected pattern.\n")
      cat("Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(postburndates[, i]), "\n")
      flags<- c(flags, paste("Error: Not all postburn transect values match the expected pattern", "Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(postburndates[, i]), "\n"), sep = " ")
      no_errors_t <- c(no_errors_t, 1)
    } else {
      no_errors_t <- c(no_errors_t, 0)
    }

    # Check for errors in postburn sample locations
    if (length(setdiff(na.omit(postburn$SampLoc[rows]), rep(c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45), each = 4))) != 0) {
      weird_rows <- which(postburn$SampLoc[rows] == setdiff(na.omit(postburn$SampLoc[rows]), rep(c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45), each = 4)))
      off_values <- postburn$SampLoc[weird_rows]

      cat("Error: Not all postburn sample locations match the expected pattern.\n")
      cat("Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(postburndates[, i]), "\n")
      no_errors_s <- c(no_errors_s, 1)
      flags<- c(flags, paste("Error: Not all postburn Sample loc values match the expected pattern", "Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(postburndates[, i]), "\n"), sep = " ")
    } else {
      no_errors_s <- c(no_errors_s, 0)
    }
  }

  if (unique(no_errors_s) == 0) {
    cat("Validation: postburn sample locations are correct.\n")
    cat("\n")
  }

  if (unique(no_errors_t) == 0) {
    cat("Validation: postburn transect values are correct.\n")
    cat("\n")
  }



}

pbsev_qc(pbsev)
