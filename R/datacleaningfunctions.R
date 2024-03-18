###Data cleaning functions


##### sample event quality control #ready to be looked at by Windy - Eva 9/11
#' Sample event quality control checks
#' @description
#' The sample_event_qc function is designed to perform quality control
#' checks on sample event data associated with different project units.
#' It takes two arguments, samples (a dataset containing sample event records)
#' and mtype (Project Unit Name). The function first filters the dataset to
#' include only the specified project unit. It then checks for missing values
#' (NAs) in the MonitoringStatus_Name, Protocols, and Visited columns. If any
#' NAs are detected, they are added to a list of flags. Additionally, the function
#' checks for mislabeled or missing monitoring statuses, protocols, and visited
#' values which will be added to flags.*IF ADJUSTING FOR DIFFERENT PROGRAM – you
#' must edit code to include monitoring statuses and protocols specific to your
#' program. This function will produce a visualization that shows which protocols
#' were collected each year and the years a fire occurred.
#' @param
#' samp
#' @param
#' mtype
#'
#' @return A list of flags or data issues in the sample events csv
#' @export
#'
#' @examples
#' sample_event_qc(samp, "PSME")
#'
sample_event_qc <- function(samp, mtype) {

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

  monitoring_status <- c("00PR01", "01Pre", "01Post", "01Year01", "01Year02", "01Year05", "02Pre", "02Post", "02Year01", "02Year02", "02Year10", "03Pre", "03Post", "03Year01", "03Year02", "00PR02", "00PR03", "00PR04", "01Year10", "01Year12", "02Year05", "03Year05","04Post", "04Year01", "04Year02", "04Pre", "04Year05", "05Post", "05Year01", "05Year02",  "02Year20", "01Year20", "00 PRE")

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
  protocols <- c("Cover - Points (metric)", "Trees (metric)", "Surface Fuels", "Density - Belts (metric)", "Cover - Species Composition (metric)", "Post Burn Severity (metric)")


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

  samples=rbind(samples, a,b,c,d,e,f)
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
#end of function

#####Protocol: Cover - species Composition (Metric) #verified 9/11 by Eva
#' Cover - species composition (metric) checks
#'@description
#' *Warning: this function is specific to Saguaro and Chiricahua national parks
#' use of canopy cover data. It most likely will not work for other programs.
#' The cover_qc function focuses on quality control checks for canopy data,
#' particularly canopy cover class values. It takes a dataset cover as input
#' and assesses the presence and correctness of canopy data. The function first
#' filters the dataset to extract canopy data and then checks if all quarters of
#' data are present for each unique macro plot and monitoring status combination.
#' If any quarter is missing, the function logs an error message. Additionally, it
#' validates the recorded canopy cover class values and provides feedback on any discrepancies.
#' @param
#' cover
#'
#' @return A list of flags or data issues in the cover csv
#' @export
#'
#' @examples
#' cover_qc(cover)
cover_qc <- function(cover) {

  canopy <- cover[which(cover$Species.Symbol == "CANOPY"),]

  if (nrow(canopy) == 0) {
    return(cat("No canopy data found. Skipping canopy-related checks"))
  }

  canopydates <- as.data.frame(str_split(unique(paste(canopy$MacroPlot.Name, canopy$Monitoring.Status, sep = ",")), ","))

  quarters <- c()
  error_rows <- c()

  # Initialize an error flag
  has_errors <- FALSE

  for(i in 1:ncol(canopydates)) {
    rows <- which((canopy[,1] == canopydates[1,i]) & (canopy[,2] == canopydates[2,i]))

    if(length(rows) == 4) {
      quarters[i] <- "All Quarters"
    } else {
      ans <- setdiff(canopy[rows, 12], c("Q1", "Q2", "Q3", "Q4"))
      cat("Error in macroplot ", canopydates[1,i], " monitoring status ", canopydates[2,i], ans, " missing\n")
      flags<- c(flags, paste("Error in macroplot ", canopydates[1,i], " monitoring status ", canopydates[2,i], ans, " missing", sep = " "))
      error_rows <- c(error_rows, paste(i))
      error_rows <- as.numeric(error_rows)

      # Set the error flag to TRUE
      has_errors <- TRUE
    }
  }

  # Display a message if there are no errors
  if (!has_errors) {
    cat("No errors found in canopy data.\n")
  }

}#end of function

# Perform quality control checks for cover class and invasive species data #verified 9/11 by Eva
#' Cover class and invasive species data
#' @description
#' *Warning: this function is specific to Saguaro and Chiricahua national parks use
#' of canopy cover data. It most likely will not work for other programs.
#' The cover_uvs_qc function is responsible for quality control checks on cover class
#' data, including canopy cover class and invasive species cover class values, as
#' well as UV descriptions. It takes a dataset cover as input and conducts validation.
#' The function first subsets the data based on UV1 values and performs checks on
#' canopy cover class values, followed by invasive species cover class values if available.
#' It then assesses the accuracy of UV descriptions (Protocol Name, Plot Quarter,
#' Count Code) associated with the cover classes.
#' @param cover
#'
#' @return A list of flags or data issues in the cover csv
#' @export
#'
#' @examples
#' cover_uvs_qc(cover)
cover_uvs_qc <- function(cover) {

  # Subset the cover data for canopy cover class
  cover_class <- cover[which(cover$UV1 == "Canopy Cover Class"),]

  # Check if there is no canopy cover class data, skip checks if empty
  if (nrow(cover_class) == 0) {
    return(cat("No canopy cover class data found. Skipping canopy cover class checks.\n"))
  }

  # Function to check cover values against acceptable values
  check_cover_values <- function(data, values, description) {
    # Convert Cover column to numeric
    data$Cover <- as.numeric(data$Cover)

    diff_values <- setdiff(data$Cover, values)
    diff_values <- diff_values[!is.na(diff_values)]  # Exclude NA values

    if (length(diff_values) == 0) {
      cat("True\n")
    } else {
      cat(paste("False, values of", diff_values, "are not acceptable for", description, "\n"))
      flags<- c(flags, paste("Values of", diff_values, "are recorded for", description, "and are not acceptable", sep = " ") )
    }
  }


  # Check canopy cover class values
  canopy_cover_values <- c(NA, 0, 10.0, 37.5, 62.5, 90, 100)
  cat("Canopy cover class values are correct...\n")
  check_cover_values(cover_class, canopy_cover_values, "canopy cover class")
  cat("\n")

  # Subset the cover data for invasive species
  invasive_class <- cover[which(cover$UV1 == "Invasive Species"),]

  # Check invasive cover class values if data is available
  if (nrow(invasive_class) > 0) {
    invasive_values <- c(NA, 0, 0.5, 3, 8, 18, 37.5, 62.5, 85, 100)
    cat("Invasive cover class values are correct...\n")
    check_cover_values(invasive_class, invasive_values, "invasive cover class")
  } else {
    cat("No invasive cover class data found. Skipping invasive cover class checks.\n")
  }
  cat("\n")
  # Function to check UV descriptions against acceptable values
  uv_description_checks <- function(desc_column, description) {
    desc_values <- setdiff(unique(na.omit(desc_column)), description)
    if (length(desc_values) == 0) {
      cat("True\n")
    } else {
      cat(paste("False,", desc_values, "is not an acceptable description for", description, "\n"))
      flags<- c(flags, paste(desc_values, "is written for", description, "and is not an acceptable description", sep = " ") )
    }
  }

  # Check UV1 descriptions
  cat("UV1 descriptions are correct...\n")
  uv_description_checks(cover$UV1Desc, "Protocol Name")
  cat("\n")
  # Check UV2 descriptions
  cat("UV2 descriptions are correct...\n")
  uv_description_checks(cover$UV2Desc, "Plot Quarter")
  cat("\n")
  # Check UV3 descriptions
  cat("UV3 descriptions are correct...\n")
  uv_description_checks(cover$UV3Desc, "Count Code")
  cat("\n")
  return(flags)
} #end function


##### Fuel 1000 function #verified 9/11 by Eva
#' Fuel 1000
#' @description
#'The fuel1000_qc function conducts quality control checks on surface fuels data
#'within the surface fuels coarse woody debris (1000-hr) data set. It begins by
#'validating decay class, transect, and slope values, and ensures that log diameter
#'values, transects sampled, and sampled transect length are within the specified range.
#'*IF ADJUSTING FOR DIFFERENT PROGRAM – you must edit code to include values specific to
#'your program. It also identifies missing diameter values. Additionally, it performs
#'outlier tests using Rosner test for the "Slope" and "Dia" columns, logging any detected
#'outliers and their details.
#'
#' @param fuel1000
#'
#' @return A list of flags or data issues in the fuel csv
#' @export
#'
#' @examples
#' fuel1000_qc(fuel1000)

fuel1000_qc <- function(fuel1000) {

  # Helper function for validation checks
  validate <- function(column_name, valid_values, error_message) {
    # Extract non-missing values from the specified column
    values <- na.omit(fuel1000[[column_name]])

    # Find indices of values that are not in the list of valid_values
    invalid_indices <- which(!values %in% valid_values)

    if (length(invalid_indices) > 0) {
      # Prepare the error message including invalid values and their row details
      error_msg <- paste(error_message, values[invalid_indices],
                         "in events/s", fuel1000[which(fuel1000[[column_name]] %in% values[invalid_indices]),"MacroPlot.Name"], fuel1000[which(fuel1000[[column_name]] %in% values[invalid_indices]), "Monitoring.Status"], collapse = "\n")
#        cat(paste(c("Error in macroplot", plots[plot], "transect", transects[transect], "1000hr fuels: multiple different values for slopes:",unique(transect_1000$Slope),  "\n"), collapse = " "))
      # Print the error message and add it to the flags vector
      cat(error_msg, "\n")
      flags<- c(flags, paste("Error:", error_msg))
    } else {
      cat("All", column_name, "values are valid: TRUE\n")
    }
  }

  # Validation checks

  # Validate DecayCl column
  validate("DecayCl", c(3, 4), "Invalid decay class value")
  cat("\n")
  # Validate NumTran column
  validate("NumTran", 4, "Invalid num trans value")
  cat("\n")
  # Validate TranLen column
  validate("TranLen", 50, "Invalid tran len value")
  cat("\n")
  # Validate Transect column
  validate("Transect", c(1, 2, 3, 4), "Error in fuel 1000, invalid transect number")
  cat("\n")
  # Validate slopes and tree diameters
  validate_slope_dia <- function(column_name, max, min, error_message) {
    # Extract non-missing values from the specified column
    values <- na.omit(fuel1000[[column_name]])

    # Find indices of values that are outside the specified range
    invalid_indices <- which(values < min | values > max)

    if (length(invalid_indices) > 0) {
      # Prepare the error message including invalid values and their row details
      error_msg <- paste(error_message, values[invalid_indices],
                         "in events/s", fuel1000[which(fuel1000[[column_name]] %in% values[invalid_indices]), "MacroPlot.Name"], fuel1000[which(fuel1000[[column_name]] %in% values[invalid_indices]), "Monitoring.Status"], collapse = "\n")

      # Print the error message and add it to the flags vector
      cat(error_msg, "\n")
      flags<- c(flags, paste("Error:", error_msg))
    } else {
      cat("All", column_name, "values are valid: TRUE\n")
    }
  }

  # Validate Slope column
  validate_slope_dia("Slope", 90, 0, "Not all slopes are under 90 in surface fuels - 1000hr")
  cat("\n")
  # Validate Dia column
  validate_slope_dia("Dia", max(fuel1000$Dia), 3, "Not all tree diameters are greater than 3 in surface fuels - 1000hr")
  # Function to perform outlier test using Rosner test on a specified column of data
  # and log any outliers detected along with their details
  cat("\n")
  test_for_outliers <- function(data, column_name) {
    # Display a message indicating the test being performed
    cat("Are there any outlier values in", column_name, "?\n")

    # Extract non-missing and non-NA/NaN/Inf values from the specified column
    column_values <- na.omit(data[[column_name]])

    # Check if there are any valid values for testing
    if (length(column_values) == 0) {
      # Print a message if no valid values are available for testing
      cat("No valid values to test\n")
      return()  # Exit the function if there are no valid values
    }

    # Perform Rosner test on the valid column values
    test <- rosnerTest(column_values)
    test <- test$all.stats
    outliers <- test[which(test$Outlier == TRUE), 4]

    # Check if any outliers were detected
    if (length(outliers) == 0) {
      # Print a message if no outliers were detected
      cat("No\n")
    } else {
      # Print information about the detected outliers and their details
      cat(paste("Yes, the outlier values according to a Rosner test are", outliers, ". They are in events",
                data[which(data[[column_name]] %in% outliers),"MacroPlot.Name"],data[which(data[[column_name]] %in% outliers),"Monitoring.Status"], "of the data table. For reference, the max, min, and mean of",
                column_name, "are", max(column_values), min(column_values), mean(column_values), "respectively",
                collapse = "\n"), "\n")

      # Add error messages about the detected outliers to the flags vector
      flags<- c(flags, paste("The", column_name, "has outlier values according to a Rosner test, which are", outliers,
                             ". They are in events", data[which(data[[column_name]] %in% outliers),"MacroPlot.Name"],data[which(data[[column_name]] %in% outliers),"Monitoring.Status"],
                             "of the data table. The max, min, and mean of", column_name, "'s are", max(column_values),
                             min(column_values), mean(column_values), "respectively", collapse = "\n"), "\n")
    }
  }

  # Perform outlier tests for the "Slope" and "Dia" columns in the fuel1000 dataset
  test_for_outliers(fuel1000, "Slope")
  cat("\n")
  test_for_outliers(fuel1000, "Dia")


  return(flags) } #end function


###
#####Fuel fine vs 1000 hr slope azimuth consistency check #verified 9/11 by Eva
#' Fuel fine vs 1000 hr slope azimuth consistency check
#' @description
#'  The transect_slope_azimuth_qc function conducts quality control checks on slope
#'  and azimuth data in the surface fuels coarse woody debris (1000-hr) and fine woody
#'  debris datasets. It uses macroplot names in the two datasets and compares slope and
#'  azimuth values within each transect. The function flags errors if ifferent values are
#'  detected for slope or azimuth within transects. It also checks for overall consistency
#'  in slope and azimuth values across years.
#' @param
#' fuel1000
#' @param
#' fine
#'
#' @return A list of flags or data issues with slope consistency
#' @export
#'
#' @examples
#' transect_slope_azimuth_qc(fuel1000, fine)

transect_slope_azimuth_qc <- function(fuel1000, fine) {
  # Print a message indicating that plots are being compared between 1000Hr fuels and fine fuels
  cat("Plots are the same in 1000Hr fuels and fine fuels...\n")

  # Check if all unique MacroPlot names in 'fuel1000' are the same as in 'fine'
  if (all(unique(fuel1000$MacroPlot.Name) %in% unique(fine$MacroPlot.Name))) {
    cat("TRUE\n")
    cat("\n")
  } else {
    # Print an error message and update flags if there are differing plot names
    flags<- c(flags, "There is a plot that differs between 1000hr fuels and fine fuels - check this before running function \n")
    return(cat("FALSE, there is a plot that differs between 1000hr fuels and fine fuels - check this before running function \n"))
  }

  # Get unique MacroPlot names from 'fuel1000'
  plots <- unique(fuel1000$MacroPlot.Name)

  # Initialize empty vectors to store error flags for slope and azimuth
  no_errors_s <- c()
  no_errors_a <- c()

  # Loop over each unique MacroPlot name
  for (plot in 1:length(plots)) {
    macroplot_1000 <- fuel1000[which(fuel1000$MacroPlot.Name == plots[plot]), ]
    macroplot_fine <- fine[which(fine$MacroPlot.Name == plots[plot]), ]

    # Determine the set of transects to compare based on their availability in 'fuel1000' and 'fine'
    if (length(unique(macroplot_1000$Transect)) > length(unique(macroplot_fine$Transect))) {
      transects <- na.omit(unique(macroplot_1000$Transect))
    } else if (length(unique(macroplot_1000$Transect)) < length(unique(macroplot_fine$Transect))) {
      transects <- na.omit(unique(macroplot_fine$Transect))
    } else {
      transects <- na.omit(unique(macroplot_fine$Transect))
    }

    # Loop over each unique transect within the current MacroPlot
    for (transect in 1:length(transects)) {
      transect_1000 <- macroplot_1000[which(macroplot_1000$Transect == transects[transect]), ]
      transect_fine <- macroplot_fine[which(macroplot_fine$Transect == transects[transect]), ]

      # Compare slope values within 1000Hr fuels
      if (length(unique(transect_1000$Slope)) == 1 | length(unique(transect_1000$Slope)) == 0) {
        no_errors_s <- c(no_errors_s, 0)
      } else {
        no_errors_s <- c(no_errors_s, 1)
        cat(paste(c("Error in macroplot", plots[plot], "transect", transects[transect], "1000hr fuels: multiple different values for slopes:",unique(transect_1000$Slope),  "\n"), collapse = " "))
        cat("\n")
        flags<- c(flags, paste(c("Error in macroplot", plots[plot], "transect", transects[transect], "1000hr fuels: multiple different values for slopes:",unique(transect_1000$Slope)), collapse = " "))
      }

      # Compare slope values within fine fuels
      if (length(unique(transect_fine$Slope)) == 1 | length(unique(transect_fine$Slope)) == 0) {
        no_errors_s <- c(no_errors_s, 0)
      } else {
        no_errors_s <- c(no_errors_s, 1)
        cat(paste(c("Error in macroplot", plots[plot], "transect", transects[transect], "fine fuels: multiple different values for slopes:", unique(transect_fine$Slope), "\n"), collapse = " "))
        cat("\n")
        flags<- c(flags, paste(c("Error in macroplot", plots[plot], "transect", transects[transect], "fine fuels: multiple different values for slopes:", unique(transect_fine$Slope)), collapse = " "))
      }

      # Compare MacroPlot azimuth values within 1000Hr fuels
      if (length(unique(transect_1000$MacroPlot.Azimuth)) == 1 | length(unique(transect_1000$MacroPlot.Azimuth)) == 0) {
        no_errors_a <- c(no_errors_a, 0)
      } else {
        no_errors_a <- c(no_errors_a, 1)
        cat(paste(c("Error in macroplot", plots[plot], "transect", transects[transect], "1000hr fuels: multiple different values for MacroPlot.Azimuths:", unique(transect_1000$MacroPlot.Azimuth), "\n"), collapse = " "))
        cat("\n")
        flags<- c(flags, paste(c("Error in macroplot", plots[plot], "transect", transects[transect], "1000hr fuels: multiple different values for MacroPlot.Azimuths:", unique(transect_1000$MacroPlot.Azimuth)), collapse = " "))
      }

      # Compare MacroPlot azimuth values within fine fuels
      if (length(unique(transect_fine$MacroPlot.Azimuth)) == 1 | length(unique(transect_fine$MacroPlot.Azimuth)) == 0) {
        no_errors_a <- c(no_errors_a, 0)
      } else {
        no_errors_a <- c(no_errors_a, 1)
        cat(paste(c("Error in macroplot", plots[plot], "transect", transects[transect], "fine fuels: multiple different values for MacroPlot.Azimuths:", unique(transect_fine$MacroPlot.Azimuth),"\n"), collapse = " "))
        cat("\n")
        flags<- c(flags, paste(c("Error in macroplot", plots[plot], "transect", transects[transect], "fine fuels: multiple different values for MacroPlot.Azimuths:", unique(transect_fine$MacroPlot.Azimuth)), collapse = " "))
      }

      # Compare slope values between fine and 1000Hr fuels
      if (length(unique(transect_fine$Slope)) == 1 & length(unique(transect_1000$Slope)) == 1) {
        if (unique(transect_1000$Slope) == unique(transect_fine$Slope)) {
          no_errors_s <- c(no_errors_s, 0)
        } else {
          no_errors_s <- c(no_errors_s, 1)
          cat(paste(c("Error in macroplot", plots[plot], "transect", transects[transect], ": different values for fine (",unique(transect_fine$Slope),") and 1000hr (",unique(transect_1000$Slope),") slopes\n"), collapse = " "))
          cat("\n")
          flags<- c(flags, paste(c("Error in macroplot", plots[plot], "transect", transects[transect], ": different values for fine and 1000hr slopes"), collapse = " "))
        }
      } else if (length(unique(transect_fine$Slope)) == 0 | length(unique(transect_1000$Slope)) == 0) {
        # Cannot compare slope values between fine and 1000Hr fuels
        # This is acceptable
      } else {
        # Error - More than 1 value for slopes in either fine or 1000Hr fuels, cannot compare values
        # This should have already been flagged
      }

      # Compare MacroPlot azimuth values between fine and 1000Hr fuels
      if (length(unique(transect_fine$MacroPlot.Azimuth)) == 1 & length(unique(transect_1000$MacroPlot.Azimuth)) == 1) {
        if (unique(transect_1000$MacroPlot.Azimuth) == unique(transect_fine$MacroPlot.Azimuth)) {
          no_errors_a <- c(no_errors_a, 0)
        } else {
          no_errors_a <- c(no_errors_a, 1)
          cat(paste(c("Error in macroplot", plots[plot], "transect", transects[transect], ": different values for fine (",unique(transect_fine$MacroPlot.Azimuth),")and 1000hr (",unique(transect_1000$MacroPlot.Azimuth),") MacroPlot.Azimuths\n"), collapse = " "))
          cat("\n")
          flags<- c(flags, paste(c("Error in macroplot", plots[plot], "transect", transects[transect], ": different values for fine and 1000hr MacroPlot.Azimuths"), collapse = " "))
        }
      } else if (length(unique(transect_fine$MacroPlot.Azimuth)) == 0 | length(unique(transect_1000$MacroPlot.Azimuth)) == 0) {
        # Cannot compare MacroPlot azimuth values between fine and 1000Hr fuels
        # This is acceptable
      } else {
        # Error - More than 1 value for MacroPlot.Azimuths in either fine or 1000Hr fuels, cannot compare values
        # This should have already been flagged
      }
    }
  }

  # Check if all transects have consistent slope values
  if (length(unique(no_errors_s)) == 1) {
    if (unique(no_errors_s) == 0) {
      cat("Slope for a transect does not change across years or between fine and 1000hr fuel data\n")
      cat("\n")
    }
  }

  # Check if all transects have consistent azimuth values
  if (length(unique(no_errors_a)) == 1) {
    if (unique(no_errors_a) == 0) {
      cat("Azimuth for a transect does not change across years or between fine and 1000hr fuel data\n")
      cat("\n")
    }
  }

  return(flags)
} # end of function


#####Protocol: Surface Fuels - Duff Litter #verified 9/11 by Eva
#' Duff Litter
#' @description
#' The duff_qc function performs quality control checks on surface fuels duff
#' and litter dataset. It first validates that the number of transects sampled is
#' consistent with monitoring protocols (*may need editing to be consistent with
#' program protocols) sample events have a different count, it generates an error
#' message, indicating the sample event (macroplot + date), and transect numbers.
#' It reports any missing or duplicate sample locations. Additionally, the function
#' checks for outlier values in litter depth and duff depth using the Rosner test,
#' generating messages if outliers are detected. Lastly, it checks for null values
#' in both variables, reporting the affected sample events, transects, and associated
#' comments. The function compiles all error messages in the 'flags' vector and returns it.
#' @param duff
#'
#' @return A list of flags or data issues in the duff csv
#' @export
#'
#' @examples
#' duff_qc(duff)
duff_qc <- function(duff) {
  # Print a message about checking duff number transects
  cat("All duff number transects are 4\n")

  # Checking if any duff number transects are not equal to 4
  if (any(unique(na.omit((duff$NumTran) == 4)) == FALSE)) {
    # Generate an error message if not all transects are 4
    message <- paste(
      "FALSE",
      "Not all duff number transects are 4 in surface fuels - duff litter, sample events:",
      duff[which(duff$NumTran %in% setdiff(unique(na.omit(duff$NumTran)), 4)), "MacroPlot.Name"], duff[which(duff$NumTran %in% setdiff(unique(na.omit(duff$NumTran)), 4)), "Monitoring.Status"],
      "has a value of",
      setdiff(unique(na.omit(duff$NumTran)), 4),
      ", comment section says",
      duff[which(duff$NumTran %in% setdiff(unique(na.omit(duff$NumTran)), 4)), "SaComment"],
      collapse = "\n")

    cat(message, "\n")
    cat("\n")
    flags<- c(flags, message)  # Add the error message to the 'flags' vector
  } else {
    cat("TRUE\n")
    cat("\n")
  }

  # Group by macro plot and sample event date (monitoring status)
  duffdates <- as.data.frame(str_split(unique(paste(duff$MacroPlot.Name, duff$Monitoring.Status, sep = ",")), ","))

  no_errors_t <- c()
  no_errors_s <- c()

  for (i in 1:ncol(duffdates)) {
    rows <- which((duff[["MacroPlot.Name"]] == duffdates[1, i]) & (duff[["Monitoring.Status"]] == duffdates[2, i]))

    # Check for errors in duff transect values
    if (length(setdiff(na.omit(duff$Transect[rows]), rep(c(1:4), each = 10))) != 0) {
      weird_rows <- which(duff$Transect[rows] == setdiff(na.omit(duff$Transect[rows]), rep(c(1:4), each = 10)))
      off_values <- duff$Transect[weird_rows]

      cat("Error: Not all duff transect values match the expected pattern.\n")
      cat("Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(duffdates[, i]), "\n")
      flags<- c(flags, paste("Error: Not all duff transect values match the expected pattern", "Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(duffdates[, i]), "\n"), sep = " ")
      no_errors_t <- c(no_errors_t, 1)
    } else {
      no_errors_t <- c(no_errors_t, 0)
    }

    # Check for errors in duff sample locations
    if (length(setdiff(na.omit(duff$SampLoc[rows]), rep(c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45), each = 4))) != 0) {
      weird_rows <- which(duff$SampLoc[rows] == setdiff(na.omit(duff$SampLoc[rows]), rep(c(1, 5, 10, 15, 20, 25, 30, 35, 40, 45), each = 4)))
      off_values <- duff$SampLoc[weird_rows]

      cat("Error: Not all duff sample locations match the expected pattern.\n")
      cat("Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(duffdates[, i]), "\n")
      no_errors_s <- c(no_errors_s, 1)
      flags<- c(flags, paste("Error: Not all duff Sample loc values match the expected pattern", "Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(duffdates[, i]), "\n"), sep = " ")
    } else {
      no_errors_s <- c(no_errors_s, 0)
    }
  }

  if (unique(no_errors_s) == 0) {
    cat("Validation: Duff sample locations are correct.\n")
    cat("\n")
  }

  if (unique(no_errors_t) == 0) {
    cat("Validation: Duff transect values are correct.\n")
    cat("\n")
  }

  # Function to check for outliers and generate messages
  check_outliers <- function(data, variable_name) {
    # Print a message about checking for outlier values
    cat(paste("Are there any outlier values in", variable_name, "?\n"))

    # Run the Rosner test to identify outliers
    test <- suppressWarnings(rosnerTest(data))
    outliers <- test$all.stats$Value[test$all.stats$Outlier]

    if (length(outliers) == 0) {
      cat("No\n")
      cat("\n")
    } else {
      # Generate a message about outlier values
      outlier_indices <- which(data %in% outliers)
      max_value <- max(na.omit(data))
      min_value <- min(na.omit(data))
      mean_value <- mean(na.omit(data))

      message <- paste(
        "Yes, the outlier values according to a rosner test are", outliers,
        ". They are in events", duff[outlier_indices, "MacroPlot.Name"],duff[outlier_indices, "Monitoring.Status"],"rows", outlier_indices,
        "of the duff data table. For reference, the max, min, and mean of", variable_name,
        "are", max_value, min_value, mean_value, "respectively",
        collapse = "\n"
      )

      cat(message, "\n")
      cat("\n")
      flags<- c(flags, message)  # Add the outlier message to the 'flags' vector
    }
  }

  # Call the check_outliers function for litter depth and duff depth
  check_outliers(duff$LittDep, "litter depth")
  check_outliers(duff$DuffDep, "duff depth")

  #check for null values
  # Function to check for outliers and generate messages
  check_nulls <- function(data, variable_name) {
    # Print a message about checking for null values
    cat(paste("Are there any null values in", variable_name, "?\n"))

    # identify null values

    nulls <- which(is.na(data) & !is.na(duff[, c("Transect", "SampLoc")]))

    if (length(nulls) == 0) {
      cat("No\n")
      cat("\n")
    } else {
      # Generate a message about outlier values
      null_values <- data[nulls]


      message <- paste(
        "Yes. They are in events", duff[nulls, "MacroPlot.Name"],duff[nulls, "Monitoring.Status"], "transect and sample locations", unique(duff[nulls,c("Transect", "SampLoc")]),
        "of the duff data table. Comments say",duff[nulls, c("SaComment", "Comment")],
        collapse = " "
      )

      cat(message, "\n")
      cat("\n")
      flags<- c(flags, message)  # Add the outlier message to the 'flags' vector
    }
  }

  check_nulls(duff$LittDep, "litter depth")
  check_nulls(duff$DuffDep, "duff depth")

  return(flags) } #end function

#####Protocol: Surface Fuels - Fine Litter #verified 9/12 by Eva
#' Fine Litter Fuels
#' @description
#' The fine_fuels_qc function performs quality control checks on surface
#'  fuels fine woody debris dataset. It verifies if the transect lengths
#'  for One Hour, Ten Hour, and Hundred Hour are consistent with monitoring
#'  protocols, are consistently 6 (may differ between programs and require
#'  editing in the code) and identifies sample events with missing transect
#'  length information. The number of transects is also verified to always be 4.
#'  Finally, fuel counts are run through an outlier test. Any inconsistencies or
#'  issues detected during the checks are flagged for further investigation.
#' @param fine
#'
#' @return A list of flags or data issues in the fine fuels csv
#' @export
#'
#' @examples
#' fine_fuels_qc(fine)
#'
fine_fuels_qc <- function(fine) {
  # Check if One Hour Transect Length is always 6
  cat("One Hour Transect Length is consistent with monitoring protocols...\n")
  if (length(unique(na.omit(fine$OneHrTranLen))) == 1) {
   if(unique(na.omit(fine$OneHrTranLen)) == 6){
     cat("TRUE\n")
     cat("\n")
   }else{
     cat("FALSE, see flagged sample events:\n")
     cat("\n")
     flags<- c(flags, paste("One Hour Transect Length is not consistent with monitoring protocols, see sample events:", fine[which(fine$OneHrTranLen!=6), "MacroPlot.Name"], fine[which(fine$OneHrTranLen!=6), "Monitoring.Status"]))
   }
  } else {
    cat("FALSE, see flagged sample events:\n")
    cat("\n")
    flags<- c(flags, paste("One Hour Transect Length is not consistent with monitoring protocols, see sample events:", fine[which(fine$OneHrTranLen!=6), "MacroPlot.Name"], fine[which(fine$OneHrTranLen!=6), "Monitoring.Status"]))
  }

  # Check if Ten Hour Transect Length is consistent with monitoring protocols
  cat("Ten Hour Transect Length is consistent with monitoring protocols...\n")
  if (length(unique(na.omit(fine$TenHrTranLen))) == 1) {
    if(unique(na.omit(fine$TenHrTranLen)) == 6){
      cat("TRUE\n")
      cat("\n")
    }else{
      cat("FALSE, see flagged sample events\n")
      cat("\n")
      flags<- c(flags, paste("Ten Hour Transect Length is not consistent with monitoring protocols, see sample events:", fine[which(fine$TenHrTranLen!=6), "MacroPlot.Name"], fine[which(fine$TenHrTranLen!=6), "Monitoring.Status"]))
    }
  } else {
    cat("FALSE, see flagged sample events\n")
    cat("\n")
    flags<- c(flags, paste("Ten Hour Transect Length is not consistent with monitoring protocols, see sample events:", fine[which(fine$TenHrTranLen!=6), "MacroPlot.Name"], fine[which(fine$TenHrTranLen!=6), "Monitoring.Status"]))
  }

  # Check if Hundred Hour Transect Length is consistent with monitoring protocols
  cat("Hundred Hour Transect Length is consistent with monitoring protocols...\n")
  if (length(unique(na.omit(fine$HunHrTranLen))) == 1) {
    if(unique(na.omit(fine$HunHrTranLen)) == 12){
      cat("TRUE\n")
      cat("\n")
    }else{
      cat("FALSE, see flagged sample events\n")
      cat("\n")
      flags<- c(flags, paste("Hundred Hour Transect Length is not consistent with monitoring protocols, see sample events:", fine[which(fine$HunHrTranLen!=12), "MacroPlot.Name"], fine[which(fine$HunHrTranLen!=12), "Monitoring.Status"]))
    }
  } else {
    cat("FALSE, see flagged sample events\n")
    cat("\n")
    flags<- c(flags, paste("Hundred Hour Transect Length is not consistent with monitoring protocols, see sample events:", fine[which(fine$HunHrTranLen!=12), "MacroPlot.Name"], fine[which(fine$HunHrTranLen!=12), "Monitoring.Status"]))
  }

  # Check for misplaced fuel values in transect length row
  cat("Any NA's for transect where they shouldn't be?\n")

  # Check for NAs in transect length columns (OneHr, TenHr, HunHr)
  if (anyNA(fine[which(!is.na(fine$OneHr) & !is.na(fine$TenHr) & !is.na(fine$HunHr)), "Transect"])) {
    missing_events <- fine[is.na(fine[which(!is.na(fine$OneHr) & !is.na(fine$TenHr) & !is.na(fine$HunHr)), "Transect"]),]
    missing_events_str <- paste(missing_events$MacroPlot.Name, missing_events$Monitoring.Status, sep = ": ")

    cat("There are NAs for the transect for the following sample events: ", missing_events_str, "\n")
    cat("\n")
    flags<- c(flags, "There are NAs in the columns that should be specifying the transect in the fine fuels data", missing_events_str, "\n")
  }else{
    cat("No")
    cat("\n")
  }



  # Check for misplaced fuel values in NumTran row
  cat("Any NA's for number of transects where they shouldn't be?\n")

  # Check for NAs in number of transect column (OneHr, TenHr, HunHr)
  if (anyNA(fine[which(is.na(fine$OneHr) & is.na(fine$TenHr) & is.na(fine$HunHr)), "NumTran"])) {
    missing_events <- fine[is.na(fine[which(is.na(fine$OneHr) & is.na(fine$TenHr) & is.na(fine$HunHr)), "NumTran"]),]
    missing_events_str <- paste(missing_events$MacroPlot.Name, missing_events$Monitoring.Status, sep = ": ")

    cat("There are NAs for NumTran for the following sample events: ", missing_events_str, "\n")
    cat("\n")
    flags<- c(flags, "There are NAs in the columns that should be specifying the NumTran in the fine fuels data", missing_events_str, "\n")
  }else{
    cat("No")
    cat("\n")
  }

#Check for NumTran=4
  cat("Are all number of transects (NumTran) equal to 4?\n")
if(length(na.omit(unique(fine$NumTran)))==1){
  if(na.omit(unique(fine$NumTran))==4){
    #all good
    cat("Yes\n")
  }else{
    #all number of transects equal to something other than 4
    cat("No, number of transects equal to",na.omit(unique(fine$NumTran)), "all sample events have the same problem.\n")
  }
}else{
  #no
  cat("FALSE, see flagged sample events\n")
  cat("\n")
  flags<- c(flags, paste("Number of transects (numtran) is", fine[which(fine$NumTran!=4), "NumTran"], "not 4, see sample events:", fine[which(fine$NumTran!=4), "MacroPlot.Name"], fine[which(fine$NumTran!=4), "Monitoring.Status"]
                         , "comments say", fine[which(fine$NumTran!=4), "SaComment"]))
}





  # Group by macro plot and sample event date (monitoring status)
  finedates <- as.data.frame(str_split(unique(paste(fine$MacroPlot.Name, fine$Monitoring.Status, sep = ",")), ","))

  no_errors <- c()
  cat("\n")
  cat("Do transect labels match the expected pattern?")

  # Loop through each unique combination of macro plot and sample event date
  for (i in 1:ncol(finedates)) {
    #selecting rows for sample event
    rows <- which((fine[["MacroPlot.Name"]] == finedates[1, i]) & (fine[["Monitoring.Status"]] == finedates[2, i]))

    # Check for errors in fine transect values
    if (length(setdiff(na.omit(fine$Transect[rows]), c(1:4))) != 0 | length(setdiff(c(1:4), na.omit(fine$Transect[rows]))) != 0) {
      weird_rows <- which(fine$Transect[rows] == setdiff(na.omit(fine$Transect[rows]), c(1:4)))
      if(length(weird_rows)==0){
        off_values=setdiff(c(1:4), na.omit(fine$Transect[rows]))
        cat("\n")
        cat("Error: Not all fine transect values match the expected pattern.\n")
        cat("Transect: ", paste(off_values, collapse = ", "), "missing in sample event", paste(finedates[,i]),", comments say", unique(fine$Comment[rows]),unique(fine$SaComment[rows]),  "\n")
        flags<- c(flags, paste("Error: Not all fine transect values match the expected pattern",
                               "Transect: ", paste(off_values, collapse = ", "), "missing in sample event"
                               , paste(finedates[, i], collapse=","),", comments say",
                               paste(unique(fine$Comment[rows]),unique(fine$SaComment[rows]), collapse=","),  "\n", collapse=" "))
        no_errors <<- c(no_errors, 1)
      }else{
        off_values <- fine$Transect[weird_rows]
        cat("\n")
        cat("Error: Not all fine transect values match the expected pattern.\n")
        cat("Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(finedates[, i]), "\n")
        flags<- c(flags, paste("Error: Not all fine transect values match the expected pattern", "Offending values: ", paste(off_values, collapse = ", "), "in sample event", paste(finedates[, i]), "\n"), sep = " ")

      }


      no_errors <<- c(no_errors, 1)
    } else if(any(duplicated(fine$Transect[rows]))){
      indices <- which(duplicated(fine$Transect[rows]))
      duplicated_values <- fine$Transect[indices]
      cat("\n")
      cat("Error: Transect values are duplicated.\n")
      cat("Duplicated transect values: ", paste(duplicated_values, collapse = ", "), "in sample event", paste(finedates[, i]), "\n")
      flags<- c(flags, paste("Error: Transect values are duplicated", "Offending values: ", paste(duplicated_values, collapse = ", "), "in sample event", paste(finedates[, i]), "\n"), sep = " ")
      no_errors <<- c(no_errors, 1)

    }else{
      no_errors <- c(no_errors, 0)
    }

  }


  if (unique(no_errors) == 0) {
    cat("\n")
    cat("Transect labels are correct.\n")
    cat("\n")
  }

  #check for null values
  # Function to check for outliers and generate messages
  check_nulls <- function(data, variable_name) {
    # Print a message about checking for null values
    cat(paste("Are there any null values in", variable_name, "?\n"))

    # identify null values

    nulls <- which(is.na(data) & !is.na(fine[, "Transect"]))

    if (length(nulls) == 0) {
      cat("No\n")
      no_errors <- c(no_errors, 0)
      cat("\n")
    } else {
      no_errors <<- c(no_errors, 1)
      # Generate a message about outlier values
      null_values <- data[nulls]


      message <- paste(
        "Yes. They are in events", fine[nulls, "MacroPlot.Name"],fine[nulls, "Monitoring.Status"], "transects", unique(fine[nulls,"Transect"]),
        "of the fine data table. Comments say",fine[nulls, c("SaComment", "Comment")],
        collapse = " "
      )

      cat(message, "\n")
      cat("\n")
      flags<- c(flags, message)  # Add the outlier message to the 'flags' vector
    }
  }

  check_nulls(fine$OneHr, "one hour fuel")
  check_nulls(fine$TenHr, "ten hour fuel")
  check_nulls(fine$HunHr, "hundred hour fuel")

  # Validate fine transect values
  if (length(unique(no_errors)) == 1) {
    if (unique(no_errors) == 0) {
      cat("Validation: fine transect values are correct.\n")
    }
  }

  test_for_outliers <- function(data, column_name) {
    # Display a message indicating the test being performed
    cat("Are there any outlier values in", column_name, "?\n")

    # Extract non-missing and non-NA/NaN/Inf values from the specified column
    column_values <- na.omit(data[[column_name]])

    # Check if there are any valid values for testing
    if (length(column_values) == 0) {
      # Print a message if no valid values are available for testing
      cat("No valid values to test\n")
      return()  # Exit the function if there are no valid values
    }

    # Perform Rosner test on the valid column values
    test <- rosnerTest(column_values)
    test <- test$all.stats
    outliers <- test[which(test$Outlier == TRUE), 4]

    # Check if any outliers were detected
    if (length(outliers) == 0) {
      # Print a message if no outliers were detected
      cat("No\n")
    } else {
      # Print information about the detected outliers and their details
      cat(paste("Yes, the outlier values according to a Rosner test are", outliers, ". They are in events",
                data[which(data[[column_name]] %in% outliers),"MacroPlot.Name"],data[which(data[[column_name]] %in% outliers),"Monitoring.Status"], "of the data table. For reference, the max, min, and mean of",
                column_name, "are", max(column_values), min(column_values), mean(column_values), "respectively",
                collapse = "\n"), "\n")

      # Add error messages about the detected outliers to the flags vector
      flags<- c(flags, paste("The", column_name, "has outlier values according to a Rosner test, which are", outliers,
                             ". They are in events", data[which(data[[column_name]] %in% outliers),"MacroPlot.Name"],data[which(data[[column_name]] %in% outliers),"Monitoring.Status"],
                             "of the data table. The max, min, and mean of", column_name, "'s are", max(column_values),
                             min(column_values), mean(column_values), "respectively", collapse = "\n"), "\n")
    }

  }
  # Perform outlier tests count of fuels in fine dataset
  test_for_outliers(fine, "OneHr")
  cat("\n")
  test_for_outliers(fine, "TenHr")
  cat("\n")
  test_for_outliers(fine, "HunHr")
  cat("\n")




  return(flags) } #end function

#####Protocol: Saplings #verified 9/12 by Eva
#' Saplings
#' @description
#' The saplings_qc function is designed to perform quality control checks on a
#' dataset of saplings. It specifically focuses on detecting and addressing potential
#' errors related to the macroplot size recorded for each sapling. The function begins
#' by checking whether the macroplot size is consistently recorded as 0 for all saplings,
#' which would indicate that they were not sampled by diameter class. If the unique
#' value of macroplot size is found to be 0, the function prints "TRUE" to indicate
#' the absence of errors. In case the macroplot size is not uniformly 0, the function
#'  generates an error message indicating that the value(s) should be replaced with
#'   0 in the macroplot size list for saplings.
#'
#' @param saps
#'
#' @return A list of flags or data issues in the saplings csv
#' @export
#'
#' @examples
#' saplings_qc(saps)
#'
saplings_qc <- function(saps) {

  # Check for data incorrectly entered in the sapling tab (saplings recorded as individuals at SAGU)
  cat("Macroplot size is 0 for all saplings (they weren't sampled by diameter class...)\n")

  # Check if unique MacroPlotSize value is 0
  if (length(unique(saps$MacroPlotSize)) == 1) {
    if (unique(saps$MacroPlotSize) == 0) {
      cat("TRUE\n")
    } else {
      error_message <- paste("FALSE, value of", unique(saps$MacroPlotSize), "should be replaced by 0 in macroplot size list for saplings", sep = " ")
      cat(error_message, "\n")
      flags<- c(flags, error_message)
    }
  } else {
    error_message <- paste("FALSE, value of", unique(saps$MacroPlotSize), "should be replaced by only 0 in macroplot size list for saplings", sep = " ")
    cat(error_message, "\n")
    flags<- c(flags, error_message)
  }
  return(flags) } #end function


#####Protocol: Seedlings #flags could be cleaned up, otherwise works verified 9/12 by Eva

#' Seedlings
#' @description
#' The seedlings_qc function performs quality control checks on seedling tree data within the
#' given 'seeds' dataset. It covers various aspects, ensuring the accuracy and integrity of
#' the information. The function begins by validating height class entries, flagging any
#' values outside the acceptable class values (0.15, 0.3, 0.6, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' identifies and flags heights above 4 or null values, providing detailed information about the problematic rows.
#'The function also checks for accurate species coding when the count value is 0, ensuring
#'that the species code is "TREE1." It uses the Rosner test to detect and flag outliers in seedling tree counts.
#'The section related to subplot fractions verifies specific species symbols, such as
#'"JUNI1" and "QUAR1," ensuring that their product with microplot size equals 0.005.
#'For other species, the subplot fraction should be 1. Inconsistencies are flagged,
#' providing details about problematic sample events.
#'Microplot size checks are performed based on monitoring types, ensuring adherence to
#'specified values (0.025 for PSME monitoring type, 0.005 for PIPO) and flagging any
#'discrepancies. The function also validates the 'Status' column, ensuring all entries
#'are marked as "L" for living, flagging dead seedlings. It is recommended that programs
#' who include dead seedling monitoring in their protocol change this code.
#'Additionally, the function addresses issues with missing microplot size values,
#'identifies cases where a microplot size of 0 implies non-collection (recorded in comments),
#'and ensures that all blank seedling statuses represent either no seedlings found or non-collection.
#'Finally, the function checks that species entered in the seedling tree tab are
#'labeled either "Tree" or "Shrub" in the preferred lifeform column, flagging any discrepancies.
#' @param seeds
#'
#' @return A list of flags or data issues in the seedlings csv
#' @export
#'
#' @examples
#' seedlings_qc(seeds)
seedlings_qc=function(seeds){

  htcl <- setdiff(unique(na.omit(seeds$SizeClHt)), c(0, 0.15, 0.3, 0.6, 1, 2, 3, 4, 5, 6, 7, 8, 9,10))


  cat("Correct height classes entered for seedling tree data...\n")
  if (length(htcl) == 0) {
    cat("TRUE\n")
    cat("\n")
  } else {
    cat(paste(c("FALSE value/s of", htcl, "entered which do not fit with acceptable values of 0.15-10, problem events are listed in flags", sep = " "), collapse = " "), "\n")
    cat("\n")
    flags<- c(flags, paste("Value/s of", htcl, "entered in height class for seedling tree data which do not fit with acceptable values of 0-10, problem problem events are", seeds[which(seeds$SizeClHt %in% htcl), "MacroPlot.Name"],seeds[which(seeds$SizeClHt %in% htcl), "Monitoring.Status"], sep = " "), collapse = " ")
  }


  above4=which(seeds$SizeClHt>4 | seeds$SizeClHt %in% c(NULL, NA, " ") & !is.na(seeds$Index))
  cat("Flagging heights above 4 and NULL height for seedling tree data...\n")
  if (length(above4) == 0) {
    cat("No heights above 4 or null values\n")
    cat("\n")
  } else {
    cat(paste(c(length(above4), "rows flagged for heights which are above 4 or NULL, problem events are listed in flags list", sep = " "), collapse = " "), "\n")
    cat("\n")
    flags<- c(flags, paste("Index numbers ", seeds[above4, "Index"], "flagged for seedling heights which are above 4 or NULL:",
                           seeds[above4, "SizeClHt"], "in problem event",
                           seeds[above4, "MacroPlot.Name"], seeds[above4, "Monitoring.Status"],
                           "comments say", seeds[above4, "SaComment"], sep = " "), collapse = " ")
  }


  cat("All samples with count value 0 have the species symbol TREE1...")
  # Count values of 0 with species symbol other than TREE1
  zero_counts <- which(seeds$Count == 0)
  species_with_zero_counts <- unique(seeds[zero_counts, "Species.Symbol"])
  if (length(species_with_zero_counts)==1) {
    if(species_with_zero_counts == "TREE1"){
      cat("TRUE No issues with species code when count is 0\n")
      cat("\n")
    }else{
      cat("FALSE Issues with species code when count is 0:", species_with_zero_counts, "listed for species code which should be TREE1, problem rows listed in flags\n")
      cat("\n")
      problem_rows <- which(seeds$Species.Symbol %in% species_with_zero_counts & seeds$Count == 0)
      flags<- c(flags, paste(c("Issues with species code when count is 0:", species_with_zero_counts,
                               "listed for species code which should be TREE1, problem rows are",
                               problem_rows), collapse = " "))
    }

  }else if(length(species_with_zero_counts)==0){
    cat("No samples with count 0\n")
    cat("\n")
  }else {
    cat("FALSE Issues with species code when count is 0:", species_with_zero_counts, "listed for species code which should be TREE1, problem rows listed in flags\n")
    cat("\n")
    problem_rows <- which(seeds$Species.Symbol %in% species_with_zero_counts & seeds$Count == 0)
    flags<- c(flags, paste(c("Issues with species code when count is 0:", species_with_zero_counts,
                             "listed for species code which should be TREE1, problem rows are",
                             problem_rows), collapse = " "))
  }
  # Flagging outliers
  test <- suppressWarnings({
    cat("Checking for extremely high or low values in seedling tree count...\n")
    rosnerTest(seeds$Count)
  })
  outliers <- test$all.stats[which(test$all.stats$Outlier), "Value"]

  if (length(outliers) == 0) {
    cat("No outlier values detected.\n")
    cat("\n")
  } else {
    outlier_rows <- which(seeds$Count %in% outliers)
    max_count <- max(na.omit(seeds$Count))
    min_count <- min(na.omit(seeds$Count))
    mean_count <- mean(na.omit(seeds$Count))

    outlier_msg <- paste("Outlier values detected according to Rosner test. They are",
                         paste(outliers, collapse = ", "),
                         "with events", paste(as.character(seeds[outlier_rows,"MacroPlot.Name"]),paste(as.character(seeds[outlier_rows,"Monitoring.Status"])), collapse = ", "),
                         "For reference, max, min, and mean of seed counts are", max_count, min_count, mean_count, sep = " ")

    cat(outlier_msg, "\n")
    cat("\n")
    flags <- c(flags, outlier_msg)

  }




  #start of fraction stuff

  #if species symbol is "JUNI1","QUAR1","QUEM1","QUER1","QUGA1","QUHY1","QUOB1","QURU1"
  #subfrac * microplot should equal 0.005 - CHANGING THIS PART
  #if it doesn't - flag
  #if species symbol does not match these codes
  #subfrac should equal 1
  #if it doesn't flag

  cat("Are there any issues with the subplot fractions\n")

  if(any(seeds$Species.Symbol %in% c("JUNI1","QUAR1","QUEM1","QUER1","QUGA1","QUHY1","QUOB1","QURU1"))){

    difspecies=seeds[which(seeds$Species.Symbol %in% c("JUNI1","QUAR1","QUEM1","QUER1","QUGA1","QUHY1","QUOB1","QURU1")),]
    incorrect_subfrac <- which(difspecies$SubFrac*difspecies$MicroPlotSize!=0.025)

    if(length(incorrect_subfrac>0)){
      cat("Incorrect subplot fraction/microplot size values detected:",
          difspecies[incorrect_subfrac, "SubFrac"] * difspecies[incorrect_subfrac, "MicroPlotSize"],
          "recorded in sample events",
          paste(difspecies[incorrect_subfrac, "MacroPlot.Name"], difspecies[incorrect_subfrac, "Monitoring.Status"], sep = " - "),
          "when product should be 0.025\n")
      cat("\n")

      flags<- c(flags, paste(c("Incorrect subplot fraction/microplot size values detected:",
                               difspecies[incorrect_subfrac, "SubFrac"] * difspecies[incorrect_subfrac, "MicroPlotSize"],
                               "recorded in sample events",
                               paste(difspecies[incorrect_subfrac, "MacroPlot.Name"], difspecies[incorrect_subfrac, "Monitoring.Status"], sep = " - "),
                               "when product should be 0.025\n"), collapse=" "))


    }

    nondifspecies <- seeds %>% anti_join(difspecies, by="MacroPlot.Name")


    non_one_subfrac <- unique(nondifspecies[which(nondifspecies$SubFrac!=1), "SubFrac"])

    if (length(non_one_subfrac) == 0) {
      cat("No issues with SubFrac values.\n")
      cat("\n")
    } else {
      cat("Issues with SubFrac values detected. Values that are not one:", non_one_subfrac,", problem sample events are:", paste(nondifspecies[which(nondifspecies$SubFrac!=1),"MacroPlot.Name"]),paste(nondifspecies[which(nondifspecies$SubFrac!=1),"Monitoring.Status"]), "\n")
      cat("\n")
      flags<-c(flags, paste(c("Issues with SubFrac values detected. Values that are not one:", non_one_subfrac,", problem sample events are:", paste(nondifspecies[which(nondifspecies$SubFrac==1),"MacroPlot.Name"]),paste(nondifspecies[which(nondifspecies$SubFrac==1),"Monitoring.Status"]), "\n"), collapse=" "))
    }

  }else{

    non_one_subfrac <- unique(seeds[which(seeds$SubFrac!=1), "SubFrac"])

    if (length(non_one_subfrac) == 0) {
      cat("No issues with SubFrac values.\n")
      cat("\n")
    } else {
      cat("Issues with SubFrac values detected. Values that are not one:", non_one_subfrac,", problem sample events are:", paste(seeds[which(seeds$SubFrac!=1),"MacroPlot.Name"]),paste(seeds[which(seeds$SubFrac!=1),"Monitoring.Status"]), "\n")
      cat("\n")
      flags<-c(flags, paste(c("Issues with SubFrac values detected. Values that are not one:", non_one_subfrac,", problem sample events are:", paste(seeds[which(seeds$SubFrac==1),"MacroPlot.Name"]),paste(seeds[which(seeds$SubFrac==1),"Monitoring.Status"]), "\n"), collapse=" "))
    }
  }





  #MACROPLOT STUFF START HERE
  # Checking for NAs in the microplot column for seedings
  cat("Checking for NAs in the microplot column for seedings...\n")

  # Check if there are any NAs in the MicroPlotSize column
  if (length(which(is.na(seeds$MicroPlotSize))) != 0) {
    cat("\n")
    cat("Replacing NAs with microplot size from a row with the same event and macroplot\n")
    cat("\n")

    # Identify unique events and macroplots with missing MicroPlotSize values
    events = unique(seeds[which(is.na(seeds$MicroPlotSize)), c("MacroPlot.Name", "Monitoring.Status")])

    # Iterate through each unique event-macroplot combination
    for (i in 1:nrow(events)) {
      # Find rows with the same event and macroplot as the current combination
      samp = which(events[i, "MacroPlot.Name"] == seeds[, "MacroPlot.Name"] & events[i, "Monitoring.Status"] == seeds[, "Monitoring.Status"])

      # Replace missing MicroPlotSize values with values from non-missing rows within the same combination
      seeds[samp[which(is.na(seeds[samp, "MicroPlotSize"]))], "MicroPlotSize"] = seeds[which(!is.na(seeds[samp, "MicroPlotSize"])), "MicroPlotSize"]
    }

    cat("Are there any NAs remaining after replacement?\n")
    if (length(which(is.na(seeds$MicroPlotSize))) != 0) {
      cat("Yes, there are still NAs - flagging the problem rows\n")
      cat("\n")
      flags<- c(flags, paste("NAs present in microplot column for seedings in tree seedlings protocol data. Problem rows are at indices:", which(is.na(seeds$MicroPlotSize))), sep = " ")
    } else {
      cat("No, all NAs have been resolved\n")
      cat("\n")
    }
  } else {
    cat("FALSE, no NAs found in the MicroPlotSize column\n")
    cat("\n")
  }

  # Print a message indicating that samples with microplot size of 0 were not collected and are marked as "not collected" in comments.
  cat("All the samples where microplot is 0 were when seedlings were not collected, this is recorded in comments as not collected...\n")

  # Find the rows where microplot size is 0 and the comment is not "not collected".
  zero = setdiff(seeds[which(seeds$MicroPlotSize == 0), "SaComment"], c("not collected", "NOT COLLECTED", "Not Collected"))

  # Check if there are no rows with microplot size 0 and no "not collected" comment.
  if (length(zero) == 0) {
    cat("TRUE\n")
    cat("\n")
  } else {
    cat("FALSE, some samples have microplot recorded as 0 but no comment explanation, problem events noted in flags\n")
    cat("\n")
    flags<- c(flags, paste("Some samples in seedling data have microplot recorded as 0 but no comment explanation, problem sample events are the following", seeds[which(seeds$MicroPlotSize==0 & seeds$SaComment %in% zero),"MacroPlot.Name"],seeds[which(seeds$MicroPlotSize==0 & seeds$SaComment %in% zero),"Monitoring.Status"]))
  }

  # mtype is monitoring type
  if(mtype %in% c("PSME", "QUAR09", "QUAR06")){
    # This microplot size should be 0.025 but might be 0.005 historically which is ok - but should flag
    wrong_mp_size=which(!na.omit(unique(seeds$MicroPlotSize)) %in% c(0.000,0.005,0.025))
    cat("Microplot size for this monitoring type is either 0.025, 0.005, or 0.005\n")
    if(length(wrong_mp_size)>0){
      # Something's wrong
      cat("Incorrect microplot size values detected:", seeds$MicroPlotSize[wrong_mp_size],", sample events listed in flags\n")
      cat("\n")
      flags<- c(flags, paste(c("Incorrect microplot size values detected:", seeds$MicroPlotSize[wrong_mp_size],", problem sample events:", seeds$MacroPlot.Name[wrong_mp_size], seeds$Monitoring.Status[wrong_mp_size]), collapse = " "))
    } else {
      # Everything's fine
      cat("TRUE\n")
      cat("\n")
    }
  } else if(mtype %in% c("PILE", "PIPO", "QUEM")){
    cat("Microplot size for this monitoring type is either 0.005 or 0.005\n")
    wrong_mp_size=which(!as.numeric(seeds$MicroPlotSize) %in% c(NA, 0.000,0.005))
    # This should be 0.005 microplot size
    if(length(wrong_mp_size)>0){
      # Something's wrong
      cat("Incorrect microplot size values detected:", unique(seeds$MicroPlotSize[wrong_mp_size]),", sample events listed in flags\n")
      cat("\n")
      flags<- c(flags, paste(c("Incorrect microplot size values detected:", unique(seeds$MicroPlotSize[wrong_mp_size]),", problem sample events:", seeds$MacroPlot.Name[wrong_mp_size], seeds$Monitoring.Status[wrong_mp_size]), collapse = " "))
    } else {
      # Everything's fine
      cat("TRUE\n")
      cat("\n")
    }
  } else {
    # Monitoring type code is not recognized
    cat("Monitoring type code not recognized so cannot run microplot size checks\n")
    cat("\n")
    flags<- c(flags, "Monitoring type code not recognized so cannot run microplot size checks\n")
  }

  # Check that seedling trees have valid entries for status
  cat("All seedlings are marked L for living\n")

  if (length(setdiff(unique(seeds$Status), c("", "L"))) != 0) {
    cat("FALSE, some rows have values", paste(setdiff(unique(seeds$Status), c("", "L")), collapse = " "), ", problem events noted in flags\n")
    cat("\n")
    # Update flags with information about problem rows
    flags<- c(flags, paste(c("Some rows in seedling tree database have values", setdiff(unique(seeds$Status), c("", "L")),
                             "which are not blank or L, sample events are", paste(seeds[which(seeds$Status %in% setdiff(unique(seeds$Status), c("", "L"))),
                                                                                        "MacroPlot.Name"],
                                                                                  seeds[which(seeds$Status %in% setdiff(unique(seeds$Status), c("", "L"))),
                                                                                        "Monitoring.Status"], " , ")), collapse = " "))
  } else {
    cat("TRUE\n")
    cat("\n")
  }

  cat("Are all blank seedling statuses really no seedlings found or not collected?\n")

  if (length(which(!is.na(seeds[which(seeds$Status == ""), "Count"]))) != 0) {
    cat("Some samples are missing L status, rows:", which(!is.na(seeds[which(seeds$Status == ""), "Count"])), ", sample events noted in flags\n")
    cat("\n")
    # Update flags with information about missing L status samples with counts
    flags<- c(flags, paste("Some samples in seedling data set are missing L status, sample events:",
                           seeds[which(!is.na(seeds[which(seeds$Status == ""), "Count"])), "MacroPlot.Name"],
                           seeds[which(!is.na(seeds[which(seeds$Status == ""), "Count"])), "Monitoring.Status"],
                           "have counts of", seeds[which(!is.na(seeds[which(seeds$Status == ""), "Count"])), "Count"], collapse = " "))
  } else {
    cat("TRUE, all blank seedling statuses have 0 counts for seedlings\n")
    cat("\n")
  }


  # Check that species entered in the seedling tree tab are trees
  # Check that species entered in the seedling tree tab are trees
  cat("The species entered in seedling tree tab are labeled Tree in preferred lifeform\n")

  selected_rows <- which(seeds$Count != 0 | !is.na(seeds$Count) | seeds$Count != "")
  unique_pref_lifeforms <- unique(seeds[selected_rows, "Preferred_Lifeform"])
  problem_rows <- which(seeds$Preferred_Lifeform %in% setdiff(unique_pref_lifeforms,c("Tree")))


  if (all(unique_pref_lifeforms %in% c("Tree"))) {
    cat("TRUE\n")
    cat("\n")
  } else {
    cat("FALSE, some are labeled", setdiff(unique_pref_lifeforms,c("Tree")), ", problem events in flags\n")
    cat("\n")
    flags<- c(flags, paste(c("Some preferred lifeforms in the seedlings data have the mislabel",
                             setdiff(unique_pref_lifeforms,c("Tree")), "in events", paste(seeds[problem_rows, "MacroPlot.Name"],seeds[problem_rows, "Monitoring.Status"], " , ")), collapse = " "))
  }

  #dead seedling check
  cat("All seedlings are alive?\n")
  if(all(seeds$Status=="L")){
    cat("TRUE\n")
  }else{
    cat("FALSE, problem sample events listed in flags")
    flags <- c(flags, paste("Dead seedlings recorded in sample events:", seeds[which(seeds$Status=="D"), "MacroPlot.Name"],seeds[which(seeds$Status=="D"), "Monitoring.Status"]))
  }





  return(flags)
} #end function

#####Post Burn Severity protocol:
#' Post Burn Severity Substrate Checks
#' @description
#' This function checks for incorrect values for NumTran, TranLen, NumPtsTran, substrate and vegetation values
#' in the post burn severity data frame. Values may need to be edited according to park specific protocol. The
#' default values are used in Saguaro's monitoring program. The function also checks that there are
#' 4 transects for each macroplot with 10 sample points per transect. Any errors are flagged with information
#' about the problem sample event and macroplot.
#'
#' @param pbsev
#'
#' @return A list of flags or data issues with post burn severity substrate data in the post burn severity csv
#' @export
#'
#' @examples
#' pbsev_qc(tree)
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


return(flags)
}

#####Trees - Individuals protocol:

##Tree CBH and Tree CBH Range

#' Tree CBH and Tree CBH Range
#' @description
#' The tree_CBH_qc function performs quality control checks on the 'tree' dataset.
#' It ensures that dead trees have blank live crown base height values and flags any
#' discrepancies. For living trees, it checks for unreasonable live crown base height
#' values using the Rosner test and flags outliers. The function also verifies the
#' relationship between ladder base height (LaddBaseHt) and ladder max height (LaddMaxHt),
#' ensuring the former is always less than the latter. It checks that entries with
#' LaddBaseHt values have the correct species symbol ('CANOPY'), status ('L'), SubFrac
#'  (1000), and TagNo (0), flagging any inconsistencies and providing detailed information
#'   about problematic rows. Note that the use of canopy data in the tree dataset is
#'   specific to the Saguaro Monitoring program.
#'
#' @param tree
#'
#' @return A list of flags or data issues with CBH in the tree csv
#' @export
#'
#' @examples
#' tree_CBH_qc(tree)
tree_CBH_qc=function(tree){
  #check that dead trees do not have live crown base height values

  cat("All dead trees have a blank live crown base height...\n")
  if(length(which(!is.na(tree[which(tree$Status=="D"), "LiCrBHt"])))!=0){
    cat("FALSE, problem events listed in flags\n")
    cat("\n")
    flags<-c(flags, paste("Dead trees", tree[which(!is.na(tree[which(tree$Status=="D"), "LiCrBHt"])), "TagNo"], "have a non blank live crown base height in sample events", tree[which(!is.na(tree[which(tree$Status=="D"), "LiCrBHt"])), "MacroPlot.Name"],tree[which(!is.na(tree[which(tree$Status=="D"), "LiCrBHt"])), "Monitoring.Status"]))
  }else{
    cat("TRUE\n")
    cat("\n")
  }

  #check that living trees have reasonable live crown base height values
  if(length(unique(tree$LiCrBHt))>4){

    cat("Do living trees have any unreasonably high or low live crown base height values?\n")


    test<-suppressWarnings(rosnerTest(tree$LiCrBHt))
    test=test$all.stats
    outliers=test[which(test$Outlier==TRUE),4]

    if(length(outliers)==0){
      cat("No\n")
      cat("\n")
    }else{
      cat(paste(c("Yes, the outlier values according to a rosner test are", outliers, ". They are in rows", which(tree$LiCrBHt %in% outliers), "of the tree data table. The max, min, and mean of tree LiCrBHts are", max(na.omit(tree$LiCrBHt)), min(na.omit(tree$LiCrBHt)), mean(na.omit(tree$LiCrBHt)), "respectively\n"), collapse=" "))
      cat("\n")
      flags<-c(flags,paste(c("The tree LiCrBHt in the tree data set has outlier values according to a rosner test, which are", outliers, ". They are in rows", which(tree$LiCrBHt %in% outliers), "of the tree data table. The max, min, and mean of tree LiCrBHts are", max(na.omit(tree$LiCrBHt)), min(na.omit(tree$LiCrBHt)), mean(na.omit(tree$LiCrBHt)), "respectively"), collapse=" ") )
    }

  }else{
    cat(paste(c("Not enough samples to conduct a rosner test (<4) for the outlier values for live crown base height in the tree dataset. Values are ", na.omit(tree$LiCrBHt),"\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste(c("Not enough samples to conduct a rosner test (<4) for the outlier values for live crown base height in the tree dataset. Values are ", na.omit(tree$LiCrBHt)), collapse=" "))

  }
  #check that living trees have reasonable range (min to max) of live crown base height values and that the range of crown base height values is entered correctly

  cat("Are there values for live crown base height in this dataset?\n")
  if(length(which(!is.na(tree$LaddBaseHt)))!=0){
    cat(paste("Yes, there are ", length(which(!is.na(tree$LaddBaseHt)))), "\n")
    cat("\n")
    cbh=tree[which(!is.na(tree$LaddBaseHt)),]
    #check that laddbaseht values are less than laddmaxht
    cat("Are ladder base ht values all less than ladder max ht values?\n")
    if(length(unique(cbh$LaddBaseHt<cbh$LaddMaxHt))==1){
      if(unique(cbh$LaddBaseHt<cbh$LaddMaxHt)=="TRUE"){
        cat("TRUE\n")
        cat("\n")
      }else{
        cat("Ladder Max Ht values are always higher than ladder base ht values \n")
        cat("\n")
        flags<-c(flags,"Ladder Max Ht values are always higher than ladder base ht values" )
      }

    }else{
      cat(paste("FALSE, values for ladder max height are heigher than base height in these rows:", cbh[which(cbh$LaddBaseHt>cbh$LaddMaxHt),], "\n"))
      cat("\n")
      flags<-c(flags, paste("In tree data set values for ladder max height are heigher than base height in these rows:", cbh[which(cbh$LaddBaseHt>cbh$LaddMaxHt),]))
    }

    #check that species symbol =CANOPY for all entries with ladd base ht values

    cat("Species symbol is canopy for all entries with ladd base ht values?\n")
    if(length(unique(cbh$Species.Symbol=="CANOPY"))==1){
      if(unique(cbh$Species.Symbol=="CANOPY")=="TRUE"){
        cat("TRUE\n")
        cat("\n")
      }else{
        cat("FALSE, species symbol is not canopy for ANY entries with ladd base ht values\n")
        cat("\n")
        flags<-c(flags,"species symbol is not canopy for any entries with ladd base ht values in tree data set" )
      }

    }else{
      cat(paste("FALSE, species symbol is not canopy for entries with ladd base ht values in these rows:", cbh[which(cbh$Species.Symbol!="CANOPY"),], "\n"))
      cat("\n")
      flags<-c(flags, paste("In tree data set species symbol is not canopy for entries with ladd base ht values in these rows:", cbh[which(cbh$Species.Symbol!="CANOPY"),]))
    }


    #status is L for all entries with laddbase ht values
    cat("Status is L for all entries with laddbase ht values ?\n")
    if(length(unique(cbh$Status))==1){
      if(unique(cbh$Status=="L")=="TRUE"){
        cat("TRUE \n")
        cat("\n")
      }else{
        cat("FALSE, status is not L for ANY entries with laddbase ht values\n ")
        cat("\n")
        flags<-c(flags,"status is not L for ANY entries with laddbase ht values in tree dataset" )
      }

    }else{
      cat("FALSE, status is not L for some entries with laddbase ht values in events listed in flags")
      cat("\n")
      flags<-c(flags, paste("In tree data set status is", cbh[which(cbh$Status=="D"),"Status"], "not L for tag", cbh[which(cbh$Status=="D"),"TagNo"],  "with laddbase ht value", cbh[which(cbh$Status=="D"),"LaddBaseHt"], "in these sample events:", cbh[which(cbh$Status=="D"),"MacroPlot.Name"], cbh[which(cbh$Status=="D"),"Monitoring.Status"]))
    }


    #subfrac is 1000 for all entries with laddbase ht values
    cat("SubFrac is 1000 for all entries with laddbase ht values ?\n")
    if(length(unique(cbh$SubFrac==1000))==1){
      if(unique(cbh$SubFrac==1000)=="TRUE"){
        cat("TRUE\n")
        cat("\n")
      }else{
        cat("FALSE, SubFrac is not 1000 for ANY entries with laddbase ht values\n ")
        cat("\n")
        flags<-c(flags,"SubFrac is not 1000 for ANY entries with laddbase ht values in tree dataset" )
      }

    }else{
      cat(paste("FALSE, SubFrac is not 1000 for entries with laddbase ht values in these rows:", cbh[which(cbh$SubFrac!=1000),], "\n"))
      cat("\n")
      flags<-c(flags, paste("In tree data set SubFrac is not 1000 for entries with laddbase ht values in these rows:", cbh[which(cbh$SubFrac!=1000),]))
    }



    #Tag is 0 for all entries with laddbase ht values
    cat("Tag is 0 for all entries with laddbase ht values ?\n")
    if(length(unique(cbh$TagNo==0))==1){
      if(unique(cbh$TagNo==0)=="TRUE"){
        cat("TRUE\n")
        cat("\n")
      }else{
        cat("FALSE, Tag is not 0 for ANY entries with laddbase ht values\n ")
        flags<-c(flags,"Tag is not 0 for ANY entries with laddbase ht values in tree dataset" )
      }

    }else{
      cat(paste("FALSE, Tag is not 0 for entries with laddbase ht values in these rows:", cbh[which(cbh$TagNo!=0),], "\n"))
      cat("\n")
      flags<-c(flags, paste("In tree data set Tag is not 0 for entries with laddbase ht values in these rows:", cbh[which(cbh$TagNo!=0),]))
    }

  }else{
    cat("No\n")
    cat("\n")
  }
  return(flags)
} #end function #verified 9/12 by Eva

##Crown class
#' Crown Class
#' @description
#' The tree_crown_class_qc function classifies trees based on their diameter at
#' breast height (DBH) and validates the correctness of crown class values for
#' different tree statuses. It ensures that for trees with a blank or dead status
#' and blank DBH, the crown class is either "BBD," "CUS," "DD," or blank. In the
#' case of live trees with blank DBH, it verifies that the crown class is blank.
#' For overstory trees with a dead status, the function checks for acceptable crown
#' class values such as "BAD," "CS," "LBS," "RS," or blank. Similarly, for live
#' overstory trees, it ensures crown class values are within categories
#' "C," "D," "I," "O," "SC," or blank. Additionally, for pole trees with a dead status,
#' the function checks for valid crown class values like "BBD," "CUS," "DD," "X," or blank.
#' The function flags instances where crown class values deviate from these specified
#' categories, providing quality control for tree data.
#'
#' @param tree
#'
#' @return A list of flags or data issues with crown class in the tree csv
#' @export
#'
#' @examples
#' tree_crown_class_qc(tree)
tree_crown_class_qc=function(tree){
  #check for invalid values entered for tree crown class


  #classify dbh
  pole=tree[which(tree$DBH<15.1),]
  pole$treerow=which(tree$DBH<15.1)
  overstory=tree[which(tree$DBH>=15.1),]
  overstory$treerow=which(tree$DBH>=15.1)
  blank=tree[which(tree$DBH=="" | is.na(tree$DBH)),]
  blank$treerow=which(tree$DBH=="" | is.na(tree$DBH))


  #checking that dbh blank status D trees have crown class bbd, cus, dd, or blank
  Dead_CC=setdiff(blank[which(blank$Status=="D"),"CrwnCl"], c("BBD", "CUS", "DD", ""))

  cat("Dbh blank status D trees have crown class bbd, cus, dd, or blank...\n")
  if(length(Dead_CC)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    blank_D=blank[which(blank$Status=="D"),]
    cat(paste(c("FALSE, values of", Dead_CC, "recorded for crown class, sample events in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Tag number ", blank_D[which(blank_D$CrwnCl%in% Dead_CC),"TagNo"],
                          " in tree data set has", unique(Dead_CC),
                          "recorded for crown class in sample events",
                          blank_D[which(blank_D$CrwnCl%in% Dead_CC),"MacroPlot.Name"],
                          blank_D[which(blank_D$CrwnCl%in% Dead_CC),"Monitoring.Status"],
                          "which is not included in the acceptable list of BBD, CUS, DD, or blank for trees that have no dbh"))
  }

  #checking that dbh blank status L trees have crown class blank
  Live_B=setdiff(blank[which(blank$Status=="L"),"CrwnCl"], c(""))
  cat("Dbh blank status L trees have crown class blank\n")
  if(length(Live_B)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    blank_L=blank[which(blank$Status=="L"),]
    cat(paste(c("FALSE, values of", Live_B, "recorded for crown class, sample events recorded in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Tag number ", blank_L[which(blank_L$CrwnCl%in%Live_B), "TagNo"],
                          " in tree data set has", unique(Live_B), "recorded for crown class in sample events",
                          blank_L[which(blank_L$CrwnCl%in%Live_B), "MacroPlot.Name"],
                          blank_L[which(blank_L$CrwnCl%in%Live_B), "Monitoring.Status"],
                          "which is not blank, it may mean that a live tree is missing a dbh"))
  }

  #checking that dbh overstory status D trees have crown class BAD, CS, LBS, RS, or blank

  Dead_O=setdiff(overstory[which(overstory$Status=="D"),"CrwnCl"], c("BAD", "CS", "LBS", "RS", ""))
  cat("Dbh overstory status D trees have crown class BAD, CS, LBS, RS, or blank\n")
  if(length(Dead_O)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    overstory_D=overstory[which(overstory$Status=="D"),]
    cat(paste(c("FALSE, values of", Dead_O, "recorded for crown class, sample events recorded in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Tag number ", overstory_D[which(overstory_D$CrwnCl %in% Dead_O),"TagNo"],
                          " in tree data set has ", unique(Dead_O),
                          "recorded for crown class in sample events",
                          overstory_D[which(overstory_D$CrwnCl %in% Dead_O),"MacroPlot.Name"],
                          overstory_D[which(overstory_D$CrwnCl %in% Dead_O),"Monitoring.Status"],
                          "which is not included in the acceptable list of BAD, CS, LBS, RS, or blank for overstory dead trees"))
  }


  #checking that dbh overstory status L trees have crown class C, D, I, O, SC, or blank
  Live_O=setdiff(overstory[which(overstory$Status=="L"),"CrwnCl"], c("C", "D", "I", "O", "SC", ""))
  cat("Dbh overstory status L trees have crown class C, D, I, O, SC, or blank\n")
  if(length(Live_O)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    overstory_L=overstory[which(overstory$Status=="L"),]
    cat(paste(c("FALSE, values of", Live_O, "recorded for crown class in events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Tag number ", overstory_L[which(overstory_L$CrwnCl %in% Live_O), "TagNo"],
                          " in tree data set has", unique(Live_O), "recorded for crown class in events",
                          overstory_L[which(overstory_L$CrwnCl %in% Live_O), "MacroPlot.Name"],
                          overstory_L[which(overstory_L$CrwnCl %in% Live_O), "Monitoring.Status"],
                          "which is not included in the acceptable list of C, D, I, O, SC, or blank for overstory live trees"))
  }

  #checking pole trees have crown class X, or blank
  wrong_cc=setdiff(pole[,"CrwnCl"], c("X",  ""))
  cat("Dbh pole trees have crown class X, or blank\n")
  if(length(wrong_cc)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    cat(paste(c("FALSE, values of", wrong_cc, "recorded for crown class in sample events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Tag number ", pole[which(pole$CrwnCl %in% wrong_cc),"TagNo"],
                          " in tree data set has", unique(wrong_cc), "recorded for crown class in sample events",
                          pole[which(pole$CrwnCl %in% wrong_cc),"MacroPlot.Name"],
                          pole[which(pole$CrwnCl %in% wrong_cc),"Monitoring.Status"],
                          "which is not included in the acceptable list of X, or blank for pole trees"))

  }
  return(flags)
} #end function #verified 9/12 by Eva (lots of flags)

#Tree damage
#' Tree damage
#' @description
#' The `tree_damage_qc` function validates the accuracy of tree damage codes in a dataset.
#' It checks if the codes provided (DamCd1, DamCd2, DamCd3, DamCd4, DamCd5) are within the
#' predefined acceptable list of damage codes. The function returns "TRUE" if all codes
#' are correct and included in the acceptable list, and "FALSE" with details of any
#' discrepancies found. The acceptable list includes codes such as "ABGR," "BIRD," "BLIG,"
#' and others. If discrepancies are detected, the function adds corresponding entries
#' to the quality control flags, specifying the MacroPlot.Name and Monitoring.Status
#' for the rows where the issues occur.
#'
#' @param tree
#'
#' @return A list of flags or data issues with damage class in the tree csv
#' @export
#'
#' @examples
#' tree_damage_qc(tree)
tree_damage_qc=function(tree){

  #check that tree damage codes are entered correctly
  cat("Tree damage codes are all entered correctly...\n")

  damcodes=c("ABGR", "BIRD", "BLIG", "BROK", "BROM", "BURL", "CONK", "CROK", "DTOP", "EPIC", "EPIP", "FIRE", "FORK", "FRST", "GALL", "HOLW", "INSE", "LEAN", "LICH", "LIGT", "MAMM", "MISL", "MOSS", "OZON", "ROOT", "ROTT", "SNAG", "SPAR", "SPRT", "TWIN", "UMAN", "WOND", "")

  if(length(setdiff(na.omit(unique(str_trim(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )))),damcodes))%in%0){
    cat("TRUE\n")
  }else{
    cat(paste("FALSE" , setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes), "included in events listed in flags\n"), sep=" ")
    cat("\n")
    cat("Acceptable list includes:", damcodes, "\n")
    flags<- c(flags, paste(setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes), "-included in row",
                           tree[which(tree[,"DamCd1"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes)|
                                        tree[,"DamCd2"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes) |
                                        tree[,"DamCd3"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes) |
                                        tree[,"DamCd4"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes) |
                                        tree[,"DamCd5"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes)),
                                "MacroPlot.Name"],
                           tree[which(tree[,"DamCd1"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes)|
                                        tree[,"DamCd2"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes) |
                                        tree[,"DamCd3"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes) |
                                        tree[,"DamCd4"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes) |
                                        tree[,"DamCd5"]%in%setdiff(unique(c(tree$DamCd1, tree$DamCd2,tree$DamCd3, tree$DamCd4, tree$DamCd5 )),damcodes)),
                                "Monitoring.Status"],
                           "of tree data set for damage code which is not included in acceptable list", sep=" "))
  }
  return(flags)

} #end function #verified 9/12 by Eva


###Tree DBH
#' Tree DBH
#' @description
#' The `tree_dbh_qc` function conducts quality control on the Diameter at Breast Height
#' (DBH) values in a tree dataset. It first checks if all DBH values are under 2.5;
#' if any exceed this limit, it flags the events with the specific DBH values,
#' MacroPlot.Name, and Monitoring.Status. Subsequently, the function performs a
#' check for outlier values using a rosner test on the DBH values. If outliers
#' are detected, it adds entries to the quality control flags with details on the
#' outlier values, the corresponding events (MacroPlot.Name and Monitoring.Status),
#' and statistics on the maximum, minimum, and mean of the DBH values in the dataset.
#'
#'
#' @param tree
#'
#' @return A list of flags or data issues with DBH in the tree csv
#' @export
#'
#' @examples
#' tree_dbh_qc(tree)
tree_dbh_qc=function(tree){


  #checking for DBH under 2.5
  cat("All tree DBHs are over 2.5\n")

  if(any(unique(na.omit((tree$DBH)<2.5)))==TRUE){
    cat(paste(c("Not all DBH are over 2.5 in tree data - ", tree[which(tree$DBH<2.5), "DBH"], "recorded in event/s listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Not all DBH are over 2.5 in tree data- ", tree[which(tree$DBH<2.5), "DBH"], "recorded in event/s",
                          tree[which(tree$DBH<2.5), "MacroPlot.Name"],
                          tree[which(tree$DBH<2.5), "Monitoring.Status"], sep=" "))
  }else{
    cat("TRUE\n")
    cat("\n")
  }

  cat("Are there any outlier values in tree DBH?\n")


  test<-suppressWarnings(rosnerTest(tree$DBH))
  test=test$all.stats
  outliers=test[which(test$Outlier==TRUE),4]

  if(length(outliers)==0){
    cat("No\n")
    cat("\n")
  }else{
    cat(paste(c("Yes, the outlier values in the tree dbh according to a rosner test are", outliers, ". They are in sample events listed in flags of the tree data table. For reference, the max, min, and mean of tree DBH are", max(na.omit(tree$DBH)), min(na.omit(tree$DBH)), mean(na.omit(tree$DBH)), "respectively\n"), collapse=" "))
    cat("\n")
    flags<-c(flags,paste("The tree DBH in the tree data set has outlier values according to a rosner test, which are", outliers, ". They are in events",
                         tree[which(tree$DBH %in% outliers), "MacroPlot.Name"],
                         tree[which(tree$DBH %in% outliers), "Monitoring.Status"],
                         "of the duff data table. For reference, the max, min, and mean of tree DBH's are", max(na.omit(tree$DBH)), min(na.omit(tree$DBH)), mean(na.omit(tree$DBH)), "respectively", collapse=" ") )
  }
  return(flags)
} #end function #verified 9/12 by Eva

##tree dd
#' Tree dead and down
#' @description
#' The `tree_dd_qc` function focuses on quality control for fallen trees or trees that
#' are too short to be counted as individuals in a dataset. It identifies fallen trees
#' based on specific crown classes (BBD, CUS, DD) and creates a subset named `fallen_trees`.
#' The function then checks if values for Diameter at Breast Height (DBH), tree height
#' (Ht), and live crown base height (LiCrBHt) have been deleted for trees that are no
#' longer counted. For each parameter, it reports "TRUE" if the values have been deleted as
#' expected. If any values are still present, it flags the events with details on the
#' specific parameter values, crown class, and associated events (MacroPlot.Name and
#' Monitoring.Status). The quality control results are stored in the `flags` variable
#' and returned by the function.
#'
#' @param tree
#'
#' @return A list of flags or data issues with dead and down trees in the tree csv
#' @export
#'
#' @examples
#' tree_dd_qc(tree)
tree_dd_qc=function(tree){

  fallen_trees=tree[which(tree$CrwnCl=="BBD" | tree$CrwnCl=="CUS" | tree$CrwnCl=="DD"),]

  fallen_trees$treerow=which(tree$CrwnCl=="BBD" | tree$CrwnCl=="CUS" | tree$CrwnCl=="DD")

  cat("Values for DBH have been deleted for trees that are no longer standing...\n")
  if(any(is.na(fallen_trees$DBH)==FALSE)){
    cat(paste(c("FALSE, values of", fallen_trees[which(!is.na(fallen_trees$DBH)),10], "are recorded for tree DBH that have crown class", unique(fallen_trees[which(!is.na(fallen_trees$DBH)),13]), ",problem events are listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Values of", fallen_trees[which(!is.na(fallen_trees$DBH)),10], "are recorded for tree DBH that have crown class", fallen_trees[which(!is.na(fallen_trees$DBH)),13], ",problem events are",
                          fallen_trees[which(!is.na(fallen_trees$DBH)),"MacroPlot.Name"] ,
                          fallen_trees[which(!is.na(fallen_trees$DBH)),"Monitoring.Status"] ),collapse=" ")
  }else{
    cat("TRUE\n")
    cat("\n")
  }



  cat("Values for height have been deleted for trees that are no longer standing...\n")
  if(any(is.na(fallen_trees$Ht)==FALSE)){
    cat(paste(c("FALSE, values of", fallen_trees[which(!is.na(fallen_trees$Ht)),"Ht"], "are recorded for tree Ht that have crown class", fallen_trees[which(!is.na(fallen_trees$Ht)),"CrwnCl"], ",problem events are", fallen_trees[which(!is.na(fallen_trees$Ht)),c("MacroPlot.Name", "Monitoring.Status")], "\n" ), collapse=" "))
    cat("\n")
    flags<-c(flags, paste(c("FALSE, values of", fallen_trees[which(!is.na(fallen_trees$Ht)),"Ht"], "are recorded for tree Ht that have crown class", fallen_trees[which(!is.na(fallen_trees$Ht)),"CrwnCl"], ",problem events are", fallen_trees[which(!is.na(fallen_trees$Ht)),c("MacroPlot.Name", "Monitoring.Status")] ), collapse=" "))
  }else{
    cat("TRUE\n")
    cat("\n")
  }



  cat("Values for crown base height have been deleted for trees that are no longer standing...\n")
  if(any(is.na(fallen_trees$LiCrBHt)==FALSE)){
    cat(paste(c("FALSE, values of", fallen_trees[which(!is.na(fallen_trees$LiCrBHt)),"LiCrBHt"], "are recorded for tree LiCrBHt that have crown class", fallen_trees[which(!is.na(fallen_trees$LiCrBHt)),"CrwnCl"], ",problem events are", fallen_trees[which(!is.na(fallen_trees$LiCrBHt)),c("MacroPlot.Name", "Monitoring.Status")], "\n" ), collapse=" "))
    cat("\n")
    flags<-c(flags, paste(c("FALSE, values of", fallen_trees[which(!is.na(fallen_trees$LiCrBHt)),"LiCrBHt"], "are recorded for tree LiCrBHt that have crown class", fallen_trees[which(!is.na(fallen_trees$LiCrBHt)),"CrwnCl"], ",problem events are", fallen_trees[which(!is.na(fallen_trees$LiCrBHt)),c("MacroPlot.Name", "Monitoring.Status")] ), collapse=" "))
  }else{
    cat("TRUE\n")
    cat("\n")
  }
  return(flags)
}#end function #verified 10/2 by Eva



##Tree area multiplier
#' Tree area multiplier
#' @description
#' The `tree_area_multiplier_qc` function conducts quality control checks on subplot
#' fractions for different tree categories based on Diameter at Breast Height (DBH).
#' It classifies trees into `pole` (DBH < 15.1), `overstory` (DBH ≥ 15.1), and `blank`
#' (missing or blank DBH). For overstory trees, the function ensures that all have a
#' subplot fraction (`SubFrac`) of 1. For pole trees, it verifies a subplot fraction of 0.5.
#' For trees with blank or missing DBH, the function checks for a subplot fraction of 1000
#' (specific to Saguaros monitoring program) or blank. The results are stored in the `flags`
#' variable, containing details about events where subplot fractions deviate from the
#' specified criteria, and are returned by the function.
#'
#' @param tree
#'
#' @return A list of flags or data issues with fraction in the tree csv
#' @export
#'
#' @examples
#' tree_area_multiplier_qc(tree)
tree_area_multiplier_qc=function(tree){
  ##check that correct subplot fraction is entered for trees
  #classify dbh
  pole=tree[which(tree$DBH<15.1),]
  pole$treerow=which(tree$DBH<15.1)
  overstory=tree[which(tree$DBH>=15.1),]
  overstory$treerow=which(tree$DBH>=15.1)
  blank=tree[which(tree$DBH=="" | is.na(tree$DBH)),]
  blank$treerow=which(tree$DBH=="" | is.na(tree$DBH))
  cat("All pole trees have subplot fraction of 0.25 or 0.5\n")
  if(length(unique(pole$SubFrac)) %in% c(1,2)){
    if(unique(pole$SubFrac) %in% c(0.25,0.5)){
      cat("TRUE\n")
      cat("\n")
    }else{
      wrongsubfract=unique(pole[which(pole$SubFrac %in% setdiff(pole$SubFrac, c(0.25,0.5))), "SubFrac"])
      cat(paste("FALSE, subfrac values for pole trees include", wrongsubfract, "when it should be equal to 0.25 or 0.5. Problem events are:", pole[which(pole$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
                pole[which(pole$SubFrac %in% wrongsubfract), "Monitoring.Status"],"tree",pole[which(pole$SubFrac %in% wrongsubfract), "TagNo"], "\n", collapse=" "))
      cat("\n")
      flags<-c(flags, paste("FALSE, subfrac values for pole trees include", wrongsubfract, "when it should be equal to 0.25 or 0.5. Problem events are:", pole[which(pole$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
                            pole[which(pole$SubFrac %in% wrongsubfract), "Monitoring.Status"], "tree", pole[which(pole$SubFrac %in% wrongsubfract), "TagNo"]))
    }
  }else{
    #more than one result not just one
    wrongsubfract=unique(pole[which(pole$SubFrac %in% setdiff(pole$SubFrac, c(0.25,0.5))), "SubFrac"])
    cat(paste("FALSE, subfrac values for pole trees include", wrongsubfract, "when it should be equal to 0.25 or 0.5. Problem events are:", pole[which(pole$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
              pole[which(pole$SubFrac %in% wrongsubfract), "Monitoring.Status"],"tree",pole[which(pole$SubFrac %in% wrongsubfract), "TagNo"], "\n", collapse=" "))
    cat("\n")
    flags<-c(flags, paste("FALSE, subfrac values for pole trees include", wrongsubfract, "when it should be equal to 0.25 or 0.5. Problem events are:", pole[which(pole$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
                          pole[which(pole$SubFrac %in% wrongsubfract), "Monitoring.Status"], "tree", pole[which(pole$SubFrac %in% wrongsubfract), "TagNo"]))
  }

  cat("All blank dbh trees have subplot fraction of 1000 or blank\n")
  if(length(unique(na.omit(blank$SubFrac)))==1){
    if(unique(na.omit(blank$SubFrac))==1000){
      cat("TRUE\n")
      cat("\n")
    }else{
      wrongsubfract=unique(na.omit(blank$SubFrac))
      wrongsubfract=wrongsubfract[! wrongsubfract==1000]
      if(all(wrongsubfract %in% c(1,0.5,0.25))){
        #and
        if(all(blank[which(blank$SubFrac %in% wrongsubfract), "CrwnCl"] %in% c("DD", "CUS", "BBD"))){
          #all good
          cat("TRUE\n")
          cat("\n")
        }else{
          diffcc=setdiff(unique(blank[which(blank$SubFrac %in% wrongsubfract), "CrwnCl"]), c("DD", "CUS", "BBD"))
          if(all(blank[which(blank$SubFrac %in% wrongsubfract & blank$CrwnCl %in% diffcc), "Status"]=="X")){
            #all good
            cat("TRUE\n")
            cat("\n")
          }else{
            #error
            #df with blank dbh, subfrac thats not 1000, crown class thats not DD, CUS, BBD
            df=blank[which(blank$SubFrac %in% wrongsubfract & blank$CrwnCl %in% diffcc),]
            #of that df, trees which status are not x
            df=df[df$Status!="X", ]
            cat(paste("FALSE, subfrac values for blank dbh trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                      df[, "Monitoring.Status"],"tree",df[, "TagNo"], "\n", collapse=" "))
            cat("\n")
            flags<-c(flags, paste("FALSE, subfrac values for blank trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                                  df[, "Monitoring.Status"], "tree", df[, "TagNo"]))
          }
        }
      }else{
        #error
        #df with blank dbh, subfrac thats not 1000, crown class thats not DD, CUS, BBD
        df=blank[which(blank$SubFrac %in% wrongsubfract & blank$CrwnCl %in% diffcc),]
        #of that df, trees which status are not x
        df=df[df$Status!="X", ]
        cat(paste("FALSE, subfrac values for blank dbh trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                  df[, "Monitoring.Status"],"tree",df[, "TagNo"], "\n", collapse=" "))
        cat("\n")
        flags<-c(flags, paste("FALSE, subfrac values for blank trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                              df[, "Monitoring.Status"], "tree", df[, "TagNo"]))
        }
   }
  }else{
    wrongsubfract=unique(na.omit(blank$SubFrac))
    wrongsubfract=wrongsubfract[! wrongsubfract==1000]
    if(all(wrongsubfract %in% c(1,0.5,0.25))){
      #and
      if(all(blank[which(blank$SubFrac %in% wrongsubfract), "CrwnCl"] %in% c("DD", "CUS", "BBD"))){
        #all good
        cat("TRUE\n")
        cat("\n")
      }else{
        diffcc=setdiff(unique(blank[which(blank$SubFrac %in% wrongsubfract), "CrwnCl"]), c("DD", "CUS", "BBD"))
        if(all(blank[which(blank$SubFrac %in% wrongsubfract & blank$CrwnCl %in% diffcc), "Status"]=="X")){
          #all good
          cat("TRUE\n")
          cat("\n")
        }else{
          #error
          #df with blank dbh, subfrac thats not 1000, crown class thats not DD, CUS, BBD
          df=blank[which(blank$SubFrac %in% wrongsubfract & blank$CrwnCl %in% diffcc),]
          #of that df, trees which status are not x
          df=df[df$Status!="X", ]
          cat(paste("FALSE, subfrac values for blank dbh trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                    df[, "Monitoring.Status"],"tree",df[, "TagNo"], "\n", collapse=" "))
          cat("\n")
          flags<-c(flags, paste("FALSE, subfrac values for blank trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                                df[, "Monitoring.Status"], "tree", df[, "TagNo"]))
        }
      }
    }else{
      #error
      #df with blank dbh, subfrac thats not 1000, crown class thats not DD, CUS, BBD
      df=blank[which(blank$SubFrac %in% wrongsubfract & blank$CrwnCl %in% diffcc),]
      #of that df, trees which status are not x
      df=df[df$Status!="X", ]
      cat(paste("FALSE, subfrac values for blank dbh trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                df[, "Monitoring.Status"],"tree",df[, "TagNo"], "\n", collapse=" "))
      cat("\n")
      flags<-c(flags, paste("FALSE, subfrac values for blank trees include", wrongsubfract, "when it should be equal to 1000.(Doesn't have CC DD, CUS, BBD, or status X) Problem events are:", df[, "MacroPlot.Name"],
                            df[, "Monitoring.Status"], "tree", df[, "TagNo"]))
      }
    }



  cat("All overstory trees have subplot fraction of 1\n")
  if(length(unique(overstory$SubFrac))==1){
    if(unique(overstory$SubFrac)==1){
      cat("TRUE\n")
      cat("\n")
    }else{
      wrongsubfract=unique(overstory[which(overstory$SubFrac!=1), "SubFrac"])
      cat(paste("FALSE, subfrac values for overstory trees include", wrongsubfract, "when it should be equal to 1. Problem events are:", overstory[which(overstory$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
                overstory[which(overstory$SubFrac %in% wrongsubfract), "Monitoring.Status"],"tree",overstory[which(overstory$SubFrac %in% wrongsubfract), "TagNo"], "\n", collapse=" "))
      cat("\n")
      flags<-c(flags, paste("FALSE, subfrac values for overstory trees include", wrongsubfract, "when it should be equal to 1. Problem events are:", overstory[which(overstory$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
                            overstory[which(overstory$SubFrac %in% wrongsubfract), "Monitoring.Status"], "tree", overstory[which(overstory$SubFrac %in% wrongsubfract), "TagNo"], "\n", collapse=" "))
    }
  }else{
    #more than one result not just one
    wrongsubfract=unique(overstory[which(overstory$SubFrac!=1), "SubFrac"])
    cat(paste("FALSE, subfrac values for overstory trees include", wrongsubfract, "when it should be equal to 1. Problem events are:", overstory[which(overstory$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
              overstory[which(overstory$SubFrac %in% wrongsubfract), "Monitoring.Status"],"tree",overstory[which(overstory$SubFrac %in% wrongsubfract), "TagNo"], "\n", collapse=" "))
    cat("\n")
    flags<-c(flags, paste("FALSE, subfrac values for overstory trees include", wrongsubfract, "when it should be equal to 1. Problem events are:", overstory[which(overstory$SubFrac %in% wrongsubfract), "MacroPlot.Name"],
                          overstory[which(overstory$SubFrac %in% wrongsubfract), "Monitoring.Status"], "tree", overstory[which(overstory$SubFrac %in% wrongsubfract), "TagNo"], "\n", collapse=" "))
  }

  return(flags)
}#end function #verified 10/2 by Eva - lots of flags however


##Tree sample area
#' Tree sample area
#' @description
#' The `tree_sample_area_qc` function performs quality control checks on the sample area
#' information of the tree data. It first ensures that all macroplot sizes (`MacroPlotSize`)
#' are 0.1, with detailed information about problematic events stored in the `flags` variable.
#' Similarly, it verifies that all snagplot sizes (`SnagPlotSize`) are 0.1. The function also
#' checks that break point diameters (`BrkPntDia`) are 15.1. For each of these checks, it
#' provides a logical value and details about any non-compliance events stored in the `flags`
#' variable, which is returned by the function.
#'
#' @param tree
#'
#' @return A list of flags or data issues with header in the tree csv
#' @export
#'
#' @examples
#' tree_sample_area_qc(tree)
tree_sample_area_qc=function(tree){

  #check for incorrect sampling information
  #need to filter for only plot level information - all else will be blank
  #take out rows with tag numbers - these include tree information
  tree=tree[which(is.na(tree$TagNo)),]

  cat("All macroplot sizes are 0.1 or blank...\n")
  if(length(unique(tree$MacroPlotSize))==1){
    if(unique(unique(tree$MacroPlotSize) %in% c(0.1))){
      cat("TRUE\n")
      cat("\n")
    }else{
      #something wrong
      cat(paste(c("FALSE, macroplot size values for trees include", unique(tree$MacroPlotSize), "when it should only be 0.1, problem events listed in flags\n"), collapse=" "))
      cat("\n")
      flags<-c(flags, paste("MacroPlotSize values for all trees in tree data set is", unique(tree$MacroPlotSize), "when it should only be 0.1, problem events:", tree[which(tree$MacroPlotSize==setdiff(unique(tree$MacroPlotSize), c(0.1))), "MacroPlot.Name"],
                            tree[which(tree$MacroPlotSize==setdiff(unique(tree$MacroPlotSize), c(0.1))), "Monitoring.Status"]))
    }
  }else{
    #something wrong
    cat(paste(c("FALSE, macroplot size values for trees include", unique(tree$MacroPlotSize), "when it should only be 0.1, problem events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("MacroPlotSize values for all trees in tree data set is", unique(tree$MacroPlotSize), "when it should only be 0.1, problem events:", tree[which(tree$MacroPlotSize==setdiff(unique(tree$MacroPlotSize), c(0.1))), "MacroPlot.Name"],
                          tree[which(tree$MacroPlotSize==setdiff(unique(tree$MacroPlotSize), c(0.1))), "Monitoring.Status"]))
  }


  cat("All snagplot sizes are 0.1...\n")
  if(length(unique(tree$SnagPlotSize))==1){
    if(unique(unique(tree$SnagPlotSize) %in% c(0.1))){
      cat("TRUE\n")
      cat("\n")
    }else{
      #something wrong
      cat(paste("FALSE, Snagplot size values for trees include", unique(tree$SnagPlotSize), "when it should only be 0.1, problem events listed in flags\n"), collapse=" ")
      cat("\n")
      flags<-c(flags, paste("SnagPlotSize values for all trees in tree data set is", unique(tree$SnagPlotSize), "when it should only be 0.1, problem events:", tree[which(tree$SnagPlotSize==setdiff(unique(tree$SnagPlotSize), c(0.1))), "MacroPlot.Name"],
                            tree[which(tree$SnagPlotSize==setdiff(unique(tree$SnagPlotSize), c(0.1))), "Monitoring.Status"]))
    }
  }else{
    #something wrong
    cat(paste("FALSE, Snagplot size values for trees include", unique(tree$SnagPlotSize), "when it should only be 0.1, problem events listed in flags\n"), collapse=" ")
    cat("\n")
    flags<-c(flags, paste("SnagPlotSize values for all trees in tree data set is", unique(tree$SnagPlotSize), "when it should only be 0.1, problem events:", tree[which(tree$SnagPlotSize==setdiff(unique(tree$SnagPlotSize), c(0.1))),"MacroPlot.Name"],
                          tree[which(tree$SnagPlotSize==setdiff(unique(tree$SnagPlotSize), c(0.1))), "Monitoring.Status"]))
  }



  cat("All Break point diameters are 15.1 or blank...\n")
  if(length(unique(tree$BrkPntDia))==1){
    if(unique(unique(tree$BrkPntDia) %in% c(15.1))){
      cat("TRUE\n")
      cat("\n")
    }else{
      #something wrong
      cat(paste(c("FALSE, Break point diameter values for trees include", unique(tree$BrkPntDia), "when it should only be 15.1, problem events listed in flags\n"), collapse=" "))
      cat("\n")
      flags<-c(flags, paste("Break Point Diameter values for all trees in tree data set is", unique(tree$BrkPntDia), "when it should only be 15.1, problem events:", tree[which(tree$BrkPntDia==setdiff(unique(tree$BrkPntDia), c(15.1))), "MacroPlot.Name"],
                            tree[which(tree$BrkPntDia==setdiff(unique(tree$BrkPntDia), c(15.1))), "Monitoring.Status"]))
    }
  }else{
    #something wrong
    cat(paste(c("FALSE, Break point diameter values for trees include", unique(tree$BrkPntDia), "when it should only be 15.1, problem events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Break Point Diameter values for all trees in tree data set is", unique(tree$BrkPntDia), "when it should only be 15.1, problem events:", tree[which(tree$BrkPntDia==setdiff(unique(tree$BrkPntDia), c(15.1))), "MacroPlot.Name"],
                          tree[which(tree$BrkPntDia==setdiff(unique(tree$BrkPntDia), c(15.1))), "Monitoring.Status"]))
  }

  return(flags)
}#end function #verified 10/11 by Eva



###tree height
#' Tree height
#' @description
#'The `tree_height_qc` function categorizes trees based on diameter at breast height
#'(DBH) into pole, overstory, and blank trees. For overstory trees, it conducts a
#'Rosner test to identify height outliers, reporting details if four or more valid
#'samples are available. The function also checks pole tree data to ensure entered
#'height classes fall within the acceptable range of 1 to 10 or are blank, flagging
#'any deviations and providing event details in the `flags` variable returned by the function.
#' @param tree
#'
#' @return A list of flags or data issues with height in the tree csv
#' @export
#'
#' @examples
#' tree_height_qc(tree)
tree_height_qc=function(tree){

  #classify dbh
  pole=tree[which(tree$DBH<15.1),]
  pole$treerow=which(tree$DBH<15.1)
  overstory=tree[which(tree$DBH>=15.1),]
  overstory$treerow=which(tree$DBH>=15.1)
  blank=tree[which(tree$DBH=="" | is.na(tree$DBH)),]
  blank$treerow=which(tree$DBH=="" | is.na(tree$DBH))


  cat("Are there any outlier values in overstory tree height?\n")

  if(length(na.omit(overstory$Ht))>4){
    test<-suppressWarnings(rosnerTest(na.omit(overstory$Ht)))
    test=test$all.stats
    outliers=test[which(test$Outlier==TRUE),4]

    if(length(outliers)==0){
      cat("No\n")
      cat("\n")
    }else{
      cat(paste(c("Yes, the outlier values according to a rosner test are", outliers, ". They are in events listed in flags. The max, min, and mean of overstory tree height are", max(na.omit(overstory$Ht)), min(na.omit(overstory$Ht)), mean(na.omit(overstory$Ht)), "respectively","\n"), collapse=" "))
      cat("\n")
      flags<-c(flags,paste("The overstory tree height in the overstory data set has outlier values according to a rosner test, which are", outliers, ". They are in events", overstory[which(overstory$Ht %in% outliers), "MacroPlot.Name"],
                           overstory[which(overstory$Ht %in% outliers), "Monitoring.Status"], "of the overstory data table.
                           The max, min, and mean of overstory tree height's are", max(na.omit(overstory$Ht)), min(na.omit(overstory$Ht)), mean(na.omit(overstory$Ht)), "respectively", collapse=" ") )
    }
  }else{
    cat(paste(c("Not enough samples to conduct a rosner test (<4) for the outlier values for overstory ht in the tree dataset. Values are ", na.omit(overstory$Ht),"\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste(c("Not enough samples to conduct a rosner test (<4) for the outlier values for overstory ht in the tree dataset. Values are ", na.omit(overstory$Ht)), collapse=" "))
  }


  ###check that pole dbh ht is 1-10 or blank

  cat("Correct height classes entered for pole tree data...\n")
  htcl=setdiff(unique(na.omit(pole$Ht)), c(0:10, NA))

  if(length(htcl)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    cat(paste(c("FALSE", htcl, "values entered which do not fit with acceptable values of 0-10 or NA, problem events listed in flags\n"), sep=" "), collapse=" ")
    cat("\n")
    flags<-c(flags,paste(htcl, "values entered in height for pole tree data which do not fit with acceptable values of 0-10 or NA, problem rows are", pole[which(pole$Ht %in% htcl),"MacroPlot.Name"],
                         pole[which(pole$Ht %in% htcl),"Monitoring.Status"], sep=" ", collapse=" ") )
  }
  return(flags)
}#end function #verified 10/11 by Eva



##Tree severity
#' Tree severity
#' @description
#' The `tree_severity_qc` function checks if all char heights, scorch heights,
#' and scorch percentage heights are blank (NA) for trees measured before fires, flagging any
#' discrepancies and providing event details in the `flags` variable returned by the function. The function
#' also checks char, scorch and scorch percentage data for the sample events immediately post burn or 1 year after the fire. The user
#' has the option to filter out pole trees for char data checks (some protocols record this data
#' for pole trees and some do not) with the argument filterpoles="Y" or filterpoles="N" (default).
#' The function checks that char height and scorch height are not blank or excessively high for
#' standing trees post burn, that scorch percentage is not over 100 or blank for live trees, and that trees
#' with crown class DD, BBD, or CUS have blank char and scorch heights and scorch percentages of
#' blank or 100. Errors are flagged with details about the problem sample event.
#'
#' @param tree
#'
#' @return A list of flags or data issues with severity in the tree csv
#' @export
#'
#' @examples
#' tree_severity_qc(tree, filterpoles="Y")
tree_severity_qc=function(tree, filterpoles="N"){

  pretree=tree[which(tree$Monitoring.Status %in% c("00PR01","00PR02","01Pre")),]
  #need to add everything thats not immediate post *Post(nothing after)
  #note if monitoring status format is different change code



  cat("All char heights are blank for pre burn trees...")
  if(length(unique(pretree$CharHt))==1){
    if(is.na(unique(pretree$CharHt))){
      cat("TRUE\n")
      cat("\n")
    }else{
      #something wrong
      cat(paste(c("FALSE, Char height values for pre burn trees in tree data set include", unique(pretree$CharHt), "when it should only be NA, problem events listed in flags\n"), collapse=" "))
      cat("\n")
      flags<-c(flags, paste(c("Char height values for pre burn trees in tree data set include", unique(pretree$CharHt), "when it should only be NA, problem events:",
                              pretree[which(pretree$CharHt==setdiff(unique(pretree$CharHt), NA)),"MacroPlot.Name"],
                              pretree[which(pretree$CharHt==setdiff(unique(pretree$CharHt), NA)),"Monitoring.Status"],
                              "\n"), collapse=" "))
    }
  }else{
    #something wrong
    cat(paste(c("FALSE, Char height values for pre burn trees in tree data set include", unique(pretree$CharHt), "when it should only be NA, problem events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Char height values for pre burn trees in tree data set include", unique(pretree$CharHt), "when it should only be NA, problem events:",
                          pretree[which(pretree$CharHt==setdiff(unique(pretree$CharHt), NA)),"MacroPlot.Name"],
                          pretree[which(pretree$CharHt==setdiff(unique(pretree$CharHt), NA)),"Monitoring.Status"], "\n"), collapse=" ")
  }


  cat("All Scorch heights are blank for pre burn trees...")
  if(length(unique(pretree$ScorchHt))==1){
    if(is.na(unique(pretree$ScorchHt))){
      cat("TRUE\n")
      cat("\n")
    }else{
      #something wrong
      cat(paste(c("FALSE, Scorch height values for pre burn trees in tree data set include", unique(pretree$ScorchHt), "when it should only be NA, problem events listed in flags\n"), collapse=" "))
      cat("\n")
      flags<-c(flags, paste("Scorch height values for pre burn trees in tree data set include", unique(pretree$ScorchHt), "when it should only be NA, problem rows:",
                            pretree[which(pretree$ScorchHt==setdiff(unique(pretree$ScorchHt), NA)),"MacroPlot.Name"],
                            pretree[which(pretree$ScorchHt==setdiff(unique(pretree$ScorchHt), NA)),"Monitoring.Status"],"\n"), collapse=" ")
    }
  }else{
    #something wrong
    cat(paste(c("FALSE, Scorch height values for pre burn trees in tree data set include", unique(pretree$ScorchHt), "when it should only be NA, problem events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste("Scorch height values for pre burn trees in tree data set include", unique(pretree$ScorchHt), "when it should only be NA, problem rows:",
                          pretree[which(pretree$ScorchHt==setdiff(unique(pretree$ScorchHt), NA)),"MacroPlot.Name"],
                          pretree[which(pretree$ScorchHt==setdiff(unique(pretree$ScorchHt), NA)),"Monitoring.Status"], "\n"), collapse=" ")
  }


  cat("All Scorch percentage heights are blank for pre burn trees...")
  if(length(unique(pretree$CrScPct))==1){
    if(is.na(unique(pretree$CrScPct))){
      cat("TRUE\n")
      cat("\n")
    }else{
      #something wrong
      cat(paste(c("FALSE, Scorch percentage height values for pre burn trees in tree data set include", unique(pretree$CrScPct), "when it should only be NA, problem rows listed in flags\n"), collapse=" "))
      cat("\n")
      flags<-c(flags, paste("Scorch percentage height values for pre burn trees in tree data set include", unique(pretree$CrScPct), "when it should only be NA, problem rows:",
                            pretree[which(pretree$CrScPct==setdiff(unique(pretree$CrScPct), NA)),"MacroPlot.Name"], "\n",
                            pretree[which(pretree$CrScPct==setdiff(unique(pretree$CrScPct), NA)),"Monitoring.Status"], "\n"),
               collapse=" ")
    }
  }else{
    #something wrong
    cat(paste(c("FALSE, Scorch percentage height values for pre burn trees in tree data set include", unique(pretree$CrScPct), "when it should only be NA, problem rows listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste(c("Scorch percentage height values for pre burn trees in tree data set include", unique(pretree$CrScPct), "when it should only be NA, problem events:",
                            pretree[which(pretree$CrScPct==setdiff(unique(pretree$CrScPct), NA)),"MacroPlot.Name"],
                            pretree[which(pretree$CrScPct==setdiff(unique(pretree$CrScPct), NA)),"Monitoring.Status"], "\n"), collapse=" "))
  }

  cat("\n")
  test_for_outliers <- function(data, column_name) {
    # Display a message indicating the test being performed
    cat("Are there any outlier values in", column_name, "?\n")

    # Extract non-missing and non-NA/NaN/Inf values from the specified column
    column_values <- na.omit(data[[column_name]])

    # Check if there are any valid values for testing
    if (length(column_values) == 0) {
      # Print a message if no valid values are available for testing
      cat("No valid values to test\n")
      return()  # Exit the function if there are no valid values
    }

    # Perform Rosner test on the valid column values
    test <- rosnerTest(column_values)
    test <- test$all.stats
    outliers <- test[which(test$Outlier == TRUE), 4]

    # Check if any outliers were detected
    if (length(outliers) == 0) {
      # Print a message if no outliers were detected
      cat("No\n")
    } else {
      # Print information about the detected outliers and their details
      cat(paste("Yes, the outlier values according to a Rosner test are", outliers, ". They are in events",
                data[which(data[[column_name]] %in% outliers),"MacroPlot.Name"],data[which(data[[column_name]] %in% outliers),"Monitoring.Status"], "of the data table. For reference, the max, min, and mean of",
                column_name, "are", max(column_values), min(column_values), mean(column_values), "respectively",
                collapse = "\n"), "\n")

      # Add error messages about the detected outliers to the flags vector
      flags<- c(flags, paste("The", column_name, "has outlier values according to a Rosner test, which are", outliers,
                             ". They are in events", data[which(data[[column_name]] %in% outliers),"MacroPlot.Name"],data[which(data[[column_name]] %in% outliers),"Monitoring.Status"],
                             "of the data table. The max, min, and mean of", column_name, "'s are", max(column_values),
                             min(column_values), mean(column_values), "respectively", collapse = "\n"), "\n")
    }
  }


#filter for just immediate post reads
  pattern <- "(0[1-9]|10)Post"
  postburntrees = tree[grep(pattern, tree$Monitoring.Status),]

  #filter out pole trees
  if(filterpoles=="Y"){
    cat("Filtering out pole trees for char data checks\n")
    postburntrees = postburntrees[which(postburntrees$DBH>15.1),]
  }else if(filterpoles=="N"){
    cat("Including pole trees for char data checks (to exclude add argument filterpoles=Y to function.)\n")
  }else{
    cat("Unknown argument for filterpoles, defaulting to N (not filtering out poles for char data checks)")
    #error in argument
  }




  test_for_outliers(postburntrees, "CharHt")
  `%!in%` <- Negate(`%in%`)
  cat("All trees in post read have a char height value\n")
  blank_char=postburntrees[which(postburntrees$CharHt=="" | is.na(postburntrees$CharHt| postburntrees$TagNo!="NA")),]
  if(any(!is.na(blank_char$TagNo) &
         blank_char$CrwnCl %!in% c("DD", "BBD", "CUS"))){
    cat("FALSE, problem sample events listed in flags\n")
    x=which(!is.na(blank_char$TagNo) | blank_char$TagNo!=""| blank_char$TagNo!="NA" &
              blank_char$CrwnCl %!in% c("DD", "BBD", "CUS"))
    flags <- c(flags, paste("Tree", blank_char[x, "TagNo"], " has a blank char height and tag number is not blank and crown class is not DD, BBD, or CUS. Sample event is", blank_char[x, "MacroPlot.Name"],
                            blank_char[x, "Monitoring.Status"]))


  }else{
    cat("TRUE\n")
  }

test_for_outliers(postburntrees, "ScorchHt")
cat("All trees in post read have a scorch height value\n")
blank_scor=postburntrees[which(postburntrees$ScorchHt=="" | is.na(postburntrees$ScorchHt) | postburntrees$ScorchHt=="NA"),]
if(any(!is.na(blank_scor$TagNo) | blank_scor$TagNo!="" | blank_scor$TagNo!="NA")){
  blank_scor_tag=blank_scor[which(!is.na(blank_scor$TagNo) | blank_scor$TagNo!="" | blank_scor$TagNo!="NA"),]
  if(any(blank_scor_tag$Status!="D")){
    cat("FALSE, problem sample events listed in flags\n")
    x=which(!is.na(blank_scor$TagNo) | blank_scor$TagNo!=""| blank_scor$TagNo!="NA")
    y=which(blank_scor$Status!="D")
    x=intersect(x,y)
    flags <- c(flags, paste("Tree", blank_scor[x, "TagNo"], " has a blank scorch height and tag number is not blank and status is not dead. Sample event is", blank_scor[x, "MacroPlot.Name"],
                            blank_scor[x, "Monitoring.Status"]))

  }else{
    cat("TRUE\n")
    #good
  }



}else{
  cat("TRUE\n")
}

cat("All trees in post read have a scorch percentage equal to or under 100%\n")
if(all(na.omit(postburntrees$CrScPct)<=100)){
  cat("TRUE\n")

}else{
  cat("FALSE, problem sample events listed in flags\n")
  x=which(na.omit(postburntrees$CrScPct)>100)
  flags <- c(flags, paste("Tree", postburntrees[x, "TagNo"], " has a a scorch percentage over 100%. Sample event is", postburntrees[x, "MacroPlot.Name"],
                          postburntrees[x, "Monitoring.Status"]))

}

cat("All live trees have a scorch percentage value\n")
blank_scor_p=postburntrees[which(postburntrees$CrScPct=="" | is.na(postburntrees$CrScPct) | postburntrees$CrScPct=="NA"),]
blank_scor_p=blank_scor_p[which(blank_scor_p$Status=="L"),]
blank_scor_p=blank_scor_p[which(!is.na(blank_scor_p$TagNo) | blank_scor_p$TagNo!="" | blank_scor_p$TagNo!="NA"),]
if(nrow(blank_scor_p)>1){
  cat("FALSE, problem sample events listed in flags\n")
  flags <- c(flags, paste("Tree", blank_scor_p[, "TagNo"], "is a live tree with a blank scorch percentage. Sample event is", blank_scor_p[, "MacroPlot.Name"],
                          blank_scor_p[, "Monitoring.Status"]))

}else{
  cat("TRUE\n")
}


cat("All rows with crown class DD, BBD, or CUS have a blank char and scorch height and a scorch percentage of blank or 100\n")
postburntrees_cc=postburntrees[(which(postburntrees$CrwnCl %in% c("DD", "BBD", "CUS"))),]
if(nrow(postburntrees_cc)>1){
  postburntrees_cc_c=postburntrees_cc[which(!is.na(postburntrees_cc$CharHt) | postburntrees_cc$CharHt!="" | postburntrees_cc$CharHt!="NA"),]
  flags <- c(flags, paste("Tree", postburntrees_cc_c[, "TagNo"], "has crown class", postburntrees_cc_c[, "CrwnCl"], "and a non blank char height of", postburntrees_cc_c[, "CharHt"],". Sample event is", postburntrees_cc_c[, "MacroPlot.Name"], postburntrees_cc_c[, "Monitoring.Status"]))
  postburntrees_cc_s=postburntrees_cc[which(!is.na(postburntrees_cc$ScorchHt) | postburntrees_cc$ScorchHt!="" | postburntrees_cc$ScorchHt!="NA"),]
  flags <- c(flags, paste("Tree", postburntrees_cc_s[, "TagNo"], "has crown class", postburntrees_cc_s[, "CrwnCl"], "and a non blank Scorch height of", postburntrees_cc_s[, "ScorchHt"],". Sample event is", postburntrees_cc_s[, "MacroPlot.Name"], postburntrees_cc_s[, "Monitoring.Status"]))
  postburntrees_cc_p=postburntrees_cc[which(!is.na(postburntrees_cc$CrScPct) | postburntrees_cc$CrScPct!="" | postburntrees_cc$CrScPct!="NA" | postburntrees_cc$CrScPct==100),]
  flags <- c(flags, paste("Tree", postburntrees_cc_p[, "TagNo"], "has crown class", postburntrees_cc_p[, "CrwnCl"], "and a non blank or 100 Scorch percentage of", postburntrees_cc_p[, "CrScPct"],". Sample event is", postburntrees_cc_p[, "MacroPlot.Name"], postburntrees_cc_p[, "Monitoring.Status"]))


}else{
  cat("TRUE\n")
}




  return(flags)
}#end function #verified 10/11 by Eva

#tree status and life form
#' Tree status and life form
#' @description
#' The tree_status_lifeform_qc function assesses the quality of data related to tree status
#' and life form in the provided tree dataset. It ensures that tree statuses are either
#' "L" or "D" and that all preferred life forms are "Tree." The function reports any deviations
#' from these criteria, flagging problematic events and providing details such as macroplot names,
#' monitoring statuses, tag numbers, and comments.
#' @param tree
#'
#' @return A list of flags or data issues with tree status and life form in the tree csv
#' @export
#'
#' @examples
#' tree_status_lifeform_qc(tree)
tree_status_lifeform_qc=function(tree){
  ###check that tree status is L or D

  cat("Tree statuses are all L or D...")
  htcl=setdiff(unique(na.omit(tree$Status)), c("L", "D", ""))

  if(length(htcl)==0){
    cat("TRUE\n")
    cat("\n")
  }else{
    cat(paste(c("FALSE", htcl, "values entered which do not fit with acceptable values of L or D, problem events are in flags\n"), sep=" "), collapse=" ")
    cat("\n")
    flags<-c(flags,paste(htcl, "values entered for tree status which do not fit with acceptable values of L or D, problem events are",
                         tree[which(tree$Status %in% htcl),"MacroPlot.Name"],
                         tree[which(tree$Status %in% htcl), "Monitoring.Status"],
                         "tree tag is", tree[which(tree$Status %in% htcl), "TagNo"],
                         "comments say", tree[which(tree$Status %in% htcl), "Comment"],
                         sep=" "))
  }




  cat("All preferred lifeforms are tree for trees...")
  if(length(unique(tree$Preferred_Lifeform))==1){
    if(unique(tree$Preferred_Lifeform)=="Tree"){
      cat("TRUE")
      cat("\n")
    }else{
      #something wrong
      cat(paste(c("FALSE, preferred lifeform values for trees in tree data set include:", unique(tree$Preferred_Lifeform), ", when it should only be tree, problem events listed in flags\n"), collapse=" "))
      cat("\n")
      flags<-c(flags, paste("preferred lifeform values for  trees in tree data set include:", unique(tree$Preferred_Lifeform), ", when it should only be tree, problem events:",
                            tree[which(tree$Preferred_Lifeform %in% setdiff(unique(tree$Preferred_Lifeform), "Tree")),"MacroPlot.Name"],
                            tree[which(tree$Preferred_Lifeform %in% setdiff(unique(tree$Preferred_Lifeform), "Tree")),"Monitoring.Status"],
                            collapse=" "))
    }
  }else{
    #something wrong
    cat(paste(c("FALSE, preferred lifeform values fortrees in tree data set include", unique(tree$Preferred_Lifeform), "when it should only be tree, problem events listed in flags\n"), collapse=" "))
    cat("\n")
    flags<-c(flags, paste(c("preferred lifeform values for trees in tree data set include", unique(tree$Preferred_Lifeform), "when it should only be tree, problem events:",
                            paste(tree[which(tree$Preferred_Lifeform %in% setdiff(unique(tree$Preferred_Lifeform), "Tree")), "MacroPlot.Name"],
                                  tree[which(tree$Preferred_Lifeform %in% setdiff(unique(tree$Preferred_Lifeform), "Tree")), "Monitoring.Status"], ", ")),
                          collapse=" "))
  }
  return(flags)
}#end function #verified 10/11 by Eva

##duplicates
#' Duplicate tags
#' @description
#' The tree_duplicates_qc function addresses the identification of duplicate tree tags
#' in the dataset. It groups the data by macro plot and sample event date, then checks for
#' duplicated tag numbers within each group. If duplicates are found, it reports the error
#' events, highlighting the affected macroplot names and monitoring statuses. Additionally,
#'  the function compares differences in data for the duplicated tag numbers, pointing out
#'  any discrepancies. The final results are presented in the flags variable, which includes
#'  information about duplicate tree tags and the detected issues.
#'
#' @param tree
#'
#' @return A list of flags or data issues with duplicates in the tree csv
#' @export
#'
#' @examples
#' tree_duplicates_qc(tree)
tree_duplicates_qc=function(tree){
  #group by macro plot and sample event date (monitoring status)
  treedates=as.data.frame(str_split(unique(paste(tree$MacroPlot.Name, tree$Monitoring.Status, sep =",")), ","))
  tree[which(tree$TagNo==999),55]="Tag number 999 replaced with NA"
  tree[which(tree$TagNo==999),8]=NA

  verf=c()
  error_events=c()
  duplicates=c()
  badtags=c()
  #for loop
  for(i in 1:ncol(treedates)){
    rows=which((tree[,1]==treedates[1,i]) & (tree[,2]==treedates[2,i]))
    eve=tree[rows,]
    if(length(unique(na.omit(eve$TagNo)))==length(na.omit(eve$TagNo))){
      verf[i]="verified"
    }else{
      verf[i]="error found"
      ers=unique(na.omit(eve[which(duplicated(eve$TagNo)==TRUE), 8]))
      error_events=c(error_events, paste(c("Error found in sample event", treedates[,i], "tag numbers", unique(na.omit(eve[which(duplicated(eve$TagNo)==TRUE), 8])), "are duplicated"), collapse=" "))

      for(t in 1:length(ers)){
        duplicates=c(duplicates, eve[which(eve$TagNo==ers[t]),c(1,2,3,8,9,10,11)])
        badtags=c(badtags,unique(eve[which(eve$TagNo==ers[t]),8]))
      }

    }

  }
  badtags<<-unique(badtags)

  ###loop to find difference between tag numbers

  diff=c()
  diff2=c()
  norm=c()
  rep=rep(colnames(tree)[c(1,2,3,8,9,10,11)],length(duplicates)/7) #FIX THIS


  for(x in 1:length(duplicates)){
    if(length(setdiff(duplicates[[c(x, 1)]], duplicates[[c(x, 2)]]))==0){
      diff=c(diff, NA)
      diff2=c(diff2, NA)
      norm=c(norm,duplicates[[c(x, 1)]] )
    }else{
      diff=c(diff, setdiff(duplicates[[c(x, 1)]], duplicates[[c(x, 2)]]))
      diff2=c(diff2, setdiff(duplicates[[c(x, 2)]], duplicates[[c(x, 1)]]))
      norm=c(norm,NA )
    }

  }

  #this is so messy but it works I promise
  results=as.data.frame(rbind(rep, norm, diff, diff2))
  results[,which(!is.na(results[3,]))]

  for(g in 1:length(error_events)){
    m=seq(1,length(duplicates),7)[g]
    bad=which(!is.na(results[3,m:(m+6)]))
    bad=bad+(m-1)
    if(length(bad)==0){
      error_events[g]=paste(error_events[g],"no differences between data exist")
    }else{
      error_events[g]=paste(c(error_events[g],"; differences between data are in", results[1,bad], "which have different values of" , results[3,bad], "and", results[4,bad]),collapse=" ")
    }

  }

  cat("Any duplicate tree tags?\n")
  if(length(error_events)==0){
    cat("No\n")
    cat("\n")
  }else{
    cat(paste(error_events, "\n"), sep="\n")
    cat("\n")
  }


  ##change to quarter and subplot fraction - add this information, species too - add a comment about which trees need different tags, could be overstory and pole tagged the same number


  flags<-c(flags, error_events)

  return(flags)
}#end function

###Tree dead to alive and changing dbh
#' Tree dead to alive and changing dbh
#' @description
#'The tree_dead_to_alive_DBH_change_qc function examines tree data for instances of dynamic
#'status changes and potential Diameter at Breast Height (DBH) alterations across multiple
#'sampling events. It systematically identifies trees that consistently exhibit DBH changes
#' despite being recorded as dead. Additionally, the function captures cases where trees
#' switch between alive and dead statuses, providing specific information on dates and
#' circumstances. Flagged issues are reported in the 'flags' variable, separating DBH-related
#' concerns and occurrences of trees transitioning from dead to alive.
#' @param tree
#'
#' @return A list of flags or data issues with trees resurrecting and changing dbh when dead in the tree csv
#' @export
#'
#' @examples
#' tree_dead_to_alive_DBH_change_qc(tree)
tree_dead_to_alive_DBH_change_qc=function(tree){
  events=unique(tree$Monitoring.Status)
  plots=unique(tree$MacroPlot.Name)
  results=matrix(nrow =0, ncol = 6)


  for(pl in 1:length(plots)){

    plot_table=tree[which(tree$MacroPlot.Name==plots[pl]),]

    for(ev in 1:length(events)){

      tab=plot_table[which(plot_table$Monitoring.Status==events[ev]),]



      if(nrow(tab)>1){
        for(r in 1:nrow(tab)){

          results=rbind(results,
                        c(tab[r,"TagNo"], tab[r,"MacroPlot.Name"],
                          tab[r,"Monitoring.Status"], "Status", tab[r,"Status"], tab[r,"Date"]),
                        c(tab[r,"TagNo"], tab[r,"MacroPlot.Name"],
                          tab[r,"Monitoring.Status"], "DBH", tab[r,"DBH"], tab[r,"Date"]))
        }
      }


    }
  }
  results=as.data.frame(results)
  colnames(results)=c("TagNo", "Plot", "Monitoring_Status", "Variable", "Value", "Date")


  results=na.omit(results)

  results$Date <- as.Date(results$Date, format = "%m/%d/%Y %I:%M:%S %p")
  #if length of unique status is 1 - either all dead or alive
    #if all alive - nothing
     #if all dead - dbh check
      #flag any changes in DBH with dates
     #else(length is greater than 1) - switching it up
        #if min date has live status
          #find min dead status  - save date
          #find live status greater than save date - flag as date tree came back alive
        #else min date has dead status
           #find min live status - flag as date tree came back alive
          ##DBH check
            #isolate years where tree is dead
            #flag any changes in DBH with dates
  error_messages_DBH=c()
  error_messages_resurrections=c()
  tags=unique(results$TagNo)
  for(check in 1:length(tags)){
    tagnumber=results[which(results$TagNo==tags[check]),]
    plots=unique(tagnumber$Plot)
    for(plot in 1:length(plots)){
      individual_tree=tagnumber[which(tagnumber$Plot==plots[plot]),]
      individual_tree_s=individual_tree[which(individual_tree$Variable=="Status"),]
      individual_tree_d=individual_tree[which(individual_tree$Variable=="DBH"),]
      if(tags[check] %in% badtags){#SKIP TOO MESSY SOMETHING IS WRONG
      }else{ #running checks
        if(length(unique(individual_tree_s$Value))==1){ #either all dead or all alive"
          if(unique(individual_tree_s$Value)=="L"){#all alive - nothing to do
          }else{#all dead - DBH change check
            if(length(unique(individual_tree_d$Value))==1){#DBH doesn't change, nothing to do
            }else{#DBH DOES change - flag if the difference is greater than 1
              individual_tree_d$Value=as.numeric(individual_tree_d$Value)
              if(all(abs(diff(unique(individual_tree_d$Value)))<1)){#doesn't meet threshold of concern
              }else{#meets threshold - flag
                dates=unique(format(individual_tree_d$Date, "%Y-%m-%d"))
                error_messages_DBH=c(error_messages_DBH, paste(c("Tree number", tags[check],"in plot", unique(individual_tree$Plot),
                                                                 "has been dead for every sampling event but its DBH changes in the following values:",
                                                                 paste(individual_tree_d$Value, sep=", "),
                                                                 "on the following dates", paste(dates, collapse=", ")),collapse=" "))
              } #closing flag
            } #closing DBH does change
          } #closing all dead dbh check
        }else{  #not all dead or alive, some entries dead some alive
          if(length(which(individual_tree_s$Date==min(individual_tree_s$Date)))>1){#2 entries for min date? - probably two trees with same tag
            error_messages_resurrections=c(error_messages_resurrections, paste(c("Tree number", tags[check],"in plot", unique(individual_tree$Plot), "has two different entries for the min date - one alive and one dead, min date is", min(individual_tree_s$Date)), collapse = " "))
          }else{#only one entry for min date
            if(individual_tree_s[which(individual_tree_s$Date==min(individual_tree_s$Date)),"Value"]=="L"){#tree starts out alive - switches to dead then alive
              min_dead_date=min(individual_tree_s[which(individual_tree_s$Value=="D"),"Date"]) #record min dead date
              min_dead_date=as.Date(min_dead_date)
              new_alive_date=individual_tree_s[which(individual_tree_s$Value=="L" & individual_tree_s$Date>min_dead_date),"Date"] #record which dates are greater than min dead date with live status
              new_alive_date=as.Date(new_alive_date)
              if(length(new_alive_date)==0){ #no dates where tree is alive and was previously dead
              }else{ #there ARE dates where tree is alive and was previously dead
                error_messages_resurrections=c(error_messages_resurrections, paste(c("Tree number", tags[check],"in plot", unique(individual_tree$Plot), "started out alive in", format(min(individual_tree_s$Date), "%Y-%m-%d"), "was recorded dead in", format(min_dead_date, "%Y-%m-%d"), "and then recorded to be alive again in", format(new_alive_date, "%Y-%m-%d")), collapse = " "))
              }#close error message
            }else{#tree starts out dead - switches to alive
              new_alive_date=min(individual_tree_s[which(individual_tree_s$Value=="L"),"Date"]) #min alive date
              new_alive_date=as.Date(new_alive_date)
              error_messages_resurrections=c(error_messages_resurrections, paste(c("Tree number", tags[check], "in plot", unique(individual_tree$Plot),"started out dead in", format(min(individual_tree_s$Date), "%Y-%m-%d"), "and then recorded to be alive again in", format(new_alive_date, "%Y-%m-%d")), collapse = " "))
            }#DBH check for fluctuating trees
            dead_tree_dates=individual_tree_s[which(individual_tree_s$Value=="D"),"Date"]
            if(length(unique(individual_tree_d[which(individual_tree_d$Date %in% dead_tree_dates),5]))==1){ #no change in DBH - nothing to do
            }else{ #change in dbh
              individual_tree_d$Value=as.numeric(individual_tree_d$Value)
              if(all(abs(diff(unique(individual_tree_d$Value)))<1)){ #doesn't meet threshold of concern (1)
              }else{#meets threshold - difference is greater than 1
                dates=unique(format(individual_tree_d$Date, "%Y-%m-%d"))
                error_messages_DBH=c(error_messages_DBH, paste(c("Tree number", tags[check],"in plot", unique(individual_tree$Plot),
                                                                 "has been changing DBH when its dead in the following values:",
                                                                 paste(individual_tree_d$Value, sep=", "),
                                                                 "on the following dates",paste(dates, collapse=",")),  collapse=" "))
              }#closing error message
            }#closing change in dbh while dead
        }#closing only one entry for min date
        }#closing fluctuating from dead to alive
      }#closing not a bad tag - running checks
    }#closing loop through plots
  }#closing loop through tags
#flagging errors
  flags<-c(flags, error_messages_DBH) #add dates #check for increase in dbh which would be weird
  flags<-c(flags, error_messages_resurrections)
  cat(paste(error_messages_DBH,"\n"), sep="\n")
  cat("\n")
  cat(paste(error_messages_resurrections,"\n"), sep="\n")

#INFO ABOUT TREE RESURRECTIONS
  #tree data - trees are tagged, in theory once that tree dies it shouldn't come back alive, it shouldn't shrink - make sure it doesn't
  #how much should it matter for density calculations...
  #case by case, look at data sheet and see what happened, could be a different tree but trees have tags in them. sometimes duplicate numbers
  #once a tree dies dbh stays the same
  #is data useful and necessary
  #how do trees look over time
  #number the checks for easy reference?
  #notes of things to change: is rows the most useful to go back and correct data? maybe macroplot and sample event is better
  #add number of each test for reference?
  return(flags)
}#end function

#end of document

#all comments ever
#separate by protocol
#' Comments
#' @description
#' The `comments` function compiles unique, non-empty comments from protocols cover, 1000
#' HR Fuels, Duff, Fine Fuels, Saplings, Seedlings, and Trees, excluding common placeholders.
#' These are protocols typically used in Saguaro’s monitoring program, but the list should be
#' edited for different programs.  It categorizes comments by protocol, including macro plot
#'  names, monitoring statuses, and associated comments.
#' @param
#' cover
#' @param
#' fuel1000
#' @param
#' duff
#' @param
#' fine
#' @param
#' saps
#' @param
#' seeds
#' @param
#' tree
#'
#' @return A list of comments excluding some repeated ones that aren't relevant
#' @export
#'
#' @examples
#' comments(cover, fuel1000,duff, fine, saps, seeds, tree)
comments=function(cover, fuel1000, duff, fine, saps=NA, seeds, tree){
  comments=c()
  cover_comments=unique(na.omit(cover$Comment))
  cover_comments=cover_comments[! cover_comments %in% c('no data collected', 'data not collected', '')]
  fuel1000_comments=unique(na.omit(fuel1000$Comment))
  fuel1000_comments=fuel1000_comments[! fuel1000_comments %in% c('no data collected', 'data not collected', '')]
  duff_comments=unique(na.omit(duff$Comment))
  duff_comments=duff_comments[! duff_comments %in% c('no data collected', 'data not collected', '')]
  fine_comments=unique(na.omit(fine$Comment))
  fine_comments=fine_comments[! fine_comments %in% c('no data collected', 'data not collected', '')]
  if(is.vector(saps)){
    #skip
  }else{
  saps_comments=unique(na.omit(saps$Comment))
  saps_comments=saps_comments[! saps_comments %in% c('no data collected', 'data not collected', '')]
  }
  seeds_comments=unique(na.omit(seeds$Comment))
  seeds_comments=seeds_comments[! seeds_comments %in% c('no data collected', 'data not collected', '')]
  tree_comments=unique(na.omit(tree$Comment))
  tree_comments=tree_comments[! tree_comments %in% c('no data collected', 'data not collected', '', 'missing tag', 'NEEDS DBH', 'Tag missing')]
  comments=c("COVER PROTOCOL",
             paste(cover[which(cover$Comment %in% cover_comments), "MacroPlot.Name"],
                   cover[which(cover$Comment %in% cover_comments), "Monitoring.Status"],
                   cover[which(cover$Comment %in% cover_comments), "Comment"], sep=", "),
             "1000 HR FUELS PROTOCOL",
             paste(fuel1000[which(fuel1000$Comment %in% fuel1000_comments), "Index"],
                   fuel1000[which(fuel1000$Comment %in% fuel1000_comments), "MacroPlot.Name"],
                   fuel1000[which(fuel1000$Comment %in% fuel1000_comments), "Monitoring.Status"],
                   fuel1000[which(fuel1000$Comment %in% fuel1000_comments), "Comment"], sep=", "),

             "DUFF PROTOCOL",
             paste(
                   duff[which(duff$Comment %in% duff_comments), "MacroPlot.Name"],
                   duff[which(duff$Comment %in% duff_comments), "Monitoring.Status"],
                   duff[which(duff$Comment %in% duff_comments), "Comment"], sep=", "),


             "FINE FUELS PROTOCOL",
             paste("Index",  fine[which(fine$Comment %in% fine_comments), "Index"],
                   fine[which(fine$Comment %in% fine_comments), "MacroPlot.Name"],
                   fine[which(fine$Comment %in% fine_comments), "Monitoring.Status"],
                   fine[which(fine$Comment %in% fine_comments), "Comment"], sep=", "),


             "SEEDLINGS PROTOCOL",
             paste("Index",  seeds[which(seeds$Comment %in% seeds_comments), "Index"],
                   seeds[which(seeds$Comment %in% seeds_comments), "MacroPlot.Name"],
                   seeds[which(seeds$Comment %in% seeds_comments), "Monitoring.Status"],
                   seeds[which(seeds$Comment %in% seeds_comments), "Comment"], sep=", "),


             "TREES PROTOCOL",
             paste( tree[which(tree$Comment %in% tree_comments), "TagNo"],
                    tree[which(tree$Comment %in% tree_comments), "MacroPlot.Name"],
                    tree[which(tree$Comment %in% tree_comments), "Monitoring.Status"],
                    tree[which(tree$Comment %in% tree_comments), "Comment"], sep=", ")


  )

  if(is.vector(saps)){
    #skip
  }else{

  comments=c(comments,
  "SAPLINGS PROTOCOL",
  paste(saps[which(saps$Comment %in% saps_comments), "Index"],
        saps[which(saps$Comment %in% saps_comments), "MacroPlot.Name"],
        saps[which(saps$Comment %in% saps_comments), "Monitoring.Status"],
        saps[which(saps$Comment %in% saps_comments), "Comment"], sep=", "))
}
  return(comments)
}

#function to seperate flags by plot
#' Format flags
#' @description
#' The format_flags function generates an Excel output that consists of multiple sheets,
#'  each corresponding to a distinct macro plot and an additional sheet for comments.
#'  For each macro plot sheet, the data is organized with a column named "Issue" containing
#'  flagged information. Additional columns include "Resolved," "Resolved_by," "Action_need,"
#'  and "Other_notes," providing a structured format for documenting and tracking the
#'  resolution status of flagged issues.
#' @param
#' flags
#' @param
#' samp
#' @param
#' mtype
#' @param
#' comments
#'
#' @return Excel file separating issues by plot
#' @export
#'
#' @examples
#' format_flags(flags, samp, mtype, comments)
format_flags=function(flags, samp, mtype, comments){
  #blank list

  plots=unique(samp[which(samp$ProjectUnit_Name == mtype), "MacroPlot_Name"])



  df_flags <- data.frame(matrix(ncol = length(plots), nrow = 0))

  for(i in 1:length(flags)){
    for(x in 1:length(plots)){
      if(grepl(plots[x], flags[i])){
        df_flags[nrow(df_flags)+1, x]=flags[i]
      }else{
        #nothing
      }
    }
  }
  plots=gsub(pattern=":", x=plots, replacement="_")
  colnames(df_flags)=plots
  df_flags$Comments=c(comments_list, rep(NA,nrow(df_flags)-length(comments_list)))

  data = list()

  for(p in 1:ncol(df_flags)){
    col=na.omit(df_flags[,p])
    x1=as.data.frame(col)
    data[p]=x1
    #cat(colnames(df_flags[p]))
    #cat("\n")
    #cat("\n")
    #cat("\n")
    #cat("\n")
    #cat(paste(col, "\n"), sep="\n")
  }

  for(t in 1:(length(plots)+1)){
    data[[t]]=as.data.frame(data[[t]])
    colnames(data[[t]])="Issue"
    data[[t]]$Resolved=rep(NA, nrow(data[[t]]))
    data[[t]]$Resolved_by=rep(NA, nrow(data[[t]]))
    data[[t]]$Action_need=rep(NA, nrow(data[[t]]))
    data[[t]]$Other_notes=rep(NA, nrow(data[[t]]))
  }

  names(data)=c(plots, "Comments")
  todaysdate=Sys.Date()


  write.xlsx(
    x=data,
    file = paste(mtype,"flags_QAQC", todaysdate, ".xlsx"),
    col_names = TRUE,
    format_headers = TRUE,
    use_zip64 = FALSE
  )

}

