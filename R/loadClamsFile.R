#' @title Loads a data file
#'
#' @description
#' Loads a CLAMS data file.
#'
#' @details
#' The CLAMS data file is processed line by line in one of three
#' modes: "META", "DATA", and "EVENTS". These modes correspond to the
#' sections of the file. Data lines are written to a file in the same
#' directory and with the same name as the input file but with the
#' word "Extracted" added prior to the file extension.
#'
#' While in mode "DATA" lines deemed to be invalid are written to a
#' file in the same directory and with the same name as the input file
#' but with the word "Rejected" added prior to the file extension. For
#' example, lines which contain three consecutive measurements which
#' are reported as zero to three significant digits are rejected.
#'
#' @param input.file the file containing the CLAMS data
#'
#' @return
#' A CLAMS data list containing two elements:
#' \item{meta.data}{a list of meta data values}
#' \item{measurements}{a data frame of measurements}
#'
#' @examples
#' ## Assign a CLAMS data file
#' data.dir <- system.file("extdata", "Test", package="CLAMS")
#' clams.file <- file.path(data.dir, "Collection-2013-01-15", "2013-01-15.0101.CSV")
#'
#' ## Load a CLAMS data file
#' clams.data <- loadClamsFile(clams.file)

loadClamsFile <- function(input.file) {

  ## Copyright (c) 2014 Katherine B. and Raymond A. LeClair
  ## 
  ## This program can be redistributed and/or modified under the terms
  ## of the MIT License as published by the Open Source Initiative.
  ## 
  ## See the LICENSE file or http://opensource.org/licenses/MIT.
  
  ## Initialize return value
  meta.data <- list()
  measurements <- data.frame()
  clams.data <- list(meta.data=meta.data, measurements=measurements)
  
  ## Open file connections 
  input.conn <- file(input.file, open='r')
  input.file <- sub(".csv$", ".CSV", input.file, perl=TRUE)
  output.file <- sub(".CSV$", "_Extracted.CSV", input.file, perl=TRUE)
  output.conn <- file(output.file, open='w')
  reject.file <- sub(".CSV$", "_Rejected.CSV", input.file, perl=TRUE)
  reject.conn <- file(reject.file, open='w')
  
  ## Assign list of names to use in parsing meta data lines
  META.DATA.NAMES <- list(
    
    "CSV FILE CREATION"="csv.file.creation",
    "CSV Generation Date"="csv.file.creation",
    "EXPERIMENT START"="experiment.start",
    "Experiment Started"="experiment.start",
    "DATA FILENAME"="data.filename",
    "Data Filename"="data.filename",
    "GROUP/CAGE"="group.cage",
    "Group/Cage"="group.cage",
    "O2 CAL Date"="o2.cal.date",
    "O2 Calibration Date"="o2.cal.date",
    "CO2 CAL Date"="co2.cal.date",
    "CO2 Calibration Date"="co2.cal.date",
    "SUBJECT ID"="subject.id",
    "Subject ID"="subject.id",
    "SUBJECT MASS"="subject.mass",
    "Subject Mass \\(G\\)"="subject.mass",
    "REFERENCE SETTLE TIME"="reference.settle.time",
    "Reference Settle Time \\(s\\)"="reference.settle.time",
    "REFERENCE MEASURE TIME"="reference.measure.time",
    "Reference Measure Time \\(s\\)"="reference.measure.time",
    "CAGE SETTLE TIME"="cage.settle.time",
    "Cage Settle Time \\(s\\)"="cage.settle.time",
    "CAGE MEASURE TIME"="cage.measure.time",
    "Cage Measure Time \\(s\\)"="cage.measure.time",
    "REFERENCE METHOD"="reference.method",
    "Reference Method"="reference.method",
    "HEAT CALCULATION METHOD"="heat.calculation.method",
    "Heat Calculation Method"="heat.calculation.method"

    )
    
  ## Read and handle each line of the data file
  mode <- "META"
  while (length(line <- readLines(input.conn, n=1)) > 0) {

    ## Match each line to determine its mode
    if (grepl(":DATA", line)) {
      mode <- "DATA"
    } else if (grepl(":EVENTS", line)) {
      mode <- "EVENTS"
    }

    ## Handle each line based on its mode
    if (mode == "META") {
      
      ## Consider each meta data name
      for (name in names(META.DATA.NAMES)) {
        legal.name = META.DATA.NAMES[[name]]
        
        ## Search for the current name in line
        if (grepl(paste0("^", name, ":"), line, perl=TRUE) ||
            grepl(paste0("^", name, ","), line, perl=TRUE)) {
      
          ## Parse and assign the name-value pair
          substrings <- strsplit(line, paste0(name, ":"))
          if (length(substrings[[1]]) == 1) {
            substrings <- strsplit(line, paste0(name, ","))
          }
          meta.data[[legal.name]] <- substrings[[1]][2]
      
          ## Stop searching, since we found a name-value pair, and
          ## only one is expected per meta data line
          break
        }
      }
      
    } else if (mode == "DATA") {
      
      ## Write measurement lines
      if (!(grepl(":DATA", line)
            || grepl("^=", line, perl=TRUE)
            || grepl('^#', line, perl=TRUE)
            || grepl("0.000,0.000,0.000", line))) {
        writeLines(line, output.conn)
      } else {
        writeLines(line, reject.conn)
      }
      
    } else if (mode == "EVENTS") {
      
      ## TODO: Parse event lines

    }
  }
  
  ## Close file connections
  close(reject.conn)
  close(output.conn)
  close(input.conn)
  
  ## Read measurements and create corresponding data frame
  tryCatch({
    measurements <- read.csv(output.file)
  }, warning = function(w) {
    ## Do nothing
  }, error = function(e) {
    message('No measurements found')
  }, finally = {
    ## Do nothing
  })

  ## Return a list containing the meta data and measurements
  clams.data <- list(meta.data=meta.data, measurements=measurements)
}
