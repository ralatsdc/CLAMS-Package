#' @title Aligns measurement columns
#'
#' @description
#' Aligns a measurement column from the measurements data frame of
#' each CLAMS data list in a CLAMS collection list to a common time
#' sequence in seconds.
#'
#' @details
#' Optionally includes label columns which may be required for
#' plotting. By default, label columns are not included.
#'
#' Optionally selects a subset of the measurements based on a
#' selection named column single equality or minimum, maximum pair
#' column value condition. By default, all measurements are inlcuded.
#'
#' @param clams.list a CLAMS data or collection list
#' @param msr.name the measurement column name
#' @param lbl.names the label column names
#' @param sel.name the selection column name
#' @param sel.condition the selection column single equality or
#' minimum, maximum pair condition
#'
#' @return
#' A CLAMS measurement data frame containing the selected values of
#' the measurement and label columns from each CLAMS data list.
#'
#' @examples
#' ## Assign and load a CLAMS data directory
#' data.dir <- system.file("extdata", "Test", package="CLAMS")
#' clams.dir <- file.path(data.dir, "Collection-2014-12-19")
#' clams.coll <- loadClamsDir(clams.dir)
#'
#' ## Select columns and remove outliers
#' clams.coll <- selectColumns(clams.coll, do.remove.outliers=TRUE)
#'
#' ## Append test conditions
#' clams.coll <- appendColumn(clams.coll, "LIGHT", TRUE,
#'                            start.str="06:00:00 AM", stop.str="06:00:00 PM", is.daily=TRUE)
#' clams.coll <- appendColumn(clams.coll, "DARK", TRUE,
#'                            start.str="06:00:00 PM", stop.str="06:00:00 AM", is.daily=TRUE)
#' clams.coll <- appendColumn(clams.coll, "TEMP.30", TRUE,
#'                            start.str="12/21/2014 6:00:00 AM", stop.str="12/22/2014 6:00:00 AM")
#' clams.coll <- appendColumn(clams.coll, "TEMP.22", TRUE,
#'                            start.str="12/24/2014 6:00:00 AM", stop.str="12/25/2014 6:00:00 AM")
#'
#' ## Align all VO2 measurments
#' clams.msr <- alignMeasurement(clams.coll, "VO2")
#' 
#' ## Align VO2 measurments including only values during periods of light
#' clams.msr <- alignMeasurement(clams.coll, "VO2", sel.name="LIGHT", sel.condition=TRUE)
#'
#' ## Align VO2 measurments including only values for which the
#' ## corresponding XTOT measurement lies in the interval (250, 750)
#' clams.msr <- alignMeasurement(clams.coll, "VO2", sel.name="XTOT", sel.condition=c(250, 750))
#'
#' ## Align VO2 measurments also appending test condition LIGHT
#' clams.msr <- alignMeasurement(clams.coll, "VO2", c("LIGHT"))

alignMeasurement <- function(clams.list, msr.name, lbl.names=NULL, sel.name=NULL, sel.condition=NULL) {

  ## Copyright (c) 2014 Katherine B. and Raymond A. LeClair
  ## 
  ## This program can be redistributed and/or modified under the terms
  ## of the MIT License as published by the Open Source Initiative.
  ## 
  ## See the LICENSE file or http://opensource.org/licenses/MIT.

  ## Initialize return value
  clams.msr <- list()

  ## If the input CLAMS list is a data list, convert it to a
  ## collection list
  if (identical(names(clams.list), c("meta.data", "measurements"))) {
    is.data <- TRUE
    clams.list <- list(DATA=clams.list)
    
  } else {
    is.data <- FALSE
  }
  
  ## Check length and mode of user provided input
  if (length(msr.name) != 1 || !is.character(msr.name)) {
    stop("A single, character measurement name is required")
  }
  if (!is.null(lbl.names)) {
    if (!is.character(lbl.names)) {
      stop("Character label names are required")
    }
  }
  if (!is.null(sel.name)) {
    if (is.null(sel.condition)) {
      stop("A selection condition must be provided with a name")
    }
    if (length(sel.name) != 1 || !is.character(sel.name)) {
      stop("A single, character selection name is required")
    }
  }
  if (!is.null(sel.condition)) {
    if (is.null(sel.name)) {
      stop("A selection name must be provided with a condition")
    }
    if (length(sel.condition) > 2) {
      stop("A selection must use a single equality or minimum, maximum pair condition")
    }
  }
  
  ## Consider each CLAMS data list in order to assign measurement and
  ## label values, and measurement and common time sequences in
  ## seconds from the start of measurement
  msr.values <- list()
  msr.labels <- list()
  msr.times <- list()
  com.to <- -Inf
  com.by <- +Inf
  for (i.list in seq(1, length(clams.list))) {
    clams.data <- clams.list[[i.list]]
    
    ## If a selection name is specified, use the selection condition
    ## to assign the selected measurement and label values, otherwise,
    ## assign all measurement and label values
    if (!is.null(sel.name)) {
      sel.value <- clams.data$measurements[[sel.name]]
      if (length(sel.condition) == 1) {
        msr.index <- sel.value == sel.condition
      } else if (length(sel.condition) == 2) {
        msr.index <- sel.condition[1] < sel.value & sel.value < sel.condition[2]
      }
      msr.value <- clams.data$measurements[[msr.name]][msr.index]
      msr.label <- list()
      for (lbl.name in lbl.names) {
        msr.label[[lbl.name]] <- clams.data$measurements[[lbl.name]][msr.index]
      }
    }  else {
      msr.value <- clams.data$measurements[[msr.name]]
      msr.label <- list()
      for (lbl.name in lbl.names) {
        msr.label[[lbl.name]] <- clams.data$measurements[[lbl.name]]
      }
    }

    ## Compute time between measurements, in seconds
    d.t <- clams.data$measurements$D.T
    del.d.t <- as.double(difftime(d.t[2], d.t[1], units="sec"))
    
    ## Assign a measurement time sequence in seconds from the start of
    ## measurement
    n.msr <- length(msr.value)
    msr.time <- seq(0, del.d.t * (n.msr - 1), del.d.t)

    ## Find the maximum duration and minimum interval of the
    ## measurements in seconds
    com.to <- max(com.to, msr.time[n.msr], na.rm=TRUE)
    com.by <- min(com.by, del.d.t, na.rm=TRUE)

    ## Assign measurement, label, and time values
    msr.values[[i.list]] <- msr.value
    msr.labels[[i.list]] <- msr.label
    msr.times[[i.list]] <- msr.time
  }

  ## Assign a common time sequence in seconds from the start of
  ## measurement
  com.time <- seq(0, com.to, com.by)
  n.com <- length(com.time)
  
  ## Consider each CLAMS data list in order to align measurements to a
  ## common time sequence
  com.label <- list()
  clams.msr <- list(D.T=com.time)
  for (i.list in seq(1, length(clams.list))) {
    clams.data <- clams.list[[i.list]]
    msr.value <- msr.values[[i.list]]
    msr.label <- msr.labels[[i.list]]
    msr.time <- msr.times[[i.list]]
    
    ## Assign each measurement and label value to the common time
    ## interval in which the measurement was collected, with equality
    ## at the earlier time
    com.value <- vector(mode=typeof(msr.value), n.com)
    if (length(com.label) == 0) {
      for (lbl.name in lbl.names) {
        com.label[[lbl.name]] <- vector(mode=typeof(msr.label[[lbl.name]]), n.com)
      }
    }
    n.msr <- length(msr.value)
    i.com <- 1
    for (i.msr in seq(1, n.msr)) {
      while (i.com <= n.com && msr.time[i.msr] >= com.time[i.com]) {
        i.com <- i.com + 1
      }
      i.com <- i.com - 1
      com.value[i.com] <- msr.value[i.msr]
      for (lbl.name in lbl.names) {
        com.label[[lbl.name]][i.com] <- msr.label[[lbl.name]][i.msr]
      }
    }
    
    ## Append the measurement value as a column with the subject
    ## identifier as the column name
    subj.id <- toupper(clams.data$meta.data$subject.id)
    clams.msr[[subj.id]] <- com.value
  }
  
  ## Append the label value as a column with the label name as the
  ## column name
  for (lbl.name in lbl.names) {
    clams.msr[[lbl.name]] <- com.label[[lbl.name]]
  }

  ## Return the aligned CLAMS measurement and label values
  data.frame(clams.msr)
}
