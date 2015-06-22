#' @title Analyzes measurement columns
#'
#' @description
#' Aligns a measurement column from the measurements data frame of
#' each CLAMS data list in a CLAMS collection list, then fits an
#' analysis of variance model, and computes a corresponding effects
#' object.
#' 
#' @details
#' Optionally specifies the model using a formula. By default, the
#' formula is specified as:
#'   paste(msr.name, "~ GENOTYPE + ADJ.WEIGHT")
#' 
#' Optionally selects a subset of the measurements based on a
#' selection named column single equality or minimum, maximum pair
#' column value condition. By default, all measurements are included.
#'
#' Optionally specifies the function used to aggregate the
#' measurements. By default, the \code{mean} is used to aggregate
#' measurements of oxygen consumption ("VO2"), carbon monoxide
#' production ("VCO2"), respiratory exchange ratio ("RER"), and body
#' temperature ("BODY.TEMP"), and the \code{sum} is used to
#' aggregate heat production ("HEAT"), accumulated food consumption
#' ("FEED1.ACC"), and total ("XTOT") and ambient ("XAMB")
#' activity. The default can be overridden for any measurement by
#' providing a list with an element equal to the desired function and
#' named for the measurement.
#'
#' Note that the \code{mean} is used to aggregate all other numeric
#' column values, and \code{unique} is used to aggregate character
#' column values.
#' 
#' @param clams.coll a CLAMS collection list
#' @param msr.name the measurement column name
#' @param mdl.formula the character formula specifying the model
#' @param sel.name the selection column name
#' @param sel.condition the selection column single equality or
#' minimum, maximum pair condition
#' @param agg.functions the functions used to aggregate the
#' measurements
#' 
#' @return
#' A CLAMS analysis list containing three elements:
#' \item{name}{the name of the analyzed measurement}
#' \item{data}{a data frame containing the aligned and aggregated
#' measurements}
#' \item{model}{an analysis of variance model}
#' \item{effect}{an effects object}
#'
#' @examples
#' ## Assign and load a CLAMS data directory
#' data.dir <- system.file("extdata", "Test", package="CLAMS")
#' clams.dir <- file.path(data.dir, "Collection-2015-01-06")
#' clams.coll <- loadClamsDir(clams.dir)
#' 
#' ## Select columns and remove outliers
#' clams.coll <- selectColumns(clams.coll, do.remove.outliers=TRUE) 
#'   
#' ## Append a column indicating genotype, temperature, original weight,
#' ## and adjusted weight
#' clams.coll <- appendColumn(clams.coll, "genotype", c("KO", "WT", "KO", "KO", "WT"))
#' clams.coll <- appendColumn(clams.coll, "temp.22", TRUE, 
#'                            start.str="01/08/2015 06:00:00 AM", stop.str="01/09/2015 06:00:00 AM")
#' clams.coll <- appendColumn(clams.coll, "orig.weight", c(23.8, 25.8, 22.6, 25.5, 25.5))
#' clams.coll <- appendColumn(clams.coll, "adj.weight", c(23.5, 24.3, 22.6, 22.4, 24.9), 
#'                            start.str="01/08/2015 06:00:00 AM", stop.str="01/09/2015 06:00:00 AM")
#'   
#' ## Analyze heat production
#' clams.analysis <- analyzeMeasurement(clams.coll, "HEAT", sel.name="TEMP.22", sel.condition=TRUE)

analyzeMeasurement <- function(
  clams.coll, msr.name, mdl.formula=NULL, sel.name=NULL, sel.condition=NULL, agg.functions=NULL) {

  ## Copyright (c) 2014 Katherine B. and Raymond A. LeClair
  ## 
  ## This program can be redistributed and/or modified under the terms
  ## of the MIT License as published by the Open Source Initiative.
  ## 
  ## See the LICENSE file or http://opensource.org/licenses/MIT.

  ## Check length and mode of user provided input
  if (!is.list(clams.coll)) {
    stop("A CLAMS collection list is required")
  }
  if (length(msr.name) != 1 || !is.character(msr.name)) {
    stop("A single, character measurement name is required")
  }
  if (!is.null(mdl.formula)) {
    if (!is.character(mdl.formula)) {
      stop("A character formula string is required to specify the model")
    }
    mdl.formula <- as.formula(mdl.formula)
  } else {
    mdl.formula <- as.formula(paste(msr.name, "~ GENOTYPE + ADJ.WEIGHT"))
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

  ## Define functions used for aggregating the named measurement
  def.functions <- list(
    "VO2"=mean,
    "ADJ.VO2"=mean,
    "VCO2"=mean,
    "ADJ.VCO2"=mean,
    "RER"=mean,
    "BODY.TEMP"=mean,
    "HEAT"=sum,
    "FEED1.ACC"=sum,
    "XTOT"=sum,
    "XAMB"=sum)
  if (!is.null(agg.functions)) {
    if (!is.list(agg.functions)) {
      stop("Aggregation functions must be mode list")
    }
    if (is.null(names(agg.functions))) {
      stop("Aggregation function list element names are missing")
    }
    if (length(setdiff(names(agg.functions), names(def.functions))) > 0) {
      stop("Aggregation function list element names are invalid")
    }
    for (agg.function in agg.functions) {
      if (!is.function(agg.function)) {
        stop("Aggregation function elements do not all have mode function")
      }
    }
  } else {
    agg.functions <- list()
  }
  for (name in setdiff(names(def.functions), names(agg.functions))) {
    agg.functions[[name]] <- def.functions[[name]]
  }
  if (!(msr.name %in% names(agg.functions))) {
    stop("Unexpected measurement name")
  }

  ## Initialize return value
  clams.analysis <- list()
  clams.analysis$name <- msr.name

  ## Determine independent variables
  ind.vars <- setdiff(all.vars(mdl.formula), msr.name)

  ## Align measurement and model variables
  col.data <- list()
  for (mdl.var in c(msr.name, ind.vars)) {
    col.data[[mdl.var]] <- alignMeasurement(clams.coll, mdl.var, sel.name=sel.name, sel.condition=sel.condition)

    ## Aligned numeric measurements default to 0.0 if missing, so set
    ## to NA for later removal
    if (!is.factor(col.data[[mdl.var]][[2]])) {
      col.data[[mdl.var]][col.data[[mdl.var]] == 0] <- NA
    }
  }

  ## Find the subject names
  sub.names <- setdiff(names(col.data[[msr.name]]), "D.T")

  ## Define a function to remove empty strings from string sets
  uniquediff <- function (x) {
    setdiff(unique(x), "")
  }

  ## Create the data frame required for fitting the model
  data <- data.frame(apply(col.data[[msr.name]][sub.names], 2, agg.functions[[msr.name]], na.rm=TRUE))
  names(data) <- msr.name
  clams.analysis$data <- data
  for (mdl.var in ind.vars) {
    if (is.factor(col.data[[mdl.var]][[2]])) {
      data <- data.frame(apply(col.data[[mdl.var]][sub.names], 2, uniquediff))

    } else if (is.numeric(col.data[[mdl.var]][[2]])) {
      data <- data.frame(apply(col.data[[mdl.var]][sub.names], 2, mean, na.rm=TRUE))

    } else {
      stop("Unexpected mode for model variable")
    }
    names(data) <- mdl.var
    clams.analysis$data <- cbind(clams.analysis$data, data)
  }
  
  ## Fit an analysis of variance model
  clams.analysis$model <- aov(mdl.formula, clams.analysis$data)

  ## Construct the corresponding effects object
  clams.analysis$effect <- effect("GENOTYPE", clams.analysis$model)

  clams.analysis
}
