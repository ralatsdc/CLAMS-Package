#' @title Plots aligned measurement columns
#'
#' @description
#' Plots aligned measurement columns from the measurements data frame
#' of each CLAMS data list in a CLAMS collection list as a function of
#' a common time sequence in seconds. Optionally, removes outliers.
#'
#' @details
#' Places PDF commands needed for the plot in output.file.
#' 
#' Aggregates named measurement columns as labeled columns using the
#' \code{mean}. Optionally plots and labels named test
#' conditions. Note that measurement column and test condition labels
#' may be character or expression vectors.
#' 
#' By default outliers are not removed. If \code{do.remove.outliers}
#' is \code{TRUE}, values greater than three standard deviations from
#' the mean are removed within the duration of each condition.
#' 
#' Optionally filters the measurements by applying a trailing one hour
#' average within a duration determined by the filter flag string. Set
#' the filter flag string to "c" to filter within the duration of each
#' condition, to "m" to filter within the duration of the
#' measurements, or to any other value, or omit entirely, to prevent
#' filtering. By default, measurements are not filtered.
#'
#' Optionally plots error bars. By default, error bars are not
#' plotted.
#'
#' Optionally includes a legend at a specified location. By default, a
#' legend is not included.
#' 
#' @param clams.msr aligned measurement columns
#' @param msr.label measurement column labels
#' @param output.file file to contain plot output
#' @param agg.names aggregation column names
#' @param agg.labels aggregation column labels
#' @param con.names test condition column names
#' @param con.labels test condition plot labels
#' @param do.remove.outliers flag to remove outliers, or not
#' @param do.filter flag to filter measurements, or not
#' @param do.error.bars flag to plot error bars, or not
#' @param legend.coord position as one of "bottomright", "bottom",
#'   "bottomleft", "left", "topleft", "top", "topright", "right" and
#'   "center"
#'
#' @return
#' A boolean indicating success, or not.
#'
#' @examples
#' ##  Assign and load a CLAMS data directory
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
#' ## Align oxygen consumption measurements for each specimen and append
#' ## test conditions
#' clams.msr <- alignMeasurement(clams.coll, "VO2", c("LIGHT", "DARK", "TEMP.30", "TEMP.22"))
#'
#' ## Plot aligned measurements, aggregating and labeling named columns,
#' ## labeling test conditions, filtering measurements, and plotting
#' ## error bars
#' plotMeasurement(clams.msr, "Oxygen Consumption, ml/kg/hr", "VO2.pdf",
#'                 agg.names=list(c("CRE.NEG.NH", "CRE.NEG.HL", "CRE.NEG.RL"),
#'                                c("CRE.POS.HR.2", "CRE.POS.HR.4", "CRE.POS.2R")),
#'                 agg.labels=c("CRE.NEG", "CRE.POS"),
#'                 con.names=c("TEMP.30", "TEMP.22"),
#'                 con.labels=expression(paste("30", degree, "C"), paste("22", degree, "C")),
#'                 do.filter="c", do.error.bars=TRUE, legend.coord="topleft")
#'
#' @export

plotMeasurement <- function(clams.msr, msr.label, output.file, agg.names, agg.labels,
                            con.names=NULL, con.labels=NULL, do.remove.outliers=FALSE, do.filter="n",
                            do.error.bars=FALSE, legend.coord="") {
  
  
  ## Copyright (c) 2014 Katherine B. and Raymond A. LeClair
  ## 
  ## This program can be redistributed and/or modified under the terms
  ## of the MIT License as published by the Open Source Initiative.
  ## 
  ## See the LICENSE file or http://opensource.org/licenses/MIT.
  
  ## Check length and mode of user provided input
  if (!is.data.frame(clams.msr)) {
    stop("A CLAMS measurement data frame is required")
  }
  if (length(setdiff(c("LIGHT", "DARK"), names(clams.msr))) != 0) {
    stop("The CLAMS measurement data frame must contain a 'LIGHT' and 'DARK' column")
  }
  if (!(is.character(msr.label) || is.expression(msr.label))) {
    stop("The measurement label must be mode 'character' or 'expression'")
  }
  if (length(output.file) > 1 || !is.character(output.file)) {
    stop("The output file must be mode 'character'")
  }
  if (is.null(agg.names) || is.null(agg.labels)) {
    stop("Aggregation names and labels must be specified together")
  }
  if (!is.list(agg.names)) {
    stop("Aggregation names must be a list of character vectors")
  } else {
    for (names in agg.names) {
      if (!is.vector(names, mode="character")) {
        stop("Aggregation names must be a list of character vectors")
      }
    }
  }
  if (!(is.vector(agg.labels, mode="character") || is.vector(agg.labels, mode="expression"))) {
    stop("Aggregation labels must be character or expression vectors")
  }
  if (length(agg.names) != length(agg.labels)) {
    stop("Aggregation names and labels must have equal length")
  }
  if (!is.null(con.names)) {
    if (is.null(con.labels)) {
      stop("Condition names and labels must be specified together")
    }
    if (!is.vector(con.names, mode="character")) {
      stop("Condition names must be character vectors")
    }
  }
  if (!is.null(con.labels)) {
    if (is.null(con.names)) {
      stop("Condition names and labels must be specified together")
    }
    if (!(is.vector(con.labels, mode="character") || is.vector(con.labels, mode="expression"))) {
      stop("Condition labels must be character or expression vectors")
    }
  }
  if (length(do.remove.outliers) > 1 || !is.logical(do.remove.outliers)) {
    stop("The remove outliers flag must have mode 'logical'")
  }
  if (length(do.filter) > 1 || !is.character(do.filter)) {
    stop("The filter flag must have mode 'character'")
  }
  if (length(do.error.bars) > 1 || !is.logical(do.error.bars)) {
    stop("The error bar flag must have mode 'logical'")
  }
  if (! legend.coord %in% c("bottomright", "bottom", "bottomleft",
                            "left", "topleft", "top", "topright",
                            "right", "center", "")) {
    stop("Unexpected legend coordinates")
  }
  
  ## Initialize return value
  status <- FALSE
  
  ## Remove outliers within the duration of each condition, if requested
  msr.names <- setdiff(names(clams.msr), c("D.T", "LIGHT", "DARK", con.names))
  if (do.remove.outliers) {
    n.cn <- length(con.names)
    if (n.cn > 0) {
      for (i.cn in 1 : n.cn) {
        con <- con.names[i.cn]
        idx.con <- clams.msr[[con]]
        for (msr.name in msr.names) {
          mn.msr <- mean(clams.msr[[msr.name]], na.rm=TRUE)
          sd.msr <- sd(clams.msr[[msr.name]], na.rm=TRUE)
          clams.msr[[msr.name]][clams.msr[[msr.name]] < mn.msr - 3 * sd.msr] <- NA
          clams.msr[[msr.name]][mn.msr + 3 * sd.msr < clams.msr[[msr.name]]] <- NA
        }
      }
    }
  }

  ## Replace each NA with the most recent non-NA prior to it for each
  ## measurement column
  clams.msr[msr.names] <- zoo::na.locf(clams.msr[msr.names])

  ## Filter, if requested, then aggregate aligned measurement columns
  clams.sem <- list()
  d.t <- clams.msr[["D.T"]]
  n.cf <- length(d.t[d.t <= 3600]) # Number of samples in one hour
  n.an <- length(agg.names)
  for (i.an in 1 : n.an) {
    ans <- agg.names[[i.an]]
    lbl <- agg.labels[i.an]
    
    ## Filter the measurement to be aggregated, if requested
    if (do.filter == "c") {

      ## Filter within the duration of each condition
      n.cn <- length(con.names)
      if (n.cn > 0) {
        for (i.cn in 1 : n.cn) {
          con <- con.names[i.cn]
          idx.con <- clams.msr[[con]]
          ## Note that NAs are handled by earlier replacing them with
          ## the previous value with the result that the filter
          ## weights are still exactly correct
          clams.msr[idx.con, ans] <- filter(clams.msr[idx.con, ans], rep(1, n.cf) / n.cf,
                                            method="convolution", sides=1)
        }
      }
    } else if (do.filter == "m") {

      ## Filter within the duration of the experiment
      clams.msr[ans] <- filter(clams.msr[ans], rep(1, n.cf) / n.cf,
                               method="convolution", sides=1)
    }
    
    ## Aggregate aligned measurement columns
    clams.msr[[lbl]] <- apply(clams.msr[ans], 1, mean)
    clams.sem[[lbl]] <- apply(clams.msr[ans], 1, sd) / sqrt(length(ans))
  }
  
  ## Turn on the graphic device
  pdf(output.file, width=5.5, height=4.5)
  
  ## Configure and label the axes
  yrng <- range(clams.msr[agg.labels], na.rm=TRUE)
  yint <- diff(yrng)
  ylim <- yrng + yint * c(-0.120, 0.040)
  xlim <- range(d.t / 3600)
  plot(d.t / 3600, clams.msr[[agg.labels[1]]],
       type="b", col="white", bty="o", tcl=0.25, xpd=TRUE,
       xaxs="r", xlim=xlim, xaxt="n", yaxs="r", ylim=ylim,
       xlab="Time, h", ylab=msr.label)
  axis(side=1, at=seq(xlim[1], xlim[2] + 12, 12), tcl=0.25)
       
  ## Fill rectangles corresponding to periods of light and dark
  col <- list(LIGHT="gray98", DARK="gray92")
  int <- 1 : length(d.t)
  for (ilm in c("LIGHT", "DARK")) {
    idx.ilm <- clams.msr[[ilm]]
    idx.dff <- diff(int[idx.ilm]) > 1
    d.t.min <- min(d.t[idx.ilm])
    d.t.beg <- c(d.t.min, d.t[idx.ilm][c(FALSE, idx.dff)])
    d.t.max <- max(d.t[idx.ilm])
    d.t.end <- c(d.t[idx.ilm][c(idx.dff, FALSE)], d.t.max)
    n.int <- length(d.t.beg)
    for (i.int in seq(1, n.int)) {
      polygon(c(d.t.beg[i.int], d.t.beg[i.int], d.t.end[i.int], d.t.end[i.int]) / 3600,
              c(ylim[1], ylim[2], ylim[2], ylim[1]), col=col[[ilm]], border=NA)
    }
  }
  
  ## Label the conditions
  yarr <- ylim[2] + yint * 0.175
  ytxt <- ylim[2] + yint * 0.200
  n.cn <- length(con.names)
  if (n.cn > 0) {
    for (i.cn in 1 : n.cn) {
      con <- con.names[i.cn]
      lbl <- con.labels[i.cn]
      idx.con <- clams.msr[[con]]
      d.t.con <- range(d.t[idx.con])
      suppressMessages(
        arrows(d.t.con[1] / 3600, yarr, d.t.con[2] / 3600, yarr,
               length=0.10, angle=20, code=3, xpd=TRUE)
      )
      text(mean(d.t.con) / 3600, ytxt, pos=3, lbl, xpd=TRUE)
    }
  }
  
  ## Plot the measurements, and error bars, if requested, and a legend
  col <- c("black", "black", "black", "black", "black", "black", "black", "black")
  pch <- c(21, 21, 21)
  bg <- c("black", "white", "grey")
  n.an <- length(agg.names)
  i.do <- 0
  idx <- seq(1, length(d.t), n.cf)
  for (i.an in 1 : n.an) {
    lbl <- agg.labels[i.an]
    ## Note that NAs are handled by earlier replacing them with the
    ## previous value with the result that the indexed value is valid
    ## with the correct mean and standard error of the mean
    lines(d.t[idx] / 3600, clams.msr[[lbl]][idx],
          type="o", pch=pch[i.an], col=col[i.an], bg=bg[i.an], cex=0.8)
    if (do.error.bars) {
      ym <- clams.msr[[lbl]] - clams.sem[[lbl]] / 2
      yp <- clams.msr[[lbl]] + clams.sem[[lbl]] / 2
      int <- seq(n.cf + (i.an - 1) * n.cf, length(d.t), 2 * n.cf)
      suppressWarnings(
        arrows(d.t[int] / 3600, ym[int], d.t[int] / 3600, yp[int],
               length=0.05, angle=90, code=3, col=col[i.an])
      )
    }
  }
  if (legend.coord != "") {
    legend(legend.coord, agg.labels,
           lty=rep(1, n.an), pch=pch[1 : n.an], col=col[1 : n.an], pt.bg=bg[1 : n.an],
           inset=c(0.02, 0.02), xpd=TRUE, bty="n")
  }
  
  ## Turn off the graphic device
  dev.off()
  status <- TRUE
}
