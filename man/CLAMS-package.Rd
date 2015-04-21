\name{CLAMS-package}
\alias{CLAMS-package}
\alias{CLAMS}
\docType{package}
\title{
  Performs CLAMS Data Analysis
}
\description{
  Provides functions to simplify the analysis of data collected using
  the Columbus Instruments Comprehensive Lab Animal Monitoring System
  (CLAMS)
}
\details{
  \tabular{ll}{
    Package: \tab CLAMS\cr
    Type: \tab Package\cr
    Version: \tab 2.4-4\cr
    Date: \tab 2015-04-20\cr
    License: \tab MIT + file LICENSE\cr
  }
  The Columbus Instruments Comprehensive Lab Animal Monitoring System
  (CLAMS) incorporates sub-systems for open circuit calorimetry, and
  monitoring of activity, body mass, feeding, drinking, body core
  temperature, and heart rate in an optional environmental chamber.

  The CLAMS data collection software produces a primary data file which
  separates each measured quantity into epochs defined by the response
  time of calorimetric readings. The data file typically includes the
  following meta-data:
  \tabular{l}{
    CSV FILE CREATION\cr
    EXPERIMENT START\cr
    DATA FILENAME\cr
    GROUP/CAGE\cr
    O2 CAL Date\cr
    CO2 CAL Date\cr
    SUBJECT ID\cr
    SUBJECT MASS\cr
    REFERENCE SETTLE TIME\cr
    REFERENCE MEASURE TIME\cr
    CAGE SETTLE TIME\cr
    CAGE MEASURE TIME\cr
    REFERENCE METHOD\cr
    HEAT CALCULATION METHOD\cr
  }
  And measurements in the data file typically include:
  \tabular{ll}{
    INTERVAL \tab Interval Number\cr
    CHAN \tab Chamber Number\cr
    DATE.TIME \tab Date and Time of Sample\cr
    VO2 \tab Rate of Oxygen Consumption [vol/mass/time, ml/kg/hr]\cr
    O2IN \tab Oxygen Concentration at Chamber Inlet\cr
    O2OUT \tab Oxygen Concentration at Chamber Outlet\cr
    DO2 \tab Oxygen Concentration Difference [O2 In-O2 Out]\cr
    ACCO2 \tab Accumulative Oxygen Consumption\cr
    VCO2 \tab Rate of Carbon Dioxide Consumption [vol/mass/time, ml/kg/hr]\cr
    CO2IN \tab Carbon Dioxide Concentration at Chamber Inlet\cr
    CO2OUT \tab Carbon Dioxide Concentration at Chamber Outlet\cr
    DCO2 \tab Carbon Dioxide Concentration Difference [CO2 Out-CO2 In]\cr
    ACCCO2 \tab Accumulative Carbon Dioxide Consumption [l]\cr
    RER \tab Respiratory Exchange Ratio [VCO2/VO2]\cr
    HEAT \tab Rate of Heat Production [kcal/hr]\cr
    FLOW \tab Mass of Ventilating the Chamber [LPM]\cr
    STATUS1 \tab Dynamic: Animal is Feeding , Stable: Reading Obtained\cr
    FEED1 \tab Mass of Food Consumed During Interval [GRAMS]\cr
    FEED1.ACC \tab Accumulative Food Consumption [GRAMS]\cr
    XTOT \tab Total Number of X-axis IR-Beam Breaks [COUNTS]\cr
    XAMB \tab Number of Ambulatory X-axis IR-Beam Breaks [COUNTS]\cr
    BODY.TEMP \tab Body Core Temperature as Measured by Telemetry [degC]\cr
  }

  During a typical data collection, illumination is controlled to
  produce periods of light, and dark. And if an environmental chamber is
  used, temperature is controlled.
  
  The functions of this package simplify loading the primary data file
  into R, selecting measurements for analysis, appending test
  conditions, aligning measurements between chambers or data collection
  experiments, and plotting aligned measurements.
}
\author{
  Author: Katherine and Raymond LeClair\cr
  Maintainer: Raymond LeClair <raymond.leclair@gmail.com>
}
% \references{
% }
\keyword{package}
% \seealso{
% }
\examples{
## Load measurments for all specimens
data.dir <- system.file("extdata", "Test", package="CLAMS")
clams.dir <- file.path(data.dir, "Collection-2014-12-19")
clams.coll <- loadClamsDir(clams.dir)

## Select columns and remove outliers
clams.coll <- selectColumns(clams.coll, do.remove.outliers=TRUE)

## Append test conditions
clams.coll <- appendColumn(clams.coll, "LIGHT", TRUE,
                           start.str="06:00:00 AM", stop.str="06:00:00 PM", is.daily=TRUE)
clams.coll <- appendColumn(clams.coll, "DARK", TRUE,
                           start.str="06:00:00 PM", stop.str="06:00:00 AM", is.daily=TRUE)
clams.coll <- appendColumn(clams.coll, "TEMP.30", TRUE,
                           start.str="12/21/2014 6:00:00 AM", stop.str="12/22/2014 6:00:00 AM")
clams.coll <- appendColumn(clams.coll, "TEMP.22", TRUE,
                           start.str="12/24/2014 6:00:00 AM", stop.str="12/25/2014 6:00:00 AM")

## Align and plot oxygen consumption for each specimen
clams.msr <- alignMeasurement(clams.coll, "VO2", c("LIGHT", "DARK", "TEMP.30", "TEMP.22"))
plotMeasurement(clams.msr, "Oxygen Consumption, ml/kg/hr", "VO2.pdf",
                agg.names=list(c("CRE.NEG.NH", "CRE.NEG.HL", "CRE.NEG.RL"), c("CRE.POS.HR.2", "CRE.POS.HR.4", "CRE.POS.2R")),
                agg.labels=c("CRE.NEG", "CRE.POS"),
                con.names=c("TEMP.30", "TEMP.22"),
                con.labels=expression(paste("30", degree, "C"), paste("22", degree, "C")),
                do.filter="m", do.error.bars=TRUE)

## Align and plot respiratory exchange ratio for each specimen
clams.msr <- alignMeasurement(clams.coll, "RER", c("LIGHT", "DARK", "TEMP.30", "TEMP.22"))
plotMeasurement(clams.msr, "Respiratory Exchange Ratio, RER", "RER.pdf",
                agg.names=list(c("CRE.NEG.NH", "CRE.NEG.HL", "CRE.NEG.RL"), c("CRE.POS.HR.2", "CRE.POS.HR.4", "CRE.POS.2R")),
                agg.labels=c("CRE.NEG", "CRE.POS"),
                con.names=c("TEMP.30", "TEMP.22"),
                con.labels=expression(paste("30", degree, "C"), paste("22", degree, "C")),
                do.filter="m", do.error.bars=TRUE)
}
