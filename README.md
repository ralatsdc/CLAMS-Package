CLAMS-Package
=============

Provides an R Package which includes functions to simplify the
analysis of data collected using the Columbus Instruments
Comprehensive Lab Animal Monitoring System (CLAMS)

Details
-------

The Columbus Instruments Comprehensive Lab Animal Monitoring System
(CLAMS) incorporates sub-systems for open circuit calorimetry, and
monitoring of activity, body mass, feeding, drinking, body core
temperature, and heart rate in an optional environmental chamber.

The CLAMS data collection software produces a primary data file which
separates each measured quantity into epochs defined by the response
time of calorimetric readings. The data file typically includes the
following meta-data:
* CSV FILE CREATION
* EXPERIMENT START
* DATA FILENAME
* GROUP/CAGE
* O2 CAL Date
* CO2 CAL Date
* SUBJECT ID
* SUBJECT MASS
* REFERENCE SETTLE TIME
* REFERENCE MEASURE TIME
* CAGE SETTLE TIME
* CAGE MEASURE TIME
* REFERENCE METHOD
* HEAT CALCULATION METHOD

And measurements in the data file typically include:
* INTERVAL - Interval Number
* CHAN - Chamber Number
* DATE.TIME - Date and Time of Sample
* VO2 - Rate of Oxygen Consumption [vol/mass/time, ml/kg/hr]
* O2IN - Oxygen Concentration at Chamber Inlet
* O2OUT - Oxygen Concentration at Chamber Outlet
* DO2 - Oxygen Concentration Difference [O2 In-O2 Out]
* ACCO2 - Accumulative Oxygen Consumption
* VCO2 - Rate of Carbon Dioxide Consumption [vol/mass/time, ml/kg/hr]
* CO2IN - Carbon Dioxide Concentration at Chamber Inlet
* CO2OUT - Carbon Dioxide Concentration at Chamber Outlet
* DCO2 - Carbon Dioxide Concentration Difference [CO2 Out-CO2 In]
* ACCCO2 - Accumulative Carbon Dioxide Consumption [l]
* RER - Respiratory Exchange Ratio [VCO2/VO2]
* HEAT - Rate of Heat Production [kcal/hr]
* FLOW - Mass of Ventilating the Chamber [LPM]
* STATUS1 - Dynamic: Animal is Feeding , Stable: Reading Obtained
* FEED1 - Mass of Food Consumed During Interval [GRAMS]
* FEED1.ACC - Accumulative Food Consumption [GRAMS]
* XTOT - Total Number of X-axis IR-Beam Breaks [COUNTS]
* XAMB - Number of Ambulatory X-axis IR-Beam Breaks [COUNTS]
* BODY.TEMP - Body Core Temperature as Measured by Telemetry [degC]

During a typical data collection, illumination is controlled to
produce periods of light, and dark. And if an environmental chamber is
used, temperature is controlled.
  
The functions of this package simplify loading the primary data file
into R, selecting measurements for analysis, appending test
conditions, aligning measurements between chambers or data collection
experiments, and plotting aligned measurements.

Author: Katherine and Raymond LeClair. Maintainer: Raymond LeClair
<raymond.leclair@gmail.com>

Installation
------------

# In your browser, navigate to https://github.com/ralatsdc/CLAMS-Package
# Select the most current release branch from the `branch` drop-down
# Click the `Download ZIP` button, save the zip file to your local
  machine, then extract
# Open the project in RStudio and select `Build > Build & Reload`
