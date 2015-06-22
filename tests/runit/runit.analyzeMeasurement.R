clams.coll <- loadClamsDir(file.path(data.dir, "Collection-2015-01-06"))

clams.coll <- selectColumns(clams.coll, do.remove.outliers=TRUE) 

clams.coll <- appendColumn(clams.coll, "genotype", c("KO", "WT", "KO", "KO", "WT"));
clams.coll <- appendColumn(clams.coll, "temp.22", TRUE, 
                           start.str="01/08/2015 06:00:00 AM", stop.str="01/09/2015 06:00:00 AM")
clams.coll <- appendColumn(clams.coll, "orig.weight", c(23.8, 25.8, 22.6, 25.5, 25.5))
clams.coll <- appendColumn(clams.coll, "adj.weight", c(23.5, 24.3, 22.6, 22.4, 24.9),
                           start.str="01/08/2015 06:00:00 AM", stop.str="01/09/2015 06:00:00 AM")

.setup <- function () {
}

test.FailsWithInvalidInput <- function () {
  checkException(analyzeMeasurement(0.0, "HEAT", sel.name="TEMP.22", sel.condition=TRUE))
  checkException(analyzeMeasurement(clams.coll, c("HEAT", "VO2"), sel.name="TEMP.22", sel.condition=TRUE))
  checkException(analyzeMeasurement(clams.coll, 0.0, sel.name="TEMP.22", sel.condition=TRUE))
  checkException(analyzeMeasurement(clams.coll, "HEAT", mdl.formula=as.formula("HEAT ~ GENOTYPE")))
  checkException(analyzeMeasurement(clams.coll, "HEAT", sel.name="TEMP.22"))
  checkException(analyzeMeasurement(clams.coll, "HEAT", sel.name=c("TEMP.22", "TEMP.4"), sel.condition=TRUE))
  checkException(analyzeMeasurement(clams.coll, "HEAT", sel.name=0.0, sel.condition=TRUE))
  checkException(analyzeMeasurement(clams.coll, "HEAT", sel.condition=TRUE))
  checkException(analyzeMeasurement(clams.coll, "HEAT", sel.name="TEMP.22", sel.condition=c(TRUE, FALSE, TRUE)))
  checkException(analyzeMeasurement(clams.coll, "HEAT", agg.functions=sum))
  checkException(analyzeMeasurement(clams.coll, "HEAT", agg.functions=list()))
  checkException(analyzeMeasurement(clams.coll, "HEAT", agg.functions=list("VO2"=NULL)))
  checkException(analyzeMeasurement(clams.coll, "HEAT", agg.functions=list("BO2"=sum)))
  checkException(analyzeMeasurement(clams.coll, "HEAR", sel.name="TEMP.22", sel.condition=TRUE))
}

test.analyzeMeasurement <- function () {
  clams.ancova.heat <- analyzeMeasurement(clams.coll, "HEAT", sel.name="TEMP.22", sel.condition=TRUE)
  checkEquals(clams.ancova.heat$effect$fit[1], 57.52061538461538)
  checkEquals(clams.ancova.heat$effect$fit[2], 60.16907692307694)
}

.teardown <- function () {
}
