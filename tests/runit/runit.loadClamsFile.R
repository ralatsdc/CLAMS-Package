.setup <- function () {
}

test.FailsWhenFileDoesNotExist <- function () {
  checkException(loadClamsFile(file.path(data.dir, "File.Does.Not.Exist.CSV")))
}

test.LoadsEmptyFile <- function () {
  clams.data <- loadClamsFile(file.path(data.dir, "Empty.File.CSV"))
  checkEquals(clams.data$meta.data, list())
  checkEquals(clams.data$measurements, data.frame())
}

test.LoadsFileWithoutMetaData <- function () {
  clams.data.a <- loadClamsFile(file.path(data.dir, "File.Without.Meta.Values.CSV"))
  clams.data.b <- loadClamsFile(file.path(data.dir, "Collection-2013-01-15/2013-01-15.0101.CSV"))
  checkEquals(clams.data.a$meta.data, list())
  checkEquals(clams.data.b$measurements, clams.data.b$measurements)
}

test.LoadsFileWithoutMeasurments <- function () {
  clams.data.a <- loadClamsFile(file.path(data.dir, "File.Without.Data.Mode.CSV"))
  clams.data.b <- loadClamsFile(file.path(data.dir, "Collection-2013-01-15/2013-01-15.0101.CSV"))
  checkEquals(clams.data.a$meta.data, clams.data.b$meta.data)
  checkEquals(clams.data.a$measurements, data.frame())
  checkEquals(clams.data.a$meta.data, clams.data.b$meta.data)

  clams.data.a <- loadClamsFile(file.path(data.dir, "File.Without.Data.Values.1.CSV"))
  checkEquals(nrow(clams.data.a$measurements), 0)
  checkEquals(ncol(clams.data.a$measurements), 23)

  clams.data.a <- loadClamsFile(file.path(data.dir, "File.Without.Data.Values.2.CSV"))
  checkEquals(clams.data.a$meta.data, clams.data.b$meta.data)
  checkEquals(clams.data.a$measurements, data.frame())
}

test.LoadsFile <- function () {
  clams.data <- loadClamsFile(file.path(data.dir, "Collection-2013-01-15/2013-01-15.0101.CSV"))

  checkEquals(clams.data$meta.data$csv.file.creation, "3/27/2014 1:26:31 PM")
  checkEquals(clams.data$meta.data$experiment.start, "1/15/2013 5:34:05 PM")
  checkEquals(clams.data$meta.data$data.filename, "C:\\Users\\Katherine\\Desktop\\2013-01-15.CDTA")
  checkEquals(clams.data$meta.data$group.cage, "0101")
  checkTrue(is.na(clams.data$meta.data$o2.cal.date))
  checkTrue(is.na(clams.data$meta.data$co2.cal.date))
  checkEquals(clams.data$meta.data$subject.id, "WT1")
  checkEquals(clams.data$meta.data$subject.mass, "26.2 G")
  checkEquals(clams.data$meta.data$reference.settle.time, "85 seconds")
  checkEquals(clams.data$meta.data$reference.measure.time, "5 seconds")
  checkEquals(clams.data$meta.data$cage.settle.time, "85 seconds")
  checkEquals(clams.data$meta.data$cage.measure.time, "5 seconds")
  checkEquals(clams.data$meta.data$reference.method, "EVRYN N=6")
  checkEquals(clams.data$meta.data$heat.calculation.method, "STD_0")

  checkEquals(nrow(clams.data$measurements), 522)
  checkEquals(ncol(clams.data$measurements), 23)
  checkEquals(names(clams.data$measurements),
              c("INTERVAL", "CHAN", "DATE.TIME", "VO2", "O2IN",
                "O2OUT", "DO2", "ACCO2", "VCO2", "CO2IN", "CO2OUT",
                "DCO2", "ACCCO2", "RER", "HEAT", "FLOW", "STATUS1",
                "FEED1", "FEED1.ACC", "XTOT", "XAMB", "BODY.TEMP",
                "X"))

  checkEquals(clams.data$measurements[[1]][1], 1)
  checkEquals(clams.data$measurements[[2]][1], 0101)
  checkEquals(as.character(clams.data$measurements[[3]][1]), "1/15/2013 5:37:19 PM ")
  checkEquals(clams.data$measurements[[4]][1], 4767)
  checkEquals(clams.data$measurements[[5]][1], 20.93)
  checkEquals(clams.data$measurements[[6]][1], 20.52)
  checkEquals(clams.data$measurements[[7]][1], 0.41)
  checkEquals(clams.data$measurements[[8]][1], 0.0)
  checkEquals(clams.data$measurements[[9]][1], 4216)
  checkEquals(clams.data$measurements[[10]][1], 0.054)
  checkEquals(clams.data$measurements[[11]][1], 0.425)
  checkEquals(clams.data$measurements[[12]][1], 0.371)
  checkEquals(clams.data$measurements[[13]][1], 0.0)
  checkEquals(clams.data$measurements[[14]][1], 0.884)
  checkEquals(clams.data$measurements[[15]][1], 0.6)
  checkEquals(clams.data$measurements[[16]][1], 0.50)
  checkEquals(as.character(clams.data$measurements[[17]][1]), "STABLE")
  checkEquals(clams.data$measurements[[18]][1], 0.03)
  checkEquals(clams.data$measurements[[19]][1], 0.03)
  checkEquals(clams.data$measurements[[20]][1], 119)
  checkEquals(clams.data$measurements[[21]][1], 70)
  checkEquals(clams.data$measurements[[22]][1], 0.0)

  checkEquals(clams.data$measurements[[1]][522], 522)
  checkEquals(clams.data$measurements[[2]][522], 0101)
  checkEquals(as.character(clams.data$measurements[[3]][522]), "1/19/2013 12:47:49 PM ")
  checkEquals(clams.data$measurements[[4]][522], 5191)
  checkEquals(clams.data$measurements[[5]][522], 20.89)
  checkEquals(clams.data$measurements[[6]][522], 20.45)
  checkEquals(clams.data$measurements[[7]][522], 0.44)
  checkEquals(clams.data$measurements[[8]][522], 12.3)
  checkEquals(clams.data$measurements[[9]][522], 3465)
  checkEquals(clams.data$measurements[[10]][522], 0.052)
  checkEquals(clams.data$measurements[[11]][522], 0.370)
  checkEquals(clams.data$measurements[[12]][522], 0.318)
  checkEquals(clams.data$measurements[[13]][522], 11.2)
  checkEquals(clams.data$measurements[[14]][522], 0.668)
  checkEquals(clams.data$measurements[[15]][522], 0.6)
  checkEquals(clams.data$measurements[[16]][522], 0.48)
  checkEquals(as.character(clams.data$measurements[[17]][522]), "STABLE")
  checkEquals(clams.data$measurements[[18]][522], -0.04)
  checkEquals(clams.data$measurements[[19]][522], 20.21)
  checkEquals(clams.data$measurements[[20]][522], 695)
  checkEquals(clams.data$measurements[[21]][522], 143)
  checkEquals(clams.data$measurements[[22]][522], 0.0)
  checkTrue(is.na(clams.data$measurements[[23]][522]))
}

.teardown <- function () {
}
