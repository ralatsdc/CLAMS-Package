clams.data.a <- loadClamsFile(file.path(data.dir, "Collection/2013-01-15.0101.CSV"))

.setup <- function () {
}

test.SelectsDefault <- function () {
  clams.data.b <- selectColumns(clams.data.a)
  checkEquals(names(clams.data.b$measurements),
              c("INTERVAL", "CHAN", "DATE.TIME", "VO2", "VCO2",
                "HEAT", "RER", "FEED1.ACC", "XTOT", "XAMB",
                "BODY.TEMP"))
}

test.SelectsAll <- function () {
  col.names = c(
    "INTERVAL", "CHAN", "DATE.TIME", "VO2", "O2IN", "O2OUT", "DO2",
    "ACCO2", "VCO2", "CO2IN", "CO2OUT", "DCO2", "ACCCO2", "RER",
    "HEAT", "FLOW", "STATUS1", "FEED1", "FEED1.ACC", "XTOT", "XAMB",
    "BODY.TEMP", "X")
  clams.data.b <- selectColumns(clams.data.a, col.names)
  checkEquals(names(clams.data.b$measurements), col.names)
}  

test.FailsWhenSelectingAbsentColumn <- function () {
  clams.data <- loadClamsFile(file.path(data.dir, "Collection/2013-01-15.0101.CSV"))
  checkException(select(clams.data, c("Col.Name.Does.Not.Exist")))
}

test.RemovesOutliers <- function () {
  clams.data.a$measurements$VO2[1] <- -1
  clams.data.a$measurements$VO2[2] <- 0
  clams.data.a$measurements$VO2[3] <- mean(clams.data.a$measurements$VO2)

  clams.data.a$measurements$VCO2[1] <- -1
  clams.data.a$measurements$VCO2[2] <- 0
  clams.data.a$measurements$VCO2[3] <- mean(clams.data.a$measurements$VCO2)

  clams.data.a$measurements$HEAT[1] <- -1
  clams.data.a$measurements$HEAT[2] <- 0
  clams.data.a$measurements$HEAT[3] <- mean(clams.data.a$measurements$HEAT)

  clams.data.a$measurements$RER[1] <- 0.4
  clams.data.a$measurements$RER[2] <- mean(clams.data.a$measurements$RER)
  clams.data.a$measurements$RER[3] <- mean(clams.data.a$measurements$RER)
  clams.data.a$measurements$RER[4] <- 1.3

  clams.data.a$measurements$XTOT[1] <- -1
  clams.data.a$measurements$XTOT[2] <- 0
  
  clams.data.a$measurements$XAMB[1] <- -1
  clams.data.a$measurements$XAMB[2] <- 0

  clams.data.b <- selectColumns(clams.data.a, do.remove.outliers=TRUE)

  checkTrue(is.na(clams.data.b$measurements$VO2[1]))
  checkTrue(is.na(clams.data.b$measurements$VO2[2]))
  checkTrue(!is.na(clams.data.b$measurements$VO2[3]))

  checkTrue(is.na(clams.data.b$measurements$VCO2[1]))
  checkTrue(is.na(clams.data.b$measurements$VCO2[2]))
  checkTrue(!is.na(clams.data.b$measurements$VCO2[3]))

  checkTrue(is.na(clams.data.b$measurements$HEAT[1]))
  checkTrue(is.na(clams.data.b$measurements$HEAT[2]))
  checkTrue(!is.na(clams.data.b$measurements$HEAT[3]))

  checkTrue(is.na(clams.data.b$measurements$RER[1]))
  checkTrue(!is.na(clams.data.b$measurements$RER[2]))
  checkTrue(!is.na(clams.data.b$measurements$RER[3]))
  checkTrue(is.na(clams.data.b$measurements$RER[4]))

  checkTrue(is.na(clams.data.b$measurements$XTOT[1]))
  checkTrue(!is.na(clams.data.b$measurements$XTOT[2]))
  
  checkTrue(is.na(clams.data.b$measurements$XAMB[1]))
  checkTrue(!is.na(clams.data.b$measurements$XAMB[2]))

  clams.data.a$measurements$VO2[1] <- mean(clams.data.a$measurements$VO2) + 4 * sd(clams.data.a$measurements$VO2)
  clams.data.a$measurements$VO2[2] <- mean(clams.data.a$measurements$VO2) - 4 * sd(clams.data.a$measurements$VO2)

  clams.data.a$measurements$VCO2[1] <- mean(clams.data.a$measurements$VCO2) + 4 * sd(clams.data.a$measurements$VCO2)
  clams.data.a$measurements$VCO2[2] <- mean(clams.data.a$measurements$VCO2) - 4 * sd(clams.data.a$measurements$VCO2)

  clams.data.a$measurements$HEAT[1] <- mean(clams.data.a$measurements$HEAT) + 4 * sd(clams.data.a$measurements$HEAT)
  clams.data.a$measurements$HEAT[2] <- mean(clams.data.a$measurements$HEAT) - 4 * sd(clams.data.a$measurements$HEAT)

  clams.data.a$measurements$RER[1] <- mean(clams.data.a$measurements$RER) + 4 * sd(clams.data.a$measurements$RER)
  clams.data.a$measurements$RER[2] <- mean(clams.data.a$measurements$RER) - 4 * sd(clams.data.a$measurements$RER)

  clams.data.a$measurements$XTOT[1] <- mean(clams.data.a$measurements$XTOT) + 4 * sd(clams.data.a$measurements$XTOT)
  clams.data.a$measurements$XTOT[2] <- mean(clams.data.a$measurements$XTOT) - 4 * sd(clams.data.a$measurements$XTOT)
  
  clams.data.a$measurements$XAMB[1] <- mean(clams.data.a$measurements$XAMB) + 4 * sd(clams.data.a$measurements$XAMB)
  clams.data.a$measurements$XAMB[2] <- mean(clams.data.a$measurements$XAMB) - 4 * sd(clams.data.a$measurements$XAMB)

  clams.data.b <- selectColumns(clams.data.a, do.remove.outliers=TRUE)

  checkTrue(is.na(clams.data.b$measurements$VO2[1]))
  checkTrue(is.na(clams.data.b$measurements$VO2[2]))

  checkTrue(is.na(clams.data.b$measurements$VCO2[1]))
  checkTrue(is.na(clams.data.b$measurements$VCO2[2]))

  checkTrue(is.na(clams.data.b$measurements$HEAT[1]))
  checkTrue(is.na(clams.data.b$measurements$HEAT[2]))

  checkTrue(is.na(clams.data.b$measurements$RER[1]))
  checkTrue(is.na(clams.data.b$measurements$RER[2]))

  checkTrue(is.na(clams.data.b$measurements$XTOT[1]))
  checkTrue(is.na(clams.data.b$measurements$XTOT[2]))
  
  checkTrue(is.na(clams.data.b$measurements$XAMB[1]))
  checkTrue(is.na(clams.data.b$measurements$XAMB[2]))
}

test.ReturnsClamsDataOrCollection <- function () {
  clams.coll.a <- loadClamsDir(file.path(data.dir, "Collection"))
  clams.coll.b <- selectColumns(clams.coll.a)
  checkEquals(names(clams.coll.b), c("2013-01-15.0101.CSV",
                                     "2013-01-15.0102.CSV",
                                     "2013-01-15.0103.CSV",
                                     "2013-01-15.0104.CSV",
                                     "2013-01-15.0105.CSV",
                                     "2013-01-15.0106.CSV"))
                                   
  clams.data.b <- selectColumns(clams.data.a)
  checkEquals(names(clams.data.b), c("meta.data",
                                     "measurements"))
}

.teardown <- function () {
}
