# Loading6AxDataNoDriving.R

# # load 6 axis data from accelerometer and gyroscope, xyz-axis, no driving data (indiscernible from 'still')
# # considering 'biking', 'downstairs', 'running', 'still', 'upstairs', 'walking' 6 categories

# # use RSQLite
library(RSQLite)    # load DBI package

# # biking
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Training/JAMES_measure_BIKING_151217T203208.db")
.biking1 <- dbGetQuery(conn=.connectToDb, statement="SELECT * FROM measurements")
# # cut the matrix by (conditions applicable to the rest)
# #     1) delete the first 5 seconds, 16Hz * 5 = 80
# #     2) clean up the matrix to 12000 rows
# #     3) trim "_id", "accTstamp", "gyroTstamp" columns
bikingData <- as.matrix(.biking1)[81:12080, c(-1,-2,-6)]
colnames(bikingData) <- c("aX","aY","aZ","gX","gY","gZ")
# # 12213 samples in .biking1

# # going downstairs
# # similar to biking case, but can not finish all measurements once
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Training/JAMES_measure_DOWNSTAIRS_151218T090756.db")
.downstairs1 <- dbGetQuery(conn=.connectToDb, statement="SELECT * FROM measurements")
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Training/JAMES_measure_DOWNSTAIRS_151218T092807.db")
.downstairs2 <- dbGetQuery(conn=.connectToDb, statement="SELECT * FROM measurements")
downstairsData <- rbind(as.matrix(.downstairs1)[81:6080, c(-1,-2,-6)], as.matrix(.downstairs2)[81:6080, c(-1,-2,-6)])
colnames(downstairsData) <- c("aX","aY","aZ","gX","gY","gZ")
# # 8780 samples in .downstairs1
# # 6716 samples in .downstairs2

# # running
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Training/JAMES_measure_RUNNING_151217T220308.db")
.running1 <- dbGetQuery(conn=.connectToDb, statement="SELECT * FROM measurements")
runningData <- as.matrix(.running1)[81:12080, c(-1,-2,-6)]
colnames(runningData) <- c("aX","aY","aZ","gX","gY","gZ")
# # 12149 samples in .running1

# # still
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Training/JAMES_measure_STILL_151217T152240.db")
.still1 <- dbGetQuery(conn=.connectToDb, statement="SELECT * FROM measurements")
stillData <- as.matrix(.still1)[81:12080, c(-1,-2,-6)]
colnames(stillData) <- c("aX","aY","aZ","gX","gY","gZ")
# # 12238 samples in .still1

# # going upstairs
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Training/JAMES_measure_UPSTAIRS_151218T085404.db")
.upstairs1 <- dbGetQuery(conn=.connectToDb, statement="SELECT * FROM measurements")
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Training/JAMES_measure_UPSTAIRS_151218T091856.db")
.upstairs2 <- dbGetQuery(conn=.connectToDb, statement="SELECT * FROM measurements")
upstairsData <- rbind(as.matrix(.upstairs1)[81:6080, c(-1,-2,-6)], as.matrix(.upstairs2)[81:6080, c(-1,-2,-6)])
colnames(upstairsData) <- c("aX","aY","aZ","gX","gY","gZ")
# # 6700 samples .upstairs1
# # 6816 samples .upstairs2

# # walking
.connectToDb <- dbConnect(drv=RSQLite::SQLite(), dbname="DataSets/Training/JAMES_measure_WALKING_151217T221617.db")
.walking1 <- dbGetQuery(conn=.connectToDb, statement="SELECT * FROM measurements")
walkingData <- as.matrix(.walking1)[81:12080, c(-1,-2,-6)]
colnames(walkingData) <- c("aX","aY","aZ","gX","gY","gZ")
# # 12308 samples