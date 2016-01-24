# Preprocessing.R

# Description: remove some singular data from acc and gyro

func_preprocess <- function(inputMatrix) {
	# inputAccMatrix: row - time series, column - accX,accY,accZ, gyroX,gyroY,gyroZ
    accMatrix <- inputMatrix[, 1:3]
    gyroMatrix <- inputMatrix[, 4:6]
    # only cut the singular value out of [-40, +20] for acc, [-5, +5] for gyro, 'running' is the extreme
    accMatrix[accMatrix > +20] <- +20
    accMatrix[accMatrix < -40] <- -40
    gyroMatrix[gyroMatrix > +5] <- +5
    gyroMatrix[gyroMatrix < -5] <- -5
    # scaling is left to the PCA part
	return(cbind(accMatrix, gyroMatrix))
}