# # Preprocessing.R

# # Description: remove some singular/unexpected data from acc and gyro

func_preprocess <- function(inputMatrix) {
	# inputAccMatrix: row - time index, column - accX,accY,accZ, gyroX,gyroY,gyroZ
    accMatrix <- inputMatrix[, 1:3]
    gyroMatrix <- inputMatrix[, 4:6]
    # only cut the singular value out of [-40, +40] for acc, [-5, +5] for gyro, 'running' is the extreme
    # since we don't know the orientation of the wearable device, the upper and lower bound need to be symmetric
    accMatrix[accMatrix > +40] <- +40   # normally, +25, but due to symmetric, we make it +40
    accMatrix[accMatrix < -40] <- -40
    gyroMatrix[gyroMatrix > +5] <- +5
    gyroMatrix[gyroMatrix < -5] <- -5
    # scaling is left to the PCA part
	return(cbind(accMatrix, gyroMatrix))
}