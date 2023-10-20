library(pracma)
library(matrixStats)

HypsometricIntegral <- function(filename) {
  dat <- read.csv(file = filename, header = TRUE)
  dat_matrix <- (as.matrix(dat))
  h <- dat_matrix[,2] - colMins(dat_matrix, cols = 2)
  A <- colMaxs(dat_matrix, cols = 1)
  H <- h / max(h)
  a_A <- (A - dat_matrix[,1])/A
  HI <- trapz(a_A,H)
  
}

filenames <- list.files(path = "data", pattern = "csv", full.names = TRUE)
HI_values <- sapply(filenames, HypsometricIntegral)
write.csv(HI_values, file = "HI_values_Shanxi.csv")

