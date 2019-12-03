#Task 1
library(readr)

#Read in teeth data
teeth_data <- read.csv("teeth.csv")
N <- nrow(teeth_data)
N
A <- colnames(teeth_data)
