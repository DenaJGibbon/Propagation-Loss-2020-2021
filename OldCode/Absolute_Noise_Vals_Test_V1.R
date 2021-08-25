# Measure absolute noise values in R

# Load libraries
library(tuneR)
library(stringr)
library(seewave)
library(signal)

# Read in wave file
temp.wave <-
  readWave(
    "/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/Calibration.wav"
  )

# Set microphone sensitivity
gain <- 40

Sensitivity <- -137.9794 + gain - 0.9151 # dB relative to 1 volt per pascal

# Need to filter before calculating RMS 

# Normalise the values 
data <- (temp.wave@left) / (2 ^ 16/2)
range(data)

# Calibrate the data with the microphone sensitivity
data_cal <- data/ (10^(Sensitivity/20))

# Calculate RMS
data_rms <- rms(data_cal)

# Calculate in dB re 20uPa
data_level <- 20*log10(data_rms)

# Noise values
data_level # Should be 95.8

