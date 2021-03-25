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

# SWIFT mic has a signal to noise ratio (min) of >58db with a frequency response of 50 Hz to 16 kHz (SNR here corresponds to dynamic range of system)

# Calculate the system sensitivity. In your case:
# Mic: -44 dB re 1V/Pa – 20*log10(1,000,000/20)  <-  -137.9794 dB re 1V/20uPa
# Gain: 40 dB
# Voltage: 20*log10(0.9) = -0.9151 dB

# Total system sensitivity S = -98.8945 dB re 1V/20uPa
Sensitivity <- -137.9794+40-.9151
Sensitivity

# Normalise the values 
data <- (temp.wave@left) / 2 ^ 15
range(data)

# Calibrate the data with the microphone sensitivity
data_cal <- data/ (10^(Sensitivity/20))

# Calculate RMS
data_rms <- rms(data_cal)

# Calculate in dB re 20uPa
data_level <- 20*log10(data_rms)

# Noise values
data_level # Should be 95.8

