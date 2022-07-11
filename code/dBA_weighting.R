library(seewave)
library(tidyverse)

# A weight for the 1/3 Octave centre frequencies
A.weight <- dBweight(f=c(20,25,31.5,40,50,63,80,100,125,160,200,250,
                 + 315,400,500,630,800,1000,1500,
                 + 1600,2000,2500,3150,4000,5000,
                 + 6300,8000,10000,12500,16000,20000))$A

?dBweight

A.weight

view(A.weight)
