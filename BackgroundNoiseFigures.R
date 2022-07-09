# Noise plots
library(ggpubr)
library(stringr)

RunganNoise <-
  read.csv("/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/ThirdOctaveBandDFRunganRungan.csv")

RunganNoise$time <- str_split_fixed(RunganNoise$wav.file,pattern='_',n=3)[,3]
RunganNoise$time <- substr(RunganNoise$time,1,2)
RunganNoise <- subset(RunganNoise,noise.valuedb >20)
ggscatter(data=RunganNoise,x='center.freq',y='noise.valuedb',color = 'time')+ylim(20,65)+ theme(legend.position = "none")

MaliauNoise <-
  read.csv("/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/ThirdOctaveBandDFMaliau.csv")

MaliauNoise$time <- str_split_fixed(MaliauNoise$wav.file,pattern='_',n=3)[,3]
MaliauNoise$time <- substr(MaliauNoise$time,1,2)

ggscatter(data=MaliauNoise,x='center.freq',y='noise.valuedb',color ='time')
