library(lme4)
library(lmerTest)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(MASS)
library(multcomp)
library(FSA)
library(DHARMa)

setwd("~/github/Propagation-Loss-2020-2021/data")
mean.power <- read.csv("Mean_PowerDb.csv",T,",")
dB <- read.csv("Rungan_Signal-Noise.csv",T,",")

dB$time.cat <- factor(dB$time.cat, levels = c("predawn", "dawn", "morning","midday", "afternoon", "predusk", "dusk", "night"))

dB$PAM.No <- factor(dB$PAM.No, levels = c("1_K", "6_K", "2_K","5_MS", "7_MS", "3_LP", "4_LP", "8_LP", "char"))

dB$distance.from.source <- factor(dB$distance.from.source, levels = c("10", "100", "250","500"))

plot.mean.Power <- mean.power %>% 
  ggboxplot(x='Time', y='Mean.Power', facet.by='Sound.Type', font.label = list(size = 16, face = "plain"), xlab="Time", ylab="Mean Power [dB]")
plot.mean.Power


plot.Noise.Loc <- dB %>% 
  ggboxplot(x='PAM.No', y='NoisevalueDb', fill='time.cat', facet.by='Sound', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Background Noise [dB]")
plot.Noise.Loc


#### Noise by Sound Type ###
plot.Noise.sweep <- dB %>% 
  filter(Sound=='Downsweep' | Sound=="Upsweep" | Sound=="Phaser") %>% 
  ggboxplot(x='PAM.No', y='NoisevalueDb', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Background Noise [dB]", title="Phaser & Sweeps")
plot.Noise.sweep
ggsave("Noise_sweep", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Noise.Halb <- dB %>% 
  filter(Sound=='Halbstart' | Sound=="Halbpeak" | Sound=="Halbend") %>% 
  ggboxplot(x='PAM.No', y='NoisevalueDb', fill = 'Sound', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Background Noise [dB]")
plot.Noise.Halb
ggsave("Noise_Halb", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)


plot.Noise.Hfun <- dB %>% 
  filter(Sound=='Hfunstart' | Sound=="Hfuntrill") %>% 
  ggboxplot(x='PAM.No', y='NoisevalueDb', fill = 'Sound', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Background Noise [dB]")
plot.Noise.Hfun
ggsave("Noise_Hfun", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)


plot.Noise.Pmor <- dB %>% 
  filter(Sound=='Pmor_start' | Sound=="Pmor") %>% 
  ggboxplot(x='PAM.No', y='NoisevalueDb', fill = 'Sound', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Background Noise [dB]")
plot.Noise.Pmor
ggsave("Noise_Pmor", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)


plot.Noise.Pwur <- dB %>% 
  filter(Sound=='PwurP' | Sound=="PwurS") %>% 
  ggboxplot(x='PAM.No', y='NoisevalueDb', fill = 'Sound', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Background Noise [dB]")
plot.Noise.Pwur
ggsave("Noise_Pwur", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

### Power Signal ###
plot.Signal.sweep <- dB %>% 
  filter(Sound=='Downsweep' | Sound=="Upsweep" | Sound=="Phaser") %>% 
  ggboxplot(x='PAM.No', y='PowerDb', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Phaser & Sweeps")
plot.Signal.sweep
ggsave("Signal_sweep", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)


plot.Signal.Halb <- dB %>% 
  filter(Sound=='Halbstart' | Sound=="Halbpeak" | Sound=="Halbend") %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'Sound', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]")
plot.Signal.Halb
ggsave("Signal_Halb", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.Halbstart <- dB %>% 
  filter(Sound=='Halbstart') %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]"title="Halb start")
plot.Signal.Halbstart
ggsave("Signal_Halbstart", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.Halbpeak <- dB %>% 
  filter(Sound=='Halbpeak') %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Halb peak")
plot.Signal.Halbpeak
ggsave("Signal_Halbpeak", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.Halbend <- dB %>% 
  filter(Sound=='Halbend') %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Halb end")
plot.Signal.Halbend
ggsave("Signal_Halbend", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.Hfun <- dB %>% 
  filter(Sound=='Hfunstart' | Sound=="Hfuntrill") %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'Sound', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]")
plot.Signal.Hfun
ggsave("Signal_Hfun", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.Hfunstart <- dB %>% 
  filter(Sound=='Hfunstart') %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Hfun start")
plot.Signal.Hfunstart
ggsave("Signal_Hfunstart", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.Hfunstart.afternoon <- dB %>% 
  filter(Sound=="Hfunstart") %>% 
  filter(time.cat=="afternoon") %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Hfun start afternoon")
plot.Signal.Hfunstart.afternoon
ggsave("Signal_Hfunstart_afternoon", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.Hfuntrill <- dB %>% 
  filter(Sound=="Hfuntrill") %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Hfun trill")
plot.Signal.Hfuntrill
ggsave("Signal_Hfuntrill", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.Hfuntrill.afternoon <- dB %>% 
  filter(Sound=="Hfuntrill") %>% 
  filter(time.cat=="afternoon") %>% 
    ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Hfun trill afternoon")
plot.Signal.Hfuntrill.afternoon
ggsave("Signal_Hfuntrill_afternoon", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)


plot.Signal.Pmor <- dB %>% 
  filter(Sound=='Pmor_start' | Sound=='Pmor') %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'Sound', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]")
plot.Signal.Pmor
ggsave("Signal_Pmor", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.Pmorstart <- dB %>% 
  filter(Sound=='Pmor_start') %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Pmor start")
plot.Signal.Pmorstart
ggsave("Signal_Pmorstart", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.Pmorpulse <- dB %>% 
  filter(Sound=='Pmor') %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Pmor pulse")
plot.Signal.Pmorpulse
ggsave("Signal_Pmorpulse", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.Pmorpulse.morn <- dB %>% 
  filter(Sound=='Pmor') %>% 
  filter(time.cat=='morning') %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Pmor pulse am")
plot.Signal.Pmorpulse.morn
ggsave("Signal_Pmorpulse_morning", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.Pmorpulse.after <- dB %>% 
  filter(Sound=='Pmor') %>% 
  filter(time.cat=='afternoon') %>%
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Pmor pulse pm")
plot.Signal.Pmorpulse.after
ggsave("Signal_Pmorpulse_afternoon", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)


plot.Signal.Pwur <- dB %>% 
  filter(Sound=='PwurP' | Sound=="PwurS") %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'Sound', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]")
plot.Signal.Pwur
ggsave("Signal_Pwur", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.PwurP <- dB %>% 
  filter(Sound=='PwurP') %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title = "Pwur Pulse")
plot.Signal.PwurP
ggsave("Signal_PwurP", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.PwurS <- dB %>% 
  filter(Sound=='PwurS') %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'distance.from.source', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Pwur Sigh")
plot.Signal.PwurS
ggsave("Signal_PwurS", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Signal.PwurS.100 <- dB %>% 
  filter(Sound=="PwurS") %>% 
  filter(distance.from.source==100) %>% 
  ggboxplot(x='PAM.No', y='PowerDb', fill = 'Swift', facet.by='time.cat', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]")
plot.Signal.PwurS.100
ggsave("Signal_PwurS 100m", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)


#### Characterization ###
plot.Character.Signal <- dB %>% 
  filter(Loc_Name=='char1' | Loc_Name=='char2' | Loc_Name=='char3') %>% 
  ggboxplot(x='Sound', y='PowerDb', fill='Swift', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Characterization")
plot.Character.Signal
ggsave("Signal_Character", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)

plot.Character.Noise <- dB %>% 
  filter(Loc_Name=='char1' | Loc_Name=='char2' | Loc_Name=='char3') %>% 
  ggboxplot(x='Sound', y='NoisevalueDb', fill='Swift', font.label = list(size = 16, face = "plain"), xlab="Location", ylab="Signal Power [dB]", title="Characterization: Noise")
plot.Character.Noise
ggsave("Signal_Noise", plot = last_plot(), path = '/Users/Wendy/github/Propagation-Loss-2020-2021/Figures/', device = "jpeg", scale = 1, dpi = 300,limitsize = TRUE, bg = NULL)


# Model Building ----------------------------------------------------------


data <- read.csv("data.csv",T,",")
data$call.type <- as.factor(data$call.type) 
data$site <- as.factor(data$site) 
data$time.code <- as.factor(data$time.code) 

z_data <- scale(data[x:x], center = TRUE, scale = TRUE)


# center frequency --------------------------------------------------------

mod.maliau=lm(ReceivedLevel~Center.Freq+Distance +(||),data=z_data)
summary(mod.maliau)
require(DHARMa)
fittedModel=mod.maliau
simulationOutput=simulateResiduals(fittedModel=fittedModel)
plot(simulationOutput)


summary(glht(mod.pulse.center, linfct = mcp(Pulse.Type = "Tukey")), test = adjusted("holm"))

plot.typical.center <- ggboxplot(data=typical.pulses,x='Cluster',y='Center.Freq.Hz', font.label = list(size = 16, face = "plain"), xlab="Cluster", ylab="Center Frequency [Hz]")