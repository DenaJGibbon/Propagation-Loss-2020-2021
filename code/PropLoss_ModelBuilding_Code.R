library(lme4)
library(lmerTest)
library(ggplot2)
library(MASS)
library(multcomp)
library(FSA)
library(DHARMa)
library(ggpubr)

data <- read.csv("data.csv",T,",")
data$call.type <- as.factor(data$call.type) 
data$site <- as.factor(data$site) 
data$time.cat <- as.factor(data$time.cat) 

### Z scaling frequency & distance data??
data$center.frequency <- scale(data$center.frequency, center = TRUE, scale = TRUE)
data$distance <- scale(data$distance, center = TRUE, scale = TRUE)


# Do we still need this?? characterize propagation loss as a function of frequency & distance at each site ------------------
## Build 2 models: 1 for Maliau, 1 for Rungan
## Loss of dB from reference recorder = signal - noise of selection
## Frequency = (log) center frequency of selection
## Distance = (log) distance from reference recorder



# 1. compare prop loss across habitats, sound types, times of day ------------------
## LMM or GAM? AIC model selection (include all 2-way interactions?)
## Response: Prop.Loss = dB loss per doubling distance
## Site = Maliau or Mungku Baru
## Habitat = Dipterocarp, Kerangas, Mixed Swamp, Low Pole
## Call type = Hfun start, Hfun peak, Halb start, Halb peak, Pmor roar, Pwur roar, Pwur sigh
## Time = predawn, dawn, morning, midday, afternoon, predusk, dusk, night
## Random effects = recorder ID, recorder location

## Example model = LMM
lmm.prop.loss.full <- lmer(Prop.Loss ~ Site + Habitat + Call.Type + Time + (Call.Type|recorder.ID + Call.Type|recorder.location), data=data)
summary(lmm.prop.loss.full)
require(DHARMa)
fittedModel = lmm.prop.loss.full
simulationOutput=simulateResiduals(fittedModel=fittedModel)
plot(simulationOutput)

### Use this for posthoc tests for variables of interest, e.g., call type
summary(glht(lmm.prop.loss.full, linfct = mcp(Call.Type = "Tukey")), test = adjusted("holm"))

### Plotting data
plot.prop.loss <- ggboxplot(data=typical.pulses,x = 'Call.Type', y = 'Prop.Loss', fill = 'Habitat', font.label = list(size = 16, face = "plain"), xlab = "Call type", ylab = "dB loss per doubling distance")


# 2  compare background noise across habitats, sound types, time ------------------
## LMM or GAM? AIC model selection (include all 2-way interactions?)
## Response: Noise = SPL of background noise in third-octave bands
## Frequency bin = Third-octave bands? or avg. center frequency of each call type?
## Site = Maliau or Mungku Baru
## Habitat = Dipterocarp, Kerangas, Mixed Swamp, Low Pole
## Time = predawn, dawn, morning, midday, afternoon, predusk, dusk, night
## Random effects = recorder ID, recorder location



# 3. estimate detection distances -----------------------------------------



