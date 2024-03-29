# This is the R code to estimate propagation loss in Borneo 
# Testing 123


# NOTES: In this setup both the recorders and the playback speakers were moving
# Swift recorders are indicated by S00974
# There are 8 locations that are part of the permanent grid (Loc_name); in this study 3 habitat types were sampled but all 8 locations
# One series of playbacks with the same Swift; playback speaker moves to 100m, 250m and 500m 

# Version summary ---------------------------------------------------------
# Version 2. Add adaptive noise estimates
# Version 3. Modify noise calculations so divide into ten 1-sec bins
# Version 4. Remove adaptive noise estimates
# Version 5. July 12- Run for two sites; had to remove '-' from text file names so that the .wav files would match
# Version 6. recorder in code is analogous to playback #; Loc_Name is a permanent location

# Part 1. Load necessary packages -------------------------------------------------------------
library(seewave)
library(tuneR)
library(stringr)
library(plyr)
library(ggpubr)
library(geosphere)
library(plotKML)
library(dplyr)
library(googledrive)


# Part 2. Set up data -------------------------------------------------------------

# Playback template table
setwd <- '/Users/Wendy/github/Propagation-Loss-2020-2021'

SelectionIDs <- 
  read.delim("SelectionLabels_S00974_20190811_101922_updated.txt")
#NOTE that there are two Pwur call types

# Remove pulses
PulsesToRemove <- c(10,11,19,20,21,30,31,32,33,34,
                    44, 45, 53, 54, 55, 64,65,66,67,68,
                    78, 79, 87, 88, 89, 98,99,100,101,102)

PlaybackSeq <- seq(1,nrow(SelectionIDs),1)

PlaybackSeqUpdated <- PlaybackSeq[-PulsesToRemove]
#SelectionIDs <- SelectionIDs[-PulsesToRemove,]

# Sound file location
SoundFiles.input <- 
  '/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/3Jun/sound_files/'

# SoundFiles.input <- 
#   '/Users/Wendy/github/Propagation-Loss-2020-2021/mungku_baru_data/test/sound_files/'

# Selection table location
Rungan.playbacks.wav.files <- read.csv('Rungan.playbacks.wav.files.csv')
  
input.dir <- 
  "/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/RunganAll"

# Read in GPS data
#recorder.gps <- plotKML::readGPX("/Users/Wendy/github/Propagation-Loss-2020-2021/mungku_baru_data/MB Playbacks 50 m.GPX") 

# Set duration of the noise selection before the start of the actual selection
timesecs <- 10

# Set a buffer duration before and after selection to calculate inband power
# NOTE: Right now it is set to zero so it cuts the selection right at the boundaries of the selection table
signal.time.buffer <- 0

# Set the duration of the noise subsamples (in seconds)
noise.subsamples <- 0.5

# Set microphone gain
gain <- 40

# Calculate the microphone sensitivity for the system
Sensitivity <- -137.9794 + gain - 0.9151 # dB relative to 1 volt per pascal

# Set distance of first recorder to playback speaker (in meters)
dist.to.playback <- 100

# Read in selection tables with full file path names
selection.tables <- 
  list.files(input.dir,full.names = T)

# Read in selection tables with short file path names for adding name column
selection.tables.short <-
  list.files(input.dir,full.names = F)

# Create an index for recorder (Wendy changed last line from 1 to 2)
# list.of.recorders <- str_split_fixed(selection.tables.short, pattern = '_',3)[,2]
list.of.recorders <- str_split_fixed(selection.tables.short, pattern = '_',3)[,1]

# Read in selection tables and combine with file name
combined.template.table.test <- data.frame()

for(a in 1:length(selection.tables)){
  
  template.table.temp <- read.delim(selection.tables[a],fill = T,header=T,stringsAsFactors = F)
  
  recorder <- list.of.recorders[a]
  file.name <- selection.tables.short[a]
  file.name <- str_split_fixed(file.name,pattern = '.txt',n=2)[,1]
  
  
  template.table.temp <- cbind.data.frame(template.table.temp,recorder,file.name)
  
  template.table.temp <-template.table.temp[,c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", 
                          "Low.Freq..Hz.", "High.Freq..Hz.",
                          "SNR.NIST.Quick..dB.FS.", "Sound.Type", "recorder", "file.name"
  )]
  combined.template.table.test <- rbind.data.frame(combined.template.table.test,template.table.temp)
  #combined.template.table.test <- bind_rows(combined.template.table.test,template.table.temp)
}

# Check output to make sure it looks right
tail(combined.template.table.test)
length(unique(combined.template.table.test$file.name))

# Part 3. Calculate distance for each ---------------------------------------------------

# Read in data file
# Each row corresponds to a playback
rungan_data <- read.csv("PropLoss_test.csv")

# Loop to match files
# Identifying each unique playback number
pb.index <- unique(combined.template.table.test$recorder)

combined.template.table.test.add.dist <- data.frame()
for(b in 1:length(pb.index)){
  #from pb.rungan, find distance 
  single.pb.subset <- subset(rungan_data,PB_No==pb.index[b])
  subset.add.dist <- subset(combined.template.table.test,recorder==pb.index[b])
  subset.add.dist$distance.from.source <- rep(single.pb.subset$Dist, nrow(subset.add.dist))
  subset.add.dist$Loc_Name <- rep(single.pb.subset$Loc_Name, nrow(subset.add.dist))
  combined.template.table.test.add.dist <- rbind.data.frame(combined.template.table.test.add.dist,subset.add.dist)
}

# Add a distance column
# combined.template.table.test$distance.from.source <- str_split_fixed(combined.template.table.test$file.name,pattern = '_',n=4)[,4]

# Add a date column
combined.template.table.test.add.dist$date <- str_split_fixed(combined.template.table.test.add.dist$file.name,pattern = '_',n=4)[,3]

# Add a time column
time.temp <- str_split_fixed(combined.template.table.test.add.dist$file.name,pattern = '_',n=4)[,4]
#combined.template.table.test.add.dist$time <- str_split_fixed(time.temp,pattern = '.txt',n=2)[,1]
combined.template.table.test.add.dist$time <- substr(time.temp,start=1,stop=4)

# Check structure of resulting data frame
head(combined.template.table.test.add.dist)
table(combined.template.table.test.add.dist$distance.from.source)

# Part 4. Calculate absolute receive levels for each selection --------------------

combined.template.table.test.add.dist$Loc_Name <- 
  as.factor(combined.template.table.test.add.dist$Loc_Name)

# Subset so can focus on one series of playbacks -- NOTE:this is for trouble-shooting only
combined.template.table.test.add.dist <-
  subset(combined.template.table.test.add.dist,
         recorder==68 |recorder==73 | recorder==74 |
           recorder==42 |recorder==112 | recorder==74 | recorder ==49 |
           recorder==84 |recorder==34 | recorder==91 | recorder ==79 |
           recorder==43 |recorder==111 | recorder==73 | recorder ==46 |
           recorder==85 |recorder==36 | recorder==88 | recorder ==80 |
           recorder==68 |recorder==45 | recorder==87 |
           recorder == 'char1' |recorder == 'char2' |recorder == 'char3' )

# This subset contains 3 distance classes in two habitat types
# combined.template.table.test.add.dist <-
#    subset(combined.template.table.test.add.dist,
#             recorder==91 |recorder==88 | recorder==87 |
#             recorder == 'char1'| 
#               recorder==68 |recorder==73 | recorder==74 | recorder == 'char3'  )


# Create an index for each file
file.name.index <- unique(combined.template.table.test.add.dist$Loc_Name)


# Create an empty dataframe to add to iteratively in the loop
BackgroundNoiseRemovedDF <- data.frame()

# The loop to calculate inband power (after subtracting the noise) for each selection from the wave file
for(b in 1:length(file.name.index)){tryCatch({
  
  # Subset by recorder and date index
  combined.playbacks <- subset(combined.template.table.test.add.dist,Loc_Name==file.name.index[b])

  unique.playbacks <- unique(combined.playbacks$recorder)
  
  for(j in 1:length(unique.playbacks)){
  print(paste('processing', j, 'out of',length(unique.playbacks), 'for location', file.name.index[b]))
  singleplayback.df <- subset(combined.playbacks,recorder==unique.playbacks[j])
  
  # Create sound file path
  soundfile <- str_split_fixed(singleplayback.df$file.name[1],pattern = '_',n=2)[,2]
  soundfile <- str_split_fixed(soundfile,pattern = '-',n=2)[,1]
  soundfile.path <- paste(soundfile,'.wav',sep='')
  wav.file.index <- which(Rungan.playbacks.wav.files$name %in% soundfile.path)
  
  if(file.exists(soundfile.path)==FALSE){
  drive_download(as_id(Rungan.playbacks.wav.files$id[wav.file.index]))
  }
  
  # Read in the long .wav file
  wavfile.temp <- tuneR::readWave(Rungan.playbacks.wav.files$name[wav.file.index])
  

  # Check to make sure number of selections matches the template
  #if(nrow(singleplayback.df)==nrow(SelectionIDs)){ 
    
    # Use the Raven selection table to cut each selection into an individual .wav file
    ListofWavs <- 
      lapply(1:nrow(singleplayback.df), function(x) cutw(wavfile.temp, from= (singleplayback.df$Begin.Time..s.[x]- signal.time.buffer),
                                                         to= (singleplayback.df$End.Time..s.[x]+signal.time.buffer), output='Wave'))
    
    
    # Use the Raven selection table to extract a longer duration .wav file for noise
    NoiseWav1 <- cutw(wavfile.temp, from= (singleplayback.df$Begin.Time..s.[1]-timesecs),
                      to=singleplayback.df$Begin.Time..s.[1], output='Wave')
    
    NoiseWav2 <- cutw(wavfile.temp, from= (singleplayback.df$Begin.Time..s.[nrow(singleplayback.df)]),
                      to= (singleplayback.df$Begin.Time..s.[nrow(singleplayback.df)]+timesecs), output='Wave')
    
    NoiseWavList <- list(NoiseWav1,NoiseWav2)
    
    # Matches each selection with the corresponding noise and selection .wav file and calculate absolute receive level
    for(d in 1:nrow(singleplayback.df)){
      
      print(d)
      
      # Subset the correspond row from the selection table
      Selectiontemp <- singleplayback.df[d,]
      
      noise.value.list <- list()
      
      for(e in 1:length(NoiseWavList)){
        
        # Take the corresponding noise file
        NoiseWavetemp <- NoiseWavList[[e]]
        
        # Filter to the frequency range of the selection
        filteredwaveform<- bwfilter(NoiseWavetemp, 
                                    from=Selectiontemp$Low.Freq..Hz., 
                                    to=Selectiontemp$High.Freq..Hz.,
                                    n=3)
        
        # Add the filtered waveform back into the .wav file
        NoiseWavetemp@left <- c(filteredwaveform)
        
        # Assign a new name
        w.dn.filt <- NoiseWavetemp
        
        # Calculate the duration of the sound file
        dur.seconds <- duration(w.dn.filt)
        
        # Divide into evenly spaced bins (duration specified above)
        bin.seq <- seq(from=0, to=dur.seconds, by=noise.subsamples)
        
        # Create a list of all the noise subsample bins to estimate noise
        bin.seq.length <- length(bin.seq)-1
        
        # Create a list of shorter sound files to calculate noise
        subsamps.1sec <- lapply(1:bin.seq.length, function(i) 
          extractWave(w.dn.filt, 
                      from=as.numeric(bin.seq[i]), to=as.numeric(bin.seq[i+1]), 
                      xunit = c("time"),plot=F,output="Wave"))
        
        
        # Calculate noise for each noise time bin 
        noise.list <- list()
        
        for (k in 1:length(subsamps.1sec)) { 
          
          # Read in .wav file 
          temp.wave <- subsamps.1sec[[k]]
          
          # Normalise the values 
          data <- (temp.wave@left) / (2 ^ 16/2)
          
          # Calibrate the data with the microphone sensitivity
          data_cal <- data/ (10^(Sensitivity/20))
          
          # Calculate RMS
          data_rms <- rms(data_cal)
          
          
          noise.list[[k]] <- data_rms
          
        }
        
        # Take the minimum value from the noise samples
        noise.value.list[[e]] <- min(unlist(noise.list))
        
      }
      
      noise.value <- median(unlist(noise.value.list))
      
      # Isolate the corresponding .wav file for the playback selection
      SignalWavtemp <-  ListofWavs[[d]]
      
      # Filter to the frequency range of the selection
      w.dn.filt <- bwfilter(SignalWavtemp, 
                            from=Selectiontemp$Low.Freq..Hz., 
                            to=Selectiontemp$High.Freq..Hz.,n=3)
      
      # Normalise the values 
      data <- w.dn.filt/ (2 ^ 16/2)
      
      # Calibrate the data with the microphone sensitivity
      data_cal <- data/ (10^(Sensitivity/20))
      
      # Calculate RMS
      signal.value <- rms(data_cal)
      
      
      # Calculate absolute receive level of the signal in the selection in dB (subtracting noise)
      Selectiontemp$PowerDb <- 20 * log10((signal.value-noise.value))
      
      # Calculate noise level in dB
      Selectiontemp$NoisevalueDb <- 20 * log10((noise.value))
      
      # Match the signal with the sound type
      Selectiontemp$Sound.Type <-  SelectionIDs[d,]$Sound.Type
      
      # Print the output
      print(Selectiontemp)
      
      # Combine into a dataframe
      BackgroundNoiseRemovedDF <- rbind.data.frame(BackgroundNoiseRemovedDF,Selectiontemp)
      write.csv(BackgroundNoiseRemovedDF,'BackgroundNoiseRemovedDFJune2022Rungan.csv',row.names = F)
    }

    
  
  # Remove file from memory
  unlink(Rungan.playbacks.wav.files$name[wav.file.index])
  rm(wavfile.temp)
  }
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }


# Reference only has one set of playbacks so want to append   
BackgroundNoiseRemovedDF_char1 <-subset(BackgroundNoiseRemovedDF, Loc_Name == 'char1')
BackgroundNoiseRemovedDF_char1 <- rbind.data.frame(BackgroundNoiseRemovedDF_char1,BackgroundNoiseRemovedDF_char1 ,BackgroundNoiseRemovedDF_char1)
BackgroundNoiseRemovedDF_char1$Selection <- SelectionIDs$Selection
BackgroundNoiseRemovedDF_char1$Sound.Type <- SelectionIDs$Sound.Type

BackgroundNoiseRemovedDF_char2 <-subset(BackgroundNoiseRemovedDF, Loc_Name == 'char2')
BackgroundNoiseRemovedDF_char2 <- rbind.data.frame(BackgroundNoiseRemovedDF_char2,BackgroundNoiseRemovedDF_char2 ,BackgroundNoiseRemovedDF_char2)
BackgroundNoiseRemovedDF_char2$Selection <- SelectionIDs$Selection
BackgroundNoiseRemovedDF_char2$Sound.Type <- SelectionIDs$Sound.Type

BackgroundNoiseRemovedDF_char3 <-subset(BackgroundNoiseRemovedDF, Loc_Name == 'char3')
BackgroundNoiseRemovedDF_char3 <- rbind.data.frame(BackgroundNoiseRemovedDF_char3,BackgroundNoiseRemovedDF_char3 ,BackgroundNoiseRemovedDF_char3)
BackgroundNoiseRemovedDF_char3$Selection <- SelectionIDs$Selection
BackgroundNoiseRemovedDF_char3$Sound.Type <- SelectionIDs$Sound.Type

BackgroundNoiseRemovedDF <- subset(BackgroundNoiseRemovedDF,Loc_Name != 'char3')
CombinedDF <-rbind.data.frame(BackgroundNoiseRemovedDF,BackgroundNoiseRemovedDF_char1)

# Part 5. Propagation Loss --------------------------------------------------------

# Create an index with unique date/time combinations
# NOTE: We will need to update this for the full dataset
Loc_Name.index <- 
  unique(CombinedDF$Loc_Name)

# Create dataframe to match characterization units  
char.matching <- data.frame(
  char = c('char1','char2','char3'),
  rec=c('S00976', 'S01143', 'S00974')
)

# Create empty dataframe for propagation loss
observed.prop.loss <- data.frame()

# Loop to calculate propagation loss
for(z in 1:length(Loc_Name.index)) { tryCatch({ 
  
  # Subset data frame to focus on unique date/time
  temp.playback <- subset(CombinedDF, 
                          Loc_Name==Loc_Name.index[z])
  
  # See how many unique playbacks
  unique(temp.playback$distance.from.source)
  
 playback.index <-  which(rungan_data$Loc_Name == unique(temp.playback$Loc_Name))
 
 temp.playback.data <- rungan_data[playback.index[1],]
  
 temp.char.info <- char.matching[which(char.matching$rec== temp.playback.data$ARU_ID),]
  
 TempReferenceDF <- subset(CombinedDF,Loc_Name== temp.char.info$char)
 
 # Create an index for each unique file in the playback
  file.index <- unique(temp.playback$distance.from.source)
  SelectionIndex <- (SelectionIDs$Sound.Type)
  
  temp.playback$distance.from.source <- as.character(temp.playback$distance.from.source)
  
  # This isolates each selection in the original template one by one
  for(a in 1:length(SelectionIndex)){
    print(a)
    # Subset the same selection from each of the recorders
    small.sample.playback.test <- data.frame()
    for(b in 1:length(file.index) ){
      temp.table <- subset(temp.playback,distance.from.source==file.index[b])
      temp.table$Sound.Type <- SelectionIDs$Sound.Type
      temp.table <- temp.table[a,]
      small.sample.playback.test <- 
        rbind.data.frame(small.sample.playback.test,
                                                     temp.table )
    }
    
    #Append reference
    small.sample.playback.test <- rbind.data.frame(small.sample.playback.test,
                                                   TempReferenceDF[a,])
    
    # Reorder based on distance from speaker
    small.sample.playback.test <-  arrange(small.sample.playback.test, distance.from.source)  
    
    
    # Create an index for each unique recorder in the new subset dataset
    recorder.index.test <- unique(small.sample.playback.test$distance.from.source)
    
    # Create a new column with receive levels standardized so the closest recorder is 0
    small.sample.playback.test$PowerDb.zero <- 
      small.sample.playback.test$PowerDb[1]-small.sample.playback.test$PowerDb
    
    small.sample.playback.test$distance.from.source
    
    # Loop to calculate propagation loss
    for(c in 1: (length(recorder.index.test)-1)){tryCatch({ 
      
      # Isolate the recorder that we will use to estimate receive levels
      temp.recorder.received <- subset(small.sample.playback.test,distance.from.source==recorder.index.test[c])
      
      # Isolate the recorder we consider as the 'source' for our relative calculations
      temp.recorder.source <- subset(small.sample.playback.test,distance.from.source==recorder.index.test[c-1])
      
      # Based on our distance matrix above calculate the distance between the two recorders
      distance.from.source <- as.numeric(temp.recorder.received$distance.from.source) - as.numeric(temp.recorder.source$distance.from.source)
        
      # Assign the actual receive level (not zeroed) to new variable
      actual.receive.level <- temp.recorder.received$PowerDb
      
      # Assign zeroed receive level to new variable 
      zero.receive.level <- temp.recorder.received$PowerDb.zero
      
      # Assign 'source' level to new variable
      source.level <- temp.recorder.source$PowerDb.zero
      
      # Assign distance to new variable
      distance <- as.numeric(distance.from.source)
      
      # Assign noise level estimate to new variable
      noise.level <- temp.recorder.received$NoisevalueDb
      
      # Calculate the distance ratio for propagation loss equation
      dist.ratio <- log10(distance/10)
      
      # Calculate the 'magic x'
      magic.x <-  zero.receive.level/dist.ratio
      
      # Calculate excess attenuation
      
      excess.attenuation <- magic.x -6
      
      # Assign sound type to new variable
      Sound.type <- temp.recorder.received$Sound.Type
      
      # Assign time  to new variable
      time <- temp.recorder.received$time
      
      # Assign date to new variable
      date <- temp.recorder.received$date
      
      pb.id <- Loc_Name.index[z]
      # Combine all into a new temp dataframe
      temp.df <- cbind.data.frame(zero.receive.level,actual.receive.level,source.level,distance,Sound.type,time,date,magic.x,excess.attenuation,noise.level,
                                  pb.id)
      
      # Combine all observations into one dataframe
      observed.prop.loss <- rbind.data.frame(observed.prop.loss,temp.df)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    
  }
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

unique(observed.prop.loss$Sound.type)

(observed.prop.loss$excess.attenuation)
mean(observed.prop.loss$magic.x)


observed.prop.loss$sound.type.for.median <- str_split_fixed (observed.prop.loss$Sound.type, pattern = '_',n=2)[,2]

# Isolate sound category names
observed.prop.loss$Sound.category <- 
  str_split_fixed(observed.prop.loss$Sound.type, pattern = '_',n=3)[,2]


observed.prop.loss.median.df <- data.frame()

pb.id.index <- unique(observed.prop.loss$pb.id)

for(a in 1:length(pb.id.index)){
  temp.observed <-  subset(observed.prop.loss,pb.id==pb.id.index[a])
  
  sound.type.index <- 
    unique(temp.observed$sound.type.for.median  )
  
  distance.index <- unique(temp.observed$distance  )
  
  for(b in 1:length(sound.type.index)){
    
    temp.subset <- subset(temp.observed,sound.type.for.median==sound.type.index[b])
    
    for(c in 1:length(distance.index)){
      
      temp.subset.distance <-  subset(temp.subset,distance==distance.index[c])
      temp.subset.distance <- na.omit(temp.subset.distance)
      median.x <- median(temp.subset.distance$magic.x) 
      
      temp.subset.distance <- temp.subset.distance[1,c("distance", "Sound.type", "time", "date", "magic.x", "noise.level", 
                                                       "pb.id", "sound.type.for.median", "Sound.category")]
      
      temp.subset.distance$median.x <- median.x
      
      temp.subset.distance$pb.id <- pb.id.index[a]
      observed.prop.loss.median.df <- rbind.data.frame(observed.prop.loss.median.df,temp.subset.distance)
    }
  }
  
}


head(observed.prop.loss)
# Subset so we focus on primates
observed.prop.loss.subset <- subset(observed.prop.loss,
                                    Sound.category=="Hfunstart" |Sound.category=="Hfuntrill" 
                                    |Sound.category=="Halbpeak"
                                    |Sound.category=="Halbend"
                                    |Sound.category=="Pmor"|Sound.category=="PwurP"|Sound.category=="PwurS") #

# Give more informative names
observed.prop.loss.subset$Sound.category <- 
  plyr::revalue(observed.prop.loss.subset$Sound.category,
                c(Halbpeak= "Gibbon C. Kali Peak",Halbend= "Gibbon C. Kali End",
                  Hfunstart = "Gibbon Sabah Intro",
                  Hfuntrill = "Gibbon Sabah Trill",
                  Pmor="Orangutan Sabah", 
                  PwurP="Orangutan C. Kali Pulse",
                  PwurS="Orangutan C. Kali Sigh"))

# Plot observed change in receive level by distance
observed.prop.loss.subset.site1 <-subset(observed.prop.loss.subset,pb.id =='5 (S4A07578)')
site1.plot <- ggpubr::ggscatter(data = observed.prop.loss.subset.site1,x='distance', 
                  y='actual.receive.level',
                  color='Sound.category',
                  facet.by = 'Sound.category',xlim=c(0,500), #ylim=c(-70,-20),
                  add = c("loess"),title='3 (S4A07867)')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")

site1.plot 

# Plot observed change in receive level by distance
observed.prop.loss.subset.site2 <- subset(observed.prop.loss.subset,pb.id =='7 (S4A07360)')
site2.plot <- ggpubr::ggscatter(data = observed.prop.loss.subset.site2,x='distance', 
                  y='actual.receive.level',
                  color='Sound.category',
                  facet.by = 'Sound.category',xlim=c(0,500), #ylim=c(-70,-20),
                  add = c("loess"),title='7 (S4A07360)')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")

cowplot::plot_grid(site1.plot,site2.plot)

observed.prop.loss.subset$distance <- as.factor(observed.prop.loss.subset$distance)
ggboxplot(data=observed.prop.loss.subset,x='Sound.category',y='magic.x',
          fill='distance')


# Plot observed change in receive level by distance for all signals
ggpubr::ggscatter(data = observed.prop.loss,x='distance', y='actual.receive.level',
                  color='Sound.type',
                  facet.by = 'Sound.type',xlim=c(0,500), #ylim=c(-70,-20),
                  add = c("loess"),title='')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")


# Part 6.  Observed results relative to spherical and cylindrical  --------
hist(observed.prop.loss.subset$magic.x)

# We can subset by sound category- here it is by "Hfunstart"
playback.line.1 <- median(na.omit(subset(observed.prop.loss,
                                         Sound.category=="Halbstart")$magic.x))

# Or we can combine all of our data
#playback.line.1 <- median(na.omit(observed.prop.loss$magic.x))

# Set the equations for observed, spherical and cylindrical spreading
eq1 <- function(x){ playback.line.1*log10(x)}
eq2 <- function(x){ -20*log10(x)}
eq3 <- function(x){ -10*log10(x)}

# Create a series of points based on the above equations
Estimated1 <- cbind.data.frame(seq(1:500),eq1(1:500),rep('Estimated',500))
colnames(Estimated1) <- c("X","Value","Label")
Spherical <- cbind.data.frame(seq(1:500),eq2(1:500),rep('Spherical',500))
colnames(Spherical) <- c("X","Value","Label")
Cylindrical <-  cbind.data.frame(seq(1:500),eq3(1:500),rep('Cylindrical',500))
colnames(Cylindrical) <- c("X","Value","Label")

# Combine all three into a single dataframe
attenuation.df <- rbind.data.frame(Estimated1,Spherical,Cylindrical)

# Plot the results
ggplot(data = attenuation.df,aes(x=X, y=Value,group=Label, colour=Label,linetype=Label))+
  geom_line() +theme_bw() + scale_color_manual(values = c("black","red","darkgray"))+
  theme(legend.title = element_blank())+ 
  scale_linetype_manual(values=c( "solid","twodash", "dotted"))+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))+
  xlab("Distance from source (m)") + ylab("Amplitude (dB)")+
  ylim(-60,0)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


# Plot magic x by distance
observed.prop.loss.x.dist <- subset(observed.prop.loss,
                                    Sound.category=="Hfuntrill")

plot(observed.prop.loss.x.dist$magic.x ~ observed.prop.loss.x.dist$distance,
     xlab='Distance (m)', ylab='Magic x')

plot(observed.prop.loss.x.dist$noise.level ~ observed.prop.loss.x.dist$distance,
     xlab='Distance (m)', ylab='Noise')

ggpubr::ggscatter(data = observed.prop.loss.x.dist,x='distance', y='actual.receive.level',
                  color='Sound.type',
                  #xlim=c(0,300), #ylim=c(-70,-20),
                  title='')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")


ggpubr::ggscatter(data = observed.prop.loss.x.dist,x='distance', y='actual.receive.level',
                  color='Sound.type',
                  #xlim=c(0,300), #ylim=c(-70,-20),
                  title='')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")


ggpubr::ggboxplot(data=observed.prop.loss.subset,
                  x='Sound.category',y='noise.level')


