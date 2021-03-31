# This is the R code to estimate propagation loss in Borneo 


# Version summary ---------------------------------------------------------
# Version 2. Add adaptive noise estimates
# Version 3. Modify noise calculations so divide into ten 1-sec bins


# Part 1. Load necessary packages -------------------------------------------------------------
library(seewave)
library(tuneR)
library(stringr)
library(plyr)
library(ggpubr)

# Part 2. Set up data -------------------------------------------------------------

# Playback template table
SelectionIDs <- read.delim("/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/SelectionLabels_S00974_20190811_101922_updated.txt")

# Sound file location
SoundFiles.input <- 
  '/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/Background Noise Test/SoundFiles/'

# Selection table location
input.dir <- 
  "/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/Background Noise Test/SelectionTables"

# Read in GPS data
recorder.gps <- plotKML::readGPX("/Users/denaclink/Downloads/MB Playbacks 50 m.GPX") 

# Set duration of the noise selection before the start of the actual selection
timesecs <- 10

# Set a buffer duration before and after selection to calculate inband power
# NOTE: Right now it is set to zero so it cuts the selection right at the boundaries of the selection table
signal.time.buffer <- 0

# Set the duration of the noise subsamples.
noise.subsamples <- 0.10

# Set microphone gain
gain <- 40

# Calculate the microphone sensitivity for the system
Sensitivity <- -137.9794 + gain - 0.9151 # dB relative to 1 volt per pascal

# Set distance of first recorder to playback speaker (in meters)
dist.to.playback <- 17.21

# Read in selection tables with full file path names
selection.tables <- 
  list.files(input.dir,full.names = T)

# Read in selection tables with short file path names for adding name column
selection.tables.short <-
  list.files(input.dir,full.names = F)

# Create an index for recorder
list.of.recorders <- str_split_fixed(selection.tables.short, pattern = '_',3)[,1]

# Read in selection tables and combine with file name
combined.template.table.test <- data.frame()

for(a in 1:length(selection.tables)){
  
  template.table.temp <- read.delim(selection.tables[a],fill = T,header=T,stringsAsFactors = F)
  
  recorder <- list.of.recorders[a]
  file.name <- selection.tables.short[a]
  file.name <- str_split_fixed(file.name,pattern = '.txt',n=2)[,1]
  
  template.table.temp <- cbind.data.frame(template.table.temp,recorder,file.name)
  
  
  combined.template.table.test <- rbind.data.frame(combined.template.table.test,template.table.temp)
}

# Check output to make sure it looks right
head(combined.template.table.test)

# Part 3. Create distance matrix based on GPS points ---------------------------------------------------

# Convert name so that it matches dataframe
recorder.gps$waypoints$name <- str_remove(recorder.gps$waypoints$name, '0')

# Subset only necessary columns from GPS data
small.gps.df <- recorder.gps$waypoints[,c('lon','lat','name')]
colnames(small.gps.df) <- c('lon','lat','recorder')

# Combine into a distance matrix
xy.coords <- cbind(c(small.gps.df$lon), 
                   c(small.gps.df$lat))

dist.mat <- distm( xy.coords, fun = distHaversine)

# Add recorder names to distance matrix
colnames(dist.mat) <- c(as.character(small.gps.df$recorder))
rownames(dist.mat) <- c(as.character(small.gps.df$recorder))

# Check output
dist.mat


# Combine all into a single dataframe
BackgroundNoiseRemovedDF.test <- merge(combined.template.table.test,small.gps.df,by='recorder')

# Add a date column
BackgroundNoiseRemovedDF.test$date <- str_split_fixed(BackgroundNoiseRemovedDF.test$file.name,pattern = '_',n=3)[,2]

# Add a time column
time.temp <- str_split_fixed(BackgroundNoiseRemovedDF.test$file.name,pattern = '_',n=3)[,3]
BackgroundNoiseRemovedDF.test$time <- str_split_fixed(time.temp,pattern = '.txt',n=2)[,1]
BackgroundNoiseRemovedDF.test$time <- substr(BackgroundNoiseRemovedDF.test$time,start=1,stop=4)

# Check structure of resulting data frame
head(BackgroundNoiseRemovedDF.test)


# Part 4. Calculate absolute receive levels for each selection --------------------

# Create an index for each file
file.name.index <- unique(BackgroundNoiseRemovedDF.test$file.name)

# Create an empty dataframe to add to iteratively in the loop
BackgroundNoiseRemovedDF <- data.frame()

# The loop to calculate inband power (after subtracting the noise) for each selection from the wave file
for(b in 1:length(file.name.index)){
  # Subset by recorder and date index
  singleplayback.df <- subset(BackgroundNoiseRemovedDF.test,file.name==file.name.index[b])
  
  # Create sound file path
  soundfile.path <- paste(SoundFiles.input,singleplayback.df$file.name[1],'.wav',sep='')
  
  # Read in the long .wav file
  wavfile.temp <- tuneR::readWave(soundfile.path)
  
  # Downsample so that comparable with C. Kalimantan recordings
  wavfile.temp <- tuneR::downsample(wavfile.temp,16000)
  
  # Check to make sure number of selections matches the template
  if(nrow(singleplayback.df)==nrow(SelectionIDs)){ 
    
  # Use the Raven selection table to cut each selection into an individual .wav file
  ListofWavs <- 
    lapply(1:nrow(singleplayback.df), function(x) cutw(wavfile.temp, from= (singleplayback.df$Begin.Time..s.[x]- signal.time.buffer),
                                                                 to= (singleplayback.df$End.Time..s.[x]+signal.time.buffer), output='Wave'))
  
  
  # Use the Raven selection table to extract a longer duration .wav file for noise
  ListofNoiseWavs <- 
    lapply(1:nrow(singleplayback.df), function(x) cutw(wavfile.temp, from= (singleplayback.df$Begin.Time..s.[x]-timesecs),
                                                                   to=singleplayback.df$Begin.Time..s.[x], output='Wave'))
  
  # Matches each selection with the corresponding noise and selection .wav file and calculate absolute receive level
  for(d in 1:nrow(singleplayback.df)){
   
    # Focus on a single selection
    Selectiontemp <-  SelectionIDs[d,]

    # Take the correspond noise file
    NoiseWavetemp <- ListofNoiseWavs[[d]]
   
   # Filter to the frequency range of the selection
   filteredwaveform<- bwfilter(NoiseWavetemp, from=Selectiontemp$Low.Freq..Hz., to=Selectiontemp$High.Freq..Hz.)
   
   # Add the filtered waveform back into the .wav file
   NoiseWavetemp@left <- c(filteredwaveform)
   
   # Assign a new name
   w.dn.filt <- NoiseWavetemp
   
   # Calculate the duration of ths sound file
   dur.seconds <- duration(w.dn.filt)
   
   # Divide into evenly spaced bins (duration specified above)
   bin.seq <- seq(from=0, to=dur.seconds, by=noise.subsamples)
   
   # Create a list of all the 1-s bins to estimate noise
   bin.seq.length <- length(bin.seq)-1
   
   # Create a list of shorter sound files to calculate noise
   subsamps.1sec <- lapply(1:bin.seq.length, function(i) 
     extractWave(w.dn.filt, from=as.numeric(bin.seq[i]), to=as.numeric(bin.seq[i+1]), 
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
   noise.value <- min(unlist(noise.list))
 
   # Isolate the corresponding .wav file for the playback selection
   SignalWavtemp <-  ListofWavs[[d]]
      
   # Filter to the frequency range of the selection
     w.dn.filt <- bwfilter(SignalWavtemp, from=Selectiontemp$Low.Freq..Hz., to=Selectiontemp$High.Freq..Hz.)
     
    
     # Normalise the values 
      data <- w.dn.filt/ 2 ^ 15
      
      # Calibrate the data with the microphone sensitivity
      data_cal <- data/ (10^(Sensitivity/20))
      
      # Calculate RMS
      signal.value <- rms(data_cal)
      
      # Subset the correspond row from the selection table
      Selectiontemp <- singleplayback.df[d,]
      
      # Calculate absolute receive level in dB
      Selectiontemp$PowerDb <- 20 * log10((signal.value-noise.value))
      
      # Calculate noise level in dB
      Selectiontemp$NoisevalueDb <- 20 * log10((noise.value))
      
      # Print the output
      print(Selectiontemp)
      
      # Combine into a dataframe
      BackgroundNoiseRemovedDF <- rbind.data.frame(BackgroundNoiseRemovedDF,Selectiontemp)
  }
  
  }
}


# Part 5. Propagation Loss --------------------------------------------------------

# Create an index with unique date/time combinations
date.time.combo <- paste(BackgroundNoiseRemovedDF$date,BackgroundNoiseRemovedDF$time,sep='_')
unique.date.time.combo <- unique(date.time.combo)

# Create empty dataframe for propagation loss
observed.prop.loss <- data.frame()

# Loop to calculate propagation loss
for(z in 1:length(unique.date.time.combo)) { tryCatch({ # Ignore the 25 m spacing
  
  # Subset by unique date/time index
  temp.date.time.subset <- str_split_fixed(unique.date.time.combo[z],pattern = '_',n=2) 
  
  # Subset data frame to focus on unique date/time
  temp.playback <- subset(BackgroundNoiseRemovedDF, date==temp.date.time.subset[,1] & time==temp.date.time.subset[,2])
  
  # See how many unique playbacks
  unique(temp.playback$file.name)
  
  # Create an index for each unique file in the playback
  file.index <- unique(temp.playback$file.name)
  
  # This isolates each selection in the original template one by one
  for(a in 1:102){
    
    # Subset the same selection from each of the recorders
    small.sample.playback.test <- data.frame()
    for(b in 1:length(file.index) ){
      temp.table <- subset(temp.playback,file.name==file.index[b])
      temp.table$Sound.Type <- SelectionIDs$Sound.Type
      temp.table <- temp.table[c(a),]
      small.sample.playback.test <- rbind.data.frame(small.sample.playback.test,temp.table )
    }
    
    # Create an index indicating which recorder and which selection
    small.sample.playback.test$selection.index <- 
      paste(small.sample.playback.test$recorder,small.sample.playback.test$Selection,sep='_')
    
    # Create an index for each unique recorder in the new subset dataset
    recorder.index.test <- unique(small.sample.playback.test$recorder)
    
    # Create a new column with receive levels standardized so the closest recorder is 0
    small.sample.playback.test$PowerDb.zero <- 
      small.sample.playback.test$PowerDb-small.sample.playback.test$PowerDb[1]
    
    # Loop to calculate propagation loss; note the index starts at 2 since we use the closest one as the reference
    for(c in 2:length(recorder.index.test)){
      
      # Isolate the recorder that we will use to estimate receive levels
      temp.recorder.received <- subset(small.sample.playback.test,recorder==recorder.index.test[c])
      
      # Isolate the recorder we consider as the 'source' for our relative calculations
      temp.recorder.source <- subset(small.sample.playback.test,recorder==recorder.index.test[c-1])
      
      # Based on our distance matrix above calculate the distance between the two recorders
      distance.from.source <- dist.mat[c(temp.recorder.received$recorder),c(small.sample.playback.test$recorder[1])]
      
      # Assign the actual receive level (not zeroed) to new variable
      actual.receive.level <- temp.recorder.received$PowerDb
      
      # Assign zeroed receive level to new variable 
      zero.receive.level <- temp.recorder.received$PowerDb.zero
      
      # Assign 'source' level to new variable
      source.level <- temp.recorder.source$PowerDb.zero
      
      # Assign distance to new variable
      distance <- distance.from.source
      
      # Assign noise level estimate to new variable
      noise.level <- temp.recorder.received$NoisevalueDb
      
      # Calculate the distance ratio for propagation loss equation
      dist.ratio <- log10(distance/dist.to.playback)
      
      # Calculate the 'magic x'
      magic.x <-  zero.receive.level/dist.ratio
      
      # Assign sound type to new variable
      Sound.type <- temp.recorder.received$Sound.Type
      
      # Assign time  to new variable
      time <- temp.recorder.received$time
      
      # Assign date to new variable
      date <- temp.recorder.received$date
      
      # Combine all into a new temp dataframe
      temp.df <- cbind.data.frame(zero.receive.level,actual.receive.level,source.level,distance,Sound.type,time,date,magic.x,noise.level)
      
      # Combine all observations into one dataframe
      observed.prop.loss <- rbind.data.frame(observed.prop.loss,temp.df)
      
    }
    
  }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

# Isolate sound category names
observed.prop.loss$Sound.category <- str_split_fixed(observed.prop.loss$Sound.type, pattern = '_',n=3)[,2]

# Subset so we focus on primates
observed.prop.loss.subset <- subset(observed.prop.loss,
                                    Sound.category=="Hfunstart" |Sound.category=="Hfuntrill" |Sound.category=="Halb"
                                    |Sound.category=="Pmor"|Sound.category=="Pwur") #
# Give more informative names
observed.prop.loss.subset$Sound.category <- 
  plyr::revalue(observed.prop.loss.subset$Sound.category,
                c(Hfunstart = "Gibbon Sabah Intro",
 Hfuntrill = "Gibbon Sabah Trill",Pmor="Orangutan Sabah", 
 Halb="Gibbon C. Kali",Pwur="Orangutan C. Kali"))

# Plot observed change in receive level by distance
ggpubr::ggscatter(data = observed.prop.loss.subset,x='distance', y='actual.receive.level',
                  color='Sound.category',
                  facet.by = 'Sound.category',xlim=c(0,300), #ylim=c(-70,-20),
                  add = c("loess"),title='')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")

# Plot observed change in receive level by distance for all signals
ggpubr::ggscatter(data = observed.prop.loss,x='distance', y='actual.receive.level',
                  color='Sound.category',
                  facet.by = 'Sound.category',xlim=c(0,300), #ylim=c(-70,-20),
                  add = c("loess"),title='')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")


# Part 6.  Observed results relative to spherical and cylindrical  --------

# We can subset by sound category- here it is by "Hfunstart"
playback.line.1 <- median(na.omit(subset(observed.prop.loss,
                                     Sound.category=="Hfunstart")$magic.x))

# Or we can combine all of our data
playback.line.1 <- median(na.omit(observed.prop.loss$magic.x))
  
# Set the equations for observed, spherical and cylindrical spreading
eq1 <- function(x){ playback.line.1*log10(x)}
eq2 <- function(x){ -10*log10(x)}
eq3 <- function(x){ -20*log10(x)}

# Create a series of points based on the above equations
Estimated1 <- cbind.data.frame(seq(1:1000),eq1(1:1000),rep('Estimated',1000))
colnames(Estimated1) <- c("X","Value","Label")
Spherical <- cbind.data.frame(seq(1:1000),eq2(1:1000),rep('Spherical',1000))
colnames(Spherical) <- c("X","Value","Label")
Cylindrical <-  cbind.data.frame(seq(1:1000),eq3(1:1000),rep('Cylindrical',1000))
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
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())





