# Version 2. Add adaptive noise estimates

# Load necessary packages
library(seewave)
library(tuneR)


# Set up data -------------------------------------------------------------

# Playback template table
SelectionIDs <- read.delim("/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/SelectionLabels_S00974_20190811_101922_updated.txt")


# Sound file location
SoundFiles.input <- 
  '/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/Background Noise Test/SoundFiles/'

# Selection table location
input.dir <- 
  "/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/Background Noise Test/SelectionTables"

# Set duration of noise selection
timesecs <- median(SelectionIDs$Delta.Time..s.)*5

# Set duration before and after selection to calculate inband power
signal.time.buffer <- 1
  
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


# Read in GPS locations ---------------------------------------------------

# Read in datasheet
recorder.gps <- plotKML::readGPX("/Users/denaclink/Downloads/MB Playbacks 50 m.GPX") 

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
BackgroundNoiseRemovedDF.test$date <- str_split_fixed(BackgroundNoiseRemovedDF.test$file.name,pattern = '_',n=3)[,2]
time.temp <- str_split_fixed(BackgroundNoiseRemovedDF.test$file.name,pattern = '_',n=3)[,3]
BackgroundNoiseRemovedDF.test$time <- str_split_fixed(time.temp,pattern = '.txt',n=2)[,1]
BackgroundNoiseRemovedDF.test$time <- substr(BackgroundNoiseRemovedDF.test$time,start=1,stop=4)

# Check structure of resulting data frame
head(BackgroundNoiseRemovedDF.test)

# Create an index 
file.name.index <- unique(BackgroundNoiseRemovedDF.test$file.name)

# Create an empty dataframe to add to iteratively in the loop
BackgroundNoiseRemovedDF <- data.frame()

# The loop to calculate inband power (after subtracting the noise) for each selection from the wave file
for(b in 1:length(file.name.index)){
  singleplayback.df <- subset(BackgroundNoiseRemovedDF.test,file.name==file.name.index[b])
  soundfile.path <- paste(SoundFiles.input,singleplayback.df$file.name[1],'.wav',sep='')
  wavfile.temp <- tuneR::readWave(soundfile.path)
  
  # Downsample so that comparable with C. Kalimantan recordings
  wavfile.temp <- tuneR::downsample(wavfile.temp,16000)
  
  # Check to make sure templates match
  if(nrow(singleplayback.df)==nrow(SelectionIDs)){ 
    
  # We can use the Raven selection table to cut each selection into an individual .wav file
  ListofWavs <- 
    lapply(1:nrow(singleplayback.df), function(x) cutw(wavfile.temp, from= (singleplayback.df$Begin.Time..s.[x]- signal.time.buffer),
                                                                 to= (singleplayback.df$End.Time..s.[x]+signal.time.buffer), output='Wave'))
  
  
  
  # Now we take a larger selection to estimate background noise
  ListofNoiseWavs <- 
    lapply(1:nrow(singleplayback.df), function(x) cutw(wavfile.temp, from= (singleplayback.df$Begin.Time..s.[x]-timesecs),
                                                                   to=singleplayback.df$Begin.Time..s.[x], output='Wave'))
  
  
    
  #pdf( paste( singleplayback.df$file.name[1],'noisedetections.pdf'))
  #par(mfrow = c(4, 4))
  # An even larger selection to plot spectrograms to show location of noise 
  # ListofNoiseWavsForSpectrogram <- 
  #   lapply(1:nrow(singleplayback.df), function(x) cutw(wavfile.temp, from= (singleplayback.df$Begin.Time..s.[x]-timesecs),
  #                                                      to=singleplayback.df$End.Time..s.[x], output='Wave'))
  
  
  # This loop matches each selection with the corresponding noise and selection .wav file
  for(d in 1:nrow(singleplayback.df)){
   Selectiontemp <-  SelectionIDs[d,]

   NoiseWavetemp <- ListofNoiseWavs[[d]]
   
   # Filter to the frequency range of the selection
   w.dn.filt <- fir(NoiseWavetemp, 
                    from=Selectiontemp$Low.Freq..Hz., 
                    to=Selectiontemp$High.Freq..Hz.,output = "Wave") 
   w.dn.filt <- tuneR::normalize(w.dn.filt, unit="16")
   
   # Calculate the duration
   dur.seconds <- duration(w.dn.filt)
   
   # Divide into evenly spaced 1-sec bins 
   bin.seq <- seq(from=0, to=dur.seconds, by=1)
   length(bin.seq)
   
   # Create a list of all the 1-s bins for SNR analysis
   bin.seq.length <- length(bin.seq)-1
   subsamps.1sec <- lapply(1:bin.seq.length, function(i) 
     extractWave(w.dn.filt, from=as.numeric(bin.seq[i]), to=as.numeric(bin.seq[i+1]), 
                 xunit = c("time"),plot=F,output="Wave"))
   
   # Calculate noise for each 1 sec bin 
   noise.list <- list()
   for (k in 1:length(subsamps.1sec)) { 
     
     # Read in .wav file 
     wav <- subsamps.1sec[[k]]
     
     bin.seq.smaller <- seq(from=0, to=16000, by=160)
     bin.seq.length.small <- length(bin.seq.smaller)-1
     subsamps.short <- lapply(1:bin.seq.length.small, function(i) 
       extractWave(wav, from=as.numeric(bin.seq.smaller[i]), 
                   to=as.numeric(bin.seq.smaller[i+1]), xunit = c("samples"),
                   plot=F,output="Wave"))
     
     # Calculate sum of squares for each 10 ms long sample
     sum.square.list <- list ()
     
     for (f in 1:length(subsamps.short)){
       wav.temp <- subsamps.short[[f]]
       sum.square.list[[f]] <- sum(wav.temp@left^2)
     }
     
     #Find median value for each 10 ms clip
     noise.list[[k]] <- median(unlist(sum.square.list))
     
   }
   
   ## Now have a list with 5 values corresponding to median value for each 10 ms subsample
   ## that are candidate noise; use the 10th percentile of the distribution for noise estimate
   #noise.value <- quantile(unlist(noise.list), c(.10))
   noise.value <- min(unlist(noise.list))
   noise.value
   
   Noisevalindex <- which.min(abs(unlist(sum.square.list)-noise.value))
   Noiseloc <- bin.seq.smaller[Noisevalindex]
  

# Spectrogram code start --------------------------------------------------
   # zcolors = colorRampPalette (c('dark blue','blue','cyan','light green','yellow',
   #                               'orange','red', 'brown'))
   # dynamicrange = 30
   # zrange = c(dynamicrange,0)
   # nlevels = abs (zrange[1] - zrange[2]) * 1.2
   # 
   # levels = pretty(zrange, nlevels)
   # zcolors = zcolors(length(levels) - 1)
   # 
   # short.wav <-  ListofNoiseWavsForSpectrogram[[d]] 
   # Fs <-short.wav@samp.rate
   # step <- trunc(5*Fs/1000)             # one spectral slice every 5 ms
   # window <- trunc(20*Fs/1000)          # 40 ms data window
   # fftn <- 2^ceiling(log2(abs(window))) # next highest power of 2
   # spg <- specgram(short.wav@left, fftn, Fs, window, window-step)
   # S <- abs(spg$S[2:(fftn*8000/Fs),])   # magnitude in range 0<f<=4000 Hz.
   # S <- S/max(S)         # normalize magnitude so that max is 0 dB.
   # 
   # Sdb <- 20*log10(S) # Convert to dB
   # 
   # Sdb[which(Sdb < (-1 * dynamicrange))] = -1 * dynamicrange
   # 
   # range01 <- function(x){(x-min(x))/(max(x)-min(x))}
   # 
   # Noisevalindex <- which.min(abs(unlist(sum.square.list)-noise.value))
   # Noiseloc <-  range01(bin.seq.smaller)[Noisevalindex]
   # 
   # image(t(Sdb),col=zcolors, axes = FALSE,useRaster = TRUE,xlab='Time (s)',ylab='Frequency (Hz)',
   #       main=Selectiontemp$Sound.Type)
   # segments(Noiseloc,0,Noiseloc,8000,col='white')
   #   
# Spectrogram code finish --------------------------------------------------
   
   
   SignalWavtemp <-  ListofWavs[[d]]
      
   # Filter to the frequency range of the selection
      w.dn.filt <- fir(SignalWavtemp, 
                       from=Selectiontemp$Low.Freq..Hz., 
                       to=Selectiontemp$High.Freq..Hz.,output = "Wave") 
      w.dn.filt <- tuneR::normalize(w.dn.filt, unit="16")
      
      
      # Calculate the duration
      dur.seconds <- duration(w.dn.filt)
      
      # Divide into evenly spaced 1-sec bins 
      bin.seq <- seq(from=0, to=dur.seconds, by=1)
      length(bin.seq)
      
      if(length(bin.seq) > 1){
      # Create a list of all the 1-s bins for SNR analysis
      bin.seq.length <- length(bin.seq)-1
      subsamps.1sec <- lapply(1:bin.seq.length, function(i) 
        extractWave(w.dn.filt, from=as.numeric(bin.seq[i]), to=as.numeric(bin.seq[i+1]), 
                    xunit = c("time"),plot=F,output="Wave"))
      }else{
        subsamps.1sec <- list(w.dn.filt)
      }
      
      # Calculate signal for each 1 sec bin for the selection
      signal.list <- list()
      for (k in 1:length(subsamps.1sec)) { 
        
        # Read in .wav file 
        wav <- subsamps.1sec[[k]]
        
        bin.seq.smaller <- seq(from=0, to=16000, by=160)
        bin.seq.length.small <- length(bin.seq.smaller)-1
        subsamps.short <- lapply(1:bin.seq.length.small, function(i) 
          extractWave(wav, from=as.numeric(bin.seq.smaller[i]), 
                      to=as.numeric(bin.seq.smaller[i+1]), xunit = c("samples"),
                      plot=F,output="Wave"))
        
        # Calculate sum of squares for each 10 ms long sample
        sum.square.list <- list ()
        
        for (f in 1:length(subsamps.short)){
          wav.temp <- subsamps.short[[f]]
          sum.square.list[[f]] <- sum(wav.temp@left^2)
        }
        
        #Find median value for each 10 ms clip
        signal.list[[k]] <- median(unlist(sum.square.list))
        
      }
      
      ## Now have a list with values corresponding to median value for each 10 ms subsample
      ## that are candidate signal; use the 75th percentile of the distribution for signal estimate
      signal.value <- quantile(unlist(signal.list), c(.95))
      signal.value
      
      Selectiontemp <- singleplayback.df[d,]
      Selectiontemp$PowerDb <- 10 * log10((signal.value-noise.value))
      print(Selectiontemp)
      BackgroundNoiseRemovedDF <- rbind.data.frame(BackgroundNoiseRemovedDF,Selectiontemp)
  }
  
  #graphics.off()
  }
}

BackgroundNoiseRemovedDF <- na.omit(BackgroundNoiseRemovedDF)

cor(BackgroundNoiseRemovedDF$PowerDb,BackgroundNoiseRemovedDF$Inband.Power..dB.FS.)

# Propagation Loss --------------------------------------------------------

date.time.combo <- paste(BackgroundNoiseRemovedDF$date,BackgroundNoiseRemovedDF$time,sep='_')
unique.date.time.combo <- unique(date.time.combo)

observed.prop.loss <- data.frame()

for(z in 1:length(unique.date.time.combo)){#tryCatch({ # Ignore the 25 m spacing
  
  temp.date.time.subset <- str_split_fixed(unique.date.time.combo[z],pattern = '_',n=2) 
  
  temp.playback <- subset(BackgroundNoiseRemovedDF, date==temp.date.time.subset[,1] & time==temp.date.time.subset[,2])
  
  # See how many unique playbacks
  unique(temp.playback$file.name)
  
  file.index <- unique(temp.playback$file.name)
  
  for(a in 1:34){
    
    small.sample.playback.test <- data.frame()
    for(b in 1:length(file.index) ){
      temp.table <- subset(temp.playback,file.name==file.index[b])
      temp.table$Sound.Type <- SelectionIDs$Sound.Type
      temp.table <- temp.table[c(a),]
      small.sample.playback.test <- rbind.data.frame(small.sample.playback.test,temp.table )
    }
    
    
    small.sample.playback.test$selection.index <- paste(small.sample.playback.test$recorder,small.sample.playback.test$Selection,sep='_')
    
    selection.index.identifier <- unique(small.sample.playback.test$selection.index)
    
    recorder.index.test <- unique(small.sample.playback.test$recorder)
    
    # Move to zero
    small.sample.playback.test$Inband.Power..dB.FS. <- small.sample.playback.test$Inband.Power..dB.FS.-small.sample.playback.test$Inband.Power..dB.FS.[1]
    
    
    for(c in 2:length(recorder.index.test)){
      
      temp.recorder.received <- subset(small.sample.playback.test,recorder==recorder.index.test[c])
      
      temp.recorder.source <- subset(small.sample.playback.test,recorder==recorder.index.test[c-1])
      
      distance.from.source <- dist.mat[c(temp.recorder.received$recorder),c(small.sample.playback.test$recorder[1])]
      
      receive.level <- temp.recorder.received$PowerDb
      source.level <- temp.recorder.source$PowerDb
      distance <- distance.from.source
      
      
      dist.ratio <- log10(distance/10)
      
      magic.x<-  receive.level/dist.ratio
      
      Sound.type <- temp.recorder.received$Sound.Type
      time <- temp.recorder.received$time
      date <- temp.recorder.received$date
      temp.df <- cbind.data.frame(receive.level,source.level,distance,Sound.type,time,date,magic.x)
      observed.prop.loss <- rbind.data.frame(observed.prop.loss,temp.df)
      
    }
    
  }
  #}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

date.time.combo <- paste(observed.prop.loss$date,observed.prop.loss$time,sep='_')
unique.date.time.combo <- unique(date.time.combo)

observed.prop.loss$Sound.category <- str_split_fixed(observed.prop.loss$Sound.type, pattern = '_',n=3)[,2]

observed.prop.loss.subset <- subset(observed.prop.loss,
                                    Sound.category=="Hfunstart" |Sound.category=="Hfuntrill" |Sound.category=="Halb"
                                    |Sound.category=="Pmor"|Sound.category=="Pwur")


unique.times <- 
  unique(observed.prop.loss.subset$time)


observed.prop.loss.subset$Sound.category <- 
  plyr::revalue(observed.prop.loss.subset$Sound.category,
                c(Hfunstart = "Gibbon Sabah Intro",
 Hfuntrill = "Gibbon Sabah Trill",Pmor="Orangutan Sabah", 
 Halb="Gibbon C. Kali",Pwur="Orangutan C. Kali"))

ggpubr::ggscatter(data = observed.prop.loss.subset,x='distance', y='receive.level',
                  color='Sound.category',
                  facet.by = 'Sound.category',xlim=c(0,300), #ylim=c(-70,-20),
                  add = c("loess"),title='All playback times combined')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")
