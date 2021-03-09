library(seewave)
library(tuneR)

# Playback template table
SelectionIDs <- read.delim("/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/SelectionLabels_S00974_20190811_101922_updated.txt")

# Sound file location
SoundFiles.input <- 
  '/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/Background Noise Test/SoundFiles/'

# Selection table location
input.dir <- 
  "/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/Background Noise Test/SelectionTables"

# Set duration of noise selection
timesecs <- 3

# Read in selection tables with full file path names
selection.tables <- 
  list.files(input.dir,full.names = T)

selection.tables.short <-
  list.files(input.dir,full.names = F)

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


recorder.gps <- plotKML::readGPX("/Users/denaclink/Downloads/MB Playbacks 50 m.GPX") 

recorder.gps$waypoints$name <- str_remove(recorder.gps$waypoints$name, '0')

small.gps.df <- recorder.gps$waypoints[,c('lon','lat','name')]
colnames(small.gps.df) <- c('lon','lat','recorder')

xy.coords <- cbind(c(small.gps.df$lon), 
                   c(small.gps.df$lat))

dist.mat <- distm( xy.coords, fun = distHaversine)

colnames(dist.mat) <- c(as.character(small.gps.df$recorder))
rownames(dist.mat) <- c(as.character(small.gps.df$recorder))

# Combine all into a single dataframe
BackgroundNoiseRemovedDF.test <- merge(combined.template.table.test,small.gps.df,by='recorder')
BackgroundNoiseRemovedDF.test$date <- str_split_fixed(BackgroundNoiseRemovedDF.test$file.name,pattern = '_',n=3)[,2]
time.temp <- str_split_fixed(BackgroundNoiseRemovedDF.test$file.name,pattern = '_',n=3)[,3]
BackgroundNoiseRemovedDF.test$time <- str_split_fixed(time.temp,pattern = '.txt',n=2)[,1]
BackgroundNoiseRemovedDF.test$time <- substr(BackgroundNoiseRemovedDF.test$time,start=1,stop=4)


file.name.index <- unique(BackgroundNoiseRemovedDF.test$file.name)

BackgroundNoiseRemovedDF <- data.frame()
for(b in 1:length(file.name.index)){
  
  singleplayback.df <- subset(BackgroundNoiseRemovedDF.test,file.name==file.name.index[b])
  soundfile.path <- paste(SoundFiles.input,singleplayback.df$file.name[1],'.wav',sep='')
  wavfile.temp <- tuneR::readWave(soundfile.path)
  
  if(nrow(singleplayback.df)==nrow(SelectionIDs)){ # Check to make sure templates match
    
  # We can use the Raven selection table to isolate these particular sounds
  ListofWavs <- lapply(1:nrow(singleplayback.df), function(x) cutw(wavfile.temp, from=singleplayback.df$Begin.Time..s.[x],
                                                                 to=singleplayback.df$End.Time..s.[x], output='Wave'))
  
  Noiseclip1 <- cutw(wavfile.temp, from= (singleplayback.df$Begin.Time..s.[1]-timesecs),
                     to=singleplayback.df$Begin.Time..s.[1], output='Wave')
  Noiseclip2 <- cutw(wavfile.temp, from= (singleplayback.df$Delta.Time..s.[nrow(singleplayback.df)]),
                     to=(singleplayback.df$Delta.Time..s.[nrow(singleplayback.df)]+timesecs), output='Wave')
  
  
  ListofWavsNoise <- c(Noiseclip2)
  
  
  # Calculate the background noise
  NoiseDF <- data.frame()
  
  for(c in 1:1){
  Wavtemp <- ListofWavsNoise[[c]]
  
  for(d in 1:nrow(SelectionIDs)){
   Selectiontemp <-  SelectionIDs[d,]

   # Filter to the frequency range of the selection
   w.dn.filt <- fir(Wavtemp, 
                    from=Selectiontemp$Low.Freq..Hz., 
                    to=Selectiontemp$High.Freq..Hz.,output = "Wave") 
   w.dn.filt <- tuneR::normalize(w.dn.filt, unit="16")
   
   
   # Calculate the duration
     dur.seconds <- duration(w.dn.filt)
     
     bin.seq.smaller <- seq(from=0, to=dur.seconds, by=0.1)
     bin.seq.length.small <- length(bin.seq.smaller)-1
     subsamps.short <- lapply(1:bin.seq.length.small, function(i) 
       extractWave(w.dn.filt, from=as.numeric(bin.seq.smaller[i]), 
                   to=as.numeric(bin.seq.smaller[i+1]), xunit = c("time"),
                   plot=F,output="Wave"))
     
     # Calculate sum of squares for each 10 ms long sample
     sum.square.list <- list ()
     
     for (f in 1:length(subsamps.short)){
       wav.temp <- subsamps.short[[f]]
       sum.square.list[[f]] <- sum(wav.temp@left^2)
     }
     
   #Find median value for each 10 ms clip
   #noise.value <- as.data.frame(quantile(unlist(sum.square.list), c(.1)))
   Selectiontemp$noise.value <- median(unlist(sum.square.list))
   NoiseDF <- rbind.data.frame(NoiseDF,Selectiontemp)
  }
  }
  
  
 
    singleplayback.df$Sound.Type <- SelectionIDs$Sound.Type
    
    for(d in 1:nrow(singleplayback.df)){
      
      Selectiontemp <- singleplayback.df[d,]
     
      Wavtemp <-  ListofWavs[[d]]
      # Filter to the frequency range of the selection
      w.dn.filt <- fir(Wavtemp, 
                       from=Selectiontemp$Low.Freq..Hz., 
                       to=Selectiontemp$High.Freq..Hz.,output = "Wave") 
      w.dn.filt <- tuneR::normalize(w.dn.filt, unit="16")
      
      
      # Calculate the duration
      dur.seconds <- duration(w.dn.filt)
      
      bin.seq.smaller <- seq(from=0, to=dur.seconds, by=0.1)
      bin.seq.length.small <- length(bin.seq.smaller)-1
      subsamps.short <- lapply(1:bin.seq.length.small, function(i) 
        extractWave(w.dn.filt, from=as.numeric(bin.seq.smaller[i]), 
                    to=as.numeric(bin.seq.smaller[i+1]), xunit = c("time"),
                    plot=F,output="Wave"))
      
      # Calculate sum of squares for each 10 ms long sample
      sum.square.list <- list ()
      
      for (f in 1:length(subsamps.short)){
        wav.temp <- subsamps.short[[f]]
        sum.square.list[[f]] <- sum(wav.temp@left^2)
      }
      
      #Find median value for each 10 ms clip
      #signal.value <- as.data.frame(quantile(unlist(sum.square.list), c(.75)))
      signal.value <- median(unlist(sum.square.list))
      Selectiontemp$signal.value <- signal.value
    
      Selectiontemp$PowerDb <- 10 * log10((Selectiontemp$signal.value-NoiseDF[d,]$noise.value))
      print(Selectiontemp)
      BackgroundNoiseRemovedDF <- rbind.data.frame(BackgroundNoiseRemovedDF,Selectiontemp)
  }
  
  
  }
}

cor(BackgroundNoiseRemovedDF$Inband.Power..dB.FS.,BackgroundNoiseRemovedDF$PowerDb)
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

observed.prop.loss.subset <- subset(observed.prop.loss,Sound.category=="Hfunstart" |Sound.category=="Hfuntrill" |Sound.category=="Halb" )
observed.prop.loss.subset <- subset(observed.prop.loss.subset, time !="0720")


unique.times <- unique(observed.prop.loss.subset$time)


observed.prop.loss.subset$Sound.category <- 
  plyr::revalue(observed.prop.loss.subset$Sound.category,
                c(Hfunstart = "Gibbon Sabah Intro",
 Hfuntrill = "Gibbon Sabah Trill",Pmor="Orangutan Sabah", 
 Halb="Gibbon C. Kali",Pwur="Orangutan C. Kali"))

ggpubr::ggscatter(data = observed.prop.loss.subset,x='distance', y='receive.level',color='Sound.category',
                  facet.by = 'Sound.category',xlim=c(0,300), #ylim=c(-70,-20),
                  add = c("loess"),title='All playback times combined')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")
