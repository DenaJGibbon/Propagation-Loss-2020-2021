MaliauDF <- read.csv("/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/BackgroundNoiseRemovedMaliauJune2022.csv")

head(MaliauDF)

# Create an index with unique date/time combinations
date.time.combo <- paste(MaliauDF$date,MaliauDF$time,sep='_')
unique.date.time.combo <- unique(date.time.combo)


# Create empty dataframe for propagation loss
observed.prop.lossMaliau <- data.frame()

# Loop to calculate propagation loss
for(z in 1:length(unique.date.time.combo)) { tryCatch({ 
  
  # Subset by unique date/time index
  temp.date.time.subset <- 
    str_split_fixed(unique.date.time.combo[z],pattern = '_',n=2) 
  
  # Subset data frame to focus on unique date/time
  temp.playback <- subset(MaliauDF, date==temp.date.time.subset[,1] & time==temp.date.time.subset[,2])
  
  # See how many unique playbacks
  unique(temp.playback$file.name)
  
  # Create an index for each unique file in the playback
  file.index <- unique(temp.playback$file.name)
  SelectionIndex <- (SelectionIDs$Sound.Type)
  
  # This isolates each selection in the original template one by one
  for(a in 1:length(SelectionIndex)){
    
    # Subset the same selection from each of the recorders
    small.sample.playback.test <- data.frame()
    for(b in 1:length(file.index) ){
      temp.table <- subset(temp.playback,file.name==file.index[b])
      temp.table$Sound.Type <- SelectionIDs$Sound.Type
      temp.table <- temp.table[a,]
      small.sample.playback.test <- rbind.data.frame(small.sample.playback.test,temp.table )
    }
    
    # Create an index for each unique recorder in the new subset dataset
    recorder.index.test <- unique(small.sample.playback.test$recorder)
    
    # Create a new column with receive levels standardized so the closest recorder is 0
    small.sample.playback.test$PowerDb.zero <- 
      small.sample.playback.test$PowerDb-small.sample.playback.test$PowerDb[1]
    
    results.subset <- small.sample.playback.test
    distance.from.source <- dist.mat[c(small.sample.playback.test$recorder),c(small.sample.playback.test$recorder[1])]
    results.subset$log_ddistance <- log(distance.from.source + dist.to.playback)
    
    lm.part1=lm(PowerDb~log_ddistance,results.subset)
    
    slope=as.numeric(coef(lm.part1)[2])
    pred.signal_0m=as.numeric(coef(lm.part1)[1])
    
    # Loop to calculate propagation loss; note the index starts at 2 since we use the closest one as the reference
    for(c in 2:length(recorder.index.test)){tryCatch({ 
      
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
      observed.prop.lossMaliau <- rbind.data.frame(observed.prop.lossMaliau,temp.df)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    
  }
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}
