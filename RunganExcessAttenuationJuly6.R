RunganDF <- read.csv("/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/BackgroundNoiseRemovedDFRunganJune2022.csv")
PredictedSpreading <- read.csv("/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/Predicted_dB_Spherical.csv")
PredictedSpreadingRungan <- subset(PredictedSpreading,Site=='Munkgu')

# Read in data file
# Each row corresponds to a playback
rungan_data <- read.csv("PropLoss_test.csv")

head(RunganDF)
table(RunganDF$distance.from.source, RunganDF$time)

# Create an index with unique date/time combinations
date.time.combo <- paste(RunganDF$date,RunganDF$time,sep='_')
unique.date.time.combo <- unique(date.time.combo)

Loc_Name.index <- 
  unique(CombinedDF$Loc_Name)

Loc_Name.index  <- Loc_Name.index[- which(Loc_Name.index %in% c('char1','char2','char3'))]

# Create dataframe to match characterization units  
char.matching <- data.frame(
  char = c('char1','char2','char3'),
  rec=c('S00976', 'S01143', 'S00974')
)

# Create empty dataframe for propagation loss
observed.prop.lossRungan <- data.frame()
DoublingDistanceDF <- data.frame()
# Loop to calculate propagation loss
for(z in 1:length(Loc_Name.index)) { tryCatch({ 
  
  # Subset data frame to focus on unique date/time
  temp.playback <- subset(RunganDF,Loc_Name==Loc_Name.index[z])
  
  # Create an index for each unique file in the playback
  SelectionIndex <- (SelectionIDs$Sound.Type)
  
  playback.index <-  which(rungan_data$Loc_Name == unique(temp.playback$Loc_Name))
  
  temp.playback.data <- rungan_data[playback.index[1],]
  
  temp.char.info <- char.matching[which(char.matching$rec== temp.playback.data$ARU_ID),]
  
  TempReferenceDF <- subset(CombinedDF,Loc_Name== temp.char.info$char)
  
  # Create an index for each unique file in the playback
  distance.index <- unique(temp.playback$distance.from.source)
  SelectionIndex <- (SelectionIDs$Sound.Type)
  
  temp.playback$distance.from.source <- as.character(temp.playback$distance.from.source)
  
  
  # This isolates each selection in the original template one by one
  for(a in 1:length(SelectionIndex)){
    
    # Subset the same selection from each of the recorders
    small.sample.playback.test <- data.frame()
    for(b in 1:length(distance.index) ){
      temp.table <- subset(temp.playback,distance.from.source==distance.index[b])
      temp.table$Sound.Type <- SelectionIDs$Sound.Type
      temp.table <- temp.table[a,]
      small.sample.playback.test <- rbind.data.frame(small.sample.playback.test,temp.table )
    }
    
    
    small.sample.playback.test <- rbind.data.frame(small.sample.playback.test,
                                                   TempReferenceDF[a,])
    
    # Reorder based on distance from speaker
    small.sample.playback.test <-  arrange(small.sample.playback.test, distance.from.source)  
    
    # Create a new column with receive levels standardized so the closest recorder is 0
    small.sample.playback.test$PowerDb.zero <- 
      small.sample.playback.test$PowerDb-small.sample.playback.test$PowerDb[1]
    
    results.subset <- small.sample.playback.test
    results.subset$log_ddistance <- log(as.numeric(results.subset$distance.from.source))
    
    results.subset <- results.subset[1:3,]
    
    #the lm includes channel as a dummy, as only the intercept should be affected by the microphone
    lm.part1=lm(PowerDb~log_ddistance,results.subset)
    
    slope=as.numeric(coef(lm.part1)[2])
    pred.signal_0m=as.numeric(coef(lm.part1)[1])
    DoublingDistanceDFtemp <- cbind.data.frame(slope,pred.signal_0m)
    
    distance.index.test <- unique(small.sample.playback.test$distance.from.source)
    
    # Loop to calculate propagation loss; note the index starts at 2 since we use the closest one as the reference
    for(c in 2:length(distance.index.test)){tryCatch({ 
     
      # Isolate the recorder that we will use to estimate receive levels
      temp.recorder.received <- subset(small.sample.playback.test,distance.from.source==distance.index.test[c])
      
      # Isolate the recorder we consider as the 'source' for our relative calculations
      temp.recorder.source <- subset(small.sample.playback.test,distance.from.source==distance.index.test[1])
      
       
      # Assign the actual receive level (not zeroed) to new variable
      actual.receive.level <- temp.recorder.received$PowerDb
      if(length(actual.receive.level)==0){
        print( small.sample.playback.test$Sound.Type[1] )
      }
      # Assign zeroed receive level to new variable 
      zero.receive.level <- temp.recorder.received$PowerDb.zero
      
      # Assign 'source' level to new variable
      source.level <- temp.recorder.source$PowerDb.zero
      
      # Assign distance to new variable
      distance <- as.numeric(temp.recorder.received$distance.from.source)
      
      isolate.distance <- which.min(abs(PredictedSpreadingRungan$Dist2 - distance))
      
      PredictedSpreadingRunganTemp <- PredictedSpreadingRungan[isolate.distance,]
      
      ActualDbDifference <- temp.recorder.source$PowerDb  - temp.recorder.received$PowerDb  
      
      ExcessAttenuation <-  ActualDbDifference -PredictedSpreadingRunganTemp$dBLoss_Spherical
      
      # Assign noise level estimate to new variable
      noise.level <- temp.recorder.received$NoisevalueDb
      
      # Calculate the distance ratio for propagation loss equation
      dist.ratio <- log10(10/dist.to.playback)
      
      # Calculate the 'magic x'
      magic.x <-  zero.receive.level/dist.ratio
      
      # Assign sound type to new variable
      Sound.type <- temp.recorder.received$Sound.Type
      
      # Assign time  to new variable
      time <- temp.recorder.received$time
      
      # Assign date to new variable
      date <- temp.recorder.received$date
      
      # Habitat type
      habitat <- temp.playback.data$Habitat
      # Combine all into a new temp dataframe
      temp.df <- cbind.data.frame(zero.receive.level,actual.receive.level,source.level,distance,Sound.type,time,date,magic.x,noise.level,ExcessAttenuation,habitat)
      
      # Combine all observations into one dataframe
      observed.prop.lossRungan <- rbind.data.frame(observed.prop.lossRungan,temp.df)
      
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      DoublingDistanceDF <- rbind.data.frame(DoublingDistanceDF,DoublingDistanceDFtemp)
    }
    
  }
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  
}

observed.prop.lossRungan$Call.category <- str_split_fixed(observed.prop.lossRungan$Sound.type,pattern = '_',n=3)[,2]
observed.prop.lossRunganSubset <- subset(observed.prop.lossRungan,Sound.type=="1_Pmor_P2" |Sound.type== "1_Hfunstart")
ggscatter(data=observed.prop.lossRunganSubset, y='distance',
          x='ExcessAttenuation',color = 'Sound.type',facet.by = 'habitat')+geom_jitter(width = 1.5, height = 1)

ggscatter(data=observed.prop.lossRungan, y='distance',
          x='ExcessAttenuation',facet.by = 'habitat')+geom_jitter(width = 1.5, height = 1)

RunganPlot <- ggscatter(data=observed.prop.lossRunganSubset, y='distance',
                        x='ExcessAttenuation',color = 'Sound.type',facet.by = 'Sound.type')+geom_jitter(width = 1.5, height = 1)+ylim(0,500)

cowplot::plot_grid(RunganPlot,MaliauPlot)
