# R code to calculate SNR from playback files
library(tuneR)
library(seewave)
library(stringr)

# V1 add 60-s noise to each selection and calculate SNR for the whole segment 

# Set the directory with the location of sound and text files
# NOTE: For this example they are the same but will probably be different based on file structure for all recordings
input.dir.sound <- '/Users/denasmacbook/Downloads/Files For Dena'
input.dir.text <- '/Users/denasmacbook/Downloads/Files For Dena'

# List all .wav or .txt files in specified directory 
sound.files <- list.files(input.dir.sound,pattern='.wav',full.names = T)
sound.files.short.names <- list.files(input.dir.sound,pattern='.wav',full.names = F)
selection.tables <- list.files(input.dir.text,pattern='.txt',full.names = T)

# Create new DF
SNRdf.new <- data.frame()
# Loop to calculate SNR for individual selections
for( a in 1:length(selection.tables)){

# Read in the selection table
temp.selection.table <- read.delim2(selection.tables[a],fill = T, stringsAsFactors = F)    

# Match selection table name to sound file
temp.name.txt <- str_split_fixed(selection.tables[a],pattern = 'selection',n=2)[,2]
sound.file.name <- str_split_fixed(temp.name.txt,pattern = '.txt',n=2)[,1]

# Match sound file name in selection table to the name in the directory
sound.file.path <- paste(input.dir.sound,'/',sound.file.name,'.wav',sep='')
  
# Read in the .wav file
temp.wav <- tuneR::readWave(filename =  sound.file.path)

# There were 34 selections per playback and 3 playbacks
# First, will calculate a noise value for each of the three playbacks
playback.breaks <- c(1,35,69,103) # This indicates which selections to use for noise calculation

for(b in 1: (length(playback.breaks)-1)){
  # Find start row of playback
  start.row <- temp.selection.table[ playback.breaks[b],]
  playback.start.time <- as.numeric(start.row$Begin.Time..s.)
  
  # Find end row of playback
  end.row <-   temp.selection.table[ (playback.breaks[b+1]-1) ,]
  playback.end.time <- as.numeric(end.row$End.Time..s.)
  
  # Isolate the selections of a single playback
  single.playback.selections <- temp.selection.table[playback.breaks[b]:(playback.breaks[b+1]-1),]
  
  # Convert start and end time to numeric
  single.playback.selections$Begin.Time..s. <- as.numeric(single.playback.selections$Begin.Time..s.)
  single.playback.selections$End.Time..s. <- as.numeric(single.playback.selections$End.Time..s.)
  
  # Convert frequency to numeric
  single.playback.selections$Low.Freq..Hz. <- as.numeric(single.playback.selections$Low.Freq..Hz.)
  single.playback.selections$High.Freq..Hz. <- as.numeric(single.playback.selections$High.Freq..Hz.)
  
  # Add 60 seconds of noise
  for(c in 1:nrow(single.playback.selections)){
    print(paste('processing Soundfile', a, 'Playback', b, 'Selection', c, 'out of', nrow(single.playback.selections)))
    temp.wav.cut <- single.playback.selections[c,]
    
    if(temp.wav.cut$Begin.Time..s. > 60){
      short.wav <- cutw(temp.wav,from = (temp.wav.cut$Begin.Time..s.-60),to= (temp.wav.cut$End.Time..s.+60),output = 'Wave')
    } else {
      
      short.wav <- cutw(temp.wav,from = (temp.wav.cut$Begin.Time..s.),to= (temp.wav.cut$End.Time..s.+120),output = 'Wave')
      
    }
    
    # Filter to the frequency range of the selection
    w.dn.filt <- fir(short.wav, from=temp.wav.cut$Low.Freq..Hz., to=temp.wav.cut$High.Freq..Hz.,output = "Wave") 
    w.dn.filt <- tuneR::normalize(w.dn.filt, unit="16")
    
    # Calculate the duration
    dur.seconds <- duration(short.wav)
    
    # Divide into evenly spaced 1-sec bins 
    bin.seq <- seq(from=0, to=dur.seconds, by=1)
    length(bin.seq)
    
    # Create a list of all the 1-s bins for SNR analysis
    bin.seq.length <- length(bin.seq)-1
    subsamps.1sec <- lapply(1:bin.seq.length, function(i) 
      extractWave(w.dn.filt, from=as.numeric(bin.seq[i]), to=as.numeric(bin.seq[i+1]), 
                  xunit = c("time"),plot=F,output="Wave"))
    
    # Calculate noise for each 1 sec bin for the longer recording
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
    
    ## Now have a list with 120 values corresponding to median value for each 10 ms subsample
    ## that are candidate noise; use the 10th percentile of the distribution for noise estimate
    noise.value <- quantile(unlist(noise.list), c(.10))
    noise.value
    
    signal.value <- quantile(unlist(noise.list), c(.75,.99))
    signal.value
    
    # Calculate SNR in dB
    # Here we are using the 99th quantile
    SNR.dB.high <- 10 * log10((signal.value[2]-noise.value)/noise.value)
    print(SNR.dB.high)
    print(temp.wav.cut$SNR.NIST.Quick..dB.FS.)
    
    # NOTE: I only include the first 10 columns of each selection table
    new.temp.df <-  cbind.data.frame(temp.wav.cut[,1:10],SNR.dB.high,sound.file.name)
 
    # Add columns iteratively to dataframe
    SNRdf.new <- rbind.data.frame(SNRdf.new,new.temp.df)
}

}

}
