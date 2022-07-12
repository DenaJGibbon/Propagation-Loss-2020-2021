# load packages
library(baRulho)
library(stringr)
library(tuneR)
library(seewave)
library(signal)
library(geosphere)
library(plyr)


input.dir <- '/Users/denaclink/Downloads/Maliau Basin Selection Tables'

# Read in selection tables with full file path names
selection.tables <- 
  list.files(input.dir,full.names = T)

selection.tables.short <-
  list.files(input.dir,full.names = F)

list.of.recorders <- str_split_fixed(selection.tables.short, pattern = '_',3)[,1]

# Read in selection tables and combine with file name
combined.template.table <- data.frame()

for(a in 1:length(selection.tables)){
  
template.table.temp <- read.table(selection.tables[a],fill = T,header=T)

template.table.temp <- cbind.data.frame(template.table.temp[,c(3,1,5:8,11)],list.of.recorders[a],selection.tables.short[a])

colnames(template.table.temp) <- c("channel", "selec", "start", "end", "bottom.freq", "top.freq", 'annotation',"sel.comment","rec.comment")

combined.template.table <- rbind.data.frame(combined.template.table,template.table.temp)
}

# Subset a single annotation

recorder.index <- unique(combined.template.table$sel.comment)

for.extended.selection.table <- data.frame()

for(b in 1:length(recorder.index)){
print(b)

temp.recorder.index.all <- 
  subset(combined.template.table,sel.comment==recorder.index[b])
  
temp.recorder.index <- subset(temp.recorder.index.all,annotation=='Gibbon')
temp.table <- temp.recorder.index[1,]

recorder <- paste(str_split_fixed(temp.table$rec.comment,pattern = '_',n=4)[,2],str_split_fixed(temp.table$rec.comment,pattern = '_',n=4)[,3],
                  sep='_')

# List all files in audio directory
list.all.recorders <- 
  list.files(paste('/Volumes/Seagate Backup Plus Drive/Audio files/Swift/',sep=''),pattern=recorder,  recursive = TRUE)

# Isolate individual file names
wav.files.names <- str_split_fixed(list.all.recorders, pattern='/',n=4)[,4]

if(nchar(wav.files.names[1])==0){
wav.files.names <- str_split_fixed(list.all.recorders, pattern='/',n=3)[,3]
}


# Convert name from text file to appropriate format
temp.file.name <- str_split_fixed(temp.table$rec.comment, pattern = '_',n=2)[,2]

temp.file.name <- 
  paste(str_split_fixed(temp.file.name, pattern = '[.]',n=2)[,1],'.wav', sep='')

# Read in the .wav file
 temp.wav <- readWave(filename =   paste('/Volumes/Seagate Backup Plus Drive/Audio files/Swift/',
                             list.all.recorders[which(wav.files.names %in% temp.file.name)],sep=''))

for(c in 1:nrow(temp.recorder.index)){

temp.table <-   temp.recorder.index[c,]
# Cut the .wav file at appropriate point
short.temp.wav <- cutw(temp.wav,from = temp.table$start,to=temp.table$end, output = 'Wave')

writeWave(short.temp.wav, filename = paste(c, '_',temp.file.name,sep=''),extensible = F)

sound.files <- (paste(c, '_',temp.file.name,sep=''))
new.combined.table <- cbind.data.frame(sound.files,temp.table)

new.combined.table$start <- 0
new.combined.table$end <- temp.table$end-temp.table$start

for.extended.selection.table <- rbind.data.frame(for.extended.selection.table,new.combined.table)

# Make a spectrogram to check output
# Fs <- short.temp.wav@samp.rate
# step <- trunc(5*Fs/1000)             # one spectral slice every 5 ms
# window <- trunc(20*Fs/1000)          # 40 ms data window
# fftn <- 2^ceiling(log2(abs(window))) # next highest power of 2
# spg <- specgram(x=short.temp.wav@left, n=fftn, Fs=Fs, window=window, overlap = 95)
# S <- abs(spg$S[2:(fftn*4000/Fs),])   # magnitude in range 0<f<=4000 Hz.
# S <- S/max(S)         # normalize magnitude so that max is 0 dB.
# S[S < 10^(-40/10)] <- 10^(-40/10)    # clip below -40 dB.
# S[S > 10^(-3/10)] <- 10^(-3/10)      # clip above -3 dB.
# 
# new.color.vals <- c('red','gray')
# shadesOfGrey <- colorRampPalette(c("grey0", "grey100"))
# image(t(20*log10(S)),col = rev(shadesOfGrey(512)),
#       axes=F,ylim=c(0,0.5))

}

 temp.recorder.index.noise <- subset(temp.recorder.index.all,annotation=='Ambient')
 
 for( d in 1:nrow(temp.recorder.index.noise)){
 temp.table <-   temp.recorder.index.noise[d,]
 

 # Cut the .wav file at appropriate point
 short.noise.wav1 <- cutw(temp.wav,from = (temp.table$start) ,to=temp.table$end, output = 'Wave')
 
 writeWave(short.noise.wav1, filename = paste('noise1', '_',temp.file.name,sep=''),extensible = F)
 
 # short.noise.wav2 <- cutw(temp.wav,from = temp.table$end ,to= (temp.table$end+5), output = 'Wave')
 # 
 # writeWave(short.noise.wav2, filename = paste('noise2', '_',temp.file.name,sep=''),extensible = F)
 # 
}
 
}



#write.csv(for.extended.selection.table,'for.extended.selection.table.csv')
#for.extended.selection.table <- read.csv('for.extended.selection.table.csv')
#for.extended.selection.table <- for.extended.selection.table[,-c(1)]
  
# Just focus on the first call in the playback series
# for.extended.selection.table <- for.extended.selection.table[c(1,3,5,7,9),]
# 
# 
# for.extended.selection.table.convert <- selection_table(X = for.extended.selection.table, pb = FALSE, 
#                 extended = TRUE, confirm.extended = FALSE)

# Calculate distance for all recorders and playback
TNC.recorders <- read.csv("/Users/denasmacbook/Downloads/Swiftsonly_recorderandplaybackdata.csv")

# Convert to Swift date structure
day <- str_split_fixed(TNC.recorders$loc.or.date,n=2,pattern='-')[,1]
day.swift <- paste('201910',day,sep='')

# Add column with new date format
TNC.recorders$day.swift <- day.swift

# Subset playback name
pb.name <- str_split_fixed(temp.table$rec.comment,pattern='_',n=2)[,1]
pb.date <- str_split_fixed(temp.table$rec.comment,pattern='_',n=4)[,3]

# Get playback info
pb.location <- subset(TNC.recorders,unique.id==pb.name)

# In case playback has same name on different dates subset by date
pb.location.info <- subset(pb.location,day.swift==pb.date)[1,]

recorders.for.playback.subset <- TNC.recorders[ which(TNC.recorders$unique.id %in% c(list.of.recorders)),]


xy.coords <- cbind(c(recorders.for.playback.subset$lon,pb.location.info$lon), 
      c(recorders.for.playback.subset$lat,pb.location.info$lat))

dist.mat <- distm( xy.coords, fun = distHaversine)

colnames(dist.mat) <- c(as.character(recorders.for.playback.subset$unique.id), as.character(pb.location.info$unique.id))
rownames(dist.mat) <- c(as.character(recorders.for.playback.subset$unique.id), as.character(pb.location.info$unique.id))

# Calculate SNR
all.snr.df <- data.frame()
for(y in 1:nrow(for.extended.selection.table)){
  
temp.table <-   for.extended.selection.table[y,]

signal <- tuneR::readWave( as.character(temp.table$sound.files))
shortened.name <- substr(as.character(temp.table$sound.files),start=3,stop=nchar(as.character(temp.table$sound.files)))
noise1 <- tuneR::readWave( paste('noise1_',shortened.name,sep=''))
#noise2 <- tuneR::readWave( paste('noise2_',shortened.name,sep=''))


snr.val1 <- sum(signal@left^2)/sum(noise@left^2)
#snr.val2 <- seewave::rms(signal@left)/seewave::rms(noise2@left)

snr.val1db <- 10 * log10(snr.val1)
#snr.val2db <- 20 * log10(snr.val2)

distance.val <- dist.mat[which(rownames(dist.mat)== temp.table$sel.comment),8]

snr.df <- cbind.data.frame(temp.table$sel.comment,snr.val1db,distance.val)
all.snr.df <- rbind.data.frame(all.snr.df,snr.df)
}




transmission.loss.df <- data.frame()
distance.index <- sort(unique(all.snr.df$distance.val))
snr.index <- rev(sort(unique(all.snr.df$snr.val1db)))

for(a in 2:length(distance.index)){
  print(a)
  temp.snr.table <- subset(all.snr.df,snr.val1db==snr.index[a])
  
  
  for(b in 1:1){
    snr.table.subset <- temp.snr.table[b,]
    snr.template.sub <- subset(all.snr.df, snr.val1db==snr.index[1])
    snr.template.sub <- snr.template.sub[1,]
    
    highest.snr.index <- which(colnames(dist.mat)== snr.template.sub$`temp.table$sel.comment`)
    distance.val.new <- dist.mat[which(rownames(dist.mat)== snr.table.subset$`temp.table$sel.comment`),highest.snr.index]
    
    dist.from.source <- distance.val.new
    
    
    transmission.loss <- abs(snr.template.sub$snr.val1db-snr.table.subset$snr.val1db)
    temp.rec <- snr.table.subset[1]
    temp.df <- cbind.data.frame(temp.rec,transmission.loss,dist.from.source)
    colnames(temp.df) <- c('recorder','SNR.dB.high', 'dist.from.source')
    print(temp.df)
    transmission.loss.df <- rbind.data.frame(transmission.loss.df,temp.df)
  }
}

plot(transmission.loss.df$SNR.dB.high ~transmission.loss.df$dist.from.source)

xval <- ( abs(transmission.loss.df$SNR.dB.high)/log10(transmission.loss.df$dist.from.source))
xval

length(unique(paste(TNC.recorders$lon,TNC.recorders$lat,sep='_')))

