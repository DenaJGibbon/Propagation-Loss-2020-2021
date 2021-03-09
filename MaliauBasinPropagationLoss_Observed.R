library(ggplot2)
library(stringr)
library(geosphere)
library(plyr)

# Read in selection tables
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
  
  template.table.temp <- read.delim(selection.tables[a],fill = T,header=T,stringsAsFactors = F)
  
  recorder <- list.of.recorders[a]
  file.name <- selection.tables.short[a]
  
  template.table.temp <- cbind.data.frame(template.table.temp,recorder,file.name)
  
 
  combined.template.table <- rbind.data.frame(combined.template.table,template.table.temp)
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
all.combined.df <- merge(combined.template.table,small.gps.df,by='recorder')
all.combined.df$date <- str_split_fixed(all.combined.df$file.name,pattern = '_',n=3)[,2]
time.temp <- str_split_fixed(all.combined.df$file.name,pattern = '_',n=3)[,3]
all.combined.df$time <- str_split_fixed(time.temp,pattern = '.txt',n=2)[,1]
all.combined.df$time <- substr(all.combined.df$time,start=1,stop=4)

all.combined.df$recorder <- as.factor(all.combined.df$recorder)
plot(all.combined.df$Inband.Power..dB.FS.~ all.combined.df$recorder)
# One playback


SelectionIDs <- read.delim("/Users/denaclink/Desktop/RStudio Projects/Propagation-Loss-2020-2021/SelectionLabels_S00974_20190811_101922_updated.txt")

date.time.combo <- paste(all.combined.df$date,all.combined.df$time,sep='_')
unique.date.time.combo <- unique(date.time.combo)

observed.prop.loss <- data.frame()

for(z in 1:21){#tryCatch({ # Ignore the 25 m spacing
 
temp.date.time.subset <- str_split_fixed(unique.date.time.combo[z],pattern = '_',n=2) 
  
temp.playback <- subset(all.combined.df, date==temp.date.time.subset[,1] & time==temp.date.time.subset[,2])

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
  
  receive.level <- temp.recorder.received$Inband.Power..dB.FS.
  source.level <- temp.recorder.source$Inband.Power..dB.FS.
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
observed.prop.loss.subset <- subset(observed.prop.loss.subset, time =="0800")
observed.prop.loss.subset <- subset(observed.prop.loss.subset, date =="20190823")


unique.times <- unique(observed.prop.loss.subset$time)

observed.prop.loss.subset$Sound.category <- plyr::revalue(observed.prop.loss.subset$Sound.category, c(Hfunstart = "Gibbon Sabah Intro", 
                                                   Hfuntrill = "Gibbon Sabah Trill",
                                                   Pmor="Orangutan Sabah", Halb="Gibbon C. Kali",Pwur="Orangutan C. Kali"))


ggpubr::ggscatter(data = observed.prop.loss.subset,x='distance', y='receive.level',color='Sound.category',
                  facet.by = 'Sound.category',xlim=c(0,300), #ylim=c(-70,-20),
                   add = c("loess"),title='All playback times combined')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")


for(y in 1:14){
observed.prop.loss.subset.times <- subset(observed.prop.loss.subset,time==unique.times[y])


TimePlot <- ggpubr::ggscatter(data = observed.prop.loss.subset.times,x='distance', y='receive.level',color='Sound.category',
                  facet.by = 'Sound.category',xlim=c(0,300),#ylim=c(-70,-20),
                   add = c("loess"),title=unique.times[y])+
                  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none") 

print(TimePlot)
}

unique(observed.prop.loss.subset$Sound.category)
playback.line.1 <- median(observed.prop.loss.subset$magic.x)

eq1 <- function(x){ playback.line.1*log10(x)}
eq3 <- function(x){ -10*log10(x)}
eq4 <- function(x){ -20*log10(x)}


Estimated1 <- cbind.data.frame(seq(1:1000),eq1(1:1000),rep('Estimated',1000))
colnames(Estimated1) <- c("X","Value","Label")
Spherical <- cbind.data.frame(seq(1:1000),eq3(1:1000),rep('Spherical',1000))
colnames(Spherical) <- c("X","Value","Label")
Cylindrical <-  cbind.data.frame(seq(1:1000),eq4(1:1000),rep('Cylindrical',1000))
colnames(Cylindrical) <- c("X","Value","Label")


attenuation.df <- rbind.data.frame(Estimated1,Spherical,Cylindrical)

attenuation.plot <- ggplot(data = attenuation.df,aes(x=X, y=Value,group=Label, colour=Label,linetype=Label))+
  geom_line() +theme_bw() + scale_color_manual(values = c("black","red","darkgray"))+
  theme(legend.title = element_blank())+ 
  scale_linetype_manual(values=c( "solid","twodash", "dotted"))+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))+
  xlab("Distance from source (m)") + ylab("Amplitude (dB)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

attenuation.plot



