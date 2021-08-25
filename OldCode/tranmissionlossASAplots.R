library(ggplot2)
library(ggpubr)
library(stringr)
library(viridis)

snr.table.full <- read.csv("/Users/denasmacbook/maliau_playbacks/Nov29alldates.snr.comparison.full.csv")
snr.table.0827 <- read.csv("/Users/denasmacbook/maliau_playbacks/08272019Nov29alldates.snr.comparison.full.csv")
snr.table.full <- rbind.data.frame(snr.table.full,snr.table.0827)
snr.table.full$recorder <- str_split_fixed(snr.table.full$file, pattern= '_',n=3)[,1]
snr.table.full$time <- substr(str_split_fixed(snr.table.full$file, pattern= '_',n=3)[,3],start=1,stop=4)

snr.table.full$date.classifier <- as.character(snr.table.full$date.classifier)
unique(snr.table.full$date.classifier)
unique(snr.table.full$recorder)

 snr.table.full.08262019 <- subset(snr.table.full,date.classifier==08262019)
 snr.table.full.08272019 <- subset(snr.table.full,date.classifier==08272019)
 snr.table.full <- subset(snr.table.full,date.classifier!=08262019)
 snr.table.full <- subset(snr.table.full,date.classifier!=08272019)
snr.table.full$recorder <- as.factor( plyr::revalue(snr.table.full$recorder, c("M1"="25", "M2"="50",
                                                                               "M3"="100", "M4"="150",
                                                                               "M5"="200","M6"="250","M7"="300")))

 snr.table.full.08262019$recorder <- as.factor(snr.table.full.08262019$recorder)
 snr.table.full.08262019 <- subset(snr.table.full.08262019, recorder != "M3")
 snr.table.full.08262019 <- droplevels(subset(snr.table.full.08262019, recorder != "M4"))
 snr.table.full.08262019 <- droplevels(subset(snr.table.full.08262019, recorder != "M5"))
 snr.table.full.08262019 <- droplevels(subset(snr.table.full.08262019, recorder != "M6"))
 
 snr.table.full.08262019$recorder <- as.factor( plyr::revalue(snr.table.full.08262019$recorder,c("M1"="10", "M2"="50","M7"="25", 
                                                              "M8"="50","M9"="100")))
 
 snr.table.full.08272019$recorder <- as.factor( plyr::revalue(snr.table.full.08272019$recorder,c("M1"="10", "M2"="50",
                                                                                                 "M7"="25","M8"="50",
                                                                                                 "M9"="100", "M12"="200")))
 snr.table.full.08272019 <- subset(snr.table.full.08272019,snr.high >0)
 snr.table.full <- rbind.data.frame(snr.table.full,snr.table.full.08262019,snr.table.full.08272019)
 snr.table.full <- subset(snr.table.full,time != '0840')

snr.table.full$recorder <- factor(snr.table.full$recorder, levels = c(sort(as.integer(levels(snr.table.full$recorder)))))

snr.table.full$date.classifier <-as.factor(snr.table.full$date.classifier)


ggerrorplot(snr.table.full, x = "recorder", y = "snr.high", color = "time",shape= "date.classifier",
            desc_stat = "mean_se", add='jitter',
            position = position_dodge(0.3)) + 
  ylab("SNR (dB)") + xlab("Distance (m)")+
  theme(legend.title = element_blank())+
  scale_color_manual(values = viridis(n=length(unique(snr.table.full$time))) )
  

# Transmission loss calculations

snr.table.full$date.time.index <- as.factor(paste(snr.table.full$date.classifier,snr.table.full$time,sep='_'))


date.classifier.index <- as.character(unique(snr.table.full$date.time.index))

transmission.loss.df <- data.frame()
for(a in 1:length(date.classifier.index)){
  snr.table.by.date <- subset(snr.table.full,date.time.index==as.character(date.classifier.index[a]))
  snr.table.by.date$date.time.index <-as.character(snr.table.by.date$date.time.index)
  index.subset <- unique(snr.table.by.date$date.time.index)
  
for(b in 1:length(index.subset)){
    snr.table.subset <-subset(snr.table.by.date,date.time.index==(index.subset[b]))
   
    snr.table.subset$female.id <- paste(str_split_fixed(snr.table.subset$index,pattern = '_',n=3)[,1],
                                        str_split_fixed(snr.table.subset$index,pattern = '_',n=3)[,2],sep='_')
    
    female.index <- unique(snr.table.subset$female.id)
    recorder.index <- unique(snr.table.subset$recorder)
    
    for(c in 1:length(female.index)){
    subset.female <- subset(snr.table.subset, female.id == female.index[c])
    mean.snr.by.recorder <- aggregate(subset.female$snr.high, list(subset.female$recorder),mean)
    
    for(d in 1:(nrow(mean.snr.by.recorder)-1)){
      
      snr.first <- mean.snr.by.recorder[1,2]
      snr.second <- mean.snr.by.recorder[d+1,2]
      dist.from.source <- mean.snr.by.recorder[d+1,1]
      transmission.loss <- snr.second - snr.first 
      
      temp.df <- cbind.data.frame(subset.female[1,11], transmission.loss,dist.from.source,index.subset[b])
      colnames(temp.df) <- c('female.id','transmission.loss', 'dist.from.source', 'date.time.index')
      print(temp.df)
      transmission.loss.df <- rbind.data.frame(transmission.loss.df,temp.df)
    }
  }
}
}

temp.transmission.loss.df <- transmission.loss.df 
temp.transmission.loss.df <-na.omit(temp.transmission.loss.df)

temp.transmission.loss.df$date.classifier <- str_split_fixed(temp.transmission.loss.df$date.time.index,pattern = '_',n=2)[,1]
temp.transmission.loss.df$time <- str_split_fixed(temp.transmission.loss.df$date.time.index,pattern = '_',n=2)[,2]
# temp.transmission.loss.df$dist.from.source <- as.factor( plyr::revalue(temp.transmission.loss.df$dist.from.source, 
#                                                                   c("10"="0-10", "25"="0-25",
#                                                                     "50"="0-50", "100"="0-100",
#                                                                   "150"="0-150", "200"="0-200",
#                                                                   "250"="0-250","300"="0-300")))

temp.transmission.loss.df$time <- as.factor(temp.transmission.loss.df$time)


ggerrorplot(temp.transmission.loss.df, x = "dist.from.source", y = "transmission.loss", 
            color = "dist.from.source", shape="date.classifier", add='jitter',
            desc_stat = "mean_se", 
            position = position_dodge(0.3)) + 
  ylab("delta SNR (dB)") + xlab("Distance (m)")+
  theme(legend.title = element_blank())+
  scale_color_manual(values = viridis ::viridis(n=length(levels(as.factor(transmission.loss.df$dist.from.source)))))
  


library(lme4)
library(bbmle)

temp.transmission.loss.df$time.numeric <- as.numeric(temp.transmission.loss.df$time)
temp.transmission.loss.df$dist.from.source <- (as.numeric(as.character(temp.transmission.loss.df$dist.from.source)))

temp.transmission.loss.df$dist.from.source.scale <- scale(log(temp.transmission.loss.df$dist.from.source), center = TRUE, scale = TRUE)

temp.transmission.loss.df$playbackline <- plyr::revalue(temp.transmission.loss.df$date.classifier, c("8232019"="Playback Line 1", "8242019"="Playback Line 1", "8252019"="Playback Line 1", "8262019"="Playback Line 2", "8272019"="Playback Line 2"))

null.model <- lmer(transmission.loss ~ (1|playbackline), data= temp.transmission.loss.df)
distance.model <- lmer(transmission.loss ~ (dist.from.source.scale) + (1|playbackline), data= temp.transmission.loss.df)
distance.time.model <- lmer(transmission.loss ~ (dist.from.source.scale) + time.numeric+ (1|playbackline), data= temp.transmission.loss.df)
time.model <- lmer(transmission.loss ~ time.numeric+ (1|playbackline), data= temp.transmission.loss.df)
AICctab(null.model,distance.model, distance.time.model,time.model)



temp.transmission.loss.df$fit <- predict(distance.model)   #Add model fits to dataframe

plot.all.dist <- ggplot(temp.transmission.loss.df,aes(((dist.from.source.scale)), transmission.loss, col=playbackline)) + 
  #facet_grid(~date.classifier) +
  geom_line(aes(y=fit), size=0.8,alpha = 0.3) +
   #geom_jitter(width = 0.75, height = 0.75)+
  geom_point(alpha = 0.3) + 
  theme_bw() +xlab("Distance to source (m)") + ylab("delta SNR (dB)")+ 
  guides(colour=guide_legend(title=""))+ #ylim(-25,0)+
  scale_color_manual(values = c("red","blue")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))

plot.all.dist


###########################


spreading.df <- temp.transmission.loss.df
spreading.df <- droplevels(subset(spreading.df,time!="0840"))
spreading.df <- droplevels(subset(spreading.df,time!="1120"))
spreading.df <- subset(spreading.df, playbackline=="Playback Line 2")
# spreading.df <- subset(spreading.df, date.classifier=="8272019")

spreading.df$date.index <- spreading.df$date.time.index 

date.time.index.spread <- unique(spreading.df$date.index)
solvex <- data.frame()
for (b in 1:length(date.time.index.spread)){
  spreading.df.sub <- subset(spreading.df, date.index==date.time.index.spread[b])
  female.sub.index <- unique(spreading.df.sub$female.id)
  for(c in 1:length(female.sub.index))
    subset.by.female <-subset(spreading.df.sub, female.id==female.sub.index[c])
    for (d in 2:nrow(subset.by.female)){ tryCatch({
      print(d)
      subset1 <- subset.by.female[d,]
      #subset2 <- subset.by.female[d+1,]
      
      tl.subtract <- abs(subset1$transmission.loss - 
                           as.numeric(as.character(subset.by.female[1,]$transmission.loss)))
      distance.factor <- as.numeric(as.character(subset1$dist.from.source))/as.numeric(as.character(subset.by.female[1,]$dist.from.source))
      xval <- (tl.subtract/log10(distance.factor))
      temp.df <- cbind.data.frame(xval,subset1)
      solvex <- rbind.data.frame(solvex,temp.df)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
}

#Check mean
solvex <- na.omit(solvex)

# 17.24363 or 17.9 with snr.low
playback.line.1 <- 17.00654
playback.line.2 <- 10.4522
#playback.line.comb <- mean(solvex$xval)

eq1 <- function(x){ -playback.line.1*log10(x)}
eq2 <- function(x){ -playback.line.2*log10(x)}
eq3 <- function(x){ -10*log10(x)}
eq4 <- function(x){ -20*log10(x)}


Estimated1 <- cbind.data.frame(seq(1:1000),eq1(1:1000),rep('Estimated',1000))
colnames(Estimated1) <- c("X","Value","Label")
Estimated2 <- cbind.data.frame(seq(1:1000),eq2(1:1000),rep('Estimated 2',1000))
colnames(Estimated2) <- c("X","Value","Label")
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
  xlab("Distance to source (m)") + ylab("Amplitude (dB)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

attenuation.plot


cowplot::plot_grid(plot.all.dist,attenuation.plot, labels = c("A", "B"))

# Temperature and humidity plots

temperature.df <- read.csv("/Users/denasmacbook/clean/temp.added.csv")
temperature.df$time <-str_split_fixed(temperature.df$date.time, pattern = "_", n=2)[,2]

par(mfrow=c(1,2))
plot(temperature.df$temp ~ temperature.df$time, xlab="Time (h)", ylab="Temperature (C)")
abline(lm(temperature.df$temp ~ as.numeric(temperature.df$time)),col='red')
plot(temperature.df$humidity~ temperature.df$time, xlab="Time (h)", ylab="Humidity (%)")
abline(lm(temperature.df$humidity ~ as.numeric(temperature.df$time)),col='red')

# Add temperature to TL dataframe
temp.dates.times <- unique(temp.transmission.loss.df$date.time.index)

temp.transmission.loss.df.add.temp <- data.frame()
for(g in 1:length(temp.dates.times)){
  subset.tl <- subset(temp.transmission.loss.df,date.time.index==temp.dates.times[g])
  subset.temp <- subset(temperature.df,date.time==as.character(temp.dates.times[g]))
  temperature <- subset.temp$temp
  df.temperature.added <- cbind.data.frame(subset.tl,temperature)
  temp.transmission.loss.df.add.temp <- rbind.data.frame(temp.transmission.loss.df.add.temp,df.temperature.added)
}

temp.transmission.loss.df.add.temp$dist.from.source <- as.factor(temp.transmission.loss.df.add.temp$dist.from.source)
temperature.model <- lmer(transmission.loss ~ (temperature) + (1|dist.from.source), data= temp.transmission.loss.df.add.temp)
temp.transmission.loss.df.add.temp$fit <- predict(temperature.model)   #Add model fits to dataframe


ggplot(temp.transmission.loss.df.add.temp,aes(x=temperature,y=transmission.loss,group=dist.from.source, col=dist.from.source)) + 
  #facet_grid(~date.classifier) +
  geom_line(aes(y=fit)) +
  geom_jitter(width = 0.75, height = 0.75)+
  geom_point(alpha = 0.3) + 
  theme_bw() +xlab("Temperature (C)") + ylab("delta SNR (dB)")+ 
  guides(colour=guide_legend(title="Distance to source (m)"))+ #ylim(-25,0)+
  scale_color_manual(values = viridis(n=length(unique(temp.transmission.loss.df.add.temp$dist.from.source )))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))


