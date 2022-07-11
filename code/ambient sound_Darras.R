rm(list=ls())
library(data.table)
library(seewave)
library(tuneR)

foldername="C:/Users/KD/Documents/Boulot/2012_Jambi/"
# foldername="D:/Docs/Boulot/2012_Jambi/"

#duration of audio slices in seconds to process (120 seconds of 44100 Hz WAV works)
slice_duration=120
#choose files from specific collections
collection="ambient US"
channel=c("left","right")
#create empty df
ambient=NULL

#source medianspec custom script
source(paste(foldername,"Detection spaces/medianspec.R",sep=""))

#import plot codes
codes0=fread(paste(foldername,"Data/Recordings info.csv",sep=""))
frequency.scale=fread(paste(foldername,"Detection spaces/frequency scale.csv",sep=""))

codes=codes0[Ambient_sound=="yes" & Exclude_channel!="both" & Collection==collection]

#initialize progress bar
pb=txtProgressBar(min=0,max=nrow(codes),style=3)

#index loop
for (j in 1:nrow(codes)){
  filepath_flac=paste(foldername,codes[j,Path_from_root],codes[j,Filename],sep="")
  filepath_wav=paste(substr(filepath_flac,1,nchar(filepath_flac)-5),".wav",sep="")
  #convert to WAV
  wav2flac(filepath_flac,reverse=T,path2exe="C:/Program Files/FLAC")
  
  #there are 2 bytes per sample and 2 channels, so sampling frequency is multiplied with 4
  duration_seconds=round(file.info(filepath_wav)$size/(codes[j,Sampling_frequency]*4))
  
  factor_slice=(slice_duration*(round(codes[j,Sampling_frequency]/44100)))/duration_seconds
  
  for (i in 1:(1/factor_slice)){
    sound0=readWave(filepath_wav,from=(i-1)*duration_seconds*factor_slice
                    ,to=i*duration_seconds*factor_slice,units="seconds")
    freq=sound0@samp.rate
    
      for (c in channel){
        if (c=="left" & codes[j,Exclude_channel]!="left") {microphone=codes[j,Left_mic]
        
        if (substr(microphone,1,1)=="U") {freq_scale=frequency.scale[microphone.type=="SMXUS",frequency]
        microphone.type="SMXUS"} else {freq_scale=frequency.scale[microphone.type=="SMXII",frequency]
        microphone.type="SMXII"}
        
        } else if (c=="right" & codes[j,Exclude_channel]!="right"){microphone=codes[j,Right_mic]
        
        if (substr(microphone,1,1)=="U") {freq_scale=frequency.scale[microphone.type=="SMXUS",frequency]
        microphone.type="SMXUS"} else {freq_scale=frequency.scale[microphone.type=="SMXII",frequency]
        microphone.type="SMXII"}
        
        }
        sound.mono=mono(sound0,c)
        
        #must not be normalized! When normalized maximum sound level always = 73.98 dB
        spectrum=as.data.frame(medianspec(sound.mono,f=freq,main=paste(codes$Plot[j],c,i),norm=F))
        spectrum$SPL=20*log10(spectrum$y/(2*10e-5))
        
        spectrum.adjust=approx(spectrum$x,spectrum$SPL,xout=freq_scale)
        
        #bind all data into data frame
        ambient=rbind(ambient,cbind(iteration=paste(j,",",i,sep="")
                                    ,Plot=codes[j,Plot]
                                    ,frequency=freq_scale
                                    ,ambient.noise.uncalibrated=spectrum.adjust$y
                                    ,microphone=microphone
                                    ,microphone.type=microphone.type
                                    ,channel=c
                                    ,date=codes[j,Date]
                                    ,Time=codes[j,Time]
                                    ,duration=duration_seconds/factor_slice))
      }
    }
  file.remove(filepath_wav)  
  setTxtProgressBar(pb,j)
  }

rm(sound0)
ambient1=data.table(ambient)
ambient1[,frequency:=as.numeric(frequency)]

ambient1[,ambient.noise.uncalibrated:=as.numeric(ambient.noise.uncalibrated)]

#import microphone sensitivity, acoustic and ultrasonic tables bound
sensitivity=fread(paste(foldername,"Detection spaces/microphone sensitivity.csv",sep=""))

#filter out night recordings recorded by SMX-II microphone
#there are DUPLICATES IN AMBIENT1!
ambient2=merge(ambient1[!(microphone.type=="SMXII" & Time=="sunset"),],sensitivity
               ,all.x=T,by=c("microphone","frequency","microphone.type"),allow.cartesian = T)

#count duplicates
ambient.sub=ambient1[,.(count.iterations=.N)
                         ,by=.(Plot,frequency,microphone,microphone.type,iteration)]

#microphones X22, X03 and U09 are not calibrated yet, assuming they are as sensitive as the mean
mean.sensitivity.SMXUS=ambient2[microphone.type=="SMXUS"
                                ,.(sensitivity.mean=mean(sensitivity,na.rm=T)
                                   ,microphone="U09"),.(frequency,microphone.type)]

mean.sensitivity.SMXIIX22=ambient2[microphone.type=="SMXII"
                                ,.(sensitivity.mean=mean(sensitivity,na.rm=T)
                                   ,microphone="X22"),.(frequency,microphone.type)]
mean.sensitivity.SMXIIX03=ambient2[microphone.type=="SMXII"
                                   ,.(sensitivity.mean=mean(sensitivity,na.rm=T)
                                      ,microphone="X03"),.(frequency,microphone.type)]

ambient3=merge(ambient2,rbind(mean.sensitivity.SMXUS,rbind(mean.sensitivity.SMXIIX03,mean.sensitivity.SMXIIX22)),all.x=T
                ,by=c("microphone","microphone.type","frequency"))
ambient3[microphone=="U09" | microphone=="X22",sensitivity:=sensitivity.mean]
ambient3[microphone=="U09" & frequency!=40,value:="extrapolated"]
ambient3[microphone=="U09" & frequency==40,value:="measured"]

ambient3[microphone=="X22" & frequency %in% frequency.scale[value=="measured",frequency]
         ,value:="measured"]
ambient3[microphone=="X22" & frequency %in% frequency.scale[value=="extrapolated",frequency]
         ,value:="extrapolated"]
ambient3[microphone=="U09" & frequency<20,response.certain:=0]
ambient3[microphone=="U09" & frequency>=20,response.certain:=1]
ambient3[microphone=="X22" & frequency>15,response.certain:=0]
ambient3[microphone=="X22" & frequency<=15,response.certain:=1]


# calibrate sound recordings ----------------------------------------------

# to make them comparable to other studies following Merchant et al. 2015 MEE
#from SM2 plus manual: "SMX-II Sensitivity: -36?4dB (0dB=1V/pa@1KHz) -> VADC = 1.41 V
#ADC: 1V rms full-scale 16-bit
#default pre-amplifier gain on SM2+ units is 48dB
ambient3[,ambient.noise:=ambient.noise.uncalibrated-(sensitivity+48+20*log10(1/1.41)+20*log10(2^(16-1)))]
#for rows that do not have a microphone, remove sensitivity correction
ambient3[is.na(sensitivity),ambient.noise:=ambient.noise.uncalibrated-(48+20*log10(1/1.41)+20*log10(2^(16-1)))]

#compute mean ambient sound levels for two channels combined
ambient.summary0=ambient3[,.(ambient.noise=meandB(ambient.noise))
                           ,by=.(Plot,frequency,Time,value,microphone.type,response.certain)]
  
write.table(ambient3,paste(foldername,"Detection spaces/ambient sound raw ",collection,".csv",sep=""),sep=",",row.names=F)
write.table(ambient.summary0,paste(foldername,"Detection spaces/ambient sound ",collection,".csv",sep=""),sep=",",row.names=F)