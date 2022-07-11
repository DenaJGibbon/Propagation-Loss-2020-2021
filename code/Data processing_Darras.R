#clear all variables
rm(list=ls())

library(data.table)
library(seewave)

#custom function for taking average of logarithms (needs to be checked)
logmean=function(x){
  log(mean(exp(x)))
}

#set the foldername to the path where you have all scripts and data files
foldername="D:/Docs/Boulot/2012_Jambi/"
# foldername="C:/Users/KD/Documents/Boulot/2012_Jambi/"

#defining constants
#distance at which the sound falloff slope (transmission) is calculated
far_distance=100
#reference sound level to standardize all sound levels
ref_amp=80

#load frequency scale
frequency.scale=fread(paste(foldername,"Detection spaces/frequency scale.csv",sep=""))

#import correction coefficients for angular loudspeaker directivity, measured separately for 0 and 5 meter height (offset angle up and down)
directivity.acoustic=fread(paste(foldername,"Detection spaces/loudspeaker directivity/results acoustic directivity.csv",sep=""))
directivity.ultrasonic=fread(paste(foldername,"Detection spaces/loudspeaker directivity/results ultrasonic directivity.csv",sep=""))
directivity0=rbind(directivity.acoustic,directivity.ultrasonic)

#find mean amplitude at each angle and frequency
directivity1=directivity0[,.(signal_amp=mean(as.numeric(signal_amp)))
                          ,.(angle=as.numeric(angle),frequency)]

#find out signal amplitude at 0 degrees
directivity.max=directivity1[angle==0,.(signal_amp_0=max(signal_amp))
                             ,.(frequency)]
directivity2=merge(directivity1,directivity.max,by="frequency")

#standardize signal amplitude relative to measurement at 0 degrees
directivity2[,signal_amp_standard:=signal_amp-signal_amp_0]

#export for graph plotting
write.table(directivity2,paste(foldername,"Detection spaces/loudspeaker directivity/results directivity.csv",sep=""),row.names = F,sep=",")

for (f in unique(as.numeric(directivity2[,frequency]))){
  directivity.interpolated=approx(directivity2[frequency==f,angle],directivity2[frequency==f,signal_amp_standard],xout=seq(1,90,1))
  directivity2=rbind(directivity2,data.table(frequency=f,angle=directivity.interpolated$x
                                             ,signal_amp=NA,signal_amp_0=NA
                                             ,signal_amp_standard=directivity.interpolated$y))
}
directivity3=directivity2[,.(frequency,angle,signal_amp_standard)]
directivity3[,frequency:=as.numeric(frequency)]

#import sound level results
results0=fread(paste(foldername,"Detection spaces/raw sound levels.csv",sep=""))

#import measurement times
SPinfo1=fread(paste(foldername,"Detection spaces/Measurement times.csv",sep=""))

#remove wrongly measured signals
results1=results0[quality_check!=0,-c("quality_check","iteration","duration"),with=F]

#calculate logarithm of direct distance
results1[,log_ddistance:=log((distance^2+(height-2)^2)^(1/2))]

#calculate angle with horizontal at heights 0.1 and 5m, correct for signal level loss due to offset angle
results1[,angle:=round(57.2957795*atan(abs(height-2)/distance))]

#merge with directivity correction values
results2=merge(results1,directivity3,all.x=T,by=c("frequency","angle"))
#correct for directivity loss
results2[,signal.dir:=signal-signal_amp_standard]

#append landuse system information: plot letters denote landscape and land-use
results2[,landuse:=factor(substr(plot,2,2)
                          ,levels=c("F","J","R","O","E","C")
                          ,labels=c("Forest","Jungle rubber","Rubber","Oil palm","Young oil palm","Bare land"))]

#use separately measured ambient sound levels for all land-uses except for bare land and young oil palm
ambient0=fread(paste(foldername,"Detection spaces/ambient sound.csv",sep=""))

# direction -1=back, +1=front
results2[,num_direction:=1]
results2[direction=="b",num_direction:=-1]

#set rows where acoustic signal.dir<background +4 to NA
#ultrasound was checked manually so no need to filter them out
results2[frequency!=40 & signal.dir<background+4,signal.dir:=NA]

# #optional signal quality and clipping visualization
# library(ggplot2)
# ggplot(results0[height==2,],aes(factor(quality_check)))+
#   geom_bar(stat="bin")+
#   facet_grid(frequency~distance)
# 
# ggplot(results2[results2$height==2,],aes(factor(signal_clipped)))+
#   geom_bar(stat="bin")+
#   facet_grid(frequency~distance)

#filter out rows of clipped signals
results3=results2[signal_clipped==0,]

# model of sound falloff --------------------------------------------------

transmission0=data.table()
pb=txtProgressBar(min=0,max=length(unique(results3$plot)),style=3)

#compare linear models vs log models
linvslog=NULL

#single models in loop are faster than one huge model and avoids RAM overload issues
for (p in unique(results3[,plot])){
  for (h in unique(results3[plot==p,height])){
    for (d in unique(results3[plot==p & height==h,num_direction])){
      for (f in unique(results3[plot==p & height==h & num_direction==d,frequency])){
        #subset of data
        results.subset=results3[plot==p & height==h & num_direction==d & frequency==f,]
        #the lm includes channel as a dummy, as only the intercept should be affected by the microphone
        lm.part1=lm(signal.dir~log_ddistance+channel,results.subset)
        
        slope=as.numeric(coef(lm.part1)[2])
        pred.signal_0m=as.numeric(coef(lm.part1)[1])
        transmission0=rbind(transmission0,data.table(plot=p,landuse=as.character(results.subset$landuse[1])
                                                     ,height=h,num_direction=d,frequency=f,transmission=slope,pred.signal_0m=pred.signal_0m,value="measured"))
      }
      #results for frequencies not measured with pure tones are extrapolated
      extrapolated0=approx(as.numeric(as.character(transmission0[height==h & plot==p & num_direction==d,frequency]))
                           ,as.numeric(as.character(transmission0[height==h & plot==p & num_direction==d,transmission]))
                           ,xout=unique(frequency.scale[value=="extrapolated",frequency]),rule=2)
      
      extrapolated1=data.table(cbind(plot=p,landuse=as.character(results.subset[1,landuse])
                                     ,height=h,num_direction=d,frequency=extrapolated0$x,transmission=extrapolated0$y,pred.signal_0m=NA,value="extrapolated"))
      transmission0=rbind(transmission0,extrapolated1)
    }
  }
  setTxtProgressBar(pb,which(unique(results3$plot)==p))
}

transmission0[frequency!=40,frequency_type:="audible sound"]
transmission0[frequency==40,frequency_type:="ultrasound"]
transmission0[,num_direction:=as.numeric(num_direction)]

#defining formats
transmission0[,pred.signal_0m:=as.numeric(pred.signal_0m)]
transmission0[,transmission:=as.numeric(transmission)]
transmission0[,frequency:=as.numeric(frequency)]

#calculate offset from reference sound level for standardizing
transmission0[,diff_ref:=pred.signal_0m-ref_amp]

#standardize the predicted sound levels
transmission0[,stan_pred.signal_far:=pred.signal_0m-diff_ref+(transmission*log(far_distance))]

#landuse as factor
transmission0[,landuse:=factor(landuse
                               ,levels=c("Forest","Jungle rubber","Rubber","Oil palm","Young oil palm","Bare land"))]

#append landscape information
transmission0[,landscape:=factor(substr(plot,1,1)
                                 ,levels=c("B","H"),labels=c("Bukit 12","Harapan"))]

#take mean of ambient noise and filter out sunrise ultrasound measurements from young oil palm
# because they were measured by more accurate SMXII mics too
ambient1=ambient0[!(substr(Plot,2,2)=="E" & Time=="sunrise" & microphone.type=="SMXUS" & is.na(ambient.noise))
                  ,.(ambient.noise=meandB(ambient.noise))
                  ,.(plot=Plot,frequency,Time,value)]

transmission1=rbind(transmission0,transmission0)
transmission1[,Time:="sunrise"]
transmission1[nrow(transmission0):nrow(transmission1),Time:="sunset"]

#attaching ambient sound values and filling in the blank reference signal amplitude values
detection.spaces0=merge(transmission1,ambient1,all.x=T,by=c("plot","frequency","value","Time"))

#calculating extinction distance and detection space area
detection.spaces0[,extinction:=(ambient.noise-ref_amp)/transmission]
detection.spaces0[,area_ha:=((exp(extinction))^2*pi)/10000]

# calculating expected species based on species-area models ---------------
babbler=detection.spaces0$height==2 & detection.spaces0$frequency==3 & detection.spaces0$Time=="sunrise"
babbler[which(is.na(babbler))]=F
frog=detection.spaces0$height==0.1 & detection.spaces0$frequency==1.3 & detection.spaces0$Time=="sunset"
frog[which(is.na(frog))]=F
bat=detection.spaces0$height==5 & detection.spaces0$frequency==40 & detection.spaces0$Time=="sunset"
bat[which(is.na(bat))]=F
cicada=detection.spaces0$height==5 & detection.spaces0$frequency==14 & detection.spaces0$Time=="sunrise"
cicada[which(is.na(cicada))]=F

detection.spaces0[babbler,species:="Malacocincla malaccensis"]
detection.spaces0[bat,species:="Tylonycteris robustula"]
detection.spaces0[cicada,species:="cicada"]
detection.spaces0[frog,species:="frog"]

#using 100dB for frog, mean of 5 south american species
detection.spaces0[frog,extinction_species:=(ambient.noise-100)/transmission]
detection.spaces0[frog,area_ha_species:=((exp(extinction_species))^2*pi)/10000]

#using 71 dB for Malacocincla malaccensis, measured from sample recording
detection.spaces0[babbler,extinction_species:=(ambient.noise-71)/transmission]
detection.spaces0[babbler,area_ha_species:=((exp(extinction_species))^2*pi)/10000]

#using 59 dB for Tylonycteris robustula, measured from sample recording
detection.spaces0[bat,extinction_species:=(ambient.noise-59)/transmission]
detection.spaces0[bat,area_ha_species:=((exp(extinction_species))^2*pi)/10000]

#using cicada call reference 97 dB
detection.spaces0[cicada,extinction_species:=(ambient.noise-97)/transmission]
detection.spaces0[cicada,area_ha_species:=((exp(extinction_species))^2*pi)/10000]

#deriving species numbers
#MacLean W.P, Kellner R. & Dennis H. (1977) Island lists of West Indian amphibians and reptiles. Smithsonian Herpetol. Info. Serv. 40.
detection.spaces0[frog,pred_species:=(area_ha_species*10000)^0.41]

#Brooks T.M., Pimm S.L., Kapos V. & Ravilious C. (1999). Threat from deforestation to montane and lowland birds and mammals in insular South-east Asia. J. Anim. Ecol., 68, 1061-1078
detection.spaces0[babbler,pred_species:=(area_ha_species*10000)^0.42]

#bats most relevant model from Koopman (1958), best fit with species/area model (See McCoy 1979)
detection.spaces0[bat,pred_species:=(area_ha_species*10000)^0.08]

#Davies N. & Smith D.S. (1998). Munroe revisited: A survey of West Indian butterfly faunas and their species-area relationship. Global Ecol. Biogeogr. Lett., 7, 285-294
detection.spaces0[cicada,pred_species:=(area_ha_species*10000)^0.2]

#merging mean added species numbers across plots per height and frequency with data frame
#remove HE3 whose ambient sound was measured with an uncalibrated microphone
detection.spaces1=merge(detection.spaces0[plot!="HE3"]
                        ,detection.spaces0[plot!="HE3",.(mean_pred_species=mean(pred_species,na.rm=T))
                                           ,.(frequency,height,species,Time)]
                        ,all.x=T,by=c("frequency","height","species","Time"))

#calculating species number relative to mean
detection.spaces1[,rel_pred_species:=pred_species/mean_pred_species]

#merging with sound transmission measurement time data
detection.spaces2=merge(detection.spaces1,SPinfo1,all.x=T,by=c("plot","num_direction","frequency_type"))

#calculate excess attenuation
theoretical_attenuation=ref_amp-20*log10(far_distance-1)
detection.spaces2[,excess_attenuation:=theoretical_attenuation-stan_pred.signal_far]

#include vegetation data
plants=fread(paste(foldername,"Detection spaces/vegetation data.csv",sep=""))
#merge vegetation data
detection.spaces3=merge(detection.spaces2,plants,by.x="plot",by.y="plotID",all.x=T)

#take mean of two directions for subsequent statistical analyis
detection.spaces.agg=detection.spaces3[,.(transmission=mean(transmission,na.rm=T)
                                          ,ambient.noise=meandB(ambient.noise)
                                          ,excess_attenuation=meandB(excess_attenuation)
                                          ,extinction=logmean(extinction)
                                          ,extinction_species=logmean(extinction_species)
                                          ,area_ha=mean(area_ha,na.rm=T)
                                          ,tree_density=mean(trees_quarter_ha,na.rm=T)*4
                                          ,basal_area_m2_ha=mean(basal_area_m2_ha,na.rm = T)
                                          ,tree_height_m=mean(tree_height_m,na.rm = T)
                                          ,crown_base_height_m=mean(crown_base_height_m,na.rm = T)
                                          #five 5x5 subplots, scaled to hectare
                                          ,understory_density=mean(understory_abundance,na.rm = T)/(125*10000)
                                          ,understory_height_m=mean(understory_height_m,na.rm = T)
                                          ,pred_species=mean(pred_species,na.rm = T)
                                          ,rel_pred_species=mean(rel_pred_species,na.rm = T))
                                       ,.(plot,frequency,height,landuse,value,landscape,Time,species)]

#computing sound levels standardized to reference level for diagnostic plots
diff_ref1=detection.spaces3[,.(diff_ref=mean(diff_ref,na.rm=T))
                            ,.(plot,height=as.numeric(height),num_direction,frequency)]
results4=merge(results3,diff_ref1,by=c("plot","height","num_direction","frequency"))
results4[,stan_signal.dir:=signal.dir-diff_ref]

#output results
write.table(results4,paste(foldername,"Detection spaces/results.csv",sep=""),sep=",",row.names=F)
write.table(detection.spaces3,paste(foldername,"Detection spaces/transmission raw.csv",sep=""),sep=",",row.names=F)
write.table(detection.spaces.agg,paste(foldername,"Detection spaces/detection_spaces_agg.csv",sep=""),sep=",",row.names=F)