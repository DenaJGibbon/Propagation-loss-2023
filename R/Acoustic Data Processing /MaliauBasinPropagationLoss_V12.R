# This is the R code to estimate propagation loss in Borneo 


# Version summary ---------------------------------------------------------
# Version 2. Add adaptive noise estimates
# Version 3. Modify noise calculations so divide into ten 1-sec bins
# Version 4. Remove adaptive noise estimates
# Version 5. Ignores playbacks that overlap two recordings.
# Version 6. Add playbacks over two recordings; filter before downsample to prevent aliasing
# Version 7. Remove all orangutan Sounds and upsweeps to focus only on gibbons
# Version 8. Add back adaptive noise
# Version 9. Shift frequency of trill down to avoid background noise
# Version 10. Add signal without background noise removed; remove .25 quantile noise
# Version 11. Add back in P. morio; clean up for publication
# Version 12. Modify so code returns comparable values to PAMGuide

# Part 1. Load necessary packages -------------------------------------------------------------
library(seewave)
library(tuneR)
library(stringr)
library(plyr)
library(ggpubr)
library(geosphere)
library(XML)
library(gpx)


# Part 2. Set up data -------------------------------------------------------------

# Sound file location - NEED TO CHANGE THIS FOR YOUR MACHINE
SoundFiles.input <- 
  '/Users/denaclink/Library/CloudStorage/Box-Box/CCB Datastore/Projects/2018/2018_BRP_Borneo_T0046/Clink_BRP_3TB/2019 Maliau Basin/Playbacks_50m'

# Playback template table
SelectionIDsMaliau <- 
  read.delim("data/SelectionLabels_S00974_20190811_101922_updated_april2024.txt")

# Separate sound type character
TempSoundType <- 
  str_split_fixed(SelectionIDsMaliau$Sound.Type, pattern = '_',n=3)[,2]

# Take the first two letters to identify by species
TempSoundType <- substr(TempSoundType,start = 1,stop=2)

# Identify which sound types to keep
SoundsToRemove <- which(TempSoundType!="Hf" & TempSoundType!="Ha" & TempSoundType!="Pm") # & TempSoundType!="Pm"

# Create a sequency for each row
PlaybackSeq <- seq(1,nrow(SelectionIDsMaliau),1)

# Remove rows that we are not foucsing on
SelectionIDsMaliau <- SelectionIDsMaliau[-SoundsToRemove,]

# Create list of sound files
SoundFiles.input.list <- list.files(SoundFiles.input,full.names = T,recursive = T)

# Selection table location
input.dir <- 
  "data/Maliau Basin Selection Tables"

# Read in GPS function
source('R/readGPX.R')

# Read in GPS points
recorder.gps <- readGPX("data/MB Playbacks 50 m.GPX") 

# Set duration of the noise selection before the start of the actual selection
timesecs <- 5

# Set a buffer duration before and after selection to calculate inband power
# NOTE: Right now it is set to zero so it cuts the selection right at the boundaries of the selection table
signal.time.buffer <- 0

# Set the duration of the noise subsamples (in seconds)
noise.subsamples <- 0.25

# Set microphone gain
gain <- 40

# Calculate the microphone sensitivity for the system
Sensitivity <- (-44-120) + gain+ 20*log10(1/0.9) # dB relative to 1 volt per pascal

# Set distance (hypotenuse) of first recorder to playback speaker (in meters)
dist.to.playback <- 26.4

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

# Check output to make sure it looks right
head(combined.template.table.test)

# Part 3. Create distance matrix based on GPS points ---------------------------------------------------

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
dput((dist.mat+dist.to.playback)[,1])

# Combine all into a single dataframe
BackgroundNoiseRemovedDFMaliau.test <- merge(combined.template.table.test,small.gps.df,by='recorder')

# Add a date column
BackgroundNoiseRemovedDFMaliau.test$date <- str_split_fixed(BackgroundNoiseRemovedDFMaliau.test$file.name,pattern = '_',n=3)[,2]

# Add a time column
time.temp <- str_split_fixed(BackgroundNoiseRemovedDFMaliau.test$file.name,pattern = '_',n=3)[,3]
BackgroundNoiseRemovedDFMaliau.test$time <- str_split_fixed(time.temp,pattern = '.txt',n=2)[,1]
BackgroundNoiseRemovedDFMaliau.test$time <- substr(BackgroundNoiseRemovedDFMaliau.test$time,start=1,stop=4)

# Check structure of resulting data frame
head(BackgroundNoiseRemovedDFMaliau.test)


# Part 4. Calculate absolute receive levels for each selection --------------------

# Create an index for each file
file.name.index <- unique(BackgroundNoiseRemovedDFMaliau.test$file.name)

# Determine date only
date.only <- str_split_fixed(file.name.index,pattern = '_',n=2)[,2] 

# Combine data and file name
file.name.date <- cbind.data.frame(file.name.index,date.only)                

# Order by date
file.name.date.reorder <- file.name.date[order(file.name.date$date.only), ]

# Sorty by date
file.name.index.sorted <- file.name.date.reorder$file.name.index

# Remove recordings with gibbons calling and from 25-m spacing line
file.name.index.sorted <- file.name.index.sorted[-c(1:4,167:202)]

# Identify times from file name
times <- 
  str_split_fixed(file.name.index.sorted,pattern = '_',n=3)[,3]

# Create hour vector
times <- substr(times,start=3,stop=4)

# Determine number of seconds in recording to identify if need to stitch together
duration.secs <- 40*60

# Create an empty dataframe to add to iteratively in the loop
BackgroundNoiseRemovedDFMaliau <- data.frame()

# The loop to calculate inband power (after subtracting the noise) for each selection from the wave file
for(b in 1:length(file.name.index.sorted)){ tryCatch({ 
  print(paste('processing sound file',file.name.index.sorted[b] ))
  
  # Subset by recorder and date index
  singleplayback.df <- subset(BackgroundNoiseRemovedDFMaliau.test,file.name==file.name.index.sorted[b])
  
  singleplayback.df <-singleplayback.df[-SoundsToRemove,]
 
  # Create sound file path
  soundfile.path <-  SoundFiles.input.list[str_detect(SoundFiles.input.list,singleplayback.df$file.name[1])]
  
  # Count number of slashes
  nslash <- str_count(soundfile.path,'/')
  
  # Determine end time
  Selection.end.time <- 
    singleplayback.df[nrow(singleplayback.df),]$End.Time..s.
  
  if(Selection.end.time > duration.secs ){
    soundfile.path.index <-  which(str_detect(SoundFiles.input.list,file.name.index.sorted[b]))
    short.wav <- str_split_fixed(soundfile.path,'/',nslash+1)[,nslash+1]
    # Read in the long .wav file
    wavfile.temp1 <- tuneR::readWave(soundfile.path)
    
    # Find wav file adjacent to link them
    adjacent.path <- SoundFiles.input.list[soundfile.path.index+1]
    wavfile.temp2 <-  tuneR::readWave(adjacent.path)
    
    filteredwaveformdownsample1<- bwfilter(wavfile.temp1, 
                                          to=18000,
                                          n=3,
                                          output = 'Wave')
    wavfile.temp1@left <-  c(filteredwaveformdownsample1@left)
    
    filteredwaveformdownsample2<- bwfilter(wavfile.temp2, 
                                          to=18000,
                                          n=3,
                                          output = 'Wave')
    wavfile.temp2@left <-  c(filteredwaveformdownsample2@left)
    
    # Combine both
    wavfile.temp1@left <- c(wavfile.temp1@left,wavfile.temp2@left)
    wavfile.temp <-  wavfile.temp1
    print('Combined two wave files')
    rm(wavfile.temp1)
    rm(wavfile.temp2)
    rm(filteredwaveformdownsample1)
  } else {
  
  short.wav <- str_split_fixed(soundfile.path,'/',nslash+1)[,nslash+1]
  # Read in the long .wav file
  wavfile.temp <- tuneR::readWave(soundfile.path)
  
  # Filter before downsample to prevent aliasing
  filteredwaveformdownsample<- bwfilter(wavfile.temp, 
                                        to=18000,
                                        n=3,
                                        output = 'Wave')
  wavfile.temp@left <-  c(filteredwaveformdownsample@left )
  
  # Downsample so that comparable with C. Kalimantan recordings
  wavfile.temp <- tuneR::downsample(wavfile.temp,16000)
  rm(filteredwaveformdownsample)
  }
  
  # Normalize around mean to remove offset
  wavfile.temp@left <- wavfile.temp@left- mean(wavfile.temp@left)
  
  # Check to make sure number of selections matches the template
  if(nrow(singleplayback.df)==nrow(SelectionIDsMaliau)){
    
  which.trill <- 
    which(str_detect(SelectionIDsMaliau$Sound.Type,'trill') ) 
  
  singleplayback.df$High.Freq..Hz.[which.trill] <- '1425'
  
  singleplayback.df$High.Freq..Hz. <- as.numeric(singleplayback.df$High.Freq..Hz.)
  
  print('Calculating receive levels')
  
  # Use the Raven selection table to cut each selection into an individual .wav file
  ListofWavs <- 
    lapply(1:nrow(singleplayback.df), function(x) cutw(wavfile.temp, from= (singleplayback.df$Begin.Time..s.[x]- signal.time.buffer),
                                                                 to= (singleplayback.df$End.Time..s.[x]+signal.time.buffer), output='Wave'))


  #writeWave(ListofWavs[[1]],'TestWavGibbon.wav', extensible = F)
  # Matches each selection with the corresponding noise and selection .wav file and calculate absolute receive level
  for(d in 1:nrow(singleplayback.df)){
    
    print(d)
    
    # Subset the correspond row from the selection table
    Selectiontemp <- singleplayback.df[d,]
    
    Selectiontemp$Sound.Type <-  SelectionIDsMaliau[d,]$Sound.Type
    
    # Use the Raven selection table to extract a longer duration .wav file for noise
    NoiseWav1 <- cutw(wavfile.temp, from= (Selectiontemp$Begin.Time..s.-timesecs),
                      to=Selectiontemp$Begin.Time..s.[1], output='Wave')
    
    NoiseWav2 <- cutw(wavfile.temp, from= Selectiontemp$End.Time..s.,
                      to= Selectiontemp$End.Time..s.[1]+timesecs, output='Wave')
    
    NoiseWavList <- list(NoiseWav1,NoiseWav2)
    
    noise.value.list <- list()
    noise.location.list <- list()
    for(e in 1:length(NoiseWavList)){
    
    # Take the corresponding noise file
    NoiseWavetemp <- NoiseWavList[[e]]
   
   # Filter to the frequency range of the selection
   filteredwaveform<- bwfilter(NoiseWavetemp, 
                               from=Selectiontemp$Low.Freq..Hz., 
                               to=Selectiontemp$High.Freq..Hz.,
                               n=3,
                               output = 'Wave')
   
   # Add the filtered waveform back into the .wav file
   NoiseWavetemp@left <- c(filteredwaveform@left)
   
   # Assign a new name
   w.dn.filt <- NoiseWavetemp
   
   # Calculate the duration of the sound file
   dur.seconds <- seewave::duration(w.dn.filt)
   
   # Divide into evenly spaced bins (duration specified above)
   bin.seq <- seq(from=0, to=dur.seconds, by=noise.subsamples)
   
   # Create a list of all the noise subsample bins to estimate noise
   bin.seq.length <- length(bin.seq)-1
   
   # Create a list of shorter sound files to calculate noise
   subsamps.1sec <- lapply(1:bin.seq.length, function(i) 
     extractWave(w.dn.filt, 
                 from=as.numeric(bin.seq[i]), to=as.numeric(bin.seq[i+1]), 
                 xunit = c("time"),plot=F,output="Wave"))
   
   
   # Calculate noise for each noise time bin 
   noise.list <- list()
   for (k in 1:length(subsamps.1sec)) { 
     
     # Read in .wav file 
     temp.wave <- subsamps.1sec[[k]]
     
     # Normalise the values 
     data <- temp.wave@left/ (2 ^ 16/2)
     
     # Calibrate the data with the microphone sensitivity
     data_cal <- data/ (10^(Sensitivity/20))
     
     # Calculate RMS
     data_rms <- rms(data_cal)
     
     noise.list[[k]] <- data_rms
     
   }
   
  
   # Take the minimum value from the noise samples
   noise.value.list[[e]] <- quantile(unlist(noise.list),.25)
   noise.location.list[[e]] <- bin.seq[which(unlist(noise.list) <= quantile(unlist(noise.list),.25))]
    }
   
    
   noise.value <- min(unlist(noise.value.list))/20
   
   
   noise.index <- which.min(unlist(noise.value.list))
   wavdur <-  Selectiontemp$End.Time..s.- Selectiontemp$Begin.Time..s.

   # Isolate the corresponding .wav file for the playback selection
   SignalWavtemp <-  ListofWavs[[d]]
      
   # Filter to the frequency range of the selection
   w.dn.filt <- bwfilter(SignalWavtemp, 
                         from=Selectiontemp$Low.Freq..Hz., 
                         to=Selectiontemp$High.Freq..Hz.,n=3,
                         output = 'Wave')
     
       
      # Normalise the values 
      data <- w.dn.filt@left/ (2 ^ 16/2)
      
      # Calibrate the data with the microphone sensitivity
      data_cal <- data/ (10^(Sensitivity/20))
      
      # Calculate RMS
      data_rms <- rms(data_cal)
      
      # Convert to dB
      signal.value <- data_rms
      
      # Calculate absolute receive level of the signal in the selection in dB (subtracting noise)
      Selectiontemp$PowerDb <- 20 * log10((signal.value-noise.value))
      
      # Calculate absolute receive level of the signal in the selection in dB (not subtracting noise)
      Selectiontemp$PowerDbWithNoise <- 20 * log10((signal.value))
      
      # Calculate noise level in dB
      Selectiontemp$NoisevalueDb <- 20 * log10((noise.value))
      
      Selectiontemp$Sound.Type <-  SelectionIDsMaliau[d,]$Sound.Type
      
      # Print the output
      print(Selectiontemp)
      
       # WavName <-paste(Selectiontemp$file.name, Selectiontemp$recorder,Selectiontemp$Sound.Type,Selectiontemp$Selection,
       #                 round(Selectiontemp$PowerDb,1), sep='_')
       # NewWavName <-paste('/Volumes/DJC Files/PropLossClips/Maliau/',WavName,'.wav',sep='')
       # writeWave(ListofWavs[[d]],filename = NewWavName, extensible = F)
      # Combine into a dataframe
      BackgroundNoiseRemovedDFMaliau <- rbind.data.frame(BackgroundNoiseRemovedDFMaliau,Selectiontemp)
      write.csv(BackgroundNoiseRemovedDFMaliau,'data/MaliauPropLossApril2024test.csv',row.names = F)
  }
  
  }
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
rm(wavfile.temp)
rm(ListofWavs)
rm(NoiseWav1)
rm(NoiseWav2)
rm(NoiseWavList)
rm(filteredwaveform)
rm(filteredwaveformdownsample)
rm(filteredwaveformdownsample2)
}


BackgroundNoiseRemovedDFMaliau <- read.csv("data/MaliauPropLossApril2024.csv")

# Part 5. Propagation Loss --------------------------------------------------------

# Create an index with unique date/time combinations
date.time.combo <- paste(BackgroundNoiseRemovedDFMaliau$date,BackgroundNoiseRemovedDFMaliau$time,sep='_')
unique.date.time.combo <- unique(date.time.combo)

# Create empty dataframe for propagation loss
observed.prop.lossMaliau <- data.frame()

# Loop to calculate propagation loss
for(z in 1:length(unique.date.time.combo)) { tryCatch({ 
  
  # Subset by unique date/time index
  temp.date.time.subset <- 
    str_split_fixed(unique.date.time.combo[z],pattern = '_',n=2) 
  
  # Subset data frame to focus on unique date/time
  temp.playback <- subset(BackgroundNoiseRemovedDFMaliau, date==temp.date.time.subset[,1] & time==temp.date.time.subset[,2])
  
  # See how many unique playbacks
  unique(temp.playback$file.name)
  
  # Create an index for each unique file in the playback
  file.index <- unique(temp.playback$file.name)
  SelectionIndex <- (SelectionIDsMaliau$Sound.Type)
  
  # This isolates each selection in the original template one by one
  for(a in 1:length(SelectionIndex)){
    
    # Subset the same selection from each of the recorders
    small.sample.playback.test <- data.frame()
    for(b in 1:length(file.index) ){
      temp.table <- subset(temp.playback,file.name==file.index[b])
      temp.table$Sound.Type <- SelectionIDsMaliau$Sound.Type
      temp.table <- temp.table[a,]
      small.sample.playback.test <- rbind.data.frame(small.sample.playback.test,temp.table )
    }
    
    # Create an index for each unique recorder in the new subset dataset
    recorder.index.test <- unique(small.sample.playback.test$recorder)
    
    # Create a new column with receive levels standardized so the closest recorder is 0
    small.sample.playback.test$PowerDb.zero <- 
      small.sample.playback.test$PowerDb-small.sample.playback.test$PowerDb[1]
    
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

# Take the median value for replicate selections
RecorderDateTime.index <- unique(paste(observed.prop.lossMaliau$distance,
                                       observed.prop.lossMaliau$date,
                                       observed.prop.lossMaliau$time,sep='_'))

observed.prop.lossMaliau$RecorderDateTime <- unique(paste(#observed.prop.lossMaliau$distance,
                                       observed.prop.lossMaliau$date,
                                       observed.prop.lossMaliau$time,sep='_'))

observed.prop.lossMaliau$sound.type.for.median <- str_split_fixed (observed.prop.lossMaliau$Sound.type, pattern = '_',n=2)[,2]

# Isolate sound category names
observed.prop.lossMaliau$Sound.category <- 
  str_split_fixed(observed.prop.lossMaliau$Sound.type, pattern = '_',n=3)[,2]


observed.prop.lossMaliau.median.df <- data.frame()

for(a in 1:length(RecorderDateTime.index)){
 temp.observed <-  subset(observed.prop.lossMaliau,RecorderDateTime==RecorderDateTime[a])
 
 sound.type.index <- 
   unique(temp.observed$sound.type.for.median  )
 
 distance.index <- unique(temp.observed$distance  )
 
 for(b in 1:length(sound.type.index)){
   
   temp.subset <- subset(temp.observed,sound.type.for.median==sound.type.index[b])
   
   for(c in 1:length(distance.index)){
     
    temp.subset.distance <-  subset(temp.subset,distance==distance.index[c])
    median.x <- median(temp.subset.distance$magic.x) 
    
    temp.subset.distance <- temp.subset.distance[1,c("distance", "actual.receive.level","Sound.type", "time", "date", "magic.x", 
                           "RecorderDateTime", "sound.type.for.median", "Sound.category")]
   
    temp.subset.distance$median.x <- median.x
    observed.prop.lossMaliau.median.df <- rbind.data.frame(observed.prop.lossMaliau.median.df,temp.subset.distance)
   }
 }
   
}



# Subset so we focus on primates
observed.prop.lossMaliau.subset <-  subset(observed.prop.lossMaliau.median.df,
                                             Sound.category=="Hfunstart" |Sound.category=="Hfuntrill" 
                                             |Sound.category=="Halbpeak"
                                             |Sound.category=="Halbend"
                                             |Sound.category=="Pmor"|Sound.category=="PwurP"|Sound.category=="PwurS") #

# Give more informative names
observed.prop.lossMaliau.subset$Sound.category <- 
  plyr::revalue(observed.prop.lossMaliau.subset$Sound.category,
                c(Halbpeak= "Gibbon C. Kali Peak",Halbend= "Gibbon C. Kali End",
                  Hfunstart = "Gibbon Sabah Intro",
 Hfuntrill = "Gibbon Sabah Trill",Pmor="Orangutan Sabah", 
                  PwurP="Orangutan C. Kali Pulse",
                  PwurS="Orangutan C. Kali Sigh"))

# Plot observed change in receive level by distance
MaliauPlot <- ggpubr::ggscatter(data = observed.prop.lossMaliau.subset,
                  x='distance', y='actual.receive.level',
                  color='Sound.category',
                  facet.by = 'Sound.category',xlim=c(0,400), #ylim=c(-70,-20),
                  add = c("loess"),title='')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")+ggtitle('Maliau')

# Plot observed change in receive level by distance for all signals
ggpubr::ggscatter(data = observed.prop.lossMaliau,x='distance', y='actual.receive.level',
                  color='Sound.type',
                  facet.by = 'Sound.type',xlim=c(0,300), #ylim=c(-70,-20),
                  add = c("loess"),title='')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")


# Part 6.  Observed results relative to spherical and cylindrical  --------
hist(observed.prop.lossMaliau.subset$magic.x)

# We can subset by sound category- here it is by "Hfunstart"
playback.line.1 <- median(na.omit(subset(observed.prop.lossMaliau,
                                     Sound.category=="Pwur")$magic.x))

# Or we can combine all of our data
#playback.line.1 <- median(na.omit(observed.prop.lossMaliau$magic.x))
  
# Set the equations for observed, spherical and cylindrical spreading
eq1 <- function(x){ playback.line.1*log10(x)}
eq2 <- function(x){ -20*log10(x)}
eq3 <- function(x){ -10*log10(x)}

# Create a series of points based on the above equations
Estimated1 <- cbind.data.frame(seq(1:500),eq1(1:500),rep('Estimated',500))
colnames(Estimated1) <- c("X","Value","Label")
Spherical <- cbind.data.frame(seq(1:500),eq2(1:500),rep('Spherical',500))
colnames(Spherical) <- c("X","Value","Label")
Cylindrical <-  cbind.data.frame(seq(1:500),eq3(1:500),rep('Cylindrical',500))
colnames(Cylindrical) <- c("X","Value","Label")


# Combine all three into a single dataframe
attenuation.df <- rbind.data.frame(Estimated1,Spherical,Cylindrical)

# Plot the results
ggplot(data = attenuation.df,aes(x=X, y=Value,group=Label, colour=Label,linetype=Label))+
  geom_line() +theme_bw() + scale_color_manual(values = c("black","red","darkgray"))+
  theme(legend.title = element_blank())+ 
  scale_linetype_manual(values=c( "solid","twodash", "dotted"))+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"))+
  xlab("Distance from source (m)") + ylab("Amplitude (dB)")+
  ylim(-70,0)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Plot magic x by distance
observed.prop.lossMaliau.x.dist <- subset(observed.prop.lossMaliau,
                                    Sound.category=="Halbstart")

plot(observed.prop.lossMaliau.x.dist$magic.x ~ observed.prop.lossMaliau.x.dist$distance,
     xlab='Distance (m)', ylab='Magic x')

plot(observed.prop.lossMaliau.x.dist$noise.level ~ observed.prop.lossMaliau.x.dist$distance,
     xlab='Distance (m)', ylab='Noise')

ggpubr::ggscatter(data = observed.prop.lossMaliau.x.dist,x='distance', y='actual.receive.level',
                  color='Sound.type',
                  #xlim=c(0,300), #ylim=c(-70,-20),
                  title='')+
  xlab('Distance (m)')+ ylab('Receive Level (dB)')+ theme(legend.position = "none")


