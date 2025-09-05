# This is the R code to estimate propagation loss in Borneo 

# NOTES: In this setup both the playbacks and the playback speakers were moving
# Swift playbacks are indicated by S00974
# There are 8 locations that are part of the permanent grid (Loc_name); in this study 3 habitat types were sampled but all 8 locations
# One series of playbacks with the same Swift; playback speaker moves to 100m, 250m and 500m 

# Version summary ---------------------------------------------------------
# Version 2. Add adaptive noise estimates
# Version 3. Modify noise calculations so divide into ten 1-sec bins
# Version 4. Remove adaptive noise estimates
# Version 5. July 12- Run for two sites; had to remove '-' from text file names so that the .wav files would match
# Version 6. playbacks in code is analogous to playback #; Loc_Name is a permanent location
# Version 7. Add noise analysis in 1/3 octave bands
# Version 8. Shift C. Kalimantan orang pulse frequency down; omit non primate annotations
# Version 9. Add adaptive noise back in
# Version 10. Add adaptive noise back in; take .25 quantile noise
# Version 11. Clean up for publication
# Version 12. Modify so code returns comparable values to PAMGuide
# Version 13. Remove 110_S01143_20190812_130005-1.txt; need to account 

# Part 1. Load necessary packages -------------------------------------------------------------
library(seewave)
library(tuneR)
library(stringr)
library(plyr)
library(ggpubr)
library(geosphere)
#library(plotKML)
library(dplyr)
library(googledrive)
#googledrive::drive_auth() # NEED TO UNCOMMENT TO ACCESS GOOGLE DRIVE

# Part 2. Set up data -------------------------------------------------------------

# Playback template table
SelectionIDsRungan <- 
  read.delim("data/SelectionLabels_S00974_20190811_101922_updated_april2024.txt")

# isolate sound type without replicate
TempSoundType <- 
  str_split_fixed(SelectionIDsRungan$Sound.Type, pattern = '_',n=3)[,2]

TempSoundType <- substr(TempSoundType,start = 1,stop=2)

# Remove pulses
# DENA NOTE: Why did we remove some pulses from rungan?
PulsesToRemove <- which(TempSoundType!="Hf" & TempSoundType!="Ha"
                        & TempSoundType!="Pm" & TempSoundType!="Pw" )

PlaybackSeq <- seq(1,nrow(SelectionIDsRungan),1)

PlaybackSeqUpdated <- PlaybackSeq[-PulsesToRemove]
SelectionIDsRungan <- SelectionIDsRungan[-PulsesToRemove,]

# Selection table location
input.dir <- 'data/RunganAll'

# Read in file that has Google Drive sound file locations
Rungan.playbacks.wav.files <- read.csv('data/Rungan.playbacks.wav.files.csv')
  
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

# Set distance of first playbacks to playback speaker (in meters)
dist.to.playback <- 10

# Read in selection tables with full file path names
selection.tables <- 
  list.files(input.dir,full.names = T)

# Read in selection tables with short file path names for adding name column
selection.tables.short <-
  list.files(input.dir,full.names = F)

# Create an index for playbacks (Wendy changed last line from 1 to 2)
# list.of.playbacks <- str_split_fixed(selection.tables.short, pattern = '_',3)[,2]
list.of.playbacks <- str_split_fixed(selection.tables.short, pattern = '_',3)[,1]

# This ran over two recordings: "data/RunganAll/36_S00974_20190806_090003_100004.txt"
# Dec 4 2024: Ignore #110 and #36 for now

selection.tables <- selection.tables[-c(9,30)]
list.of.playbacks <- list.of.playbacks[-c(9,30)]

# Read in selection tables and combine with file name
combined.template.table.test <- data.frame()

for(a in 1:length(selection.tables)){
  
  template.table.temp <- read.delim(selection.tables[a],fill = T,header=T,stringsAsFactors = F)
  
  playbacks <- list.of.playbacks[a]
  file.name <- selection.tables.short[a]
  file.name <- str_split_fixed(file.name,pattern = '.txt',n=2)[,1]
  
  
  template.table.temp <- cbind.data.frame(template.table.temp,playbacks,file.name)
  
  template.table.temp <-template.table.temp[,c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", 
                          "Low.Freq..Hz.", "High.Freq..Hz.",
                          "SNR.NIST.Quick..dB.FS.", "Sound.Type", "playbacks", "file.name"
  )]
  
  print(template.table.temp$playbacks)
  combined.template.table.test <- rbind.data.frame(combined.template.table.test,template.table.temp)
  #combined.template.table.test <- bind_rows(combined.template.table.test,template.table.temp)
}

# Check output to make sure it looks right
tail(combined.template.table.test)
length(unique(combined.template.table.test$file.name))

# Part 3. Calculate distance for each ---------------------------------------------------

# Read in data file
# Each row corresponds to a playback
rungan_data <- read.csv("data/PropLoss_test.csv")

# Loop to match files
# Identifying each unique playback number
pb.index <- unique(combined.template.table.test$playbacks)

combined.template.table.test.add.dist <- data.frame()
for(b in 1:length(pb.index)){
  #from pb.rungan, find distance 
  single.pb.subset <- subset(rungan_data,PB_No==pb.index[b])
  subset.add.dist <- subset(combined.template.table.test,playbacks==pb.index[b])
  subset.add.dist$distance.from.source <- rep(single.pb.subset$Dist, nrow(subset.add.dist))
  subset.add.dist$Loc_Name <- rep(single.pb.subset$Loc_Name, nrow(subset.add.dist))
  combined.template.table.test.add.dist <- rbind.data.frame(combined.template.table.test.add.dist,subset.add.dist)
}

# Add a distance column
# combined.template.table.test$distance.from.source <- str_split_fixed(combined.template.table.test$file.name,pattern = '_',n=4)[,4]

# Add a date column
combined.template.table.test.add.dist$date <- str_split_fixed(combined.template.table.test.add.dist$file.name,pattern = '_',n=4)[,3]

# Add a time column
time.temp <- str_split_fixed(combined.template.table.test.add.dist$file.name,pattern = '_',n=4)[,4]
#combined.template.table.test.add.dist$time <- str_split_fixed(time.temp,pattern = '.txt',n=2)[,1]
combined.template.table.test.add.dist$time <- substr(time.temp,start=1,stop=4)

# Check structure of resulting data frame
head(combined.template.table.test.add.dist)
table(combined.template.table.test.add.dist$distance.from.source)

# Part 4. Calculate absolute receive levels for each selection --------------------

combined.template.table.test.add.dist$Loc_Name <- 
  as.factor(combined.template.table.test.add.dist$Loc_Name)

# Create an index for each file
file.name.index <- unique(combined.template.table.test.add.dist$Loc_Name)

# Create an empty dataframe to add to iteratively in the loop
BackgroundNoiseRemovedDFRungan <- data.frame()

# The loop to calculate inband power (after subtracting the noise) for each selection from the wave file
for(b in 1:length(file.name.index)){tryCatch({
  
  # Subset by playbacks and date index
  combined.playbacks <- subset(combined.template.table.test.add.dist,Loc_Name==file.name.index[b])

  unique.playbacks <- unique(combined.playbacks$playbacks)
  
  for(j in 1:length(unique.playbacks)){
  print(paste('processing', j, 'out of',length(unique.playbacks), 'for location', file.name.index[b]))
  singleplayback.df <- subset(combined.playbacks,playbacks==unique.playbacks[j])
  
  # Remove rows to focus only on apes
   singleplayback.df <- 
    singleplayback.df[-PulsesToRemove,]
  
  if( nrow(singleplayback.df)==26  ){
  singleplayback.df$Call.type <- SelectionIDsRungan$Sound.Type[1:nrow(singleplayback.df)]
  } else{
    singleplayback.df$Call.type <- SelectionIDsRungan$Sound.Type
    
  }
   
  # Create sound file path
  soundfile <- str_split_fixed(singleplayback.df$file.name[1],pattern = '_',n=2)[,2]
  soundfile <- str_split_fixed(soundfile,pattern = '-',n=2)[,1]
  soundfile.path <- paste(soundfile,'.wav',sep='')
  wav.file.index <- which(Rungan.playbacks.wav.files$name %in% soundfile.path)
  
  if(file.exists(soundfile.path)==FALSE){
  drive_download(as_id(Rungan.playbacks.wav.files$id[wav.file.index]))
  }
  
  # Read in the long .wav file
  wavfile.temp <- tuneR::readWave(Rungan.playbacks.wav.files$name[wav.file.index])
  
  # Normalize around mean to remove offset
  wavfile.temp@left <- wavfile.temp@left- mean(wavfile.temp@left)
  
  # Check to make sure number of selections matches the template
  #if(nrow(singleplayback.df)==nrow(SelectionIDs)){ 
    
    # Use the Raven selection table to cut each selection into an individual .wav file
    ListofWavs <- 
      lapply(1:nrow(singleplayback.df), function(x) cutw(wavfile.temp, from= (singleplayback.df$Begin.Time..s.[x]- signal.time.buffer),
                                                         to= (singleplayback.df$End.Time..s.[x]+signal.time.buffer), output='Wave'))
    
    # Matches each selection with the corresponding noise and selection .wav file and calculate absolute receive level
    for(d in 1:nrow(singleplayback.df)){
      
      print(d)
      
      # Subset the correspond row from the selection table
      Selectiontemp <- singleplayback.df[d,]
      
      OrangYN <- SelectionIDsRungan[d,]$Class
      
      # Shift C. orangutans down
      if(OrangYN=="Orang"){
        Selectiontemp$Low.Freq..Hz. <- SelectionIDsRungan[d,]$Low.Freq..Hz.
        Selectiontemp$High.Freq..Hz.<- SelectionIDsRungan[d,]$High.Freq..Hz.
      }
      
      
      # Use the Raven selection table to extract a longer duration .wav file for noise
      NoiseWav1 <- cutw(wavfile.temp, from= Selectiontemp$Begin.Time..s. - timesecs,
                        to=Selectiontemp$Begin.Time..s., output='Wave')
      
      NoiseWav2 <- cutw(wavfile.temp, from= Selectiontemp$End.Time..s.,
                        to= Selectiontemp$End.Time..s. + timesecs, output='Wave')
      
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
                                    n=3,output = "Wave")
        
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
          data <- (temp.wave@left) / (2 ^ 16/2)
          
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
      
      
      noise.value <- min(unlist(noise.value.list))
      
      
      noise.index <- which.min(unlist(noise.value.list))
      wavdur <-  Selectiontemp$End.Time..s.- Selectiontemp$Begin.Time..s.
     
      # Isolate the corresponding .wav file for the playback selection
      SignalWavtemp <-  ListofWavs[[d]]
      
      # Filter to the frequency range of the selection
      w.dn.filt <- bwfilter(SignalWavtemp, 
                            from=Selectiontemp$Low.Freq..Hz., 
                            to=Selectiontemp$High.Freq..Hz.,n=3,output = "Wave")
      
      
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
      
      # Match the signal with the sound type
      Selectiontemp$Sound.Type <-  SelectionIDsRungan[d,]$Sound.Type
      
      # Print the output
      print(Selectiontemp)
      
      # Combine into a dataframe
      BackgroundNoiseRemovedDFRungan <- rbind.data.frame(BackgroundNoiseRemovedDFRungan,Selectiontemp)
      write.csv(BackgroundNoiseRemovedDFRungan,'data/RunganPropLossSept2025test.csv',row.names = F)
    }

    
  
  # Remove file from memory
  unlink(Rungan.playbacks.wav.files$name[wav.file.index])
  rm(wavfile.temp)
  }
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }


# Reference only has one set of playbacks so want to append   
BackgroundNoiseRemovedDFRungan_char1 <-subset(BackgroundNoiseRemovedDFRungan, Loc_Name == 'char1')
BackgroundNoiseRemovedDFRungan_char1 <- rbind.data.frame(BackgroundNoiseRemovedDFRungan_char1,BackgroundNoiseRemovedDFRungan_char1 ,BackgroundNoiseRemovedDFRungan_char1)
BackgroundNoiseRemovedDFRungan_char1$Selection <- SelectionIDsRungan$Selection
BackgroundNoiseRemovedDFRungan_char1$Sound.Type <- SelectionIDsRungan$Sound.Type

BackgroundNoiseRemovedDFRungan_char2 <-subset(BackgroundNoiseRemovedDFRungan, Loc_Name == 'char2')
BackgroundNoiseRemovedDFRungan_char2 <- rbind.data.frame(BackgroundNoiseRemovedDFRungan_char2,BackgroundNoiseRemovedDFRungan_char2 ,BackgroundNoiseRemovedDFRungan_char2)
BackgroundNoiseRemovedDFRungan_char2$Selection <- SelectionIDsRungan$Selection
BackgroundNoiseRemovedDFRungan_char2$Sound.Type <- SelectionIDsRungan$Sound.Type

BackgroundNoiseRemovedDFRungan_char3 <-subset(BackgroundNoiseRemovedDFRungan, Loc_Name == 'char3')
BackgroundNoiseRemovedDFRungan_char3 <- rbind.data.frame(BackgroundNoiseRemovedDFRungan_char3,BackgroundNoiseRemovedDFRungan_char3 ,BackgroundNoiseRemovedDFRungan_char3)
BackgroundNoiseRemovedDFRungan_char3$Selection <- SelectionIDsRungan$Selection
BackgroundNoiseRemovedDFRungan_char3$Sound.Type <- SelectionIDsRungan$Sound.Type

#BackgroundNoiseRemovedDFRungan <- subset(BackgroundNoiseRemovedDFRungan,Loc_Name != 'char3')
CombinedDF <-rbind.data.frame(BackgroundNoiseRemovedDFRungan,BackgroundNoiseRemovedDFRungan_char1,BackgroundNoiseRemovedDFRungan_char2,BackgroundNoiseRemovedDFRungan_char3)

write.csv(CombinedDF,'data/RunganPropLossSept2025_addcharacterized.csv',row.names = F)

