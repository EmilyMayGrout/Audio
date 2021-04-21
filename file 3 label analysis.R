rm(list=ls())

setwd("F:/PhD/Desktop/Edic Mini")

library(seewave)
library(fftw)
library(tuneR)
library(rgl)
library(rpanel)
library(ggplot2)
library(viridis)
library(grid)
library(gridExtra)
library(phonTools)#for spectrogram(x) but also doesn't work
library(lubridate)

#trying with one of the files
file3wav <- 'D:/PhD/Desktop/Edic Mini/Eobs edic mini_6887/file_3_6887_detailed.wav'
file3labels <- read.csv("D:/PhD/Desktop/Edic Mini/call labels 6887/detailed/cleaned detailed/6887_3det.csv", sep="\t")

call_labelmetrics <- file3labels

#perhaps the time format for the start and duration column are wrong?

call_labelmetrics$domfrequency <- NA #before the loop, make an empty column to hold metric
call_labelmetrics$absolute_amplitude <- NA
call_labelmetrics$hilbert_amplitude <- NA

i=4

for(i in seq(1, nrow(call_labelmetrics))){
  
  t0 <- call_labelmetrics$Start[i]
  
  if(length(strsplit(t0,":")[[1]])==2){
    t0 <- as.numeric(ms(t0)) #put the start time in seconds
  }else{
    t0 <- as.numeric(hms(t0)) #put the start time in seconds
  }
  dur <- call_labelmetrics$Duration[i]
  dur <- as.numeric(ms(dur)) #put duration in seconds
  
  if(dur>0){
    call <- readWave(file3wav, from = t0, to = (t0 + dur), units = "seconds", header = FALSE, toWaveMC = NULL)
    
    test=meanspec(call, f=22050) #so it doesn't take ages to run, calculate the mean 
    ##the error is in meanspec
    
    domfrequency <- fpeaks(test, nmax = NULL, amp = NULL, freq = NULL, threshold = NULL,
                           mel = FALSE, plot=F) #output of your metric
    dom_res=summary(domfrequency)
    absamplitude <- env(call@left, f=22050, envt = "abs", plot=F)
    hilbertamplitude <- env(call@left, f=22050, envt = "hil",plot=F)
    
    
    call_labelmetrics$domfrequency[i] <- domfrequency[1,1] #store the output in the table   
    call_labelmetrics$absolute_amplitude[i] <- absamplitude[1,1]
    call_labelmetrics$hilbert_amplitude[i] <- hilbertamplitude[1,1]
    
  }
  print(i)
}




