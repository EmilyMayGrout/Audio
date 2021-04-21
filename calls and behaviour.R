#this script is looking at calls emitted during different behavioural states

#just looking at 6887 for now
#setwd("E:/PhD/Desktop/Edic Mini/call labels 6887/detailed/cleaned detailed/")
setwd("E:/PhD/Desktop/Edic Mini/call labels 6887/detailed/cleaned detailed/")

det_6887 <- list.files(pattern = ".csv")

all_6887 <- NA

for(file in det_6887){
  
  day <- read.delim(file = file, h=F)
  day$day <- file
  day <- day[-1,]
  all_6887 = rbind(all_6887, day, make.row.names = F) 
}

unique(all_6887$V1)
#remove NA row
all_6887 <- all_6887[-1,]
unique(is.na(all_6887$V1))
#change column names
colnames(all_6887) <- c("Name", "Start", "Duration", "Time.Format", "Type", "Description", "File")


#converts text format "00:00:00" into a number of seconds
string2sec <- function(string){
  
  splt <- strsplit(as.character(string),split=":")[[1]]
  
  if(length(splt) == 2){
    sec <- as.numeric(splt[2])
    min <- as.numeric(splt[1])
    hour <- 0
  }else if(length(splt) == 3){
    sec <- as.numeric(splt[3])
    min <- as.numeric(splt[2])
    hour <- as.numeric(splt[1])
  }else{
    return(NA)
  }
  return(3600*hour + 60*min + sec)
}

  
all_6887$starts <- as.numeric(sapply(as.character(all_6887$Start),string2sec))
all_6887$ends <- all_6887$starts + as.numeric(sapply(as.character(all_6887$Duration),string2sec))
all_6887$durations <- all_6887$ends - all_6887$starts 
#names <- as.character(all_6887[,1])
#newLabels <- cbind(starts,ends,names)

#make column to describe if call is coati or behaviour or other (e.g. bird)
unique(all_6887$Name)

#to simplify the chirpgr so can compare general features
all_6887[all_6887 == "chirpgr n/1"] <- "chirpgr"
all_6887[all_6887 == "chirpgr n/2"] <- "chirpgr"
all_6887[all_6887 == "chirpgr n/3"] <- "chirpgr"
all_6887[all_6887 == "chirpgr n/4"] <- "chirpgr"
all_6887[all_6887 == "chirpgr n/5"] <- "chirpgr"
all_6887[all_6887 == "chirpgr y/1"] <- "chirpgr"
all_6887[all_6887 == "chirpgr y/2"] <- "chirpgr"
all_6887[all_6887 == "chirpgr y/3"] <- "chirpgr"
all_6887[all_6887 == "chirpgr y/4"] <- "chirpgr"
all_6887[all_6887 == "chirpgr y/5"] <- "chirpgr"
all_6887[all_6887 == "nf chirpgr n/5"] <- "chirpgr"
all_6887[all_6887 == "chirp y/3"] <- "chirp"
all_6887[all_6887 == "chirp y/2"] <- "chirp"
all_6887[all_6887 == "chirp n/1"] <- "chirp"
all_6887[all_6887 == "chirp n/4"] <- "chirp"
all_6887[all_6887 == "chirp n/5"] <- "chirp"
all_6887[all_6887 == "chirp y/5"] <- "chirp"
all_6887[all_6887 == "chirp n/3"] <- "chirp"
all_6887[all_6887 == "chirp y/1"] <- "chirp"
all_6887[all_6887 == "chirp y/4"] <- "chirp"
all_6887[all_6887 == "chirp n/2"] <- "chirp"
all_6887[all_6887 == "nf chirp n/2"] <- "nf chirp"
all_6887[all_6887 == "nf chirpgr n/2"] <- "nf chirp"
all_6887[all_6887 == "grunt n/2"] <- "grunt"
all_6887[all_6887 == "chirpgr - repeat begins"] <- "chirpgr"


unique(all_6887$Name)

calls <- c("cluck", "grunt", "snort", "chirpgr", "barking", "chirp", "chitter", "chittering", "click grunt", "growl", "grunt breath",
           "hum", "purr", "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "bop", "click", "grunting",
           "squeal grunt", "bark", "squeal growl", "squeal grunts")

behaviour <- c("breath", "breathing", "foraging", "panting", "scratch", "sneeze", "drinking", "snore", "wake up", "chewing", "drinking",
               "walking", "head shake", "resting", "running", "swallow")
sort(table(all_6887$Name))

other <- c("bird", "buzz", "howler", "cargo horn", "collar", "dumdum", "unk rhythm", "water", "wood", "engine", "insect", "wind")
nf_calls <- c("nf chirp", "nf chirpgr", "nf click grunt", "nf squeal", "nf barking", "nf grunt", "nf chitter", "nf clickgr")
nf_behaviour <- c("nf drinking")


all_6887$call_type <- ifelse(all_6887$Name %in% calls, "call",
                      ifelse(all_6887$Name %in% behaviour, "behaviour", 
                      ifelse(all_6887$Name %in% nf_behaviour, "nf_behaviour", 
                      ifelse(all_6887$Name %in% nf_calls, "nf_calls", "other"))))

all_6887$middle <- ((all_6887$durations)/2) + all_6887$starts
window <- 30
all_6887$pre_time <- all_6887$middle - window
all_6887$post_time <- all_6887$middle + window

day_1 <- subset(all_6887[  all_6887$File=="6887_1det.csv", ])
day_2 <- subset(all_6887[  all_6887$File=="6887_2det.csv", ])
day_3 <- subset(all_6887[  all_6887$File=="6887_3det.csv", ])
day_4 <- subset(all_6887[  all_6887$File=="6887_4det.csv", ])
day_5 <- subset(all_6887[  all_6887$File=="6887_5det.csv", ])
day_6 <- subset(all_6887[  all_6887$File=="6887_6det.csv", ])



all_6887$chirpgr_freq[row] <- NA
all_6887$chitter_freq[row] <- NA
all_6887$grunt_freq[row] <- NA
all_6887$snort_freq[row] <- NA
all_6887$chirp_freq[row] <- NA
all_6887$chittering_freq[row] <- NA
all_6887$growl_freq[row] <- NA
all_6887$gruntbreath_freq[row] <- NA
all_6887$hum_freq[row] <- NA
all_6887$quack_freq[row] <- NA
all_6887$squeal_freq[row] <- NA
all_6887$squealchitters_freq[row] <- NA
all_6887$squealing_freq[row] <- NA
all_6887$barkgrunt_freq[row] <- NA
all_6887$barkgrunting_freq[row] <- NA
all_6887$squealgrunt_freq[row] <- NA
all_6887$squealgrunts_freq[row] <- NA

#for loop to calculate the frequency of chirpgr and chitters within a particular time window (30s) 
for (row in 1:nrow(all_6887)){

  #get the pre and post  boundaries of the window
  pre <- all_6887$pre_time[row]
  post <- all_6887$post_time[row]
  
  #get the current file
  curr_file <- all_6887$File[row]
  
  #get all calls within the window
  in_window <- (all_6887$middle > pre) & (all_6887$middle < post) & (all_6887$File == curr_file)
  rows_in_window <- which(in_window)
  calls_in_window <- all_6887$Name[rows_in_window]
  
  #count up calls of different types and store in data frame
  all_6887$chirpgr_freq[row] <- sum(calls_in_window == "chirpgr")
  all_6887$chitter_freq[row] <- sum(calls_in_window == "chitter")
  all_6887$grunt_freq[row] <- sum(calls_in_window == "grunt")
  all_6887$snort_freq[row] <- sum(calls_in_window == "snort")
  all_6887$chirp_freq[row] <- sum(calls_in_window == "chirp")
  all_6887$chittering_freq[row] <- sum(calls_in_window == "chittering")
  all_6887$growl_freq[row] <- sum(calls_in_window == "growl")
  all_6887$gruntbreath_freq[row] <- sum(calls_in_window == "grunt breath")
  all_6887$hum_freq[row] <- sum(calls_in_window == "hum")
  all_6887$quack_freq[row] <- sum(calls_in_window == "quack")
  all_6887$squeal_freq[row] <- sum(calls_in_window == "squeal")
  all_6887$squealchitters_freq[row] <- sum(calls_in_window == "squeal chitters")
  all_6887$squealing_freq[row] <- sum(calls_in_window == "squealing")
  all_6887$barkgrunt_freq[row] <- sum(calls_in_window == "bark grunt")
  all_6887$barkgrunting_freq[row] <- sum(calls_in_window == "bark grunting")
  all_6887$squealgrunt_freq[row] <- sum(calls_in_window == "squeal grunt")
  all_6887$squealgrunts_freq[row] <- sum(calls_in_window == "squeal grunts")
  
  
  
  #not including less than 5 calls: "purr", "barking", "click", "click grunt", "grunting", "squeal growl", "bop", "cluck", "bark"
 
}

#now looking at each behaviour separately to sum the number of calls in the 30 second window divided by the number of times that behaviour occurred

chirpgr <- all_6887[all_6887$chirpgr_freq > 0,]
#for all chewing events, how many chirpgr were there? 
chewing_chirpgr <- chirpgr[chirpgr$Name == "chewing",]
chirpgr_prob <- sum(chewing_chirpgr$chirpgr_freq)/count(chewing_chirpgr)

#make dataframe for only behaviours
all_behaviours <- subset(all_6887, all_6887$call_type == "behaviour")
all_behaviours$Name <- as.character(all_behaviours$Name)

#split the dataframe into list of data frames for each behaviour
data_list <- split(all_behaviours, f = all_behaviours$Name)


out <- NULL
for (i in 1:length(data_list)){
  
  behaviour <- data_list[i]
  sum_chirpgr <- sum(behaviour[[1]][[15]])
  sum_chitter <- sum(behaviour[[1]][[16]])
  sum_grunt <- sum(behaviour[[1]][[17]])
  sum_snort <- sum(behaviour[[1]][[18]])
  sum_chirp <- sum(behaviour[[1]][[19]])
  sum_chittering <- sum(behaviour[[1]][[20]])
  sum_growl <- sum(behaviour[[1]][[21]])
  sum_gruntbreath <- sum(behaviour[[1]][[22]])
  sum_hum <- sum(behaviour[[1]][[23]])
  sum_quack <- sum(behaviour[[1]][[24]])
  sum_squeal <- sum(behaviour[[1]][[25]])
  sum_squealchitters <- sum(behaviour[[1]][[26]])
  sum_squealing <- sum(behaviour[[1]][[27]])
  sum_barkgrunt <- sum(behaviour[[1]][[28]])
  sum_barkgrunting <- sum(behaviour[[1]][[29]])
  sum_squealgrunt <- sum(behaviour[[1]][[30]])
  sum_squealgrunts <- sum(behaviour[[1]][[31]])
  
  combined <- data.frame(cbind(names(data_list[i]), sum_chirpgr, sum_chitter, sum_grunt, sum_snort, sum_chirp, sum_chittering, sum_growl, sum_gruntbreath, sum_hum,
                               sum_quack, sum_squeal, sum_squealchitters, sum_squealing, sum_barkgrunt, sum_barkgrunting, sum_squealgrunt, sum_squealgrunts, nrow(data_list[[i]])))
  
  out <- data.frame(rbind(out, combined))
  
}

colnames(out)[colnames(out) == "V1"] <- "behaviour_type"
colnames(out)[colnames(out) == "V19"] <- "behaviour_freq"

#convert all columns to numeric
out$behaviour_type <- as.character(out$behaviour_type)
out$sum_chirpgr <- as.numeric(as.character(out$sum_chirpgr))
out$sum_chitter <- as.numeric(as.character(out$sum_chitter))
out$sum_grunt <- as.numeric(as.character(out$sum_grunt))
out$sum_snort <- as.numeric(as.character(out$sum_snort))
out$sum_chirp <- as.numeric(as.character(out$sum_chirp))
out$sum_chittering <- as.numeric(as.character(out$sum_chittering))
out$sum_growl <- as.numeric(as.character(out$sum_growl))
out$sum_gruntbreath <- as.numeric(as.character(out$sum_gruntbreath))
out$sum_hum <- as.numeric(as.character(out$sum_hum))
out$sum_quack <- as.numeric(as.character(out$sum_quack))
out$sum_squeal <- as.numeric(as.character(out$sum_squeal))
out$sum_squealchitters <- as.numeric(as.character(out$sum_squealchitters))
out$sum_squealing <- as.numeric(as.character(out$sum_squealing))
out$sum_barkgrunt <- as.numeric(as.character(out$sum_barkgrunt))
out$sum_barkgrunting <- as.numeric(as.character(out$sum_barkgrunting))
out$sum_squealgrunt <- as.numeric(as.character(out$sum_squealgrunt))
out$sum_squealgrunts <- as.numeric(as.character(out$sum_squealgrunts))
out$behaviour_freq <- as.numeric(as.character(out$behaviour_freq))


i=1
for(i in 1:nrow(out)){
  
 out$chirpgr_prob[i] <- (out$sum_chirpgr[i]/out$behaviour_freq[i])
 out$chitter_prob[i] <- (out$sum_chitter[i]/out$behaviour_freq[i])
 out$grunt_prob[i] <- (out$sum_grunt[i]/out$behaviour_freq[i])
 out$snort_prob[i] <- (out$sum_snort[i]/out$behaviour_freq[i])
 out$chirp_prob[i] <- (out$sum_chirp[i]/out$behaviour_freq[i])
 out$chittering_prob[i] <- (out$sum_chittering[i]/out$behaviour_freq[i])
 out$growl_prob[i] <- (out$sum_growl[i]/out$behaviour_freq[i])
 out$gruntbreath_prob[i] <- (out$sum_gruntbreath[i]/out$behaviour_freq[i])
 out$hum_prob[i] <- (out$sum_hum[i]/out$behaviour_freq[i])
 out$quack_prob[i] <- (out$sum_quack[i]/out$behaviour_freq[i])
 out$squeal_prob[i] <- (out$sum_squeal[i]/out$behaviour_freq[i])
 out$squealchitters_prob[i] <- (out$sum_squealchitters[i]/out$behaviour_freq[i])
 out$squealing_prob[i] <- (out$sum_squealing[i]/out$behaviour_freq[i])
 out$barkgrunt_prob[i] <- (out$sum_barkgrunt[i]/out$behaviour_freq[i])
 out$barkgrunting_prob[i] <- (out$sum_barkgrunting[i]/out$behaviour_freq[i])
 out$squealgrunt_prob[i] <- (out$sum_squealgrunt[i]/out$behaviour_freq[i])
 out$squealgrunts_prob[i] <- (out$sum_squealgrunts[i]/out$behaviour_freq[i])
 
}
#filter data frame
summary_stats <- out[, c(1, 20:36)]

par(mfrow=c(2,2), mar=c(8,4,4,2)) 
barplot(summary_stats$chirpgr_prob, names.arg = summary_stats$behaviour_type, ylim = c(0, 4.5), srt = 45, adj = 1, las=2)
barplot(summary_stats$chitter_prob, names.arg = summary_stats$behaviour_type, ylim = c(0, 4.5))
barplot(summary_stats$grunt_prob, names.arg = summary_stats$behaviour_type, ylim = c(0, 4.5))
barplot(summary_stats$squeal_prob, names.arg = summary_stats$behaviour_type, ylim = c(0, 4.5))


#transposing data frame for plotting
transposed_summary <- as.data.frame(t(summary_stats), header= F)
transposed_summary <- transposed_summary[-1, ]
colnames(transposed_summary) <- c("breath", "breathing", "chewing", "drinking", "foraging", "headshake", "panting",
                                "resting", "running", "scratch", "sneeze", "snore", "swallow", "wakeup", "walking")

transposed_summary$breath <- as.numeric(as.character(transposed_summary$breath))
transposed_summary$breathing <- as.numeric(as.character(transposed_summary$breathing))
transposed_summary$chewing <- as.numeric(as.character(transposed_summary$chewing))
transposed_summary$drinking <- as.numeric(as.character(transposed_summary$drinking))
transposed_summary$foraging <- as.numeric(as.character(transposed_summary$foraging))
transposed_summary$headshake <- as.numeric(as.character(transposed_summary$headshake))
transposed_summary$panting <- as.numeric(as.character(transposed_summary$panting))
transposed_summary$resting <- as.numeric(as.character(transposed_summary$resting))
transposed_summary$running <- as.numeric(as.character(transposed_summary$running))
transposed_summary$scratch <- as.numeric(as.character(transposed_summary$scratch))
transposed_summary$sneeze <- as.numeric(as.character(transposed_summary$sneeze))
transposed_summary$snore <- as.numeric(as.character(transposed_summary$snore))
transposed_summary$swallow <- as.numeric(as.character(transposed_summary$swallow))
transposed_summary$wakeup <- as.numeric(as.character(transposed_summary$wakeup))
transposed_summary$walking <- as.numeric(as.character(transposed_summary$walking))

#remove the rarer behaviours for the  - breathing, resting, snore, swallow, wakeup and the gruntbreath and barkgrunting call
subset_transposed_summary <- transposed_summary[-c(8, 15, 17), -c(2, 8, 12:14)]
#plotting the probability of calling for each behaviour
par(mfrow=c(1,1), mar=c(6,8,3,8), xpd = T ) 

barplot(as.matrix(subset_transposed_summary), las=2, beside = T, col = topo.colors(14), ylab = "Average call frequency", ylim = c(0, 5.5))
legend("topright", inset = c(- 0.1, 0), legend = c("chirpgrunt", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "hum", "quack", "squeal",
                                 "squealchitters", "squealing", "barkgrunt", "squealgrunt"), lty = c(1,1), col = topo.colors(17), bty = "n")

sort(table(all_6887$Name))

#total number of each call type per row

all_6887$sum_calls <- NA
for(i in 1:nrow(all_6887)){

all_6887$sum_calls[i] <- all_6887$chirpgr_freq[i]+all_6887$chitter_freq[i]+all_6887$grunt_freq[i]+all_6887$snort_freq[i]+
                         all_6887$chirp_freq[i]+all_6887$chittering_freq[i]+all_6887$growl_freq[i]+all_6887$gruntbreath_freq[i]+
                         all_6887$hum_freq[i]+all_6887$quack_freq[i]+all_6887$squeal_freq[i]+all_6887$squealchitters_freq[i]+
                         all_6887$squealing_freq[i]+all_6887$barkgrunt_freq[i]+all_6887$barkgrunting_freq[i]+all_6887$squealgrunt_freq[i]+
                         all_6887$squealgrunts_freq[i]

}
#ordering the dataframe
day_1 <- day_1[order(day_1$starts),]

#looking at calls emitted for each behaviour
par(mfrow=c(3,2), mar=c(8,4,4,2)) 

#chewing
chewing <- all_6887[all_6887$Name=="chewing",]
boxplot(chewing$chirpgr_freq, chewing$chitter_freq, chewing$grunt_freq, chewing$snort_freq, chewing$chirp_freq, 
        chewing$chittering_freq, chewing$growl_freq, chewing$gruntbreath_freq, chewing$hum_freq, chewing$quack_freq,
        chewing$squeal_freq, chewing$squealchitters_freq, chewing$squealing_freq, chewing$barkgrunt_freq, 
        chewing$barkgrunting_freq, chewing$squealgrunt_freq, chewing$squealgrunts_freq, 
        names = c("chirpgr", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "grunt breath", "hum",
                   "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "squeal grunt", "squeal grunts"),
                  srt = 45, adj = 1, las=2, horizontal = F, ylab = "Chewing", ylim = c(0,15))

#sniffing
sniff <- all_6887[all_6887$Name=="sniff",]
boxplot(sniff$chirpgr_freq, sniff$chitter_freq, sniff$grunt_freq, sniff$snort_freq, sniff$chirp_freq, 
        sniff$chittering_freq, sniff$growl_freq, sniff$gruntbreath_freq, sniff$hum_freq, sniff$quack_freq,
        sniff$squeal_freq, sniff$squealchitters_freq, sniff$squealing_freq, sniff$barkgrunt_freq, 
        sniff$barkgrunting_freq, sniff$squealgrunt_freq, sniff$squealgrunts_freq, 
        names = c("chirpgr", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "grunt breath", "hum",
                  "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "squeal grunt", "squeal grunts"),
        srt = 45, adj = 1, las=2, horizontal = F, ylab = "Sniffing", ylim = c(0,15))

#drinking
drinking <- all_6887[all_6887$Name=="drinking",]
boxplot(drinking$chirpgr_freq, drinking$chitter_freq, drinking$grunt_freq, drinking$snort_freq, drinking$chirp_freq, 
        drinking$chittering_freq, drinking$growl_freq, drinking$gruntbreath_freq, drinking$hum_freq, drinking$quack_freq,
        drinking$squeal_freq, drinking$squealchitters_freq, drinking$squealing_freq, drinking$barkgrunt_freq, 
        drinking$barkgrunting_freq, drinking$squealgrunt_freq, drinking$squealgrunts_freq, 
        names = c("chirpgr", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "grunt breath", "hum",
                  "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "squeal grunt", "squeal grunts"),
        srt = 45, adj = 1, las=2, horizontal = F, ylab = "Drinking", ylim = c(0,15))#scratch

#scratching
scratch <- all_6887[all_6887$Name=="scratch",]
boxplot(scratch$chirpgr_freq, scratch$chitter_freq, scratch$grunt_freq, scratch$snort_freq, scratch$chirp_freq, 
        scratch$chittering_freq, scratch$growl_freq, scratch$gruntbreath_freq, scratch$hum_freq, scratch$quack_freq,
        scratch$squeal_freq, scratch$squealchitters_freq, scratch$squealing_freq, scratch$barkgrunt_freq, 
        scratch$barkgrunting_freq, scratch$squealgrunt_freq, scratch$squealgrunts_freq, 
        names = c("chirpgr", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "grunt breath", "hum",
                  "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "squeal grunt", "squeal grunts"),
        srt = 45, adj = 1, las=2, horizontal = F, ylab = "Scratching", ylim = c(0,15))

#walking
walking <- all_6887[all_6887$Name=="walking",]
boxplot(walking$chirpgr_freq, walking$chitter_freq, walking$grunt_freq, walking$snort_freq, walking$chirp_freq, 
        walking$chittering_freq, walking$growl_freq, walking$gruntbreath_freq, walking$hum_freq, walking$quack_freq,
        walking$squeal_freq, walking$squealchitters_freq, walking$squealing_freq, walking$barkgrunt_freq, 
        walking$barkgrunting_freq, walking$squealgrunt_freq, walking$squealgrunts_freq, 
        names = c("chirpgr", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "grunt breath", "hum",
                  "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "squeal grunt", "squeal grunts"),
        srt = 45, adj = 1, las=2, horizontal = F, ylab = "Walking", ylim = c(0,15))

#panting
panting <- all_6887[all_6887$Name=="panting",]
boxplot(panting$chirpgr_freq, panting$chitter_freq, panting$grunt_freq, panting$snort_freq, panting$chirp_freq, 
        panting$chittering_freq, panting$growl_freq, panting$gruntbreath_freq, panting$hum_freq, panting$quack_freq,
        panting$squeal_freq, panting$squealchitters_freq, panting$squealing_freq, panting$barkgrunt_freq, 
        panting$barkgrunting_freq, panting$squealgrunt_freq, panting$squealgrunts_freq, 
        names = c("chirpgr", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "grunt breath", "hum",
                  "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "squeal grunt", "squeal grunts"),
        srt = 45, adj = 1, las=2, horizontal = F, ylab = "Panting", ylim = c(0,15))


#look at ratio of each call type for each behaviour


#plot ratio of that call type compared to other call types for each row
par(mfrow=c(3,2), mar=c(8,4,4,2))
chewing <- all_6887[all_6887$Name=="chewing",]
boxplot((chewing$chirpgr_freq/chewing$sum_calls), (chewing$chitter_freq/chewing$sum_calls), (chewing$grunt_freq/chewing$sum_calls), (chewing$snort_freq/chewing$sum_calls), (chewing$chirp_freq/chewing$sum_calls), 
        (chewing$chittering_freq/chewing$sum_calls), (chewing$growl_freq/chewing$sum_calls), (chewing$gruntbreath_freq/chewing$sum_calls), (chewing$hum_freq/chewing$sum_calls), (chewing$quack_freq/chewing$sum_calls),
        (chewing$squeal_freq/chewing$sum_calls), (chewing$squealchitters_freq/chewing$sum_calls), (chewing$squealing_freq/chewing$sum_calls), (chewing$barkgrunt_freq/chewing$sum_calls), 
        (chewing$barkgrunting_freq/chewing$sum_calls), (chewing$squealgrunt_freq/chewing$sum_calls), (chewing$squealgrunts_freq/chewing$sum_calls), 
        names = c("chirpgr", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "grunt breath", "hum",
                  "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "squeal grunt", "squeal grunts"),
        srt = 45, adj = 1, las=2, horizontal = F, ylab = "Chewing", ylim = c(0,1.1))

#sniffing
sniff <- all_6887[all_6887$Name=="sniff",]
boxplot((sniff$chirpgr_freq/sniff$sum_calls), (sniff$chitter_freq/sniff$sum_calls), (sniff$grunt_freq/sniff$sum_calls), (sniff$snort_freq/sniff$sum_calls), (sniff$chirp_freq/sniff$sum_calls), 
        (sniff$chittering_freq/sniff$sum_calls), (sniff$growl_freq/sniff$sum_calls), (sniff$gruntbreath_freq/sniff$sum_calls), (sniff$hum_freq/sniff$sum_calls), (sniff$quack_freq/sniff$sum_calls),
        (sniff$squeal_freq/sniff$sum_calls), (sniff$squealchitters_freq/sniff$sum_calls), (sniff$squealing_freq/sniff$sum_calls), (sniff$barkgrunt_freq/sniff$sum_calls), 
        (sniff$barkgrunting_freq/sniff$sum_calls), (sniff$squealgrunt_freq/sniff$sum_calls), (sniff$squealgrunts_freq/sniff$sum_calls), 
        names = c("chirpgr", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "grunt breath", "hum",
                  "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "squeal grunt", "squeal grunts"),
        srt = 45, adj = 1, las=2, horizontal = F, ylab = "Sniffing", ylim = c(0,1.1))

drinking <- all_6887[all_6887$Name=="drinking",]
boxplot((drinking$chirpgr_freq/drinking$sum_calls), (drinking$chitter_freq/drinking$sum_calls), (drinking$grunt_freq/drinking$sum_calls), (drinking$snort_freq/drinking$sum_calls), (drinking$chirp_freq/drinking$sum_calls), 
        (drinking$chittering_freq/drinking$sum_calls), (drinking$growl_freq/drinking$sum_calls), (drinking$gruntbreath_freq/drinking$sum_calls), (drinking$hum_freq/drinking$sum_calls), (drinking$quack_freq/drinking$sum_calls),
        (drinking$squeal_freq/drinking$sum_calls), (drinking$squealchitters_freq/drinking$sum_calls), (drinking$squealing_freq/drinking$sum_calls), (drinking$barkgrunt_freq/drinking$sum_calls), 
        (drinking$barkgrunting_freq/drinking$sum_calls), (drinking$squealgrunt_freq/drinking$sum_calls), (drinking$squealgrunts_freq/drinking$sum_calls), 
        names = c("chirpgr", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "grunt breath", "hum",
                  "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "squeal grunt", "squeal grunts"),
        srt = 45, adj = 1, las=2, horizontal = F, ylab = "Drinking", ylim = c(0,1.1))

scratch <- all_6887[all_6887$Name=="scratch",]
boxplot((scratch$chirpgr_freq/scratch$sum_calls), (scratch$chitter_freq/scratch$sum_calls), (scratch$grunt_freq/scratch$sum_calls), (scratch$snort_freq/scratch$sum_calls), (scratch$chirp_freq/scratch$sum_calls), 
        (scratch$chittering_freq/scratch$sum_calls), (scratch$growl_freq/scratch$sum_calls), (scratch$gruntbreath_freq/scratch$sum_calls), (scratch$hum_freq/scratch$sum_calls), (scratch$quack_freq/scratch$sum_calls),
        (scratch$squeal_freq/scratch$sum_calls), (scratch$squealchitters_freq/scratch$sum_calls), (scratch$squealing_freq/scratch$sum_calls), (scratch$barkgrunt_freq/scratch$sum_calls), 
        (scratch$barkgrunting_freq/scratch$sum_calls), (scratch$squealgrunt_freq/scratch$sum_calls), (scratch$squealgrunts_freq/scratch$sum_calls), 
        names = c("chirpgr", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "grunt breath", "hum",
                  "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "squeal grunt", "squeal grunts"),
        srt = 45, adj = 1, las=2, horizontal = F, ylab = "Scratching", ylim = c(0,1.1))

walking <- all_6887[all_6887$Name=="walking",]
boxplot((walking$chirpgr_freq/walking$sum_calls), (walking$chitter_freq/walking$sum_calls), (walking$grunt_freq/walking$sum_calls), (walking$snort_freq/walking$sum_calls), (walking$chirp_freq/walking$sum_calls), 
        (walking$chittering_freq/walking$sum_calls), (walking$growl_freq/walking$sum_calls), (walking$gruntbreath_freq/walking$sum_calls), (walking$hum_freq/walking$sum_calls), (walking$quack_freq/walking$sum_calls),
        (walking$squeal_freq/walking$sum_calls), (walking$squealchitters_freq/walking$sum_calls), (walking$squealing_freq/walking$sum_calls), (walking$barkgrunt_freq/walking$sum_calls), 
        (walking$barkgrunting_freq/walking$sum_calls), (walking$squealgrunt_freq/walking$sum_calls), (walking$squealgrunts_freq/walking$sum_calls), 
        names = c("chirpgr", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "grunt breath", "hum",
                  "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "squeal grunt", "squeal grunts"),
        srt = 45, adj = 1, las=2, horizontal = F, ylab = "Walking", ylim = c(0,1.1))

panting <- all_6887[all_6887$Name=="panting",]
boxplot((panting$chirpgr_freq/panting$sum_calls), (panting$chitter_freq/panting$sum_calls), (panting$grunt_freq/panting$sum_calls), (panting$snort_freq/panting$sum_calls), (panting$chirp_freq/panting$sum_calls), 
        (panting$chittering_freq/panting$sum_calls), (panting$growl_freq/panting$sum_calls), (panting$gruntbreath_freq/panting$sum_calls), (panting$hum_freq/panting$sum_calls), (panting$quack_freq/panting$sum_calls),
        (panting$squeal_freq/panting$sum_calls), (panting$squealchitters_freq/panting$sum_calls), (panting$squealing_freq/panting$sum_calls), (panting$barkgrunt_freq/panting$sum_calls), 
        (panting$barkgrunting_freq/panting$sum_calls), (panting$squealgrunt_freq/panting$sum_calls), (panting$squealgrunts_freq/panting$sum_calls), 
        names = c("chirpgr", "chitter", "grunt", "snort", "chirp", "chittering", "growl", "grunt breath", "hum",
                  "quack", "squeal", "squeal chitters", "squealing", "bark grunt", "bark grunting", "squeal grunt", "squeal grunts"),
        srt = 45, adj = 1, las=2, horizontal = F, ylab = "Panting", ylim = c(0,1.1))

#look at temporal pattern of calls - focus on chirpgrunts
#make new column with 1/0 for presence of chirpgr
all_6887$chirpgr_pres <- ifelse(all_6887$Name == "chirpgr", 1,0)
#do same for chitters
all_6887$chitter_pres <- ifelse(all_6887$Name == "chitter", 1,0)

#put all times in right format for converting to time
all_6887$Start <- as.vector(all_6887$Start)
nchar(all_6887$Start)
all_6887$NewStart = ifelse(nchar(all_6887$Start) == 9, paste0("00:",all_6887$Start), all_6887$Start)
all_6887$NewStart = ifelse(nchar(all_6887$Start) == 11, paste0("0",all_6887$Start), all_6887$NewStart)
all_6887$NewStart = ifelse(nchar(all_6887$Start) == 8, paste0("00:0",all_6887$Start), all_6887$NewStart)

#now convert to time format
#all_6887$NewStart = strftime(as.POSIXlt(all_6887$NewStart, format = "%H:%M:%S.%OS"), format = "%H:%M:%OS") 

#all_6887$NewStart = as.POSIXlt(all_6887$NewStart, format = "%H:%M:%OS", origin = "2020-02-22") 
#all_6887$time <- format(all_6887$NewStart, format = "%H:%M:%OS")
#want to have milliseconds but not sure how
#all_6887$milliseconds <- sep(all_6887$Start, ".")

all_6887$time <- hms::as_hms(all_6887$NewStart)

day_1 <- subset(all_6887[  all_6887$File=="6887_1det.csv", ])
day_2 <- subset(all_6887[  all_6887$File=="6887_2det.csv", ])
day_3 <- subset(all_6887[  all_6887$File=="6887_3det.csv", ])
day_4 <- subset(all_6887[  all_6887$File=="6887_4det.csv", ])
day_5 <- subset(all_6887[  all_6887$File=="6887_5det.csv", ])
day_6 <- subset(all_6887[  all_6887$File=="6887_6det.csv", ])

#par(mfrow=c(1,1))
#order the dataframe by time
day_1 <- day_1[order(day_1$time),]
day_2 <- day_2[order(day_2$time),]
day_3 <- day_3[order(day_3$time),]
day_4 <- day_4[order(day_4$time),]
day_5 <- day_5[order(day_5$time),]
day_6 <- day_6[order(day_6$time),]

#plot cumulative sum of chirpgrunts over time

#par(mfrow=c(3,2), mar=c(8,4,4,2))
#plot(day_1$time, cumsum(day_1$chirpgr_pres), type = "l", col = "maroon4", xaxt = "n", ylab = "Cumulative Sum of Calls", xlab = "Day 1")
#points(day_1$time, cumsum(day_1$chitter_pres), type = "l", col = "chartreuse3")
#axis.POSIXct(side=1, day_1$time, format = "%H:%M:%OS")
#legend(x = "topleft", legend = c("chirpgrunt", "chitter"), lty = c(1,1), col = c("maroon4", "chartreuse3"), bty = "n")
#plot(day_2$time, cumsum(day_2$chirpgr_pres), type = "l", col = "maroon4", xaxt = "n", ylab = "Cumulative Sum of Calls", xlab = "Day 2")
#points(day_2$time, cumsum(day_2$chitter_pres), type = "l", col = "chartreuse3")
#axis.POSIXct(side=1, day_2$time, format = "%H:%M:%OS")
#legend(x = "topleft", legend = c("chirpgrunt", "chitter"), lty = c(1,1), col = c("maroon4", "chartreuse3"), bty = "n")

par(mfrow=c(2,2), mar=c(8,4,4,2))
plot(day_3$time, cumsum(day_3$chirpgr_pres), type = "l", col = "maroon4", xaxt = "n", ylab = "Cumulative Sum of Calls", xlab = "Day 3")
points(day_3$time, cumsum(day_3$chitter_pres), type = "l", col = "chartreuse3")
axis.POSIXct(side=1, day_3$time, format = "%H:%M:%OS")
legend(x = "topleft", legend = c("chirpgrunt", "chitter"), lty = c(1,1), col = c("maroon4", "chartreuse3"), bty = "n")

plot(day_4$time, cumsum(day_4$chirpgr_pres), type = "l", col = "maroon4", xaxt = "n", ylab = "Cumulative Sum of Calls", xlab = "Day 4")
points(day_4$time, cumsum(day_4$chitter_pres), type = "l", col = "chartreuse3")
axis.POSIXct(side=1, day_4$time, format = "%H:%M:%OS")
legend(x = "topleft", legend = c("chirpgrunt", "chitter"), lty = c(1,1), col = c("maroon4", "chartreuse3"), bty = "n")

plot(day_5$time, cumsum(day_5$chirpgr_pres), type = "l", col = "maroon4", xaxt = "n", ylab = "Cumulative Sum of Calls", xlab = "Day 5")
points(day_5$time, cumsum(day_5$chitter_pres), type = "l", col = "chartreuse3")
axis.POSIXct(side=1, day_5$time, format = "%H:%M:%OS")
legend(x = "topleft", legend = c("chirpgrunt", "chitter"), lty = c(1,1), col = c("maroon4", "chartreuse3"), bty = "n")

plot(day_6$time, cumsum(day_6$chirpgr_pres), type = "l", col = "maroon4", xaxt = "n", ylab = "Cumulative Sum of Calls", xlab = "Day 6")
points(day_6$time, cumsum(day_6$chitter_pres), type = "l", col = "chartreuse3")
axis.POSIXct(side=1, day_6$time, format = "%H:%M:%OS")
legend(x = "topleft", legend = c("chirpgrunt", "chitter"), lty = c(1,1), col = c("maroon4", "chartreuse3"), bty = "n")


#get the number of calls within each time bin (e.g 10 min bins)

#make a list for each day


day_list <- split(all_6887, f = all_6887$File)

library(rbin)



  



breaks1 <- c(0, 600, 1200, 1800, 2400, 3000, 3600, 4200, 4800, 5400, 6000, 6600, 7200, 7800, 
            8400, 9000, 9600, 10200, 10800, 11400, 12000, 12600, 13200, 13800, 14400, 15000 )
intervals <- c("[0-600)","[600-1200)", "[1200-1800)", "[1800-2400)", "[2400-3000)", "[3000-3600)", "[3600-4200)", "[4200-4800)", 
                "[4800-5400)", "[5400-6000)", "[6000-6600)", "[6600-7200)", "[7200-7800)", "[7800-8400)", "[8400-9000)", "[9000-9600)",
                "[9600-10200)", "[10200-10800)", "[10800-11400)", "[11400-12000)", "[12000-12600)", "[12600-13200)", "[13200-13800)", 
                "[13800-14400)", "[14400-15000)")

#file <- day_list[i]


library(dplyr)

day_1 <- day_1 %>% mutate(binned_r_value = cut(day_1$starts, breaks = breaks1)) %>%
  group_by(binned_r_value) %>%
  tally()
#doesn't work




#marjory is 6887
#Marjory <- read.csv(file ='E:/PhD/Collars & maps/Eobs/Smarties/Marjory_forR.csv')
plot(Marjory$utm.easting, Marjory$utm.northing, type = "l")
Mar <- data.frame(x = 1:10,y = 1:10,col = 1:10)
ggplot(so,aes(x = x, y = y)) + 
  geom_line(aes(group = 1,colour = col))

#want to get gps points and calls in same data frame
#first need to put Start time in all_6887 into the right time format


























