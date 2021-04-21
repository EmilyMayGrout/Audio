rm(list=ls())

setwd("F:/PhD/Desktop/Edic Mini")

#reading in 6887 files for each day
file1_6887 <- read.csv("call labels 6887/File 1/01.04.20_file1complete_6887.csv", sep="\t")
file1_6887 <- cbind(file1_6887, day="6887_1")
file2_6887 <- read.csv("call labels 6887/File 2/09.04.20_looped_file2_6887.csv", sep="\t")
file2_6887 <- cbind(file2_6887, day="6887_2")
file3_6887 <- read.csv("call labels 6887/File 3/04.04.2020_file3_6887_finished.csv", sep="\t")
file3_6887 <- cbind(file3_6887, day="6887_3")
file4_6887 <- read.csv("call labels 6887/File 4/complete_file4_6887.csv", sep="\t")
file4_6887 <- cbind(file4_6887, day="6887_4")
file5_6887 <- read.csv("call labels 6887/File 5/file5_complete_6887.csv", sep="\t")
file5_6887 <- cbind(file5_6887, day="6887_5")
file6_6887 <- read.csv("call labels 6887/File 6/file6_complete_6887.csv", sep="\t")
file6_6887 <- cbind(file6_6887, day="6887_6")

combined_6887orig <- rbind(file1_6887, file2_6887, file3_6887, file4_6887, file5_6887, file6_6887)
colnames(combined_6887orig)[colnames(combined_6887orig) == "ï..Name"] <- "Name"
write.csv(combined_6887orig, file="call labels 6887/combined_6887orig.csv")

combined_6887cleaned <- combined_6887orig

#delete rows with end and unsure comment
combined_6887cleaned <- combined_6887cleaned[-c(1778, 2646, 3981, 6722),] 

unique(combined_6887cleaned$Name)
#sorting spelling errors
combined_6887cleaned[grep("chiro", combined_6887cleaned$Name),1] <- sub("o", "p", combined_6887cleaned[grep("chiro", combined_6887cleaned$Name),1])
combined_6887cleaned[combined_6887cleaned == "chew"] <- "chewing"
combined_6887cleaned[combined_6887cleaned == "unk cluck?"] <- "unk cluck"
combined_6887cleaned[combined_6887cleaned == "soft scratch"] <- "scratch"
combined_6887cleaned[combined_6887cleaned == " unk bird"] <- "unk bird"
combined_6887cleaned[combined_6887cleaned == " chirpgr n/1"] <- "chirpgr n/1"
combined_6887cleaned[combined_6887cleaned == "chiter"] <- "chitter"
combined_6887cleaned[combined_6887cleaned == "Marker 294"] <- "head shake"
combined_6887cleaned[combined_6887cleaned == "chirpgr yn/1"] <- "chirpgr n/1"
combined_6887cleaned[combined_6887cleaned == "gurnt"] <- "grunt"
combined_6887cleaned[combined_6887cleaned == "chttering"] <- "chittering"
combined_6887cleaned[combined_6887cleaned == "chiewng"] <- "chewing"
combined_6887cleaned[combined_6887cleaned == "intect"] <- "insect"
combined_6887cleaned[combined_6887cleaned == "low grunt?"] <- "unk low grunt"
combined_6887cleaned[combined_6887cleaned == "unk low grunt?"] <- "unk low grunt"
combined_6887cleaned[combined_6887cleaned == "unk cluck?"] <- "unk cluck"
combined_6887cleaned[combined_6887cleaned == "bird?"] <- "unk bird"
combined_6887cleaned[combined_6887cleaned == "sqeal"] <- "squeal"
combined_6887cleaned[combined_6887cleaned == "eating"] <- "chewing"
combined_6887cleaned[combined_6887cleaned == "eating?"] <- "unk chewing"
combined_6887cleaned[combined_6887cleaned == "nfchirpgr"] <- "nf chirpgr"
combined_6887cleaned[combined_6887cleaned == "chirpdragr"] <- "chirpgr"
combined_6887cleaned[combined_6887cleaned == "chittering birds"] <- "bird"
combined_6887cleaned[combined_6887cleaned == "chittering birds"] <- "bird"
combined_6887cleaned[combined_6887cleaned == "low sqeual"] <- "low squeal"
combined_6887cleaned[combined_6887cleaned == "chirptra n/3"] <- "chirp n/3"
combined_6887cleaned[combined_6887cleaned == "chirpgr 1/1"] <- "chirpgr n/1"
combined_6887cleaned[combined_6887cleaned == "unk choming"] <- "unk chewing"
combined_6887cleaned[combined_6887cleaned == "bird? "] <- "unk bird"
combined_6887cleaned[combined_6887cleaned == "nchirpgr n/2"] <- "chirpgr n/2"
combined_6887cleaned[combined_6887cleaned == "chirps bird?"] <- "unk bird"
combined_6887cleaned[combined_6887cleaned == "unk bird.chirp"] <- "unk bird"
combined_6887cleaned[combined_6887cleaned == "nfchittering"] <- "nf chittering"
combined_6887cleaned[combined_6887cleaned == "chirpgr gr straight"] <- "chirpgr"
combined_6887cleaned[combined_6887cleaned == "chop chop/eating"] <- "chop chop/chewing"
combined_6887cleaned[combined_6887cleaned == "drinking/eating"] <- "drinking/chewing"
combined_6887cleaned[combined_6887cleaned == "unk bird/chitteirng"] <- "unk bird/chittering"
combined_6887cleaned[combined_6887cleaned == "ukn rhythm"] <- "unk rhythm"
combined_6887cleaned[combined_6887cleaned == "shaky chirpgr"] <- "chirpgr"
combined_6887cleaned[combined_6887cleaned == "loud chitters"] <- "chittering"
combined_6887cleaned[combined_6887cleaned == "chitters"] <- "chittering"
combined_6887cleaned[combined_6887cleaned == "unk chomp?"] <- "unk chomp"
combined_6887cleaned[combined_6887cleaned == "fast low squeals"] <- "low squeals"
combined_6887cleaned[combined_6887cleaned == "unk rhythm/drinking?"] <- "unk rhythm"
combined_6887cleaned[combined_6887cleaned == "unk bird/chirptra"] <- "unk bird/chirp"
combined_6887cleaned[combined_6887cleaned == "walk fast"] <- "fast walk"
combined_6887cleaned[combined_6887cleaned == "unk chomp?"] <- "unk chomp"
combined_6887cleaned[combined_6887cleaned == "chewing "] <- "chewing"
combined_6887cleaned[combined_6887cleaned == "chirps  "] <- "chirps"
combined_6887cleaned[combined_6887cleaned == "chirp "] <- "chirp"
combined_6887cleaned[combined_6887cleaned == "chewing "] <- "chewing"
combined_6887cleaned[combined_6887cleaned == "nf chirp "] <- "nf chirp"
combined_6887cleaned[combined_6887cleaned == "chirptra "] <- "chirptra"
combined_6887cleaned[combined_6887cleaned == "chitteirng squeals"] <- "chittering squeals"
combined_6887cleaned[combined_6887cleaned == "lark grunt"] <- "bark grunt"
combined_6887cleaned[combined_6887cleaned == "chitering"] <- "chittering"
combined_6887cleaned[combined_6887cleaned == "grunt "] <- "grunt"
combined_6887cleaned[combined_6887cleaned == "drinking?"] <- "unk drinking"
combined_6887cleaned[combined_6887cleaned == "chirs"] <- "chirps"
combined_6887cleaned[combined_6887cleaned == "sqeal triple"] <- "squealing"
combined_6887cleaned[combined_6887cleaned == "unk low dumdum"] <- "unk dumdum"
combined_6887cleaned[combined_6887cleaned == "big medium grunt"] <- "medium grunt"
combined_6887cleaned[combined_6887cleaned == "chirptra "] <- "chirptra"
combined_6887cleaned[combined_6887cleaned == "low bark"] <- "bark"
combined_6887cleaned[combined_6887cleaned == "unk dum dum"] <- "unk dumdum"
combined_6887cleaned[combined_6887cleaned == "sqeal double"] <- "squealing"
combined_6887cleaned[combined_6887cleaned == "chittering "] <- "chittering"
combined_6887cleaned[combined_6887cleaned == "squeal "] <- "squeal"
combined_6887cleaned[combined_6887cleaned == "chittering "] <- "chittering"
combined_6887cleaned[combined_6887cleaned == "chittering birds? "] <- "unk bird"
combined_6887cleaned[combined_6887cleaned == "deeo growl"] <- "deep growl"
combined_6887cleaned[combined_6887cleaned == "chittering squeel"] <- "chittering squeal"
combined_6887cleaned[combined_6887cleaned == "long sqeal"] <- "long squeal"
combined_6887cleaned[combined_6887cleaned == "unk low hum"] <- "unk hum"
combined_6887cleaned[combined_6887cleaned == "chittering birds?"] <- "unk bird"
combined_6887cleaned[combined_6887cleaned == "chirsra"] <- "chirpsra"
combined_6887cleaned[combined_6887cleaned == "open mouth?  "] <- "unk mouth open"
combined_6887cleaned[combined_6887cleaned == "chirps thin"] <- "chirps"
combined_6887cleaned[combined_6887cleaned == "chitteirng/bird"] <- "unk bird/chittering"
combined_6887cleaned[combined_6887cleaned == "walk/run"] <- "walking"
combined_6887cleaned[combined_6887cleaned == "nf chirptra "] <- "nf chirptra"
combined_6887cleaned[combined_6887cleaned == "chitteirng"] <- "chittering"
combined_6887cleaned[combined_6887cleaned == "low squeal "] <- "low squeal"
combined_6887cleaned[combined_6887cleaned == "hea dshake"] <- "head shake"
combined_6887cleaned[combined_6887cleaned == "unk waking up squeel"] <- "unk waking up squeal"
combined_6887cleaned[combined_6887cleaned == "shirpgr"] <- "chirpgr"
combined_6887cleaned[combined_6887cleaned == "chittering bird?"] <- "unk chittering bird"
combined_6887cleaned[combined_6887cleaned == "scratching"] <- "scratch"
combined_6887cleaned[combined_6887cleaned == "tree creek?"] <- "unk tree creek"
combined_6887cleaned[combined_6887cleaned == "chittering mixed"] <- "chittering"
combined_6887cleaned[combined_6887cleaned == "sqeaks and whine?"] <- "squeaks and unk whine"
combined_6887cleaned[combined_6887cleaned == "nf faint chitter/chirp"] <- "nf chitter/chirp"
combined_6887cleaned[combined_6887cleaned == "unk chirp/bird"] <- "unk bird/chirp"
combined_6887cleaned[combined_6887cleaned == "chirpdragr "] <- "chirpgr"
combined_6887cleaned[combined_6887cleaned == "sqeals"] <- "squealing"
combined_6887cleaned[combined_6887cleaned == "chirgr n/1"] <- "chirpgr n/1"
combined_6887cleaned[combined_6887cleaned == "unk chittering/bird"] <- "unk bird/chittering"
combined_6887cleaned[combined_6887cleaned == "chitters & sqeals"] <- "chittering squeals"
combined_6887cleaned[combined_6887cleaned == "chirpgr "] <- "chirpgr"
combined_6887cleaned[combined_6887cleaned == "unk chittering bird"] <- "unk bird"
combined_6887cleaned[combined_6887cleaned == "unk chomping"] <- "unk chewing"
combined_6887cleaned[combined_6887cleaned == "quack?"] <- "unk quack"
combined_6887cleaned[combined_6887cleaned == "faint squeal"] <- "nf squeal"
combined_6887cleaned[combined_6887cleaned == "panitng"] <- "panting"
combined_6887cleaned[combined_6887cleaned == "chirpgr n/1 "] <- "chirpgr n/1"
combined_6887cleaned[combined_6887cleaned == "unk low bird"] <- "unk bird"
combined_6887cleaned[combined_6887cleaned == "chirptra  "] <- "chirptra"
combined_6887cleaned[combined_6887cleaned == "grchirpgr"] <- "chirpgr"
combined_6887cleaned[combined_6887cleaned == "unk bird/chitters"] <- "unk bird/chittering"
combined_6887cleaned[combined_6887cleaned == "unk creeky door?"] <- "unk creeky door"
combined_6887cleaned[combined_6887cleaned == "chirpgr n/3 "] <- "chirpgr n/3"
combined_6887cleaned[combined_6887cleaned == "chirpgr n/2 "] <- "chirpgr n/2"
combined_6887cleaned[combined_6887cleaned == "nf chirpgr n/2 "] <- "nf chirpgr n/2"
combined_6887cleaned[combined_6887cleaned == "nf chirpgr  "] <- "nf chirpgr"
combined_6887cleaned[combined_6887cleaned == "unk chittering bird?"] <- "unk bird"
combined_6887cleaned[combined_6887cleaned == "nf chirpgr "] <- "nf chirpgr"
combined_6887cleaned[combined_6887cleaned == "cargo boat horn"] <- "cargo horn"
combined_6887cleaned[combined_6887cleaned == "water sound"] <- "water"
combined_6887cleaned[combined_6887cleaned == "unk songlike "] <- "unk songlike"
combined_6887cleaned[combined_6887cleaned == "unk high bird"] <- "unk bird"
combined_6887cleaned[combined_6887cleaned == "water quack"] <- "water"
combined_6887cleaned[combined_6887cleaned == "loud triple chittering"] <- "loud chittering"
combined_6887cleaned[combined_6887cleaned == "unk rhyhm"] <- "unk rhythm"
combined_6887cleaned[combined_6887cleaned == "barking/eating"] <- "barking/chewing"
combined_6887cleaned[combined_6887cleaned == "boat noise"] <- "cargo horn"
combined_6887cleaned[combined_6887cleaned == "low chittering squeal"] <- "chittering squeal"
combined_6887cleaned[combined_6887cleaned == "fast walk"] <- "walking"
combined_6887cleaned[combined_6887cleaned == "unk chittering bird?"] <- "unk bird"
combined_6887cleaned[combined_6887cleaned == "unk chitter/bird"] <- "unk bird/chitter"

unique(combined_6887cleaned$Name) 

write.csv(combined_6887cleaned, file="call labels 6887/combined_6887cleaned.csv", row.names = F)

#write a csv in audition format for each file seperately
 for(day in unique(combined_6887cleaned$day)){
    dayTable <- combined_6887cleaned[which(combined_6887cleaned$day == day),-7]
    write.table(dayTable,file=paste("call labels 6887/",day,".csv",sep=""),row.names = F,quote=F,sep="\t" )
  }
#the cleaned files are saved with the clean audio on audition
###next could filter calls into fewer categories###

combined_6887cleaned <- read.csv(file="call labels 6887/combined_6887cleaned.csv")



#reading in 6895 files for each day
file1_6895 <- read.csv("call labels 6895/file 1/14.05.20_file1_6895_complete.csv", sep="\t")
file1_6895 <- cbind(file1_6895, day="6895_1")
file2_6895 <- read.csv("call labels 6895/file 2/6895_file2_complete.csv", sep="\t")
file2_6895 <- cbind(file2_6895, day="6895_2")
file3_6895 <- read.csv("call labels 6895/file 3/complete_6895_file 3.csv", sep="\t")
file3_6895 <- cbind(file3_6895, day="6895_3")
file4_6895 <- read.csv("call labels 6895/file 4/complete_file4_6895.csv", sep="\t")
file4_6895 <- cbind(file4_6895, day="6895_4")
file5_6895 <- read.csv("call labels 6895/file 5/finished_6895_file5.csv", sep="\t")
file5_6895 <- cbind(file5_6895, day="6895_5")
file6_6895 <- read.csv("call labels 6895/file 6/file6_complete_6895.csv", sep="\t")
file6_6895 <- cbind(file6_6895, day="6895_6")
file7_6895 <- read.csv("call labels 6895/file 7/file7_complete_6895.csv", sep="\t")
file7_6895 <- cbind(file7_6895, day="6895_7")

combined_6895orig <- rbind(file1_6895, file2_6895, file3_6895, file4_6895, file5_6895, file6_6895, file7_6895)
colnames(combined_6895orig)[colnames(combined_6895orig) == "ï..Name"] <- "Name"
write.csv(combined_6895orig, file="call labels 6895/combined_6895orig.csv",  row.names = F)

combined_6895orig <- read.csv(file="call labels 6895/combined_6895orig.csv")

combined_6895cleaned <- combined_6895orig

#delete rows with end and incorrect labels
combined_6895cleaned <- combined_6895cleaned[-c(625, 10169, 10229),] 

unique(combined_6895cleaned$Name)
#sorting spelling errors
combined_6895cleaned[grep("chiro", combined_6895cleaned$Name),1] <- sub("o", "p", combined_6895cleaned[grep("chiro", combined_6895cleaned$Name),1])
combined_6895cleaned[combined_6895cleaned == "eating"] <- "chewing"
combined_6895cleaned[combined_6895cleaned == "plane"] <- "engine"
combined_6895cleaned[combined_6895cleaned == "unk bird?"] <- "unk bird"
combined_6895cleaned[combined_6895cleaned == "chirpgr  "] <- "chirpgr"
combined_6895cleaned[combined_6895cleaned == "chirpgr "] <- "chirpgr"
combined_6895cleaned[combined_6895cleaned == "bird?"] <- "unk bird"
combined_6895cleaned[combined_6895cleaned == "unk bird?"] <- "unk bird"
combined_6895cleaned[combined_6895cleaned == "whine?"] <- "unk whine"
combined_6895cleaned[combined_6895cleaned == "snore "] <- "snore"
combined_6895cleaned[combined_6895cleaned == "anthropogenic noise"] <- "engine"
combined_6895cleaned[combined_6895cleaned == "moneky?"] <- "capuchin"
combined_6895cleaned[combined_6895cleaned == "water bop"] <- "water"
combined_6895cleaned[combined_6895cleaned == "low brr "] <- "low brr"
combined_6895cleaned[combined_6895cleaned == "howler loud"] <- "howler"
combined_6895cleaned[combined_6895cleaned == "unk bird chirps?"] <- "unk bird/chirps"
combined_6895cleaned[combined_6895cleaned == "unk bird or chittering"] <- "unk bird/chittering"
combined_6895cleaned[combined_6895cleaned == "chirpr n/1"] <- "chirpgr n/1"
combined_6895cleaned[combined_6895cleaned == " chirpgr n/1"] <- "chirpgr n/1"
combined_6895cleaned[combined_6895cleaned == "chirgr y/2"] <- "chirpgr y/2"
combined_6895cleaned[combined_6895cleaned == "brd"] <- "bird"
combined_6895cleaned[combined_6895cleaned == "f chirpgr"] <- "nf chirpgr"
combined_6895cleaned[combined_6895cleaned == "driking"] <- "drinking"
combined_6895cleaned[combined_6895cleaned == " chirpgr y/3"] <- "chirpgr y/3"
combined_6895cleaned[combined_6895cleaned == "chirpgr  y/4"] <- "chirpgr y/4"
combined_6895cleaned[combined_6895cleaned == "scratch "] <- "scratch"
combined_6895cleaned[combined_6895cleaned == "gurnt"] <- "grunt"
combined_6895cleaned[combined_6895cleaned == "squal chitter"] <- "squeal chitter"
combined_6895cleaned[combined_6895cleaned == "Marker 134"] <- "scratch"
combined_6895cleaned[combined_6895cleaned == "chitteirng squeal"] <- "chittering squeal"
combined_6895cleaned[combined_6895cleaned == "unk chitter/bird"] <- "unk bird/chitter"
combined_6895cleaned[combined_6895cleaned == "chiter"] <- "chitter"
combined_6895cleaned[combined_6895cleaned == "unk rythm/scratching"] <- "unk rhythm/scratching"
combined_6895cleaned[combined_6895cleaned == "chewing "] <- "chewing"
combined_6895cleaned[combined_6895cleaned == "unk coati or bird"] <- "unk bird"
combined_6895cleaned[combined_6895cleaned == "eating with hums"] <- "chewing with hums"
combined_6895cleaned[combined_6895cleaned == "harmonic squeal"] <- "squeal"
combined_6895cleaned[combined_6895cleaned == "unk brid "] <- "unk bird"
combined_6895cleaned[combined_6895cleaned == "nf chirda"] <- "nf chirpda"
combined_6895cleaned[combined_6895cleaned == "helicopter"] <- "engine"
combined_6895cleaned[combined_6895cleaned == "sqeual grunt"] <- "squeal grunt"
combined_6895cleaned[combined_6895cleaned == "chiprgr"] <- "chirpgr"
combined_6895cleaned[combined_6895cleaned == "unk sqeak"] <- "unk squeak"
combined_6895cleaned[combined_6895cleaned == "nf chirda"] <- "nf chirpda"
combined_6895cleaned[combined_6895cleaned == "chirpgr n/q"] <- "chirpgr n/2"
combined_6895cleaned[combined_6895cleaned == "chirpgr n/3 "] <- "chirpgr n/3"
combined_6895cleaned[combined_6895cleaned == "chitters "] <- "chitters"
combined_6895cleaned[combined_6895cleaned == "chirpgr n/4 "] <- "chirpgr n/4"
combined_6895cleaned[combined_6895cleaned == " exhale"] <- "exhale"
combined_6895cleaned[combined_6895cleaned == "chirgpgr n/5"] <- "chirpgr n/5"
combined_6895cleaned[combined_6895cleaned == "chrpgr n/2"] <- "chirpgr n/2"
combined_6895cleaned[combined_6895cleaned == "unk rythm"] <- "unk rhythm"
combined_6895cleaned[combined_6895cleaned == "quacking?"] <- "unk quack"
combined_6895cleaned[combined_6895cleaned == "unk grunt "] <- "unk grunt"
combined_6895cleaned[combined_6895cleaned == "scratching"] <- "scratch"
combined_6895cleaned[combined_6895cleaned == "snore hum "] <- "snore hum"
combined_6895cleaned[combined_6895cleaned == "unk sqeals"] <- "unk squeals"
combined_6895cleaned[combined_6895cleaned == "chirpgr n/2  "] <- "chirpgr n/2"
combined_6895cleaned[combined_6895cleaned == "chirpgr n/1 "] <- "chirpgr n/1"
combined_6895cleaned[combined_6895cleaned == "nf chirptra "] <- "nf chirptra"
combined_6895cleaned[combined_6895cleaned == "nf chirpgr "] <- "nf chirpgr"
combined_6895cleaned[combined_6895cleaned == "unk low grunt "] <- "unk low grunt"
combined_6895cleaned[combined_6895cleaned == "low squeak "] <- "low squeak"
combined_6895cleaned[combined_6895cleaned == "plane?"] <- "engine"
combined_6895cleaned[combined_6895cleaned == "chirpgr n/2 "] <- "chirpgr n/2"
combined_6895cleaned[combined_6895cleaned == "growl "] <- "growl"
combined_6895cleaned[combined_6895cleaned == "quacking?"] <- "unk quack"
combined_6895cleaned[combined_6895cleaned == "unk chittering/bird"] <- "unk bird/chittering"
combined_6895cleaned[combined_6895cleaned == "chittering/bird"] <- "unk bird/chittering"
combined_6895cleaned[combined_6895cleaned == "long big buzz"] <- "long buzz"
combined_6895cleaned[combined_6895cleaned == "hirpgr y/3"] <- "chirpgr y/3"
combined_6895cleaned[combined_6895cleaned == "mchirpgr n/4"] <- "chirpgr n/4"
combined_6895cleaned[combined_6895cleaned == "sqeal chitter"] <- "squeal chitter"
combined_6895cleaned[combined_6895cleaned == "unk high chittering"] <- "unk bird/chittering"
combined_6895cleaned[combined_6895cleaned == "squeal/squeal"] <- "squeal/squeak"
combined_6895cleaned[combined_6895cleaned == "nf chirp "] <- "nf chirp"
combined_6895cleaned[combined_6895cleaned == "nf chirpg"] <- "nf chirpgr"
combined_6895cleaned[combined_6895cleaned == "loud bird"] <- "bird"
combined_6895cleaned[combined_6895cleaned == "nf low grunt"] <- "nf grunt"
combined_6895cleaned[combined_6895cleaned == "unk clucking/eating"] <- "unk clucking/chewing"
combined_6895cleaned[combined_6895cleaned == "chirpgrgr"] <- "chirpgr"
combined_6895cleaned[combined_6895cleaned == "chripgr n/1"] <- "chirpgr n/1"
combined_6895cleaned[combined_6895cleaned == "nf chirgpr"] <- "nf chirpgr"
combined_6895cleaned[combined_6895cleaned == "chitter/bird"] <- "unk bird/chitter"
combined_6895cleaned[combined_6895cleaned == "chitter/unk bird"] <- "unk bird/chitter"
combined_6895cleaned[combined_6895cleaned == "low sqeual"] <- "low squeal"
combined_6895cleaned[combined_6895cleaned == "nf chirpgr  "] <- "nf chirpgr"
combined_6895cleaned[combined_6895cleaned == "small whine"] <- "whine"
combined_6895cleaned[combined_6895cleaned == "chirgr n/2"] <- "chirpgr n/2"
combined_6895cleaned[combined_6895cleaned == "unk low squeak buz"] <- "unk low squeak buzz"
combined_6895cleaned[combined_6895cleaned == "unk chirp/bird"] <- "unk bird/chirp"
combined_6895cleaned[combined_6895cleaned == "back scratch?"] <- "scratch"
combined_6895cleaned[combined_6895cleaned == "low squealgrunt"] <- "low squealgr"
combined_6895cleaned[combined_6895cleaned == "chitering"] <- "low squealgr"
combined_6895cleaned[combined_6895cleaned == "unk scratching"] <- "scratch"
combined_6895cleaned[combined_6895cleaned == "unk drinking"] <- "unk drinking/chopchop"

unique(combined_6895cleaned$Name)

write.csv(combined_6895cleaned, file="call labels 6895/combined_6895cleaned.csv", row.names = F)

for(day in unique(combined_6895cleaned$day)){
  dayTable <- combined_6895cleaned[which(combined_6895cleaned$day == day),-7]
  write.table(dayTable,file=paste("call labels 6895/",day,".csv",sep=""),row.names = F,quote=F,sep="\t" )
}


#the cleaned files are saved with the clean audio on audition

#now filtering calls on audition and saving it to new file 


save.image(file = "label cleaning.RData")

which(all_6887$Name == "chitteirng squeals")


