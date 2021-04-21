#the files have been cleaned of most spelling errors (done in label cleaning.R), the unk calls have been removed and the call/call types have been categorised

rm(list=ls())

setwd("F:/PhD/Desktop/Edic Mini")

##cleaning the labels which have been filtered on audition to remove unk's##

###6887###

cleaned_6887_1 <- read.csv("call labels 6887/6887_1_cleaned.csv", sep="\t")
cleaned_6887_1 <- cbind(cleaned_6887_1, day="6887_1")
cleaned_6887_2 <- read.csv("call labels 6887/6887_2_cleaned.csv", sep="\t")
cleaned_6887_2 <- cbind(cleaned_6887_2, day="6887_2")
cleaned_6887_3 <- read.csv("call labels 6887/6887_3_cleaned.csv", sep="\t")
cleaned_6887_3 <- cbind(cleaned_6887_3, day="6887_3")
cleaned_6887_4 <- read.csv("call labels 6887/6887_4_cleaned.csv", sep="\t")
cleaned_6887_4 <- cbind(cleaned_6887_4, day="6887_4")
cleaned_6887_5 <- read.csv("call labels 6887/6887_5_cleaned.csv", sep="\t")
cleaned_6887_5 <- cbind(cleaned_6887_5, day="6887_5")
cleaned_6887_6 <- read.csv("call labels 6887/6887_6_cleaned.csv", sep="\t")
cleaned_6887_6 <- cbind(cleaned_6887_6, day="6887_6")

#cleaning detailed file to put back into audition because already labelled chitters in the chittering
#detailed_6887_1 <- read.csv("call labels 6887/detailed/file1_6887_detailed.csv", sep="\t")
#detailed_6887_1 <- cbind(detailed_6887_1, day="6887_1")
#detailed_6887_2 <- read.csv("call labels 6887/detailed/file2_6887_detailed.csv", sep="\t")
#detailed_6887_2 <- cbind(detailed_6887_2, day="6887_2")
#detailed_6887_3 <- read.csv("call labels 6887/detailed/file3_6887_detailed.csv", sep="\t")
#detailed_6887_3 <- cbind(detailed_6887_3, day="6887_3")

#bindcombined_6887 <- rbind(detailed_6887_1, detailed_6887_2, detailed_6887_3)
#colnames(bindcombined_6887)[colnames(bindcombined_6887) == "ï..Name"] <- "Name"

##I ran the filtered code below, now saving the files back 

#for(day in unique(bindcombined_6887$day)){
#  dayTable <- bindcombined_6887[which(bindcombined_6887$day == day),-7]
#  write.table(dayTable,file=paste("call labels 6887/",day,"filter.csv",sep=""),row.names = F,quote=F,sep="\t" )
#}

##now saving these files into the detailed.wav files in audition




#putting all days into same file
cleanedcombined_6887orig <- rbind(cleaned_6887_1, cleaned_6887_2, cleaned_6887_3,cleaned_6887_4, cleaned_6887_5, cleaned_6887_6)
colnames(cleanedcombined_6887orig)[colnames(cleanedcombined_6887orig) == "ï..Name"] <- "Name"
head(cleanedcombined_6887orig)

write.csv(cleanedcombined_6887orig, file="call labels 6887/cleanedcombined_6887orig.csv",  row.names = F)

cleanedcombined_6887orig <- read.csv(file="call labels 6887/cleanedcombined_6887orig.csv")
unique(cleanedcombined_6887orig$Name)


#now binding labels together reduce detail
bindcombined_6887 <- cleanedcombined_6887orig

unique(bindcombined_6887$Name)

#all chittering variants will be categorised to:
#chitter
#chittering
#squeal chitters
bindcombined_6887[bindcombined_6887 == "chittering sqeal"] <- "squeal chitters"
bindcombined_6887[bindcombined_6887 == "chitters"] <- "chittering"
bindcombined_6887[bindcombined_6887 == "chittering squeal crescendo"] <- "squeal chitters"
bindcombined_6887[bindcombined_6887 == "chittersqueal"] <- "squeal chitters"
bindcombined_6887[bindcombined_6887 == "big squealing chitters"] <- "squeal chitters"
bindcombined_6887[bindcombined_6887 == "low squeal chitters"] <- "squeal chitters"
bindcombined_6887[bindcombined_6887 == "squealing chitters"] <- "squeal chitters"
bindcombined_6887[bindcombined_6887 == "loud chittering squeal"] <- "squeal chitters"
bindcombined_6887[bindcombined_6887 == "chitter squeals"] <- "squeal chitters"
bindcombined_6887[bindcombined_6887 == "chittering squeal"] <- "squeal chitters"
bindcombined_6887[bindcombined_6887 == "low chitters"] <- "chittering"
bindcombined_6887[bindcombined_6887 == "loud chittering"] <- "chittering"
bindcombined_6887[bindcombined_6887 == "loud chitter"] <- "chitter"
bindcombined_6887[bindcombined_6887 == "chittering sqeal crescendo"] <- "squeal chitters"
bindcombined_6887[bindcombined_6887 == "chittering squeals"] <- "squeal chitters"
bindcombined_6887[bindcombined_6887 == "chirping"] <- "chittering"
bindcombined_6887[bindcombined_6887 == "big chitter"] <- "chitter"
bindcombined_6887[bindcombined_6887 == "chirp chitters"] <- "chittering"
bindcombined_6887[bindcombined_6887 == "chirpgr n/4 "] <- "chirpgr n/4"
bindcombined_6887[bindcombined_6887 == "nf chittering"] <- "chittering"

 

#squeal 
#squealing
#squeal grunt
#squeal grunts
bindcombined_6887[bindcombined_6887 == "long squeal"] <- "squeal"
bindcombined_6887[bindcombined_6887 == "squeak"] <- "squeal"
bindcombined_6887[bindcombined_6887 == "low squeal buzz"] <- "squeal"
bindcombined_6887[bindcombined_6887 == "squeals"] <- "squealing"
bindcombined_6887[bindcombined_6887 == "big low squeal"] <- "squeal"
bindcombined_6887[bindcombined_6887 == "low squeak"] <- "squeal"
bindcombined_6887[bindcombined_6887 == "low squeal grunt"] <- "squeal grunt"
bindcombined_6887[bindcombined_6887 == "low squealgrunts"] <- "squeal grunts"
bindcombined_6887[bindcombined_6887 == "big squeal"] <- "squeal"
bindcombined_6887[bindcombined_6887 == "big squeals"] <- "squealing"
bindcombined_6887[bindcombined_6887 == "big squeal"] <- "squeal"
bindcombined_6887[bindcombined_6887 == "low squeal"] <- "squeal"
bindcombined_6887[bindcombined_6887 == "aggressive"] <- "squealing"
bindcombined_6887[bindcombined_6887 == "low squeal growl"] <- "squeal growl"
bindcombined_6887[bindcombined_6887 == "squeak growl"] <- "squeal growl"
bindcombined_6887[bindcombined_6887 == "nf low squeal"] <- "nf squeal"
bindcombined_6887[bindcombined_6887 == "low squeals"] <- "squealing"



#if no grunt in chirp, then just chirp (no chirptra, chirpd etc.)
bindcombined_6887[bindcombined_6887 == "chirptra n/1"] <- "chirp n/1"
bindcombined_6887[bindcombined_6887 == "chirptra y/1"] <- "chirp y/1"
bindcombined_6887[bindcombined_6887 == "chirptra y/4"] <- "chirp y/4"
bindcombined_6887[bindcombined_6887 == "chirpsa"] <- "chirp"
bindcombined_6887[bindcombined_6887 == "chirpdr"] <- "chirp"
bindcombined_6887[bindcombined_6887 == "nf chirptra"] <- "nf chirp"
bindcombined_6887[bindcombined_6887 == "chipgr n/1"] <- "chirpgr n/1"
bindcombined_6887[bindcombined_6887 == "nf chirpsr"] <- "nf chirp"
bindcombined_6887[bindcombined_6887 == "chirpsa"] <- "chirp"
bindcombined_6887[bindcombined_6887 == "chirptra"] <- "chirp"
bindcombined_6887[bindcombined_6887 == "chirptra n/2"] <- "chirp n/2"
bindcombined_6887[bindcombined_6887 == "chirpdra"] <- "chirp"
bindcombined_6887[bindcombined_6887 == "nf chirps"] <- "nf chirp"
bindcombined_6887[bindcombined_6887 == "chirpgr straight"] <- "chirpgr"
bindcombined_6887[bindcombined_6887 == "chirpdra breath"] <- "chirp"
bindcombined_6887[bindcombined_6887 == "chirps/bird"] <- "bird"
bindcombined_6887[bindcombined_6887 == "nf chirpda"] <- "nf chirp"
bindcombined_6887[bindcombined_6887 == "chirptra y/2"] <- "chirp y/2"
bindcombined_6887[bindcombined_6887 == "chirpsr"] <- "chirp"
bindcombined_6887[bindcombined_6887 == "chirpsra"] <- "chirp"
bindcombined_6887[bindcombined_6887 == "chirpd"] <- "chirp"
bindcombined_6887[bindcombined_6887 == "nf chirpsra"] <- "nf chirp"
bindcombined_6887[bindcombined_6887 == "nf chirpdra"] <- "nf chirp"
bindcombined_6887[bindcombined_6887 == "chirps"] <- "chirp"
bindcombined_6887[bindcombined_6887 == "chirpg n/1"] <- "chirpgr n/1"
bindcombined_6887[bindcombined_6887 == "chirp barkgr"] <- "chirpgr"
bindcombined_6887[bindcombined_6887 == "shaky chirp"] <- "chirp"
bindcombined_6887[bindcombined_6887 == "low chirp"] <- "chirp"
bindcombined_6887[bindcombined_6887 == "nf chirpdr"] <- "nf chirp"


#others
bindcombined_6887[bindcombined_6887 == "collar rub"] <- "collar"
bindcombined_6887[bindcombined_6887 == "huan"] <- "bird"
bindcombined_6887[bindcombined_6887 == "cicadas"] <- "insect"
bindcombined_6887[bindcombined_6887 == "sniffing"] <- "sniff"
bindcombined_6887[bindcombined_6887 == "big sniff"] <- "sniff"
bindcombined_6887[bindcombined_6887 == "big breath"] <- "breath"
bindcombined_6887[bindcombined_6887 == "run grunt"] <- "grunt"
bindcombined_6887[bindcombined_6887 == "slow grunt"] <- "grunt"
bindcombined_6887[bindcombined_6887 == "low grunt"] <- "grunt"
bindcombined_6887[bindcombined_6887 == "low hum"] <- "hum"
bindcombined_6887[bindcombined_6887 == "long buzz"] <- "buzz"
bindcombined_6887[bindcombined_6887 == "medium buzz"] <- "buzz"
bindcombined_6887[bindcombined_6887 == "squealhum"] <- "hum"
bindcombined_6887[bindcombined_6887 == "deep breath"] <- "breath"
bindcombined_6887[bindcombined_6887 == "exhale"] <- "breath"
bindcombined_6887[bindcombined_6887 == "inhale"] <- "breath"
bindcombined_6887[bindcombined_6887 == "medium grunt"] <- "grunt"
bindcombined_6887[bindcombined_6887 == "low grunt"] <- "grunt"
bindcombined_6887[bindcombined_6887 == "deep growl"] <- "growl"
bindcombined_6887[bindcombined_6887 == "low bop"] <- "bop"
bindcombined_6887[bindcombined_6887 == "upward buzz"] <- "buzz"
bindcombined_6887[bindcombined_6887 == "low grunts"] <- "grunting"
bindcombined_6887[bindcombined_6887 == "low buzz"] <- "buzz"
bindcombined_6887[bindcombined_6887 == "medium hum"] <- "hum"
bindcombined_6887[bindcombined_6887 == "fast walking"] <- "walking"
bindcombined_6887[bindcombined_6887 == "snore hum"] <- "snore"
bindcombined_6887[bindcombined_6887 == "short exhale"] <- "exhale"
bindcombined_6887[bindcombined_6887 == "lip smack"] <- "chop"



unique(bindcombined_6887$Name)

call_list6887 <- as.data.frame(unique(bindcombined_6887$Name))

for(day in unique(bindcombined_6887$day)){
  dayTable <- bindcombined_6887[which(bindcombined_6887$day == day),-7]
  write.table(dayTable,file=paste("call labels 6887/",day,"filter.csv",sep=""),row.names = F,quote=F,sep="\t" )
}



##need to keep filtering
##need to save to new csv files called filter6887_1 ect 





###################################################################################################################################
                      
###6895###

cleaned_6895_1 <- read.csv("call labels 6895/6895_1_cleaned.csv", sep="\t")
cleaned_6895_1 <- cbind(cleaned_6895_1, day="6895_1")
cleaned_6895_2 <- read.csv("call labels 6895/6895_2_cleaned.csv", sep="\t")
cleaned_6895_2 <- cbind(cleaned_6895_2, day="6895_2")
cleaned_6895_3 <- read.csv("call labels 6895/6895_3_cleaned.csv", sep="\t")
cleaned_6895_3 <- cbind(cleaned_6895_3, day="6895_3")
cleaned_6895_4 <- read.csv("call labels 6895/6895_4_cleaned.csv", sep="\t")
cleaned_6895_4 <- cbind(cleaned_6895_4, day="6895_4")
cleaned_6895_5 <- read.csv("call labels 6895/6895_5_cleaned.csv", sep="\t")
cleaned_6895_5 <- cbind(cleaned_6895_5, day="6895_5")
cleaned_6895_6 <- read.csv("call labels 6895/6895_6_cleaned.csv", sep="\t")
cleaned_6895_6 <- cbind(cleaned_6895_6, day="6895_6")
cleaned_6895_7 <- read.csv("call labels 6895/6895_7_cleaned.csv", sep="\t")
cleaned_6895_7 <- cbind(cleaned_6895_7, day="6895_7")


cleanedcombined_6895orig <- rbind(cleaned_6895_1, cleaned_6895_2, cleaned_6895_3,cleaned_6895_4, cleaned_6895_5, cleaned_6895_6, cleaned_6895_7)
colnames(cleanedcombined_6895orig)[colnames(cleanedcombined_6895orig) == "ï..Name"] <- "Name"
write.csv(cleanedcombined_6895orig, file="call labels 6895/cleanedcombined_6895orig.csv",  row.names = F)

cleanedcombined_6895orig <- read.csv(file="call labels 6895/cleanedcombined_6895orig.csv")

bindcombined_6895 <- cleanedcombined_6895orig

unique(bindcombined_6895$Name)

#cleaning detailed file to put back into audition because already labeled chitters in the chittering
#detailed_6895_1 <- read.csv("call labels 6895/detailed/file1_6895_detailed.csv", sep="\t")
#detailed_6895_1 <- cbind(detailed_6895_1, day="6895_1")
#detailed_6895_2 <- read.csv("call labels 6895/detailed/file2_6895_detailed.csv", sep="\t")
#detailed_6895_2 <- cbind(detailed_6895_2, day="6895_2")

#bindcombined_6895 <- rbind(detailed_6895_1, detailed_6895_2)
#colnames(bindcombined_6895)[colnames(bindcombined_6895) == "ï..Name"] <- "Name"

##I ran the filtered code below, now saving the files back 

for(day in unique(bindcombined_6895$day)){
  dayTable <- bindcombined_6895[which(bindcombined_6895$day == day),-7]
  write.table(dayTable,file=paste("call labels 6895/",day,"filter.csv",sep=""),row.names = F,quote=F,sep="\t" )
}



##used same filters as 6887 so some calls will still be missing - now adding them in

#all chittering variants will be categorised to:
#chitter
#chittering
#squeal chitters
bindcombined_6895[bindcombined_6895 == "chittering sqeal"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "chitters"] <- "chittering"
bindcombined_6895[bindcombined_6895 == "chittering squeal crescendo"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "chittersqueal"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "big squealing chitters"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "low squeal chitters"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "squealing chitters"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "loud chittering squeal"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "chitter squeals"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "chittering squeal"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "low chitters"] <- "chittering"
bindcombined_6895[bindcombined_6895 == "loud chittering"] <- "chittering"
bindcombined_6895[bindcombined_6895 == "loud chitter"] <- "chitter"
bindcombined_6895[bindcombined_6895 == "chittering sqeal crescendo"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "chittering squeals"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "chirping"] <- "chittering"
bindcombined_6895[bindcombined_6895 == "big chitter"] <- "chitter"
bindcombined_6895[bindcombined_6895 == "chirp chitters"] <- "chittering"
bindcombined_6895[bindcombined_6895 == "chirpgr n/4 "] <- "chirpgr n/4"
bindcombined_6895[bindcombined_6895 == "squeal chittering"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "soft chitter"] <- "chitter"
bindcombined_6895[bindcombined_6895 == "double chitter squeal"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "low chitter"] <- "chitter"
bindcombined_6895[bindcombined_6895 == "double chitter squeal"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "low hum squeal"] <- "squeal"
bindcombined_6895[bindcombined_6895 == "loud chittering squeals"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "squeaking chitters"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "low squealing chitters"] <- "squeal chitters"
bindcombined_6895[bindcombined_6895 == "squeal chitter"] <- "chitter"
bindcombined_6895[bindcombined_6895 == "low chittering hum"] <- "chittering"
bindcombined_6895[bindcombined_6895 == "nf chittering"] <- "chittering"




#squeal 
#squealing
#squeal grunt
#squeal grunts
bindcombined_6895[bindcombined_6895 == "long squeal"] <- "squeal"
bindcombined_6895[bindcombined_6895 == "squeak"] <- "squeal"
bindcombined_6895[bindcombined_6895 == "low squeal buzz"] <- "squeal"
bindcombined_6895[bindcombined_6895 == "squeals"] <- "squealing"
bindcombined_6895[bindcombined_6895 == "big low squeal"] <- "squeal"
bindcombined_6895[bindcombined_6895 == "low squeak"] <- "squeal"
bindcombined_6895[bindcombined_6895 == "low squeal grunt"] <- "squeal grunt"
bindcombined_6895[bindcombined_6895 == "low squealgrunts"] <- "squeal grunts"
bindcombined_6895[bindcombined_6895 == "big squeal"] <- "squeal"
bindcombined_6895[bindcombined_6895 == "big squeals"] <- "squealing"
bindcombined_6895[bindcombined_6895 == "big squeal"] <- "squeal"
bindcombined_6895[bindcombined_6895 == "low squeal"] <- "squeal"
bindcombined_6895[bindcombined_6895 == "aggressive"] <- "squealing"
bindcombined_6895[bindcombined_6895 == "low squeal growl"] <- "squeal growl"
bindcombined_6895[bindcombined_6895 == "squeak growl"] <- "squeal growl"
bindcombined_6895[bindcombined_6895 == "nf low squeal"] <- "nf squeal"
bindcombined_6895[bindcombined_6895 == "low squeals"] <- "squealing"
bindcombined_6895[bindcombined_6895 == "low squealing"] <- "squealing"
bindcombined_6895[bindcombined_6895 == "low squealgr"] <- "squeal grunt"
bindcombined_6895[bindcombined_6895 == "low squeal n/2"] <- "squeal"
bindcombined_6895[bindcombined_6895 == "loud squealing"] <- "squealing"
bindcombined_6895[bindcombined_6895 == "nf squealgr"] <- "nf squeal grunt"



#if no grunt in chirp, then just chirp (no chirptra, chirpd etc.)
bindcombined_6895[bindcombined_6895 == "chirptra n/1"] <- "chirp n/1"
bindcombined_6895[bindcombined_6895 == "chirptra y/1"] <- "chirp y/1"
bindcombined_6895[bindcombined_6895 == "chirptra y/4"] <- "chirp y/4"
bindcombined_6895[bindcombined_6895 == "chirpsa"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "chirpdr"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "nf chirptra"] <- "nf chirp"
bindcombined_6895[bindcombined_6895 == "chipgr n/1"] <- "chirpgr n/1"
bindcombined_6895[bindcombined_6895 == "nf chirpsr"] <- "nf chirp"
bindcombined_6895[bindcombined_6895 == "chirpsa"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "chirptra"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "chirptra n/2"] <- "chirp n/2"
bindcombined_6895[bindcombined_6895 == "chirpdra"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "nf chirps"] <- "nf chirp"
bindcombined_6895[bindcombined_6895 == "chirpgr straight"] <- "chirpgr"
bindcombined_6895[bindcombined_6895 == "chirpdra breath"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "chirps/bird"] <- "bird"
bindcombined_6895[bindcombined_6895 == "nf chirpda"] <- "nf chirp"
bindcombined_6895[bindcombined_6895 == "chirptra y/2"] <- "chirp y/2"
bindcombined_6895[bindcombined_6895 == "chirpsr"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "chirpsra"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "chirpd"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "nf chirpsra"] <- "nf chirp"
bindcombined_6895[bindcombined_6895 == "nf chirpdra"] <- "nf chirp"
bindcombined_6895[bindcombined_6895 == "chirps"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "chirpg n/1"] <- "chirpgr n/1"
bindcombined_6895[bindcombined_6895 == "chirp barkgr"] <- "chirpgr"
bindcombined_6895[bindcombined_6895 == "shaky chirp"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "low chirp"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "nf chirpdr"] <- "nf chirp"
bindcombined_6895[bindcombined_6895 == "chirpgr long"] <- "chirpgr"
bindcombined_6895[bindcombined_6895 == "chirpda"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "chirptra n/4"] <- "chirp n/4"
bindcombined_6895[bindcombined_6895 == "chirptra"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "nf chirpsa"] <- "nf chirp"
bindcombined_6895[bindcombined_6895 == "chirps n/1"] <- "chirp n/1"
bindcombined_6895[bindcombined_6895 == "chirtra"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "chirpdra y/1"] <- "chirp y/1"
bindcombined_6895[bindcombined_6895 == "nf chirptrs"] <- "nf chirp"
bindcombined_6895[bindcombined_6895 == "chirps long"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "chitter chirp"] <- "chirp"
bindcombined_6895[bindcombined_6895 == "chirpgr squeal y/2"] <- "chirpgr y/2"




#others
bindcombined_6895[bindcombined_6895 == "collar rub"] <- "collar"
bindcombined_6895[bindcombined_6895 == "huan"] <- "bird"
bindcombined_6895[bindcombined_6895 == "cicadas"] <- "insect"
bindcombined_6895[bindcombined_6895 == "sniffing"] <- "sniff"
bindcombined_6895[bindcombined_6895 == "big sniff"] <- "sniff"
bindcombined_6895[bindcombined_6895 == "big breath"] <- "breath"
bindcombined_6895[bindcombined_6895 == "run grunt"] <- "grunt"
bindcombined_6895[bindcombined_6895 == "slow grunt"] <- "grunt"
bindcombined_6895[bindcombined_6895 == "low grunt"] <- "grunt"
bindcombined_6895[bindcombined_6895 == "low hum"] <- "hum"
bindcombined_6895[bindcombined_6895 == "long buzz"] <- "buzz"
bindcombined_6895[bindcombined_6895 == "medium buzz"] <- "buzz"
bindcombined_6895[bindcombined_6895 == "squealhum"] <- "hum"
bindcombined_6895[bindcombined_6895 == "deep breath"] <- "breath"
bindcombined_6895[bindcombined_6895 == "exhale"] <- "breath"
bindcombined_6895[bindcombined_6895 == "inhale"] <- "breath"
bindcombined_6895[bindcombined_6895 == "medium grunt"] <- "grunt"
bindcombined_6895[bindcombined_6895 == "low grunt"] <- "grunt"
bindcombined_6895[bindcombined_6895 == "deep growl"] <- "growl"
bindcombined_6895[bindcombined_6895 == "low bop"] <- "bop"
bindcombined_6895[bindcombined_6895 == "upward buzz"] <- "buzz"
bindcombined_6895[bindcombined_6895 == "low grunts"] <- "grunting"
bindcombined_6895[bindcombined_6895 == "low buzz"] <- "buzz"
bindcombined_6895[bindcombined_6895 == "medium hum"] <- "hum"
bindcombined_6895[bindcombined_6895 == "fast walking"] <- "walking"
bindcombined_6895[bindcombined_6895 == "snore hum"] <- "snore"
bindcombined_6895[bindcombined_6895 == "short exhale"] <- "exhale"
bindcombined_6895[bindcombined_6895 == "big snore"] <- "snore"
bindcombined_6895[bindcombined_6895 == "low bops"] <- "bop"
bindcombined_6895[bindcombined_6895 == "big growl"] <- "growl"
bindcombined_6895[bindcombined_6895 == "bee"] <- "insect"
bindcombined_6895[bindcombined_6895 == "bees"] <- "insect"
bindcombined_6895[bindcombined_6895 == "low bark"] <- "bark"
bindcombined_6895[bindcombined_6895 == "walking fast"] <- "walking"
bindcombined_6895[bindcombined_6895 == "chirptra n/3"] <- "chirp n/3"
bindcombined_6895[bindcombined_6895 == "nf grunting!!"] <- "nf grunting"
bindcombined_6895[bindcombined_6895 == "low growl"] <- "growl"
bindcombined_6895[bindcombined_6895 == "nd chirp"] <- "nf chirp"
bindcombined_6895[bindcombined_6895 == "high pitched calls"] <- "bird"
bindcombined_6895[bindcombined_6895 == "low squeak buzzes"] <- "buzz"
bindcombined_6895[bindcombined_6895 == "low coo"] <- "bop"
bindcombined_6895[bindcombined_6895 == "low coos"] <- "bop"
bindcombined_6895[bindcombined_6895 == "barkgr"] <- "bark grunt"
bindcombined_6895[bindcombined_6895 == "chewing with hums"] <- "chewing"
bindcombined_6895[bindcombined_6895 == "sigh"] <- "breath"
bindcombined_6895[bindcombined_6895 == "breathing"] <- "breath"
bindcombined_6895[bindcombined_6895 == "breath grunt"] <- "breath"
bindcombined_6895[bindcombined_6895 == "low grunt n/5"] <- "grunt"
bindcombined_6895[bindcombined_6895 == "hum growl"] <- "growl"


unique(bindcombined_6895$Name)

call_list6895 <- as.data.frame(unique(bindcombined_6895$Name))

#saving list of call names in file - want to add a column for the call frequency in these files
unique(bindcombined_6895$Name)
unique(bindcombined_6887$Name)

call_list6887 <- as.data.frame(table(bindcombined_6887$Name))
colnames(call_list6887)[colnames(call_list6887) == "Var1"] <- "Call"

call_list6895 <- as.data.frame(table(bindcombined_6895$Name))
colnames(call_list6895)[colnames(call_list6895) == "Var1"] <- "Call"

library(dplyr)
test <- bind_rows(call_list6887, call_list6895, .id = "id")
test1 <- bind_rows(call_list6887$Call, call_list6895$Call)

#baptiste code to make a joint dataframe with frequency of call and name for both coatis #

callNames <- unique( c(as.character(call_list6887[,1]), as.character(call_list6895[,1])))
callValuesTable1 <- call_list6887[ match(callNames, as.character(call_list6887[,1])) ,2]  
callValuesTable2 <- call_list6895[ match(callNames, as.character(call_list6895[,1])) ,2]  

newTable <- as.data.frame(cbind(callNames,callValuesTable1,callValuesTable2))

newTable[which(is.na(newTable[,2])),2] <- 0
newTable[which(is.na(newTable[,3])),3] <- 0

newTable <-  newTable[order(newTable[,1]),]

call_list6895 <- write.csv(call_list6895, "call_list6895.csv", row.names = F)
call_list6887 <- write.csv(call_list6887, "call_list6887.csv", row.names = F)














