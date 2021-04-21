#This code is the same as the label filtering to ensure all the labels are the same and then acoustic feature analysis


rm(list=ls())

setwd("D:/PhD/Desktop/Edic Mini")

#these files have ben filtered and the individual chitters have been labeled within chittering, squealing and squealing chitters
#6887
det_6887_1 <- read.csv("call labels 6887/detailed/file1_6887_detailed.csv", sep="\t")
det_6887_1 <- cbind(det_6887_1, day="6887_1")
det_6887_2 <- read.csv("call labels 6887/detailed/file2_6887_detailed.csv", sep="\t")
det_6887_2 <- cbind(det_6887_2, day="6887_2")
det_6887_3 <- read.csv("call labels 6887/detailed/file3_6887_detailed.csv", sep="\t")
det_6887_3 <- cbind(det_6887_3, day="6887_3")
det_6887_4 <- read.csv("call labels 6887/detailed/file4_6887_detailed.csv", sep="\t")
det_6887_4 <- cbind(det_6887_4, day="6887_4")
det_6887_5 <- read.csv("call labels 6887/detailed/file5_6887_detailed.csv", sep="\t")
det_6887_5 <- cbind(det_6887_5, day="6887_5")
det_6887_6 <- read.csv("call labels 6887/detailed/file6_6887_detailed.csv", sep="\t")
det_6887_6 <- cbind(det_6887_6, day="6887_6")
#6895
det_6895_1 <- read.csv("call labels 6895/detailed/file1_6895_detailed.csv", sep="\t")
det_6895_1 <- cbind(det_6895_1, day="6895_1")
det_6895_2 <- read.csv("call labels 6895/detailed/file2_6895_detailed.csv", sep="\t")
det_6895_2 <- cbind(det_6895_2, day="6895_2")
det_6895_3 <- read.csv("call labels 6895/detailed/file3_6895_detailed.csv", sep="\t")
det_6895_3 <- cbind(det_6895_3, day="6895_3")
det_6895_4 <- read.csv("call labels 6895/detailed/file4_6985_detailed.csv", sep="\t")
det_6895_4 <- cbind(det_6895_4, day="6895_4")
det_6895_5 <- read.csv("call labels 6895/detailed/file5_6895_detailed.csv", sep="\t")
det_6895_5 <- cbind(det_6895_5, day="6895_5")
det_6895_6 <- read.csv("call labels 6895/detailed/file6_6895_detailed.csv", sep="\t")
det_6895_6 <- cbind(det_6895_6, day="6895_6")
det_6895_7 <- read.csv("call labels 6895/detailed/file7_6895_detailed.csv", sep="\t")
det_6895_7 <- cbind(det_6895_7, day="6895_7")

#6887
detcombined_6887orig <- rbind(det_6887_1, det_6887_2, det_6887_3,det_6887_4, det_6887_5, det_6887_6)
colnames(detcombined_6887orig)[colnames(detcombined_6887orig) == "ï..Name"] <- "Name"
head(detcombined_6887orig)
write.csv(detcombined_6887orig, file="call labels 6887/detailed/detcombined_6887orig.csv",  row.names = F)
detcombined_6887orig <- read.csv(file="call labels 6887/detailed/detcombined_6887orig.csv")
unique(detcombined_6887orig$Name)

#now deting labels together reduce detail
detcombined_6887 <- detcombined_6887orig
unique(detcombined_6887$Name)

#6895
detcombined_6895orig <- rbind(det_6895_1, det_6895_2, det_6895_3, det_6895_4, det_6895_5, det_6895_6, det_6895_7)
colnames(detcombined_6895orig)[colnames(detcombined_6895orig) == "ï..Name"] <- "Name"
head(detcombined_6895orig)
write.csv(detcombined_6895orig, file="call labels 6895/detailed/detcombined_6895orig.csv",  row.names = F)
detcombined_6895orig <- read.csv(file="call labels 6895/detailed/detcombined_6895orig.csv")
unique(detcombined_6895orig$Name)

#now deting labels together reduce detail
detcombined_6895 <- detcombined_6895orig
unique(detcombined_6895$Name)

#redo cleaning to make sure all labels have been cleaned (copy + pasted code from label filtering)

detcombined_6887[detcombined_6887 == "chittering sqeal"] <- "squeal chitters"
detcombined_6887[detcombined_6887 == "chitters"] <- "chittering"
detcombined_6887[detcombined_6887 == "chittering squeal crescendo"] <- "squeal chitters"
detcombined_6887[detcombined_6887 == "chittersqueal"] <- "squeal chitters"
detcombined_6887[detcombined_6887 == "big squealing chitters"] <- "squeal chitters"
detcombined_6887[detcombined_6887 == "low squeal chitters"] <- "squeal chitters"
detcombined_6887[detcombined_6887 == "squealing chitters"] <- "squeal chitters"
detcombined_6887[detcombined_6887 == "loud chittering squeal"] <- "squeal chitters"
detcombined_6887[detcombined_6887 == "chitter squeals"] <- "squeal chitters"
detcombined_6887[detcombined_6887 == "chittering squeal"] <- "squeal chitters"
detcombined_6887[detcombined_6887 == "low chitters"] <- "chittering"
detcombined_6887[detcombined_6887 == "loud chittering"] <- "chittering"
detcombined_6887[detcombined_6887 == "loud chitter"] <- "chitter"
detcombined_6887[detcombined_6887 == "chittering sqeal crescendo"] <- "squeal chitters"
detcombined_6887[detcombined_6887 == "chittering squeals"] <- "squeal chitters"
detcombined_6887[detcombined_6887 == "chirping"] <- "chittering"
detcombined_6887[detcombined_6887 == "big chitter"] <- "chitter"
detcombined_6887[detcombined_6887 == "chirp chitters"] <- "chittering"
detcombined_6887[detcombined_6887 == "chirpgr n/4 "] <- "chirpgr n/4"
detcombined_6887[detcombined_6887 == "nf chittering"] <- "chittering"

#squeal 
#squealing
#squeal grunt
#squeal grunts
detcombined_6887[detcombined_6887 == "long squeal"] <- "squeal"
detcombined_6887[detcombined_6887 == "squeak"] <- "squeal"
detcombined_6887[detcombined_6887 == "low squeal buzz"] <- "squeal"
detcombined_6887[detcombined_6887 == "squeals"] <- "squealing"
detcombined_6887[detcombined_6887 == "big low squeal"] <- "squeal"
detcombined_6887[detcombined_6887 == "low squeak"] <- "squeal"
detcombined_6887[detcombined_6887 == "low squeal grunt"] <- "squeal grunt"
detcombined_6887[detcombined_6887 == "low squealgrunts"] <- "squeal grunts"
detcombined_6887[detcombined_6887 == "big squeal"] <- "squeal"
detcombined_6887[detcombined_6887 == "big squeals"] <- "squealing"
detcombined_6887[detcombined_6887 == "big squeal"] <- "squeal"
detcombined_6887[detcombined_6887 == "low squeal"] <- "squeal"
detcombined_6887[detcombined_6887 == "aggressive"] <- "squealing"
detcombined_6887[detcombined_6887 == "low squeal growl"] <- "squeal growl"
detcombined_6887[detcombined_6887 == "squeak growl"] <- "squeal growl"
detcombined_6887[detcombined_6887 == "nf low squeal"] <- "nf squeal"
detcombined_6887[detcombined_6887 == "low squeals"] <- "squealing"

#if no grunt in chirp, then just chirp (no chirptra, chirpd etc.)
detcombined_6887[detcombined_6887 == "chirptra n/1"] <- "chirp n/1"
detcombined_6887[detcombined_6887 == "chirptra y/1"] <- "chirp y/1"
detcombined_6887[detcombined_6887 == "chirptra y/4"] <- "chirp y/4"
detcombined_6887[detcombined_6887 == "chirpsa"] <- "chirp"
detcombined_6887[detcombined_6887 == "chirpdr"] <- "chirp"
detcombined_6887[detcombined_6887 == "nf chirptra"] <- "nf chirp"
detcombined_6887[detcombined_6887 == "chipgr n/1"] <- "chirpgr n/1"
detcombined_6887[detcombined_6887 == "nf chirpsr"] <- "nf chirp"
detcombined_6887[detcombined_6887 == "chirpsa"] <- "chirp"
detcombined_6887[detcombined_6887 == "chirptra"] <- "chirp"
detcombined_6887[detcombined_6887 == "chirptra n/2"] <- "chirp n/2"
detcombined_6887[detcombined_6887 == "chirpdra"] <- "chirp"
detcombined_6887[detcombined_6887 == "nf chirps"] <- "nf chirp"
detcombined_6887[detcombined_6887 == "chirpgr straight"] <- "chirpgr"
detcombined_6887[detcombined_6887 == "chirpdra breath"] <- "chirp"
detcombined_6887[detcombined_6887 == "chirps/bird"] <- "bird"
detcombined_6887[detcombined_6887 == "nf chirpda"] <- "nf chirp"
detcombined_6887[detcombined_6887 == "chirptra y/2"] <- "chirp y/2"
detcombined_6887[detcombined_6887 == "chirpsr"] <- "chirp"
detcombined_6887[detcombined_6887 == "chirpsra"] <- "chirp"
detcombined_6887[detcombined_6887 == "chirpd"] <- "chirp"
detcombined_6887[detcombined_6887 == "nf chirpsra"] <- "nf chirp"
detcombined_6887[detcombined_6887 == "nf chirpdra"] <- "nf chirp"
detcombined_6887[detcombined_6887 == "chirps"] <- "chirp"
detcombined_6887[detcombined_6887 == "chirpg n/1"] <- "chirpgr n/1"
detcombined_6887[detcombined_6887 == "chirp barkgr"] <- "chirpgr"
detcombined_6887[detcombined_6887 == "shaky chirp"] <- "chirp"
detcombined_6887[detcombined_6887 == "low chirp"] <- "chirp"
detcombined_6887[detcombined_6887 == "nf chirpdr"] <- "nf chirp"

#others
detcombined_6887[detcombined_6887 == "collar rub"] <- "collar"
detcombined_6887[detcombined_6887 == "huan"] <- "bird"
detcombined_6887[detcombined_6887 == "cicadas"] <- "insect"
detcombined_6887[detcombined_6887 == "sniffing"] <- "sniff"
detcombined_6887[detcombined_6887 == "big sniff"] <- "sniff"
detcombined_6887[detcombined_6887 == "big breath"] <- "breath"
detcombined_6887[detcombined_6887 == "run grunt"] <- "grunt"
detcombined_6887[detcombined_6887 == "slow grunt"] <- "grunt"
detcombined_6887[detcombined_6887 == "low grunt"] <- "grunt"
detcombined_6887[detcombined_6887 == "low hum"] <- "hum"
detcombined_6887[detcombined_6887 == "long buzz"] <- "buzz"
detcombined_6887[detcombined_6887 == "medium buzz"] <- "buzz"
detcombined_6887[detcombined_6887 == "squealhum"] <- "hum"
detcombined_6887[detcombined_6887 == "deep breath"] <- "breath"
detcombined_6887[detcombined_6887 == "exhale"] <- "breath"
detcombined_6887[detcombined_6887 == "inhale"] <- "breath"
detcombined_6887[detcombined_6887 == "medium grunt"] <- "grunt"
detcombined_6887[detcombined_6887 == "low grunt"] <- "grunt"
detcombined_6887[detcombined_6887 == "deep growl"] <- "growl"
detcombined_6887[detcombined_6887 == "low bop"] <- "bop"
detcombined_6887[detcombined_6887 == "upward buzz"] <- "buzz"
detcombined_6887[detcombined_6887 == "low grunts"] <- "grunting"
detcombined_6887[detcombined_6887 == "low buzz"] <- "buzz"
detcombined_6887[detcombined_6887 == "medium hum"] <- "hum"
detcombined_6887[detcombined_6887 == "fast walking"] <- "walking"
detcombined_6887[detcombined_6887 == "snore hum"] <- "snore"
detcombined_6887[detcombined_6887 == "short exhale"] <- "exhale"
detcombined_6887[detcombined_6887 == "lip smack"] <- "chop"

#6895 cleaning

#all chittering variants will be categorised to:
#chitter
#chittering
#squeal chitters
detcombined_6895[detcombined_6895 == "chittering sqeal"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "chitters"] <- "chittering"
detcombined_6895[detcombined_6895 == "chittering squeal crescendo"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "chittersqueal"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "big squealing chitters"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "low squeal chitters"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "squealing chitters"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "loud chittering squeal"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "chitter squeals"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "chittering squeal"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "low chitters"] <- "chittering"
detcombined_6895[detcombined_6895 == "loud chittering"] <- "chittering"
detcombined_6895[detcombined_6895 == "loud chitter"] <- "chitter"
detcombined_6895[detcombined_6895 == "chittering sqeal crescendo"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "chittering squeals"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "chirping"] <- "chittering"
detcombined_6895[detcombined_6895 == "big chitter"] <- "chitter"
detcombined_6895[detcombined_6895 == "chirp chitters"] <- "chittering"
detcombined_6895[detcombined_6895 == "chirpgr n/4 "] <- "chirpgr n/4"
detcombined_6895[detcombined_6895 == "squeal chittering"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "soft chitter"] <- "chitter"
detcombined_6895[detcombined_6895 == "double chitter squeal"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "low chitter"] <- "chitter"
detcombined_6895[detcombined_6895 == "double chitter squeal"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "low hum squeal"] <- "squeal"
detcombined_6895[detcombined_6895 == "loud chittering squeals"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "squeaking chitters"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "low squealing chitters"] <- "squeal chitters"
detcombined_6895[detcombined_6895 == "squeal chitter"] <- "chitter"
detcombined_6895[detcombined_6895 == "low chittering hum"] <- "chittering"
detcombined_6895[detcombined_6895 == "nf chittering"] <- "chittering"

#squeal 
#squealing
#squeal grunt
#squeal grunts
detcombined_6895[detcombined_6895 == "long squeal"] <- "squeal"
detcombined_6895[detcombined_6895 == "squeak"] <- "squeal"
detcombined_6895[detcombined_6895 == "low squeal buzz"] <- "squeal"
detcombined_6895[detcombined_6895 == "squeals"] <- "squealing"
detcombined_6895[detcombined_6895 == "big low squeal"] <- "squeal"
detcombined_6895[detcombined_6895 == "low squeak"] <- "squeal"
detcombined_6895[detcombined_6895 == "low squeal grunt"] <- "squeal grunt"
detcombined_6895[detcombined_6895 == "low squealgrunts"] <- "squeal grunts"
detcombined_6895[detcombined_6895 == "big squeal"] <- "squeal"
detcombined_6895[detcombined_6895 == "big squeals"] <- "squealing"
detcombined_6895[detcombined_6895 == "big squeal"] <- "squeal"
detcombined_6895[detcombined_6895 == "low squeal"] <- "squeal"
detcombined_6895[detcombined_6895 == "aggressive"] <- "squealing"
detcombined_6895[detcombined_6895 == "low squeal growl"] <- "squeal growl"
detcombined_6895[detcombined_6895 == "squeak growl"] <- "squeal growl"
detcombined_6895[detcombined_6895 == "nf low squeal"] <- "nf squeal"
detcombined_6895[detcombined_6895 == "low squeals"] <- "squealing"
detcombined_6895[detcombined_6895 == "low squealing"] <- "squealing"
detcombined_6895[detcombined_6895 == "low squealgr"] <- "squeal grunt"
detcombined_6895[detcombined_6895 == "low squeal n/2"] <- "squeal"
detcombined_6895[detcombined_6895 == "loud squealing"] <- "squealing"
detcombined_6895[detcombined_6895 == "nf squealgr"] <- "nf squeal grunt"

#if no grunt in chirp, then just chirp (no chirptra, chirpd etc.)
detcombined_6895[detcombined_6895 == "chirptra n/1"] <- "chirp n/1"
detcombined_6895[detcombined_6895 == "chirptra y/1"] <- "chirp y/1"
detcombined_6895[detcombined_6895 == "chirptra y/4"] <- "chirp y/4"
detcombined_6895[detcombined_6895 == "chirpsa"] <- "chirp"
detcombined_6895[detcombined_6895 == "chirpdr"] <- "chirp"
detcombined_6895[detcombined_6895 == "nf chirptra"] <- "nf chirp"
detcombined_6895[detcombined_6895 == "chipgr n/1"] <- "chirpgr n/1"
detcombined_6895[detcombined_6895 == "nf chirpsr"] <- "nf chirp"
detcombined_6895[detcombined_6895 == "chirpsa"] <- "chirp"
detcombined_6895[detcombined_6895 == "chirptra"] <- "chirp"
detcombined_6895[detcombined_6895 == "chirptra n/2"] <- "chirp n/2"
detcombined_6895[detcombined_6895 == "chirpdra"] <- "chirp"
detcombined_6895[detcombined_6895 == "nf chirps"] <- "nf chirp"
detcombined_6895[detcombined_6895 == "chirpgr straight"] <- "chirpgr"
detcombined_6895[detcombined_6895 == "chirpdra breath"] <- "chirp"
detcombined_6895[detcombined_6895 == "chirps/bird"] <- "bird"
detcombined_6895[detcombined_6895 == "nf chirpda"] <- "nf chirp"
detcombined_6895[detcombined_6895 == "chirptra y/2"] <- "chirp y/2"
detcombined_6895[detcombined_6895 == "chirpsr"] <- "chirp"
detcombined_6895[detcombined_6895 == "chirpsra"] <- "chirp"
detcombined_6895[detcombined_6895 == "chirpd"] <- "chirp"
detcombined_6895[detcombined_6895 == "nf chirpsra"] <- "nf chirp"
detcombined_6895[detcombined_6895 == "nf chirpdra"] <- "nf chirp"
detcombined_6895[detcombined_6895 == "chirps"] <- "chirp"
detcombined_6895[detcombined_6895 == "chirpg n/1"] <- "chirpgr n/1"
detcombined_6895[detcombined_6895 == "chirp barkgr"] <- "chirpgr"
detcombined_6895[detcombined_6895 == "shaky chirp"] <- "chirp"
detcombined_6895[detcombined_6895 == "low chirp"] <- "chirp"
detcombined_6895[detcombined_6895 == "nf chirpdr"] <- "nf chirp"
detcombined_6895[detcombined_6895 == "chirpgr long"] <- "chirpgr"
detcombined_6895[detcombined_6895 == "chirpda"] <- "chirp"
detcombined_6895[detcombined_6895 == "chirptra n/4"] <- "chirp n/4"
detcombined_6895[detcombined_6895 == "chirptra"] <- "chirp"
detcombined_6895[detcombined_6895 == "nf chirpsa"] <- "nf chirp"
detcombined_6895[detcombined_6895 == "chirps n/1"] <- "chirp n/1"
detcombined_6895[detcombined_6895 == "chirtra"] <- "chirp"
detcombined_6895[detcombined_6895 == "chirpdra y/1"] <- "chirp y/1"
detcombined_6895[detcombined_6895 == "nf chirptrs"] <- "nf chirp"
detcombined_6895[detcombined_6895 == "chirps long"] <- "chirp"
detcombined_6895[detcombined_6895 == "chitter chirp"] <- "chirp"
detcombined_6895[detcombined_6895 == "chirpgr squeal y/2"] <- "chirpgr y/2"

#others
detcombined_6895[detcombined_6895 == "collar rub"] <- "collar"
detcombined_6895[detcombined_6895 == "huan"] <- "bird"
detcombined_6895[detcombined_6895 == "cicadas"] <- "insect"
detcombined_6895[detcombined_6895 == "sniffing"] <- "sniff"
detcombined_6895[detcombined_6895 == "big sniff"] <- "sniff"
detcombined_6895[detcombined_6895 == "big breath"] <- "breath"
detcombined_6895[detcombined_6895 == "run grunt"] <- "grunt"
detcombined_6895[detcombined_6895 == "slow grunt"] <- "grunt"
detcombined_6895[detcombined_6895 == "low grunt"] <- "grunt"
detcombined_6895[detcombined_6895 == "low hum"] <- "hum"
detcombined_6895[detcombined_6895 == "long buzz"] <- "buzz"
detcombined_6895[detcombined_6895 == "medium buzz"] <- "buzz"
detcombined_6895[detcombined_6895 == "squealhum"] <- "hum"
detcombined_6895[detcombined_6895 == "deep breath"] <- "breath"
detcombined_6895[detcombined_6895 == "exhale"] <- "breath"
detcombined_6895[detcombined_6895 == "inhale"] <- "breath"
detcombined_6895[detcombined_6895 == "medium grunt"] <- "grunt"
detcombined_6895[detcombined_6895 == "low grunt"] <- "grunt"
detcombined_6895[detcombined_6895 == "deep growl"] <- "growl"
detcombined_6895[detcombined_6895 == "low bop"] <- "bop"
detcombined_6895[detcombined_6895 == "upward buzz"] <- "buzz"
detcombined_6895[detcombined_6895 == "low grunts"] <- "grunting"
detcombined_6895[detcombined_6895 == "low buzz"] <- "buzz"
detcombined_6895[detcombined_6895 == "medium hum"] <- "hum"
detcombined_6895[detcombined_6895 == "fast walking"] <- "walking"
detcombined_6895[detcombined_6895 == "snore hum"] <- "snore"
detcombined_6895[detcombined_6895 == "short exhale"] <- "exhale"
detcombined_6895[detcombined_6895 == "big snore"] <- "snore"
detcombined_6895[detcombined_6895 == "low bops"] <- "bop"
detcombined_6895[detcombined_6895 == "big growl"] <- "growl"
detcombined_6895[detcombined_6895 == "bee"] <- "insect"
detcombined_6895[detcombined_6895 == "bees"] <- "insect"
detcombined_6895[detcombined_6895 == "low bark"] <- "bark"
detcombined_6895[detcombined_6895 == "walking fast"] <- "walking"
detcombined_6895[detcombined_6895 == "chirptra n/3"] <- "chirp n/3"
detcombined_6895[detcombined_6895 == "nf grunting!!"] <- "nf grunting"
detcombined_6895[detcombined_6895 == "low growl"] <- "growl"
detcombined_6895[detcombined_6895 == "nd chirp"] <- "nf chirp"
detcombined_6895[detcombined_6895 == "high pitched calls"] <- "bird"
detcombined_6895[detcombined_6895 == "low squeak buzzes"] <- "buzz"
detcombined_6895[detcombined_6895 == "low coo"] <- "bop"
detcombined_6895[detcombined_6895 == "low coos"] <- "bop"
detcombined_6895[detcombined_6895 == "barkgr"] <- "bark grunt"
detcombined_6895[detcombined_6895 == "chewing with hums"] <- "chewing"
detcombined_6895[detcombined_6895 == "sigh"] <- "breath"
detcombined_6895[detcombined_6895 == "breathing"] <- "breath"
detcombined_6895[detcombined_6895 == "breath grunt"] <- "breath"
detcombined_6895[detcombined_6895 == "low grunt n/5"] <- "grunt"
detcombined_6895[detcombined_6895 == "hum growl"] <- "growl"


unique(detcombined_6887$Name)
unique(detcombined_6895$Name)

#save the csv back into detailed

for(day in unique(detcombined_6887$day)){
  dayTable <- detcombined_6887[which(detcombined_6887$day == day),-7]
  write.table(dayTable,file=paste("call labels 6887/detailed/",day,"det.csv",sep=""),row.names = F,quote=F,sep="\t" )
}

for(day in unique(detcombined_6895$day)){
  dayTable <- detcombined_6895[which(detcombined_6895$day == day),-7]
  write.table(dayTable,file=paste("call labels 6895/detailed/",day,"det.csv",sep=""),row.names = F,quote=F,sep="\t" )
}


#combine the individuals into a summary table with frequency for each individual per call
detcall_list6887 <- as.data.frame(unique(detcombined_6887$Name))
detcall_list6895 <- as.data.frame(unique(detcombined_6895$Name))

#saving list of call names in file - want to add a column for the call frequency in these files
detcall_list6887 <- as.data.frame(table(detcombined_6887$Name))
colnames(detcall_list6887)[colnames(detcall_list6887) == "Var1"] <- "call"
detcall_list6895 <- as.data.frame(table(detcombined_6895$Name))
colnames(detcall_list6895)[colnames(detcall_list6895) == "Var1"] <- "call"

library(dplyr)

#baptiste code to make a joint dataframe with frequency of call and name for both coatis #

detcall_list6887  <- read.csv("call labels 6887/detailed/detcall_list6887.csv")
detcall_list6895  <- read.csv("call labels 6895/detailed/detcall_list6895.csv")


callNames <- unique( c(as.character(detcall_list6887[,1]), as.character(detcall_list6895[,1])))
callValuesTable1 <- detcall_list6887[ match(callNames, as.character(detcall_list6887[,1])) ,2]  
callValuesTable2 <- detcall_list6895[ match(callNames, as.character(detcall_list6895[,1])) ,2] 

newTable <- as.data.frame(cbind(callNames,callValuesTable1,callValuesTable2))

call_listboth<- write.csv(newTable, "call_listboth.csv", row.names=F)

colnames(newTable)[colnames(newTable) == "callValuesTable1"] <- "6887"
colnames(newTable)[colnames(newTable) == "callValuesTable2"] <- "6895"

newTable[which(is.na(newTable[,2])),2] <- 0
newTable[which(is.na(newTable[,3])),3] <- 0

newTable <-  newTable[order(newTable[,1]),]

#number of calls for each call type
detcall_list6887 <- write.csv(detcall_list6887, "call labels 6887/detailed/detcall_list6887.csv", row.names = F)
detcall_list6895 <- write.csv(detcall_list6895, "call labels 6895/detailed/detcall_list6895.csv", row.names = F)


###############################################################################################################################################
###############################################################################################################################################

#Acoustic feature analysis

#want to extract:
#99% energy duration
#mean spectral entropy
#temporal entropy
#cepstral peak prominence
#centroid frequency
#peak frequency
#lower -10dB frequency, upper -10dB frequency
#root mean square (RMS) bandwidth
#-10 dB bandwidth
#fundamental frequency (F0) mean
#F0 start, F0 mid, F0 end
#signal-to-noise ratio (SNR)
#An additional variable F0 temp was generated to capture the change in fundamental frequency over time by subtracting F0 start from F0 end. 
#Noise level for SNR was calculated using the minimum of either the noise level in the 100ms pre- or post-signal. 
#SNR was used to remove noisy calls (SNR < 10dB, N=1,381) from the dataset prior to any analyses



rm(list=ls())

setwd("D:/PhD/Desktop/Edic Mini")

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

#file1_6887wav <- 'F:/PhD/Desktop/Edic Mini/Eobs edic mini_6887/file_1_6887_detailed.wav'
#file1_6887 <-readWave(file1_6887wav)
#file1_6887call_labels$Start <- hms(file1_6887call_labels$Start, format = "%H:%M:%OS")
#file1_6887call_labels <-  file1_6887call_labels[order(file1_6887call_labels[,2]),]
#file1_6887call_labels <- read.csv('F:/PhD/Desktop/Edic Mini/call labels 6887/detailed/cleaned detailed/6887_1det.csv', sep = '\t')

all_6887wav <- 'D:/PhD/Desktop/Edic Mini/all_6887.wav'
#all_6887 <- readWave(all_6887wav)
all_6887_labels <- read.csv('D:/PhD/Desktop/Edic Mini/all_6887_clean.csv', sep = '\t')
unique(all_6887_labels$ï..Name)

call_labelmetrics <- all_6887_labels

call_labelmetrics$domfrequency <- NA #before the loop, make an empty column to hold metric
call_labelmetrics$absolute_amplitude <- NA
call_labelmetrics$hilbert_amplitude <- NA

i=1

#loop for each wave file

for(i in seq(1, nrow(call_labelmetrics))){

  t0 <- call_labelmetrics$Start[i]
  
  if(length(strsplit(t0,":")[[1]])==2){
    t0 <- as.numeric(ms(t0)) #put the start time in seconds
  }else{
    t0 <- as.numeric(hms(t0)) #put the start time in seconds
  }
  dur <- call_labelmetrics$Duration[i]
  dur <- as.numeric(ms(dur)) #put duration in seconds
  
  if(dur>0.025){
    call <- readWave(all_6887wav, from = t0, to = t0 + dur, units = 'seconds')
    test=meanspec(call, f=22050) #so it doesn't take ages to run, calculate the mean
    #error in meanspec line
    
    if(!all(is.na(as.data.frame(test)$y))){
      domfrequency <- fpeaks(test, nmax = NULL, amp = NULL, freq = NULL, threshold = NULL, mel =FALSE, plot=F) #output of your metric
      dom_res=summary(domfrequency)
      absamplitude <- env(call@left, f=22050, envt = "abs",plot=F)
      hilbertamplitude <- env(call@left, f=22050, envt = "hil",plot=F)
      
      
      call_labelmetrics$domfrequency[i] <- domfrequency[1,1] #store the output in the table   
      call_labelmetrics$absolute_amplitude[i] <- absamplitude[1,1]
      call_labelmetrics$hilbert_amplitude[i] <- hilbertamplitude[1,1]
      #fundamental
    }
  }
  print(i)
}

str(call_labelmetrics$hilbert_amplitude)

hist(call_labelmetrics$domfrequency[which(call_labelmetrics$ï..Name=="chittering")])

#hard to extract fundamental frequency


#roughness records how curved the call is
rough <- meanspec(call@left, f=22050)
rough <- roughness(rough)




#visualise the spectrogram for each call label
wave_6887 <- 'D:/PhD/Desktop/Edic Mini/all_6887.wav'
#spectrogram(wave_6887, fs=22050, windowlength = 5)





