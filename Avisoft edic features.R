#this script is to put the acoustic features into a dataframe and get the average value for each call type with standard deviations

setwd("E:/PhD/R/Audio/Edic mini/Avisoft features/")

files <- list.files(pattern=".txt")

header <- read.delim("E:/PhD/Avisoft/headings.txt")

all_features <- NA
for(file in files){
  
  features <- read.delim(file=file,h=F)
  colnames(features) <- colnames(header)
  all_features = rbind(all_features, features, make.row.names=FALSE) 
}

all_features <-  all_features[-1,]
all_features <- all_features[,-1]

#now want to get average values with SD for each call type 

unique(all_features$label)

library(tidyverse)

combined_mean <- all_features %>%
                 group_by(label) %>%
                 summarise_all(mean)
combined_mean <- combined_mean[,-2] 

#adding column name to tell difference between mean and SD

colnames(combined_mean) <- paste("mean", colnames(combined_mean))

#do same calculations for SD

combined_sd <- all_features %>%
               group_by(label) %>%
               summarise_all(sd)

combined_sd <- combined_sd[,-2]  
colnames(combined_sd) <- paste("sd", colnames(combined_sd))

#merging the mean and sd into the same data frame (it has 121 columns so not ideal)
mean_sd_features <- merge(combined_mean, combined_sd, by.x ="mean label", by.y="sd label" , all=T )


colnames(mean_sd_features)

#plotting the mean values
barplot(combined_mean$`mean duration`, names.arg =  combined_mean$`mean label`, las=2)

#need to remove non-coati sounds

#just for chirpgrunt
str(all_features)
#make a dataframe for only coati calls
unique(all_features$label)
calls_all_features <-  all_features[ all_features$label %in% c("chirpgr", "buzz","cluck", "grunt", "snort" ,"chirpgr" ,"nf chirpgr","chitter" , "chittering" , "chirp", "quack" ,"squeal" ,"squealing","nf chirp" ,"growl" ,  "hum" , "dumdum" , "click grunt","squeal chitters", "purr",                   
 "nf click grunt", "nf squeal", "barking" , "grunt breath","chirpgr n/1" , "chirpgr y/2" ,"chirpgr y/1","chirpgr n/3" ,"chirpgr y/3" , "chirpgr n/4" , "chirpgr n/2","chirpgr y/4","bark grunt","bop", "chirp n/1", "nf barking","chirp n/2","bark grunting", "squeal grunt", "click", 
 "chirpgr n/5","nf chirpgr n/5", "grunting","chirp y/1","nf chirpgr n/2","nf grunt", "bark","chirpgr y/5" ,"chirp n/3", "chirp y/2","nf chitter","nf clickgr","squeal grunts","squeal growl", "chirp y/4", "chirp n/4", "chirp y/3", "snore grunt", "snore squeal","whine","chop chop",              
 "chuckles","nf grunting","nf squeal grunt","growl breath" , "chitter squeal","chirp y/5","whistle","nf growl","grunt n/2","chirp n/5"), ]

#to simplify the chirpgr so can compare general features
calls_all_features[calls_all_features == "chirpgr n/1"] <- "chirpgr"
calls_all_features[calls_all_features == "chirpgr n/2"] <- "chirpgr"
calls_all_features[calls_all_features == "chirpgr n/3"] <- "chirpgr"
calls_all_features[calls_all_features == "chirpgr n/4"] <- "chirpgr"
calls_all_features[calls_all_features == "chirpgr n/5"] <- "chirpgr"
calls_all_features[calls_all_features == "chirpgr y/1"] <- "chirpgr"
calls_all_features[calls_all_features == "chirpgr y/2"] <- "chirpgr"
calls_all_features[calls_all_features == "chirpgr y/3"] <- "chirpgr"
calls_all_features[calls_all_features == "chirpgr y/4"] <- "chirpgr"
calls_all_features[calls_all_features == "chirpgr y/5"] <- "chirpgr"
calls_all_features[calls_all_features == "nf chirpgr n/5"] <- "chirpgr"
calls_all_features[calls_all_features == "chirp y/3"] <- "chirp"
calls_all_features[calls_all_features == "chirp y/2"] <- "chirp"
calls_all_features[calls_all_features == "chirp n/1"] <- "chirp"
calls_all_features[calls_all_features == "chirp n/4"] <- "chirp"
calls_all_features[calls_all_features == "chirp n/5"] <- "chirp"
calls_all_features[calls_all_features == "chirp y/5"] <- "chirp"
calls_all_features[calls_all_features == "chirp n/3"] <- "chirp"
calls_all_features[calls_all_features == "chirp y/1"] <- "chirp"
calls_all_features[calls_all_features == "chirp y/4"] <- "chirp"
calls_all_features[calls_all_features == "chirp n/2"] <- "chirp"
calls_all_features[calls_all_features == "nf chirp n/2"] <- "nf chirp"
calls_all_features[calls_all_features == "nf chirpgr n/2"] <- "nf chirp"
calls_all_features[calls_all_features == "grunt n/2"] <- "grunt"

unique(calls_all_features$label)

#only chirpgr
chirpgr_all_features <-  calls_all_features[ calls_all_features$label == c("chirpgr"), ] 

#removing nf calls

focal_all_calls <- calls_all_features
unique(focal_all_calls$label)

focal_all_calls <-  focal_all_calls[ focal_all_calls$label %in% c("buzz","cluck","grunt","snort","chirpgr","chitter","chittering","chirp",         
               "quack","squeal","squealing","growl", "hum","dumdum","click grunt","squeal chitters","purr","barking","grunt breath","bark grunt",
               "bop","nf barking","bark grunting","squeal grunt","click","grunting","bark","squeal grunts","squeal growl", 
              "snore grunt","snore squeal","whine","chop chop","chuckles","growl breath","chitter squeal","whistle"),]      
  
#want to plot it with raw data to get error bars - not sure how to do this
#first get the mean and sd for each call type (code repeated from above)

call_mean <- focal_all_calls %>%
  group_by(label) %>%
  summarise_all(mean)
call_mean <- call_mean[,-2] 
#adding column name to tell difference between mean and SD
colnames(call_mean) <- paste("mean", colnames(call_mean))

#do same calculations for SD

call_sd <- focal_all_calls %>%
  group_by(label) %>%
  summarise_all(sd)
call_sd <- call_sd[,-2]  
colnames(call_sd) <- paste("sd", colnames(call_sd))

#merging the mean and sd into the same data frame (it has 121 columns so not ideal)
mean_sd_calls <- merge(call_mean, call_sd, by.x ="mean label", by.y="sd label" , all=T )


#look at freq of all calls
op <- par(mar=c(8,4,4,2)) # the 10 allows the names to fit below the barplot

#removing some rarer/redundant call types for the plot
mean_sd_calls_test <- mean_sd_calls[-c(3,4,11,17,21,24,34,35 ),]

#for peak frequency centre
plot <- barplot(height = mean_sd_calls_test$`mean peak.freq.centre.`, names = mean_sd_calls_test$`mean label`, ylim=c(0,11000),
        horiz = F, las = 2, bty="n", cex.axis = 0.9, cex.names = 1.2, col = "plum4", ylab = "Mean peak frequency")
arrows(plot, mean_sd_calls_test$`mean peak.freq.centre.`- mean_sd_calls_test$`sd peak.freq.centre.`,
       plot, mean_sd_calls_test$`mean peak.freq.centre.`+ mean_sd_calls_test$`sd peak.freq.centre.`, angle=90, code=3, length=0.05)

#for peak amplitude centre

plot <- barplot(height = mean_sd_calls$`mean peak.ampl.centre.`, names = mean_sd_calls$`mean label`, ylim=c(-55, 0),
                horiz = F, las = 2, bty="n", cex.axis = 0.9, cex.names = 0.8, col = "plum4")
arrows(plot, mean_sd_calls$`mean peak.ampl.centre.`- mean_sd_calls$`sd peak.ampl.centre.`,
       plot, mean_sd_calls$`mean peak.ampl.centre.`+ mean_sd_calls$`sd peak.ampl.centre.`, angle=90, code=3, length=0.05)

#for min frequency centre
plot <- barplot(height = mean_sd_calls$`mean min.freq.centre.`, names = mean_sd_calls$`mean label`, ylim=c(0,11000),
                horiz = F, las = 2, bty="n", cex.axis = 0.9, cex.names = 0.8, col = "plum4")
arrows(plot, mean_sd_calls$`mean min.freq.centre.`- mean_sd_calls$`sd min.freq.centre.`,
       plot, mean_sd_calls$`mean min.freq.centre.`+ mean_sd_calls$`sd min.freq.centre.`, angle=90, code=3, length=0.05, col="blue")


#want to graph different features for specific parameters
#make a data frame for the features to keep
str(mean_sd_calls)
filtered_mean_calls <- select(mean_sd_calls, `mean label`,`mean peak.freq.centre.`, `mean min.freq.centre.`, 
                              `mean max.freq.centre.`,`mean peak.ampl.centre.`,`mean peaks.maxentire.` )

filtered_sd_calls <- select(mean_sd_calls,`sd peak.freq.centre.`,`sd min.freq.centre.`,`sd max.freq.centre.`, 
                            `sd peak.ampl.centre.`,`sd peaks.maxentire.`)

filtered_mean_calls2 <- select(mean_sd_calls, `mean min.freq.centre.`,`mean peak.freq.centre.` )

filtered_mean_calls3 <- select(mean_sd_calls, `mean peak.freq.centre.`, `mean min.freq.centre.`, 
                        `mean max.freq.centre.`)

barplot(t(filtered_mean_calls2), names = filtered_mean_calls$`mean label`,horiz = F, las = 2, 
        bty="n", cex.axis = 0.9, cex.names = 0.8, col=colors()[c(543,545)])
# Add legend to barplot
legend("topright",                                  
       legend = c("minimum frequency","peak frequency"),cex = 0.75,bty = "n",
       fill = colors()[c(543,545)])

plot <- barplot(t(filtered_mean_calls3), names = filtered_mean_calls$`mean label`,horiz = F, las = 2, 
        bty="n", cex.axis = 0.9, cex.names = 0.8, col=colors()[c(543,544,545)], beside=T, ylim=c(0,11000))
legend("topleft",                                  
       legend = c("peak frequency", "minimum frequency", "maximum frequency"), cex = 0.75, bty = "n",
       fill = colors()[c(543,544,545)])

#find frequency for each call type

call_sum <- as.data.frame(table(focal_all_calls$label))
colnames(call_sum) <- c("Call name", "Sum")
call_sum1 <- call_sum$Sum
plot <- barplot(call_sum1, names = call_sum$`Call name`,las = 2, 
                bty="n", cex.axis = 0.9, cex.names = 0.8, col=colors()[c(545)], beside=T)

#want to remove calls where there are less than 5 instances

#next steps is to add date and time for the calls_all_features and add the GPS points to the same data frame

#want to boxplot the mean peak frequency for each call type 


#make plot for frequency of each call type in call_all_features



call_table <- as.data.frame(table(calls_all_features$label))

colnames(call_table)[colnames(call_table) == "Var1"] <- "label"
call_table$label <- as.character(call_table$label)
call_table$Freq <- as.numeric(call_table$Freq)
barplot(call_table)

call_table <- call_table[call_table$label != "nf grunting" & call_table$label != "nf grunt" & call_table$label != "nf click grunt" 
                         & call_table$label != "nf barking" & call_table$label != "nf chirpgr" & call_table$label != "nf chitter"
                         & call_table$label != "nf squeal" & call_table$label != "nf chirp" & call_table$label != "nf clickgr" & call_table$label != "nf growl"
                         & call_table$label !="nf squeal grunt" & call_table$label != "squeal grunts" & call_table$label != "grunt breath" 
                         & call_table$label != "bark grunting" & call_table$label != "dumdum",]

barplot(call_table[ ,2], names.arg = call_table[ ,1], horiz = F, las = 2, 
        bty="n", cex.axis = 1, cex.names = 1, ylab = "Number of labelled calls", col = "maroon4")



