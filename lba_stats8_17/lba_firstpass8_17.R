library(ggplot2)
library(GGally)
library(lattice)
library(ez)
library(doBy)
library(psych)
library(nlme)
library(lsr)
library(plyr)

rm(list=ls()); par(mfrow = c(1,1))
setwd("~/Rstudio-server/lba_stats8_17")


my.axis.font<-theme(axis.title.x = element_text(size=18), axis.title.y = element_text(size=18),
                    axis.text.x = element_text(size=18), axis.text.y = element_text(size=18), 
                    plot.title=element_text(size=18,face="bold"),
                    legend.title=element_text(size=14),legend.text=element_text(size=14))

'%ni%' <- Negate('%in%')

# Get CSV -----------------------------------------------------------------

fulldata<-read.csv("output.csv", header=FALSE)

gsub(fulldata$rt[\d], "\\1", x)

colnames(fulldata) <- c("subjectid", "type_time","response_mode","load_type",
                        "rt", "r1","r2","s_type","acc")
#hist(as.numeric(fulldata$rt))
summary(fulldata)

summaryBy(rt~acc + response_mode + load_type,data=fulldata, FUN=c(mean,median,var), na.rm=TRUE)
summaryBy(acc~subjectid + response_mode, data = fulldata, FUN=c(mean,median,var), na.rm=TRUE)

length(levels(factor(fulldata$subjectid)))

fulldata$rt <- as.integer(fulldata$rt)

fulldata <- subset(fulldata, acc == 'correct' | acc == 'wrong')
fulldata <- subset(fulldata, rt > 200 & rt < 5000)

ddply(fulldata, .(subjectid), summarize, Numrt = length(unique(rt)))


# Plot data ---------------------------------------------------------------

qplot(response_mode, rt, data=fulldata, geom="boxplot", color = load_type, ylim = c(0,1500)) #Look at RT's by condition

levels(fulldata$acc)<- droplevels(fulldata$acc)


clean.mean <- aggregate(list(fulldata$rt,as.numeric(as.character(fulldata$acc))),
                        by = list(fulldata$load_type,fulldata$response_mode, fulldata$subjectid),
                        FUN = 'mean')

colnames(clean.mean) <- c("load_type","response_mode","subid","rt","acc")

clean.mean$acc <- (clean.mean$acc*100)

qplot(response_mode, acc, data=fulldata, geom="boxplot", color = load_type, ylim = c(0,5000)) #Look at RT's by condition


# histograms --------------------------------------------------------------

ggplot(data = fulldata, mapping = aes(rt,color=response_mode))+
  geom_histogram(binwidth = 10)+ xlim(c(0,5000))

