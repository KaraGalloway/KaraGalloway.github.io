#FINALLLLLL

library(ggplot2)
library(tidyverse)
library(modelr)
library(easystats)
library(broom)
library(fitdistrplus)

#Okay let's add the data in first

#The E. coli data: 
colidat <- read_csv("E.Coli_count_summary.csv")
#Farm distance data
farmdist <- read_csv("Farms_near_sites.csv")

#Alrighty let's see what this data looks like 

names(colidat)
glimpse(colidat)
summary(colidat)#the min and max for Level makes perfect sense
                #because they are also the min and max for the 
                #Quanti-tray analysis test used on the samples 

na.omit(colidat)

#Let's turn the dates into dates using LUBRIDATE!! #a new thing I'm learning
#but before I do that I need to change the format of my dates
colidat$Date <- gsub("/","-", colidat$Date)
colidat$Date <- mdy(colidat$Date)

#Okay now it should work
colidat$Date = as_date(as.character(colidat$Date))

#Comparing each of the bacteria levels per site for each sample
ggplot(colidat, aes(x = Site, y = Level)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Comparing levels and dates
ggplot(colidat, aes(x = Date, y = Level)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Seems like some of the later dates have higher E. coli levels...
#I wonder if it has anything to do with precipitation bringing more
#solutes into the water?? Maybe I'll look into that along with cows...

#Can I plot date and site??
ggplot(colidat, aes(x = Date, y = Site)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#Turns out I can! Totally accurate because we sampled half the sites 
#every Tuesday and the other half every Thursday!

#Now let's do the farm data
names(farmdist)
glimpse(farmdist)
summary(farmdist) #distances from 1.04 mi to 8.77 mi

#comparing closest site to distance
ggplot(farmdist, aes(x = `Closest site`, y = `Distance (mi)`)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Some of the farms are repeated a couple times... 
#Comparing farms to distance from sites
ggplot(farmdist, aes(x = `Farm Name`, y = `Distance (mi)`)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Now let's compare in the next script!!