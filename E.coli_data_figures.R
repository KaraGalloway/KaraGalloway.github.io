library(ggplot2)
library(tidyverse)
library(modelr)
library(easystats)
library(broom)
library(fitdistrplus)

#data
#The E. coli data: 
colidat <- read_csv("E.Coli_count_summary.csv")
#Farm distance data
farmdist <- read_csv("Farms_near_sites.csv")

#Find average E. coli levels for each site
avg_ecoli <- colidat %>% 
  group_by(Site) %>% 
  summarise(avg_ecoli_count = mean(Level, na.rm = TRUE))
print(avg_ecoli)

#Find average farm distance for each site
avg_farmdist <- farmdist %>% 
  group_by(`Closest site`) %>% 
  summarise(avg_dist_to_farm = mean(`Distance (mi)`, na.rm = TRUE))
print(avg_farmdist)

ggplot(avg_farmdist, aes(y = avg_dist_to_farm, x = `Closest site`)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Plot them together...
colnames(avg_farmdist)[colnames(avg_farmdist) == "Closest site"] <- "Site"
merged <- merge(avg_ecoli, avg_farmdist, by = "Site")

ggplot(merged, aes(x = avg_dist_to_farm, y = avg_ecoli_count, label = Site)) +
  geom_point()

#Correlation??
cor.test(merged$avg_dist_to_farm, merged$avg_ecoli_count)
#Doesn't look like there is.... any... correlation
#very weak correlation... 0.07

#What about average precipitation??
#I don't know what the precipitation level is for each individual site
#But I do know precipitation by date, so let's do that!

#Add precipitation data
precipdat <- read_csv("Precip_data.csv")

#look at the data
glimpse(precipdat) #huh... not a lot of precipitation those days... 
#I totally remember a few storms those weeks! Guess not! 
#Well, let's still try it 

#Find average E. coli levels for each date 
avg_ecoli_date <- colidat %>% 
  group_by(Date) %>% 
  summarise(ecoli_date_avg = mean(Level, na.rm = TRUE))

#Plot against precipitation
precip_merge <- merge(avg_ecoli_date, precipdat)
ggplot(precip_merge, aes(x = ecoli_date_avg, y = Precip)) +
  geom_point()

#Correlation??
cor.test(precip_merge$ecoli_date_avg, precip_merge$Precip)
#Wow this one actually shows correlation!

#Conclusion
#Distance relative to farms doesn't seem to make a difference
#in e.coli levels at each site. However, precipitation does seem
#to make a difference. This makes sense because precipitation often
#leads to runoff into streams and rivers and drains that feed into
#Utah Lake. More runoff of farm material is likely to make more of
#a difference than sheer distance to those farms.