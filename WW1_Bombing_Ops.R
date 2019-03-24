####Import libraries and data----
library(tidyverse)
library(lubridate)
library(Amelia) #use this to check for missing values with missmap()
library(leaflet)

df <- read_csv("Thor_WWI_Bombing_Operations.csv")


#Clean up column classification----
colnames(df) <- tolower(colnames(df))
  
df$msndate <- mdy(df$msndate)


####I noticed there are only 4 countries represented and thought this would be better as a factor
df %>% 
  group_by(country) %>% 
  summarize(n())

#There were 3 values NA.  Based on the take off base and target we can see that they were most likely Italian
country.missing <- df %>% 
  filter(is.na(country))

#df <- df[!is.na(df$country),] This can be used if you just want to eliminate the 3

country <- function(country){
              if(is.na(country)){
                return("ITALY")
              }else if(country == "RAF"){
                return("UK")
              }else{
                return(country)
             }
           }

df$country <- sapply(df$country, country)

df$country <- as.factor(df$country)


####I want to set take off time to either day or night and set this as a factor

#I noticed there is a French group called "Night Bombardment" so I reasonably can set this to Night after checking if #any of them are set to day to refute this hypothesis

df %>% filter(unit == "NIGHT BOMBARDMENT GRP")
df$takeofftime[grepl("NIGHT BOMBARDMENT GRP", df$unit)] <- "NIGHT"

#since I have charachters and date information in one vector I want to split the ones off that can become #data times to investigate these furhter 
df.takeoff <- df %>% 
  filter(takeofftime != "NIGHT" & takeofftime != "DAY")

df.takeoff <- df.takeoff[-(293:295),]
df.takeoff$takeofftime <- df.takeoff$takeofftime %>% 
                            mdy_hm() %>% 
                            hour()

df.takeoff %>% 
  group_by(takeofftime) %>% 
  summarize(n())

#After filtering the data I see that the latest take off times in the date time group were 1800 in summer #months.  Therefore I think it is reasonable to set all of the data that is not NA or Night to Day (Yes, I # # saw the two values marked as evening and to me this is still daylight).
dayOrNight <- function(time){
          if(is.na(time)){
            return(time)
          }else if(time == "DAY"){
            return(time)
          }else if(time == "NIGHT"){
            return(time)
            }else{
            return("DAY")
          }
}

df$takeofftime <- sapply(df$takeofftime, dayOrNight)

df %>% 
  group_by(takeofftime) %>% 
  summarize(n())

df$takeofftime <- as.factor(df$takeofftime)

#I am curious if country can tell us if an attack is more likely to prove day or night to clean up the NAs
##########            Still needs work ############
ggplot(df)+
  geom_point(aes(x = takeofftime, y = country))

takeoff.NA <- df %>% 
  filter(is.na(takeofftime))


####EDA----

#What was the most common platform for night attacks?

df %>%
  filter(takeofftime == "NIGHT") %>% 
  group_by(mds) %>% 
  summarize(n())

#Since airships were the most common platform, break these out by country
df %>% 
  filter(mds == "AIRSHIP") %>% 
  group_by(country) %>% 
  summarize(n())

#The airship bombs were all Italian attacks, who were the targets
df %>% 
  filter(mds == "AIRSHIP") %>% 
  group_by(tgtcountry) %>% 
  summarize(n())

airship <- df %>% filter(mds == "AIRSHIP")

airship_attack_map <- leaflet() %>% 
                      addTiles() %>% 
                        addCircleMarkers( data = airship,
                                          color = "black",
                                          radius = 2,
                                          popup = ~paste("Date: ", msndate, "<br/>",
                                                         "Attack Location: ", tgtlocation, "<br/>",
                                                         "Target: ", tgttype, "<br/>")
                                          )
airship_attack_map


#what was the total frequency of attacks by month over the course of the war?
a <- year(df$msndate)
b <- month(df$msndate)
df$year_month <- paste(a, b, sep = "-")
df$year_month <- ymd(df$year_month, truncated = 1)

df %>% 
  group_by(year_month, country) %>% 
  summarize(total = n()) %>% 
  ggplot()+
  geom_line(aes(x = year_month, y = total))+
  geom_point(aes(x = year_month, y = total))+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
df %>% 
  group_by(year_month) %>% 
  summarize(total = n()) %>% 
  arrange(desc(total))

#Lets see what this looks like by country
df %>% 
  group_by(year_month, country) %>% 
  summarize(total = n()) %>% 
  ggplot()+
  geom_line(aes(x = year_month, y = total, color = country, group = country))+
  geom_point(aes(x = year_month, y = total, color = country, group = country))+
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#where were the bombings of October 1918 concentrated?
us_oct_18 <- df %>% 
                filter(country == "USA" & year_month == "1918-10-01")

ggplot(us_oct_18)+
  geom_bar(aes(tgtcountry))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#what platforms did the US fly at the height of operations
us_oct_18 %>% 
  group_by(mds) %>% 
  summarize(total = n()) %>% 
  arrange(desc(total))
  
  
  
  
  
  
  
  
