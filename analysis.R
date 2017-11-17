# Set wd
setwd("~/Downloads/ufo project")

# Call in libraries
library(tidyverse)
library(lubridate)

# Read in data
dat <- read.csv("ufo_data.csv", header = FALSE, stringsAsFactors = FALSE)  # colnames don't exist
names(dat) <- c("Date", "City", "State", "Country", 
                "Shape", "Unkown", "Duration", "Report", "Reported", 
                "Latitude", "Longitude")
dat$Latitude <- as.numeric(dat$Latitude)

# Separate date
days <- c()
months <- c()
years <- c()
hours <- c()
minutes <- c()
for(d in 1:nrow(dat)) {
  print(d)
  date.cleaned <- parse_date_time(dat[d,1], orders = "mdy HM")
  days[d] <- day(date.cleaned)
  months[d] <- month(date.cleaned)
  years[d] <- year(date.cleaned)
  hours[d] <- hour(date.cleaned)
  minutes[d] <- minute(date.cleaned)
}
dat$Day <- days
dat$Month <- months
dat$Year <- years
dat$Hour <- hours
dat$Minute <- minutes
dat$AbsoluteDayTime <- dat$Hour*60 + dat$Minute

# visualize
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

world <- map_data("world")
ggplot() + 
  geom_polygon(data = world, aes(x=long, y = lat, group = group), fill = NA, color = "black") + 
  coord_fixed(1.3) +  geom_point(data = dat, aes(x = Longitude, y = Latitude), color = "red", size = 0.1)

# reports
library(XML)
# Convenience function to convert html codes
html2txt <- function(str) {
  xpathApply(htmlParse(str, asText=TRUE),
             "//body//text()", 
             xmlValue)[[1]]
}

dat = dat[!dat$Report == "",]  # 15 empty reports

for(r in 1:length(dat$Report)) {
  print(r)
  unclean.report <- html2txt(dat$Report[r])
  cleaner.report <- tolower(unclean.report)
  dat$Report[r] <- gsub('[[:punct:]]', '', cleaner.report)
}

dat <- read.csv("new_ufo_dat.csv")
total <- paste(dat$Report, collapse = " ")

reports <- html2txt(dat$Report)
reports <- tolower(reports)
reports <- gsub('[[:punct:]]', '', reports)  # remove punctuation
reports <- gsub('\n', ' ', reports)  # 
