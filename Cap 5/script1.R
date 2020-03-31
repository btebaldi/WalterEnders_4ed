# clear variables
rm(list = ls())

# load libraries
library(readxl)
library(ggplot2)
library(lubridate)

# read the database
db <- read_excel("Database/TERRORISM.XLS")

# Create a date column
db$Date = lubridate::ymd(db$Year*10000+db$Month*100 + 01)
db$Id = 1:nrow(db)

# Create the breaks
db.breaks = seq(from =1, to=164, by=12)

ggplot(db) +
  geom_line(aes(Id, Domestic)) + 
  scale_x_continuous(breaks=db.breaks, labels = db$Year[db.breaks]) +
  labs(
    title = "Panel (a): Domestic Incidents",
    subtitle = NULL,
    caption = "FIGURE 5.1 Domestic and Transnational Terrorism",
    x = NULL,
    y = "Incidents per quarter"
  )


ggplot(db) +
  geom_line(aes(Id, Transnational)) + 
  scale_x_continuous(breaks=db.breaks, labels = db$Year[db.breaks]) +
  labs(
    title = "Panel (b): Transnational Incidents",
    subtitle = NULL,
    caption = "FIGURE 5.1 Domestic and Transnational Terrorism",
    x = NULL,
    y = "Incidents per quarter"
  )
