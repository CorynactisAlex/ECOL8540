# Author: Alex Lee (aklee@uga.edu)
# Date:   May 15-17, 2017
# Title:  Visualization exercises

## Set Workspace
setwd("~/R/Classes/ECOL 8540 Intro/ECOL8540")

## Load Packages
library(ggplot2)
library(plotly)
library(lubridate)

## Load Data
mers <- read.csv('cases.csv')

mers$hospitalized[890] <- c('2015-02-20')
mers <- mers[-471,]

mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized) 
class(mers$onset2)

day0 <- min(na.omit(mers$onset2))

mers$epi.day <- as.numeric(mers$onset2 - day0)

# Making a Plot
ggplot(data = mers) +
  geom_bar(mapping = aes(x = epi.day)) +
  labs(x     = 'Epidemic day', 
       y     = 'Case count', 
       title = 'Global count of MERS cases by date of symptom onset', caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# fill = country
ggplot(data = mers) +
  geom_bar(mapping = aes(x    = epi.day,
                         fill = country)) +
  labs(x     = 'Epidemic day', 
       y     = 'Case count', 
       title = 'Global count of MERS cases by date of symptom onset', caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# 
ggplot(data = mers, position = fill) +
  geom_bar(mapping = aes(x    = epi.day,
                         fill = country)) +
  labs(x     = 'Epidemic day', 
       y     = 'Case count', 
       title = 'Global count of MERS cases by date of symptom onset', caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# coord_flip and coord_polar
ggplot(data = mers, position = fill) +
  geom_bar(mapping = aes(x    = epi.day,
                         fill = country)) +
  labs(x     = 'Epidemic day', 
       y     = 'Case count', 
       title = 'Global count of MERS cases by date of symptom onset', caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") +
  coord_flip() +
  coord_polar()

## UNIVARIATE PLOTS ##
mers$infectious.period <- mers$hospitalized2 - mers$onset2
class(mers$infectious.period)
mers$infectious.period <- as.numeric(mers$infectious.period,
                                     units = "days")
ggplot(data = mers) +
  geom_histogram(aes(x = infectious.period)) +
  labs(x = 'Infectious period', 
       y = 'Frequency', 
       title = 'Distribution of calculated MERS infectious period',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.")

# Positive infectious periods only
mers$infectious.period2 <- ifelse(mers$infectious.period < 0,
                                  0,
                                  mers$infectious.period)
ggplot(data = mers) +
  geom_histogram(aes(x = infectious.period2)) +
  labs(x = 'Infectious period', 
       y = 'Frequency', 
       title = 'Distribution of calculated MERS infectious period (positive values only)',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.")

# Density Plot
ggplot(data = mers) +
  geom_density(mapping = aes(x = infectious.period2)) +
  labs(x = 'Infectious period', 
       y = 'Frequency', 
       title = 'Probability density for MERS infectious period (positive values only)',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.")

# Area Plot
ggplot(data = mers) +
  geom_area(stat = 'bin',
            mapping = aes(x = infectious.period2)) +
  labs(x = 'Infectious period', 
       y = 'Frequency', 
       title = 'Area plot for MERS infectious period (positive values only)',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.")

# Dot Plot
ggplot(data = mers) +
  geom_dotplot(mapping = aes(x = infectious.period2)) +
  labs(x = 'Infectious period', 
       y = 'Frequency', 
       title = 'Area plot for MERS infectious period (positive values only)',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.")

# Bar Plot
ggplot(data = mers) +
  geom_bar(mapping = aes(x = infectious.period2)) +
  labs(x = 'Infectious period', 
       y = 'Frequency', 
       title = 'Area plot for MERS infectious period (positive values only)',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.")

## BIVARIATE PLOTS ##

# Infectious period over the course of the MERS epidemic
ggplot(data = mers) +
  geom_line(mapping = aes(y = infectious.period2, 
                          x = onset2))  +
  labs(x = 'Onset',
       y = 'Infectious Period', 
       title = 'Infectious Period During MERS Epidemic (positive values only)',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.")

# Societal Learning
ggplot(data = mers) +
  geom_line(mapping = aes(y = infectious.period2, 
                          x = onset2))  +
  geom_smooth(mapping = aes(y = infectious.period2, 
                            x = onset2),
              method = "loess")  +
  labs(x = 'Onset', 
       y = 'Infectious Period', 
       title = 'Infectious Period During MERS Epidemic (positive values only)',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.")

# By Country
ggplot(data = mers) +
  geom_line(mapping = aes(y = infectious.period2, 
                          x = onset2))  +
  geom_smooth(mapping = aes(y = infectious.period2, 
                            x = onset2,
                            fill = country),
              method = "loess")  +
  labs(x = 'Onset', 
       y = 'Infectious Period', 
       title = 'Infectious Period During MERS Epidemic (positive values only)',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.")

## FACETING ##
# By Country
ggplot(data = mers, 
       mapping = aes(x=epi.day, y=infectious.period2)) + 
  geom_point(mapping = aes(color=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', 
       y = 'Infectious period',
       title='MERS infectious period by country', 
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.")

# By Country & Gender
ggplot(data = subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea', 'UAE' )),
       mapping = aes(x=epi.day, y=infectious.period2 )) +
  geom_point(mapping = aes(color=country)) +
  facet_grid(gender ~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = 'Epidemic day', 
       y = 'Infectious period',
       title = 'MERS infectious period by gender and country', 
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.")

# Case Fatality Rate
ggplot(data = subset(mers, death %in% TRUE & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea', 'UAE' )),
       mapping = aes(x = epi.day, y = infectious.period2 )) +
  geom_point(mapping = aes(color=country)) +
  facet_grid(~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = 'Epidemic day', 
       y = 'Infectious period',
       title = 'MERS infectious period by gender and country', 
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.")

# More
epi.curve <- ggplot(data=mers) +
  geom_bar(mapping = aes(x = epi.day)) +
  labs(x = 'Epidemic day', 
       y = 'Case count', 
       title = 'Global count of MERS cases by date of symptom onset',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplotly(epi.curve)
