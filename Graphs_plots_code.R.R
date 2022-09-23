library(psych)
library(dplyr)
library(tidyverse)
library(ggplot2)

economic_indicators_data <- read.csv("Key Economic Indicators.csv")
str(economic_indicators_data)
corrdata <- cor(economic_indicators_data)
View(corrdata)
write.csv(corrdata, "corrKey_economic_indicators.csv")
adtrendsdata <- read.csv("ad spending.csv")
View(adtrendsdata)
colnames(adtrendsdata)
adtrendstable <- table(adtrendsdata)
View(adtrendstable)
adtrendslongtable <- adtrendsdata %>% 
  pivot_longer(c('X2017', 'X2018', 'X2019', 'X2020', 'X2021', 'X2022', 'X2023',
                 'X2024', 'X2025','X2026', 'X2027'), names_to = "year", 
               values_to = "ad_spends")
View(adtrendslongtable)
colnames(adtrendslongtable)
adtrendsdata1 <- data.frame(adtrendslongtable)
View(adtrendsdata1)
class(adtrendsdata1)
str(adtrendsdata1)
adtrendsdata1$Year <- c('2017','2018', '2019', '2020', '2021', '2022', '2023', '2024', '2025', '2026', '2027')
adtrendsdata1$Ad.spending.per.internet.user.in.GBP <- as.factor(adtrendsdata1$Ad.spending.per.internet.user.in.GBP)
adtrendsdata1$year <- as.factor(adtrendsdata1$year)
adtrendsdata1$Year <- as.factor(adtrendsdata1$Year)
ggplot(data = adtrendsdata1, mapping = aes(x = Year, y = ad_spends, color = Ad.spending.per.internet.user.in.GBP)) + 
  geom_point() + geom_smooth() + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
                                       plot.title = element_text(size = 15),
                                       plot.subtitle = element_text(size = 13),
                                       axis.text.y = element_text(size = 11)) +
  labs(x = "Year", y = " Ad spend per internet user", title = "Advertisement Spending Per Internet User in GBP")

###-----------------channels KPI--------------------------###
channels_KPI <- read.csv("channels KPI.csv")
str(channels_KPI)
ggplot(data = channels_KPI) +
  geom_bar(mapping = aes(x = fct_reorder(Channel, -Average.Forecasted.overall.reach.for.the.next.5.years.in.million.),
      y = Average.Forecasted.overall.reach.for.the.next.5.years.in.million.), 
           stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size = 11), 
                                      axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
                                      plot.title = element_text(size = 15),
                                      plot.subtitle = element_text(size = 13),
                                      axis.text.y = element_text(size = 11)) +
  labs(x = "Channel names", y = "Average Reach for the next 5 years", title = "Reach of different advertising channels", subtitle = "In millions") 



colnames(channels_KPI)

channels_KPI_clean <- drop_na(channels_KPI)

ggplot(data = channels_KPI_clean) +
  geom_bar(mapping = aes(x = fct_reorder(Channel, -Click.through.rates.of.the.channel.in..),
                         y = Click.through.rates.of.the.channel.in..), 
           stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size = 11), 
                                      axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
                                      plot.title = element_text(size = 15),
                                      plot.subtitle = element_text(size = 13),
                                      axis.text.y = element_text(size = 11)) +
  labs(x = "Channel names", y = "Click Through Rates", title = "Click Through Rates of advertising channels", subtitle = "In %") 

ggplot(data = channels_KPI_clean) +
  geom_bar(mapping = aes(x = fct_reorder(Channel, -Cost.per.click.of.ads.in.the.channel),
                         y = Cost.per.click.of.ads.in.the.channel), 
           stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size = 11), 
                                      axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
                                      plot.title = element_text(size = 15),
                                      plot.subtitle = element_text(size = 13),
                                      axis.text.y = element_text(size = 11)) +
  labs(x = "Channel names", y = "Cost per click of ads", title = "Cost per click of ads", subtitle = "In GBP") 
ggplot(data = channels_KPI_clean) +
  geom_bar(mapping = aes(x = fct_reorder(Channel, -Conversion.rates.of.the.ads.in.the.channel),
                         y = Conversion.rates.of.the.ads.in.the.channel), 
           stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size = 11), 
                                      axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
                                      plot.title = element_text(size = 15),
                                      plot.subtitle = element_text(size = 13),
                                      axis.text.y = element_text(size = 11)) +
  labs(x = "Channel names", y = "Conversion rates of the ads in the channels", title = "Conversion rates of the advertisements in the channels", subtitle = "In percentage") 


ggplot(data = channels_KPI_clean) +
  geom_bar(mapping = aes(x = fct_reorder(Channel, -Cost.per.conversion),
                         y = Cost.per.conversion), 
           stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size = 11), 
                                      axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
                                      plot.title = element_text(size = 15),
                                      plot.subtitle = element_text(size = 13),
                                      axis.text.y = element_text(size = 11)) +
  labs(x = "Channel names", y = "Cost per action", title = "Cost per action in different channels", subtitle = "In GBP") 




ggplot(data = channels_KPI_clean) +
  geom_bar(mapping = aes(x = fct_reorder(Channel, -Max.Volume.of.expected.conversion.in.million.),
                         y = Max.Volume.of.expected.conversion.in.million.), 
           stat = "identity") + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size = 11), 
                                      axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
                                      plot.title = element_text(size = 15),
                                      plot.subtitle = element_text(size = 13),
                                      axis.text.y = element_text(size = 11)) +
  labs(x = "Channel names", y = "Max.Volume.of.expected.conversion", title = "Maximum Volume of conversion", subtitle = "In Million") 


















