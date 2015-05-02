library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(extrafont)


data = read.table("data/cleaned.csv") 

weekday_labels = c("E", "T", "K", "N", "R", "L", "P")

# Proportion funded by hour&day signed
data5 = data %>%
  group_by(ApplicationSignedWeekday, ApplicationSignedHour) %>%
  summarise(count_total=n(), count_funded=sum(WasFunded)) %>%
  mutate(perc_funded=count_funded/count_total) %>%
  select(ApplicationSignedWeekday, ApplicationSignedHour, count_funded) %>%
  dcast(ApplicationSignedHour~ApplicationSignedWeekday) %>%
  select(-ApplicationSignedHour)

names(data5$ApplicationSignedWeekday) = weekday_labels

ggplot(data5, aes(ApplicationSignedWeekday, ApplicationSignedHour)) +
  geom_tile(aes(fill = count_funded), colour = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  scale_y_reverse() +
  #scale_x_discrete(breaks=1:7, labels=weekday_labels) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        text=element_text(size=16, family="Open Sans"))




