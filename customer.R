library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(extrafont)


data = read.table("data/cleaned.csv") 

data2 = data %>%
  mutate(funded_perc=FundedAmount/AppliedAmount) %>%
  group_by(UserName) %>%
  summarise(mean_funding=mean(funded_perc))






