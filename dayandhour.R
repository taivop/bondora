library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(extrafont)
library(grid)


data = read.table("data/cleaned.csv") 

weekday_labels = c("E", "T", "K", "N", "R", "L", "P")

# Proportion funded by hour&day signed
data5 = data %>%
  group_by(ApplicationSignedWeekday, ApplicationSignedHour) %>%
  summarise(count_total=n(), count_funded=sum(WasFunded)) %>%
  mutate(perc_funded=count_funded/count_total) %>%
  mutate(`Kokku taotlusi`=count_total, `Rahastatud taotlusi`=count_funded,
         `Rahastatud taotluste osakaal`=perc_funded)


# Plotting
svg("figures/time_heatmap.svg",width=10,height=6)

bgcolor = "white"
ggplot(data5, aes(ApplicationSignedWeekday, ApplicationSignedHour)) +
  geom_tile(aes(fill = `Kokku taotlusi`), colour = bgcolor) +
  scale_fill_gradient(low = "white", high = "#fc4e2a") +
  scale_y_reverse(breaks=seq(0,23,3), expand=c(0.02, 0)) +
  scale_x_discrete(breaks=1:7, labels=weekday_labels, limits=c(1:7), expand=c(0.02, 0)) +
  ylab("Taotluse esitamise kellaaeg") +
  xlab("Taotluse esitamise nädalapäev") + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        text=element_text(size=16, family="Open Sans"),
        panel.background=element_rect(fill=bgcolor), legend.position="top",
        legend.key.width=unit(0.1, "npc"))

dev.off()


