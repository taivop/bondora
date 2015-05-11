library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(extrafont)

# Import fonts
#font_import(pattern="[O/o]pen")
#loadfonts(device="win")

data = read.table("data/cleaned.csv") %>%
  filter(Country=="EE")
  

data2 = data %>%
  group_by(Age, Gender) %>%
  summarise(count=n()) %>%
  filter(Age %in% 18:71 & !is.na(Gender) & Gender != 2)

data2_funded = data %>%
  filter(WasFunded == 1) %>%
  group_by(Age, Gender) %>%
  summarise(count=n()) %>%
  filter(Age %in% 18:71 & !is.na(Gender) & Gender != 2)

# Distribute data into buckets
breaks=c(18, seq(20,100,by=3))
b = breaks
b[1] = 17 # quick fix
labels = paste(b[1:length(b)-1]+1,
                 b[2:length(b)],
                 sep = '-')
  
data2f = data2 %>% filter(Gender == "Naine")
data3f = transform(data2f, Age_group=cut(as.numeric(sub('[%]', '', Age)), labels=labels,
                                           breaks=breaks, include.lowest=TRUE))
data2m = data2 %>% filter(Gender == "Mees")
data3m = transform(data2m, Age_group=cut(as.numeric(sub('[%]', '', Age)), labels=labels, 
                                           breaks=breaks, include.lowest=TRUE))
data2f_funded = data2_funded %>% filter(Gender == "Naine")
data3f_funded = transform(data2f_funded, Age_group=cut(as.numeric(sub('[%]', '', Age)), labels=labels,
                                         breaks=breaks, include.lowest=TRUE))
data2m_funded = data2_funded %>% filter(Gender == "Mees")
data3m_funded = transform(data2m_funded, Age_group=cut(as.numeric(sub('[%]', '', Age)), labels=labels, 
                                         breaks=breaks, include.lowest=TRUE))

data3 = rbind(data3f, data3m) %>%
  rename(Sugu=Gender)


# Plotting
png("figures/pyramid.png", width=600, height=600, res=80)

ggplot(data=data3,aes(x=as.factor(Age_group),y=count, fill=Sugu)) + 
  geom_bar(subset=.(Sugu=="Naine"),aes(y=count,fill="N (ei rahastatud)"),stat="identity") + 
  geom_bar(data=data3f_funded,aes(y=count,fill="N (rahastati)"),stat="identity") + 
  geom_bar(subset=.(Sugu=="Mees"),aes(y=count*(-1),fill="M (ei rahastatud)"),stat="identity") + 
  geom_bar(data=data3m_funded,aes(y=count*(-1),fill="M (rahastati)"),stat="identity") + 
  scale_y_continuous(breaks = seq(-2000, 2000, 500),
                     labels=as.character(c(seq(2000,0,-500), seq(500,2000,500)))) + 
  coord_flip() +
  scale_fill_manual(values = c("#abd9e9", "#2c7bb6", "#fdae61", "#d7191c")) +
  ylab("Taotluste arv") +
  xlab("Vanusegrupp") + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
        text=element_text(size=16, family="Open Sans"), legend.position="top",
        axis.title.y=element_text(vjust=1))

dev.off()










