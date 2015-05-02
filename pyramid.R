library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)

data = read.table("data/cleaned.csv") 


data2 = data %>%
  group_by(Age, Gender) %>%
  summarise(count=n()) %>%
  filter(Age >= 18 & !is.na(Gender) & Gender != 2)


ggplot(data2, aes(x=Age, y=count, fill=factor(Gender))) +
  geom_bar(stat="identity")

# Rahvastikupüramiid
ggplot(data=data2,aes(x=as.factor(Age),fill=factor(Gender))) + 
  geom_bar(subset=.(Gender=="Naine"),aes(y=count),stat="identity") + 
  geom_bar(subset=.(Gender=="Mees"),aes(y=count*(-1)),stat="identity") + 
  coord_flip()


# Katse 2
breaks=c(18, seq(20,100,by=3))
data2f = data2 %>% filter(Gender == "Naine")
data3f = transform(data2f, Age_group=cut(as.numeric(sub('[%]', '', Age)), 
                                           breaks=breaks, include.lowest=TRUE))
data2m = data2 %>% filter(Gender == "Mees")
data3m = transform(data2m, Age_group=cut(as.numeric(sub('[%]', '', Age)), 
                                           breaks=breaks, include.lowest=TRUE))
data3 = rbind(data3f, data3m) %>%
  rename(Sugu=Gender)



ggplot(data=data3,aes(x=as.factor(Age_group),y=count, fill=Sugu)) + 
  geom_bar(subset=.(Sugu=="Naine"),aes(y=count),stat="identity") + 
  geom_bar(subset=.(Sugu=="Mees"),aes(y=count*(-1)),stat="identity") + 
  scale_y_continuous(breaks = seq(-2000, 2000, 500),
                     labels=as.character(c(seq(2000,0,-500), seq(500,2000,500)))) + 
  coord_flip() +
  scale_fill_manual(values = c("#377EB8", "#E41A1C")) +
  ylab("Taotluste arv") +
  xlab("Vanusegrupp") + 
  theme_bw() +
  theme(panel.grid.minor = element_blank())














