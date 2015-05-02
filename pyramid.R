library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

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
  #scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10))) + 
  coord_flip()



# Kasutades välist package't
library(pyramid)

data_pyramid = data2 %>%
  dcast(Age ~ Gender) %>%
  select(Mees, Naine, Age) %>%
  mutate(Mees=ifelse(is.na(Mees),0,Mees), Naine=ifelse(is.na(Naine),0,Naine))

pyramid(data_pyramid,
        Llab="Mehed", Rlab="Naised", Clab="Vanus")

