library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(extrafont)


data = read.table("data/cleaned.csv") 

# What % of the application was funded
data2 = data %>%
  mutate(FundedAmount = ifelse(is.na(FundedAmount), 0, FundedAmount)) %>%
  mutate(funded_perc=FundedAmount/AppliedAmount) %>%
  mutate(VerificationType=mapvalues(VerificationType, from=1:4, to=
                                  c("Sissetulekut ei kontrollitud",
                                    "Sissetulekut ei kontrollitud, kinnitati telefoni teel",
                                    "Sissetulekut kontrolliti",
                                    "Sissetulekut ja kulusid kontrolliti"))) %>%
  group_by(credit_score, VerificationType) %>%
  summarise(mean_funding=mean(funded_perc), count=n()) %>%
  na.omit() %>%
  filter(credit_score > 0)

# Plot
ggplot(data2) +
  geom_bar(aes(x=credit_score, y=mean_funding, fill=VerificationType), stat="identity") +
  xlab("Krediidiskoor") +
  ylab("Rahuldatud taotluste osakaal") + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        text=element_text(size=16, family="Open Sans"), legend.position="none") +
  facet_wrap(~ VerificationType)



