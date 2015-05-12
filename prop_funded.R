library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(extrafont)


data = read.table("data/cleaned.csv") 

# What % of the application was funded - COUNTRY
data3 = data %>%
  mutate(FundedAmount = ifelse(is.na(FundedAmount), 0, FundedAmount)) %>%
  mutate(funded_perc=FundedAmount/AppliedAmount) %>%
  mutate(Country=mapvalues(Country, from=c("EE", "ES", "FI", "SK"), to=
                              c("Eesti",
                                "Hispaania",
                                "Soome",
                                "Slovakkia"))) %>%
  group_by(credit_score, Country) %>%
  summarise(mean_funding=mean(funded_perc), count=n()) %>%
  na.omit() %>%
  filter(credit_score > 0)

# Plot
svg("figures/prop_funded_country.svg",width=10,height=6)
ggplot(data3) +
  geom_bar(aes(x=credit_score, y=mean_funding, fill=Country), stat="identity") +
  scale_x_continuous(breaks=seq(500,1000,100)) +
  xlab("Krediidiskoor") +
  ylab("Rahuldatud taotluste osakaal") + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text=element_text(size=16, family="Open Sans"), legend.position="none") +
  facet_wrap(~ Country) +
  scale_fill_manual(values=c("#225db9", "#d7c30c", "#aaaaaa", "#a60e1d"))
dev.off()






# What % of the application was funded - LANGUAGE
data4 = data %>%
  mutate(FundedAmount = ifelse(is.na(FundedAmount), 0, FundedAmount)) %>%
  mutate(funded_perc=FundedAmount/AppliedAmount) %>%
  mutate(Language=mapvalues(language_code, from=1:7, to=
                                      c("eesti",
                                        "inglise",
                                        "vene",
                                        "soome",
                                        "saksa",
                                        "hispaania",
                                        "slovakkia"))) %>%
  filter(language_code <= 7 & !(language_code %in% c(5,7))) %>% # saksa ja slovakkia keeles vähe värki
  group_by(credit_score, Language) %>%
  summarise(mean_funding=mean(funded_perc), count=n()) %>%
  na.omit() %>%
  filter(credit_score > 0)

# Plot
svg("figures/prop_funded_language.svg",width=10,height=6)
ggplot(data4) +
  geom_bar(aes(x=credit_score, y=mean_funding, fill=Language), stat="identity") +
  scale_x_continuous(breaks=seq(500,1000,100)) +
  xlab("Krediidiskoor") +
  ylab("Rahuldatud taotluste osakaal") + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text=element_text(size=16, family="Open Sans"), legend.position="none") +
  facet_wrap(~ Language) +
  scale_fill_brewer(palette="Set1")
dev.off()

