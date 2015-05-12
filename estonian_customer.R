library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(extrafont)

### estonian_customer.R: do analyses on Estonian customers

data = read.table("data/cleaned.csv") %>%
  filter(Country=="EE")


# Mean salary by occupation
data5 = data %>%
  filter(occupation_area %in% 1:19) %>%
  group_by(occupation_area) %>%
  
  summarise(mean_salary=mean(income_from_principal_employer),
            count=n()) %>%
  arrange(desc(mean_salary)) %>%
  mutate(Tegevusala = mapvalues(occupation_area, from=1:19, to=c(
    "Muu", "Mäetööstus", "Töötlev tööstus", "Energeetika", "Kommunaalmajandus",
    "Ehitus", "Jae- või hulgimüük", "Transport ja laotöö", "Haiglatöö ja toitlustus",
    "Infotehnoloogia ja telekommunikatsioon", "Finants- ja kindlustussektor",
    "Kinnisvara", "Teadustegevus", "Avalik haldus", "Avalik teenistus ja sõjavägi",
    "Haridus", "Tervishoid ja sotsiaalhoolekanne", "Kunst ja meelelahutus",
    "Põllumajandus, metsandus ja kalandus"))) %>%
  mutate(`Keskmine netopalk`=round(mean_salary), Inimesi=count) %>%
  select(Tegevusala, `Keskmine netopalk`, Inimesi)

# Table of mean salaries
library(xtable)
xt = xtable(data5)
digits(xt) = 0
print(xt, type = "html", file="figures/salarytable.html", include.rownames=FALSE)


# Salary distributions by occupation
data6 = data %>%
  filter(occupation_area %in% 1:19) %>%
  mutate(Tegevusala = mapvalues(occupation_area, from=1:19, to=c(
    "Muu", "Mäetööstus", "Töötlev tööstus", "Energeetika", "Kommunaalmajandus",
    "Ehitus", "Jae- või hulgimüük", "Transport ja laotöö", "Haiglatöö ja toitlustus",
    "Infotehnoloogia ja telekommunikatsioon", "Finants- ja kindlustussektor",
    "Kinnisvara", "Teadustegevus", "Avalik haldus", "Avalik teenistus ja sõjavägi",
    "Haridus", "Tervishoid ja sotsiaalhoolekanne", "Kunst ja meelelahutus",
    "Põllumajandus, metsandus ja kalandus"))) %>%
  rename(Palk=income_from_principal_employer) %>%
  filter(Palk < 4000 & Palk > 0) %>%
  filter(Tegevusala != "Mäetööstus") %>% # liiga vähe andmeid
  select(Tegevusala, Palk)

svg("figures/salarydist_by_occupation.svg",width=12,height=12)
ggplot(data6) +
  geom_bar(aes(x=Palk), fill="#807dba") +
  facet_wrap(~ Tegevusala, scales="free_y", ncol=3) +
  ylab("Inimeste arv") +
  xlab("Netopalk, €") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text=element_text(size=16, family="Open Sans"), legend.position="none",
        axis.title.x=element_text(vjust=-0.5))
dev.off()

# Salary distribution of all Bondora lenders from Estonia

svg("figures/salarydist.svg",width=10,height=6)
ggplot(data6) +
  geom_bar(aes(x=Palk),fill="#756bb1") +
  geom_vline(xintercept = mean(data6$Palk), linetype="longdash") +
  geom_vline(xintercept = median(data6$Palk)) +
  ylab("Inimeste arv") +
  xlab("Netopalk, €") + 
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text=element_text(size=16, family="Open Sans"), legend.position="none")
dev.off()

# Mean and median
mean(data6$Palk)
median(data6$Palk)
  

# How Estonian people write their counties
maakonnad = data %>%
  filter(Country=="EE") %>%
  group_by(County) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

maakonnad2 = maakonnad %>%
  arrange(County)

  