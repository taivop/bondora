library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(extrafont)


data = read.table("data/cleaned.csv") %>%
  filter(Country=="EE")

# Küsitud raha
data2 = data %>%
  filter(AppliedAmount < 12000) # Kolm outlierit: 40k, 40k, 20k

ggplot(data2) +
  geom_bar(aes(x=AppliedAmount))


# Inimese võlad selle järgi, kuidas ta plaanib laenu kasutada
data3 = data %>%
  mutate(TotalNumDebts=as.numeric(levels(TotalNumDebts)[TotalNumDebts])) %>%
  filter(TotalNumDebts < 15, UseOfLoan %in% 0:8) %>%
  mutate(UseOfLoanString = mapvalues(UseOfLoan, from=0:8, to=c(
    "Laenude konsolideerimine",
    "Kinnisvara",
    "Koduremont",
    "Ettevõtlus",
    "Haridus",
    "Reisimine",
    "Sõiduk",
    "Muu",
    "Tervishoid")))

ggplot(data3) +
  geom_bar(aes(x=TotalNumDebts)) +
  facet_wrap(~ UseOfLoanString, scales="free")


# Inimese haridustase vs kuidas plaanib laenu kasutada
data4 = data %>%
  filter(UseOfLoan %in% 0:8) %>%
  mutate(UseOfLoanString = mapvalues(UseOfLoan, from=0:8, to=c(
    "Laenude konsolideerimine",
    "Kinnisvara",
    "Koduremont",
    "Ettevõtlus",
    "Haridus",
    "Reisimine",
    "Sõiduk",
    "Muu",
    "Tervishoid")))

ggplot(data4) +
  geom_bar(aes(x=education_id)) +
  facet_wrap(~ UseOfLoanString, scales="free")


# Keskmine palk töövaldkonna järgi
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
  rename(`Keskmine palk`=mean_salary, N=count) %>%
  select(Tegevusala, `Keskmine palk`, N)

data5 # TEHA TABEL


# Palkade jaotus tegevusala järgi
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
        text=element_text(size=16, family="Open Sans"), legend.position="none")
dev.off()

# Palkade üldine jaotus (Bondorast laenajate hulgas)

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

# Keskmine ja mediaan
mean(data6$Palk)
median(data6$Palk)
  

# Kuidas Eesti inimesed oma maakonda kirjutavad
maakonnad = data %>%
  group_by(County) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
# VILNIUSE maakond :D

  