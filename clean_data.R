library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)



data = read.table("data/LoanData.csv", header=TRUE, sep=";", quote="") %>%
  mutate(
    LoanApplicationStartedDate = as.Date(LoanApplicationStartedDate, format="%d.%m.%Y"),
    LoanDate = as.Date(LoanDate, format="%d.%m.%Y"),
    ContractEndDate = as.Date(ContractEndDate, format="%d.%m.%Y"),
    FirstPaymentDate = as.Date(FirstPaymentDate, format="%d.%m.%Y"),
    MaturityDate_Original = as.Date(MaturityDate_Original, format="%d.%m.%Y"),
    MaturityDate_Last = as.Date(MaturityDate_Last, format="%d.%m.%Y"),
    GracePeriodStart = as.Date(GracePeriodStart, format="%d.%m.%Y"),
    GracePeriodEnd = as.Date(GracePeriodEnd, format="%d.%m.%Y"),
    InDebt1Day_StartDate = as.Date(InDebt1Day_StartDate, format="%d.%m.%Y"),
    InDebt7Day_StartDate = as.Date(InDebt7Day_StartDate, format="%d.%m.%Y"),
    InDebt14Day_StartDate = as.Date(InDebt14Day_StartDate, format="%d.%m.%Y"),
    InDebt21Day_StartDate = as.Date(InDebt21Day_StartDate, format="%d.%m.%Y"),
    InDebt30Day_StartDate = as.Date(InDebt30Day_StartDate, format="%d.%m.%Y"),
    InDebt60Day_StartDate = as.Date(InDebt60Day_StartDate, format="%d.%m.%Y"),
    DebtRestructuringDate = as.Date(DebtRestructuringDate, format="%d.%m.%Y"),
    Default_StartDate = as.Date(Default_StartDate, format="%d.%m.%Y"),
    ScoringDate = as.Date(ScoringDate, format="%d.%m.%Y"),
    ScoringDate_V0 = as.Date(ScoringDate_V0, format="%d.%m.%Y"),
    ScoringDate_V1 = as.Date(ScoringDate_V1, format="%d.%m.%Y"),
    Gender = mapvalues(factor(Gender), from = c(0,1,2), to = c("Mees", "Naine", "Määramata"))
    )
# Piira kuupäev kuidagi ära, nt 2014. aastal väljaantud laenud

write.table(data, file="data/cleaned.csv")
