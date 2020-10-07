#My Analysis of the Dataset
Colonialism <- read.csv("~/Data Work/Colonialism.csv")
attach(Colonialism)
library(tidyr)
library(lm.beta)
library(ggplot2)
library(leaps)
library(broom)

#Basic Analysis of time as a colony and GDP per capita
TimeColonised <- lm(log(GDP_Per_Capita) ~ YearsCol)
TimeColonisedSt <- lm.beta(TimeColonised)
summary(TimeColonisedSt)

#First Multiple Regression 
MutipleMod1 <- lm(log(GDP_Per_Capita) ~ TotalLit + NonCorruption)
MultipleMod1St <- lm.beta(MutipleMod1)
summary(MultipleMod1St)

#Large Test
MultipleMod2 <- lm(log(GDP_Per_Capita) ~  YearsCol + Unemployment + Intensity + Unemployment^2 + MotherInvestment)
MultipleMod2St <- lm.beta(MultipleMod2)
summary(MultipleMod2St)

#Larger
MultipleMod3 <- lm(log(GDP_Per_Capita) ~ YearsCol + Intensity + MotherInvestment + Conquered. + FirstPolity + TotalLit)
MultipleMod3St <- lm.beta(MultipleMod3)
summary(MultipleMod3St)

#TransformationRegression 
MultipleMod4 <- lm(log(GDP_Per_Capita) ~ YearsCol + Intensity + MotherInvestment + Conquered. + FirstPolity + EconomicChange + SocialChange + PoliticalChange)
MultipleMod4St <- lm.beta(MultipleMod4)
summary(MultipleMod4St)

#TransformationRegression2
MultipleMod5 <- lm(log(GDP_Per_Capita) ~ YearsCol + Intensity + MotherInvestment + Conquered. + FirstPolity + EconomicChange + SocialChange + PoliticalChange)
MultipleMod5St <- lm.beta(MultipleMod5)
summary(MultipleMod5St)

#ChangeRegressionSimple
ChangeModel <- lm(Change~YearsCol)
summary(ChangeModel)

#ChangeRegressionMultiple
ChangeModel2 <- lm(log(GDP_Per_Capita) ~ GDP.EstI  +Intensity + TotalLit + NonCorruption)
ChangeModel2St <- lm.beta(ChangeModel2)
summary

#No NA Dataframe
ColonialismNoNa <- na.omit((Colonialism[, c(2:30)]))
attach(ColonialismNoNa)

#Leaps
Al <- regsubsets(x = ColonialismNoNa[, c(4:27)], y = log(GDP_Per_Capita), nbest = 2, nvmax = 8, method = "forward")

#FinalModels
FirstFinal <- lm(log(GDP_Per_Capita) ~ YearsCol + Intensity + FirstPolity + Mercantilism + Mining + Immigration + PoliticalChange + TotalChange)
FirstFinalSt <- lm.beta(FirstFinal)
summary(FirstFinalSt)

SecondFinal <- lm(log(GDP_Per_Capita) ~ YearsCol + Intensity + FirstPolity + Mining + Immigration + PoliticalChange + EconomicChange + SocialChange +TotalChange)
SecondFinalSt <- lm.beta(SecondFinal)
summary(SecondFinalSt)

#Plot of Colony In Years and GDP
ggplot(Colonialism, aes(x=YearsCol, y = log(GDP_Per_Capita)))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_bw()+
  xlab("Years a Colony")+
  ylab("Natural Log of GDP per Capita")

