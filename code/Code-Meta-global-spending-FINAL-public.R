rm(list = ls()) #clear list

#automatic installation of required packages
packages <- c("xlsx","calibrate","stargazer","sandwich","lmtest","getopt","CausalGAM","ggplot2","reshape2","xts",
              "lattice","gridExtra","gtable","plm","lfe","lmtest","car","tis","foreign","MASS","quantreg","ggrepel",
              "dplyr","stringr","datasets","rio","psych","systemfit","MatchIt","CRTgeeDR","eurostat","plyr","zoo","ggthemes",
              "robumeta","metafor","dplyr","clubSandwich","Hmisc","metafor","pracma","pkgs","broom","sjPlot", "here", "data.table")

#load packages
library(xlsx) #Excel-Paket laden
library(calibrate) #Laden des Pakets, das f??r Datenbeschriftung n??tig ist
library (stargazer) #Laden des Pakets, mit dem R-Regressionsoutput in Latex-Tabellen ??bergef??hrt werden kann
library(sandwich)
library(lmtest)
library(getopt)
library(CausalGAM)
library(ggplot2)
library(reshape2)
library(xts)
library(lattice)
library(gridExtra)
library(gtable)
library(plm)
library(lfe)
library(lmtest)
library(car)
library(tis)
library(foreign)
library(MASS)
library(quantreg)
library(ggrepel)
library(dplyr)
library(stringr)
library(ggplot2)
library(datasets)
library(rio)
library(psych)
library(systemfit)
library(foreign)
library(MatchIt)
library(CRTgeeDR)
library(plyr)
library(zoo)
library(ggthemes)
library("robumeta")
library("metafor")
library("dplyr")
library(clubSandwich)
library(Hmisc)
library(metafor)
library(pracma)
library(broom)
library(sjPlot)
library(here)
library(data.table)

dat <- fread(here("data/Meta-data-globalisation-spending.csv"))

#calculate the partial correlation coefficient
dat$PartialCorrelationCoefficient <- dat$Tstatistic / (sqrt((dat$Tstatistic^2)+dat$DegreesofFreedom))

#calculate the standard error of the partial correlation coefficient
dat$StandardErrorPartialCorrelation <- sqrt((1-(dat$PartialCorrelationCoefficient)^2)/dat$DegreesofFreedom)
dat$TstatisticPartialCorrelation <- dat$PartialCorrelationCoefficient / dat$StandardErrorPartialCorrelation
#Precision
dat$PrecSE <- 1 / dat$StandardErrorPartialCorrelation
#InverseSE
dat$InverseSE <- 1 / dat$StandardError
#Variance 
dat$Variance <- dat$StandardErrorPartialCorrelation^2
#PrecVariance
dat$PrecVariance <- 1 / dat$Variance

#transform r to Z and calculate the corresponding sample variances.

dat <- escalc(measure="ZCOR", ri=PartialCorrelationCoefficient, ni=Observations, data=dat) 

#average year
dat$MeanYearData<- (dat$StartingYearData+dat$EndYearData)/2

dat_long <- melt(dat, id=1:64)

####
#Publication bias (section 4)

### Funnel plot (Figure 1)

plot_funnel <- ggplot(data=dat,
                      aes(x=PartialCorrelationCoefficient, y=PrecSE)) +
  geom_point() +
  xlab("Partial correlation coefficient") +
  ylab("Inverse of the standard error") +
  ggtitle("Funnel plot of globalization-spending\n partial correlations (n=1182)")+
  theme(title=element_text(size=10, face='bold'))+
  theme(panel.background = element_rect(fill = "white")) +
  theme(legend.position="bottom")+
  theme(legend.title=element_blank()) +
  geom_vline(xintercept=0, colour="black", linetype=2)+
  theme(legend.text = element_text(colour="black", size = 6))+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.text.y=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))
plot_funnel

#Table 2
#Descriptive statistics
mean(dat_long$PartialCorrelationCoefficient)
mean(dat_long$TotalSpending)
mean(dat_long$SocialSpending)
mean(dat_long$PublicInvestment)
mean(dat_long$GovernmentConsumption)
mean(dat_long$EducationSpending)
mean(dat_long$HealthSpending)
mean(dat_long$OtherSpending)
mean(dat_long$TradeGlobalization)
mean(dat_long$FinancialGlobalization)
mean(dat_long$OverallEconomicGlobalization)
mean(dat_long$CrossSection)
mean(dat_long$StartingYearData)
mean(dat_long$EndYearData)
mean(dat_long$AdvancedCountriesOnly)
mean(dat_long$DevelopingCountriesOnly)
mean(dat_long$MixofCountries)
mean(dat_long$LevelLevel)
mean(dat_long$ChangeChange)
mean(dat_long$LevelChange)
mean(dat_long$ChangeLevel)
mean(dat_long$OLS)
mean(dat_long$GMM)
mean(dat_long$RandomEffects)
mean(dat_long$OtherEstimator)
mean(dat_long$CountryFixedEffects)
mean(dat_long$TimeFixedEffects)
mean(dat_long$GlobalizationInteracted)
mean(dat_long$StandardErrorPartialCorrelation)
mean(dat_long$EconomicsJournal)
mean(dat_long$Primary)
mean(dat_long$Crossauthor)
mean(dat_long$Prior)
mean(dat_long$GDPgrowth)
mean(dat_long$Unemployment)
mean(dat_long$TermsOfTradeRisk)
mean(dat_long$IncomeLevel)
mean(dat_long$OldAge)
mean(dat_long$PartisanPolitics)
mean(dat_long$LaborPower)
mean(dat_long$Democracy)
mean(dat_long$MeanYearData)

sd(dat_long$PartialCorrelationCoefficient)
sd(dat_long$TotalSpending)
sd(dat_long$SocialSpending)
sd(dat_long$PublicInvestment)
sd(dat_long$GovernmentConsumption)
sd(dat_long$EducationSpending)
sd(dat_long$HealthSpending)
sd(dat_long$OtherSpending)
sd(dat_long$TradeGlobalization)
sd(dat_long$FinancialGlobalization)
sd(dat_long$OverallEconomicGlobalization)
sd(dat_long$CrossSection)
sd(dat_long$StartingYearData)
sd(dat_long$EndYearData)
sd(dat_long$AdvancedCountriesOnly)
sd(dat_long$DevelopingCountriesOnly)
sd(dat_long$MixofCountries)
sd(dat_long$LevelLevel)
sd(dat_long$ChangeChange)
sd(dat_long$LevelChange)
sd(dat_long$ChangeLevel)
sd(dat_long$OLS)
sd(dat_long$GMM)
sd(dat_long$RandomEffects)
sd(dat_long$OtherEstimator)
sd(dat_long$CountryFixedEffects)
sd(dat_long$TimeFixedEffects)
sd(dat_long$GlobalizationInteracted)
sd(dat_long$StandardErrorPartialCorrelation)
sd(dat_long$EconomicsJournal)
sd(dat_long$Primary)
sd(dat_long$Crossauthor)
sd(dat_long$Prior)
sd(dat_long$GDPgrowth)
sd(dat_long$Unemployment)
sd(dat_long$TermsOfTradeRisk)
sd(dat_long$IncomeLevel)
sd(dat_long$OldAge)
sd(dat_long$PartisanPolitics)
sd(dat_long$LaborPower)
sd(dat_long$Democracy)
sd(dat_long$MeanYearData)

#Results on publication selection bias (table 1)

#column (1)
#average precision-weighted partial correlation
pubbias_2 <- lm(PartialCorrelationCoefficient ~1, weights=PrecVariance, data=dat_long)
summary(pubbias_2)
coeftest(pubbias_2, vcov.=function(x) vcovHC(x, method="white1", type="HC1"))

#column (2)
#FAT-PET test
pubbias_1 <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation, weights=PrecVariance, data=dat_long)
summary(pubbias_1)
coeftest(pubbias_1, vcov.=function(x) vcovHC(x, method="white1", type="HC1"))

#column (3)
#Fisher's z-transformed
pubbias_3 <- lm(yi ~ StandardErrorPartialCorrelation, weights=PrecVariance, data=dat_long)
summary(pubbias_3)
coeftest(pubbias_3, vcov.=function(x) vcovHC(x, method="white1", type="HC1"))

###
#Multivariate meta-regression analysis (section 5)

#Table 3
#column (1)
#general-to-specific
pubbias_2_var_gts <- lm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + SocialSpending + PublicInvestment + GovernmentConsumption + EducationSpending + HealthSpending  + OtherSpending + FinancialGlobalization + OverallEconomicGlobalization + DevelopingCountriesOnly + MixofCountries + ChangeChange + LevelChange + ChangeLevel  + OLS + RandomEffects + OtherEstimator + CountryFixedEffects + EconomicsJournal + Prior + GDPgrowth + Unemployment +  TermsOfTradeRisk +Democracy, weights=PrecVariance, data=dat_long)
summary(pubbias_2_var_gts)

coef_test(pubbias_2_var_gts, vcov = "CR1", #CR1 refers to small sample correction; see package documentation clubSandwich
          cluster = dat_long$id, test = "naive-t")

#column (2)
#Random effects panel
RE <- rma(PartialCorrelationCoefficient, Variance, mods = ~ StandardErrorPartialCorrelation + SocialSpending + PublicInvestment + GovernmentConsumption + EducationSpending + HealthSpending  + OtherSpending + FinancialGlobalization + OverallEconomicGlobalization + DevelopingCountriesOnly + MixofCountries + ChangeChange + LevelChange + ChangeLevel  + OLS + RandomEffects + OtherEstimator + CountryFixedEffects + EconomicsJournal + Prior + GDPgrowth + Unemployment +  TermsOfTradeRisk + Democracy, weights=PrecSE, weighted=TRUE, method="REML", data=dat_long) 
summary(RE)

coef_test(RE, vcov = "CR1", #CR1 refers to small sample correction; see package documentation clubSandwich
          cluster = dat_long$id, test = "naive-t")

#column (3)
#Robust regression
Robust <- rlm(PartialCorrelationCoefficient ~ StandardErrorPartialCorrelation + SocialSpending + PublicInvestment + GovernmentConsumption + EducationSpending + HealthSpending  + OtherSpending + FinancialGlobalization + OverallEconomicGlobalization + DevelopingCountriesOnly + MixofCountries + ChangeChange + LevelChange + ChangeLevel  + OLS + RandomEffects + OtherEstimator + CountryFixedEffects + EconomicsJournal + Prior + GDPgrowth + Unemployment +  TermsOfTradeRisk + Democracy, data=dat_long)
summary(Robust)

#column (4)
#Fisher's z
pubbias_2_var_gts_Fisher <- lm(yi ~ StandardErrorPartialCorrelation + SocialSpending + PublicInvestment + GovernmentConsumption + EducationSpending + HealthSpending  + OtherSpending + FinancialGlobalization + OverallEconomicGlobalization + DevelopingCountriesOnly + MixofCountries + ChangeChange + LevelChange + ChangeLevel  + OLS + RandomEffects + OtherEstimator + CountryFixedEffects + EconomicsJournal + Prior + GDPgrowth + Unemployment +  TermsOfTradeRisk + Democracy, weights=PrecVariance, data=dat_long)
summary(pubbias_2_var_gts_Fisher)

coef_test(pubbias_2_var_gts_Fisher, vcov = "CR1", #CR1 refers to small sample correction; see package documentation clubSandwich
          cluster = dat_long$id, test = "naive-t")

#stargazer table
#WLS with standard errors clustered at the study level
ses.WLS <- list(coef_test(pubbias_2_var_gts, vcov = "CR1", #CR1 refers to small sample correction; see package documentation clubSandwich
                          cluster = dat_long$id, test = "naive-t")[,2]) #heteroskedasticity-robust standard errors
tvals.WLS <- list(coef_test(pubbias_2_var_gts, vcov = "CR1", #CR1 refers to small sample correction; see package documentation clubSandwich
                            cluster = dat_long$id, test = "naive-t")[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

#WLS (Fisher's z) with standard errors clustered at the study level
ses.WLS.Fisher <- list(coef_test(pubbias_2_var_gts_Fisher, vcov = "CR1", #CR1 refers to small sample correction; see package documentation clubSandwich
                          cluster = dat_long$id, test = "naive-t")[,2]) #heteroskedasticity-robust standard errors
tvals.WLS.Fisher <- list(coef_test(pubbias_2_var_gts_Fisher, vcov = "CR1", #CR1 refers to small sample correction; see package documentation clubSandwich
                            cluster = dat_long$id, test = "naive-t")[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.

#Robust regression with standard errors clustered at the study level
ses.Robust <- list(coeftest(Robust,vcov=NeweyWest(Robust, verbose=T))[,2]) #heteroskedasticity-robust standard errors
tvals.Robust <- list(coeftest(Robust,vcov=NeweyWest(Robust, verbose=T))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
tvals.Robust

#note that the random effects regression results are not included in this stargazer version of the table due to compatibility issues
stargazer(pubbias_2_var_gts, Robust, pubbias_2_var_gts_Fisher, t=list(unlist(tvals.WLS), unlist(tvals.Robust), unlist(tvals.WLS.Fisher)), se=list(unlist(ses.WLS), unlist(ses.Robust), unlist(ses.WLS.Fisher)))

####
#Appendix

#Figure A1
#frequency plot
partialvector <- dat_long$PartialCorrelationCoefficient
h<-hist(partialvector, breaks=10, col="red", xlab="partial correlation coefficient", 
        main="Distribution of globalization-spending\n partial correlation coefficients") 
xfit<-seq(min(partialvector),max(partialvector),length=40) 
yfit<-dnorm(xfit,mean=mean(partialvector),sd=sd(partialvector)) 
yfit <- yfit*diff(h$mids[1:2])*length(partialvector) 
lines(xfit, yfit, col="blue", lwd=2)

#kernel density plot
d <- density(dat_long$PartialCorrelationCoefficient) # returns the density data 
plot(d) # plots the results

hist(partialvector,breaks = 10, freq=F,main="Distribution of partial correlations:\n globalization and govnmt. spending\n (n=1182)",xlab="partial correlation coefficient\n economic globalization-government spending",ylab="density", ylim=c(0,3), xlim=c(-1,1))
lines(density(partialvector), col="red", lwd=2) 
curve(dnorm(x, mean = mean(partialvector), sd = sd(partialvector)), add=TRUE, col="blue", lty="dotted")
density(partialvector)

#distributional statistics
max(dat_long$PartialCorrelationCoefficient)
min(dat_long$PartialCorrelationCoefficient)
sd(dat_long$PartialCorrelationCoefficient)

#Table A1
#all-set
#unweighted average
uwa <- sum(dat$yi, na.rm=TRUE) / 1182
uwa

uwa <- sum(dat$PartialCorrelationCoefficient, na.rm=TRUE) / 1182
uwa

#(precision-weighted) average
dat$PreSE <- 1/dat$Variance
numerator <- dat$PartialCorrelationCoefficient*dat$PreSE
wa <- sum(numerator, na.rm=TRUE)/sum(dat$PreSE, na.rm=TRUE)
wa

#median
median(dat$PartialCorrelationCoefficient, na.rm=TRUE)

res <- rma(yi, vi, data=dat, method="REML") #Random Effecs
res 
predict(res, digits=3, transf=transf.ztor)
confint(res)  

fes <- rma(yi, vi, data=dat, method="FE") #Fixed Effecs
fes 
predict(fes, digits=3, transf=transf.ztor)
confint(fes)  

hes <- rma(yi, vi, data=dat, method="HS") #Hunter-Schmidt
hes 
predict(hes, digits=3, transf=transf.ztor)
confint(hes)  

#exclude top and bottom 10%
topbottom <- group_by(dat_long, id) %>%
  mutate(rank = rank(desc(PartialCorrelationCoefficient))) %>%
  filter(rank >=118) %>%
  filter(rank <=1064) %>%
  arrange(rank)

#unweighted average
uwa <- sum(topbottom$yi, na.rm=TRUE) / 946
uwa

uwa <- sum(topbottom$PartialCorrelationCoefficient, na.rm=TRUE) / 901
uwa

#(precision-weighted) average
topbottom$PreSE <- 1/topbottom$Variance
numerator <- topbottom$PartialCorrelationCoefficient*topbottom$PreSE
wa <- sum(numerator, na.rm=TRUE)/sum(topbottom$PreSE, na.rm=TRUE)
wa

#median
median(topbottom$PartialCorrelationCoefficient, na.rm=TRUE)

res <- rma(yi, vi, data=topbottom, method="REML") #Random Effecs
res 
predict(res, digits=3, transf=transf.ztor)
confint(res)  

fes <- rma(yi, vi, data=topbottom, method="FE") #Fixed Effecs
fes 
predict(fes, digits=3, transf=transf.ztor)
confint(fes)  

hes <- rma(yi, vi, data=topbottom, method="HS") #Hunter-Schmidt
hes 
predict(hes, digits=3, transf=transf.ztor)
confint(hes)
