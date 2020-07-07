#NLBELT anovas

setwd("/Users/katebuckeridge/OneDrive - University of Edinburgh/R/NLBELT/N2Oms")
setwd("C:/Users/kbuckeri/OneDrive - University of Edinburgh/R/NLBELT/N2Oms") ## at work

require(MASS)
require(lsmeans)
require(car)
citation("MASS")
citation("stats")#part of R
citation("car")
?Anova
?aov

#To cite the MASS package in publications use:
  
#  Venables, W. N. & Ripley, B. D. (2002)
#Modern Applied Statistics with S. Fourth
#Edition. Springer, New York. ISBN
#0-387-95457-0



#####################################################
##### N2O ##################################

n2o <- read.csv("JMPnlbelt2016.csv", header=T)

names(n2o)

#change Temp from integer to factor
#n2o$Temp = factor(n2o$Temp) #didn't work, changed 5 to t5, etc, that worked
summary(n2o$Region)

#fixed effect model
model.n2o.1 <- lm(Rate ~ Region*temp*soil, data = n2o)

#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.n2o.1)
par(opar)
# residual vs fitted (non-linear patterns) should: bounce randomly around 0, form horizontal, no outliers
# normal Q-Q (normal distribution) should: be on the line or there is skew
# scale-location (homoscedasticity = equal variance) should see: equal spread along line, line horizontal
# Residuals vs leverage looks for influential outliers
# look for consistent case numbers across plots. Here, 18, 23 and 26 keep popping up, but they are real values


#outliers and not normal, log transform
n2o$Rate_log = log(n2o$Rate)

#new model
model.n2o.2 <- lm(Rate_log ~ Region*temp*soil, data = n2o)
opar = par (mfrow = c(2,2))
plot(model.n2o.2)
par(opar)
# better

#run anova
anova(model.n2o.2)
#Analysis of Variance Table
#Response: Rate_log
#Df Sum Sq Mean Sq F value    Pr(>F)    
#Region            2 14.712   7.356  7.2548  0.001619 ** 
#  temp              2 11.868   5.934  5.8521  0.005007 ** 
#  soil              2 68.622  34.311 33.8378 2.979e-10 ***
#  Region:temp       4  4.364   1.091  1.0758  0.377568    
#temp:soil         4  3.408   0.852  0.8401  0.505818    
#Region:soil       4  3.924   0.981  0.9675  0.432951    
#Region:temp:soil  8  5.345   0.668  0.6589  0.724765    
#Residuals        54 54.755   1.014                  
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#new model with type3 SS
options(contrasts = c("contr.sum","contr.poly"))
model.n2o.2 <- lm(Rate_log ~ Region*temp*soil, data = n2o)
drop1(model.n2o.2, .~., test="F")
#same result as lm and anova

# another way to get type3
options(contrasts=c("contr.sum", "contr.poly"))
m.7 <- lm(Rate_log ~ Region*temp*soil, data = n2o)
Anova(m.7, type="III")



#new model with type2
require(car)
Anova(lm(Rate_log ~ Region*temp*soil, data=n2o, type=2))
#didn't work, or at least says it didn't use the type arguement. Yet it reports a type 2 and same values as ty6pe 1 or 3

## remove ns terms
model.n2o.3 <- lm(Rate_log ~ Region+temp+soil, data = n2o)
opar = par (mfrow = c(2,2))
plot(model.n2o.3)
par(opar)

anova(model.n2o.3)
#Response: Rate_log
#Df Sum Sq Mean Sq F value    Pr(>F)    
#Region     2 14.712   7.356  7.5821  0.001010 ** 
#  temp       2 11.868   5.934  6.1162  0.003482 ** 
#  soil       2 68.622  34.311 35.3646 1.663e-11 ***
#  Residuals 74 71.795   0.970  

lsmeans(model.n2o.2, pairwise ~ Region)
lsmeans(model.n2o.3, pairwise ~ Region)
#$lsmeans
#Region     lsmean        SE df   lower.CL    upper.CL
#ER     -0.4132154 0.1895613 74 -0.7909246 -0.03550625
#GC      0.5295203 0.1895613 74  0.1518112  0.90722952
#SR     -0.3301721 0.1895613 74 -0.7078813  0.04753705

#Results are averaged over the levels of: temp, soil 
#Confidence level used: 0.95 

#$contrasts
#contrast   estimate        SE df t.ratio p.value
#ER - GC  -0.9427358 0.2680802 74  -3.517  0.0021
#ER - SR  -0.0830433 0.2680802 74  -0.310  0.9485
#GC - SR   0.8596925 0.2680802 74   3.207  0.0056

#Results are averaged over the levels of: temp, soil 
#P value adjustment: tukey method for a family of 3 means 

lsmeans(model.n2o.2, pairwise ~ temp)
lsmeans(model.n2o.3, pairwise ~ temp)
#$lsmeans
#temp      lsmean        SE df    lower.CL   upper.CL
#t15  -0.08126583 0.1895613 74 -0.45897501  0.2964433
#t25   0.40242223 0.1895613 74  0.02471305  0.7801314
#t5   -0.53502359 0.1895613 74 -0.91273276 -0.1573144

#Results are averaged over the levels of: Region, soil 
#Confidence level used: 0.95 

#$contrasts
#contrast    estimate        SE df t.ratio p.value
#t15 - t25 -0.4836881 0.2680802 74  -1.804  0.1752
#t15 - t5   0.4537578 0.2680802 74   1.693  0.2148
#t25 - t5   0.9374458 0.2680802 74   3.497  0.0023

#Results are averaged over the levels of: Region, soil 
#P value adjustment: tukey method for a family of 3 means 

lsmeans(model.n2o.2, pairwise ~ soil)
lsmeans(model.n2o.3, pairwise ~ soil)
#$lsmeans
#soil     lsmean        SE df   lower.CL   upper.CL
#c    -0.2140142 0.1895613 74 -0.5917234  0.1636949
#m    -1.1204158 0.1895613 74 -1.4981249 -0.7427066
#o     1.1205628 0.1895613 74  0.7428536  1.4982720

#Results are averaged over the levels of: Region, temp 
#Confidence level used: 0.95 

#$contrasts
#contrast   estimate        SE df t.ratio p.value
#c - m     0.9064015 0.2680802 74   3.381  0.0033
#c - o    -1.3345770 0.2680802 74  -4.978  <.0001
#m - o    -2.2409786 0.2680802 74  -8.359  <.0001

#Results are averaged over the levels of: Region, temp 
#P value adjustment: tukey method for a family of 3 means 




############################################################
###### Combo ######
n2oC <- n2o[n2o$soil == "o", ]

model.combo.1 <- lm(Combo ~ Region+temp+temp*Region, data = n2oC)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.combo.1)
par(opar)

summary(n2o$Combo)

n2o$combo_log <- log(n2o$Combo+15)

model.combo.3 <- lm(combo_log ~ Region+temp+temp*Region, data = n2o)
opar = par (mfrow = c(2,2))
plot(model.combo.3)
par(opar) # I think this is worse, stick with model1

anova(model.combo.3)
#Region still not sig
n2o$Combo

anova(model.combo.1)
#Analysis of Variance Table

#Response: Combo
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# Region       2  50.673  25.336  4.1060 0.0339591 *  
# temp         2 170.701  85.351 13.8318 0.0002298 ***
# Region:temp  4  14.380   3.595  0.5826 0.6791874    
# Residuals   18 111.071   6.171 

## remove ns terms
model.combo.2 <- lm(Combo ~ temp + Region, data = n2o)
opar = par (mfrow = c(2,2))
plot(model.combo.2)
par(opar)

anova(model.combo.2)
#Response: Combo
# Df  Sum Sq Mean Sq F value   Pr(>F)    
# temp       2 170.701  85.351 14.9677 7.88e-05 ***
# Region     2  50.673  25.336  4.4432  0.02394 *  
# Residuals 22 125.451   5.702 

emmeans(model.combo.2, pairwise ~ Region)
# $emmeans
# Region emmean    SE df lower.CL upper.CL
# ER      -3.94 0.796 22    -5.59    -2.29
# GC      -6.79 0.796 22    -8.44    -5.14
# SR      -3.84 0.796 22    -5.49    -2.19
# 
# Results are averaged over the levels of: temp 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast estimate   SE df t.ratio p.value
# ER - GC    2.8567 1.13 22  2.538  0.0474 
# ER - SR   -0.0964 1.13 22 -0.086  0.9960 
# GC - SR   -2.9531 1.13 22 -2.623  0.0396 
# 
# Results are averaged over the levels of: temp 
# P value adjustment: tukey method for comparing a family of 3 estimates 

emmeans(model.combo.2, pairwise ~ temp)
# $emmeans
# temp emmean    SE df lower.CL upper.CL
# t15   -4.77 0.796 22    -6.42   -3.118
# t25   -7.98 0.796 22    -9.63   -6.330
# t5    -1.82 0.796 22    -3.47   -0.173
# 
# Results are averaged over the levels of: Region 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast  estimate   SE df t.ratio p.value
# t15 - t25     3.21 1.13 22  2.853  0.0241 
# t15 - t5     -2.95 1.13 22 -2.616  0.0402 
# t25 - t5     -6.16 1.13 22 -5.470  <.0001 
# 
# Results are averaged over the levels of: Region 
# P value adjustment: tukey method for comparing a family of 3 estimates 




#########################################################
### Combo percent

model.combop.1 <- lm(Combop ~ Region+temp+temp*Region, data = n2oC)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.combop.1)
par(opar)

#%, so stick with original data

anova(model.combop.1)
#Region not sig, same result as with combo

#Analysis of Variance Table
# Response: Combop
# Df  Sum Sq Mean Sq F value   Pr(>F)   
# Region       2  2059.3  1029.7  0.5883 0.565597   
# temp         2 21527.6 10763.8  6.1499 0.009215 **
# Region:temp  4  3469.9   867.5  0.4956 0.739151   
# Residuals   18 31504.5  1750.3  

## remove ns terms
model.combop.2 <- lm(Combop ~ temp, data = n2oC)
opar = par (mfrow = c(2,2))
plot(model.combop.2)
par(opar)

anova(model.combop.2)
# Response: Combop
# Df Sum Sq Mean Sq F value   Pr(>F)   
# temp       2  21528 10763.8  6.9756 0.004091 **
# Residuals 24  37034  1543.1 

lsmeans(model.combop.2, pairwise ~ temp)

# $lsmeans
# temp lsmean   SE df lower.CL upper.CL
# t15  -124.6 13.1 24   -151.6    -97.5
# t25   -87.9 13.1 24   -114.9    -60.8
# t5    -55.4 13.1 24    -82.5    -28.4
# 
# Confidence level used: 0.95 
# 
# $contrasts
# contrast  estimate   SE df t.ratio p.value
# t15 - t25    -36.7 18.5 24 -1.981  0.1386 
# t15 - t5     -69.1 18.5 24 -3.733  0.0029 
# t25 - t5     -32.4 18.5 24 -1.752  0.2073 

#P value adjustment: tukey method for a family of 3 means 




###################################################
###### nir #######

#fixed effect model
model.nir.1 <- lm(nirS ~ Region+temp+soil+temp*Region+temp*soil+Region*soil+Region*temp*soil, data = n2o)
opar = par (mfrow = c(2,2))
plot(model.nir.1)
par(opar)

#outliers and not normal, log transform
n2o$nir_log = log(n2o$nirS)

#new model
model.nir.2 <- lm(nir_log ~ Region+temp+soil+temp*Region+temp*soil+Region*soil+Region*temp*soil, data = n2o)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nir.2)
par(opar)
# better, and more clearly bimodal

anova(model.nir.2)
#Analysis of Variance Table

#Response: nir_log
#Df  Sum Sq Mean Sq  F value    Pr(>F)    
#Region            2  0.2299  0.1149   0.6069    0.5505    
#temp              2  0.3412  0.1706   0.9006    0.4153    
#soil              1 29.0003 29.0003 153.1101 1.572e-14 ***
#  Region:temp       4  0.2834  0.0708   0.3740    0.8256    
#temp:soil         2  0.1547  0.0774   0.4084    0.6678    
#Region:soil       2  0.1608  0.0804   0.4245    0.6573    
#Region:temp:soil  4  0.2331  0.0583   0.3077    0.8710    
#Residuals        36  6.8187  0.1894   

## remove ns terms
anova(model.nir.3)
#Response: nir_log
#Df  Sum Sq Mean Sq F value    Pr(>F)    
#soil       1 29.0003 29.0003  183.42 < 2.2e-16 ***
#  Residuals 52  8.2217  0.1581  

model.nir.3 <- lm(nir_log ~ soil, data = n2o)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nir.3)
par(opar)

lsmeans(model.nir.2, pairwise ~ soil)
lsmeans(model.nir.3, pairwise ~ soil)
#$lsmeans
#soil   lsmean         SE df lower.CL upper.CL
#m    11.98635 0.07652407 52 11.83279 12.13991
#o    13.45201 0.07652407 52 13.29845 13.60557

#Confidence level used: 0.95 

#$contrasts
#contrast  estimate        SE df t.ratio p.value
#m - o    -1.465663 0.1082214 52 -13.543  <.0001




############################################################
###### nosZ #############

#fixed effect model
model.nos.1 <- lm(nosZ ~ Region+temp+soil+temp*Region+temp*soil+Region*soil+Region*temp*soil, data = n2o)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nos.1)
par(opar)

#outliers and not normal, log transform
n2o$nos_log = log(n2o$nosZ)

#new model
model.nos.2 <- lm(nos_log ~ Region*temp*soil, data = n2o)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nos.2)
par(opar)
# better, and more clearly bimodal

anova(model.nos.2)
#Response: nos_log
#                   Df Sum Sq Mean Sq  F value    Pr(>F)    
#  Region            2  3.074   1.537  10.9587 0.0001918 ***
#  temp              2  1.033   0.517   3.6837 0.0350340 *  
#  soil              1 74.915  74.915 534.1302 < 2.2e-16 ***
#  Region:temp       4  0.195   0.049   0.3478 0.8438250    
#  temp:soil         2  0.141   0.071   0.5036 0.6085501    
#  Region:soil       2  0.904   0.452   3.2228 0.0515759 .  
#  Region:temp:soil  4  0.588   0.147   1.0477 0.3963755    
#  Residuals        36  5.049   0.140   

#new model with type3 SS
options(contrasts = c("contr.sum","contr.poly"))
model.nos.3 <- lm(nos_log ~ Region*temp*soil, data = n2o)
drop1(model.nos.3, .~., test="F")
#same result as lm and anova

# another way to get type3
options(contrasts=c("contr.sum", "contr.poly"))
m.nos.4 <- lm(nos_log ~ Region*temp*soil, data = n2o)
Anova(m.nos.4, type="II")

# same results with type 2 and 3


## remove ns terms

model.nos.3 <- lm(nos_log ~ Region+temp+soil+Region*soil, data = n2o)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nos.3)
par(opar)

anova(model.nos.3)
#Response: nos_log
#Df Sum Sq Mean Sq  F value    Pr(>F)    
#Region       2  3.074   1.537  11.8364 7.128e-05 ***
#  temp         2  1.033   0.517   3.9788   0.02548 *  
#  soil         1 74.915  74.915 576.9104 < 2.2e-16 ***
#  Region:soil  2  0.904   0.452   3.4810   0.03911 *  
#  Residuals   46  5.973   0.130 

lsmeans(model.nos.2, pairwise ~ Region)
lsmeans(model.nos.3, pairwise ~ Region)
#$lsmeans
#Region   lsmean         SE df lower.CL upper.CL
#ER     14.72811 0.08493655 46 14.55715 14.89908
#GC     15.22135 0.08493655 46 15.05038 15.39232
#SR     14.70323 0.08493655 46 14.53227 14.87420

#Results are averaged over the levels of: temp, soil 
#Confidence level used: 0.95 

#$contrasts
#contrast    estimate        SE df t.ratio p.value
#ER - GC  -0.49323464 0.1201184 46  -4.106  0.0005
#ER - SR   0.02487886 0.1201184 46   0.207  0.9766
#GC - SR   0.51811351 0.1201184 46   4.313  0.0002

#Results are averaged over the levels of: temp, soil 
#P value adjustment: tukey method for a family of 3 means

lsmeans(model.nos.2, pairwise ~ temp)
lsmeans(model.nos.3, pairwise ~ temp)
#$lsmeans
#temp   lsmean         SE df lower.CL upper.CL
#t15  14.87043 0.08493655 46 14.69946  15.0414
#t25  14.72213 0.08493655 46 14.55116  14.8931
#t5   15.06013 0.08493655 46 14.88916  15.2311

#Results are averaged over the levels of: Region, soil 
#Confidence level used: 0.95 

#$contrasts
#contrast    estimate        SE df t.ratio p.value
#t15 - t25  0.1482992 0.1201184 46   1.235  0.4392
#t15 - t5  -0.1897006 0.1201184 46  -1.579  0.2647
#t25 - t5  -0.3379998 0.1201184 46  -2.814  0.0193

#Results are averaged over the levels of: Region, soil 
#P value adjustment: tukey method for a family of 3 means  

lsmeans(model.nos.3, pairwise ~ soil)
#$lsmeans
#soil   lsmean         SE df lower.CL upper.CL
#m    13.70639 0.0693504 46 13.56679 13.84598
#o    16.06208 0.0693504 46 15.92248 16.20167

#Results are averaged over the levels of: Region, temp 
#Confidence level used: 0.95 

#$contrasts
#contrast estimate        SE df t.ratio p.value
#m - o    -2.35569 0.09807628 46 -24.019  <.0001

#Results are averaged over the levels of: Region, temp 

#slightly sig R x s interaction means I have to do multiple pairwise, which requires subsetting data by soil
n2oO <- n2o[n2o$soil == "o", ]
n2oM <- n2o[n2o$soil == "m", ]


#check to make sure that subsetting worked properly
summary(n2oO$soil)
summary(n2oM$soil)


library(agricolae)  # to run Tukeys multiple comparisons for R x s

#check shape
model.org.nos.0 <- lm(nosZ ~ Region, data = n2oO)
opar = par (mfrow = c(2,2))
plot(model.org.nos.0)
par(opar)

#can't use Tukeys with lm
model.org.nos.1 <-aov(nosZ ~ Region, data = n2oO, na.action = na.exclude)

TukeyHSD(model.org.nos.1, "Region", order=TRUE)
#Tukey multiple comparisons of means
#95% family-wise confidence level
#factor levels have been ordered

#Fit: aov(formula = nosZ ~ Region, data = n2oO, na.action = na.exclude)

#$Region
#diff      lwr      upr     p adj
#ER-SR 2067167 -1928525  6062860 0.4132420
#GC-SR 8553578  4557885 12549271 0.0000501
#GC-ER 6486411  2490718 10482104 0.0012893

#check shape
model.min.nos.0 <- lm(nosZ ~ Region, data = n2oM)
opar = par (mfrow = c(2,2))
plot(model.min.nos.0)
par(opar)

#can't use Tukeys with lm
model.min.nos.1 <-aov(nosZ ~ Region, data = n2oM, na.action = na.exclude)

TukeyHSD(model.min.nos.1, "Region", order=TRUE)
#Tukey multiple comparisons of means
#95% family-wise confidence level
#factor levels have been ordered

#Fit: aov(formula = nosZ ~ Region, data = n2oM, na.action = na.exclude)

#$Region
#diff        lwr      upr     p adj
#SR-ER 220763.7 -273484.44 715011.8 0.5140818
#GC-ER 461924.1  -32323.98 956172.2 0.0700966
#GC-SR 241160.5 -253087.65 735408.6 0.4540746




##########################################################
##### nirnos #########

#fixed effect model
n2o.2 <- n2o[n2o$soil!= "c",]
summary(n2o.2)
model.nn.1 <- lm(nirnos ~ Region+temp+soil+temp*Region+temp*soil+Region*soil+Region*temp*soil, data = n2o.2)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nn.1)
par(opar)

#outliers and not normal, log transform
n2o.2$nn_log = log(n2o.2$nirnos)


#new model
model.nns.2 <- lm(nn_log ~ Region+temp+soil+temp*Region+temp*soil+Region*soil+Region*temp*soil, data = n2o.2)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nns.2)
par(opar)
# better

anova(model.nns.2)
#Response: nn_log
#                   Df  Sum Sq Mean Sq F value    Pr(>F)    
#  Region            2  3.4086  1.7043  9.1527 0.0006113 ***
#  temp              2  0.3073  0.1537  0.8252 0.4462716    
#  soil              1 10.6940 10.6940 57.4308  5.81e-09 ***
#  Region:temp       4  0.3588  0.0897  0.4817 0.7489253    
#  temp:soil         2  0.0308  0.0154  0.0826 0.9208944    
#  Region:soil       2  1.2930  0.6465  3.4720 0.0418019 *  
#  Region:temp:soil  4  0.4595  0.1149  0.6169 0.6532685    
#  Residuals        36  6.7034  0.1862  

## remove ns terms
model.nns.3 <- lm(nn_log ~ Region+soil+Region*soil, data = n2o.2)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nns.3)
par(opar)

anova(model.nns.3)
#Analysis of Variance Table
#Response: nn_log
#              Df  Sum Sq Mean Sq F value    Pr(>F)    
#  Region       2  3.4086  1.7043 10.4082 0.0001759 ***
#  soil         1 10.6940 10.6940 65.3082  1.66e-10 ***
#  Region:soil  2  1.2930  0.6465  3.9482 0.0258561 *  
#  Residuals   48  7.8598  0.1637    

lsmeans(model.nns.2, pairwise ~ Region)
lsmeans(model.nns.3, pairwise ~ Region)
#$lsmeans
#Region    lsmean         SE df  lower.CL  upper.CL
#ER     -2.087921 0.09537844 48 -2.279692 -1.896150
#GC     -2.503987 0.09537844 48 -2.695758 -2.312215
#SR     -1.903248 0.09537844 48 -2.095020 -1.711477

#Results are averaged over the levels of: soil 
#Confidence level used: 0.95 

#$contrasts
#contrast   estimate        SE df t.ratio p.value
#ER - GC   0.4160654 0.1348855 48   3.085  0.0093
#ER - SR  -0.1846727 0.1348855 48  -1.369  0.3650
#GC - SR  -0.6007382 0.1348855 48  -4.454  0.0001

#Results are averaged over the levels of: soil 
#P value adjustment: tukey method for a family of 3 means

lsmeans(model.nns.2, pairwise ~ soil)
lsmeans(model.nns.3, pairwise ~ soil)
#$lsmeans
#soil    lsmean         SE df  lower.CL  upper.CL
#m    -1.720038 0.07787617 48 -1.876619 -1.563458
#o    -2.610066 0.07787617 48 -2.766646 -2.453485

#Results are averaged over the levels of: Region, temp 
#Confidence level used: 0.95 

#$contrasts
#contrast  estimate       SE df t.ratio p.value
#m - o    0.8900275 0.1101335 48   8.081  <.0001

#Results are averaged over the levels of: Region, temp 

### need to assess the R x s interaction

boxplot(n2o.2$nirnos ~ n2o.2$Region + n2o.2$soil)

#slightly sig R x s interaction means I have to do multiple pairwise, which requires subsetting data by soil
n2oO <- n2o[n2o$soil == "o", ]
n2oM <- n2o[n2o$soil == "m", ]


#check to make sure that subsetting worked properly
summary(n2oO$soil)
summary(n2oM$soil)


library(agricolae)  # to run Tukeys multiple comparisons for R x s

#check shape
model.org.nns.0 <- lm(nirnos ~ Region, data = n2oO)
opar = par (mfrow = c(2,2))
plot(model.org.nns.0)
par(opar)

n2oO$nns_log = log(n2oO$nirnos)

model.org.nns.1 <- lm(nns_log ~ Region, data = n2oO)
opar = par (mfrow = c(2,2))
plot(model.org.nns.1)
par(opar)

#can't use Tukeys with lm
model.org.nns.1 <-aov(nns_log ~ Region, data = n2oO, na.action = na.exclude)

summary(model.org.nns.1)
#            Df Sum Sq Mean Sq F value   Pr(>F)    
#Region       2  4.428  2.2140    17.5 2.05e-05 ***
#  Residuals   24  3.036  0.1265     

TukeyHSD(model.org.nns.1, "Region", order=TRUE)
#Fit: aov(formula = nns_log ~ Region, data = n2oO, na.action = na.exclude)

#$Region
#diff         lwr       upr     p adj
#ER-GC 0.6291537  0.21043631 1.0478711 0.0027248
#SR-GC 0.9787542  0.56003686 1.3974716 0.0000148
#SR-ER 0.3496006 -0.06911684 0.7683179 0.1142285

#check shape
model.min.nns.0 <- lm(nirnos ~ Region, data = n2oM)
opar = par (mfrow = c(2,2))
plot(model.min.nns.0)
par(opar)

n2oM$nns_log = log(n2oM$nirnos)

model.min.nns.1 <- lm(nns_log ~ Region, data = n2oM)
opar = par (mfrow = c(2,2))
plot(model.min.nns.1)
par(opar)

#can't use Tukeys with lm
model.min.nns.1 <-aov(nns_log ~ Region, data = n2oM, na.action = na.exclude)

TukeyHSD(model.min.nns.1, "Region", order=TRUE)
#Fit: aov(formula = nns_log ~ Region, data = n2oM, na.action = na.exclude)

#$Region
#diff        lwr       upr     p adj
#ER-GC 0.20297720 -0.3247931 0.7307475 0.6082835
#SR-GC 0.22272207 -0.3050482 0.7504923 0.5511053
#SR-ER 0.01974487 -0.5080254 0.5475151 0.9952002

# like with nos, larger diff between regions in org vs min




##########################################################
####### d15N

model.d15n.1 <- lm(D15N ~ Region+temp+soil+temp*Region+temp*soil+Region*soil+Region*temp*soil, data = n2o)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.d15n.1)
par(opar)

anova(model.d15n.1)
lsmeans(model.d15n.1, pairwise ~ Region)

#outliers and not normal, log transform
n2o$d15n30 <- n2o$D15N + 30

n2o$d15n_log = log(n2o$d15n30)

#new model
model.d15n.2 <- lm(d15n_log ~ Region+temp+soil+temp*Region+temp*soil+Region*soil+Region*temp*soil, data = n2o)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.d15n.2)
par(opar)
#better

anova(model.d15n.2)
#Analysis of Variance Table
#Response: d15n_log
#Df  Sum Sq Mean Sq F value  Pr(>F)  
#Region            2  2.0943 1.04714  2.7005 0.07642 .
#temp              2  0.4339 0.21695  0.5595 0.57483  
#soil              2  1.6299 0.81496  2.1017 0.13232  
#Region:temp       4  1.8561 0.46403  1.1967 0.32311  
#temp:soil         4  2.2111 0.55278  1.4256 0.23830  
#Region:soil       4  1.5945 0.39862  1.0280 0.40144  
#Region:temp:soil  8  0.3381 0.04226  0.1090 0.99874  
#Residuals        53 20.5515 0.38776 

## remove ns terms

model.d15n.3 <- lm(d15n_log ~ Region, data = n2o)
anova(model.d15n.3)
#Response: d15n_log
#Df  Sum Sq Mean Sq F value  Pr(>F)  
#Region     2  2.0943 1.04714  2.8177 0.06591 .
#Residuals 77 28.6151 0.37163

lsmeans(model.d15n.3, pairwise ~ Region)
#$lsmeans
#Region   lsmean        SE df lower.CL upper.CL
#ER     3.564754 0.1173196 77 3.331141 3.798367
#GC     3.957828 0.1173196 77 3.724215 4.191441
#SR     3.783221 0.1195545 77 3.545158 4.021284

#Confidence level used: 0.95 

#$contrasts
#contrast   estimate        SE df t.ratio p.value
#ER - GC  -0.3930741 0.1659150 77  -2.369  0.0525
#ER - SR  -0.2184669 0.1675027 77  -1.304  0.3972
#GC - SR   0.1746072 0.1675027 77   1.042  0.5526
#P value adjustment: tukey method for a family of 3 means 




##########################################################
####### NH4 ##########

n2o.din <- read.csv("JMPnlbelt2016DIN.csv", header=T)
n2o.din$temp = factor(n2o.din$temp)

summary(n2o.2)
model.nh4.1 <- lm(NH4 ~ Region*temp*soil, data = n2o.din)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nh4.1)
par(opar)

summary(n2o.din$NH4)
n2o.din$nh43 <- n2o.din$NH4 + 3
n2o.din$log_nh4 <- log(n2o.din$nh43)

model.nh4.2 <- lm(log_nh4 ~ Region*temp*soil, data = n2o.din)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nh4.2)
par(opar)
hist(n2o.din$log_nh4)
boxCox(model.nh4.2) #Error in bc1(out, lambda) : First argument must be strictly positive.

anova(model.nh4.2)
#  Analysis of Variance Table
#  Response: log_nh4
#                   Df  Sum Sq Mean Sq F value    Pr(>F)    
#  Region            2  3.8240  1.9120 13.2785 4.792e-05 ***
#  temp              2 15.9875  7.9938 55.5157 1.000e-11 ***
#  soil              1  8.3768  8.3768 58.1756 5.025e-09 ***
#  Region:temp       4  0.5066  0.1267  0.8797   0.48577    
#  Region:soil       2  1.1896  0.5948  4.1307   0.02426 *  
#  temp:soil         2  8.6921  4.3460 30.1827 2.008e-08 ***
#  Region:temp:soil  4  0.4945  0.1236  0.8586   0.49802    
#  Residuals        36  5.1837  0.1440 

lsmeans(model.nh4.2, pairwise ~ Region)
lsmeans(model.nh4.2, pairwise ~ temp)
lsmeans(model.nh4.2, pairwise ~ soil)

n2o.din.org <- n2o.din[n2o.din$soil == "o",]
n2o.din.min <- n2o.din[n2o.din$soil == "m",]
summary(n2o.din.org)
summary(n2o.din.min)

### NH4 soil * Region interaction

model.dino.1 <- lm(log_nh4 ~ Region, data = n2o.din.org)
opar = par (mfrow = c(2,2))
plot(model.dino.1)
par(opar)

model.dino.org.1 <-aov(log_nh4 ~ Region, data = n2o.din.org, na.action = na.exclude)

TukeyHSD(model.dino.org.1, "Region", order=TRUE)

model.dinm.1 <- lm(log_nh4 ~ Region, data = n2o.din.min)
opar = par (mfrow = c(2,2))
plot(model.dinm.1)
par(opar)

model.dinm.min.1 <-aov(log_nh4 ~ Region, data = n2o.din.min, na.action = na.exclude)

TukeyHSD(model.dinm.min.1, "Region", order=TRUE)
## min sig diff, between GC and ER and GC and SR. org no sig diff between regions

### NH4 soil * temp interaction
model.dino.1 <- lm(log_nh4 ~ temp, data = n2o.din.org)
opar = par (mfrow = c(2,2))
plot(model.dino.1)
par(opar)

model.dino.org.1 <-aov(log_nh4 ~ temp, data = n2o.din.org, na.action = na.exclude)

TukeyHSD(model.dino.org.1, "temp", order=TRUE)

model.dinm.1 <- lm(log_nh4 ~ temp, data = n2o.din.min)
opar = par (mfrow = c(2,2))
plot(model.dinm.1)
par(opar)

model.dinm.min.1 <-aov(log_nh4 ~ temp, data = n2o.din.min, na.action = na.exclude)

TukeyHSD(model.dinm.min.1, "temp", order=TRUE)
## min sig diff between 25 and 5. org sig diff between all temps



############################################################
#### NO3 ##########

summary(n2o.2)

n2o.din <- n2o.din[1:54,] #removes the initial values

model.no3.1 <- lm(NO3 ~ Region*temp*soil, data = n2o.din)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.no3.1)
par(opar)
hist(n2o.din$NO3)

summary(n2o.din$NO3)
n2o.din$no31 <- n2o.din$NO3 + 1
n2o.din$log_no3 <- log(n2o.din$no31)

model.no3.2 <- lm(log_no3 ~ Region*temp*soil, data = n2o.din)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.no3.2)
par(opar)
hist(n2o.din$log_no3)# way better but still skewed

anova(model.no3.2)
#Analysis of Variance Table
#Response: log_no3
#                   Df Sum Sq Mean Sq F value    Pr(>F)    
#  Region            2 0.7915 0.39574  2.8374 0.0717346 .  
#  temp              2 2.7871 1.39355  9.9914 0.0003535 ***
#  soil              1 2.2403 2.24025 16.0621 0.0002947 ***
#  Region:temp       4 2.2977 0.57442  4.1185 0.0075396 ** 
#  Region:soil       2 1.3840 0.69202  4.9616 0.0124989 *  
#  temp:soil         2 1.5203 0.76014  5.4500 0.0085569 ** 
#  Region:temp:soil  4 0.7507 0.18766  1.3455 0.2720921    
#  Residuals        36 5.0211 0.13947

lsmeans(model.no3.2, pairwise ~ Region)
lsmeans(model.no3.2, pairwise ~ temp)
lsmeans(model.no3.2, pairwise ~ soil)

n2o.din.org <- n2o.din[n2o.din$soil == "o",]
n2o.din.min <- n2o.din[n2o.din$soil == "m",]
summary(n2o.din.org)
summary(n2o.din.min)

model.no3.2 <- lm(log_no3 ~ Region*temp, data = n2o.din.org)
anova(model.no3.2)
# Response: log_no3
#              Df Sum Sq Mean Sq F value    Pr(>F)    
# Region       2 2.0729 1.03646  5.1592 0.0169370 *  
# temp         2 4.2095 2.10476 10.4769 0.0009605 ***
# Region:temp  4 2.6158 0.65395  3.2552 0.0356298 *  
# Residuals   18 3.6161 0.20090


model.no3.2 <- lm(log_no3 ~ Region*temp, data = n2o.din.min)
anova(model.no3.2)
# Response: log_no3
#             Df  Sum Sq  Mean Sq F value Pr(>F)
# Region       2 0.10260 0.051298  0.6572 0.5303
# temp         2 0.09788 0.048940  0.6270 0.5455
# Region:temp  4 0.43256 0.108141  1.3855 0.2785
# Residuals   18 1.40497 0.078054 

### NO3 soil * Region interaction
## org
model.dino.2 <- lm(log_no3 ~ Region, data = n2o.din.org)
opar = par (mfrow = c(2,2))
plot(model.dino.2)
par(opar)

model.dino.org.2 <-aov(log_no3 ~ Region, data = n2o.din.org, na.action = na.exclude)

TukeyHSD(model.dino.org.2, "Region", order=TRUE)#SR-ER 0.09, rest ns

### min
model.dinm.2 <- lm(log_no3 ~ Region, data = n2o.din.min)
opar = par (mfrow = c(2,2))
plot(model.dinm.2)
par(opar)

model.dino.min.2 <-aov(log_no3 ~ Region, data = n2o.din.min, na.action = na.exclude)

TukeyHSD(model.dino.min.2, "Region", order=TRUE)
## org slightly sig between SR-ER (0.09), but not sig at all for mineral. Not sure why this is an interaction - rerun when divided by region

### NO3 soil * temp interaction
## org
model.dino.3 <- lm(log_no3 ~ temp, data = n2o.din.org)
opar = par (mfrow = c(2,2))
plot(model.dino.2)
par(opar)

model.dino.org.3 <-aov(log_no3 ~ temp, data = n2o.din.org, na.action = na.exclude)

TukeyHSD(model.dino.org.3, "temp", order=TRUE)

### min
model.dinm.3 <- lm(log_no3 ~ temp, data = n2o.din.min)
opar = par (mfrow = c(2,2))
plot(model.dinm.3)
par(opar)

model.dino.min.3 <-aov(log_no3 ~ temp, data = n2o.din.min, na.action = na.exclude)

TukeyHSD(model.dino.min.3, "temp", order=TRUE)
### org sig diff btween 5-15 (0.005), not sig for min

###### NO3 temp * Region interaction, within Org soil type (min ns)

n2o.din.er <- n2o.din.org[n2o.din.org$Region == "ER",]
n2o.din.sr <- n2o.din.org[n2o.din.org$Region == "SR",]
n2o.din.gc <- n2o.din.org[n2o.din.org$Region == "GC",]
summary(n2o.din.er)
summary(n2o.din.sr)
summary(n2o.din.gc)

### NO3 Region * temp interaction
## ER
model.diner.1 <- lm(log_no3 ~ temp, data = n2o.din.er)
opar = par (mfrow = c(2,2))
plot(model.diner.1)
par(opar)

model.diner.1 <-aov(log_no3 ~ temp, data = n2o.din.er, na.action = na.exclude)

TukeyHSD(model.diner.1, "temp", order=TRUE) # 25-15 P=0.04

### SR
model.dinsr.1 <- lm(log_no3 ~ temp, data = n2o.din.sr)
opar = par (mfrow = c(2,2))
plot(model.dinsr.1)
par(opar)

model.dinsr.1 <-aov(log_no3 ~ temp, data = n2o.din.sr, na.action = na.exclude)

TukeyHSD(model.dinsr.1, "temp", order=TRUE) #ns

### GC
model.dingc.1 <- lm(log_no3 ~ temp, data = n2o.din.gc)
opar = par (mfrow = c(2,2))
plot(model.dingc.1)
par(opar)

model.dingc.1 <-aov(log_no3 ~ temp, data = n2o.din.gc, na.action = na.exclude)

TukeyHSD(model.dingc.1, "temp", order=TRUE) #5-15 (0.001) and 5-25 (0.003)

### NO3 Region * soil O vs M only diff in SR
## ER
model.diner.2 <- lm(log_no3 ~ soil, data = n2o.din.er)
opar = par (mfrow = c(2,2))
plot(model.diner.2)
par(opar)

model.diner.2 <-aov(log_no3 ~ soil, data = n2o.din.er, na.action = na.exclude)

TukeyHSD(model.diner.2, "soil", order=TRUE) #ns

### SR
model.dinsr.2 <- lm(log_no3 ~ soil, data = n2o.din.sr)
opar = par (mfrow = c(2,2))
plot(model.dinsr.2)
par(opar)

model.dinsr.2 <-aov(log_no3 ~ soil, data = n2o.din.sr, na.action = na.exclude)

TukeyHSD(model.dinsr.2, "soil", order=TRUE) #o-m (0.0026)

### GC
model.dingc.2 <- lm(log_no3 ~ soil, data = n2o.din.gc)
opar = par (mfrow = c(2,2))
plot(model.dingc.2)
par(opar)

model.dingc.2 <-aov(log_no3 ~ soil, data = n2o.din.gc, na.action = na.exclude)

TukeyHSD(model.dingc.2, "soil", order=TRUE) #ns

###########################################################################################################################################################


##### NO3 and NH4 that include pre and final 20180223 (previous stats just had 3 temps and three regions, now this includes initial + 3 temps: does initial differ from final (5, 15, or 25)?)

##### pre-post summary of stats below:
# GC org 5        NO3  0.0000180   NH4  ns       
# GC org 15       NO3  0.0000000   NH4  0.0001115
# GC org 25       NO3  0.0000000   NH4  0.0000019     
# SR org 5        NO3  0.0413802   NH4  ns     
# SR org 15       NO3  0.0023840   NH4  ns   
# SR org 25       NO3  0.0082258   NH4  0.0223344    
# ER org 5        NO3  0.0010933   NH4  0.0251963
# ER org 15       NO3  0.0007104   NH4  ns
# ER org 25       NO3  0.0032416   NH4  0.0366404
# GC min 5        NO3   ns         NH4  ns
# GC min 15       NO3   ns         NH4  ns
# GC min 25       NO3   ns         NH4  ns
# SR min 5        NO3   0.0192633  NH4  ns
# SR min 15       NO3   0.0017471  NH4  ns
# SR min 25       NO3   0.0003895  NH4  ns
# ER min 5        NO3   0.0004702  NH4  ns
# ER min 15       NO3   0.0007900  NH4  ns
# ER min 25       NO3   0.0015562  NH4  ns


n2o.din2 <- read.csv("DIN_R_NLBELT2018.csv", header=T)
n2o.din2$Temp = factor(n2o.din2$Temp)

model.no3.1 <- lm(NO3 ~ Region*Temp*Soil, data = n2o.din2)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.no3.1)
par(opar)

summary(n2o.din2$NO3)
n2o.din2$no31 <- n2o.din2$NO3 + 1
n2o.din2$log_no3 <- log(n2o.din2$no31)

model.no3.2 <- lm(log_no3 ~ Region*Temp*Soil, data = n2o.din2)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.no3.2)
par(opar)

anova(model.no3.2)
#Analysis of Variance Table

#Response: log_no3
#                  Df Sum Sq Mean Sq F value    Pr(>F)    
# Region             0.948  0.4741  3.8096 0.0291343 *  
# Temp              3 34.933 11.6443 93.5683 < 2.2e-16 ***
#  Soil              1 11.144 11.1441 89.5489 1.495e-12 ***
#  Region:Temp       6  3.637  0.6061  4.8706 0.0005867 ***
#  Region:Soil       2  0.828  0.4140  3.3265 0.0443663 *  
#  Temp:Soil         3 11.013  3.6711 29.4992 5.842e-11 ***
#  Region:Temp:Soil  6  1.990  0.3317  2.6652 0.0259060 *  
#  Residuals        48  5.973  0.1244 

# need to isolate soil type and region
n2o.dino <- n2o.din2[n2o.din2$Soil == "O",]
n2o.dinm <- n2o.din2[n2o.din2$Soil == "M",]
n2o.dino.er <- n2o.dino[n2o.dino$Region == "ER",]
n2o.dino.sr <- n2o.dino[n2o.dino$Region == "SR",]
n2o.dino.gc <- n2o.dino[n2o.dino$Region == "GC",]
n2o.dinm.er <- n2o.dinm[n2o.dinm$Region == "ER",]
n2o.dinm.sr <- n2o.dinm[n2o.dinm$Region == "SR",]
n2o.dinm.gc <- n2o.dinm[n2o.dinm$Region == "GC",]
#check
summary(n2o.dino.er)

#anovas...ER org

model.no3.1a <- lm(NO3 ~ Temp, data = n2o.dino.er)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.no3.1a)
par(opar)

anova(model.no3.1a) #ns
summary(n2o.dino.er$NO3)
n2o.dino.er$no31 <- n2o.dino.er$NO3 + 1
n2o.dino.er$log_no3 <- log(n2o.dino.er$no31)

model.no3.1al <- lm(log_no3 ~ Temp, data = n2o.dino.er)
opar = par (mfrow = c(2,2))
plot(model.no3.1al)
par(opar)
anova(model.no3.1al) 

#             Df Sum Sq Mean Sq F value    Pr(>F)    
# Temp         3 16.9298  5.6433  19.251 0.0005123 ***
#Residuals     8  2.3451  0.2931

model.no3.1al2 <-aov(log_no3 ~ Temp, data = n2o.dino.er, na.action = na.exclude)
TukeyHSD(model.no3.1al2, "Temp", order=TRUE) 

#$Temp
#diff        lwr      upr     p adj
#5-15  0.1825307 -1.2331435 1.598205 0.9747080
#25-15 0.6099433 -0.8057310 2.025618 0.5438724
#0-15  2.9591543  1.5434800 4.374829 0.0006986
#25-5  0.4274125 -0.9882617 1.843087 0.7713232
#0-5   2.7766236  1.3609493 4.192298 0.0010737
#0-25  2.3492110  0.9335368 3.764885 0.0031731

#anovas...SR org

model.no3.1b <- lm(NO3 ~ Temp, data = n2o.dino.sr)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.no3.1b)
par(opar)

summary(n2o.dino.sr$NO3)
n2o.dino.sr$no31 <- n2o.dino.sr$NO3 + 1
n2o.dino.sr$log_no3 <- log(n2o.dino.sr$no31)

model.no3.1bl <- lm(log_no3 ~ Temp, data = n2o.dino.sr)
opar = par (mfrow = c(2,2))
plot(model.no3.1bl)
par(opar)
anova(model.no3.1bl)
#Response: log_no3
#          Df Sum Sq Mean Sq F value   Pr(>F)   
#Temp       3 10.4978  3.4993  11.446 0.00289 **
#Residuals  8  2.4458  0.3057 

model.no3.1bl2 <-aov(log_no3 ~ Temp, data = n2o.dino.sr, na.action = na.exclude)
TukeyHSD(model.no3.1bl2, "Temp", order=TRUE) 
#$Temp
#diff        lwr      upr     p adj
#25-15 0.4609699 -0.98477510 1.906715 0.7425752
#5-15  1.0033764 -0.44236855 2.449121 0.1967294
#0-15  2.4863792  1.04063422 3.932124 0.0025321
#5-25  0.5424065 -0.90333844 1.988152 0.6428282
#0-25  2.0254093  0.57966434 3.471154 0.0087952
#0-5   1.4830028  0.03725779 2.928748 0.0445258

#anovas...GC org

model.no3.1c <- lm(NO3 ~ Temp, data = n2o.dino.gc)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.no3.1c)
par(opar)

summary(n2o.dino.gc$NO3)
n2o.dino.gc$no31 <- n2o.dino.gc$NO3 + 1
n2o.dino.gc$log_no3 <- log(n2o.dino.gc$no31)

model.no3.1cl <- lm(log_no3 ~ Temp, data = n2o.dino.gc)
opar = par (mfrow = c(2,2))
plot(model.no3.1cl)
par(opar)
anova(model.no3.1cl)

#Response: log_no3
#           Df Sum Sq Mean Sq F value    Pr(>F)    
#Temp       3 20.1169  6.7056   514.3 1.739e-09 ***
#Residuals  8  0.1043  0.0130  

model.no3.1cl2 <-aov(log_no3 ~ Temp, data = n2o.dino.gc, na.action = na.exclude)
TukeyHSD(model.no3.1cl2, "Temp", order=TRUE) 

#$Temp
#diff         lwr       upr     p adj
#15-25 0.01404147 -0.2845200 0.312603 0.9986775
#5-25  1.97265344  1.6740919 2.271215 0.0000001
#0-25  3.00878732  2.7102258 3.307349 0.0000000
#5-15  1.95861196  1.6600505 2.257173 0.0000001
#0-15  2.99474584  2.6961843 3.293307 0.0000000
#0-5   1.03613388  0.7375724 1.334695 0.0000180

#anovas...ER min

model.no3.2a <- lm(NO3 ~ Temp, data = n2o.dinm.er)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.no3.2a)
par(opar)

anova(model.no3.2a) #ns
summary(n2o.dinm.er$NO3)
n2o.dinm.er$no31 <- n2o.dinm.er$NO3 + 1
n2o.dinm.er$log_no3 <- log(n2o.dinm.er$no31)

model.no3.2al <- lm(log_no3 ~ Temp, data = n2o.dinm.er)
opar = par (mfrow = c(2,2))
plot(model.no3.2al)
par(opar)
anova(model.no3.2al) 

#Df  Sum Sq Mean Sq F value    Pr(>F)    
#Temp       3 0.95916 0.31972  16.644 0.000844 ***
#Residuals  8 0.15368 0.01921

model.no3.2al2 <-aov(log_no3 ~ Temp, data = n2o.dinm.er, na.action = na.exclude)
TukeyHSD(model.no3.2al2, "Temp", order=TRUE) 

#$Temp
#diff        lwr       upr     p adj
#15-5  0.05497135 -0.3074252 0.4173679 0.9601248
#25-5  0.12256122 -0.2398354 0.4849578 0.7088189
#0-5   0.70434784  0.3419513 1.0667444 0.0011409
#25-15 0.06758987 -0.2948067 0.4299865 0.9300812
#0-15  0.64937649  0.2869799 1.0117731 0.0019454
#0-25  0.58178662  0.2193900 0.9441832 0.0038990


#anovas...SR min

model.no3.2b <- lm(NO3 ~ Temp, data = n2o.dinm.sr)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.no3.2b)
par(opar)

summary(n2o.dinm.sr$NO3)
n2o.dinm.sr$no31 <- n2o.dinm.sr$NO3 + 1
n2o.dinm.sr$log_no3 <- log(n2o.dinm.sr$no31)

model.no3.2bl <- lm(log_no3 ~ Temp, data = n2o.dinm.sr)
opar = par (mfrow = c(2,2))
plot(model.no3.2bl)
par(opar)
anova(model.no3.2bl)
#Response: log_no3
#          Df Sum Sq Mean Sq F value   Pr(>F)   
#Temp       3 1.71255 0.57085  17.294 0.0007407 ***
#Residuals  8 0.26406 0.03301

model.no3.2bl2 <-aov(log_no3 ~ Temp, data = n2o.dinm.sr, na.action = na.exclude)
TukeyHSD(model.no3.2bl2, "Temp", order=TRUE) 

#$Temp
#diff         lwr       upr     p adj
#15-25 0.2106293 -0.26441247 0.6856711 0.5221945
#5-25  0.4925147  0.01747294 0.9675565 0.0423781
#0-25  1.0078737  0.53283193 1.4829155 0.0006310
#5-15  0.2818854 -0.19315635 0.7569272 0.2999270
#0-15  0.7972444  0.32220263 1.2722862 0.0029559
#0-5   0.5153590  0.04031723 0.9904008 0.0341801

#anovas...GC min

model.no3.2c <- lm(NO3 ~ Temp, data = n2o.dinm.gc)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.no3.2c)
par(opar)
anova(model.no3.2c)

#Response: NO3
#         Df Sum Sq Mean Sq F value  Pr(>F)  
#Temp       3 2.9292 0.97641  4.0249 0.05117 .
#Residuals  8 1.9407 0.24259 

model.no3.2c2 <-aov(log_no3 ~ Temp, data = n2o.dinm.gc, na.action = na.exclude)
TukeyHSD(model.no3.2c2, "Temp", order=TRUE) 

# all ns

summary(n2o.dinm.gc$NO3)

n2o.dinm.gc$log_no3 <- log(n2o.dinm.gc$NO3)

model.no3.2cl <- lm(log_no3 ~ Temp, data = n2o.dinm.gc)
opar = par (mfrow = c(2,2))
plot(model.no3.2cl)
par(opar)
anova(model.no3.2cl) #ns

######## NH4

model.nh4.1 <- lm(NH4 ~ Region*Temp*Soil, data = n2o.din2)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nh4.1)
par(opar)

summary(n2o.din2$NH4)
n2o.din2$nh43 <- n2o.din2$NH4 + 3
n2o.din2$log_nh4 <- log(n2o.din2$nh43)

model.nh4.2 <- lm(log_nh4 ~ Region*Temp*Soil, data = n2o.din2)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nh4.2)
par(opar)

anova(model.nh4.2)
#Response: log_nh4
#                  Df  Sum Sq Mean Sq F value    Pr(>F)    
#Region            2  4.0885  2.0443 15.8200 5.280e-06 ***
#Temp              3 18.5724  6.1908 47.9087 1.799e-14 ***
#Soil              1  9.0192  9.0192 69.7971 6.449e-11 ***
#Region:Temp       6  2.1592  0.3599  2.7849   0.02095 *  
#Region:Soil       2  1.1841  0.5921  4.5819   0.01510 *  
#Temp:Soil         3  9.6668  3.2223 24.9361 7.168e-10 ***
#Region:Temp:Soil  6  1.7585  0.2931  2.2680   0.05241 .  
#Residuals        48  6.2026  0.1292  

# already isolated soil type and region

#anovas...ER org

model.nh4.1a <- lm(NH4 ~ Temp, data = n2o.dino.er)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nh4.1a)
par(opar)

anova(model.nh4.1a) 
#Response: NH4
#          Df  Sum Sq Mean Sq F value    Pr(>F)    
#Temp       3 1235.21  411.74  475.19 2.381e-09 ***
#Residuals  8    6.93    0.87 

summary(n2o.dino.er$NH4)
n2o.dino.er$nh43 <- n2o.dino.er$NH4 + 3
n2o.dino.er$log_nh4 <- log(n2o.dino.er$nh43)

model.nh4.1al <- lm(log_nh4 ~ Temp, data = n2o.dino.er)
opar = par (mfrow = c(2,2))
plot(model.nh4.1al)
par(opar)
anova(model.nh4.1al) 

#          Df  Sum Sq Mean Sq F value    Pr(>F)    
#Temp       3 14.9271  4.9757  17.182 0.0007574 ***
#Residuals  8  2.3167  0.2896

model.nh4.1al2 <-aov(log_nh4 ~ Temp, data = n2o.dino.er, na.action = na.exclude)
TukeyHSD(model.nh4.1al2, "Temp", order=TRUE) 

#$Temp
#diff         lwr      upr     p adj
#15-5  1.2547844 -0.15227176 2.661841 0.0815455
#0-5   1.6234458  0.21638964 3.030502 0.0251963
#25-5  3.1279885  1.72093233 4.535045 0.0004574
#0-15  0.3686614 -1.03839476 1.775718 0.8347913
#25-15 1.8732041  0.46614793 3.280260 0.0117463
#25-0  1.5045427  0.09748653 2.911599 0.0366404

#anovas...SR org

model.nh4.1b <- lm(NH4 ~ Temp, data = n2o.dino.sr)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nh4.1b)
par(opar)

summary(n2o.dino.sr$NH4)
n2o.dino.sr$nh41 <- n2o.dino.sr$NH4 + 1
n2o.dino.sr$log_nh4 <- log(n2o.dino.sr$nh41)

model.nh4.1bl <- lm(log_nh4 ~ Temp, data = n2o.dino.sr)
opar = par (mfrow = c(2,2))
plot(model.nh4.1bl)
par(opar)
anova(model.nh4.1bl)
#          Df Sum Sq Mean Sq F value   Pr(>F)   
#Temp       3 15.3685  5.1228   5.947 0.0196 *
#Residuals  8  6.8913  0.8614 

model.nh4.1bl2 <-aov(log_nh4 ~ Temp, data = n2o.dino.sr, na.action = na.exclude)
TukeyHSD(model.nh4.1bl2, "Temp", order=TRUE) 
#$Temp
#diff        lwr      upr     p adj
#5-0   0.3426758 -2.08410021 2.769452 0.9673319
#15-0  0.5275943 -1.89918166 2.954370 0.8957324
#25-0  2.8667827  0.44000673 5.293559 0.0223344
#15-5  0.1849185 -2.24185744 2.611695 0.9944801
#25-5  2.5241069  0.09733095 4.950883 0.0417507
#25-15 2.3391884 -0.08758759 4.765964 0.0588443

#anovas...GC org

model.nh4.1c <- lm(NH4 ~ Temp, data = n2o.dino.gc)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nh4.1c)
par(opar)

summary(n2o.dino.gc$NH4)

n2o.dino.gc$log_nh4 <- log(n2o.dino.gc$NH4)

model.nh4.1cl <- lm(log_nh4 ~ Temp, data = n2o.dino.gc)
opar = par (mfrow = c(2,2))
plot(model.nh4.1cl)
par(opar)
anova(model.nh4.1cl)

#           Df Sum Sq Mean Sq F value    Pr(>F)    
#Temp       3 14.4364  4.8121  114.82 6.473e-07 ***
#Residuals  8  0.3353  0.0419    

model.nh4.1cl2 <-aov(log_nh4 ~ Temp, data = n2o.dino.gc, na.action = na.exclude)
TukeyHSD(model.nh4.1cl2, "Temp", order=TRUE) 

#$Temp
#diff         lwr       upr     p adj
#0-5   0.1685968 -0.3666978 0.7038914 0.7492461
#15-5  1.6201005  1.0848060 2.1553951 0.0000499
#25-5  2.6700276  2.1347330 3.2053222 0.0000011
#15-0  1.4515037  0.9162091 1.9867983 0.0001115
#25-0  2.5014308  1.9661362 3.0367254 0.0000019
#25-15 1.0499271  0.5146325 1.5852217 0.0010735

#anovas...ER min

model.nh4.2a <- lm(NH4 ~ Temp, data = n2o.dinm.er)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nh4.2a)
par(opar)

summary(n2o.dinm.er$NH4)
n2o.dinm.er$nh41 <- n2o.dinm.er$NH4 + 1
n2o.dinm.er$log_nh4 <- log(n2o.dinm.er$nh41)

model.nh4.2al <- lm(log_nh4 ~ Temp, data = n2o.dinm.er)
opar = par (mfrow = c(2,2))
plot(model.nh4.2al)
par(opar)
anova(model.nh4.2al) 

#          Df  Sum Sq Mean Sq F value Pr(>F)  
#Temp       3 1.07544 0.35848  4.0806 0.0496 *
#Residuals  8 0.70281 0.08785

model.nh4.2al2 <-aov(log_nh4 ~ Temp, data = n2o.dinm.er, na.action = na.exclude)
TukeyHSD(model.nh4.2al2, "Temp", order=TRUE) 

#$Temp
#diff         lwr       upr     p adj
#15-5  0.2580330 -0.51695749 1.0330235 0.7180480
#0-5   0.3647387 -0.41025178 1.1397292 0.4763746
#25-5  0.8274263  0.05243574 1.6024168 0.0369071
#0-15  0.1067057 -0.66828480 0.8816962 0.9695636
#25-15 0.5693932 -0.20559727 1.3443837 0.1647548
#25-0  0.4626875 -0.31230298 1.2376780 0.2955546

#anovas...SR min

model.nh4.2b <- lm(NH4 ~ Temp, data = n2o.dinm.sr)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nh4.2b)
par(opar)

summary(n2o.dinm.sr$NH4)
n2o.dinm.sr$nh41 <- n2o.dinm.sr$NH4 + 1
n2o.dinm.sr$log_nh4 <- log(n2o.dinm.sr$nh41)

model.nh4.2bl <- lm(log_nh4 ~ Temp, data = n2o.dinm.sr)
opar = par (mfrow = c(2,2))
plot(model.nh4.2bl)
par(opar)
anova(model.nh4.2bl) #ns

#anovas...GC min

model.nh4.2c <- lm(NH4 ~ Temp, data = n2o.dinm.gc)
#check normality of residuals
opar = par (mfrow = c(2,2))
plot(model.nh4.2c)
par(opar)

summary(n2o.dinm.gc$NH4)

n2o.dinm.gc$log_nh4 <- log(n2o.dinm.gc$NH4)

model.nh4.2cl <- lm(log_nh4 ~ Temp, data = n2o.dinm.gc)
opar = par (mfrow = c(2,2))
plot(model.nh4.2cl)
par(opar)
anova(model.nh4.2cl) #ns


#################################################################

#### not used in 2018 ms sub

#### N2O over time #########################

n2ot <- read.csv("n2otimeRM.csv", header=T)
#aovRBFpq <- aov(DV ~ IV1*IV2 + Error(id/(IV1*IV2)), data=dfRBFpqL)
#summary(aovRBFpq)
n2ot$temp = factor(n2ot$temp)
n2ot$subject = factor(n2ot$subject)
n2ot$time = factor(n2ot$time)

hist(n2ot$N2O1)
n2ot$N2O.2 <- n2ot$N2O1 + 2
n2ot$n2o_log = log(n2ot$N2O.2)
summary(n2ot$n2o_log)
hist(n2ot$n2o_log)

n2o.time <- aov(n2o_log ~ Region*temp*soil.type + Error(subject / (Region*temp*soil.type)), data = n2ot)
# with 27 subjects, error model is singular
n2o.time <- aov(n2o_log ~ Region*temp*soil.type + Error(subject), data = n2ot)
summary(n2o.time)
#Error: subject
#               Df Sum Sq Mean Sq F value   Pr(>F)    
#temp            2  95.64   47.82   9.550  0.00149 ** 
#soil.type       2 282.52  141.26  28.211 2.83e-06 ***
#temp:soil.type  4  18.40    4.60   0.918  0.47465    
#Residuals      18  90.13    5.01                     

#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Within
#                       Df Sum Sq Mean Sq F value   Pr(>F)    
#Region                  2   80.8   40.42  17.720 3.33e-08 ***
#Region:temp             4   20.5    5.11   2.242  0.06326 .  
#Region:soil.type        4   31.9    7.97   3.495  0.00781 ** 
#Region:temp:soil.type   8    7.8    0.97   0.426  0.90548    
#Residuals             603 1375.4    2.28                     

#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#but still no time in the model - is this necessary with subject? or are all the within effects *time?
# also, where are my between interactions - suspect it is b/c I don't have a separate subject for each jar, each time...just for each region, so 3 sets each time

n2o.time.2 <- aov(n2o_log ~ Region*temp*soil.type*time + Error(subject), data = n2ot)
summary(n2o.time.2)
#Error: subject
#               Df Sum Sq Mean Sq F value   Pr(>F)    
#temp            2  95.64   47.82   9.550  0.00149 ** 
#soil.type       2 282.52  141.26  28.211 2.83e-06 ***
#temp:soil.type  4  18.40    4.60   0.918  0.47465    
#Residuals      18  90.13    5.01                     

#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Within
#                            Df Sum Sq Mean Sq F value   Pr(>F)    
#Region                       2   80.8   40.42  38.883 3.35e-16 ***
#time                         7  737.0  105.28 101.284  < 2e-16 ***
#Region:temp                  4   20.5    5.11   4.919 0.000696 ***
#Region:soil.type             4   31.9    7.97   7.670 5.72e-06 ***
#Region:time                 14   99.8    7.13   6.856 1.10e-12 ***
#temp:time                   14   25.7    1.84   1.768 0.041134 *  
#soil.type:time              14   29.3    2.09   2.011 0.015925 *  
#Region:temp:soil.type        8    7.8    0.97   0.935 0.486820    
#Region:temp:time            28    9.8    0.35   0.337 0.999520    
#Region:soil.type:time       28   18.5    0.66   0.635 0.927174    
#temp:soil.type:time         28   17.2    0.61   0.591 0.953478    
#Region:temp:soil.type:time  56    7.8    0.14   0.134 1.000000    
#Residuals                  414  430.3    1.04                     

#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# here, 81 subjects, for each jar for each time
n2ot <- read.csv("n2otimeRM81.csv", header=T)

n2ot$temp = factor(n2ot$temp)
n2ot$subject = factor(n2ot$subject)
n2ot$time = factor(n2ot$time)

hist(n2ot$N2O1)
n2ot$N2O.2 <- n2ot$N2O1 + 2
n2ot$n2o_log = log(n2ot$N2O.2)
summary(n2ot$n2o_log)
hist(n2ot$n2o_log)

n2o.time.2 <- aov(n2o_log ~ Region*temp*soil.type*time + Error(subject), data = n2ot)
summary(n2o.time.2)

#Error: subject
#                      Df Sum Sq Mean Sq F value   Pr(>F)    
#Region                 2   80.8   40.42   6.205  0.00375 ** 
#temp                   2   95.6   47.82   7.342  0.00151 ** 
#soil.type              2  282.5  141.26  21.688 1.22e-07 ***
#Region:temp            4   20.5    5.11   0.785  0.53995    
#Region:soil.type       4   31.9    7.97   1.224  0.31149    
#temp:soil.type         4   18.4    4.60   0.706  0.59125    
#Region:temp:soil.type  8    7.8    0.97   0.149  0.99617    
#Residuals             54  351.7    6.51                     

#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Error: Within
#Df Sum Sq Mean Sq F value   Pr(>F)    
#time                         7  737.0  105.28 235.822  < 2e-16 ***
#Region:time                 14   99.8    7.13  15.964  < 2e-16 ***
#temp:time                   14   25.7    1.84   4.117 1.06e-06 ***
#soil.type:time              14   29.3    2.09   4.682 6.67e-08 ***
#Region:temp:time            28    9.8    0.35   0.784   0.7781    
#Region:soil.type:time       28   18.5    0.66   1.479   0.0580 .  
#temp:soil.type:time         28   17.2    0.61   1.377   0.0991 .  
#Region:temp:soil.type:time  56    7.8    0.14   0.312   1.0000    
#Residuals                  378  168.8    0.45                     

#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

n2ot.R <-aov(n2o_log ~ Region, data = n2ot, na.action = na.exclude)
TukeyHSD(n2ot.R, "Region", order=TRUE)
#$Region
#diff        lwr       upr     p adj
#ER-SR 0.06822515 -0.3220012 0.4584515 0.9112362
#GC-SR 0.78101804  0.3907917 1.1712444 0.0000094
#GC-ER 0.71279289  0.3225666 1.1030192 0.0000609

n2ot.t <-aov(n2o_log ~ temp, data = n2ot, na.action = na.exclude)
TukeyHSD(n2ot.t, "temp", order=TRUE)
#$temp
#diff        lwr       upr     p adj
#15-5  0.6527577  0.2640367 1.0414787 0.0002611
#25-5  0.9133849  0.5246638 1.3021059 0.0000001
#25-15 0.2606272 -0.1280939 0.6493482 0.2572018

n2ot.s <-aov(n2o_log ~ soil.type, data = n2ot, na.action = na.exclude)
TukeyHSD(n2ot.s, "soil.type", order=TRUE)
#$soil.type
#diff       lwr      upr     p adj
#c-m 0.6310541 0.2618664 1.000242 0.0001957
#o-m 1.6051953 1.2360075 1.974383 0.0000000
#o-c 0.9741412 0.6049534 1.343329 0.0000000


## subset to test interactions with time
n2otO <- n2ot[n2ot$soil.type == "o", ]
n2otM <- n2ot[n2ot$soil.type == "m", ]
n2otC <- n2ot[n2ot$soil.type == "c", ]
n2otGC <- n2ot[n2ot$Region == "GC", ]
n2otSR <- n2ot[n2ot$Region == "SR", ]
n2otER <- n2ot[n2ot$Region == "ER", ]
n2ot25 <- n2ot[n2ot$temp == "25", ]
n2ot15 <- n2ot[n2ot$temp == "15", ]
n2ot5 <- n2ot[n2ot$temp == "5", ]


n2otO.T <-aov(n2o_log ~ time, data = n2otO, na.action = na.exclude)
TukeyHSD(n2otO.T, "time", order=TRUE)
n2otM.T <-aov(n2o_log ~ time, data = n2otM, na.action = na.exclude)
TukeyHSD(n2otM.T, "time", order=TRUE)
n2otC.T <-aov(n2o_log ~ time, data = n2otC, na.action = na.exclude)
TukeyHSD(n2otC.T, "time", order=TRUE)
# all soil.types, t1 and t2 differ from each other and other time points (P value<0.04), but the degree of significance varies: mineral soil less difference between t1 & t2 (P=0.02) as compared to org (P<0.0001) or combo (P<0.0001)

n2otGC.T <-aov(n2o_log ~ time, data = n2otGC, na.action = na.exclude)
TukeyHSD(n2otGC.T, "time", order=TRUE)
n2otSR.T <-aov(n2o_log ~ time, data = n2otSR, na.action = na.exclude)
TukeyHSD(n2otSR.T, "time", order=TRUE)
n2otER.T <-aov(n2o_log ~ time, data = n2otER, na.action = na.exclude)
TukeyHSD(n2otER.T, "time", order=TRUE)
# GC and ER regions and SR 2-2, 2-4 & 2-5, t1 and t2 differ from each other and other time points (P value<0.02), but SR 2-1,2-3,2-6,2-7,2-8 ns

n2ot5.T <-aov(n2o_log ~ time, data = n2ot5, na.action = na.exclude)
TukeyHSD(n2ot5.T, "time", order=TRUE)
n2ot15.T <-aov(n2o_log ~ time, data = n2ot15, na.action = na.exclude)
TukeyHSD(n2ot15.T, "time", order=TRUE)
n2ot25.T <-aov(n2o_log ~ time, data = n2ot25, na.action = na.exclude)
TukeyHSD(n2ot25.T, "time", order=TRUE)
# 5oC: t1 and t2 usually differ (P value<0.02, except 3-2 ns)
# 15oC: t1 and t2 all differ (P value<0.02)
# 25oC: t1 and t2 usually differ (P value<0.02, except 6-2, 7-2 and 8-2 ns)




## check the error term - should it include the slope as well as intercept?
n2o.time.3 <- aov(n2o_log ~ Region*temp*soil.type*time + Error(subject/time), data = n2ot)
summary(n2o.time.3)
#same results as model 2

n2o.time.4 <- aov(n2o_log ~ Region*temp*soil.type*time + Error(subject/(Region*temp*soil.type*time)), data = n2ot)
#Warning message:
#  In aov(n2o_log ~ Region * temp * soil.type * time + Error(subject/(Region *  :
#  Error() model is singular

# so stick with model 2


#### think these were done with subject = 3
## also, no time effect
anova(lmer(n2o_log ~ Region*temp*soil.type + (1|subject) + (1|Region:subject) + (1|temp:subject) + (1|soil.type:subject) + (1|Region:temp:subject) + (1|Region:soil.type:subject) + (1|temp:soil.type:subject) + (1|Region:temp:soil.type:subject), n2ot)) 

## multivariate wide format
n2otw <- read.csv("N2O_timewide.csv", header=T)
n2otw$temp = factor(n2otw$temp)
n2otw$subject = factor(n2otw$subject)

### try a reshape
n2ot.2 <- n2ot[,cbind(1:2,4:7)]
n2ot.3 <- n2ot.2[,3:8]
n2ot$time = factor(n2ot$time)
n2otTemp   <- reshape(n2ot.3, v.names="N2O1", timevar="time", idvar=c("Region.1", "temp.1", "soil.type", "subject"), direction="wide")

n2ot.4 <- reshape(n2otTemp, v.names=c("N2O1.1","N2O1.2","N2O1.3","N2O1.4","N2O1.5","N2O1.6","N2O1.7","N2O1.8", "N2O2.1","N2O2.2","N2O2.3","N2O2.4","N2O2.5","N2O2.6","N2O2.7","N2O2.8", "N2O3.1","N2O3.2","N2O3.3","N2O3.4","N2O3.5","N2O3.6","N2O3.7","N2O3.8"), timevar=c("Region.1","temp.1","soil.type"), idvar="subject", direction = "wide")

### didn't work - I think the order of IVs has to be from most fluctuating to least. But main problem is that my DV ([N2O]) is a bunch of NAs when I do the second reshape. Try to play with v.names and just get a bunch of errors

fitn2ot <- lm(cbind(N2O1.1,N2O1.1,N2O1.1,N2O1.1,N2O1.1,N2O1.1,N2O1.1 ))

fitRBFpq   <- lm(cbind(DV.1.1, DV.2.1, DV.1.2, DV.2.2, DV.1.3, DV.2.3) ~ 1,
                 data=dfRBFpqW)
inRBFpq    <- expand.grid(IV1=gl(P, 1), IV2=gl(Q, 1))

#### example data from RExRepos for 2-way RM anova, various methods

### using aov() in long format
set.seed(123)
N    <- 10
P    <- 2
Q    <- 3
muJK <- c(rep(c(1, -2), N), rep(c(2, 0), N), rep(c(3, 3), N))
dfRBFpqL <- data.frame(id =factor(rep(1:N, times=P*Q)),
                       IV1=factor(rep(rep(1:P, each=N), times=Q)),
                       IV2=factor(rep(rep(1:Q, each=N*P))),
                       DV =rnorm(N*P*Q, muJK, 2))


head(dfRBFpqL,25)
aovRBFpq <- aov(DV ~ IV1*IV2 + Error(id/(IV1*IV2)), data=dfRBFpqL)
summary(aovRBFpq)

### reshaping the data to wide format, using anova() in car package
dfTemp   <- reshape(dfRBFpqL, v.names="DV", timevar="IV1",
                    idvar=c("id", "IV2"), direction="wide")
dfRBFpqW <- reshape(dfTemp, v.names=c("DV.1", "DV.2"),
                    timevar="IV2", idvar="id", direction="wide")

library(car)
fitRBFpq   <- lm(cbind(DV.1.1, DV.2.1, DV.1.2, DV.2.2, DV.1.3, DV.2.3) ~ 1,
                 data=dfRBFpqW)
inRBFpq    <- expand.grid(IV1=gl(P, 1), IV2=gl(Q, 1))
AnovaRBFpq <- Anova(fitRBFpq, idata=inRBFpq, idesign=~IV1*IV2)
summary(AnovaRBFpq, multivariate=FALSE, univariate=TRUE)

### using anova.mlm() and mauchly.test() with data in wide format

anova(fitRBFpq, M=~IV1, X=~1, idata=inRBFpq, test="Spherical")
anova(fitRBFpq, M=~IV1 + IV2, X=~IV1, idata=inRBFpq, test="Spherical")
anova(fitRBFpq, M=~IV1 + IV2 + IV1:IV2, X=~IV1 + IV2, idata=inRBFpq, test="Spherical")

##Mauchly-Test for IV1 is unnecessary here since P=2 -> sphericity holds automatically

mauchly.test(fitRBFpq, M=~IV1, X=~1, idata=inRBFpq)
mauchly.test(fitRBFpq, M=~IV1 + IV2, X=~IV1, idata=inRBFpq)
mauchly.test(fitRBFpq, M=~IV1 + IV2 + IV1:IV2, X=~IV1 + IV2, idata=inRBFpq)

##effect size estimates: generalized n2g
##simple effects, separate error terms

summary(aov(DV ~ IV1 + Error(id/IV1), data=dfRBFpqL, subset=(IV2==1)))
summary(aov(DV ~ IV1 + Error(id/IV1), data=dfRBFpqL, subset=(IV2==2)))
summary(aov(DV ~ IV1 + Error(id/IV1), data=dfRBFpqL, subset=(IV2==3)))

#### multivariate approach
library(car)
summary(AnovaRBFpq, multivariate=TRUE, univariate=FALSE)

