rm()
setwd("E:/ASD_Nepal_article")
brj2<-read.csv("brinjal_2017.csv")
attach(brj2)
names(brj2)
library(lmerTest)
library(lme4)
library(nlme)
library(agricolae)
shapiro.test(Yield..mt.ha.)
shapiro.test(Wilt.incidence)
shapiro.test(Number.of.fruit.per.plot)
shapiro.test(Weed.fresh.mt.ha)
model2<-aov(Yield..mt.ha.~Rep+Treatments)
model2
comp1<-HSD.test(model2,"Treatments")
comp1

model3<-aov(Yield..mt.ha.~Rep+Treatments)
model3
comp2<-HSD.test(model2,"Treatments")
comp2

model4<-aov(asin(sqrt(Wilt.incidence/100))~Rep+Treatments, na.rm= "T")
model4
comp4<-HSD.test(model4,"Treatments")
comp4

model5<-aov(Wilt.incidence~Rep+Treatments)
model5
comp5<-HSD.test(model5,"Treatments")
comp5

model6<-aov(Weed.fresh.mt.ha~Rep+Treatments)
model6
comp6<-HSD.test(model6,"Treatments")
comp6
