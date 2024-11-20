CHOCOLATE=read.csv(file.choose(), header=T)
#hypothesis testing#
t.test(CHOCOLATE$Boredom, mu=0.7, alternative = "greater" )

#average ratings of each flavour amongst 60 respondants#
Flavour1=mean(CHOCOLATE$Milk)
Flavour2=mean(CHOCOLATE$White)
Flavour3=mean(CHOCOLATE$Dark)
Flavour4=mean(CHOCOLATE$Matcha)
Flavour5=mean(CHOCOLATE$Hazel)
Flavours= c(Flavour1,Flavour2,Flavour3,Flavour4,Flavour5)
plot(Flavours)

#age range of consumers#
t.test(CHOCOLATE$Age, conf.level = 0.95)

#regression analysis#
SALES=read.csv(file.choose(), header = T)
View(SALES)
library(e1071)
skewness(SALES$Sales)
skewness(SALES$Number.of.flavors)
skewness(SALES$Price)
skewness(SALES$Instagram)
skewness(SALES$Facebook)
skewness(SALES$YouTube)

Reg.sales= lm(Sales~ Number.of.flavors + Price + Instagram + Facebook + YouTube , data = SALES)
summary(Reg.sales)
