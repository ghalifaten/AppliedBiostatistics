#installing and importing packages
#install.packages("xtable")
library(xtable)

#importing data
file <- "data/pillar_data.dat"
mydata <- read.csv(file, sep = '', header = FALSE)
names(mydata) <- c("Pillar.ID",
                 "Mine.Seam",
                 "(1)",
                 "(2)",
                 "(3)",
                 "(4)",
                 "(5)",
                 "(6)",
                 "(7)",
                 "(8)",
                 "(9)",
                 "(10)")

#balance of samples
prop.table(table(mydata$Pillar.Stability))

#numerical_univariate_eda
eda.nu <- summary(mydata[, c(3:12)])
xtable(eda.nu)

#graphical_univariate_eda
par(mfrow=c(3,3))
plot(density(mydata$Pillar.Depth), main="Pillar.Depth", xlab="")
plot(density(mydata$Pillar.Height), main="Pillar.Height")
plot(density(mydata$Pillar.Width), main="Pillar.Width")
plot(density(mydata$`Width/Height`), main="Width/Height")
plot(density(mydata$Roadway.width), main="Roadway.width")
plot(density(mydata$Uniaxial_compression.strength), main="Uniaxial_compression.strength")
plot(density(mydata$Pillar.Strength), main="Pillar.Strength")
plot(density(mydata$Pillar.Stress), main="Pillar.Stress")
plot(density(mydata$`Strength/Stress`), main="Strength/Stress")

#numcerical_bivariate_eda

#graphical_bivariate_eda
mydata$Pillar.Stability <- factor(mydata$Pillar.Stability)

par(mfrow=c(3,3))
cdplot(Pillar.Stability ~ Pillar.Depth, data = mydata)
cdplot(Pillar.Stability ~ Pillar.Width, data = mydata)
cdplot(Pillar.Stability ~ Pillar.Height, data = mydata)
cdplot(Pillar.Stability ~ `Width/Height`, data = mydata)
cdplot(Pillar.Stability ~ Roadway.width, data = mydata)
cdplot(Pillar.Stability ~ Uniaxial_compression.strength, data = mydata)
cdplot(Pillar.Stability ~ Pillar.Strength, data = mydata)
cdplot(Pillar.Stability ~ Pillar.Stress, data = mydata)
cdplot(Pillar.Stability ~ `Strength/Stress`, data = mydata)


#fitting the model - 2 variables
mylogit <- glm(Pillar.Stability ~
                               `Width/Height` +
                               `Strength/Stress`, data = mydata, family = "binomial")

summary(mylogit)

#Goodness of fit
1 - pchisq(q=mylogit$null.deviance - mylogit$deviance, df=length(coef(mylogit)))
#The resulting p-value is so small very close to 0, so we can reject 
#the null hypothesis that neither variables are related to y.

drop1(mylogit, test='Chisq')
#Both rows for the independent variables are highly significant, suggesting that 
#each value, by itself, has a relationship with the response variable 
#(which we know is correct).