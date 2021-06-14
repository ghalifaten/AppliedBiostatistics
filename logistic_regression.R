#installing and importing packages
#install.packages("xtable")
#install.packages("car")
library(xtable)
library(car)

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
names(mydata) <- c("Pillar.ID",
                   "Mine.Seam",
                   "Pillar.Depth",
                   "Pillar.Height",
                   "Pillar.Width",
                   "Width/Height",
                   "Roadway.width",
                   "Uniaxial_compression.strength",
                   "Pillar.Strength",
                   "Pillar.Stress",
                   "Strength/Stress",
                   "Pillar.Stability")

par(mfrow=c(3,3))
plot(density(mydata$Pillar.Depth), main="(1)", xlab="")
plot(density(mydata$Pillar.Height), main="(2)", xlab="", ylab="")
plot(density(mydata$Pillar.Width), main="(3)", xlab="", ylab="")
plot(density(mydata$`Width/Height`), main="(4)", xlab="")
plot(density(mydata$Roadway.width), main="(5)", xlab="", ylab="")
plot(density(mydata$Uniaxial_compression.strength), main="(6)", xlab="", ylab="")
plot(density(mydata$Pillar.Strength), main="(7)", xlab="")
plot(density(mydata$Pillar.Stress), main="(8)", xlab="", ylab="")
plot(density(mydata$`Strength/Stress`), main="(9)", xlab="", ylab="")

#---No outliers
par(mfrow=c(1,2))
boxplot(mydata$`Width/Height`, main="(4)")
boxplot(mydata$`Strength/Stress`, main="(9)")

#numerical_bivariate_eda
eda.nb <- cor(mydata[, c(3:11)])
xtable(eda.nb)


#graphical_bivariate_eda
mydata$Pillar.Stability <- factor(mydata$Pillar.Stability)

par(mfrow=c(3,3))
cdplot(Pillar.Stability ~ Pillar.Depth, data = mydata, main="(1)")
cdplot(Pillar.Stability ~ Pillar.Width, data = mydata, main="(2)", ylab = "")
cdplot(Pillar.Stability ~ Pillar.Height, data = mydata, main = "(3)", ylab = "")
cdplot(Pillar.Stability ~ `Width/Height`, data = mydata, main ="(4)")
cdplot(Pillar.Stability ~ Roadway.width, data = mydata, main ="(5)", ylab = "")
cdplot(Pillar.Stability ~ Uniaxial_compression.strength, data = mydata, main="(6)", ylab = "")
cdplot(Pillar.Stability ~ Pillar.Strength, data = mydata, main ="(7)")
cdplot(Pillar.Stability ~ Pillar.Stress, data = mydata, main="(8)", ylab = "")
cdplot(Pillar.Stability ~ `Strength/Stress`, data = mydata, main ="(9)", ylab = "")


#fitting the model - 2 variables
mylogit <- glm(Pillar.Stability ~
                               `Width/Height` +
                               `Strength/Stress`, data = mydata, family = "binomial")

summary(mylogit)

#Checking for multicollinearity
vif(mylogit)

#Goodness of fit
1 - pchisq(q=mylogit$null.deviance - mylogit$deviance, df=length(coef(mylogit)))
#The resulting p-value is so small very close to 0, so we can reject 
#the null hypothesis that neither variables are related to y.

drop1(mylogit, test='Chisq')
#Both rows for the independent variables are highly significant, suggesting that 
#each value, by itself, has a relationship with the response variable 
#(which we know is correct).