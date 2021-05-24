#importing data
file <- "report_data/pillar_data.dat"
mydata <- read.csv(file, sep = '', header = FALSE)
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

#balance of samples
prop.table(table(mydata$Pillar.Stability))

#relationship of variables
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

#fitting the model
mylogit <- glm(Pillar.Stability ~
                               `Width/Height` +
                               `Strength/Stress`, data = mydata, family = "binomial")

summary(mylogit)

#Goodness of fit
1 - pchisq(q=mylogit$null.deviance - mylogit$deviance, df=length(coef(mylogit)))
#The resulting p-value is so small it’s very close to 0, so we can reject 
#the null hypothesis that neither variables are related to y.

drop1(mylogit, test='Chisq')
#Both rows for the independent variables are highly significant, suggesting that 
#each value, by itself, has a relationship with the response variable 
#(which we know is correct).