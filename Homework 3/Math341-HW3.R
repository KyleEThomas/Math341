
hondaData <- read.csv(file = "~/Desktop/CSUN/2021/Math 341/Data Files/UsedHondaCivics2015.csv", head = TRUE)


mileage <-(hondaData$mileage)

mean(mileage)

sd(mileage)

quantile(mileage)


iscamdotplot(mileage)

boxplot(mileage, horizontal = T, main = "Mileage Box Plot")


age <-(hondaData$age)

mean(age)

sd(age)

quantile(age)


iscamdotplot(age)

boxplot(age, horizontal = T, main = "Age Box Plot")



mammalsData <- read.csv(file = "~/Desktop/CSUN/2021/Math 341/Data Files/mammals.csv", head = T)


longevity <- (mammalsData$Longevity)

mean(longevity)

sd(longevity)

quantile(longevity)


iscamdotplot(longevity)

boxplot(longevity, horizontal = T, main = "Longevity Box Plot")


gestation <- (mammalsData$Gestation)

mean(gestation)

sd(gestation)

quantile(gestation)


iscamdotplot(gestation)

boxplot(gestation, horizontal = T, main = "Gestation Box Plot")

