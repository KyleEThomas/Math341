countries <- read.csv(file = "~/Desktop/AllCountries.csv", head = TRUE)
x <- sample(seq(1,nrow(countries)), 5, replace =FALSE)
print(countries[x,1])

hondaData <- read.csv(file = "~/Desktop/UsedHondaCivics2015.csv", head = TRUE)
summary(hondaData)

f <- table(hondaData$year,hondaData$type)



rf = f / 22

f

rf

slices<-(1)
lbls<-c("Civic")
pct<- round(slices/sum(slices)*100)
lbls <- paste(lbls,pct)
lbls <- pase(lbls,"%", sep="")
pie(slices,labels = lbls, col = rainbow(length(lbls)),main="Pie Chart of Models")


# Pi Chart
# Years
tableYear = table(hondaData$year)
slices<-(tableYear)
lab <- c(2006, 2008, 2009, 2011, 2012, 2013, 2014, 2015)
colors <- c("red", "orange", "yellow", "green", "blue", "cyan", "purple", "pink")
percent <- round(slices/sum(slices)*100)
lab <- paste(percent)
lab <- paste(lab, "%", sep="")
pie(slices, labels = lab, col = colors,main="Pie for Years")
legend("topright",c("2006", "2008", "2009", "2011", "2012", "2013", "2014", "2015"), cex=0.8, fill=colors)

# Pi Chart
# Type
tableType <- table(hondaData$type)
block <- c(tableType)
colors <- c("red", "blue", "yellow", "green")
percent <- round(block/sum(block)*100)
typeLab <- paste(percent)
typeLab <- paste(typeLab, "%", sep=" ")
pie(block, main="Type Pie Chart", col=colors, labels=typeLab)
legend("topright", c("EX", "Hybrid", "LX", "Si"),cex=0.8, fill = colors)



# Bar Graph
# Year
# tableYear = table(hondaData$year)s
lab <- c(2006, 2008, 2009, 2011, 2012, 2013, 2014, 2015)
block <- (tableYear)
colors <- c("red", "orange", "yellow", "green", "blue", "cyan", "purple", "pink")
barplot(block,main="Years Bar Graph",ylab="frequency",xlab="year",beside=TRUE,col=colors)

# Bar Graph
# Type
tableType <- table(hondaData$type)
block <- c(tableType)
class <- c(1,12,7,2)
colors <- c("red", "blue", "yellow", "green")
barplot(block,main="Type Bar Graph", ylab="frequency",xlab="Type",beside=TRUE,col=colors)



