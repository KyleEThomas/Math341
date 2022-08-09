
age <- read.csv(file = "/Users/kylethomas/Desktop/CSUN/2021/Spring 2021/Math 341/Data Files/ageguesses.csv", head = TRUE)
#/Users/kylethomas/Desktop/CSUN/2021/Spring 2021/Math 341/Data Files/ageguesses.csv

guess <- (age$guesses)

iscamdotplot(guess)

mean(guess)

sd(guess)
plot(sd(guess))

boxplot(guess, horizontal = T, main = "Guesses Box Plot")
quantile(guess)



movies <- read.csv(file = "/Users/kylethomas/Desktop/CSUN/2021/Spring 2021/Math 341/Data Files/HollywoodMovies.csv", head = TRUE)

rot <- c(movies$Rot)
audience <- c(movies$AudienceScore)

iscamdotplot(rot)
iscamdotplot(audience)

boxplot(rot, horizontal = T, main = "Rotten Tomatoes Box Plot")
boxplot(audience, horizontal = T, main = "Audience Box Plot")

quantile(rot)
quantile(audience)


midterm <- c(55, 50, 71, 72, 81, 94,96,99,67)
final <- c(82, 66, 78, 34, 47, 85, 99, 99, 68)

plot(midterm, final, main="Midterm vs. Finals Scores", 
     xlab="midterm",ylab="final", pch=19)

cor(final,midterm)

abline(lm(midterm ~ final))
lm(midterm ~ final)
abline(85~80)










