#Code for Nathan's Famous Hot Dog Eating Contest
hotdogs<-read.csv(paste("https://docs.google.com/spreadsheets/d/e/2PACX-1vRVF",
                  "F8zXttXwLKISNuv_0RoW4AfwsWLNxjINK5jV7XoRdqamzxZwY_XfzR32SzS",
                  "vwtRElDQtrfP2QZw/pub?output=csv",sep=""),header=T)

year<-hotdogs$Year
number<-hotdogs$HotDogs

plot(year,number,pch=16)
abline(lm(number~year), col ="red")


mean <- mean(number)
sd <- sd(number)

count <- 0
for (val in number) {
  if(val > mean)   
    count = count= count + 1
}
print(count)

cor(number, year)

coef(lm(number~year))

paste('y =', coef(lm(number~year))[[2]], '* x', '+', coef(lm(number~year))[[1]])








