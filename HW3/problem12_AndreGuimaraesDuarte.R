myData <- read.csv("MotherEducationBirthWeight.csv", header=T)
plot(myData$BIRTHWEIGHT~myData$YEARSEDUC,
     main="Baby birth weight as a function of mother's years of education",
     xlab="Mother's years of education",
     ylab="Baby's weight (g)")

rho_hat_0 <- cor(myData$YEARSEDUC, myData$BIRTHWEIGHT)

rhos <- c()
for(i in 1:25000){
  myData[1] <- myData[1][sample(1:nrow(myData)),]
  rhos[i] <- cor(myData$YEARSEDUC, myData$BIRTHWEIGHT)
}

hist(rhos, main=expression(paste("Histogram of ", hat(rho))),
     xlab=expression(hat(rho)))
abline(v = rho_hat_0, col="red")
text(rho_hat_0*1.5, 7000, bquote(hat(rho)[0] == .(round(rho_hat_0, 3))))

(p_empirical <- sum(rhos > rho_hat_0)/length(rhos))
# if two-sided, multiply by 2