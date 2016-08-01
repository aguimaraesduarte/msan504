n <- 14000
simulation <- data.frame(unif = runif(n, 0, 1)) # U~unif(0, 1)
simulation$Y <- rcauchy(n) # Y~Cauchy
simulation$auxilliary <- dcauchy(simulation$Y) # g(Y)
simulation$target <- dnorm(simulation$Y, 0, 1) # f(Y)

c <- sqrt(pi/2)

simulation$accept <- ifelse(simulation$unif < simulation$target/(c*simulation$auxilliary),
                            T, F) # accept if U<f/(c*g)

hist(simulation$Y[simulation$accept], freq=F, ylim=c(0, 0.55),breaks=75, xlim=c(-4, 4),
     main="Histogram of realizations generated
     for the Cauchy random variable", xlab="x")
curve(dnorm(x, 0, 1), -4, 4, add=T, col="red")
legend(1, 0.5, "N(0, 1)", lty=1, col="red")

writeLines(paste(sum(simulation$accept), "realizations of the standard normal distribution
out of", n, "realizations of the Cauchy distribution."))