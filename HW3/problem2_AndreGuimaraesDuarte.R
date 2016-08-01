alpha <- 2
beta <- 1.5
lambda <- 1/beta

n <- 15000
simulation <- data.frame(unif = runif(n, 0, 1)) # U~unif(0, 1)
simulation$Y <- rexp(n, lambda) # Y~Exp(1/beta)
simulation$auxilliary <- dexp(simulation$Y, lambda) # g(Y)
simulation$target <- dgamma(simulation$Y, alpha, beta) # f(Y)

c <- 1.3601

simulation$accept <- ifelse(simulation$unif < simulation$target/(c*simulation$auxilliary),
                            T, F) # accept if U<f/(c*g)

hist(simulation$Y[simulation$accept], freq=F, breaks=75, ylim=c(0, 0.55), xlim=c(0, 8),
     main="Histogram of realizations generated
     for the Gamma random variable", xlab="x")
curve(dgamma(x, alpha, beta), 0, 8, add=T, col="red")
legend(3, 0.5, paste("Gamma(", alpha, ",", beta, ")"), lty=1, col="red")

writeLines(paste(sum(simulation$accept), "realizations of the Gamma distribution
out of", n, "realizations of the exponential distribution."))
