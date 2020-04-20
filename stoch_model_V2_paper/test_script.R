# xdata <- seq(1,t_period)
# ydata <- (case_data_Ezhou)
# ydata[ydata == -Inf ] <- 0
# # print(ydata)
# fit1 <- lm(ydata~xdata)
# slope <- coef(fit1)[2]
# slope
# 
# 
# data <- cbind(xdata,ydata)
# data <- as.data.frame(data)
# colnames(data) <- c("dates","inf")

# library(pomp)
# pomp(
#   data=data,
#   times="dates",t0=0,
#   skeleton=vectorfield(
#     Csnippet("
#       DS = -Beta*S*I/N;
#       DI = Beta*S*I/N-gamma*I;
#       DR = gamma*I;")),
#   rinit=Csnippet("
#       S = S_0;
#       I = I_0;
#       R = N-S_0-I_0;"),
#   statenames=c("S","I","R"),
#   paramnames=c("Beta","gamma","N","S_0","I_0")) -> niameyA
# 
# 
# sse <- function (params) {
#   x <- trajectory(niameyA,params=params)
#   discrep <- x["I",,]-obs(niameyA)
#   sum(discrep^2)
# }
# 
# f1 <- function (beta) {
#     params <- c(Beta=beta,gamma=1,N=1077717,S_0=1077716,I_0=1)
#   sse(params)
# }
# 
# beta <- seq(from=5,to=40,by=0.5)
# SSE <- sapply(beta,f1)
# 
# beta.hat <- beta[which.min(SSE)]
# plot(beta,SSE,type='l')
# abline(v=beta.hat,lty=2)
# 
# coef(niameyA) <- c(Beta=beta.hat,gamma=1,N=1077717,S_0=1077716,I_0=1)
# x <- trajectory(niameyA,format="data.frame")
# ggplot(data=join(as.data.frame(niameyA),x,by='dates'),
#        mapping=aes(x=dates))+
#   geom_line(aes(y=inf),color='black')+
#   geom_line(aes(y=I),color='red')


require(deSolve)
sir <- function(t,x,parms){
  S <- x[1]
  I <- x[2]
  R <- x[3]
  with(as.list(parms),
       {
         dS <- -beta*S*I
         dI <- beta*S*I - nu*I
         dR <- nu*I
         res <- c(dS,dI,dR)
         list(res)
       })
}

N <- 22454888#theta[["pop_travel"]]
parms <- c(N=N,beta=0.001, nu = 1/4.6)
times <- seq(0,30,0.1)
x0 <- c(N,1,0)
stateMatrix <- ode(y=x0, times, sir, parms)
colnames(stateMatrix) <- c("time","S","I","R")
plot(stateMatrix[,"time"], stateMatrix[,"S"], type="l", lwd=2, 
     xlab="Time", ylab="Population Size")
lines(stateMatrix[,"time"], stateMatrix[,"I"], col="red", lwd=2)
lines(stateMatrix[,"time"], stateMatrix[,"R"], col="green", lwd=2)
legend("right", c("S","I","R"), col=c("black","red","green"), lwd=2)


cumdata <- cumsum(case_data_Ezhou[12:length(case_data_Ezhou)])
days <- 1:length(cumdata)
plot(days, cumdata, pch=16, xlab="Days", ylab="Cumulative Incidences")

require(bbmle)
# likelihood function
sirLL <- function(lbeta, lnu, logN, logI0) {
  parms <- c(beta=plogis(lbeta), nu=plogis(lnu))
  x0 <- c(S=exp(logN), I=exp(logI0), R=0)
  out <- ode(y=x0, days, sir, parms)
  SD <- sqrt(sum( (cumdata-(out[,3] + out[,4]))^2)/length(days) )
  -sum(dnorm(cumdata, mean=(out[,3] + out[,4]), sd=SD, log=TRUE))
}
# minimize negative-log-likelihood
fit <- mle2(sirLL, 
            start=list(lbeta=qlogis(1e-3), 
                       lnu=qlogis(.2)),
            
            fixed =list(logN=log(N), logI0=log(12) ),  
            method="Nelder-Mead",
            control=list(maxit=1E5,trace=2),
            trace=FALSE)

summary(fit)



parms <- c(beta=theta2[1], nu = theta2[2])
times <- seq(0,30,0.1)
x0 <- c(N,12,0)
stateMatrix2 <- ode(y=x0, times, sir, parms)
colnames(stateMatrix2) <- c("time","S","I","R")
plot(stateMatrix2[,"time"], stateMatrix2[,"R"] + stateMatrix2[,"I"], type="l", lwd=2, 
     xaxs="i", xlab="Time", ylab="Cumulative Deaths")
# lines(stateMatrix1[,"time"], stateMatrix1[,"R"], col=grey(0.85), lwd=2)
# points(days, cumdata, pch=16, col="red")
legend("topleft", c("Poisson", "Gaussian"), lwd=2, col=c("black",grey(0.85)))