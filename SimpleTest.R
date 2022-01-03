########################################################################
# Опыты с решением ОДУ на R
########################################################################

library(deSolve)
library(ggplot2)


##############################################################
# Пример одного ДУ
##############################################################


f_ode <- function(t, x, parms){
  return(list(x))
}

f_ode(1, 2, NULL)
times <- seq(0, 5, 1)


sol <- ode(y = 1, times = times, func = f_ode, parms = list())

ggplot() +
  geom_point(aes(x = sol[, 1], y = sol[, 2]), col = "blue") +
  geom_point(aes(x = sol[, 1], y = exp(sol[, 1])), col = "red")


##############################################################
# Пример системы ДУ
##############################################################

a <- 1
b <- 2
A <- 1000

f_ode2 <- function(t, x, parms){
  return(list(c(a * (A - x[1] - x[2]), b * (A - x[1] - x[2]))))
}

x0 <- 100
y0 <- 150
t0 <- 0

times2 <- seq(0, 5, 0.1)

sol2 <- ode(y = c(x0, y0), times = times2, func = f_ode2, parms = list())

sol_f <- function(t){
  return(list(x = ((x0 - A)*(b + a*exp(-(a + b)*(t - t0))) + y0*(-a + a*exp(-(a + b)*(t - t0))))/(a + b) + A, 
              y = ((x0 - A)*(-b + b*exp(-(a + b)*(t - t0))) + y0*(a + b*exp(-(a + b)*(t - t0))))/(a + b)))
}

sol_f(times2)


ggplot() +
  geom_point(aes(x = sol2[, 1], y = sol2[, 2]), col = "blue") +
  geom_point(aes(x = sol2[, 1], y = sol_f(times2)$x, col = "red"))


ggplot() +
  geom_point(aes(x = sol2[, 1], y = sol2[, 3]), col = "blue") +
  geom_point(aes(x = sol2[, 1], y = sol_f(times2)$y, col = "red"))


