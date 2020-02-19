
rm(list=ls())

library(jagsUI)
library(bayesplot)
library(ggplot2)

hatch_temp <- read.csv("data/hatch_temp.csv")

y <- hatch_temp$success_rate + 0.0001
arcsin <- asin(sqrt(y/100)) 



MCMC.params <- list(n.samples = 150000,
                    n.burnin = 50000,
                    n.thin = 5,
                    n.chains = 3, 
                    model.file = "models/Model_vonB.txt")

jags.data.1 <- list(y = arcsin,
                  x = hatch_temp$d20d40temp - min(hatch_temp$d20d40temp),
                  n = nrow(hatch_temp))

parameters <- c("a", "b", "sd")
jm.out.1 <- jags(data = jags.data.1,
               #inits = inits,
               parameters.to.save= parameters,
               model.file = MCMC.params$model.file,
               n.chains = MCMC.params$n.chains,
               n.burnin = MCMC.params$n.burnin,
               n.thin = MCMC.params$n.thin,
               n.iter = MCMC.params$n.samples,
               DIC = T, 
               parallel=T)


mcmc_dens(jm.out.1$samples, c("a", "b",  "sd"))

xvals <- seq(from = 0, to = max(jags.data.1$x), by = 0.01)
fit <- jm.out.1$mean$a * (1 - exp(-jm.out.1$mean$b * xvals))
fit.2.5 <- jm.out.1$q2.5$a * (1 - exp(-jm.out.1$q2.5$b * xvals))
fit.97.5 <- jm.out.1$q97.5$a * (1 - exp(-jm.out.1$q97.5$b * xvals))

df1 <- data.frame(x = jags.data.1$x,
                  y = jags.data.1$y)
df2 <- data.frame(x = xvals,
                  mean = fit,
                  low = fit.2.5,
                  high = fit.97.5)

p1 <- ggplot()    +
  geom_ribbon(data = df2,
              aes(x = x, ymin = low, ymax = high),
              fill = "coral", alpha = 0.7)+
  geom_path(data = df2, 
            aes(x = x, y = mean), color = "red",
            size = 1) +
  geom_point(data = df1, 
             aes(x = x, y = y))

p1


# or... fit a logistic function:

MCMC.params <- list(n.samples = 150000,
                    n.burnin = 50000,
                    n.thin = 5,
                    n.chains = 3, 
                    model.file = "models/Model_logistic.txt")

jags.data.2 <- list(y = hatch_temp$success_rate/100,
                  x = hatch_temp$d20d40temp - mean(hatch_temp$d20d40temp),
                  n = nrow(hatch_temp))

parameters <- c("a", "sd")
jm.out.2 <- jags(data = jags.data.2,
               #inits = inits,
               parameters.to.save= parameters,
               model.file = MCMC.params$model.file,
               n.chains = MCMC.params$n.chains,
               n.burnin = MCMC.params$n.burnin,
               n.thin = MCMC.params$n.thin,
               n.iter = MCMC.params$n.samples,
               DIC = T, 
               parallel=T)


mcmc_dens(jm.out.2$samples, c("a", "sd"))


xvals <- seq(from = min(jags.data.2$x), to = max(jags.data.2$x), by = 0.01)
fit <- (1 + exp(-xvals))^(- jm.out.2$mean$a)
fit.2.5 <- (1 + exp(-xvals))^(- jm.out.2$q2.5$a)
fit.97.5 <- (1 + exp(-xvals))^(- jm.out.2$q97.5$a)

df1 <- data.frame(x = jags.data.2$x,
                  y = jags.data.2$y)
df2 <- data.frame(x = xvals,
                  mean = fit,
                  low = fit.2.5,
                  high = fit.97.5)

p2 <- ggplot()    +
  geom_ribbon(data = df2,
              aes(x = x, ymin = low, ymax = high),
              fill = "coral", alpha = 0.7)+
  geom_path(data = df2, 
            aes(x = x, y = mean), color = "red",
            size = 1) +
  geom_point(data = df1, 
             aes(x = x, y = y))

p2


# somehow, the following is not working... no time! 2/18/2020
# or... fit a logistic function:
# 
# MCMC.params <- list(n.samples = 150000,
#                     n.burnin = 50000,
#                     n.thin = 5,
#                     n.chains = 3,
#                     model.file = "models/Model_logistic2.txt")
# 
# jags.data.3 <- list(y = hatch_temp$success_rate/100,
#                     x = hatch_temp$d20d40temp,
#                     n = nrow(hatch_temp))
# 
# parameters <- c("a", "b", "c", "d", "e", "sd")
# jm.out.3 <- jags(data = jags.data.3,
#                  #inits = inits,
#                  parameters.to.save= parameters,
#                  model.file = MCMC.params$model.file,
#                  n.chains = MCMC.params$n.chains,
#                  n.burnin = MCMC.params$n.burnin,
#                  n.thin = MCMC.params$n.thin,
#                  n.iter = MCMC.params$n.samples,
#                  DIC = T,
#                  parallel=T)
# 
# 
# mcmc_dens(jm.out.3$samples, c("a", "b", "c", "d", "e", "sd"))
# 
# 
# xvals <- seq(from = min(jags.data.3$x), to = max(jags.data.3$x), by = 0.01)
# 
# a <- jm.out.3$mean$a
# b <- jm.out.3$mean$b
# c <- jm.out.3$mean$c
# d <- jm.out.3$mean$d
# e <- jm.out.3$mean$e
# fit <- a + (b - a) /((1 + c * exp(-d * xvals))^(1/e))
# 
# fit <- jm.out.3$mean$a * (1 - (xvals/jm.out.3$mean$b) ^ jm.out.3$mean$c)
# 
# fit.2.5 <- jm.out.3$q2.5$a * (1 - (xvals/jm.out.3$q2.5$b) ^ jm.out.3$q2.5$c)
# fit.97.5 <- jm.out.3$q97.5$a * (1 - (xvals/jm.out.3$q97.5$b) ^ jm.out.3$q97.5$c)
# 
# df1 <- data.frame(x = jags.data.3$x,
#                   y = jags.data.3$y)
# df2 <- data.frame(x = xvals,
#                   mean = fit,
#                   low = fit.2.5,
#                   high = fit.97.5)
# 
# p3 <- ggplot()    +
#   geom_ribbon(data = df2,
#               aes(x = x, ymin = low, ymax = high),
#               fill = "coral", alpha = 0.7)+
#   geom_path(data = df2,
#             aes(x = x, y = mean), color = "red",
#             size = 1) +
#   geom_point(data = df1,
#              aes(x = x, y = y))
# 
# p3
