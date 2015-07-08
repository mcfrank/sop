
rm(list=ls())
source("~/Projects/R/Ranalysis/useful_dplyr.R")
d <- read.csv("~/Projects/sop/data/data.csv")

ggplot(d, aes(x = age, y = rt)) + 
  geom_point(aes(size = n, col = study)) + 
  geom_linerange(aes(ymin = rt - cih, 
                     ymax = rt + cil, 
                     col = study)) +
  geom_smooth(method="lm", formula=y ~ log(x)) + 
  ylim(c(0,2))

## same deal with weighted means

d.kid <- d %>% 
  filter(age < 5)
d.adult <- d %>% 
  filter(age > 20) %>% 
  summarise(rt = mean(rt))

ggplot(d.kid, aes(x = age, y = rt)) + 
  geom_point(aes(size = n, col = study), 
             position=position_dodge(width=.05)) + 
  geom_linerange(aes(ymin = rt - cih, 
                     ymax = rt + cil, 
                     col = study), 
                 position=position_dodge(width=.05)) +
  geom_smooth(method="lm", formula=y ~ log(x), col="black", lty=3) + 
  geom_hline(aes(yintercept=d.adult$rt), lty=2) +  
  ylim(c(0,2)) + 
  xlim(c(0,5)) + 
  xlab("Age (years)") + 
  ylab("Reaction Time (s)")


### KAIL ANALYSIS --------
# m_i = a + b (exp -ci)
# where m is the predicted RT
#       a is the adult value
#       i is age
#       b is a multiplier
#       c is a decay parameter

a = d.adult$rt

fn <- function(x, a) {
  a = .55
  b = x[1]
  c = x[2]
  df <- d.kid
  df$pred <- a + b * exp(-c * df$age)
  err <- mean((df$rt - df$pred)^2)
  return(err)
}

opt <- optim(c(1,1.5), fn, a)

d.kid$pred <- a + opt$par[1] * exp(-opt$par[2] * d.kid$age)

xs <- seq(0,5,.1)
d.pred <- data.frame(x = xs,
                     y = a + opt$par[1] * exp(-opt$par[2] * xs))

ggplot(d.kid, aes(x = age, y = rt)) + 
  geom_point(aes(size = n, col = study), 
             position=position_dodge(width=.05)) + 
  geom_linerange(aes(ymin = rt - cih, 
                     ymax = rt + cil, 
                     col = study), 
                 position=position_dodge(width=.05)) +
  geom_hline(aes(yintercept=d.adult$rt), lty=2) +  
  geom_line(data=d.pred, aes(x = x, y = y), col="black", lty=3) +
  ylim(c(0,2)) + 
  xlim(c(0,5)) + 
  xlab("Age (years)") + 
  ylab("Reaction Time (s)")

