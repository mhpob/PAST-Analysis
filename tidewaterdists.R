library(ggplot2);library(dplyr)


norm <- data.frame(dist = seq(0.1, 20, length.out = 1000))
norm$freq <- norm$dist^-3
norm$freq1 <- norm$freq/sum(norm$freq)

exp <- data.frame(dist = seq(0.1, 20, length.out = 1000))
exp$freq <- 0.6*exp(-0.6*exp$dist)
exp$freq1 <- exp$freq/sum(exp$freq)

exp <- data.frame(dist = seq(0.1, 20, length.out = 1000))
exp$freq1 <- pexp(exp$dist, rate = 0.6, lower.tail = F)

norm2 <- data.frame(dist = seq(0.5, , length.out = 1000))
norm2$freq1 <- pnorm(norm2$dist, sd = 0.4,lower.tail = F)


pl <- data.frame(dist = seq(0.1, 20, length.out = 1000))
pl$freq <- pl$dist^-1.1
pl$freq1 <- pl$freq/sum(pl$freq)


pl.01 <- data.frame(dist = seq(0.5, 20, length.out = 10000))
pl.01$freq <- pl.01$dist^-1.01
pl.01$freq1 <- pl.01$freq/sum(pl.01$freq)

ggplot()+
  geom_line(data = exp, aes(x = dist, y = freq1), color = 'red')
    # geom_line(data = norm2, aes(x = dist, y = freq1), color = 'purple')+
  # geom_line(data = pl2, aes(x = dist, y = freq1), color = 'green')+
  geom_line(data = pl.01, aes(x = dist, y = freq1), color = 'blue')+
  labs(x = 'Distance Traveled', y = 'Frequency')+theme_bw()

# ggplot()+geom_density(data = data.frame(norm), aes(x = norm))+
#   labs(x = 'Distance Traveled', y = 'Frequency')+theme_bw()


ggplot() +
  geom_line(data = exp, aes(x = dist, y = freq1), col = 'blue', lwd = 1) +
  geom_line(data = pl, aes(x = dist, y = freq1), col = 'red', lwd = 1) +
  labs(x = 'Distance from Optimal Movement', y = 'Frequency') +
  theme_bw() + ylim(0, 0.03)


ggplot()+geom_line(data = norm, aes(x = dist, y = freq1))+
  labs(x = 'Distance from Optimal Movement', y = 'Frequency')+theme_bw()

