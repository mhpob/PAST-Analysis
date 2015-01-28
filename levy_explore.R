binned <- old %>%
  filter(mean.sp.bl > 0 & mean.sp.bl < 8.513657) %>% 
  mutate(mean.sp.bl = mean.sp.bl + 1,
    bins = cut(mean.sp.bl, breaks = c(2^seq(0, 3.25, 0.25)))) %>% 
  group_by(system, bins) %>%
  tally()

j <- levels(binned$bins)
j <- strsplit(j, ',')
j <- sapply(j, strsplit, ']')
j <- data.frame(j)
j <- slice(j, 2)
j <- t(j)
j <- as.numeric(j)
binned$bins <- j

for(i in 1:length(binned$bins)){
  binned[i, 'width'] <- ifelse(binned[i, 'bins'] == 1.19, 0.19,
                           binned[i, 'bins'] - binned[i - 1, 'bins'])
}

binned <- binned %>% 
  group_by(system) %>% 
  mutate(width = as.numeric(width),
    log.nfreq = log10(n / width / 13),
         log.bins = log10(bins))

ggplot(data = binned, aes(x = log.bins, y = log.nfreq, color = sex)) + geom_point() +stat_smooth(method = 'lm')

summary(lm(log.nfreq~log.bins, data = subset(binned, length.bin == '55-65')))
summary(lm(log.nfreq~log.bins, data = subset(binned, length.bin == '65-80')))
summary(lm(log.nfreq~log.bins, data = subset(binned, length.bin == '>80')))
