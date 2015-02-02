binned <- secor.sb %>%
  filter(mean.sp.bl > 0 & mean.sp.bl < 8.513657, sex %in% c('M','F')) %>% 
  mutate(mean.sp.bl = mean.sp.bl + 1,
    bins = cut(mean.sp.bl, breaks = c(2^seq(0, 3.25, 0.25)))) %>% 
  group_by(sex, phase, bins) %>%
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
  group_by(sex, phase) %>% 
  mutate(width = as.numeric(width),
    log.nfreq = log10(n / width / 13),
         log.bins = log10(bins))

ggplot(data = binned, aes(x = log.bins, y = log.nfreq, color = sex)) + geom_point() +stat_smooth(method = 'lm') + facet_wrap(~phase)

levy <- function(leng, phase){
  summary(lm(log.nfreq ~ log.bins,
             data = binned[binned$length.bin == leng & binned$phase == phase,]))
}

levy('55-65', 'running')
levy('65-80', 'running')
levy('>80', 'running')

levy('55-65', 'spawning')
levy('65-80', 'spawning')
levy('>80', 'spawning')

summary(lm(log.nfreq ~ log.bins,
             data = binned[binned$phase == 'running' & binned$sex == 'M',]))
summary(lm(log.nfreq ~ log.bins,
             data = binned[binned$phase == 'running' & binned$sex == 'F',]))

summary(lm(log.nfreq ~ log.bins,
             data = binned[binned$phase == 'spawning' & binned$sex == 'M',]))
summary(lm(log.nfreq ~ log.bins,
             data = binned[binned$phase == 'spawning' & binned$sex == 'F',]))
