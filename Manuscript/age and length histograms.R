#Import ----

library(ggplot2); library(dplyr)

ages <- read.csv('p:/obrien/biotelemetry/past sb/ages.csv',
                 stringsAsFactors = F) %>% 
  group_by(FishID, TL) %>% 
  summarize(age = mean(Age_scale)) %>% 
  mutate(tlmm = TL * 25.4,
         collect = case_when(grepl('201410', FishID) ~ 'Fall',
                             grepl('2016', FishID) ~ '2016',
                             T ~ 'Spring'),
         collect = factor(collect, levels = c('Spring', 'Fall'), ordered = T))


# 2014 ----

tl <- ggplot() + geom_histogram(data = filter(ages, grepl('2014', FishID)),
                          aes(x = tlmm, fill = collect),
                          breaks = seq(400, 1100, 50), color = 'black',
                          position = 'stack', closed = 'right') +
  scale_fill_manual(values = c('white', 'darkgray')) +
  geom_vline(xintercept = c(450, 600, 700, 800), linetype = 'dashed',
             size = 1) +
  labs(x = 'TL (mm)', y = 'Count') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))


yrs <- ggplot() + geom_histogram(data = filter(ages, grepl('2014', FishID)),
                          aes(x = age, fill = collect),
                          breaks = seq(3, 14, 1), color = 'black',
                          position = 'identity', closed = 'left') +
  scale_fill_manual(values = c('white', 'darkgray')) +
  scale_x_continuous(breaks = seq(2, 14, 3)) +
  labs(x = 'Age (years)', y = NULL) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))

library(cowplot)
plot_grid(tl, yrs)

# 2016----
# Not used in manuscript
ggplot() + geom_histogram(data = filter(ages, grepl('2016', FishID)),
                          aes(x = tlmm),
                          breaks = seq(400, 1100, 50),
                          color = 'black', fill= 'darkgray') +
  geom_vline(xintercept = c(450, 600, 700, 800), linetype = 'dashed',
             size = 1) +
  labs(x = 'TL (mm)', y = 'Count') +
  theme_bw() +
  theme(legend.position = 'none')

ggplot() + geom_histogram(data = filter(ages, grepl('2016', FishID)),
                          aes(x = age),
                          breaks = seq(3, 14, 1), closed = 'left',
                          color = 'black', fill = 'darkgray') +
  scale_x_continuous(breaks = seq(2, 14, 3)) +
  labs(x = 'Age (years)', y = 'Count') +
  theme_bw()
