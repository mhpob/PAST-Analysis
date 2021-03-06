load('secor.sb.rda')
library(TelemetryR); library(dplyr); library(lubridate)

surv14s <- filter(secor.sb,
                  transmitter %in% paste0('A69-1601-', seq(25434, 25505, 1))) %>%
  trans_loss(., dates = 'date.local', group = 'transmitter',
             stdate = ymd_hms('2014-04-11 00:00:00'),
             enddate = ymd_hms('2018-12-31 11:59:59')) %>% 
  mutate(tagging = 'Spring',
         pct = remaining / 72)


surv14f <- filter(secor.sb,
                  transmitter %in% paste0('A69-1601-', seq(25506, 25533, 1))) %>%
  trans_loss(., dates = 'date.local', group = 'transmitter',
             stdate = ymd_hms('2014-10-30 00:00:00'),
             enddate = ymd_hms('2018-12-31 11:59:59')) %>% 
  mutate(tagging = 'Fall',
         pct = remaining / 28)

surv14 <- rbind(surv14s, surv14f)


surv16 <- filter(secor.sb,
                transmitter %in% paste0('A69-1601-', seq(53850, 53899, 1))) %>%
  trans_loss(., dates = 'date.local', group = 'transmitter',
             stdate = ymd_hms('2016-05-05 00:00:00'),
             enddate = ymd_hms('2018-12-31 11:59:59')) %>% 
  mutate(pct = remaining / 50)


library(ggplot2)
ggplot() + geom_line(data = surv14, aes(x = date, y = pct * 100,
                                        color = tagging)) +
  labs(x = 'Date', y = 'Percent 2014-tagged detected', color = 'Event') +
  scale_x_datetime(date_breaks = '6 month', date_labels = '%b %y') +
  ylim(0, 100) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8))

ggplot() + geom_line(data = surv14, aes(x = date, y = log(remaining),
                                        color = tagging)) +
  # geom_abline(intercept = 24.95, slope =-1.491e-08) +
  # geom_abline(intercept = 5.752e+01, slope = -3.847e-08) +
  labs(x = 'Date', y = 'Natural log of fish remaining', color = 'Event') +
  scale_x_datetime(date_breaks = '6 month', date_labels = '%b %y') +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8))

# Calculate tag loss

spr.lm <- lm(log(remaining) ~ date, data = surv14,
             subset = (tagging == 'Spring' & date <= '2017-04-30'))

fall.lm <- lm(log(remaining) ~ date, data = surv14,
              subset = (tagging == 'Fall' & date <= '2015-12-31'))

ggplot() +
  geom_segment(aes(x = min(surv14[surv14$tagging == 'Spring', 'date']),
                   xend = as.POSIXct('2017-04-30'),
                   y = coef(spr.lm)[1] + coef(spr.lm)[2] * as.numeric(
                     min(surv14[surv14$tagging == 'Spring', 'date'])),
                   yend = coef(spr.lm)[1] + coef(spr.lm)[2] * as.numeric(
                     as.POSIXct('2017-04-30'))),
               color = 'slategray', size = 1) +
  geom_segment(aes(x = min(surv14[surv14$tagging == 'Fall', 'date']),
                   xend = as.POSIXct('2015-12-31'),
                   y = coef(fall.lm)[1] + coef(fall.lm)[2] * as.numeric(
                     min(surv14[surv14$tagging == 'Fall', 'date'])),
                   yend = coef(fall.lm)[1] + coef(fall.lm)[2] * as.numeric(
                     as.POSIXct('2015-12-31'))),
               color = 'slategray', size = 1) +
  geom_line(data = surv14, aes(x = date, y = log(remaining),
                               linetype = tagging)) +
  scale_linetype_manual(values = c('dashed', 'twodash')) +
  labs(x = 'Date', y = 'Natural log of fish remaining', linetype = 'Event') +
  scale_x_datetime(date_breaks = '6 month', date_labels = '%b %y') +
  coord_cartesian(ylim = c(0, 4.2)) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

# Copied @ 657x475

loss_rate <- function(fitted_model){
  coefs <- summary(fitted_model)$coefficients
  
  cat('Instantaneous loss rate:',
      formatC(coefs[2,1], format = 'e', digits = 2),
      '\n')
  
  cat('95% CI:',
      formatC(coefs[2,1] - 1.96 * coefs[2,2],
              format = 'e', digits = 2),
      '--',
      formatC(coefs[2,1] + 1.96 * coefs[2,2],
              format = 'e', digits = 2),
      '\n')
  
  cat('Annual loss rate:',
      round(coefs[2,1] * 365, 2),
      '\n')
  
  cat('Percent per year:',
      round((1 - exp(coefs[2,1] * 365)) * 100, 1))
}

spr.lm <- lm(log(remaining) ~ as.Date(date), data = surv14,
             subset = (tagging == 'Spring' & date <= '2017-04-30'))

fall.lm <- lm(log(remaining) ~ as.Date(date), data = surv14,
              subset = (tagging == 'Fall' & date <= '2015-12-31'))

loss_rate(spr.lm)
loss_rate(fall.lm)
