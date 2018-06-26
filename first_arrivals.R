library(ggplot2); library(lubridate); library(dplyr)
load('secor.sb.rda')

base.data <- secor.sb %>% 
  mutate(coastal = case_when(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                         'NYB', 'Hudson', 'Long Island', 'Mass',
                                         'New Jersey') ~ T,
                            T ~ F),
         yr.adjust = case_when(date.local <= '2015-03-01' ~ 2014,
                               date.local > '2015-03-01' &
                                 date.local <= '2016-03-01' ~ 2015,
                               date.local > '2016-03-01' &
                                 date.local <= '2017-03-01' ~ 2016,
                               date.local > '2017-03-01' &
                                 date.local <= '2018-03-01' ~ 2017,
                               T ~ 2018),
         # week number using March 1 of the adjusted year as the origin
         wk.num = floor(
           as.integer(
             as.Date(date.local) - ymd(paste0(yr.adjust, '-03-01'))
           )
           / 7
         ),
         wk = floor_date(date.local, unit = 'week'))

# First week of coastal incidence ----
first.coast <- base.data %>% 
  filter(coastal == T) %>% 
  group_by(transmitter, yr.adjust) %>% 
  summarize(c.firstnum = min(wk.num))

lab.func <- function(x){
  month(ymd('2014-03-01') %m+% weeks(x), label = T, abbr = T)
}

ggplot() + geom_histogram(data = first.coast, aes(c.firstnum)) +
  facet_wrap(~ yr.adjust, ncol = 1) +
  scale_x_continuous(labels = lab.func) +
  labs(x = 'Arrival in coastal waters', y = 'Count') +
  theme_bw()

# First week of return to Chesapeake ----
first.return <- base.data %>% 
  group_by(transmitter, yr.adjust) %>% 
  filter(T %in% coastal) %>% 
  left_join(first.coast) %>% 
  filter(wk.num > c.firstnum,
         coastal == F) %>% 
  summarize(b.firstnum = min(wk.num))

ggplot() + geom_histogram(data = first.return, aes(b.firstnum), bins = 52) +
  facet_wrap(~ yr.adjust, ncol = 1) +
  scale_x_continuous(labels = lab.func) +
  labs(x = 'Return to Chesapeake Bay', y = 'Count') +
  theme_bw()

# First week above Rt 301 ----
pot.return <- base.data %>% 
  filter(month(date.local) %in% 2:5,
         # date.local > '2014-06-01',
         array %in% c('Upper Potomac', 'Mid Potomac')) %>% 
  mutate(year = as.factor(year(date.local)),
         sex = ifelse(sex == '', 'Unknown', sex)) %>% 
  group_by(transmitter, year, sex, length) %>% 
  summarize(wkfirst_pot = min(wk.num),
            datefirst_pot = min(date.local),
            wklast_pot = max(wk.num),
            datelast_pot = max(date.local),
            duration = as.numeric(datelast_pot - datefirst_pot))

# bins = 14 if plotting first date only, 15 if first and last
ggplot(data = pot.return) + geom_histogram(aes(as.Date(yday(datefirst_pot),
                                                       '2014-01-01')), bins = 15,
                                           fill = 'green', alpha = 0.7) +
  geom_histogram(aes(as.Date(yday(datelast_pot),
                             '2014-01-01')), bins = 15, fill = 'red', alpha = 0.7) +
  facet_wrap(~ year, ncol = 1) +
  scale_x_date(date_breaks = 'month', date_labels = '%b %d') +
  labs(x = 'Movement above Rt. 301', y = 'Count') +
  theme_bw()

# ggsave('301.png', width = 9, height = 7)



# Spawning period ANOVAs----
## Mean spawn date ~ year
# Random transmitter effect isn't significant.
m1 <- lme4::lmer(yday(datefirst_pot) ~ year + (1 | transmitter), data = pot.return)
m2 <- lm(yday(datefirst_pot) ~ year, data = pot.return)
anova(m1, m2)
              
# Use regular model.
model <- aov(m2)
summary(model)
TukeyHSD(model)



# Mean spawn date ~ sex
library(lme4); library(emmeans)
# Year should be random factor
m1 <- lmer(yday(datefirst_pot) ~ (1 | year), data = pot.return)
m2 <- lm(yday(datefirst_pot) ~ 1, data = pot.return)
anova(m1, m2)

# Model test
model_sex <- lmer(yday(datefirst_pot) ~ sex + (1 | year), data = pot.return)
model_null <- lmer(yday(datefirst_pot) ~ (1 | year), data = pot.return)
anova(model_sex, model_null) #No difference

# Just to show pairwise comparisons
summary(model_sex)
emmeans(model_sex, list(pairwise ~ sex), adjust = 'tukey')



# Mean spawn date ~ size
model_tl <- lmer(yday(datefirst_pot) ~ length + (1 | year), data = pot.return)
model_null <- lmer(yday(datefirst_pot) ~ (1 | year), data = pot.return)

anova(model_null, model_tl) # No difference



# Duration ~ year
# Random transmitter effect is significant when used with year as predictor
m1 <- lmer(duration ~ year + (1 | transmitter), REML = F, data = pot.return)
m2 <- lm(duration ~ year, data = pot.return)
anova(m1, m2)


model_dur <- lmer(duration ~ year + (1 | transmitter), data = pot.return)
model_null <- lmer(duration ~ (1 | transmitter), data = pot.return)
anova(model_dur, model_null)

emmeans(model_dur, list(pairwise ~ year), adjust = 'tukey')



# Duration ~ sex
# Random year effect is significant
model_sex <- lmer(duration ~ sex + (1 | year), data = pot.return)
model_null <- lm(duration ~ sex, data = pot.return)
anova(model_sex, model_null)

model_null <- lmer(duration ~ (1 | year), data = pot.return)


anova(model_sex, model_null)
emmeans(model_sex, list(pairwise ~ sex), adjust = 'tukey')



# Duration ~ size
model_tl <- lmer(duration ~ length + (1 | year), data = pot.return)
model_null <- lmer(duration ~ (1 | year), data = pot.return)

anova(model_null, model_tl)


