library(lubridate); library(ggplot2); library(dplyr)
load('secor.sb.rda')

# Data prep ----
valid.fish <- secor.sb %>%
  group_by(transmitter) %>%
  summarize(max = max(date.local)) %>%
  left_join(secor.sb) %>%
  filter(grepl('[34]/\\d*/2014', tag.date) |
           (tag.date == '10/30/2014' & year(date.local) >= 2015),
         !grepl('465', transmitter)) %>%
  mutate(year = year(date.local)) %>%
  group_by(transmitter, year, max) %>%
  summarize(max.yr = max(date.local),
            max.mo = month(max.yr)) %>%
  # Select fish that either 1) made it into August of that year, or 2) who
  # weren't heard after July of a year, but were heard in years after
  filter(max.mo >= 8 | max > max.yr)

valid.data <- ungroup(valid.fish) %>%
  left_join(mutate(secor.sb, year = year(date.local))) %>% 
  mutate(age = age + (year - 2014),
         year = as.factor(year),
         coastal = case_when(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                          'NYB', 'Hudson', 'Long Island', 'Mass',
                                          'New Jersey') ~ T,
                             T ~ F)) %>% 
  group_by(transmitter, year, age, length, weight, sex) %>% 
  summarize(coastal = T %in% coastal) %>% 
  mutate(c.num = ifelse(coastal == T, 1, 0))

# Logistic regression plot
library(ggplot2)
logis_plot <- function(data, xvar, group = year){
  xvar <- enquo(xvar)
  group <- enquo(group)
  ggplot() + geom_point(data = data, aes(x = !! xvar, y = c.num,
                                               color = !! group)) +
    stat_smooth(data = data, aes(x = !! xvar, y = c.num,
                                       color = !! group),
                method = 'glm', method.args = list(family = 'binomial')) +
    labs(x = xvar, y = 'Proportion Coastal', color = group) +
    theme_bw()
}

# logis_plot(valid.data, age)
# logis_plot(valid.data, length)
# logis_plot(valid.data, weight)


### Per-year model fitting and bootstrapping ----
log_emig <- split(valid.data, valid.data$year)

## Logistic regression on a variable vs T/F coastal
l_e_fit <- function(data, variable){
  form <- as.formula(paste0('c.num ~ ', variable))
  
  lapply(data, function(x){
    glm(form, family = 'binomial', data = x)
  })
  
}

age_fit <- l_e_fit(log_emig, 'age')
tl_fit <- l_e_fit(log_emig, 'length')
wt_fit <- l_e_fit(log_emig, 'weight')

## Bootstrapping
library(boot)

# Funtion to be bootstrapped
boot_fun <- function(x, indices, form){
  d <- x[indices,]
  fit <- glm(form, family = 'binomial', data = d)
  coeffs <- coef(fit)
  
  # Formula below is reduction of ((log(prop / (1 - prop)) - intercept) / age)
  # where prop = 0.5. Used to calculate age @ 50% coastal. Can be adjusted
  # later if different proportions are desired
  pct50 <- setNames(-coeffs[1] / coeffs[2], 'pct50')
  coeffs <- c(coeffs, pct50)
  
  # Returns GLM coefficients and the length at 50% coastal
  coeffs
}

# Apply 10k times over each year subset.
l_e_boot <- function(data, variable){
  form <- as.formula(paste0('c.num ~ ', variable))
  ncore <- as.integer(Sys.getenv('NUMBER_OF_PROCESSORS')) - 2
  
  lapply(data, function(x){
    boot(data = x, statistic = boot_fun, R = 10000, form = form,
         parallel = 'snow', ncpus = ncore)
  })
}

age_boot <- l_e_boot(log_emig, 'age')
tl_boot <- l_e_boot(log_emig, 'length')
wt_boot <- l_e_boot(log_emig, 'weight')

# BCa (adjusted bootstrap percentile) 95% CIs
l_e_bootci <- function(boot.fit){
  lapply(boot.fit, function(x){
    temp <- list(pct50 = boot.ci(x, index = 3, type = 'bca'),
                 intercept = boot.ci(x, index = 1, type = 'bca'),
                 hold = boot.ci(x, index = 2, type = 'bca'))
    names(temp)[3] <- names(x$t0)[2]
    temp
  })
}

age_bootci <- l_e_bootci(age_boot)
tl_bootci <- l_e_bootci(tl_boot)
wt_bootci <- l_e_bootci(wt_boot)

# lapply(wt_boot, `[`, 't0')
# lapply(wt_fit, summary)

## Logit model differences significance test
# Method from http://derekogle.com/fishR/examples/oldFishRVignettes/Maturity.pdf
model <- glm(c.num ~ age * year, family = 'binomial',
             data = valid.data, subset = (year %in% c(2014, 2015)))
drop1(model, ~., test = 'Chisq')

# Method from https://stats.stackexchange.com/questions/316801/how-to-compare-logistic-regression-curves
modelnoint <- glm(c.num ~ age + year, family = 'binomial',
                  data = valid.data, subset = (year %in% c(2014, 2015)))

anova(modelnoint, model, test = 'Chisq')




# occ.data <- secor.sb %>% 
#   left_join(valid.fish) %>% 
#   # Use flag to remove observations if fish didn't make it through year
#   mutate(yr.flag = ifelse(yr.flag == 2014 & date.local >= '2015-04-01',
#                           NA, yr.flag)) %>% 
#   filter(!is.na(yr.flag),
#          date.local < '2016-04-01') %>% 
#   mutate(tag.season = ifelse(tag.date == '2014-10-30', 'Fall', 'Spring'),
#          date.floor = floor_date(date.local, unit = 'month'),
#          year = ifelse(date.local < '2015-04-01', 2014, 2015),
#          coastal = ifelse(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
#                                 'Hudson', 'Long Island', 'Mass', 'New Jersey'),
#                           1, 0),
#          age.adjust = ifelse(date.local >= '2015-04-01',
#                              age + 1, age))

# pct.coastal <- occ.data %>% 
#   filter(coastal == T) %>% 
#   distinct(transmitter, date.floor, .keep_all = T) %>%
#   group_by(transmitter, age.adjust) %>% 
#   summarize(pct.coastal = n()/12)
# 
# occ.data <- left_join(occ.data, pct.coastal) %>% 
#   group_by(transmitter, year) %>% 
#   mutate(coastal = ifelse(1 %in% coastal, 1, 0),
#          pct.coastal = ifelse(is.na(pct.coastal), 0, pct.coastal)) %>%
#   ungroup() %>% 
#   distinct(tag.season, transmitter, year, coastal, sex,
#            pct.coastal, age.adjust, length)

# Age/Length v incidence, separated by 2014 tagging event ----
# ggplot() + geom_jitter(data = occ.data,
#                       aes(x = age.adjust, y = pct.coastal, color = tag.season),
#                       width = 0.1, height = 0.01) +
#   geom_smooth(data = occ.data,
#               aes(x = age.adjust, y = pct.coastal, color = tag.season)) +
#   labs(x = 'Age', y = '% of Year in Coastal Waters', color = 'Tag\nSeason') +
#   coord_cartesian(ylim = c(-0.01, 0.7)) +
#   scale_x_continuous(breaks = c(3, 6, 9, 12, 15)) +
#   theme_bw()
# 
# ggplot() + geom_jitter(data = occ.data,
#                       aes(x = length, y = pct.coastal, color = tag.season),
#                       width = 0.1, height = 0.01) +
#   geom_smooth(data = occ.data,
#               aes(x = length, y = pct.coastal, color = tag.season)) +
#   labs(x = 'Length (mm)', y = '% of Year in Coastal Waters',
#        color = 'Tag\nSeason') +
#   coord_cartesian(ylim = c(-0.01, 0.7)) +
#   theme_bw()

# Age/Length v incidence, separated by year ----
# ggplot() + geom_jitter(data = occ.data,
#                       aes(x = age.adjust, y = pct.coastal, color = factor(year)),
#                       width = 0.1, height = 0.01) +
#   geom_smooth(data = occ.data,
#               aes(x = age.adjust, y = pct.coastal, color = factor(year))) +
#   labs(x = 'Age', y = '% of Year in Coastal Waters', color = 'Year') +
#   coord_cartesian(ylim = c(-0.01, 0.7)) +
#   scale_x_continuous(breaks = c(3, 6, 9, 12, 15)) +
#   theme_bw()
# 
# ggplot() + geom_jitter(data = occ.data,
#                       aes(x = length, y = pct.coastal, color = factor(year)),
#                       width = 0.1, height = 0.01) +
#   geom_smooth(data = occ.data,
#               aes(x = length, y = pct.coastal, color = factor(year))) +
#   labs(x = 'Length (mm)', y = '% of Year in Coastal Waters',
#        color = 'Year') +
#   coord_cartesian(ylim = c(-0.01, 0.7)) +
#   theme_bw()

# Sex v coastal incidence ----
# ggplot() + geom_jitter(data = filter(occ.data, sex != ''),
#                        aes(x = age.adjust, y = pct.coastal, color = sex),
#                        width = 0.1, height = 0.01) +
#   geom_smooth(data = filter(occ.data, sex != ''),
#               aes(x = age.adjust, y = pct.coastal, color = sex)) +
#   labs(x = 'Age', y = '% of Year in Coastal Waters', color = 'Sex') +
#   coord_cartesian(ylim = c(-0.01, 0.74)) +
#   scale_x_continuous(breaks = c(3, 6, 9, 12, 15)) +
#   theme_bw()
# 
# ggplot() + geom_jitter(data = filter(occ.data, sex != ''),
#                        aes(x = length, y = pct.coastal, color = sex),
#                        width = 0.1, height = 0.01) +
#   geom_smooth(data = filter(occ.data, sex != ''),
#               aes(x = length, y = pct.coastal, color = sex)) +
#   labs(x = 'Length (mm)', y = '% of Year in Coastal Waters',
#        color = 'Sex') +
#   coord_cartesian(ylim = c(-0.01, 0.73)) +
#   theme_bw()

# Modeling ----
#   Run coastal ~ age + random(fish) for age
#   Run years separately for length, shouldn't need random transmitter effect
rm(pct.coastal, secor.sb, valid.fish)
spring <- filter(occ.data, tag.season == 'Spring')
sp.2014 <- filter(spring, year == 2014)
sp.2015 <- filter(spring, year == 2015)

# Coastal incidence v age modeling ----
library(lme4)
glm_incidence <- glmer(coastal ~ age.adjust + (1 | transmitter),
                       family = 'binomial',
                       data = spring)

# Likelihood glmm
# library(ez)
# glm_incidence_ML <- ezMixed(data = spring,
#                            dv = .(coastal),
#                            fixed = .(age.adjust),
#                            random = .(transmitter),
#                            family = 'binomial')

# Coastal incidence v length modeling ----
glm_incidence14 <- glm(coastal ~ length,
                       data = sp.2014,
                       family = 'binomial')
glm_incidence15 <- glm(coastal ~ length,
                       data = sp.2015,
                       family = 'binomial')

# % Months coastal v age ----
library(lme4)
glm_PctCoast <- glmer(pct.coastal ~ age.adjust + (1 | transmitter),
                      family = 'binomial',
                      data = spring)

# Likelihood glmm
# library(ez)
# glm_PctCoast_ML <- ezMixed(data = spring,
#                            dv = .(pct.coastal),
#                            fixed = .(age.adjust),
#                            random = .(transmitter),
#                            family = 'binomial')

# % Months coastal v length modeling ----
glm_PctCoast14 <- glm(pct.coastal ~ length,
                       data = sp.2014,
                       family = 'binomial')
glm_PctCoast15 <- glm(pct.coastal ~ length,
                       data = sp.2015,
                       family = 'binomial')
