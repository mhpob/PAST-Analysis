#                    Centimeters	Inches	Pixels at 300 dpi
# Minimum width          6.68       2.63	  789
# Maximum width         19.05	       7.5   	2250
# Height maximum        22.23	      8.75	  2625
# 


## Figure 1 ----
library(ggplot2); library(ggrepel); library(cowplot) 
library(lubridate); library(dplyr); library(sf)

# Import map
base_map <- read_sf('manuscript/plos one/atlcoast.gpkg')
base_map_plot <- ggplot() +
  geom_sf(data = base_map, fill  = 'grey', color = 'lightgray') +
  coord_sf(xlim = c(-77.5, -69), ylim = c(36.53, 43), expand = F,
           label_axes = '-NE-') +
  theme_bw()


# Select stations where striped bass were detected
stations <- read.csv('manuscript/plos one/secor_detections.csv') %>% 
  mutate(year = year(date),
         lat = round(lat, 3),
         long = round(long, 3)) %>% 
  distinct(lat, long, year)


# Plot per-year locations of detections on base map
all_years <- base_map_plot +
  geom_point(data = stations, aes(x = long, y = lat),
             color = 'black', size = 1) +
  labs(x = NULL, y = NULL) +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 12),
        plot.margin = unit(c(0, 0.05, 0, 0.05), "cm"),
        axis.text.y.right = element_text(angle = -45, vjust = 0),
        axis.text = element_text(size = 9)) +
  facet_wrap(~year)


# Select stations in/near the Chesapeake during years analyzed
inset_stations <- stations %>% 
  filter(year < 2018) %>% 
  distinct(lat, long) %>% 
  mutate(lab = '') %>% 
  rbind(data.frame(lat = c(36.98, 38.247, 38.99),
                   long = c(-76.11, -76.77, -76.37),
                   lab = c('Bay Bridge Tunnel',
                           'Potomac River', 'Bay Bridge')))


# Create general shape of the Chesapeake Bay Bridge Tunnel
cbbt <- data.frame(long = c(-76.12966, -76.08696, -76.00697,-75.98079),
                   lat = c(36.91925, 37.02896, 37.084,37.09157))


# Create Chesapeake-only inset
cb <- ggplot() +
  geom_sf(data = base_map, fill  = 'grey', color = 'lightgray') +
  coord_sf(xlim = c(-77.5, -75.5), ylim = c(36.54, 39.8), expand = F) +
  geom_path(data = cbbt, aes(x = long, y = lat), size = 1) +
  geom_point(data = inset_stations, aes(x = long, y = lat),
             color = ifelse(inset_stations$lab == '', 'black', NA), size = 1) +
  geom_label_repel(data = inset_stations, aes(x = long, y = lat, label = lab),
                            box.padding = 0.5, point.padding = 0.5, size = 2.75,
                   xlim = c(NA, -76.6)) +
  geom_point(aes(x = -76.327180, y = 38.052251),
             col = 'black', shape = 4, size = 3) +
  geom_point(aes(x = -76.938432, y = 38.337408),
             col = 'black', shape = 4, size = 3) +
  theme_bw() +
  theme(plot.margin = unit(c(0, 0.05, 0, 0.05), "cm"),
        axis.text.y.left = element_text(angle = 45, vjust = 0),
        axis.text = element_text(size = 9)) +
  labs(x = NULL, y = NULL)


# Plot inset and per-year stations together
combined <- plot_grid(cb, all_years, rel_widths = c(1, 2.34))


# Export figure
save_plot("manuscript/plos one/Figure1.tif", combined, device = 'tiff',
          base_width = 7.5,
          base_asp = 1.59)


# > sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 17763)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] sf_0.8-0        dplyr_0.8.3     lubridate_1.7.4 cowplot_1.0.0   ggrepel_0.8.1   ggplot2_3.2.1  
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.3         rstudioapi_0.10    magrittr_1.5       units_0.6-5        tidyselect_0.2.5  
# [6] munsell_0.5.0      colorspace_1.4-1   R6_2.4.1           rlang_0.4.2        stringr_1.4.0     
# [11] tools_3.6.1        grid_3.6.1         gtable_0.3.0       KernSmooth_2.23-15 e1071_1.7-3       
# [16] DBI_1.0.0          withr_2.1.2        class_7.3-15       lazyeval_0.2.2     assertthat_0.2.1  
# [21] tibble_2.1.3       lifecycle_0.1.0    crayon_1.3.4       farver_2.0.1       purrr_0.3.3       
# [26] glue_1.3.1         stringi_1.4.3      compiler_3.6.1     pillar_1.4.2       scales_1.1.0      
# [31] classInt_0.4-2     pkgconfig_2.0.3  



## Figure 2 ----
library(ggplot2); library(cowplot); library(dplyr)

# Convert from mm to cm and categorize fish by release season
fish <- read.csv('manuscript/plos one/secor_tagging_data.csv') %>% 
  mutate(length.cm = length.mm / 10,
         season = ifelse(grepl('^10', tag.date), 'Fall', 'Spring'),
         season = factor(season, levels = c('Spring', 'Fall'), ordered = T))


# Histogram of total length
tl <- ggplot() + geom_histogram(data = fish,
                                aes(x = length.cm, fill = season),
                                breaks = seq(40, 110, 5), color = 'black',
                                position = 'stack', closed = 'right') +
  scale_fill_manual(values = c('white', 'darkgray')) +
  geom_vline(xintercept = c(45, 60, 70, 80), linetype = 'dashed',
             size = 1) +
  labs(x = 'Total Length (cm)', y = 'Count') +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))


# Histogram of age
yrs <- ggplot() + geom_histogram(data = fish,
                                 aes(x = age.yrs, fill = season),
                                 breaks = seq(3, 14, 1), color = 'black',
                                 position = 'identity', closed = 'left') +
  scale_fill_manual(values = c('white', 'darkgray')) +
  scale_x_continuous(breaks = seq(2, 14, 3)) +
  labs(x = 'Age (years)', y = NULL) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 15))


# Plot histograms side-by-side
combined <- plot_grid(tl, yrs)


# Export figure
save_plot("manuscript/plos one/Figure2.tif", combined, device = 'tiff',
          base_width = 7.5,
          base_asp = 1.59)


# > sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 17763)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] dplyr_0.8.3   cowplot_1.0.0 ggplot2_3.2.1
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.3       withr_2.1.2      assertthat_0.2.1 crayon_1.3.4     grid_3.6.1      
# [6] R6_2.4.1         lifecycle_0.1.0  gtable_0.3.0     magrittr_1.5     scales_1.1.0    
# [11] pillar_1.4.2     rlang_0.4.2      farver_2.0.1     lazyeval_0.2.2   rstudioapi_0.10 
# [16] labeling_0.3     tools_3.6.1      glue_1.3.1       purrr_0.3.3      munsell_0.5.0   
# [21] compiler_3.6.1   pkgconfig_2.0.3  colorspace_1.4-1 tidyselect_0.2.5 tibble_2.1.3  



## Figure 3 ----
library(ggplot2); library(TelemetryR); library(lubridate); library(dplyr)

# TelemetryR is a package written/maintained by M. O'Brien and can be downloaded here:
# https://github.com/mhpob/TelemetryR

detections <- read.csv('manuscript/plos one/secor_detections.csv',
                       stringsAsFactors = F) %>% 
  left_join(read.csv('manuscript/plos one/secor_tagging_data.csv',
                     stringsAsFactors = F)) %>% 
  mutate(date = ymd(date),
         tag.season = ifelse(grepl('^10', tag.date), 'Fall', 'Spring'))


# Calculate transmitters remaining by release season using TelemetryR::trans_loss
spring <- detections %>%
  filter(tag.season == 'Spring') %>%
  trans_loss(., dates = 'date', group = 'transmitter',
             stdate = ymd_hms('2014-04-11 00:00:00'),
             enddate = ymd_hms('2018-12-31 11:59:59')) %>% 
  mutate(tag.season = 'Spring',
         pct = remaining / 72)


fall <- detections %>%
  filter(tag.season == 'Fall') %>%
  trans_loss(., dates = 'date', group = 'transmitter',
             stdate = ymd_hms('2014-10-30 00:00:00'),
             enddate = ymd_hms('2018-12-31 11:59:59')) %>% 
  mutate(tag.season = 'Fall',
         pct = remaining / 28)

survivors <- rbind(spring, fall)


# Per-release-season linear models on POSIX time for ease of plotting
# Spring release is modeled only through expected transmitter battery life
spr.lm <- lm(log(remaining) ~ date, data = survivors,
             subset = (tag.season == 'Spring' & date <= '2017-04-30'))

# Fall release is modeled only through period of initial mortality
fall.lm <- lm(log(remaining) ~ date, data = survivors,
              subset = (tag.season == 'Fall' & date <= '2015-12-31'))

tag_loss <- ggplot() +
  geom_segment(aes(x = min(survivors[survivors$tag.season == 'Spring', 'date']),
                   xend = as.POSIXct('2017-04-30'),
                   y = coef(spr.lm)[1] + coef(spr.lm)[2] * as.numeric(
                     min(survivors[survivors$tag.season == 'Spring', 'date'])),
                   yend = coef(spr.lm)[1] + coef(spr.lm)[2] * as.numeric(
                     as.POSIXct('2017-04-30'))),
               color = 'slategray', size = 1) +
  geom_segment(aes(x = min(survivors[survivors$tag.season == 'Fall', 'date']),
                   xend = as.POSIXct('2015-12-31'),
                   y = coef(fall.lm)[1] + coef(fall.lm)[2] * as.numeric(
                     min(survivors[survivors$tag.season == 'Fall', 'date'])),
                   yend = coef(fall.lm)[1] + coef(fall.lm)[2] * as.numeric(
                     as.POSIXct('2015-12-31'))),
               color = 'slategray', size = 1) +
  geom_line(data = survivors, aes(x = date, y = log(remaining),
                               linetype = tag.season)) +
  scale_linetype_manual(values = c('dashed', 'twodash')) +
  labs(x = NULL, y = 'Natural log of fish remaining', linetype = 'Release') +
  scale_x_datetime(date_breaks = '6 month', date_labels = '%b %y') +
  coord_cartesian(ylim = c(0, 4.2)) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))


ggsave("manuscript/plos one/Figure3.tif", tag_loss, device = 'tiff',
          width = 7.5, height = 5.42, units = 'in')


# > sessionInfo()
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 17763)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] dplyr_0.8.3      lubridate_1.7.4  TelemetryR_0.9.4 ggplot2_3.2.1   
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.3        rstudioapi_0.10   magrittr_1.5      tidyselect_0.2.5  munsell_0.5.0    
# [6] colorspace_1.4-1  R6_2.4.1          rlang_0.4.2       stringr_1.4.0     tools_3.6.1      
# [11] grid_3.6.1        data.table_1.12.6 gtable_0.3.0      withr_2.1.2       digest_0.6.23    
# [16] lazyeval_0.2.2    assertthat_0.2.1  tibble_2.1.3      lifecycle_0.1.0   crayon_1.3.4     
# [21] farver_2.0.1      purrr_0.3.3       glue_1.3.1        labeling_0.3      stringi_1.4.3    
# [26] compiler_3.6.1    pillar_1.4.2      scales_1.1.0      pkgconfig_2.0.3  



# Figures 4 and 5 ----
library(ggplot2); library(lubridate); library(ggplot2); library(dplyr)

detections <- read.csv('manuscript/plos one/secor_detections.csv',
                       stringsAsFactors = F)
tagging.data <- read.csv('manuscript/plos one/secor_tagging_data.csv',
                         stringsAsFactors = F)

# Select fish that either 1) made it into August of that year, or 2) who
# weren't heard after July of a year, but were heard in years after
valid.fish <- detections %>% 
  group_by(transmitter) %>%
  summarize(max.overall = max(date)) %>% 
  left_join(tagging.data) %>%
  right_join(detections) %>%
  filter(grepl('^[34]', tag.date) |
           (grepl('^10', tag.date) & date >= 2015)) %>%
  mutate(year = lubridate::year(date)) %>% 
  group_by(transmitter, year, max.overall) %>%
  summarize(max.within.yr = max(date),
            max.mo.within.yr = month(max.within.yr)) %>%
  filter(max.mo.within.yr >= 8 | max.overall > max.within.yr) %>% 
  ungroup()


# Select data using the valid fish as a key.
valid.data <- valid.fish %>%
  left_join(mutate(detections, year = year(date))) %>% 
  left_join(tagging.data) %>% 
  mutate(
    # Advance the scale ages by 1 after each spawning season
    age = age.yrs + (year - 2014),
    year = as.factor(year),
    length.cm = length.mm / 10,
    # Dummy-code coastal arrays
    coastal = case_when(array %in% c('VA Coast', 'MD Coast', 'DE Coast',
                                     'NYB', 'Hudson', 'Long Island', 'Mass',
                                     'New Jersey') ~ T,
                        T ~ F)) %>% 
  group_by(transmitter, year, age, length.cm) %>% 
  # Calculate number that were detected in coastal arrays
  summarize(coastal = T %in% coastal) %>% 
  mutate(c.num = ifelse(coastal == T, 1, 0))


# Figure 4
length.logis <- ggplot(data = valid.data[valid.data$year %in% c(2014, 2015),],
                       aes(x = length.cm, y = c.num, color = year)) +
  geom_point(aes(shape = year), size = 3) +
  stat_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  scale_color_grey(end = 0.5) + # gets the same colors as Figure 5
  labs(x = 'Length at Tagging (cm)', y = 'Proportion Coastal',
       color = 'Year', shape = 'Year') +
  theme_bw() +
  theme(legend.position = c(0.1, 0.8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  guides(color = guide_legend(override.aes = list(fill = NA)))


# Figure 5
age.logis <- ggplot(data = valid.data[valid.data$year %in% c(2014, 2015, 2016),],
                    aes(x = age, y = c.num, color = year)) +
  geom_point(aes(shape = year), size = 3) +
  stat_smooth(method = 'glm', method.args = list(family = 'binomial')) +
  scale_color_grey() +
  labs(x = 'Age', y = 'Proportion Coastal', color = 'Year', shape = 'Year') +
  theme_bw() +
  theme(legend.position = c(0.1, 0.8),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12)) +
  guides(color = guide_legend(override.aes = list(fill = NA)))


ggsave("manuscript/plos one/Figure4.tif", length.logis, device = 'tiff',
       width = 7.5, height = 5.28, units = 'in', compression = 'lzw')

ggsave("manuscript/plos one/Figure5.tif", age.logis, device = 'tiff',
       width = 7.5, height = 5.28, units = 'in', compression = 'lzw')
