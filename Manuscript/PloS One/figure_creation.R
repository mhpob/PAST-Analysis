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







