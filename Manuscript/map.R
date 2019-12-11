library(ggplot2); library(dplyr); library(sf)
midstates <- read_sf('p:/obrien/midatlantic/matl_states_land.shp') %>% 
  filter(!is.na(STATE_FIPS))

base_map <- ggplot() +
  geom_sf(data = midstates, fill  = 'grey', color = 'lightgray') +
  coord_sf(xlim = c(-77.5, -69), ylim = c(36.53, 43), expand = F,
           label_axes = '-NE-') +
  theme_bw()

load('secor.sb.rda')
secor.sb <- secor.sb %>% 
  filter(grepl('-25', transmitter)) %>% 
  mutate(year = lubridate::year(date.local),
         lat = round(lat, 3),
         long = round(long, 3))
stations <- unique(secor.sb[secor.sb$year <= 2018, c('lat', 'long', 'year')])
all <- base_map + geom_point(data = stations, aes(x = long, y = lat),
                             color = 'black', size = 1.5) +
  labs(x = NULL, y = NULL) +
  theme(strip.background = element_rect(fill = NA),
        strip.text = element_text(size = 12),
        plot.margin = unit(c(0, 0.05, 0, 0.05), "cm"),
        axis.text.y.right = element_text(angle = -45, vjust = 0),
        axis.text = element_text(size = 12)) +
  facet_wrap(~year)


inset_stations <- unique(secor.sb[secor.sb$year == 2015, c('lat', 'long', 'year')])
inset_stations$lab <- ""
inset_stations$type <-  
inset_stations <- rbind(inset_stations,
                        data.frame(lat = c(36.98, 38.247, 38.99),
                                   long = c(-76.11, -76.77, -76.37),
                                   year = c(2015, 2015, 2015),
                                   lab = c('Bay Bridge Tunnel',
                                           'Potomac River', 'Bay Bridge')))
cbbt <- data.frame(long = c(-76.12966, -76.08696, -76.00697,-75.98079),
                   lat = c(36.91925, 37.02896, 37.084,37.09157))




cb <- ggplot() +
  geom_sf(data = midstates, fill  = 'grey', color = 'lightgray') +
  coord_sf(xlim = c(-77.5, -75.5), ylim = c(36.6, 39.8), expand = F) +
  theme_bw() +
  theme(plot.margin = unit(c(0, 0.05, 0, 0.05), "cm"),
        axis.text.y.left = element_text(angle = 45, vjust = 0),
        axis.text = element_text(size = 12)) +
  geom_point(data = inset_stations, aes(x = long, y = lat),
             color = ifelse(inset_stations$lab == '', 'black', NA), size = 1.5) +
  ggrepel::geom_label_repel(data = inset_stations, aes(x = long, y = lat, label = lab),
                            point.padding = 0.25) +
  geom_point(aes(x = -76.327180, y = 38.052251),
             col = 'black', shape = 4, size = 3) +
  geom_point(aes(x = -76.938432, y = 38.337408),
             col = 'black', shape = 4, size = 3) +
  geom_path(data = cbbt, aes(x = long, y = lat), size = 1) +
  labs(x = NULL, y = NULL)



combined <- cowplot::plot_grid(cb, all, rel_widths = c(1, 2.34))
combined

ggsave("test.eps", combined) #855*430