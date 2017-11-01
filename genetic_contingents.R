library(lubridate); library(dplyr)
load('secor.sb.rda')


secor.sb <- secor.sb %>% 
  filter(transmitter %in% paste0('A69-1601-', seq(25434, 25533, 1)),
         date.local <= '2016-06-30')

# Detections in Apr and May
apr_may <- secor.sb %>% 
  filter(year(date.local) %in% 2015:2016,
         month(date.local) %in% 4:5)

not_heard <- filter(secor.sb, 
                    transmitter %in%
                      setdiff(levels(factor(secor.sb$transmitter)),
                              levels(factor(apr_may$transmitter)))) %>% 
  mutate(apr_may = 'notheard')


apr_may <- split(apr_may, apr_may$transmitter)
for(i in seq(1, length(apr_may), 1)){
  apr_may[[i]]$apr_may <- ifelse(T %in% grepl('Potomac', unique(apr_may[[i]]$array)),
                       'AllPot', 'NoPot')
  apr_may[[i]]$apr_may <- ifelse(unique(apr_may[[i]]$apr_may) == 'AllPot' &
                         F %in% grepl('Potomac', unique(apr_may[[i]]$array)),
                       'MixPot',
                       apr_may[[i]]$apr_may)
}

apr_may <- do.call(rbind, apr_may)
apr_may <- rbind(apr_may, not_heard)

apr_may <- apr_may %>% 
  distinct(transmitter, apr_may) %>% 
  arrange(transmitter)
