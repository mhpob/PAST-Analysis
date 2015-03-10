library(ggplot2)

## Phase (2 Lines)
ggplot() + geom_histogram(data = sp.pot,
                     aes(x = mean.sp.bl, y = ..density..), binwidth = 0.5) +
  #PLB
  stat_function(aes(x = seq(0, 8, 0.001), color = 'PLB'),
                fun = function(x){plb$norm.const * x ^ -plb$mu},
                lwd = 1) +
  #EXPB
  stat_function(aes(x = seq(0, 8, 0.001), color = 'EXPB'),
                fun = function(x){expb$norm.const * exp(-exp$lambda * x)},
                lwd = 2) +
  scale_color_manual('Model', values = c('blue', 'red')) +
  labs(title = 'Spawning, Potomac', x = 'Mean Speed (BL/s)',
       y = "Proportion of Obervations") +
  xlim(0, 8)


## Phase
ggplot() + geom_histogram(data = filter(sb.speed, mean.sp.bl <= 8,
                                        phase == 'running'),
                     aes(x = mean.sp.bl, y = ..density..), binwidth = 0.5) +
  #PL
  stat_function(aes(x = seq(0, 8, 0.001), color = 'PL'),
                fun = function(x){pl$norm.const * x ^ -pl$mu}) +
  #PLB
  stat_function(aes(x = seq(0, 8, 0.001), color = 'PLB'),
                fun = function(x){plb$norm.const * x ^ -plb$mu}) +
  #EXP
  stat_function(aes(x = seq(0, 8, 0.001), color = 'EXP'),
                fun = function(x){exp$norm.const * exp$lambda *
                      exp(-exp$lambda * (x - abn$a))}) +
  #EXPB
  stat_function(aes(x = seq(0, 8, 0.001), color = 'EXPB'),
                fun = function(x){expb$norm.const * exp(-exp$lambda * x)}) +
  scale_color_manual('Model', values = c('red', 'orange', 'blue', 'purple')) +
  labs(title = 'Non-Spawning (15% Subsample)', x = 'Mean Speed (BL/s)',
       y = "Proportion of Obervations") +
  xlim(0, 8)


## Length
ggplot() + geom_histogram(data = filter(sb.speed, mean.sp.bl <= 8,
                                        length.bin == '>80'),
                     aes(x = mean.sp.bl, y = ..density..), binwidth = 0.5) +
  #PL
  stat_function(aes(x = seq(0, 8, 0.001), color = 'PL'),
                fun = function(x){pl$norm.const * x ^ -pl$mu}) +
  #PLB
  stat_function(aes(x = seq(0, 8, 0.001), color = 'PLB'),
                fun = function(x){plb$norm.const * x ^ -plb$mu}) +
  #EXP
  stat_function(aes(x = seq(0, 8, 0.001), color = 'EXP'),
                fun = function(x){exp$norm.const * exp$lambda *
                      exp(-exp$lambda * (x - abn$a))}) +
  #EXPB
  stat_function(aes(x = seq(0, 8, 0.001), color = 'EXPB'),
                fun = function(x){expb$norm.const * exp(-exp$lambda * x)}) +
  scale_color_manual('Model', values = c('red', 'orange', 'blue', 'purple')) +
  labs(title = '>80 cm Fish', x = 'Mean Speed (BL/s)',
       y = "Proportion of Obervations") +
  xlim(0, 8)


## Sex
ggplot() + geom_histogram(data = filter(sb.speed, mean.sp.bl <= 8,
                                        sex == 'F'),
                     aes(x = mean.sp.bl, y = ..density..), binwidth = 0.5) +
  #PL
  stat_function(aes(x = seq(0, 8, 0.001), color = 'PL'),
                fun = function(x){pl$norm.const * x ^ -pl$mu}) +
  #PLB
  stat_function(aes(x = seq(0, 8, 0.001), color = 'PLB'),
                fun = function(x){plb$norm.const * x ^ -plb$mu}) +
  #EXP
  stat_function(aes(x = seq(0, 8, 0.001), color = 'EXP'),
                fun = function(x){exp$norm.const * exp$lambda *
                      exp(-exp$lambda * (x - abn$a))}) +
  #EXPB
  stat_function(aes(x = seq(0, 8, 0.001), color = 'EXPB'),
                fun = function(x){expb$norm.const * exp(-exp$lambda * x)}) +
  scale_color_manual('Model', values = c('red', 'orange', 'blue', 'purple')) +
  labs(title = 'Female', x = 'Mean Speed (BL/s)',
       y = "Proportion of Obervations") +
  xlim(0, 8)


## Combos
ggplot() + geom_histogram(data = filter(sb.speed, mean.sp.bl <= 8,
                                  phase == 'running', sex == 'F'),
                     aes(x = mean.sp.bl, y = ..density..), binwidth = 0.5) +
  #PL
  stat_function(aes(x = seq(0, 8, 0.001), color = 'PL'),
                fun = function(x){pl$norm.const * x ^ -pl$mu}) +
  #PLB
  stat_function(aes(x = seq(0, 8, 0.001), color = 'PLB'),
                fun = function(x){plb$norm.const * x ^ -plb$mu}) +
  #EXP
  stat_function(aes(x = seq(0, 8, 0.001), color = 'EXP'),
                fun = function(x){exp$norm.const * exp$lambda *
                      exp(-exp$lambda * (x - abn$a))}) +
  #EXPB
  stat_function(aes(x = seq(0, 8, 0.001), color = 'EXPB'),
                fun = function(x){expb$norm.const * exp(-exp$lambda * x)}) +
  scale_color_manual('Model', values = c('red', 'orange', 'blue', 'purple')) +
  labs(title = 'Non-Spawning Female', x = 'Mean Speed (BL/s)',
       y = "Proportion of Obervations") +
  xlim(0, 8)