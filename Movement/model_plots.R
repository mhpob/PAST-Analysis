library(ggplot2)

## Phase
ggplot() + geom_histogram(data = filter(sb.speed, dist > 1.8,
                                        phase == 'spawning'),
                     aes(x = dist, y = ..density..), binwidth = 10) +
  #PL
  stat_function(aes(x = seq(2,8,.001), color = 'PL'),
                fun = function(x){pl$norm.const * x ^ -pl$mu}) +
  #PLB
  stat_function(aes(x = seq(2,8,.001), color = 'PLB'),
                fun = function(x){plb$norm.const * x ^ -plb$mu}) +
  #EXP
  stat_function(aes(x = seq(2,8,.001), color = 'EXP'),
                fun = function(x){exp$norm.const * exp$lambda *
                      exp(-exp$lambda * (x - abn$a))}) +
  #EXPB
  stat_function(aes(x = seq(2,8,.001), color = 'EXPB'),
                fun = function(x){expb$norm.const * exp(-exp$lambda * x)}) +
  scale_color_manual('Model', values = c('red', 'green', 'blue', 'purple')) +
  ggtitle('spawning') + xlim(0,200)


## Length
ggplot() + geom_histogram(data = filter(sb.speed, mean.sp.bl > 0,
                                        length.bin == '65-80'),
                     aes(x = mean.sp.bl, y = ..density..), binwidth = 0.5) +
  #PL
  stat_function(aes(x = seq(0,8,.001), color = 'PL'),
                fun = function(x){pl$norm.const * x ^ -pl$mu}) +
  #PLB
  stat_function(aes(x = seq(0,8,.001), color = 'PLB'),
                fun = function(x){plb$norm.const * x ^ -plb$mu}) +
  #EXP
  stat_function(aes(x = seq(0,8,.001), color = 'EXP'),
                fun = function(x){exp$norm.const * exp$lambda *
                      exp(-exp$lambda * (x - abn$a))}) +
  #EXPB
  stat_function(aes(x = seq(0,8,.001), color = 'EXPB'),
                fun = function(x){expb$norm.const * exp(-exp$lambda * x)}) +
  scale_color_manual('Model', values = c('red', 'green', 'blue', 'purple')) +
  ggtitle('>80')