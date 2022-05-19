
library(ggplot2)
library(data.table)
library(ggpubr)

## Basic time series analysis and visualisation
plot_RTT <- function(provider = 'ENGLAND',
                     specialty,
                     quantiles = c(0.95, 0.50),
                     type,
                     chart_title = ''){

  res <- list()
  j <- 1
  for (i in all_months){
    
    res[[j]] <- dashboard_stats_provider(monthyear=i,
                             provider=provider,
                             specialty=specialty,
                             quantiles=quantiles,
                             type=type)
    j <- j + 1
    
  }
  
  result <- rbindlist(res)
  
  result$date <- as.Date(paste0('01-',
                               substr(result$monthyear, 1, 3),
                               '-', 
                               substr(result$monthyear, 4, 5)),
                         format = '%d-%b-%y')
                               
  p <- ggplot(result, aes(x = date, y = total.patients)) +
    geom_line() +
    ggtitle(chart_title)
 # abline(v = as.Date('2020-03-26'))
  
  return(p)
}

plot_RTT(specialty = 'Total', type = 'incomplete',
         chart_title = 'Total patients on incomplete pathways')

plot_RTT(specialty = 'Total', type = 'completeadmitted',
         chart_title = 'Total patients on completed pathways that have been admitted')

plot_RTT(specialty = 'Total', type = 'completenonadmitted',
         chart_title = 'Total patients on completed pathways that have been admitted')


plot_RTT(specialty = 'Rheumatology', type = 'incomplete')


### Comparison of volumes of care IS/NHS
plot_RTT_comp <- function(ccg_code = 'ENGLAND',
                     specialty,
                     quantiles = c(0.95, 0.50),
                     type,
                     chart_title = ''){
  
  res <- list()

  j <- 1
  for (i in all_months){
    
    res[[j]] <- dashboard_stats_ccg(monthyear=i,
                                         ccg_code=ccg_code,
                                         specialty=specialty,
                                         quantiles=quantiles,
                                         type=type,
                                         independent=0)
    res[[j+1]] <- dashboard_stats_ccg(monthyear=i,
                                         ccg_code=ccg_code,
                                         specialty=specialty,
                                         quantiles=quantiles,
                                         type=type,
                                         independent=1)
    
    j <- j + 2
    
  }
  
  result <- rbindlist(res)
  
  result$date <- as.Date(paste0('01-',
                                substr(result$monthyear, 1, 3),
                                '-', 
                                substr(result$monthyear, 4, 5)),
                         format = '%d-%b-%y')
  
  p <- ggplot(result, aes(x = date, y = total.patients / 1000, colour = independent)) +
    geom_line() +
    ggtitle(chart_title) +
    theme_classic()

  q <- ggplot(result, aes(x = date, y = number.18.or.less / 1000, colour = independent)) +
    geom_line() +
    ggtitle(chart_title) +
    theme_classic()
  
  r <- ggplot(result, aes(x = date, y = rate.18wks.or.less, colour = independent)) +
    geom_line() +
    ggtitle(chart_title) +
    theme_classic()
  
  s <- ggplot(result, aes(x = date, y = rate.52wks.or.more, colour = independent)) +
    geom_line() +
    ggtitle(chart_title) +
    theme_classic()
  
  plot <- ggarrange(p, q, r, s, common.legend = TRUE, legend = 'right') %>%
  annotate_figure(., top = text_grob(paste0('All patients ', specialty, ', pathway: ', type)))
  
  return(plot)
}

plot_RTT_comp(specialty = 'Total', type = 'incomplete')
plot_RTT_comp(specialty = 'Total', type = 'completeadmitted')
plot_RTT_comp(specialty = 'Total', type = 'completenonadmitted')
plot_RTT_comp(specialty = 'Total', type = 'incompleteDTA')
plot_RTT_comp(specialty = 'Total', type = 'newRTT')

plot_RTT_comp(specialty = 'Cardiology', type = 'incomplete')
plot_RTT_comp(specialty = 'Cardiology', type = 'completeadmitted')
plot_RTT_comp(specialty = 'Cardiology', type = 'completenonadmitted')
plot_RTT_comp(specialty = 'Cardiology', type = 'incompleteDTA')
plot_RTT_comp(specialty = 'Cardiology', type = 'newRTT')
