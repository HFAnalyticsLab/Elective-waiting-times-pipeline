
library(ggplot2)
library(data.table)
library(ggpubr)

source('2. Produce descriptive statistics.R')

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
  
  result[, .N, by = independent]
  #p <- ggplot(result, aes(x = date, y = total.patients / 1000, colour = independent)) +
  #  geom_line() +
  #  ggtitle(chart_title) +
  #  theme_classic()
  chart_1_data <- data.frame(date = unique(result$date),
                             prop = result$total.patients[result$independent == 'IS'] /
                               (result$total.patients[result$independent == 'Non-IS'] +
                               result$total.patients[result$independent == 'IS']) * 100)
  
  
  chart_2_data <- data.frame(date = unique(result$date),
                             prop = result$number.18.or.less[result$independent == 'IS'] /
                               (result$number.18.or.less[result$independent == 'Non-IS'] +
                                result$number.18.or.less[result$independent == 'IS']) * 100)
  
  p <- ggplot(chart_1_data, aes(x = date, y = prop)) +
    geom_line() +
    ggtitle(chart_title) +
    theme_classic() + 
    expand_limits(y = 0) +
    ggtitle('Proportion of patients with IS care delivered')

  #q <- ggplot(result, aes(x = date, y = number.18.or.less / 1000, colour = independent)) +
  #  geom_line() +
  #  ggtitle(chart_title) +
  #  theme_classic()
  
  q <- ggplot(chart_2_data, aes(x = date, y = prop)) +
    geom_line() +
    ggtitle(chart_title) +
    theme_classic() + 
    expand_limits(y = 0) +
    ggtitle('Proportion of patients with care delivered <18 weeks IS care delivered')
  
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


chart_pathway <- c('completeadmitted', 'completenonadmitted')

n <- 1
## save charts
for (i in chart_pathway){
  
  for (j in all_specialties){
    
    plot_RTT_comp(specialty = j, type = i)
    ggsave(paste0('Chart_', i, '_', j, '.png'), plot = last_plot())
    
    print(paste0('Saved ', n, ' of ', length(chart_pathway) * length(all_specialties)))
    
    n <- n + 1
    
  }
  
}

## get data only to export to excel
RTT_comp_data <- function(ccg_code = 'ENGLAND',
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
 
  return(result) 
}


n <- 1
data_list <- list()
## save processed data
for (i in chart_pathway){
  
  for (j in all_specialties){
    
    data_list[[n]] <- RTT_comp_data(specialty = j, type = i)
    
    print(paste0('Processed ', n, ' of ', length(chart_pathway) * length(all_specialties)))
    
    n <- n + 1
    
  }
  
}
