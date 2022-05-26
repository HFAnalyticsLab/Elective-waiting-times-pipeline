
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
  
  chart_1_data <- data.frame(date = unique(result$date),
                             prop = result$total.patients[result$independent == 'IS'] /
                               (result$total.patients[result$independent == 'Non-IS'] +
                                  result$total.patients[result$independent == 'IS']) * 100,
                             vol = result$total.patients[result$independent == 'Non-IS'] +
                               result$total.patients[result$independent == 'IS'])
  
  
  chart_2_data <- data.frame(date = unique(result$date),
                             prop = result$number.18.or.less[result$independent == 'IS'] /
                               (result$number.18.or.less[result$independent == 'Non-IS'] +
                                  result$number.18.or.less[result$independent == 'IS']) * 100,
                             vol = result$number.18.or.less[result$independent == 'Non-IS'] +
                               result$number.18.or.less[result$independent == 'IS'])
  
  ratio1 <- max(chart_1_data$vol) / max(chart_1_data$prop)
  
  ylim.vol <- c(0, max(chart_1_data$vol))
  ylim.pro <- c(0, max(chart_1_data$prop))
  
  b <- diff(ylim.vol)/diff(ylim.pro)
  a <- 0
  
  p <- ggplot(chart_1_data, aes(date, vol)) +
    geom_col(fill = 'light grey') +
    geom_line(aes(y = a + prop*b), color = 'red', size = 2) +
    scale_y_continuous('Patient volume', sec.axis = sec_axis(~./ratio1)) +
    theme_minimal() +
    ggtitle('Proportion of patients with IS care delivered with patient volume')
  
  ratio2 <- max(chart_2_data$vol) / max(chart_2_data$prop)
  
  ylim.vol <- c(0, max(chart_2_data$vol))
  ylim.pro <- c(0, max(chart_2_data$prop))
  
  b <- diff(ylim.vol)/diff(ylim.pro)
  a <- 0
  
  q <- ggplot(chart_2_data, aes(date, vol)) +
    geom_col(fill = 'light grey') +
    geom_line(aes(y = a + prop*b), color = 'red', size = 2) +
    scale_y_continuous('Patient volume', sec.axis = sec_axis(~./ratio2)) +
    theme_minimal() +
  ggtitle('Proportion of patients with care delivered <18 weeks IS care delivered')
  
  r <- ggplot(result, aes(x = date, y = rate.18wks.or.less, colour = independent)) +
    geom_line(size=1) +
    ggtitle(chart_title) +
    theme_minimal()
  
  s <- ggplot(result, aes(x = date, y = rate.52wks.or.more, colour = independent)) +
    geom_line(size=1) +
    ggtitle(chart_title) +
    theme_minimal()
  
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

data_export <- rbindlist(data_list)
write_csv(data_export, file = 'RTT_processed.csv')
