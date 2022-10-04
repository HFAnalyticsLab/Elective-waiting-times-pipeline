
## Ration of clock starts and completions


library(ggplot2)
library(data.table)

source('2. Produce descriptive statistics.R')

## lockdown period for charts:
lockdown <- annotate('rect',
                     xmin = as.Date(c('2020-03-26', '2020-11-05', '2021-01-06')),
                     xmax = as.Date(c('2020-06-23', '2020-12-02', '2021-03-08')),
                     ymin = 0, ymax = Inf, alpha = 0.4) 




## chart function to plot new clokc starts only
plot_ratio <- function(ccg_code = 'ENGLAND',
                        specialty,
                        quantiles = c(0.95, 0.50),
                        chart_title = ''){
  
  new <- list()
  adm <- list()
  non <- list()
  
  j <- 1
  for (i in all_months){
    
    new[[j]] <- dashboard_stats_ccg(monthyear=i,
                                    ccg_code=ccg_code,
                                    specialty=specialty,
                                    quantiles=quantiles,
                                    type='newRTT',
                                    independent=0)
    new[[j+1]] <- dashboard_stats_ccg(monthyear=i,
                                      ccg_code=ccg_code,
                                      specialty=specialty,
                                      quantiles=quantiles,
                                      type='newRTT',
                                      independent=1)
    
    adm[[j]] <- dashboard_stats_ccg(monthyear=i,
                                    ccg_code=ccg_code,
                                    specialty=specialty,
                                    quantiles=quantiles,
                                    type='completeadmitted',
                                    independent=0)
    adm[[j+1]] <- dashboard_stats_ccg(monthyear=i,
                                      ccg_code=ccg_code,
                                      specialty=specialty,
                                      quantiles=quantiles,
                                      type='completeadmitted',
                                      independent=1)

    non[[j]] <- dashboard_stats_ccg(monthyear=i,
                                    ccg_code=ccg_code,
                                    specialty=specialty,
                                    quantiles=quantiles,
                                    type='completenonadmitted',
                                    independent=0)
    non[[j+1]] <- dashboard_stats_ccg(monthyear=i,
                                      ccg_code=ccg_code,
                                      specialty=specialty,
                                      quantiles=quantiles,
                                      type='completenonadmitted',
                                      independent=1)
    
    j <- j + 2
    
  }
  
  result_new <- rbindlist(new) %>% .[, .(monthyear,
                                         ccg,
                                         ccg_name,
                                         specialty,
                                         type,
                                         independent,
                                         total.patients)]
  result_fin <- rbind(rbindlist(adm), rbindlist(non)) %>% .[, .(monthyear,
                                                                ccg,
                                                                ccg_name,
                                                                specialty,
                                                                type,
                                                                independent,
                                                                total.patients)]
  
  fin <- result_fin[, sum(total.patients), by =  .(monthyear,
                                            ccg,
                                            ccg_name,
                                            specialty,
                                            independent)]
  
  result <- merge(result_new, fin, by = c('monthyear', 'ccg', 'ccg_name', 'specialty', 'independent'))                                       
  result$date <- as.Date(paste0('01-',
                                substr(result$monthyear, 1, 3),
                                '-', 
                                substr(result$monthyear, 4, 5)),
                         format = '%d-%b-%y')
  
  result[, prop := total.patients / V1]
  result[, Provider := fifelse(independent == 'IS', 'IS', 'NHS')]
  
    p <-  ggplot(result, aes(date, prop, color = Provider)) +
      geom_line(size = 2) +
      theme_minimal() +
      ggtitle(paste0('Ratio of new RTTs to completed pathways - ', specialty)) +
      ylab('Pathways started for each one finished') +
      geom_hline(yintercept = 1, color = 'grey', linetype = 'dashed') +
      theme(plot.title = element_text(size = 10),
            axis.title = element_text(size = 8)) +
      lockdown
  
  return(p)
    
}

n <- 1
## save new RTT charts
for (j in all_specialties){
  
  plot_ratio(specialty = j)
  ggsave(paste0('Charts/Ratio/Chart_', j, '.png'), plot = last_plot())
  
  print(paste0('Saved ', n, ' of ', length(all_specialties)))
  
  n <- n + 1
  
}

