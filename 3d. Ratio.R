
## Ration of clock starts and completions


library(ggplot2)
library(data.table)
library(ggpubr)

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
  
  result_adm <- rbindlist(adm) %>% .[, .(monthyear,
                                         ccg,
                                         ccg_name,
                                         specialty,
                                         independent,
                                         total.patients)]
  
  result <- merge(result_new, fin, by = c('monthyear', 'ccg', 'ccg_name', 'specialty', 'independent'))
  result <- merge(result, result_adm, by = c('monthyear', 'ccg', 'ccg_name', 'specialty', 'independent'))
  
  
  comb <- result[, lapply(.SD, sum), by =  .(monthyear,
                                     ccg,
                                     ccg_name,
                                     specialty),
         .SDcols = c('total.patients.x', 'V1', 'total.patients.y')]
  
  comb$independent <- 'Total'
  comb$type <- 'newRTT'
  result <- rbind(result, comb)
    result$date <- as.Date(paste0('01-',
                                  substr(result$monthyear, 1, 3),
                                  '-', 
                                  substr(result$monthyear, 4, 5)),
                           format = '%d-%b-%y')
  result[, prop_all := total.patients.x / V1]
  result[, Provider := fifelse(independent == 'IS', 'IS', fifelse(independent == 'Non-IS', 'NHS', 'Total'))]
  
    p <-  ggplot(result, aes(date, prop_all, color = Provider)) +
      geom_line(aes(size = Provider)) +
      scale_size_manual(values =  c(IS = 1, NHS = 1, Total = 2)) +
      theme_minimal() +
 #     scale_y_continuous(limits = c(0, max(result$prop_adm))) +
      ggtitle(paste0('Ratio of new RTTs to completed pathways - ', specialty)) +
      ylab('Pathways started for each one finished') +
      geom_hline(yintercept = 1, color = 'grey', linetype = 'dashed') +
      theme(plot.title = element_text(size = 10),
            axis.title = element_text(size = 8)) +
      lockdown +
      scale_color_manual(values = c(IS = '#F8766D', NHS = '#619CFF', Total = 'black'))
    
    result[, prop_adm := total.patients.x / total.patients.y]
    #result[, prop_non := total.patients.x / (V1 - total.patients.y)]
    
    q <-  ggplot(result, aes(date, prop_adm, color = Provider)) +
      geom_line(aes(size = Provider)) +
      scale_size_manual(values =  c(IS = 1, NHS = 1, Total = 2)) +
      theme_minimal() +
  #    scale_y_continuous(limits = c(0, max(result$prop_adm))) +
      ggtitle(paste0('Ratio of new RTTs to admitted pathways - ', specialty)) +
      ylab('Pathways started for each one admitted') +
      geom_hline(yintercept = 1, color = 'grey', linetype = 'dashed') +
      theme(plot.title = element_text(size = 10),
            axis.title = element_text(size = 8)) +
      lockdown +
      scale_color_manual(values = c(IS = '#F8766D', NHS = '#619CFF', Total = 'black'))
    
    
    plot <- ggarrange(p, q, common.legend = TRUE, legend = 'bottom', align = 'hv')
    
    return(plot)
    
}

n <- 1
## save new RTT charts
for (j in all_specialties){
  
  plot_ratio(specialty = j)
  ggsave(paste0('Charts/Ratio/Chart_', j, '.png'), plot = last_plot())
  
  print(paste0('Saved ', n, ' of ', length(all_specialties)))
  
  n <- n + 1
  
}

# move all plots to s3
s3sync(path = 'Charts/',
       bucket = IHT_bucket,
       prefix = 'RTT waiting times data/Charts/',
       direction = 'upload')
unlink('Charts', recursive = TRUE)
