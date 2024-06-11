
library(ggplot2)
library(data.table)
library(ggpubr)

source('2. Produce descriptive statistics.R')

## lockdown period for charts:
lockdown <- annotate('rect',
                     xmin = as.Date(c('2020-03-26', '2020-11-05', '2021-01-06')),
                     xmax = as.Date(c('2020-06-23', '2020-12-02', '2021-03-08')),
                     ymin = 0, ymax = Inf, alpha = 0.4) 

### Comparison of volumes of care IS/NHS
plot_RTT_comp <- function(ccg_code = 'ENGLAND',
                     specialty,
                     quantiles = c(0.95, 0.50),
                     type,
                     chart_title = '',
                     start_date = NA){
  
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
  
  result$total_noNA <- ifelse(is.na(result$total.patients), 0, result$total.patients)
  result$num18.or.less_noNA <- ifelse(is.na(result$number.18.or.less), 0, result$number.18.or.less)
  result$Provider <- ifelse(result$independent == 'IS', 'Independent', 'NHS')
  
  result$date <- as.Date(paste0('01-',
                                substr(result$monthyear, 1, 3),
                                '-', 
                                substr(result$monthyear, 4, 5)),
                         format = '%d-%b-%y')
  
  if(!is.na(start_date)){
    
    result <- result[date >= as.Date(start_date), ]
    
  }
  
  chart_1_data <- data.frame(date = unique(result$date),
                             prop = result$total_noNA[result$independent == 'IS'] /
                               (result$total_noNA[result$independent == 'Non-IS'] +
                                  result$total_noNA[result$independent == 'IS']) * 100,
                             vol = result$total_noNA[result$independent == 'Non-IS'] +
                               result$total_noNA[result$independent == 'IS'])
  
  
  chart_2_data <- data.frame(date = unique(result$date),
                             prop = result$num18.or.less_noNA[result$independent == 'IS'] /
                               (result$num18.or.less_noNA[result$independent == 'Non-IS'] +
                                  result$num18.or.less_noNA[result$independent == 'IS']) * 100,
                             vol = result$num18.or.less_noNA[result$independent == 'Non-IS'] +
                               result$num18.or.less_noNA[result$independent == 'IS'])
  
  prop1 <- result$total_noNA[result$independent == 'IS'] /
    (result$total_noNA[result$independent == 'IS'] +
       result$total_noNA[result$independent == 'Non-IS'])
  prop1 <- rep(prop1, each = 2)
  
  ratio1 <- max(chart_1_data$vol) / max(chart_1_data$prop)
  
  ylim.vol1 <- c(0, max(chart_1_data$vol))
  ylim.pro1 <- c(0, max(chart_1_data$prop))
  
  b1 <- diff(ylim.vol1)/diff(ylim.pro1)
  
  if(ratio1 == Inf | is.na(ratio1)){
    
    p <- ggplot(data.frame())
    
  } else {
    
    p <-  ggplot(result, aes(date, total.patients, fill = Provider)) +
          geom_col() +
          geom_line(aes(y = prop1 * b1 * 100), color = 'black', linewidth = 2) +
          scale_y_continuous('Patient volume', sec.axis = sec_axis(~./ratio1)) +
          theme_minimal() +
          ggtitle('Proportion of patients with IS care\n delivered with patient volume') +
          theme(plot.title = element_text(size = 10),
                axis.title = element_text(size = 8))
    
    if(is.na(start_date) | as.Date(start_date) < lockdown$data$xmin[1]){
          p <- p + lockdown}
}

  prop2 <- result$num18.or.less_noNA[result$independent == 'IS'] /
    (result$num18.or.less_noNA[result$independent == 'IS'] +
       result$num18.or.less_noNA[result$independent == 'Non-IS'])
  prop2 <- rep(prop2, each = 2)
  
  ratio2 <- max(chart_2_data$vol) / max(chart_2_data$prop)
  
  ylim.vol2 <- c(0, max(chart_2_data$vol))
  ylim.pro2 <- c(0, max(chart_2_data$prop))
  
  b2 <- diff(ylim.vol2)/diff(ylim.pro2)

  if(ratio2 == Inf | is.na(ratio2)){
    
    q <- ggplot(data.frame())
    
  } else {
    
    q <- ggplot(result, aes(date, num18.or.less_noNA, fill = Provider)) +
          geom_col() +
          geom_line(aes(y = prop2 * b2 * 100), color = 'black', linewidth = 2) +
          scale_y_continuous('Patient volume', sec.axis = sec_axis(~./ratio2)) +
          theme_minimal() +
          ggtitle('Proportion of patients with IS care delivered\n in <18 weeks with patient volume') +
          theme(plot.title = element_text(size = 10),
                axis.title = element_text(size = 8))
    
      if(is.na(start_date) | as.Date(start_date) < lockdown$data$xmin[1]){
        q <- q + lockdown}
        
  }
  
  r <- ggplot(result, aes(x = date, y = weeks.50, colour = Provider)) +
    geom_line(linewidth=1) +
    ggtitle(chart_title) +
    ylab('Median weeks') +
    theme_minimal() +
    theme(axis.title = element_text(size = 8))
  
  if(is.na(start_date) | as.Date(start_date) < lockdown$data$xmin[1]){
    r <- r + lockdown}
  
  s <- ggplot(result, aes(x = date, y = rate.52wks.or.more, colour = Provider)) +
    geom_line(linewidth=1) +
    ggtitle(chart_title) +
    ylab('Proportion > 52 weeks') +
    theme_minimal() +
    theme(axis.title = element_text(size = 8))
  
  if(is.na(start_date) | as.Date(start_date) < lockdown$data$xmin[1]){
    s <- s + lockdown}
    
  
  plot <- ggarrange(p, q, r, s, common.legend = TRUE, legend = 'bottom') %>%
  annotate_figure(., top = text_grob(paste0('All patients ', specialty, ', pathway: ', type),
                                     face = 'bold'))
  
  return(plot)
}


#############################
chart_pathway <- c('completeadmitted', 'completenonadmitted')

n <- 1
## save charts
for (i in chart_pathway){
  
  for (j in all_specialties){
    
    plot_RTT_comp(specialty = j, type = i)
    ggsave(paste0('Charts/Chart_', i, '_', j, '.png'), plot = last_plot())
    
    print(paste0('Saved ', n, ' of ', length(chart_pathway) * length(all_specialties)))
    
    n <- n + 1
    
  }
  
}

## chart function to plot new clock starts only
plot_newRTT <- function(ccg_code = 'ENGLAND',
                        quantiles = c(0.95, 0.50),
                        specialty,
                        chart_title = ''){
  
  res <- list()
  
  j <- 1
  for (i in all_months){
    
    res[[j]] <- dashboard_stats_ccg(monthyear=i,
                                    ccg_code=ccg_code,
                                    specialty=specialty,
                                    quantiles=quantiles,
                                    type='newRTT',
                                    independent=0)
    res[[j+1]] <- dashboard_stats_ccg(monthyear=i,
                                      ccg_code=ccg_code,
                                      specialty=specialty,
                                      quantiles=quantiles,
                                      type='newRTT',
                                      independent=1)
    
    j <- j + 2
    
  }
  
  result <- rbindlist(res)
  
  result$total_noNA <- ifelse(is.na(result$total.patients), 0, result$total.patients)
  result$Provider <- ifelse(result$independent == 'IS', 'Independent', 'NHS')
  
  result$date <- as.Date(paste0('01-',
                                substr(result$monthyear, 1, 3),
                                '-', 
                                substr(result$monthyear, 4, 5)),
                         format = '%d-%b-%y')
  
  chart_1_data <- data.frame(date = unique(result$date),
                             prop = result$total_noNA[result$independent == 'IS'] /
                               (result$total_noNA[result$independent == 'Non-IS'] +
                                  result$total_noNA[result$independent == 'IS']) * 100,
                             vol = result$total_noNA[result$independent == 'Non-IS'] +
                               result$total_noNA[result$independent == 'IS'])
  
  prop1 <- result$total_noNA[result$independent == 'IS'] /
    (result$total_noNA[result$independent == 'IS'] +
       result$total_noNA[result$independent == 'Non-IS'])
  prop1 <- rep(prop1, each = 2)
  
  ratio1 <- max(chart_1_data$vol) / max(chart_1_data$prop)
  
  ylim.vol1 <- c(0, max(chart_1_data$vol))
  ylim.pro1 <- c(0, max(chart_1_data$prop))
  
  b1 <- diff(ylim.vol1)/diff(ylim.pro1)
  
  if(ratio1 == Inf | is.na(ratio1)){
    
    p <- ggplot(data.frame())
    
  } else {
    
    p <-  ggplot(result, aes(date, total.patients, fill = Provider)) +
      geom_col() +
      geom_line(aes(y = prop1 * b1 * 100), color = 'black', size = 2) +
      scale_y_continuous('Patient volume', sec.axis = sec_axis(~./ratio1)) +
      theme_minimal() +
      ggtitle(paste0('Monthly new clock starts with proportion by provider - ', specialty)) +
      theme(plot.title = element_text(size = 10),
            axis.title = element_text(size = 8)) +
      lockdown
  }
  
  return(p)
}

n <- 1
## save new RTT charts
for (j in all_specialties){
    
    plot_newRTT(specialty = j)
    ggsave(paste0('Charts/Update/Chart_newRTT_', j, '.png'), plot = last_plot())
    
    print(paste0('Saved ', n, ' of ', length(all_specialties)))
    
    n <- n + 1
    
}

s3sync(files = dir('Charts_2024_05_29', recursive = TRUE),
       bucket = IHT_bucket, direction = 'upload')
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
data_pathway <- c('completeadmitted', 'completenonadmitted', 'newRTT')
## save processed data
for (i in data_pathway){
  
  for (j in all_specialties){
    
    data_list[[n]] <- RTT_comp_data(specialty = j, type = i)
    
    print(paste0('Processed ', n, ' of ', length(data_pathway) * length(all_specialties)))
    
    n <- n + 1
    
  }
  
}

data_export <- rbindlist(data_list)
write_csv(data_export, file = 'RTT_processed.csv')

put_object(file = 'RTT_processed.csv',
           object = paste0(RTT_subfolder, '/RTT_processed.csv'),
           bucket = IHT_bucket)
unlink('RTT_processed.csv')
