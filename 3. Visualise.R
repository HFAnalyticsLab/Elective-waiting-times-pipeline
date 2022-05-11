
library(ggplot2)
library(data.table)

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

  return(p)
}

plot_RTT(specialty = 'Total', type = 'incomplete')

plot_RTT(specialty = 'Total', type = 'completeadmitted')

plot_RTT(specialty = 'Total', type = 'completenonadmitted')

plot_RTT(specialty = 'Rheumatology', type = 'incomplete')
