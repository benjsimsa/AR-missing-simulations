library(tidyverse)
library(ggplot2)
library(missMethods)
library(here)
library(ggpubr)

# Create the time-series 
arsim = function(N = 100, beta_0 = 0, beta_1 = 0.5, sigma = 0.5){
  set.seed(1)
  N_burning = 1000
  N_tot = N + N_burning
  error = rnorm(N_tot, 0, sigma)   
  mu = beta_0 / (1-beta_1)
  
  Y = rep(0, N_tot)  #[0 for i in 1:N_tot]
  
  Y[1]= mu + error[1]  # Initial value
  
  for (n in 2:N_tot){
    Y[n] = beta_0 + beta_1 * Y[n-1] + error[n]   # Simulate Dependent Variables
  }
  Y[(N_burning + 1):N_tot]
}

series = as.data.frame(arsim())

# 1) MCAR, compliance = 0.7 

# introduce MCAR to data 
tm = c(1:100)
series_mcar = delete_MCAR(ds = series, cols_mis = 1, p = 0.3)

data_mcar = data.frame(tm, series, series_mcar)
colnames(data_mcar) = c("time", "full_series", "series_mcar")
(plot_MCAR = ggplot(data_mcar, aes(x = tm)) + 
                        
                        geom_line(aes(y = series_mcar),
                                  color = "blue",
                                  size = 1,
                                  alpha = 0.7) +
                        geom_point(aes(y = series_mcar),
                                   color = "blue",
                                   size = 1) + 
                        
                        geom_line(aes(y = full_series,
                                      alpha = 0.3),
                                  show.legend = FALSE) +
                        geom_point(aes(y = full_series,
                                       alpha = 0.25),
                                   show.legend = FALSE) +
                        ggtitle("a) Data missing completely at random") +
                        xlab("") +
                        ylab("Process value") + 
                        theme_minimal())


# 2) missing in blocks, compliance = 0.7
block_full = data_mcar$full_series
block_missing = data_mcar$full_series
df_block = data.frame(tm, block_full, block_missing)
df_block$block_missing[5:(5+(30))] = NA

(plot_block = ggplot(df_block, aes(x = tm)) + 
    
    geom_line(aes(y = block_missing),
              color = "blue",
              size = 1,
              alpha = 0.7) +
    geom_point(aes(y = block_missing),
               color = "blue",
               size = 1) + 
    
    geom_line(aes(y = block_full,
                  alpha = 0.3),
              show.legend = FALSE) +
    geom_point(aes(y = block_full,
                   alpha = 0.25),
               show.legend = FALSE) +
    ggtitle("b) Block of missing data") +
    xlab("") +
    ylab("Process value") + 
    theme_minimal())



# 3) extreme - onesided, compliance = 0.7 

threshold_quantile = quantile(data_mcar$full_series, 0.3)

oneside_full = data_mcar$full_series
oneside_missing = data_mcar$full_series
df_oneside = data.frame(tm, oneside_full, oneside_missing)
df_oneside$oneside_missing[df_oneside$oneside_missing < threshold_quantile] = NA

(plot_oneside = ggplot(df_oneside, aes(x = tm)) + 
    
    geom_line(aes(y = oneside_missing),
              color = "blue",
              size = 1,
              alpha = 0.7) +
    geom_point(aes(y = oneside_missing),
               color = "blue",
               size = 1) + 
    
    geom_line(aes(y = oneside_full,
                  alpha = 0.3),
              show.legend = FALSE) +
    geom_point(aes(y = oneside_full,
                   alpha = 0.25),
               show.legend = FALSE) +
    geom_hline(yintercept = threshold_quantile, 
               linetype = "dashed", 
               color = "red") + 
    ggtitle("c) (1 - compliance) lowest values missing") +
    xlab("") +
    ylab("Process value") + 
    theme_minimal())




# 4) extreme - twosided, compliance = 0.7 
threshold_quantile_upper = quantile(data_mcar$full_series, 0.85)
threshold_quantile_lower = quantile(data_mcar$full_series, 0.15)


twoside_full = data_mcar$full_series
twoside_missing = data_mcar$full_series
df_twoside = data.frame(tm, twoside_full, twoside_missing)
df_twoside$twoside_missing[df_twoside$twoside_missing < threshold_quantile_lower] = NA
df_twoside$twoside_missing[df_twoside$twoside_missing > threshold_quantile_upper] = NA


(plot_twoside = ggplot(df_twoside, aes(x = tm)) + 
    
    geom_line(aes(y = twoside_missing),
              color = "blue",
              size = 1,
              alpha = 0.7) +
    geom_point(aes(y = twoside_missing),
               color = "blue",
               size = 1) + 
    
    geom_line(aes(y = twoside_full,
                  alpha = 0.3),
              show.legend = FALSE) +
    geom_point(aes(y = twoside_full,
                   alpha = 0.25),
               show.legend = FALSE) +
    geom_hline(yintercept = threshold_quantile_lower, 
               linetype = "dashed", 
               color = "red") + 
    geom_hline(yintercept = threshold_quantile_upper, 
               linetype = "dashed", 
               color = "red") + 
    ggtitle("d) (1 - compliance)/2 lowest and highest values missing") +
    xlab("Beep") +
    ylab("Process value") + 
    theme_minimal())



plot = ggarrange(plot_MCAR, plot_block, plot_oneside, plot_twoside,
                 nrow = 4)

ggsave(plot = plot, filename = here::here("plots", "missing_patterns_comparison.png"))
