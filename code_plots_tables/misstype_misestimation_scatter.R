library(tidyverse)
library(ggplot2)
library(missMethods)
library(here)
library(ggpubr)

# generate the full time-series, N = 1, T = 100

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
    
    return(Y[(N_burning + 1):N_tot])
  }
  
Y = arsim(beta_1 = 0.7)

series_full = data.frame(Y) %>% 
  mutate(Y_centered = Y - mean(Y)) %>% 
  mutate(Ylag = dplyr::lag(Y, n = 1, default = NA))

# scaterplot for the dataset without any missing data 

(scatter_full = ggplot(series_full, aes(x = Y_centered, y = Ylag)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ylim(-2, 2) + xlim (-2,2) +
  theme_minimal())


## MCAR

# introduce MCAR to data 
series_mcar = delete_MCAR(ds = data.frame(Y), cols_mis = 1, p = 0.4) %>% 
  mutate(Y_centered = Y - mean(Y, na.rm = TRUE)) %>% 
  mutate(Ylag = dplyr::lag(Y, n = 1, default = NA))

(scatter_mcar = ggplot(series_mcar, aes(x = Y_centered, y = Ylag)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    theme_minimal()) + 
    ylim(-2, 2) + xlim (-2,2)



# 2) missing in blocks, compliance = 0.6
Y[5:(5+(40))] = NA

series_block = data.frame(Y) %>% 
  mutate(Y_centered = Y-mean(Y, na.rm = TRUE)) %>% 
  mutate(Ylag = dplyr::lag(Y, n = 1, default = NA))

(scatter_block = ggplot(series_block, aes(x = Y_centered, y = Ylag)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    theme_minimal()) + 
  ylim(-2, 2) + xlim (-2,2)





# 3) extreme - onesided, compliance = 0.4

threshold_quantile = quantile(series_full$Y, 0.4)
Y[Y < threshold_quantile] = NA

series_extreme_oneside = data.frame(Y) %>% 
  mutate(Y_centered = Y-mean(Y, na.rm = TRUE)) %>% 
  mutate(Ylag = dplyr::lag(Y, n = 1, default = NA))

(scatter_oneside = ggplot(series_extreme_oneside, aes(x = Y_centered, y = Ylag)) + 
    geom_point() + 
    geom_smooth(method = "lm") + 
    theme_minimal()) + 
  ylim(-2, 2) + xlim (-2,2)


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