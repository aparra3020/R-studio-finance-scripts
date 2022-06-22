library(tidyverse)
library(tidyquant)
library(timetk)
library(highcharter)

library(binancer)

BTC = binance_klines("BTCUSDT", "1d")

BTC = BTC[,c(1:5)]
BTC = as_tibble(BTC)

BTC_xts = BTC %>% tk_xts(silent = TRUE)

BTCmacd = BTC %>% tq_mutate(select = close, mutate_fun = MACD, nFast= 5, 
                  nSlow      = 20, 
                  nSig       = 5, 
                  maType     = SMA) %>%  mutate(diff = macd - signal) 

BTCmacd_xts = BTCmacd %>% select(-c(close, open, high, low)) %>% tk_xts(silent = TRUE)
  
highchart(type = "stock")  %>% 
  hc_yAxis_multiples(
    list(top = "0%", height = "60%", title = list(text = "Precio")),
    list(top = "60%", height = "40%", title = list(text = "indicadores"))) %>% 
  hc_add_series(BTC_xts,yAxis = 0, color = "blue") %>% 
  hc_add_series(BTCmacd_xts[,1], yAxis = 1, type = "line", name = "MACD") %>% 
  hc_add_series(BTCmacd_xts[,2], yAxis = 1, type = "line", name = "MACD1") %>% 
  hc_add_series(BTCmacd_xts[,3], yAxis = 1, type = "line", name = "MACD2") %>% 
  hc_title(text = "BTC") %>% hc_add_theme(hc_theme_flat())
 

