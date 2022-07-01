#required libraries
library(highcharter)
library("tseries")
library("quantmod")
library("PortfolioAnalytics")
library("DEoptim")
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library("PerformanceAnalytics")
library("magrittr")
library("dygraphs")
library("TTR")
library(plotly)
library(timetk)
library(tidyverse)
library(tidyquant)

#SP500 index info
sp500 = tq_index("SP500")

#Download sp 500 stocks data
stocks = tq_get(sp500$symbol, from="2020-01-01",to="2021-01-01")
#SP 500 historic price
Index_info = tq_get("^GSPC",from="2020-01-01",to="2021-01-01" )
#merge SP500 with stocks dataframe
stocks = bind_rows(stocks,Index_info)

risk_free_rate = 0.0001
#Extraction of stock tickers present throughout the period 2020 to 2021
names = stocks %>% count(symbol) %>% 
  dplyr::filter(n == 253) %>% select(symbol) %>% pull()

#filtering stocks dataframe
stocks = stocks %>% filter(symbol %in% names)

#Daily log returns calculation
data = stocks %>% select(date, symbol, adjusted) %>% group_by(symbol) %>% 
  tq_mutate(select = adjusted, mutate_fun = ROC, col_rename = "Returns") %>% 
  select(-adjusted) %>% ungroup() %>% na.omit()

#average daily returns
mean_returns = data %>% group_by(symbol) %>% 
  summarise(Returns = mean(Returns)) %>% ungroup()

#Returns standard desviation 
Desvest = data %>% group_by(symbol) %>% 
  summarise(stdesvest = sd(Returns)) %>% ungroup()

#convert dataframe to time series format
time_series = data %>% pivot_wider(names_from = symbol, values_from = Returns) %>% 
  tk_xts(silent = TRUE)

{stocks} %>%
  dplyr::group_by(date, symbol) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) %>% view()

#correlation matrix
M_corr = cor(time_series, use = "complete.obs")
#covariance matrix
M_cov = cov(time_series, use = "complete.obs")

#dominance graph
dominance_chart = mean_returns %>% left_join(Desvest,by = "symbol") %>% 
  hchart(type = "scatter", hcaes(x = ((1+Returns)^254-1)*100, 
                                 y = (stdesvest*sqrt(254)*100), group = symbol)) %>% 
  hc_plotOptions(
    series = list(
      showInLegend = FALSE
    )) %>% hc_tooltip(valueDecimals = 2, ValueSuffix = "%") %>% 
  hc_yAxis(
    title = list(text = "% of returns"),
    labels = list(format = "{value}%")
  ) %>% hc_xAxis(
    title = list(text = "% of risk"),
    labels = list(format = "{value}%")
  ) %>% hc_title(
    text = "Dominance chart"
  ) %>% hc_subtitle(text = "stocks SP 500")

dominance_chart

#portfolio creation
portafolio = portfolio.spec(assets = names)
#portfolio constraints 
portafolio = add.constraint(portfolio = portafolio, type = "box", 
                            min = -1, max = 1)
portafolio = add.constraint(portfolio = portafolio, type = "full_investment")

#Min variance portfolio
min_var = add.objective(portfolio = portafolio, type = "risk", name = "var")

#Optimization
opt_minvar = optimize.portfolio(R = time_series, portfolio = min_var, optimize_method = "ROI")
opt_minvar

#max return portfolio
max_ret = add.objective(portfolio = portafolio, type = "return", name = "mean")

#Optimization
opt_maxret = optimize.portfolio(time_series, max_ret, optimize_method = "ROI")
opt_maxret

#Montecarlo simulation
Random_weights = random_portfolios(portafolio, permutations = 50000, 
                                   rp_method = "simplex")

#Calculating 
risk_montecarlo <- apply(Pesos_aleatorios, 1, function(x){
  return(sqrt(matrix(x, nrow = 1) %*% M_cov %*% matrix(x, ncol = 1)))
})

returns_montecarlo <- apply(Pesos_aleatorios, 1, function(x){
  return(x %*% colMeans(time_series, na.rm = "TRUE"))
})

Sr_montecarlo <- (returns_montecarlo-rf)/risk_montecarlo

port_20_ef = add.objective(portfolio = portafolio, type = "risk", name = "var") 
port_20_ef = add.objective(portfolio = port_20_ef, type = "return", name = "mean")


frontera = create.EfficientFrontier(R = time_series, portfolio = port_20_ef,
                                    type = "mean-sd", n.portfolios = 50)

summary(frontera)

dg = as_tibble(frontera$frontier[,c(2,1)])

dg = dg %>% mutate(SR = (mean-rf)/StdDev)


g2 <- plot_ly() %>%
  add_trace(x = risk_montecarlo, y = returns_montecarlo, color = Sr_montecarlo, 
            mode = "markers", type = "scattergl", showlegend = F,
            marker = list(size = 5, opacity = 0.5, 
                          colorbar = list(title = "Sharpe Ratio"))) %>%
  add_trace(data = dg, x = ~StdDev, y = ~mean ,mode = "markers", 
            type = "scattergl")%>% layout(title = "Frontera Eficiente",
                                          yaxis = list(title = "Rendimientos promedio", tickformat = ".2%"),
                                          xaxis = list(title = "varianza", tickformat = ".2%"))
g2

portafolios_profe = as_tibble(frontera$frontier[,c(1:23)])

portafolios_profe = portafolios_profe %>% mutate(SR = (mean-rf)/StdDev) %>% 
  rownames_to_column() %>% arrange(desc(SR)) %>% select(-out) 

pesos_port = portafolios_profe %>% slice_head() %>% select(-mean, -SR, -StdDev, -rowname) %>% 
  pivot_longer(names_to = "symbol", values_to = "peso", cols = w.AAPL:w.PAYC) %>% 
  separate(symbol, c("trash", "accion")) %>% select(-trash)

data_backtesting = tq_get(pesos_port$accion, from = "2021-03-10", to = "2021-05-21")

data_backtesting = data_backtesting %>% select(date, symbol, adjusted) %>%
  group_by(symbol) %>% 
  tq_mutate(select = adjusted, mutate_fun = ROC, col_rename = "returns") %>% 
  na.omit() %>% select(-adjusted)


backtesting = tq_portfolio(data = data_backtesting, assets_col = symbol, 
                           returns_col = returns, weights = pesos_port)

backtesting %>%  tq_transmute(select = portfolio.returns, to.weekly) %>% 
  plot_time_series(.date_var = date, 
                   .value = portfolio.returns, .smooth = FALSE, 
                   .title = "Rendimiento semanal del portafolio") 

backtesting_indice = tq_get("^GSPC", from = "2021-03-10", to = "2021-05-21")
backtesting_indice = backtesting_indice %>%  select(date, adjusted) %>%
  tq_mutate(select = adjusted, mutate_fun = ROC, col_rename = "returns") %>% 
  na.omit() %>% select(-adjusted) 

g3 = backtesting %>% left_join(backtesting_indice) %>% 
  rename_all(recode, portfolio.returns = "portafolio", returns = "SP500") %>% 
  pivot_longer(cols = portafolio:SP500, 
               names_to = "symbol", values_to = "Returns") %>% 
  plot_time_series(.date_var = date, .color_var = symbol,
                   .value = Returns, .smooth = FALSE, 
                   .title = "Rendimiento semanal del portafolio") 
g3  

cummulative = backtesting %>% left_join(backtesting_indice) %>% 
  rename_all(recode, portfolio.returns = "portafolio", returns = "SP500") %>% 
  pivot_longer(cols = portafolio:SP500, 
               names_to = "symbol", values_to = "Returns") %>% group_by(symbol) %>% 
  mutate(retorno_total = cumsum(Returns)) %>% select(-Returns) %>% ungroup() %>% 
  plot_time_series(.date_var = date, .color_var = symbol,
                   .value = retorno_total, .smooth = FALSE, 
                   .title = "Rendimiento semanal del portafolio") 
cummulative


promedios_portafolio = backtesting %>% left_join(backtesting_indice) %>% 
  rename_all(recode, portfolio.returns = "portafolio", returns = "SP500") %>% 
  pivot_longer(cols = portafolio:SP500, 
               names_to = "symbol", values_to = "Returns") %>% 
  group_by(symbol) %>% summarise(retornos = (1+mean(Returns))^254-1)

riesgo_portafolio = backtesting %>% left_join(backtesting_indice) %>% 
  rename_all(recode, portfolio.returns = "portafolio", returns = "SP500") %>% 
  pivot_longer(cols = portafolio:SP500, 
               names_to = "symbol", values_to = "Returns") %>% 
  group_by(symbol) %>% summarise(desviacion = sd(Returns)*sqrt(254))

corr_port = backtesting %>% left_join(backtesting_indice) %>% 
  rename_all(recode, portfolio.returns = "portafolio", returns = "SP500") %>%
  select(-date) %>% cor()

beta_portafolio = (corr_port[1,2]*riesgo_portafolio[1,2])/riesgo_portafolio[2,2]

indicadores_desempeno = tibble(RS_portafolio = (promedios_portafolio[1,2]-rf)/riesgo_portafolio[1,2],
                               RS_sp = (promedios_portafolio[2,2]-rf)/riesgo_portafolio[2,2],
                               RT_portafolio = (promedios_portafolio[1,2]-rf)/beta_portafolio,
                               RT_sp = (promedios_portafolio[2,2]-rf)/beta_portafolio)

Trackerror = backtesting %>% left_join(backtesting_indice) %>% 
  rename_all(recode, portfolio.returns = "portafolio", returns = "SP500") %>%
  transmute(diferencia = portafolio-SP500) %>% summarise(error = sd(diferencia))


