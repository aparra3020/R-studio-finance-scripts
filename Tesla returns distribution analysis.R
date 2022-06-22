#Required libraries

library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(tidyquant)
library(nortest)

#Download tesla data from yahoo finance
TSLA = tq_get("TSLA", from = "2020-04-01", to = "2022-05-01")

#select adjusted values and transformation to log daily returns
TSLAr = TSLA %>% select(date, adjusted) %>% 
  tq_mutate(select = adjusted, mutate_fun = ROC, col_rename = "returns") %>% 
  na.omit()

#Main metrics
metrics = TSLAr %>% select(returns) %>% na.omit() %>% 
  summarise( expected_returns = mean(returns), 
             std_desv = sd(returns), 
             variance = sd(returns)^2, 
            var_coef =  std_desv/expected_returns) 

#returns histogram
hist(TSLAr %>%  pull(returns),main = "Returns distributions",
     xlab = "returns", breaks = 30)
curve(dnorm(x, mean=metrics$expected_returns,
            sd=metrics$std_desv),
      col="darkblue", lwd=2, add=TRUE, yaxt="n")


#normality tests
lillie.test(TSLAr %>% pull(returns))
pearson.test(TSLAr %>% pull(returns))
ad.test(TSLAr %>% pull(returns))
cvm.test(TSLAr %>% pull(returns))
sf.test(TSLAr %>% pull(returns))
kurtosis(TSLAr %>% pull(returns))
skewness(TSLAr %>% pull(returns))


#risk free rate
rf = (1+0.0171)^(1/365)-1

#Stock risk premium
risk_premium = as_tibble(Return.excess(TSLAr %>% pull(returns), risk_free_rate))


