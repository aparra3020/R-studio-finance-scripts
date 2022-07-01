library(shiny)
library(ggplot2)
library(tidyverse)
library(highcharter)
library("tseries")
library("quantmod")
library("Quandl")
library("PortfolioAnalytics")
library("DEoptim")
library("PerformanceAnalytics")
library("magrittr")
library("dygraphs")
library("TTR")
library(tidyquant)



#Normalize series function
normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
}

#ETF sectors tickers
tickers = c("SLY","SPMD","SPLV","SPHB", "SPSM", "IWM", "MTUM","XRT","IYT",
            "XAR","XLF", "XLE", "XLB", "XLI", "XLK", "XLY", "XHB", "XLP",
            "XLV", "XTL", "XME", "XLU")

#Downloading data
Stocks = tq_get(tickers, from="2021-01-12",to=today())

#Stocks normalized
Snorm = Stocks %>% select(symbol, date, adjusted) %>% group_by(symbol) %>% 
    mutate(norm = normalize(adjusted)) %>% select(-adjusted) %>% 
    tq_mutate(select = norm, mutate_fun = SMA, n = 3) %>% select(-norm) %>% 
    ungroup(symbol) %>% na.omit() %>% 
    pivot_wider(names_from = symbol, values_from = SMA)

#Relative force calculation function
relative_force = function(tb, for_n, n_tickers){
    result = 0
    for (nfor in 1:n_tickers) {
        result = tb[,for_n]/tb[,nfor]+result}
    result = (result-1)/for_n
    return (result)
}

#Duplicating dataset
rels= Snorm

#Applying relative force function
for (for_n in 1:22) {
    rels[,for_n+1] = relative_force(tb = Snorm %>% select(-date),
                                    n_tickers = 22, for_n = for_n)
}

#Data manipulation
deltas = rels %>% 
  #Renaming tickers for sectors names
    rename (Financials = XLF, Energy = XLE, Materials = XLB, 
            Industrials = XLI, Technology = XLK, Consumer_discretionary = XLY,
            Home_builders = XHB, Miners = XME, Staples = XLP, Utilities = XLU,
            Healthcare = XLV, Telecom = XTL) %>% 
  #Data wrangling
    pivot_longer(SLY:Utilities, names_to = "symbol",
                 values_to = "values") %>% group_by(symbol) %>% 
    tq_mutate(select = values, mutate_fun = ROC, n = 20, type = c("discrete"),
              col_rename = "deltas") %>% arrange(date, .by_group = TRUE) %>% 
    select(-values) %>% na.omit() %>% ungroup(symbol) %>% 
    filter(symbol %in% c("Financials", "Technology", "Materials",
                             "Industrials", "Technology", "Consumer_discretionary",
                             "Home_builders", "Miners", "Staples", "Utilities",
                             "Healthcare", "Telecom"))



#User interface
ui <- fluidPage(
    fluidRow(
  column(6,  
         #Date range input bar
   dateRangeInput("Range", "Date range", min = min(deltas$date), 
                  start = min(deltas$date), end = max(deltas$date), 
                  max = max(deltas$date))
    )),
  
  #Html graph output
highchartOutput("grafica")
  
)


server <- function(input, output, server) {
   #Reactive function to update graph
    Data <- reactive(
        deltas %>% filter(date >= input$Range[1] & date <= input$Range[2])
    )
    #Graph output configuration
    output$grafica <- renderHighchart(
        Data() %>% hchart("line",hcaes(x = date, y = deltas, group = symbol))
    )
}
    
shinyApp(ui, server)


