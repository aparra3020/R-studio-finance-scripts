

library(shiny)
library(tidyverse)
library(highcharter)


{
  tunel_alcista = function(s,d){-d$c2+ max(s-d$e2,0)+d$p1-max(d$e1-s,0)}
  tunel_bajista = function(s,d){-d$p1+ max(d$e1-s,0)+d$c2-max(s-d$e2,0)}
  bull_c_spread = function(s,d){-d$c1+ max(s-d$e1,0)+d$c2-max(s-d$e2,0)}
  bull_p_spread = function(s,d){-d$p1+ max(d$e1-s,0)+d$p2-max(d$e2-s,0)}
  bear_c_spread = function(s,d){d$c1- max(s-d$e1,0)-d$c2+max(s-d$e2,0)}
  bear_p_spread = function(s,d){d$p1- max(d$e1-s,0)-d$p2+max(d$e2-s,0)}
  call_ratio_back = function(s,d){d$c1-max(s-d$e1,0)+2*(-d$c2+max(s-d$e2,0))}
  put_ratio_back = function(s,d){d$p2-max(d$e2-s,0)+2*(-d$p1+max(d$e1-s,0))}
  cuna_comprada = function(s,d){-d$c2+max(s-d$e2,0)-d$p1+max(d$e1-s,0)}
  cuna_vendida = function(s,d){d$c2 - max(s-d$e2,0)+d$p1-max(d$e1-s,0)}
  cono_comprado_e1 = function(s,d){-d$p1+max(d$e1-s,0)-d$c1+max(s-d$e1,0)}
  cono_comprado_e2 = function(s,d){-d$p1+max(d$e2-s,0)-d$c1+max(s-d$e2,0)}
  cono_vendido_e1 = function(s,d){d$c1-max(s-d$e1,0)+d$p1-max(d$e1-s,0)}
  cono_vendido_e2 = function(s,d){d$c2-max(s-d$e2,0)+d$p2-max(d$e2-s,0)}
  call_r_spread = function(s,d){-d$c1+max(s-d$e1,0)+2*(d$c2-max(s-d$e2,0))}
  put_r_spread = function(s,d){-d$p2+max(d$e2-s,0)+2*(d$p1-max(d$e1-s,0))}
  mariposa_c_call = function(s,d){-d$c1+max(s-d$e1,0)+2*(d$c2-max(s-d$e2,0))-d$c3+max(s-d$e3,0)}
  mariposa_v_call = function(s,d){2*(-d$c2+max(s-d$e2,0))+d$c3-max(s-d$e3,0)+d$c1-max(s-d$e1,0)}
  mariposa_c_put = function(s,d){-d$p1+max(d$e1-s,0)+2*(d$p2-max(d$e2-s,0))-d$p3+max(d$e3-s,0)}
  mariposa_v_put = function(s,d){2*(-d$c2+max(s-d$e2,0))+d$c3-max(s-d$e3,0)+d$c1-max(s-d$e1,0)}
}

Tendencia = function(s,d){
  result = tribble(
    ~estrategia, ~utilidad,
    "tunel alcista", tunel_alcista(s,d),
    "tunel bajista", tunel_bajista(s,d),
    "bull call spread", bull_c_spread(s,d),
    "bull put spread", bull_p_spread(s,d),
    "bear call spread", bear_c_spread(s,d),
    "bear put spread", bear_p_spread(s,d),
    "call ratio back spread", call_ratio_back(s,d),
    "put ratio back spread", put_ratio_back(s,d))
  
  return(result %>% arrange(desc(utilidad)))
  }

Volatilidad = function(s,d){
 
   result= tribble(
     ~estrategia, ~utilidad,
    "cuna comprada", cuna_comprada(s,d),
    "cuna vendida", cuna_vendida(s,d),
    "cono comprado con e1", cono_comprado_e1(s,d),
    "cono comprado con e2", cono_comprado_e2(s,d),
    "cono vendido con e1", cono_vendido_e1(s,d),
    "cono vendido con e2", cono_vendido_e2(s,d),
    "call ratio spread", call_r_spread(s,d),
    "put ratio spread", put_r_spread(s,d),
    "mariposa comprada call", mariposa_c_call(s,d),
    "mariposa vendida call", mariposa_v_call(s,d),
    "mariposa comprada put", mariposa_c_put(s,d),
    "mariposa vendida put", mariposa_v_put(s,d))
  
   return(result %>% arrange(desc(utilidad)))
}

plots = function(result,s,d,amp){

  result = result %>% slice(1)
  if(result$estrategia %in% c("mariposa comprada call", 
                              "mariposa vendida call", 
                              "mariposa comprada put", 
                              "mariposa vendida put")){
    
    min = d$e1-amp
    max = d$e3+amp
    
  if(result$estrategia == "mariposa vendida call"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = mariposa_v_call(spot_p,d)) 
  }else if(result$estrategia=="mariposa vendida put"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = mariposa_v_put(spot_p,d)) 
  }else if(result$estrategia=="mariposa comprada call"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = mariposa_c_call(spot_p,d)) 
  }else if(result$estrategia=="mariposa comprada put"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = mariposa_c_put(spot_p,d)) 
  }
  
  }else if(result$estrategia %in% c("cono comprado con e1","cono vendido con e1")){
    
    min = d$e1-amp
    max = d$e1+amp
    
    if(result$estrategia=="cono comprado con e1"){
      plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
        group_by(spot_p) %>% mutate(utilidad = cono_comprado_e1(spot_p,d)) 
    }else if(result$estrategia=="cono vendido con e1"){
      plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
        group_by(spot_p) %>% mutate(utilidad = cono_vendido_e1(spot_p,d)) 
    }
    
  }else if((result$estrategia %in% c("cono comprado con e2", 
                                        "cono vendido con e2"))){
    min = d$e2-amp
    max = d$e2+amp
    
    if(result$estrategia=="cono comprado con e2"){
      plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
        group_by(spot_p) %>% mutate(utilidad = cono_comprado_e2(spot_p,d)) 
      
    } else if(result$estrategia=="cono vendido con e2"){
      plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
        group_by(spot_p) %>% mutate(utilidad = cono_vendido_e2(spot_p,d))   
    }
  }else{
    min = d$e1-amp
    max = d$e2+amp
  if(result$estrategia=="tunel alcista"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = tunel_alcista(spot_p,d)) 
  }else if(result$estrategia=="tunel bajista"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = tunel_bajista(spot_p,d)) 
  }else if(result$estrategia=="put ratio back spread"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = put_ratio_back(spot_p,d)) 
  }else if(result$estrategia=="call ratio back spread"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = call_ratio_back(spot_p,d)) 
  }else if(result$estrategia=="bull put spread"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = bull_p_spread(spot_p,d)) 
  }else if(result$estrategia=="bull call spread"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = bull_c_spread(spot_p,d))   
  }else if(result$estrategia=="bear call spread"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = bear_c_spread(spot_p,d))   
  }else if(result$estrategia=="bear put spread"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = bear_p_spread(spot_p,d))
  }else if(result$estrategia=="cuna comprada"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = cuna_comprada(spot_p,d))   
  }else if(result$estrategia=="cuna vendida"){
    plt = tibble(spot_p = seq(from = min, to = max, length.out = 1000)) %>% 
      group_by(spot_p) %>% mutate(utilidad = cuna_vendida(spot_p,d))
    
  }  
  }
  
  return(plt)
}


ui <- fluidPage(
fluidRow(
  column(12,
selectInput(inputId = "estrategia", label = "Estrategy", 
            choices = c("Trend", "Volatility")),
numericInput(inputId = "s", label = "expected spot price", value = 4726 ))),
fluidRow(
 column(3,
  numericInput(inputId = "e1", label = "strike 1", value = 4715 ),
  numericInput(inputId = "c1", label = "call 1", value = 11.12 ),
  numericInput(inputId = "p1", label = "put 1", value = 0.58 )),
 column(3, offset = 1,
  numericInput(inputId = "e2", label = "strike 2", value = 4720 ),
  numericInput(inputId = "c2", label = "call 2", value = 7.10 ),
  numericInput(inputId = "p2", label = "put 2", value = 1.22 )),
 column(6, h4("Strike 3 just for butterfly strategies"),
  numericInput(inputId = "e3", label = "strike 3", value = 4725 ),
  numericInput(inputId = "c3", label = "call 3", value = 3.50 ),
  numericInput(inputId = "p3", label = "put 3", value = 2.82 ))
 ),
h4("The best strategy is:"),
tableOutput(outputId = "resultado"),
numericInput(inputId = "amp", label = "graph range", value = 15),
highchartOutput(outputId = "grafica")
 )


server <- function(input, output) {

  d = reactive(
    tibble(
      e1 = input$e1,
      p1 = input$p1,
      c1 = input$c1,
      e2 = input$e2,
      p2 = input$p2,
      c2 = input$c2,
      e3 = input$e3,
      c3 = input$c3,
      p3 = input$p3,
    )
  )
  
  out = reactive(
    if (input$estrategia == "Volatility"){
      Volatilidad(input$s, d())
    }else if(input$estrategia == "Trend"){
     Tendencia(input$s, d())
      
    }
  )
  
  output$resultado = renderTable({
   out()
  })
  
  output$grafica = renderHighchart({
    title = out()
    title = title %>% slice(1)
   plt = plots(out(),input$s,d(), input$amp)     
     plt %>% 
      hchart(type = "line", hcaes(x = spot_p, y = utilidad)) %>% 
      hc_tooltip(pointFormat = '{point.x:.2f} {point.y:.2f} ') %>% 
       hc_add_theme(hc_theme_bloom()) %>% hc_xAxis(
         title = list(text = "Precio Spot")) %>% hc_yAxis(
           title = list(text = "Utilidad"),
           plotLines = list(list(value = 0,color = "red", width = 3))) %>% 
       hc_title(text = title$estrategia)
  })
}

shinyApp(ui = ui, server = server)
