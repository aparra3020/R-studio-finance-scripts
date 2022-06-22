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


{
  {
estrategia_tendencia = function(s,d){
  resultados = c(
    
    tunel_alcista = tunel_alcista(s,d),
    tunel_bajista = tunel_bajista(s,d),
    bull_c_spread = bull_c_spread(s,d),
    bull_p_spread = bull_p_spread(s,d),
    bear_c_spread = bear_c_spread(s,d),
    bear_p_spread = bear_p_spread(s,d),
    call_ratio_back = call_ratio_back(s,d),
    put_ratio_back = put_ratio_back(s,d))
  
  resultados = sort(resultados, decreasing = TRUE)
  print(resultados)
  min = d$e1-3
  max = d$e2+3
  
  if(names(resultados[1])=="tunel_alcista"){
    sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
      group_by(sq) %>% mutate(utilidad = tunel_alcista(sq,d)) 
  }else if(names(resultados[1])=="tunel_bajista"){
    sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
      group_by(sq) %>% mutate(utilidad = tunel_bajista(sq,d)) 
  }else if(names(resultados[1])=="put_ratio_back"){
    sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
      group_by(sq) %>% mutate(utilidad = put_ratio_back(sq,d)) 
  }else if(names(resultados[1])=="call_ratio_back"){
    sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
      group_by(sq) %>% mutate(utilidad = call_ratio_back(sq,d)) 
  }else if(names(resultados[1])=="bull_p_spread"){
    sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
      group_by(sq) %>% mutate(utilidad = bull_p_spread(sq,d)) 
  }else if(names(resultados[1])=="bull_c_spread"){
    sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
      group_by(sq) %>% mutate(utilidad = bull_c_spread(sq,d))   
  }else if(names(resultados[1])=="bear_c_spread"){
    sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
      group_by(sq) %>% mutate(utilidad = bear_c_spread(sq,d))   
  }else if(names(resultados[1])=="bear_p_spread"){
    sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
      group_by(sq) %>% mutate(utilidad = bear_p_spread(sq,d))      
  }  
    sg %>% 
    hchart(type = "line", hcaes(x = sq, y = utilidad)) %>% 
      hc_tooltip(pointFormat = '{point.x:.2f} {point.y:.2f}')
}
  }
  
  {
Volatilidad = function(s,d){
  resultados= c(
    cuna_comprada = cuna_comprada(s,d),
    cuna_vendida = cuna_vendida(s,d),
    cono_comprado_e1 = cono_comprado_e1(s,d),
    cono_comprado_e2 = cono_comprado_e2(s,d),
    cono_vendido_e1 = cono_vendido_e1(s,d),
    cono_vendido_e2 = cono_vendido_e2(s,d),
    call_r_spread = call_r_spread(s,d),
    put_r_spread = put_r_spread(s,d),
    mariposa_c_call = mariposa_c_call(s,d),
    mariposa_v_call = mariposa_v_call(s,d),
    mariposa_c_put = mariposa_c_put(s,d),
    mariposa_v_put = mariposa_v_put(s,d))
  
  resultados = sort(resultados, decreasing = TRUE)
  print(resultados)
  
  if(names(resultados[1]) %in% c("mariposa_c_call", "mariposa_v_call", 
                 "mariposa_c_put" , "mariposa_v_put")){
    min = d$e1-3
    max = d$e3+3
    
    if(names(resultados[1]) == "mariposa_v_call"){
      sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
        group_by(sq) %>% mutate(utilidad = mariposa_v_call(sq,d)) 
    }else if(names(resultados[1])=="mariposa_v_put"){
      sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
        group_by(sq) %>% mutate(utilidad = mariposa_v_put(sq,d)) 
    }else if(names(resultados[1])=="mariposa_c_call"){
      sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
        group_by(sq) %>% mutate(utilidad = mariposa_c_call(sq,d)) 
    }else if(names(resultados[1])=="mariposa_c_put"){
      sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
        group_by(sq) %>% mutate(utilidad = mariposa_c_put(sq,d)) 
    }
 
  }else if(names(resultados[1]) %in% c("cono_comprado_e1", 
                                       "cono_vendido_e1")){
    
    min = d$e1-3
    max = d$e1+3
   
    if(names(resultados[1])=="cono_comprado_e1"){
      sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
        group_by(sq) %>% mutate(utilidad = cono_comprado_e1(sq,d)) 
    }else if(names(resultados[1])=="cono_vendido_e1"){
      sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
        group_by(sq) %>% mutate(utilidad = cono_vendido_e1(sq,d)) 
    }

  }else if((names(resultados[1]) %in% c("cono_comprado_e2", 
                                        "cono_vendido_e2"))){
    min = d$e2-3
    max = d$e2+3
    
    if(names(resultados[1])=="cono_comprado_e2"){
      sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
        group_by(sq) %>% mutate(utilidad = cono_comprado_e2(sq,d)) 
      
    } else if(names(resultados[1])=="cono_vendido_e2"){
      sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
        group_by(sq) %>% mutate(utilidad = cono_vendido_e2(sq,d))   
    }
  }
  
  else{
    min = d$e1-3
    max = d$e2+3
    
    if(names(resultados[1]) == "cuna_comprada"){
      sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
        group_by(sq) %>% mutate(utilidad = cuna_comprada(sq,d)) 
    }else if(names(resultados[1])=="cuna_vendida"){
      sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
        group_by(sq) %>% mutate(utilidad = cuna_vendida(sq,d)) 
    }else if(names(resultados[1])=="call_r_spread"){
      sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
        group_by(sq) %>% mutate(utilidad = call_r_spread(sq,d))   
    }else if(names(resultados[1])=="put_r_spread"){
      sg = tibble(sq = seq(from = min, to = max, length.out = 100)) %>% 
        group_by(sq) %>% mutate(utilidad = put_r_spread(sq,d))      
    } 
    }
    
  sg %>% 
    hchart(type = "line", hcaes(x = sq, y = utilidad)) %>% 
    hc_tooltip(pointFormat = '{point.x:.2f} {point.y:.2f} ')
}
  }
}

d = tibble(
  #poner precios de ejercicio de menor a mayor e1 el más pequeño 
  #e3 el más grande
e1 = 1140,
p1 = 3.50,
c1 = 0.19,
e2 = 1145,
p2 = 8.5,
c2 = 0.04,
e3 = 1150,
c3 = 0.01,
p3 = 12.5,
)
s = 1150

estrategia_tendencia(s,datos)
Volatilidad(s,datos)







