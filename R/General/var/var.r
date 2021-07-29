library(vars)
library(tidyverse)
data("Canada")
bd <-  Canada


## VArSelect tidy

var_select <- function(.df, lag_max = 8, type_var = 'both', time_pre = 'T_', ...){
  .ndf <- VARselect(.df, lag.max = lag_max, type = type_var, ...)
  n_var <- 
    .ndf$selection %>%
    enframe("Criterio", "Seleccion")
  t_var <- 
    .ndf$criteria %>% 
    as_tibble() %>% 
    rename_with(~paste0(time_pre, .))
  tabla <- 
    bind_cols(n_var, t_var) 
  return(tabla)
}
var_select(bd)


m1 <- VAR(bd, p = 1, type = 'both')

irf(m1) %>% class()
irf(m1) %>% typeof()

irf1 <- irf(m1) 

var_irf_tbl <- function(irf_obj){
  if(!("varirf" %in% class(irf_obj))){
    print('none')
  }
  vari <- c('irf', 'Lower', 'Upper')
  ls_irf <- 
    map(
      vari, ~(
        irf_obj[[.]] %>% 
          map_df(as_tibble, .id = 'impulso') %>% 
          mutate(t = row_number())
        )
    )
  names(ls_irf) <- vari
  df <- 
    ls_irf %>% 
    bind_rows(.id = 'tipo') %>% 
    pivot_longer(!c(t, impulso, tipo), names_to = "respuesta") %>% 
    mutate(across(where(is.character), str_to_lower)) %>% 
    pivot_wider(names_from = tipo, values_from = value) %>% 
    relocate(t)
#  df <- 
#    df %>% 
#    new_tibble(nrow = row_number(df),  class = 'irf_plot')
  return(df)
}

irf_plot_all <- function(.data, .impulso = NULL, irf_color = '#003f5c', unidad_temporal = "Tiempo", columnas = 2){
  #if(!('irf_tidy' %in% class(.data))){
  #  stop("Se necesita un 'irf_tidy', class")
  #}
  if(is.null(.impulso)){
    stop("Inserte la variable de impulso")
  }
  .impulso = enquo(.impulso)
  .plot <- 
    .data %>% 
    filter(impulso == !!.impulso) %>% 
    ggplot() +
    aes(
      t
      , irf
      , group = respuesta
      , ymin = lower
      , ymax = upper
      ) +
    geom_line(color = irf_color) +
    geom_ribbon(
      fill = 'grey'
      , alpha = .2
      , linetype = 'dashed'
      , color = irf_color
    ) +
    facet_wrap(~respuesta, ncol = columnas) +
    geom_hline(yintercept = 0, color = 'red') +
    theme_light() +
    labs(
      y = ''
      , x = unidad_temporal
      , title = glue::glue("Impulso de la variable {as_label(.impulso)}")
      ) +
    theme(
      legend.position = 'none'
      , plot.title = element_text(hjust = .5)
      , strip.background = element_rect(colour = 'white', fill = irf_color)
      , strip.text = element_text(face = 'bold')
    )
  return(.plot)
}


irf_plot_imp_res <- function(.data, .impulso = NULL, .respuesta = NULL, irf_color = '#003f5c', .x = "Tiempo"){
  #if(!('irf_tidy' %in% class(.data))){
  #  stop("Se necesita un 'irf_tidy', class")
  #}
  if(is.null(.impulso)){
    stop("Inserte la variable de impulso")
  }
  if(is.null(.impulso)){
    stop("Inserte la variable de respuesta")
  }
  .impulso = enquo(.impulso)
  .respuesta = enquo(.respuesta)
  .plot <- 
    .data %>% 
    filter(
      impulso == !!.impulso,
      respuesta == !!.respuesta
      ) %>% 
    ggplot() +
    aes(
      t
      , irf
      #, group = respuesta
      , ymin = lower
      , ymax = upper
    ) +
    geom_line(color = irf_color) +
    geom_ribbon(
      fill = 'grey'
      , alpha = .2
      , linetype = 'dashed'
      , color = irf_color
    ) +
    #facet_wrap(~respuesta, ncol = columnas) +
    geom_hline(yintercept = 0, color = 'red') +
    theme_light() +
    labs(
      y = .respuesta
      , x = .x
      , title = glue::glue("Impulso de la variable {as_label(.impulso)} a la variable {as_label(.respuesta)}")
    ) +
    theme(
      legend.position = 'none'
      , plot.title = element_text(hjust = .5)
      , strip.background = element_rect(colour = 'white', fill = irf_color)
      , strip.text = element_text(face = 'bold')
    )
  return(.plot)
}

