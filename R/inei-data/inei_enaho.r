enaho_anio <- list(
  '2004' = 280 # 
  , '2005' = 281 # 
  , '2006' = 282 # 
  , '2007' = 283 # 
  , '2008' = 284 # 
  , '2009' = 285 # 
  , '2010' = 279 # 
  , '2011' = 291 # 
  , '2012' = 324 # 
  , '2013' = 404 # 
  , '2014' = 440 # 
  , '2015' = 498 # 
  , '2016' = 546 # 
  , '2017' = 603 # 
  , '2018' = 634 # 
  , '2019' = 687 # 
  , '2020' = 737 # 
)

modulos <- c(
  "01"
  , "02"
  , "03"
  , "04"
  , "05"
)

librarian::shelf(tidyverse, glue, fs, here, janitor)

crp_cre <- 
  function(.mod, .data = 'enaho', .crp = c('inei-down', 'inei-unzip', 'solo-datos', 'rds')){
    map(.crp, ~here(.data, .x)) %>% 
      map(~rep(.x, length(.mod))) %>% 
      map(~here(.x, .mod)) %>% 
      map(~try(dir_create(.x)))
  }


inei_data <- function(anio, mod, .data = 'enaho', dlt = F){
  mod_l <- length(mod)
  anio_l <- length(anio)
  mapa <- 
    tibble(anio = rep(anio, mod_l), mod = rep(mod, anio_l)) %>% 
    mutate(
      url = glue_col('http://iinei.inei.gob.pe/iinei/srienaho/descarga/STATA/{anio}-Modulo{mod}.zip')
      , file = here(.data,  'inei-down', glue_col('modulo {mod}/{anio}.zip'))
      , unzip = here(.data, 'inei-unzip', glue_col('modulo {mod}'))
    ) %>% 
    arrange(anio, mod)
  down_1 <- pull(mapa, url)
  zip <- pull(mapa, file)
  to_unzip <- pull(mapa, unzip)
  walk2(
    .x = down_1
    , .y = zip
    , ~try(download.file(.x, .y))
  )
  #try(download.file(down_1, zip))
  walk2(zip, to_unzip, ~unzip(.x, exdir = .y))
  if(dlt){
    try(dir_delete(glue('{.data}/inei-down')))
  }
  #unzip(zip)#, exdir = to_unzip)
  #mod <- enquo(mod)
  # Formatod base
  #http://iinei.inei.gob.pe/iinei/srienaho/descarga/STATA/{0}-Modulo{1}.zip'.format(cod_enaho["enaho_{0}".format(i)], j)
  ## Se opta por Stata por default
}


move_1 <- function(no_deseado = "tabla|otro|dic", dlt = F){
  docs <- 
    tibble(
      full = dir(here('inei-unzip'), recursive = T, full.names = T, pattern = ".dta$"),
      value = dir(here('inei-unzip'), recursive = T, full.names = F, pattern = ".dta$")
    ) %>% 
    filter(!str_detect(value, no_deseado)) %>% 
    mutate(
      real = here(value), 
      anio = str_sub(value, -19, -1) %>% 
        str_extract('-(\\d)+') %>% 
        str_remove('/|-') 
      , move_to = here('solo-datos', str_sub(value, 1, 9), glue_col('enaho{anio}.dta' ))
    )
  file1 <- pull(docs, full)
  file2 <- pull(docs, move_to)
  
  #return(docs)
  walk2(.x = file1, .y = file2, ~file_move(.x, .y))
  if(dlt){
    try(dir_delete('inei-unzip'))
  }
}




save_panel <- function(mod, anios){
  l_anios <- length(anios)
  mod_panel <- 
    dir(here('solo-datos', glue('modulo {mod}')), full.names = T, recursive = T) %>% 
    map(read_dta) 
  mod_panel %>% 
    enframe %>% 
    bind_rows %>% 
    count(value) %>% 
    filter(n < l_anios) %>% 
    pull(value)
  panel_bind <- 
    mod_panel %>% 
    map(~mutate(., across(where(is.numeric), as.character))) %>% 
    bind_rows()
  mod_panel %>% 
    saveRDS(here('rds', 'panel_mod_{mod}.rds'))
  paste(mod_panel)
}


doc_dta <- function(mod, car_p = 'rds', dlt = F){
  dta <- 'solo-datos'
  docs <- dir(here(dta), mod, recursive = T)
  read_docs <- dir(here(dta), mod, recursive = T, full.names = T)
  ls_1 <- 
    tibble(
      docs
      , read_docs
    ) %>% 
    mutate(
      rds_name = str_remove(docs, ".dta") 
      , rds_name = here(car_p, glue('{rds_name}.rds'))
    )
  path1 <- pull(ls_1, read_docs)
  path2 <- pull(ls_1, rds_name)
  walk2(.x = path1, .y = path2, ~read_n_save(.x, .y))
  if(dlt){
    try(dir_delete(dta))
  }
}
