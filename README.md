# J_utilities

Este repositorio esta creado para mis pruebas

Si algo de aqui le sirve, sientase en libertad de usarlo libremente.


## [Modelos VAR](https://github.com/TJhon/j_utilities/tree/main/R/General)

- Seleccion del orden p (`var_select`)
- Tabla funcion impulso respuesta (`var_irf_tbl`)
- Graficos impulso respuesta (`irf_plot_all`) 
- Graficos impulso respuesta por variables (`irf_plot_im_res`)

## [Inei-data (enaho)](https://github.com/TJhon/j_utilities/tree/main/R/inei-data)

- Codigos de enaho (`enaho_anio` - ver source)
- Modulos de enaho (`modulos`)
- Creacion de carpetas a usar para la descarga (`crp_cre`)
- Descarga de modulos por anios o viceversa (`inei_data`)
- Mover y ordenar solo los archivos ".dta" y eliminar si se desea carpetas anteriores al proceso (`move_1`)
- Leer y guardar archivos ".dta" y guardarlos en formato ".rds" (Lectura de datos por parte de R mas rapida) (`doc_dta`)
  - En mantenimiento (not found `read_n_save`
