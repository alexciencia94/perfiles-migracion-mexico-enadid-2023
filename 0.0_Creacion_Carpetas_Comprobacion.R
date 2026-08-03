#Creación de Carpetas

carpetas <- c(
  "01_datos_originales",
  "02_diccionarios",
  "03_scripts",
  "04_datos_procesados",
  "05_tablas",
  "06_figuras",
  "07_modelos",
  "08_mplus",
  "09_documentacion"
)

invisible(
  lapply(
    carpetas,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

list.dirs(recursive = FALSE)

getwd()
renv::status()
