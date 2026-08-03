# ==============================================================================
# PROYECTO: Perfiles migratorios Mexico-Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 11B: Paso 1 del BCH manual - guardar pesos BCH
# ==============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

ruta_mplus <- "08_mplus"
ruta_documentacion <- "09_documentacion"

archivo_datos <- "base_lca_7_clases_retorno_dcat.dat"
ruta_datos <- file.path(
  ruta_mplus,
  archivo_datos
)

if (!file.exists(ruta_datos)) {
  stop(
    paste0(
      "No se encontro ",
      ruta_datos,
      ". Ejecute primero el script 11A."
    )
  )
}

if (
  length(
    readLines(
      ruta_datos,
      warn = FALSE
    )
  ) != 3660L
) {
  stop(
    "El archivo de datos no contiene las 3,660 filas esperadas."
  )
}

sintaxis <- c(
  "TITLE:",
  "  ENADID 2023 - Paso 1 BCH manual, modelo de 7 clases;",
  "",
  "DATA:",
  paste0(
    "  FILE IS ",
    archivo_datos,
    ";"
  ),
  "",
  "VARIABLE:",
  "  NAMES ARE",
  "    id subpop edad paren nacim motivo docu dest",
  "    retorno peso estrat upm;",
  "",
  "  USEVARIABLES ARE",
  "    edad paren nacim motivo docu dest;",
  "",
  "  IDVARIABLE IS id;",
  "  MISSING ARE ALL (-999);",
  "",
  "  CATEGORICAL ARE edad;",
  "  NOMINAL ARE paren nacim motivo docu dest;",
  "",
  "  CLASSES = c(7);",
  "",
  "  AUXILIARY = retorno;",
  "",
  "  WEIGHT IS peso;",
  "  STRATIFICATION IS estrat;",
  "  CLUSTER IS upm;",
  "  SUBPOPULATION IS subpop EQ 1;",
  "",
  "ANALYSIS:",
  "  TYPE = MIXTURE COMPLEX;",
  "  ESTIMATOR = MLR;",
  "  STARTS = 5000 1000;",
  "  STITERATIONS = 20;",
  "  PROCESSORS = 4;",
  "",
  "OUTPUT:",
  "  TECH1 TECH4 TECH8;",
  "",
  "SAVEDATA:",
  "  FILE IS bch_7_clases_paso1.dat;",
  "  SAVE = BCHWEIGHTS;"
)

ruta_inp <- file.path(
  ruta_mplus,
  "BCH_7_clases_paso1.inp"
)

writeLines(
  sintaxis,
  con = ruta_inp,
  useBytes = TRUE
)

informe <- c(
  "PASO 1 DEL BCH MANUAL PREPARADO",
  "",
  "Archivo de datos:",
  paste0("- ", ruta_datos),
  "",
  "Sintaxis:",
  paste0("- ", ruta_inp),
  "",
  "Archivos que debe producir Mplus:",
  "- BCH_7_clases_paso1.out",
  "- bch_7_clases_paso1.dat",
  "",
  "No interprete aun el retorno.",
  "Este paso solo vuelve a estimar el modelo final y guarda siete pesos BCH."
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_11B_BCH_paso1.txt"
  ),
  useBytes = TRUE
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")