# ==============================================================================
# PROYECTO: Perfiles migratorios Mexico-Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 13A: Sensibilidad con 6 clases - Paso 1 BCH manual
# ==============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

ruta_mplus <- "08_mplus"
ruta_documentacion <- "09_documentacion"

ruta_datos <- file.path(
  ruta_mplus,
  "base_lca_7_clases_retorno_dcat.dat"
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
  "  ENADID 2023 - Sensibilidad BCH con modelo de 6 clases;",
  "",
  "DATA:",
  "  FILE IS base_lca_7_clases_retorno_dcat.dat;",
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
  "  CLASSES = c(6);",
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
  "  FILE IS bch_6_clases_sensibilidad_paso1.dat;",
  "  SAVE = BCHWEIGHTS;"
)

ruta_inp <- file.path(
  ruta_mplus,
  "BCH_6_clases_sensibilidad_paso1.inp"
)

writeLines(
  sintaxis,
  con = ruta_inp,
  useBytes = TRUE
)

informe <- c(
  "SENSIBILIDAD CON SEIS CLASES PREPARADA",
  "",
  "Objetivo:",
  "- comprobar si las conclusiones principales sobre retorno se mantienen",
  "  con la solucion alternativa de seis clases.",
  "",
  "Este paso:",
  "- estima nuevamente la solucion de seis clases;",
  "- conserva los seis indicadores y el diseno complejo;",
  "- no usa retorno para formar las clases;",
  "- guarda seis pesos BCH individuales.",
  "",
  "Archivo Mplus:",
  "- 08_mplus/BCH_6_clases_sensibilidad_paso1.inp",
  "",
  "Archivos esperados despues de ejecutar Mplus:",
  "- BCH_6_clases_sensibilidad_paso1.out",
  "- bch_6_clases_sensibilidad_paso1.dat"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_13A_sensibilidad_6_clases.txt"
  ),
  useBytes = TRUE
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")