# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 06: Crear prueba Mplus de dos clases
# ==============================================================================

rm(list = ls())
gc()

ruta_mplus <- "08_mplus"

ruta_datos <- file.path(
  ruta_mplus,
  "base_lca_enadid.dat"
)

if (!file.exists(ruta_datos)) {
  stop(
    paste0(
      "No se encontró ",
      ruta_datos,
      ". Ejecute primero el script 05."
    )
  )
}

# Confirmar que el archivo contiene 3,259 registros.
n_lineas <- length(
  readLines(
    ruta_datos,
    warn = FALSE
  )
)

if (n_lineas != 3259L) {
  stop(
    paste0(
      "El archivo DAT contiene ",
      n_lineas,
      " líneas; se esperaban 3,259."
    )
  )
}

sintaxis <- c(
  "TITLE:",
  "  ENADID 2023 - prueba inicial LCA de 2 clases con diseno complejo;",
  "",
  "DATA:",
  "  FILE IS base_lca_enadid.dat;",
  "",
  "VARIABLE:",
  "  NAMES ARE",
  "    id edad paren nacim motivo docu dest",
  "    peso estrat upm;",
  "",
  "  USEVARIABLES ARE",
  "    edad paren nacim motivo docu dest;",
  "",
  "  IDVARIABLE IS id;",
  "",
  "  MISSING ARE ALL (-999);",
  "",
  "  CATEGORICAL ARE edad;",
  "",
  "  NOMINAL ARE",
  "    paren nacim motivo docu dest;",
  "",
  "  CLASSES = c(2);",
  "",
  "  WEIGHT IS peso;",
  "  STRATIFICATION IS estrat;",
  "  CLUSTER IS upm;",
  "",
  "ANALYSIS:",
  "  TYPE = MIXTURE COMPLEX;",
  "  ESTIMATOR = MLR;",
  "",
  "  STARTS = 500 100;",
  "  STITERATIONS = 20;",
  "  PROCESSORS = 4;",
  "",
  "OUTPUT:",
  "  TECH1 TECH4 TECH8;",
  "",
  "SAVEDATA:",
  "  FILE IS probabilidades_2_clases.dat;",
  "  SAVE = CPROBABILITIES;"
)

ruta_inp <- file.path(
  ruta_mplus,
  "LCA_2_clases_prueba.inp"
)

writeLines(
  sintaxis,
  con = ruta_inp,
  useBytes = TRUE
)

cat("\n")
cat("============================================================\n")
cat("PRUEBA DE DOS CLASES PREPARADA CORRECTAMENTE\n")
cat("Datos:", ruta_datos, "\n")
cat("Sintaxis:", ruta_inp, "\n")
cat("Registros:", n_lineas, "\n")
cat("============================================================\n")

source(
  "C:/Users/alexc/Desktop/Poryecto ENADID 2023/ENADID_Migracion/ENADID_MIGRACRION/03_scripts/06_crear_prueba_mplus_2_clases.R"
)
