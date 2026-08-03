# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# SCRIPT 08: Modelos finalistas de 5, 6 y 7 clases
# ==============================================================================

rm(list = ls())
gc()

ruta_mplus <- "08_mplus"
ruta_documentacion <- "09_documentacion"

archivo_datos <- "base_lca_enadid_subpop_correcta.dat"
ruta_datos <- file.path(ruta_mplus, archivo_datos)

if (!file.exists(ruta_datos)) {
  stop("No se encontró 08_mplus/base_lca_enadid_subpop_correcta.dat")
}

if (length(readLines(ruta_datos, warn = FALSE)) != 3660L) {
  stop("El archivo DAT no contiene 3,660 filas.")
}

crear_sintaxis_final <- function(k, incluir_tech10 = FALSE) {
  
  salida_tecnica <- if (incluir_tech10) {
    "  TECH1 TECH4 TECH8 TECH10 TECH11;"
  } else {
    "  TECH1 TECH4 TECH8 TECH11;"
  }
  
  c(
    "TITLE:",
    paste0("  ENADID 2023 - modelo finalista de ", k, " clases;"),
    "",
    "DATA:",
    paste0("  FILE IS ", archivo_datos, ";"),
    "",
    "VARIABLE:",
    "  NAMES ARE",
    "    id subpop edad paren nacim motivo docu dest",
    "    peso estrat upm;",
    "",
    "  USEVARIABLES ARE",
    "    edad paren nacim motivo docu dest;",
    "",
    "  IDVARIABLE IS id;",
    "  MISSING ARE ALL (-999);",
    "  CATEGORICAL ARE edad;",
    "  NOMINAL ARE paren nacim motivo docu dest;",
    paste0("  CLASSES = c(", k, ");"),
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
    salida_tecnica,
    "",
    "SAVEDATA:",
    paste0("  FILE IS probabilidades_", k, "_clases_final.dat;"),
    "  SAVE = CPROBABILITIES;"
  )
}

for (k in 5:7) {
  ruta <- file.path(
    ruta_mplus,
    paste0("LCA_", k, "_clases_final.inp")
  )
  
  writeLines(
    crear_sintaxis_final(
      k,
      incluir_tech10 = (k == 7)
    ),
    con = ruta,
    useBytes = TRUE
  )
}

guia <- c(
  "MODELOS FINALISTAS",
  "",
  "Ejecutar en Mplus:",
  "1. LCA_5_clases_final.inp",
  "2. LCA_6_clases_final.inp",
  "3. LCA_7_clases_final.inp",
  "",
  "Los tres modelos usan 5,000 inicios aleatorios y 1,000 optimizaciones finales.",
  "El modelo de siete clases añade TECH10 para evaluar frecuencias observadas,",
  "esperadas y residuos estandarizados univariados y bivariados.",
  "",
  "La solución de siete clases es la candidata principal.",
  "Las soluciones de cinco y seis clases se conservan como comparadores de sensibilidad."
)

writeLines(
  guia,
  file.path(
    ruta_documentacion,
    "guia_modelos_finalistas_5_6_7.txt"
  ),
  useBytes = TRUE
)

cat("\n")
cat("============================================================\n")
cat("MODELOS FINALISTAS GENERADOS\n")
cat("- 08_mplus/LCA_5_clases_final.inp\n")
cat("- 08_mplus/LCA_6_clases_final.inp\n")
cat("- 08_mplus/LCA_7_clases_final.inp\n")
cat("============================================================\n")