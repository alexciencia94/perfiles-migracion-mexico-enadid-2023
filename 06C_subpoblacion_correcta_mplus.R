# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 06C: Subpoblación correcta para Mplus conservando el diseño completo
# ==============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

paquetes <- c("dplyr", "readr", "tibble", "tidyr")

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  stop(
    paste0(
      "Faltan estos paquetes: ",
      paste(faltantes, collapse = ", ")
    )
  )
}

library(dplyr)
library(readr)
library(tibble)
library(tidyr)

ruta_procesados <- "04_datos_procesados"
ruta_mplus <- "08_mplus"
ruta_tablas <- "05_tablas"
ruta_documentacion <- "09_documentacion"

dir.create(ruta_mplus, recursive = TRUE, showWarnings = FALSE)

ruta_tmigrante <- file.path(
  ruta_procesados,
  "tmigrante_importada.rds"
)

if (!file.exists(ruta_tmigrante)) {
  stop(
    "No se encontró tmigrante_importada.rds. Ejecute primero el script 02."
  )
}

tmigrante <- readRDS(ruta_tmigrante)

if (nrow(tmigrante) != 3660L) {
  stop(
    paste0(
      "TMIGRANTE contiene ",
      nrow(tmigrante),
      " registros; se esperaban 3,660."
    )
  )
}

# ------------------------------------------------------------------------------
# 1. Preparación correcta del dominio
# ------------------------------------------------------------------------------

# IMPORTANTE:
# Los casos fuera de EUA reciben valores válidos arbitrarios en los indicadores.
# SUBPOPULATION les asignará peso cero, por lo que esos valores no intervienen
# en el modelo. Se evita así que Mplus los elimine por tener todos los
# indicadores faltantes y se conserva la estructura completa del diseño.

base_mplus <- tmigrante |>
  mutate(
    id = row_number(),
    subpop = if_else(p4_11 == "1", 1L, 0L),
    
    edad = case_when(
      subpop == 0L ~ 1L,
      p4_8_ag2 %in% c("1", "2", "3", "4") ~ as.integer(p4_8_ag2),
      TRUE ~ -999L
    ),
    
    paren = case_when(
      subpop == 0L ~ 1L,
      p4_7 == "1" ~ 1L,
      p4_7 == "2" ~ 2L,
      p4_7 == "3" ~ 3L,
      p4_7 == "4" ~ 4L,
      p4_7 == "5" ~ 5L,
      p4_7 == "8" ~ 6L,
      TRUE ~ -999L
    ),
    
    nacim = case_when(
      subpop == 0L ~ 1L,
      p4_5 == "1" ~ 1L,
      p4_5 == "2" ~ 2L,
      p4_5 %in% c("3", "4") ~ 3L,
      TRUE ~ -999L
    ),
    
    motivo = case_when(
      subpop == 0L ~ 1L,
      p4_14 == "1" ~ 1L,
      p4_14 == "2" ~ 2L,
      p4_14 == "3" ~ 3L,
      p4_14 == "4" ~ 4L,
      p4_14 == "5" ~ 5L,
      p4_14 %in% c("6", "9") ~ 6L,
      TRUE ~ -999L
    ),
    
    docu = case_when(
      subpop == 0L ~ 1L,
      p4_13 %in% c("1", "2", "5") ~ 1L,
      p4_13 %in% c("3", "4") ~ 2L,
      p4_13 == "6" ~ 3L,
      p4_13 == "7" ~ 4L,
      p4_13 == "9" ~ -999L,
      TRUE ~ -999L
    ),
    
    dest = case_when(
      subpop == 0L ~ 1L,
      p4_12 %in% as.character(1:7) ~ as.integer(p4_12),
      TRUE ~ -999L
    ),
    
    peso = as.numeric(fac_hog),
    estrat = as.integer(est_dis),
    upm = as.integer(upm_dis)
  ) |>
  select(
    id,
    subpop,
    edad,
    paren,
    nacim,
    motivo,
    docu,
    dest,
    peso,
    estrat,
    upm
  )

# ------------------------------------------------------------------------------
# 2. Validaciones
# ------------------------------------------------------------------------------

if (sum(base_mplus$subpop == 1L) != 3259L) {
  stop("La subpoblación EUA no contiene 3,259 registros.")
}

if (sum(base_mplus$subpop == 0L) != 401L) {
  stop("El complemento del dominio no contiene 401 registros.")
}

indicadores <- c(
  "edad",
  "paren",
  "nacim",
  "motivo",
  "docu",
  "dest"
)

# Fuera del dominio no puede haber -999 en los indicadores.
faltantes_fuera <- base_mplus |>
  filter(subpop == 0L) |>
  summarise(
    across(
      all_of(indicadores),
      ~ sum(.x == -999L)
    )
  )

if (any(unlist(faltantes_fuera) != 0L)) {
  stop(
    "Existen indicadores faltantes fuera de la subpoblación."
  )
}

# Los faltantes reales dentro del dominio deben mantenerse.
faltantes_dominio <- base_mplus |>
  filter(subpop == 1L) |>
  summarise(
    across(
      all_of(indicadores),
      ~ sum(.x == -999L)
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "faltantes"
  )

print(faltantes_dominio)

write_csv(
  faltantes_dominio,
  file.path(
    ruta_tablas,
    "faltantes_indicadores_mplus_subpoblacion.csv"
  )
)

if (
  anyNA(base_mplus$peso) ||
  any(base_mplus$peso <= 0) ||
  anyNA(base_mplus$estrat) ||
  anyNA(base_mplus$upm)
) {
  stop(
    "Se detectaron problemas en peso, estrato o UPM."
  )
}

# Auditoría del diseño completo y del dominio.
auditoria_diseno <- bind_rows(
  base_mplus |>
    summarise(
      universo = "TMIGRANTE completa",
      n = n(),
      estratos = n_distinct(estrat),
      upm = n_distinct(upm)
    ),
  base_mplus |>
    filter(subpop == 1L) |>
    summarise(
      universo = "Subpoblación EUA",
      n = n(),
      estratos = n_distinct(estrat),
      upm = n_distinct(upm)
    )
)

print(auditoria_diseno)

write_csv(
  auditoria_diseno,
  file.path(
    ruta_tablas,
    "auditoria_diseno_mplus_subpoblacion.csv"
  )
)

# ------------------------------------------------------------------------------
# 3. Exportación
# ------------------------------------------------------------------------------

ruta_dat <- file.path(
  ruta_mplus,
  "base_lca_enadid_subpop_correcta.dat"
)

write.table(
  base_mplus,
  file = ruta_dat,
  sep = " ",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  na = "-999",
  dec = "."
)

write_csv(
  base_mplus,
  file.path(
    ruta_mplus,
    "base_lca_enadid_subpop_correcta_revision.csv"
  )
)

if (
  length(readLines(ruta_dat, warn = FALSE)) != 3660L
) {
  stop(
    "El archivo DAT no contiene las 3,660 filas esperadas."
  )
}

# ------------------------------------------------------------------------------
# 4. Sintaxis Mplus
# ------------------------------------------------------------------------------

sintaxis <- c(
  "TITLE:",
  "  ENADID 2023 - LCA de 2 clases con dominio EUA y diseno completo;",
  "",
  "DATA:",
  "  FILE IS base_lca_enadid_subpop_correcta.dat;",
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
  "",
  "  CATEGORICAL ARE edad;",
  "  NOMINAL ARE paren nacim motivo docu dest;",
  "",
  "  CLASSES = c(2);",
  "",
  "  WEIGHT IS peso;",
  "  STRATIFICATION IS estrat;",
  "  CLUSTER IS upm;",
  "  SUBPOPULATION IS subpop EQ 1;",
  "",
  "ANALYSIS:",
  "  TYPE = MIXTURE COMPLEX;",
  "  ESTIMATOR = MLR;",
  "  STARTS = 500 100;",
  "  STITERATIONS = 20;",
  "  PROCESSORS = 4;",
  "",
  "OUTPUT:",
  "  TECH1 TECH4 TECH8;",
  "",
  "SAVEDATA:",
  "  FILE IS probabilidades_2_clases_subpop_correcta.dat;",
  "  SAVE = CPROBABILITIES;"
)

ruta_inp <- file.path(
  ruta_mplus,
  "LCA_2_clases_subpop_correcta.inp"
)

writeLines(
  sintaxis,
  con = ruta_inp,
  useBytes = TRUE
)

# ------------------------------------------------------------------------------
# 5. Informe
# ------------------------------------------------------------------------------

informe <- c(
  "SUBPOBLACIÓN CORRECTA PREPARADA PARA MPLUS",
  "",
  "Filas exportadas: 3,660.",
  "Dominio EUA: 3,259.",
  "Casos fuera del dominio: 401 con valores válidos arbitrarios y peso analítico cero.",
  "",
  "Archivos:",
  "- 08_mplus/base_lca_enadid_subpop_correcta.dat",
  "- 08_mplus/base_lca_enadid_subpop_correcta_revision.csv",
  "- 08_mplus/LCA_2_clases_subpop_correcta.inp"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_06c_subpoblacion_correcta.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")