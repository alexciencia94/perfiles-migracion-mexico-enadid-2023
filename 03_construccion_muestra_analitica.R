# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 03: Construcción de la muestra analítica
# ==============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

# ------------------------------------------------------------------------------
# 1. Paquetes
# ------------------------------------------------------------------------------

paquetes <- c(
  "dplyr",
  "readr",
  "tibble"
)

faltantes <- paquetes[
  !vapply(
    paquetes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
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

# ------------------------------------------------------------------------------
# 2. Rutas
# ------------------------------------------------------------------------------

ruta_procesados <- "04_datos_procesados"
ruta_tablas <- "05_tablas"
ruta_documentacion <- "09_documentacion"

# ------------------------------------------------------------------------------
# 3. Importar TMIGRANTE
# ------------------------------------------------------------------------------

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

cat("TMIGRANTE importada:", nrow(tmigrante), "registros.\n\n")

# ------------------------------------------------------------------------------
# 4. Verificar variables necesarias
# ------------------------------------------------------------------------------

variables_necesarias <- c(
  "llave_mig",
  "llave_hog",
  "llave_viv",
  "llave_per",
  "p4_4",
  "p4_11",
  "cond_resid",
  "p4_20_1",
  "fac_hog",
  "est_dis",
  "upm_dis"
)

variables_faltantes <- setdiff(
  variables_necesarias,
  names(tmigrante)
)

if (length(variables_faltantes) > 0) {
  stop(
    paste0(
      "Faltan variables necesarias: ",
      paste(variables_faltantes, collapse = ", ")
    )
  )
}

# ------------------------------------------------------------------------------
# 5. Comprobar universo de TMIGRANTE
# ------------------------------------------------------------------------------

tabla_p4_4 <- tmigrante |>
  count(
    p4_4,
    name = "n"
  ) |>
  arrange(p4_4)

print(tabla_p4_4)

write_csv(
  tabla_p4_4,
  file.path(
    ruta_tablas,
    "distribucion_p4_4_universo_tmigrante.csv"
  )
)

if (!all(tmigrante$p4_4 == "1")) {
  stop(
    paste0(
      "Se detectaron valores distintos de 1 en p4_4. ",
      "Debe revisarse el universo de TMIGRANTE."
    )
  )
}

cat(
  "Verificación correcta: todos los registros residían en el hogar ",
  "al momento de emigrar.\n\n"
)

# ------------------------------------------------------------------------------
# 6. Distribución del país de destino
# ------------------------------------------------------------------------------

tabla_destino <- tmigrante |>
  count(
    p4_11,
    name = "n"
  ) |>
  mutate(
    porcentaje_no_ponderado = 100 * n / sum(n)
  ) |>
  arrange(p4_11)

print(tabla_destino)

write_csv(
  tabla_destino,
  file.path(
    ruta_tablas,
    "distribucion_pais_destino.csv"
  )
)

# Según el diccionario:
# p4_11 = 1 corresponde a Estados Unidos de América.

base_eua <- tmigrante |>
  filter(p4_11 == "1")

cat(
  "Migrantes cuyo destino fue Estados Unidos:",
  nrow(base_eua),
  "\n\n"
)

# ------------------------------------------------------------------------------
# 7. Distribución de la condición de retorno
# ------------------------------------------------------------------------------

tabla_retorno <- base_eua |>
  count(
    cond_resid,
    name = "n"
  ) |>
  mutate(
    porcentaje_no_ponderado = 100 * n / sum(n)
  ) |>
  arrange(cond_resid)

print(tabla_retorno)

write_csv(
  tabla_retorno,
  file.path(
    ruta_tablas,
    "distribucion_condicion_retorno_eua.csv"
  )
)

# Según el diccionario:
# cond_resid = 1: retornó a México
# cond_resid = 2: no retornó
# cond_resid = 9: condición no especificada

base_retorno_conocido <- base_eua |>
  filter(cond_resid %in% c("1", "2"))

base_retornados <- base_eua |>
  filter(cond_resid == "1")

base_no_retornados <- base_eua |>
  filter(cond_resid == "2")

base_retorno_desconocido <- base_eua |>
  filter(cond_resid == "9")

# ------------------------------------------------------------------------------
# 8. Retornados que forman parte actualmente del hogar
# ------------------------------------------------------------------------------

tabla_pertenencia_retornados <- base_retornados |>
  count(
    p4_20_1,
    name = "n"
  ) |>
  mutate(
    porcentaje_no_ponderado = 100 * n / sum(n)
  ) |>
  arrange(p4_20_1)

print(tabla_pertenencia_retornados)

write_csv(
  tabla_pertenencia_retornados,
  file.path(
    ruta_tablas,
    "pertenencia_actual_hogar_retornados.csv"
  )
)

# Según el diccionario:
# p4_20_1 = 1: actualmente forma parte del hogar
# p4_20_1 = 2: actualmente no forma parte del hogar

base_retornados_hogar <- base_retornados |>
  filter(
    p4_20_1 == "1",
    !is.na(llave_per),
    trimws(llave_per) != ""
  )

# ------------------------------------------------------------------------------
# 9. Flujo de selección de la muestra
# ------------------------------------------------------------------------------

flujo_muestra <- tibble(
  etapa = c(
    "Migrantes internacionales en TMIGRANTE",
    "Destino Estados Unidos",
    "Condición de retorno conocida",
    "Retornó a México",
    "No retornó",
    "Condición de retorno no especificada",
    "Retornó y actualmente forma parte del hogar",
    "Retornó y actualmente no forma parte del hogar"
  ),
  n = c(
    nrow(tmigrante),
    nrow(base_eua),
    nrow(base_retorno_conocido),
    nrow(base_retornados),
    nrow(base_no_retornados),
    nrow(base_retorno_desconocido),
    sum(
      base_retornados$p4_20_1 == "1",
      na.rm = TRUE
    ),
    sum(
      base_retornados$p4_20_1 == "2",
      na.rm = TRUE
    )
  )
)

print(flujo_muestra)

write_csv(
  flujo_muestra,
  file.path(
    ruta_tablas,
    "flujo_muestra_analitica.csv"
  )
)

# ------------------------------------------------------------------------------
# 10. Validaciones esperadas
# ------------------------------------------------------------------------------

valores_esperados <- c(
  3660L,
  3259L,
  3237L,
  679L,
  2558L,
  22L,
  621L,
  58L
)

if (!identical(
  as.integer(flujo_muestra$n),
  valores_esperados
)) {
  stop(
    paste0(
      "El flujo obtenido no coincide con los valores esperados. ",
      "Revise 05_tablas/flujo_muestra_analitica.csv."
    )
  )
}

if (nrow(base_retornados_hogar) != 621L) {
  stop(
    "La base de retornados que forman parte del hogar no contiene 621 registros."
  )
}

# ------------------------------------------------------------------------------
# 11. Convertir variables técnicas del diseño
# ------------------------------------------------------------------------------

convertir_diseno <- function(datos) {
  
  datos |>
    mutate(
      fac_hog_num = as.numeric(fac_hog),
      est_dis_num = as.integer(est_dis),
      upm_dis_num = as.integer(upm_dis)
    )
}

base_eua <- convertir_diseno(base_eua)
base_retorno_conocido <- convertir_diseno(base_retorno_conocido)
base_retornados_hogar <- convertir_diseno(base_retornados_hogar)

# Comprobar conversiones
if (
  anyNA(base_eua$fac_hog_num) ||
  anyNA(base_eua$est_dis_num) ||
  anyNA(base_eua$upm_dis_num)
) {
  stop(
    "La conversión de las variables del diseño produjo valores faltantes."
  )
}

if (any(base_eua$fac_hog_num <= 0)) {
  stop(
    "Se detectaron ponderadores iguales o menores que cero."
  )
}

# ------------------------------------------------------------------------------
# 12. Guardar bases analíticas
# ------------------------------------------------------------------------------

saveRDS(
  base_eua,
  file.path(
    ruta_procesados,
    "base_eua_lca.rds"
  )
)

saveRDS(
  base_retorno_conocido,
  file.path(
    ruta_procesados,
    "base_eua_retorno_conocido.rds"
  )
)

saveRDS(
  base_retornados_hogar,
  file.path(
    ruta_procesados,
    "base_retornados_hogar.rds"
  )
)

# ------------------------------------------------------------------------------
# 13. Informe
# ------------------------------------------------------------------------------

informe <- c(
  "CONSTRUCCIÓN DE LA MUESTRA ANALÍTICA TERMINADA CORRECTAMENTE",
  "",
  paste0(
    "Migrantes internacionales: ",
    nrow(tmigrante)
  ),
  paste0(
    "Destino Estados Unidos: ",
    nrow(base_eua)
  ),
  paste0(
    "Retorno conocido: ",
    nrow(base_retorno_conocido)
  ),
  paste0(
    "Retornaron: ",
    nrow(base_retornados)
  ),
  paste0(
    "No retornaron: ",
    nrow(base_no_retornados)
  ),
  paste0(
    "Retorno no especificado: ",
    nrow(base_retorno_desconocido)
  ),
  paste0(
    "Retornados que forman parte del hogar: ",
    nrow(base_retornados_hogar)
  )
)

writeLines(
  informe,
  con = file.path(
    ruta_documentacion,
    "informe_script_03_muestra_analitica.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")