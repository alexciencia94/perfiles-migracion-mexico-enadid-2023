# ==============================================================================
# PROYECTO: Perfiles migratorios Mexico-Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 12A: Auditoria temporal previa al analisis de tiempo hasta el retorno
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
  "tidyr",
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
      paste(faltantes, collapse = ", "),
      "\nInstalelos con renv::install()."
    )
  )
}

library(dplyr)
library(readr)
library(tidyr)
library(tibble)

# ------------------------------------------------------------------------------
# 2. Rutas e importacion
# ------------------------------------------------------------------------------

ruta_procesados <- "04_datos_procesados"
ruta_tablas <- "05_tablas"
ruta_documentacion <- "09_documentacion"

ruta_base <- file.path(
  ruta_procesados,
  "base_eua_lca_7_clases_integrada.rds"
)

if (!file.exists(ruta_base)) {
  stop(
    paste0(
      "No se encontro ",
      ruta_base,
      ". Ejecute primero el script 09 corregido."
    )
  )
}

base <- readRDS(ruta_base)

if (nrow(base) != 3259L) {
  stop(
    paste0(
      "La base contiene ",
      nrow(base),
      " registros; se esperaban 3,259."
    )
  )
}

variables_requeridas <- c(
  "id",
  "llave_mig",
  "cond_resid",
  "p4_10_1",
  "p4_10_2",
  "p4_18_1",
  "p4_18_2",
  "tpo_mig",
  "clase_modal",
  "perfil_7",
  "fac_hog_num",
  "est_dis_num",
  "upm_dis_num"
)

variables_faltantes <- setdiff(
  variables_requeridas,
  names(base)
)

if (length(variables_faltantes) > 0) {
  stop(
    paste0(
      "Faltan estas variables: ",
      paste(variables_faltantes, collapse = ", ")
    )
  )
}

# ------------------------------------------------------------------------------
# 3. Funciones auxiliares
# ------------------------------------------------------------------------------

a_entero <- function(x) {
  
  x <- trimws(
    as.character(x)
  )
  
  x[
    x == ""
  ] <- NA_character_
  
  suppressWarnings(
    as.integer(x)
  )
}

mes_valido <- function(x) {
  !is.na(x) &
    x >= 1L &
    x <= 12L
}

anio_valido <- function(x) {
  !is.na(x) &
    x >= 1900L &
    x <= 2100L
}

indice_mes <- function(anio, mes) {
  
  ifelse(
    anio_valido(anio) &
      mes_valido(mes),
    anio * 12L + mes,
    NA_integer_
  )
}

# ------------------------------------------------------------------------------
# 4. Preparacion de fechas
# ------------------------------------------------------------------------------

base_temporal <- base |>
  mutate(
    mes_salida = a_entero(p4_10_1),
    anio_salida = a_entero(p4_10_2),
    
    mes_retorno = a_entero(p4_18_1),
    anio_retorno = a_entero(p4_18_2),
    
    tpo_mig_raw = a_entero(tpo_mig),
    
    # La ventana de observacion es de aproximadamente cinco anos.
    # Valores mayores de 72 meses se consideran codigos especiales o invalidos.
    tpo_mig_meses = case_when(
      is.na(tpo_mig_raw) ~ NA_integer_,
      tpo_mig_raw < 0L ~ NA_integer_,
      tpo_mig_raw > 72L ~ NA_integer_,
      TRUE ~ tpo_mig_raw
    ),
    
    indice_salida = indice_mes(
      anio_salida,
      mes_salida
    ),
    
    indice_retorno = indice_mes(
      anio_retorno,
      mes_retorno
    ),
    
    duracion_fechas_meses = case_when(
      !is.na(indice_salida) &
        !is.na(indice_retorno) ~
        indice_retorno - indice_salida,
      TRUE ~ NA_integer_
    ),
    
    evento_retorno = case_when(
      cond_resid == "1" ~ 1L,
      cond_resid == "2" ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Tres escenarios de censura, porque la fecha individual de entrevista
    # debe verificarse antes de elegir el tiempo definitivo de los no retornados.
    censura_agosto_2023 = 2023L * 12L + 8L,
    censura_septiembre_2023 = 2023L * 12L + 9L,
    censura_octubre_2023 = 2023L * 12L + 10L,
    
    tiempo_censura_agosto = case_when(
      evento_retorno == 0L &
        !is.na(indice_salida) ~
        censura_agosto_2023 - indice_salida,
      TRUE ~ NA_integer_
    ),
    
    tiempo_censura_septiembre = case_when(
      evento_retorno == 0L &
        !is.na(indice_salida) ~
        censura_septiembre_2023 - indice_salida,
      TRUE ~ NA_integer_
    ),
    
    tiempo_censura_octubre = case_when(
      evento_retorno == 0L &
        !is.na(indice_salida) ~
        censura_octubre_2023 - indice_salida,
      TRUE ~ NA_integer_
    )
  )

# ------------------------------------------------------------------------------
# 5. Auditoria de disponibilidad
# ------------------------------------------------------------------------------

resumen_disponibilidad <- tibble(
  elemento = c(
    "Muestra EUA",
    "Retorno conocido",
    "Retornaron",
    "No retornaron",
    "Retorno no especificado",
    "Fecha de salida completa",
    "Fecha de retorno completa entre retornados",
    "TPO_MIG valido entre retornados",
    "TPO_MIG valido entre no retornados"
  ),
  n = c(
    nrow(base_temporal),
    sum(
      !is.na(base_temporal$evento_retorno)
    ),
    sum(
      base_temporal$evento_retorno == 1L,
      na.rm = TRUE
    ),
    sum(
      base_temporal$evento_retorno == 0L,
      na.rm = TRUE
    ),
    sum(
      is.na(base_temporal$evento_retorno)
    ),
    sum(
      !is.na(base_temporal$indice_salida)
    ),
    sum(
      base_temporal$evento_retorno == 1L &
        !is.na(base_temporal$indice_retorno),
      na.rm = TRUE
    ),
    sum(
      base_temporal$evento_retorno == 1L &
        !is.na(base_temporal$tpo_mig_meses),
      na.rm = TRUE
    ),
    sum(
      base_temporal$evento_retorno == 0L &
        !is.na(base_temporal$tpo_mig_meses),
      na.rm = TRUE
    )
  )
)

print(resumen_disponibilidad)

write_csv(
  resumen_disponibilidad,
  file.path(
    ruta_tablas,
    "resumen_disponibilidad_temporal.csv"
  )
)

# ------------------------------------------------------------------------------
# 6. Distribucion de TPO_MIG por condicion de retorno
# ------------------------------------------------------------------------------

tabla_tpo_por_retorno <- base_temporal |>
  mutate(
    condicion = case_when(
      evento_retorno == 1L ~ "Retorno",
      evento_retorno == 0L ~ "No retorno",
      TRUE ~ "No especificado"
    ),
    tpo_estado = case_when(
      is.na(tpo_mig_meses) ~ "Faltante o codigo especial",
      TRUE ~ "Valido"
    )
  ) |>
  count(
    condicion,
    tpo_estado,
    name = "n"
  ) |>
  group_by(
    condicion
  ) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  ) |>
  ungroup()

print(tabla_tpo_por_retorno)

write_csv(
  tabla_tpo_por_retorno,
  file.path(
    ruta_tablas,
    "tpo_mig_por_condicion_retorno.csv"
  )
)

# Distribucion de los codigos originales de TPO_MIG.
tabla_tpo_raw <- base_temporal |>
  count(
    tpo_mig_raw,
    evento_retorno,
    name = "n"
  ) |>
  arrange(
    evento_retorno,
    tpo_mig_raw
  )

write_csv(
  tabla_tpo_raw,
  file.path(
    ruta_tablas,
    "distribucion_tpo_mig_original.csv"
  )
)

# ------------------------------------------------------------------------------
# 7. Concordancia entre TPO_MIG y diferencia entre fechas
# ------------------------------------------------------------------------------

concordancia_individual <- base_temporal |>
  filter(
    evento_retorno == 1L,
    !is.na(tpo_mig_meses),
    !is.na(duracion_fechas_meses)
  ) |>
  mutate(
    diferencia_tpo_fechas =
      tpo_mig_meses -
      duracion_fechas_meses,
    concordancia_exacta =
      diferencia_tpo_fechas == 0L,
    concordancia_mas_menos_1 =
      abs(diferencia_tpo_fechas) <= 1L
  )

resumen_concordancia <- concordancia_individual |>
  summarise(
    casos_comparables = n(),
    concordancia_exacta_n = sum(
      concordancia_exacta
    ),
    concordancia_exacta_porcentaje =
      100 * mean(concordancia_exacta),
    concordancia_mas_menos_1_n = sum(
      concordancia_mas_menos_1
    ),
    concordancia_mas_menos_1_porcentaje =
      100 * mean(concordancia_mas_menos_1),
    diferencia_minima = min(
      diferencia_tpo_fechas
    ),
    diferencia_mediana = median(
      diferencia_tpo_fechas
    ),
    diferencia_maxima = max(
      diferencia_tpo_fechas
    )
  )

print(resumen_concordancia)

write_csv(
  resumen_concordancia,
  file.path(
    ruta_tablas,
    "resumen_concordancia_tpo_mig_fechas.csv"
  )
)

tabla_diferencias <- concordancia_individual |>
  count(
    diferencia_tpo_fechas,
    name = "n"
  ) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  ) |>
  arrange(
    diferencia_tpo_fechas
  )

write_csv(
  tabla_diferencias,
  file.path(
    ruta_tablas,
    "distribucion_diferencia_tpo_mig_fechas.csv"
  )
)

# ------------------------------------------------------------------------------
# 8. Inconsistencias temporales
# ------------------------------------------------------------------------------

limite_inferior <- 2018L * 12L + 8L
limite_superior <- 2023L * 12L + 10L

inconsistencias <- base_temporal |>
  mutate(
    salida_antes_ventana =
      !is.na(indice_salida) &
      indice_salida < limite_inferior,
    
    salida_despues_levantamiento =
      !is.na(indice_salida) &
      indice_salida > limite_superior,
    
    retorno_antes_salida =
      evento_retorno == 1L &
      !is.na(duracion_fechas_meses) &
      duracion_fechas_meses < 0L,
    
    retorno_despues_levantamiento =
      evento_retorno == 1L &
      !is.na(indice_retorno) &
      indice_retorno > limite_superior,
    
    retorno_sin_fecha =
      evento_retorno == 1L &
      is.na(indice_retorno),
    
    no_retorno_con_fecha_retorno =
      evento_retorno == 0L &
      !is.na(indice_retorno),
    
    tpo_sin_retorno =
      evento_retorno == 0L &
      !is.na(tpo_mig_meses),
    
    alguna_inconsistencia =
      salida_antes_ventana |
      salida_despues_levantamiento |
      retorno_antes_salida |
      retorno_despues_levantamiento |
      retorno_sin_fecha |
      no_retorno_con_fecha_retorno
  ) |>
  filter(
    alguna_inconsistencia |
      tpo_sin_retorno
  ) |>
  select(
    id,
    llave_mig,
    clase_modal,
    perfil_7,
    cond_resid,
    mes_salida,
    anio_salida,
    mes_retorno,
    anio_retorno,
    tpo_mig_raw,
    tpo_mig_meses,
    duracion_fechas_meses,
    salida_antes_ventana,
    salida_despues_levantamiento,
    retorno_antes_salida,
    retorno_despues_levantamiento,
    retorno_sin_fecha,
    no_retorno_con_fecha_retorno,
    tpo_sin_retorno
  )

print(
  inconsistencias,
  n = min(
    50L,
    nrow(inconsistencias)
  )
)

write_csv(
  inconsistencias,
  file.path(
    ruta_tablas,
    "inconsistencias_temporales.csv"
  )
)

# ------------------------------------------------------------------------------
# 9. Sensibilidad de la censura para no retornados
# ------------------------------------------------------------------------------

resumen_censura <- base_temporal |>
  filter(
    evento_retorno == 0L
  ) |>
  summarise(
    n = n(),
    
    agosto_min = min(
      tiempo_censura_agosto,
      na.rm = TRUE
    ),
    agosto_mediana = median(
      tiempo_censura_agosto,
      na.rm = TRUE
    ),
    agosto_max = max(
      tiempo_censura_agosto,
      na.rm = TRUE
    ),
    
    septiembre_min = min(
      tiempo_censura_septiembre,
      na.rm = TRUE
    ),
    septiembre_mediana = median(
      tiempo_censura_septiembre,
      na.rm = TRUE
    ),
    septiembre_max = max(
      tiempo_censura_septiembre,
      na.rm = TRUE
    ),
    
    octubre_min = min(
      tiempo_censura_octubre,
      na.rm = TRUE
    ),
    octubre_mediana = median(
      tiempo_censura_octubre,
      na.rm = TRUE
    ),
    octubre_max = max(
      tiempo_censura_octubre,
      na.rm = TRUE
    ),
    
    casos_censura_negativa_agosto = sum(
      tiempo_censura_agosto < 0L,
      na.rm = TRUE
    ),
    
    casos_censura_negativa_septiembre = sum(
      tiempo_censura_septiembre < 0L,
      na.rm = TRUE
    ),
    
    casos_censura_negativa_octubre = sum(
      tiempo_censura_octubre < 0L,
      na.rm = TRUE
    )
  )

print(resumen_censura)

write_csv(
  resumen_censura,
  file.path(
    ruta_tablas,
    "sensibilidad_fecha_censura_no_retornados.csv"
  )
)

# ------------------------------------------------------------------------------
# 10. Buscar posibles variables de fecha de entrevista
# ------------------------------------------------------------------------------

# Esta busqueda no prueba que una variable sea la fecha de entrevista.
# Solo identifica nombres que ameritan revision en las tablas importadas.
archivos_rds <- c(
  tmigrante = file.path(
    ruta_procesados,
    "tmigrante_importada.rds"
  ),
  thogar = file.path(
    ruta_procesados,
    "thogar_importada.rds"
  ),
  tsdem = file.path(
    ruta_procesados,
    "tsdem_importada.rds"
  ),
  tvivienda = file.path(
    ruta_procesados,
    "tvivienda_importada.rds"
  )
)

candidatas_fecha <- lapply(
  names(archivos_rds),
  function(nombre_tabla) {
    
    ruta <- archivos_rds[[nombre_tabla]]
    
    if (!file.exists(ruta)) {
      return(
        tibble(
          tabla = nombre_tabla,
          variable = NA_character_
        )
      )
    }
    
    datos <- readRDS(ruta)
    
    candidatas <- grep(
      pattern = "fecha|entrev|visita|levant|mes|anio|ano",
      x = names(datos),
      value = TRUE,
      ignore.case = TRUE
    )
    
    if (length(candidatas) == 0) {
      candidatas <- NA_character_
    }
    
    tibble(
      tabla = nombre_tabla,
      variable = candidatas
    )
  }
) |>
  bind_rows()

print(candidatas_fecha)

write_csv(
  candidatas_fecha,
  file.path(
    ruta_tablas,
    "variables_candidatas_fecha_entrevista.csv"
  )
)

# ------------------------------------------------------------------------------
# 11. Guardar base de auditoria temporal
# ------------------------------------------------------------------------------

saveRDS(
  base_temporal,
  file.path(
    ruta_procesados,
    "base_temporal_auditoria.rds"
  )
)

# ------------------------------------------------------------------------------
# 12. Informe
# ------------------------------------------------------------------------------

informe <- c(
  "AUDITORIA TEMPORAL TERMINADA",
  "",
  paste0(
    "Muestra EUA: ",
    nrow(base_temporal),
    "."
  ),
  paste0(
    "Retorno conocido: ",
    sum(!is.na(base_temporal$evento_retorno)),
    "."
  ),
  paste0(
    "Casos comparables entre TPO_MIG y fechas: ",
    nrow(concordancia_individual),
    "."
  ),
  paste0(
    "Registros marcados para revision temporal: ",
    nrow(inconsistencias),
    "."
  ),
  "",
  "IMPORTANTE:",
  "Este script no estima aun el modelo de tiempo hasta el retorno.",
  "Primero verifica si TPO_MIG coincide con las fechas y si existe una",
  "fecha individual de entrevista que permita censurar a los no retornados.",
  "",
  "Archivos principales:",
  "- 05_tablas/resumen_disponibilidad_temporal.csv",
  "- 05_tablas/tpo_mig_por_condicion_retorno.csv",
  "- 05_tablas/resumen_concordancia_tpo_mig_fechas.csv",
  "- 05_tablas/inconsistencias_temporales.csv",
  "- 05_tablas/sensibilidad_fecha_censura_no_retornados.csv",
  "- 05_tablas/variables_candidatas_fecha_entrevista.csv",
  "- 04_datos_procesados/base_temporal_auditoria.rds"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_12A_auditoria_temporal.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")