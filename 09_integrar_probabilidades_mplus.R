# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 09: Integración de probabilidades posteriores de la LCA de 7 clases
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
  "readr",
  "dplyr",
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
      "\nInstálelos con renv::install()."
    )
  )
}

library(readr)
library(dplyr)
library(tidyr)
library(tibble)

# ------------------------------------------------------------------------------
# 2. Rutas
# ------------------------------------------------------------------------------

ruta_procesados <- "04_datos_procesados"
ruta_tablas <- "05_tablas"
ruta_mplus <- "08_mplus"
ruta_documentacion <- "09_documentacion"

ruta_probabilidades <- file.path(
  ruta_mplus,
  "probabilidades_7_clases_final.dat"
)

ruta_tmigrante <- file.path(
  ruta_procesados,
  "tmigrante_importada.rds"
)

if (!file.exists(ruta_probabilidades)) {
  stop(
    paste0(
      "No se encontró ",
      ruta_probabilidades,
      ". Copie el archivo generado por Mplus a 08_mplus."
    )
  )
}

if (!file.exists(ruta_tmigrante)) {
  stop(
    "No se encontró 04_datos_procesados/tmigrante_importada.rds."
  )
}

# ------------------------------------------------------------------------------
# 3. Importar archivo SAVEDATA de Mplus
# ------------------------------------------------------------------------------

# Orden confirmado en SAVEDATA INFORMATION:
# PAREN NACIM MOTIVO DOCU DEST EDAD
# CPROB1 ... CPROB7 MLCC PESO ID ESTRAT UPM
#
# ADVERTENCIAS:
# - Mplus guarda los indicadores NOMINAL con codificación interna iniciada en 0.
# - PESO es el ponderador normalizado por Mplus.
# - Para caracterización e inferencia se usarán las variables originales y
#   FAC_HOG de TMIGRANTE, no los indicadores recodificados ni PESO de SAVEDATA.

nombres_mplus <- c(
  "paren_mplus",
  "nacim_mplus",
  "motivo_mplus",
  "docu_mplus",
  "dest_mplus",
  "edad_mplus",
  paste0("cprob", 1:7),
  "clase_modal",
  "peso_mplus_normalizado",
  "id",
  "estrato_mplus",
  "upm_mplus"
)

probabilidades <- read_table(
  ruta_probabilidades,
  col_names = nombres_mplus,
  col_types = cols(
    .default = col_double()
  ),
  na = "*",
  progress = FALSE,
  show_col_types = FALSE
) |>
  mutate(
    id = as.integer(id),
    clase_modal = as.integer(clase_modal),
    estrato_mplus = as.integer(estrato_mplus),
    upm_mplus = as.integer(upm_mplus)
  )

# ------------------------------------------------------------------------------
# 4. Auditoría del archivo de probabilidades
# ------------------------------------------------------------------------------

variables_prob <- paste0("cprob", 1:7)

if (nrow(probabilidades) != 3259L) {
  stop(
    paste0(
      "El archivo contiene ",
      nrow(probabilidades),
      " registros; se esperaban 3,259."
    )
  )
}

if (n_distinct(probabilidades$id) != nrow(probabilidades)) {
  stop("El identificador ID no es único en el archivo de Mplus.")
}

if (!all(probabilidades$clase_modal %in% 1:7)) {
  stop("La clase modal contiene valores fuera del intervalo 1-7.")
}

# Construir la matriz de probabilidades fuera de mutate().
# Esto evita depender del pronombre "." dentro de expresiones anidadas.
matriz_prob <- as.matrix(
  probabilidades[
    variables_prob
  ]
)

probabilidades <- probabilidades |>
  mutate(
    suma_probabilidades = rowSums(
      matriz_prob
    ),
    clase_maxima_calculada = max.col(
      matriz_prob,
      ties.method = "first"
    ),
    probabilidad_maxima = apply(
      matriz_prob,
      1,
      max
    )
  )

if (
  max(
    abs(
      probabilidades$suma_probabilidades - 1
    )
  ) > 0.005
) {
  stop(
    "Las probabilidades posteriores no suman aproximadamente 1."
  )
}

if (
  !all(
    probabilidades$clase_modal ==
    probabilidades$clase_maxima_calculada
  )
) {
  stop(
    "La clase modal no coincide con la probabilidad posterior máxima."
  )
}

# Segunda probabilidad y margen de clasificación.
segunda_probabilidad <- apply(
  matriz_prob,
  1,
  function(x) {
    sort(
      x,
      decreasing = TRUE
    )[2]
  }
)

probabilidades <- probabilidades |>
  mutate(
    segunda_probabilidad = segunda_probabilidad,
    margen_clasificacion =
      probabilidad_maxima -
      segunda_probabilidad,
    clasificacion_menor_070 =
      probabilidad_maxima < 0.70,
    clasificacion_menor_080 =
      probabilidad_maxima < 0.80
  )

auditoria_probabilidades <- tibble(
  criterio = c(
    "Registros",
    "Identificadores únicos",
    "Probabilidad mínima de la suma",
    "Probabilidad máxima de la suma",
    "Máximo error absoluto de suma respecto a 1",
    "Clases modales coincidentes con el máximo",
    "Probabilidad máxima posterior media",
    "Casos con probabilidad máxima menor de 0.70",
    "Casos con probabilidad máxima menor de 0.80"
  ),
  resultado = c(
    nrow(probabilidades),
    n_distinct(probabilidades$id),
    min(probabilidades$suma_probabilidades),
    max(probabilidades$suma_probabilidades),
    max(
      abs(
        probabilidades$suma_probabilidades - 1
      )
    ),
    sum(
      probabilidades$clase_modal ==
        probabilidades$clase_maxima_calculada
    ),
    mean(probabilidades$probabilidad_maxima),
    sum(probabilidades$clasificacion_menor_070),
    sum(probabilidades$clasificacion_menor_080)
  )
)

print(auditoria_probabilidades)

write_csv(
  auditoria_probabilidades,
  file.path(
    ruta_tablas,
    "auditoria_probabilidades_7_clases.csv"
  )
)

# ------------------------------------------------------------------------------
# 5. Integrar con TMIGRANTE original mediante ID de fila
# ------------------------------------------------------------------------------

tmigrante <- readRDS(
  ruta_tmigrante
) |>
  mutate(
    id = row_number()
  )

if (nrow(tmigrante) != 3660L) {
  stop(
    "TMIGRANTE no contiene las 3,660 filas esperadas."
  )
}

ids_fuera_tmigrante <- setdiff(
  probabilidades$id,
  tmigrante$id
)

if (length(ids_fuera_tmigrante) > 0) {
  stop(
    "Existen identificadores de Mplus fuera de TMIGRANTE."
  )
}

base_integrada <- probabilidades |>
  select(
    id,
    all_of(variables_prob),
    clase_modal,
    probabilidad_maxima,
    segunda_probabilidad,
    margen_clasificacion,
    clasificacion_menor_070,
    clasificacion_menor_080
  ) |>
  inner_join(
    tmigrante,
    by = "id"
  )

if (nrow(base_integrada) != 3259L) {
  stop(
    "La integración no produjo los 3,259 registros esperados."
  )
}

if (!all(base_integrada$p4_11 == "1")) {
  stop(
    "La base integrada contiene personas cuyo destino no fue Estados Unidos."
  )
}

if (n_distinct(base_integrada$llave_mig) != 3259L) {
  stop(
    "llave_mig no es única en la base integrada."
  )
}

# ------------------------------------------------------------------------------
# 6. Variables analíticas y etiquetas de perfiles
# ------------------------------------------------------------------------------

etiquetas_perfiles <- c(
  "Migración laboral adulta sin documento",
  "Migración laboral adulta documentada por oferta",
  "Migración joven transnacional documentada",
  "Migración laboral juvenil sin documento",
  "Migración familiar de mayor edad",
  "Migración juvenil documentada por oferta",
  "Migración juvenil temporal por trabajo o estudio"
)

base_integrada <- base_integrada |>
  mutate(
    perfil_7 = factor(
      clase_modal,
      levels = 1:7,
      labels = etiquetas_perfiles
    ),
    
    fac_hog_num = as.numeric(fac_hog),
    est_dis_num = as.integer(est_dis),
    upm_dis_num = as.integer(upm_dis),
    
    retorno_bin = case_when(
      cond_resid == "1" ~ 1,
      cond_resid == "2" ~ 0,
      TRUE ~ NA_real_
    ),
    
    retorno = factor(
      cond_resid,
      levels = c("1", "2", "9"),
      labels = c(
        "Retornó",
        "No retornó",
        "No especificado"
      )
    ),
    
    sexo = factor(
      p4_6,
      levels = c("1", "2"),
      labels = c(
        "Hombre",
        "Mujer"
      )
    ),
    
    ruralidad = factor(
      t_loc_ur,
      levels = c("1", "2"),
      labels = c(
        "Urbana",
        "Rural"
      )
    ),
    
    anio_salida = suppressWarnings(
      as.integer(p4_10_2)
    ),
    
    periodo_salida = factor(
      case_when(
        anio_salida %in% c(2018L, 2019L) ~ "2018-2019",
        anio_salida %in% c(2020L, 2021L) ~ "2020-2021",
        anio_salida %in% c(2022L, 2023L) ~ "2022-2023",
        TRUE ~ NA_character_
      ),
      levels = c(
        "2018-2019",
        "2020-2021",
        "2022-2023"
      )
    )
  )

if (
  anyNA(base_integrada$fac_hog_num) ||
  any(base_integrada$fac_hog_num <= 0) ||
  anyNA(base_integrada$est_dis_num) ||
  anyNA(base_integrada$upm_dis_num)
) {
  stop(
    "Se detectaron problemas en las variables originales del diseño."
  )
}

# ------------------------------------------------------------------------------
# 7. Prevalencia de las clases
# ------------------------------------------------------------------------------

# A. Clasificación modal no ponderada.
prevalencia_modal_no_ponderada <- base_integrada |>
  count(
    clase_modal,
    perfil_7,
    name = "n_modal"
  ) |>
  mutate(
    porcentaje_modal_no_ponderado =
      100 * n_modal / sum(n_modal)
  )

# B. Clasificación modal ponderada con FAC_HOG original.
prevalencia_modal_ponderada <- base_integrada |>
  group_by(
    clase_modal,
    perfil_7
  ) |>
  summarise(
    poblacion_modal_expandida = sum(
      fac_hog_num
    ),
    .groups = "drop"
  ) |>
  mutate(
    porcentaje_modal_ponderado =
      100 *
      poblacion_modal_expandida /
      sum(poblacion_modal_expandida)
  )

# C. Prevalencia estimada con probabilidades posteriores.
prevalencia_posterior <- lapply(
  1:7,
  function(k) {
    
    tibble(
      clase_modal = k,
      prevalencia_posterior_ponderada =
        100 *
        sum(
          base_integrada$fac_hog_num *
            base_integrada[[paste0("cprob", k)]]
        ) /
        sum(
          base_integrada$fac_hog_num
        )
    )
  }
) |>
  bind_rows()

tabla_prevalencia <- prevalencia_modal_no_ponderada |>
  left_join(
    prevalencia_modal_ponderada,
    by = c(
      "clase_modal",
      "perfil_7"
    )
  ) |>
  left_join(
    prevalencia_posterior,
    by = "clase_modal"
  ) |>
  arrange(
    clase_modal
  )

print(tabla_prevalencia)

write_csv(
  tabla_prevalencia,
  file.path(
    ruta_tablas,
    "prevalencia_perfiles_7_clases.csv"
  )
)

# ------------------------------------------------------------------------------
# 8. Calidad de clasificación por clase modal
# ------------------------------------------------------------------------------

matriz_clasificacion <- base_integrada |>
  group_by(
    clase_modal,
    perfil_7
  ) |>
  summarise(
    n = n(),
    across(
      all_of(variables_prob),
      mean
    ),
    probabilidad_maxima_media =
      mean(probabilidad_maxima),
    margen_medio =
      mean(margen_clasificacion),
    .groups = "drop"
  )

print(matriz_clasificacion)

write_csv(
  matriz_clasificacion,
  file.path(
    ruta_tablas,
    "matriz_clasificacion_posterior_7_clases.csv"
  )
)

# ------------------------------------------------------------------------------
# 9. Guardar base integrada
# ------------------------------------------------------------------------------

saveRDS(
  base_integrada,
  file.path(
    ruta_procesados,
    "base_eua_lca_7_clases_integrada.rds"
  )
)

write_csv(
  base_integrada,
  file.path(
    ruta_procesados,
    "base_eua_lca_7_clases_integrada.csv"
  ),
  na = ""
)

# ------------------------------------------------------------------------------
# 10. Informe
# ------------------------------------------------------------------------------

informe <- c(
  "INTEGRACIÓN DE PROBABILIDADES DE MPLUS TERMINADA CORRECTAMENTE",
  "",
  paste0(
    "Registros integrados: ",
    nrow(base_integrada),
    "."
  ),
  paste0(
    "Identificadores únicos: ",
    n_distinct(base_integrada$id),
    "."
  ),
  paste0(
    "Probabilidad posterior máxima media: ",
    round(
      mean(base_integrada$probabilidad_maxima),
      4
    ),
    "."
  ),
  paste0(
    "Casos con probabilidad máxima <0.70: ",
    sum(base_integrada$clasificacion_menor_070),
    "."
  ),
  paste0(
    "Casos con probabilidad máxima <0.80: ",
    sum(base_integrada$clasificacion_menor_080),
    "."
  ),
  "",
  "Archivos principales:",
  "- 04_datos_procesados/base_eua_lca_7_clases_integrada.rds",
  "- 04_datos_procesados/base_eua_lca_7_clases_integrada.csv",
  "- 05_tablas/auditoria_probabilidades_7_clases.csv",
  "- 05_tablas/prevalencia_perfiles_7_clases.csv",
  "- 05_tablas/matriz_clasificacion_posterior_7_clases.csv"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_09_integracion_probabilidades.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")