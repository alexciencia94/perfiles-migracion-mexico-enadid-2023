# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 10: Caracterización ponderada de los siete perfiles latentes
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
  "tidyr",
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
      paste(faltantes, collapse = ", "),
      "\nInstálelos con renv::install()."
    )
  )
}

library(dplyr)
library(tidyr)
library(readr)
library(tibble)

# ------------------------------------------------------------------------------
# 2. Rutas e importación
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
      "No se encontró ",
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

variables_prob <- paste0("cprob", 1:7)

faltantes_variables <- setdiff(
  c(
    variables_prob,
    "clase_modal",
    "perfil_7",
    "fac_hog_num",
    "sexo",
    "ruralidad",
    "periodo_salida",
    "retorno",
    "retorno_bin"
  ),
  names(base)
)

if (length(faltantes_variables) > 0) {
  stop(
    paste0(
      "Faltan estas variables: ",
      paste(faltantes_variables, collapse = ", ")
    )
  )
}

# ------------------------------------------------------------------------------
# 3. Etiquetas de clases
# ------------------------------------------------------------------------------

etiquetas_clases <- tibble(
  clase = 1:7,
  perfil = levels(base$perfil_7)
)

# ------------------------------------------------------------------------------
# 4. Prevalencia poblacional de los perfiles
# ------------------------------------------------------------------------------

prevalencia_modal <- base |>
  group_by(
    clase = clase_modal
  ) |>
  summarise(
    n_modal = n(),
    poblacion_expandida_modal = sum(
      fac_hog_num,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    porcentaje_modal_no_ponderado =
      100 * n_modal / sum(n_modal),
    porcentaje_modal_ponderado =
      100 *
      poblacion_expandida_modal /
      sum(poblacion_expandida_modal)
  )

prevalencia_posterior <- lapply(
  1:7,
  function(k) {
    
    tibble(
      clase = k,
      poblacion_expandida_posterior = sum(
        base$fac_hog_num *
          base[[paste0("cprob", k)]],
        na.rm = TRUE
      )
    )
  }
) |>
  bind_rows() |>
  mutate(
    porcentaje_posterior_ponderado =
      100 *
      poblacion_expandida_posterior /
      sum(poblacion_expandida_posterior)
  )

tabla_prevalencia <- etiquetas_clases |>
  left_join(
    prevalencia_modal,
    by = "clase"
  ) |>
  left_join(
    prevalencia_posterior,
    by = "clase"
  )

print(tabla_prevalencia)

write_csv(
  tabla_prevalencia,
  file.path(
    ruta_tablas,
    "tabla_prevalencia_ponderada_perfiles_7_clases.csv"
  )
)

# ------------------------------------------------------------------------------
# 5. Función: caracterización modal ponderada
# ------------------------------------------------------------------------------

caracterizar_modal <- function(
    datos,
    variable,
    etiqueta_variable
) {
  
  datos |>
    filter(
      !is.na(.data[[variable]])
    ) |>
    mutate(
      categoria = as.character(
        .data[[variable]]
      )
    ) |>
    group_by(
      clase = clase_modal,
      categoria
    ) |>
    summarise(
      n = n(),
      poblacion_expandida = sum(
        fac_hog_num,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    group_by(clase) |>
    mutate(
      porcentaje_dentro_clase =
        100 *
        poblacion_expandida /
        sum(poblacion_expandida)
    ) |>
    ungroup() |>
    mutate(
      variable = etiqueta_variable,
      metodo = "Clasificación modal"
    ) |>
    left_join(
      etiquetas_clases,
      by = "clase"
    ) |>
    select(
      metodo,
      clase,
      perfil,
      variable,
      categoria,
      n,
      poblacion_expandida,
      porcentaje_dentro_clase
    )
}

# ------------------------------------------------------------------------------
# 6. Función: caracterización posterior ponderada
# ------------------------------------------------------------------------------

caracterizar_posterior <- function(
    datos,
    variable,
    etiqueta_variable
) {
  
  categorias <- datos |>
    filter(
      !is.na(.data[[variable]])
    ) |>
    pull(
      .data[[variable]]
    ) |>
    as.character() |>
    unique() |>
    sort()
  
  resultados <- vector(
    mode = "list",
    length = 7 * length(categorias)
  )
  
  contador <- 1L
  
  for (k in 1:7) {
    
    probabilidad <- datos[[paste0("cprob", k)]]
    
    denominador <- sum(
      datos$fac_hog_num *
        probabilidad *
        !is.na(datos[[variable]]),
      na.rm = TRUE
    )
    
    for (categoria_actual in categorias) {
      
      indicador_categoria <- (
        as.character(
          datos[[variable]]
        ) == categoria_actual
      )
      
      indicador_categoria[
        is.na(indicador_categoria)
      ] <- FALSE
      
      numerador <- sum(
        datos$fac_hog_num *
          probabilidad *
          indicador_categoria,
        na.rm = TRUE
      )
      
      resultados[[contador]] <- tibble(
        metodo = "Probabilidades posteriores",
        clase = k,
        variable = etiqueta_variable,
        categoria = categoria_actual,
        poblacion_expandida = numerador,
        porcentaje_dentro_clase = ifelse(
          denominador > 0,
          100 * numerador / denominador,
          NA_real_
        )
      )
      
      contador <- contador + 1L
    }
  }
  
  bind_rows(resultados) |>
    left_join(
      etiquetas_clases,
      by = "clase"
    ) |>
    select(
      metodo,
      clase,
      perfil,
      variable,
      categoria,
      poblacion_expandida,
      porcentaje_dentro_clase
    )
}

# ------------------------------------------------------------------------------
# 7. Variables auxiliares para caracterización
# ------------------------------------------------------------------------------

variables_caracterizacion <- tribble(
  ~variable,         ~etiqueta,
  "sexo",            "Sexo",
  "ruralidad",       "Ruralidad",
  "periodo_salida",  "Periodo de salida",
  "retorno",         "Condición de retorno"
)

caracterizacion_modal <- lapply(
  seq_len(nrow(variables_caracterizacion)),
  function(i) {
    
    caracterizar_modal(
      datos = base,
      variable = variables_caracterizacion$variable[i],
      etiqueta_variable = variables_caracterizacion$etiqueta[i]
    )
  }
) |>
  bind_rows()

caracterizacion_posterior <- lapply(
  seq_len(nrow(variables_caracterizacion)),
  function(i) {
    
    caracterizar_posterior(
      datos = base,
      variable = variables_caracterizacion$variable[i],
      etiqueta_variable = variables_caracterizacion$etiqueta[i]
    )
  }
) |>
  bind_rows()

write_csv(
  caracterizacion_modal,
  file.path(
    ruta_tablas,
    "caracterizacion_modal_ponderada_7_perfiles.csv"
  )
)

write_csv(
  caracterizacion_posterior,
  file.path(
    ruta_tablas,
    "caracterizacion_posterior_ponderada_7_perfiles.csv"
  )
)

# ------------------------------------------------------------------------------
# 8. Retorno descriptivo por perfil
# ------------------------------------------------------------------------------

# Excluir únicamente los 22 casos con condición de retorno no especificada.
base_retorno <- base |>
  filter(
    !is.na(retorno_bin)
  )

retorno_modal <- base_retorno |>
  group_by(
    clase = clase_modal
  ) |>
  summarise(
    n_retorno_conocido = n(),
    retornados = sum(
      retorno_bin == 1
    ),
    poblacion_total = sum(
      fac_hog_num,
      na.rm = TRUE
    ),
    poblacion_retornada = sum(
      fac_hog_num *
        retorno_bin,
      na.rm = TRUE
    ),
    prevalencia_retorno_modal =
      100 *
      poblacion_retornada /
      poblacion_total,
    .groups = "drop"
  )

retorno_posterior <- lapply(
  1:7,
  function(k) {
    
    probabilidad <- base_retorno[[paste0("cprob", k)]]
    
    denominador <- sum(
      base_retorno$fac_hog_num *
        probabilidad,
      na.rm = TRUE
    )
    
    numerador <- sum(
      base_retorno$fac_hog_num *
        probabilidad *
        base_retorno$retorno_bin,
      na.rm = TRUE
    )
    
    tibble(
      clase = k,
      poblacion_total_posterior = denominador,
      poblacion_retornada_posterior = numerador,
      prevalencia_retorno_posterior =
        100 * numerador / denominador
    )
  }
) |>
  bind_rows()

tabla_retorno_descriptiva <- etiquetas_clases |>
  left_join(
    retorno_modal,
    by = "clase"
  ) |>
  left_join(
    retorno_posterior,
    by = "clase"
  )

print(tabla_retorno_descriptiva)

write_csv(
  tabla_retorno_descriptiva,
  file.path(
    ruta_tablas,
    "retorno_descriptivo_por_perfil_7_clases.csv"
  )
)

# ------------------------------------------------------------------------------
# 9. Comparación modal frente a posterior
# ------------------------------------------------------------------------------

comparacion_metodos <- caracterizacion_modal |>
  select(
    clase,
    perfil,
    variable,
    categoria,
    porcentaje_modal =
      porcentaje_dentro_clase
  ) |>
  full_join(
    caracterizacion_posterior |>
      select(
        clase,
        perfil,
        variable,
        categoria,
        porcentaje_posterior =
          porcentaje_dentro_clase
      ),
    by = c(
      "clase",
      "perfil",
      "variable",
      "categoria"
    )
  ) |>
  mutate(
    diferencia_puntos_porcentuales =
      porcentaje_modal -
      porcentaje_posterior
  )

write_csv(
  comparacion_metodos,
  file.path(
    ruta_tablas,
    "comparacion_modal_posterior_caracterizacion.csv"
  )
)

# ------------------------------------------------------------------------------
# 10. Informe
# ------------------------------------------------------------------------------

informe <- c(
  "CARACTERIZACIÓN PONDERADA DE LOS SIETE PERFILES TERMINADA",
  "",
  paste0(
    "Muestra: ",
    nrow(base),
    " migrantes hacia Estados Unidos."
  ),
  paste0(
    "Casos con retorno conocido: ",
    nrow(base_retorno),
    "."
  ),
  "",
  "Se generaron dos estimadores descriptivos:",
  "- clasificación modal ponderada con FAC_HOG;",
  "- ponderación por probabilidades posteriores y FAC_HOG.",
  "",
  "IMPORTANTE:",
  "La tabla de retorno es descriptiva y todavía no corrige inferencialmente",
  "el error de clasificación latente. La asociación formal se desarrollará",
  "en el siguiente script mediante un procedimiento de tres pasos.",
  "",
  "Archivos:",
  "- tabla_prevalencia_ponderada_perfiles_7_clases.csv",
  "- caracterizacion_modal_ponderada_7_perfiles.csv",
  "- caracterizacion_posterior_ponderada_7_perfiles.csv",
  "- retorno_descriptivo_por_perfil_7_clases.csv",
  "- comparacion_modal_posterior_caracterizacion.csv"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_10_caracterizacion_perfiles.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")