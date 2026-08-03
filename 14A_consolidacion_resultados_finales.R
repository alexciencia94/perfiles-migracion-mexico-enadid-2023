# ==============================================================================
# PROYECTO: Perfiles migratorios Mexico-Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 14A: Consolidacion final de resultados, tablas y figuras
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
  "tibble",
  "ggplot2",
  "scales"
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

library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(scales)

# ------------------------------------------------------------------------------
# 2. Rutas
# ------------------------------------------------------------------------------

ruta_tablas <- "05_tablas"
ruta_figuras <- "06_figuras"
ruta_modelos <- "07_modelos"
ruta_mplus <- "08_mplus"
ruta_documentacion <- "09_documentacion"

ruta_final <- file.path(
  ruta_documentacion,
  "resultados_finales"
)

dir.create(
  ruta_final,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------------------------
# 3. Funciones generales
# ------------------------------------------------------------------------------

primer_archivo_existente <- function(
    candidatos,
    obligatorio = TRUE
) {
  
  existentes <- candidatos[
    file.exists(candidatos)
  ]
  
  if (length(existentes) == 0L) {
    
    if (obligatorio) {
      stop(
        paste0(
          "No se encontro ninguno de estos archivos:\n",
          paste(
            candidatos,
            collapse = "\n"
          )
        )
      )
    }
    
    return(NA_character_)
  }
  
  existentes[1]
}

a_numero <- function(x) {
  
  x <- gsub(
    ",",
    "",
    x,
    fixed = TRUE
  )
  
  suppressWarnings(
    as.numeric(x)
  )
}

extraer_ultimo_numero <- function(linea) {
  
  coincidencias <- regmatches(
    linea,
    gregexpr(
      "-?[0-9]+(?:\\.[0-9]+)?(?:[Ee][+-]?[0-9]+)?",
      linea,
      perl = TRUE
    )
  )[[1]]
  
  if (
    length(coincidencias) == 0L ||
    identical(coincidencias, character(0))
  ) {
    return(NA_real_)
  }
  
  a_numero(
    tail(
      coincidencias,
      1
    )
  )
}

extraer_linea_numero <- function(
    lineas,
    patron,
    desde = 1L
) {
  
  indices <- grep(
    patron,
    lineas,
    perl = TRUE
  )
  
  indices <- indices[
    indices >= desde
  ]
  
  if (length(indices) == 0L) {
    return(NA_real_)
  }
  
  extraer_ultimo_numero(
    lineas[
      indices[1]
    ]
  )
}

# ------------------------------------------------------------------------------
# 4. Analizador de salidas Mplus
# ------------------------------------------------------------------------------

extraer_ajuste_mplus <- function(
    archivo,
    clases
) {
  
  lineas <- readLines(
    archivo,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  inicio_resumen <- grep(
    "MODEL FIT INFORMATION",
    lineas,
    fixed = TRUE
  )
  
  desde <- ifelse(
    length(inicio_resumen) > 0L,
    inicio_resumen[1],
    1L
  )
  
  tibble(
    clases = clases,
    loglikelihood = extraer_linea_numero(
      lineas,
      "^\\s*H0 Value\\s+",
      desde
    ),
    AIC = extraer_linea_numero(
      lineas,
      "^\\s*Akaike \\(AIC\\)",
      desde
    ),
    BIC = extraer_linea_numero(
      lineas,
      "^\\s*Bayesian \\(BIC\\)",
      desde
    ),
    BIC_ajustado = extraer_linea_numero(
      lineas,
      "^\\s*Sample-Size Adjusted BIC",
      desde
    ),
    entropia = extraer_linea_numero(
      lineas,
      "^\\s*Entropy\\s+",
      desde
    ),
    archivo = basename(
      archivo
    )
  )
}

extraer_parametros_mplus <- function(
    archivo
) {
  
  lineas <- readLines(
    archivo,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  inicio_modelo <- grep(
    "MODEL RESULTS",
    lineas,
    fixed = TRUE
  )
  
  desde <- ifelse(
    length(inicio_modelo) > 0L,
    inicio_modelo[1],
    1L
  )
  
  inicios <- grep(
    "New/Additional Parameters",
    lineas,
    fixed = TRUE
  )
  
  inicios <- inicios[
    inicios > desde
  ]
  
  if (length(inicios) == 0L) {
    stop(
      paste0(
        "No se encontro la seccion New/Additional Parameters en ",
        archivo,
        "."
      )
    )
  }
  
  inicio <- inicios[1]
  
  finales_posibles <- grep(
    paste0(
      "QUALITY OF NUMERICAL RESULTS|",
      "CONFIDENCE INTERVALS OF MODEL RESULTS|",
      "TECHNICAL 1 OUTPUT"
    ),
    lineas,
    perl = TRUE
  )
  
  finales_posibles <- finales_posibles[
    finales_posibles > inicio
  ]
  
  fin <- ifelse(
    length(finales_posibles) > 0L,
    finales_posibles[1] - 1L,
    min(
      length(lineas),
      inicio + 300L
    )
  )
  
  bloque <- lineas[
    inicio:fin
  ]
  
  patron <- paste0(
    "^\\s*([A-Za-z]+[0-9]+)\\s+",
    "(-?[0-9]+\\.[0-9]+)\\s+",
    "([0-9]+\\.[0-9]+)\\s+",
    "(-?[0-9]+\\.[0-9]+)\\s+",
    "([0-9]+\\.[0-9]+)"
  )
  
  coincidencias <- regexec(
    patron,
    bloque,
    perl = TRUE
  )
  
  extraidas <- regmatches(
    bloque,
    coincidencias
  )
  
  extraidas <- extraidas[
    lengths(extraidas) == 6L
  ]
  
  if (length(extraidas) == 0L) {
    stop(
      paste0(
        "No se pudieron leer parametros adicionales de ",
        archivo,
        "."
      )
    )
  }
  
  tabla <- bind_rows(
    lapply(
      extraidas,
      function(x) {
        tibble(
          parametro = toupper(x[2]),
          estimacion = a_numero(x[3]),
          error_estandar = a_numero(x[4]),
          z = a_numero(x[5]),
          p_valor = a_numero(x[6])
        )
      }
    )
  ) |>
    distinct(
      parametro,
      .keep_all = TRUE
    )
  
  # Intervalos de confianza.
  inicio_ic <- grep(
    "CONFIDENCE INTERVALS OF MODEL RESULTS",
    lineas,
    fixed = TRUE
  )
  
  if (length(inicio_ic) > 0L) {
    
    inicios_parametros_ic <- grep(
      "New/Additional Parameters",
      lineas,
      fixed = TRUE
    )
    
    inicios_parametros_ic <- inicios_parametros_ic[
      inicios_parametros_ic >
        inicio_ic[1]
    ]
    
    if (length(inicios_parametros_ic) > 0L) {
      
      inicio_bloque_ic <- inicios_parametros_ic[1]
      
      bloque_ic <- lineas[
        inicio_bloque_ic:
          min(
            length(lineas),
            inicio_bloque_ic + 300L
          )
      ]
      
      patron_ic <- paste0(
        "^\\s*([A-Za-z]+[0-9]+)\\s+",
        "(-?[0-9]+\\.[0-9]+)\\s+",
        "(-?[0-9]+\\.[0-9]+)\\s+",
        "(-?[0-9]+\\.[0-9]+)\\s+",
        "(-?[0-9]+\\.[0-9]+)\\s+",
        "(-?[0-9]+\\.[0-9]+)\\s+",
        "(-?[0-9]+\\.[0-9]+)\\s+",
        "(-?[0-9]+\\.[0-9]+)"
      )
      
      coincidencias_ic <- regexec(
        patron_ic,
        bloque_ic,
        perl = TRUE
      )
      
      extraidas_ic <- regmatches(
        bloque_ic,
        coincidencias_ic
      )
      
      extraidas_ic <- extraidas_ic[
        lengths(extraidas_ic) == 9L
      ]
      
      if (length(extraidas_ic) > 0L) {
        
        tabla_ic <- bind_rows(
          lapply(
            extraidas_ic,
            function(x) {
              tibble(
                parametro = toupper(x[2]),
                ic95_inferior = a_numero(x[4]),
                ic95_superior = a_numero(x[8])
              )
            }
          )
        ) |>
          distinct(
            parametro,
            .keep_all = TRUE
          )
        
        tabla <- tabla |>
          left_join(
            tabla_ic,
            by = "parametro"
          )
      }
    }
  }
  
  tabla
}

extraer_wald_mplus <- function(
    archivo
) {
  
  lineas <- readLines(
    archivo,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  indice <- grep(
    "Wald Test of Parameter Constraints",
    lineas,
    fixed = TRUE
  )
  
  if (length(indice) == 0L) {
    return(
      tibble(
        estadistico_wald = NA_real_,
        grados_libertad = NA_real_,
        p_valor = NA_real_
      )
    )
  }
  
  bloque <- lineas[
    indice[1]:
      min(
        length(lineas),
        indice[1] + 25L
      )
  ]
  
  tibble(
    estadistico_wald = extraer_linea_numero(
      bloque,
      "^\\s*Value\\s+"
    ),
    grados_libertad = extraer_linea_numero(
      bloque,
      "^\\s*Degrees of Freedom\\s+"
    ),
    p_valor = extraer_linea_numero(
      bloque,
      "^\\s*P-Value\\s+"
    )
  )
}

# ------------------------------------------------------------------------------
# 5. Localizar archivos definitivos
# ------------------------------------------------------------------------------

archivo_5 <- primer_archivo_existente(
  c(
    file.path(
      ruta_mplus,
      "LCA_5_clases_final.out"
    ),
    file.path(
      ruta_mplus,
      "lca_5_clases_final.out"
    )
  ),
  obligatorio = FALSE
)

archivo_6 <- primer_archivo_existente(
  c(
    file.path(
      ruta_mplus,
      "LCA_6_clases_final.out"
    ),
    file.path(
      ruta_mplus,
      "BCH_6_clases_sensibilidad_paso1.out"
    ),
    file.path(
      ruta_mplus,
      "bch_6_clases_sensibilidad_paso1.out"
    )
  )
)

archivo_7 <- primer_archivo_existente(
  c(
    file.path(
      ruta_mplus,
      "LCA_7_clases_final.out"
    ),
    file.path(
      ruta_mplus,
      "BCH_7_clases_paso1.out"
    ),
    file.path(
      ruta_mplus,
      "bch_7_clases_paso1.out"
    )
  )
)

archivo_bch_7 <- primer_archivo_existente(
  c(
    file.path(
      ruta_mplus,
      "BCH_7_clases_retorno_ajustado_periodo_CORREGIDO.out"
    ),
    file.path(
      ruta_mplus,
      "BCH_7_clases_retorno_ajustado_periodo.out"
    ),
    file.path(
      ruta_mplus,
      "bch_7_clases_retorno_ajustado_periodo.out"
    )
  )
)

archivo_bch_6 <- primer_archivo_existente(
  c(
    file.path(
      ruta_mplus,
      "BCH_6_clases_retorno_ajustado_periodo.out"
    ),
    file.path(
      ruta_mplus,
      "bch_6_clases_retorno_ajustado_periodo.out"
    )
  )
)

# ------------------------------------------------------------------------------
# 6. Tabla principal 1: ajuste de los modelos finalistas
# ------------------------------------------------------------------------------

ajustes <- list()

if (!is.na(archivo_5)) {
  ajustes[[length(ajustes) + 1L]] <-
    extraer_ajuste_mplus(
      archivo_5,
      5L
    )
}

ajustes[[length(ajustes) + 1L]] <-
  extraer_ajuste_mplus(
    archivo_6,
    6L
  )

ajustes[[length(ajustes) + 1L]] <-
  extraer_ajuste_mplus(
    archivo_7,
    7L
  )

tabla_ajuste <- bind_rows(
  ajustes
) |>
  mutate(
    rol = case_when(
      clases == 7L ~ "Modelo principal",
      clases == 6L ~ "Sensibilidad principal",
      clases == 5L ~ "Sensibilidad adicional",
      TRUE ~ NA_character_
    )
  ) |>
  arrange(
    clases
  )

print(tabla_ajuste)

write_csv(
  tabla_ajuste,
  file.path(
    ruta_final,
    "Tabla_1_ajuste_modelos_LCA.csv"
  )
)

# ------------------------------------------------------------------------------
# 7. Tabla principal 2: prevalencia de los siete perfiles
# ------------------------------------------------------------------------------

ruta_prevalencia <- primer_archivo_existente(
  c(
    file.path(
      ruta_tablas,
      "tabla_prevalencia_ponderada_perfiles_7_clases.csv"
    ),
    file.path(
      ruta_tablas,
      "prevalencia_perfiles_7_clases.csv"
    )
  )
)

tabla_prevalencia <- read_csv(
  ruta_prevalencia,
  show_col_types = FALSE
)

write_csv(
  tabla_prevalencia,
  file.path(
    ruta_final,
    "Tabla_2_prevalencia_perfiles_7_clases.csv"
  )
)

# ------------------------------------------------------------------------------
# 8. Tabla principal 3: retorno ajustado en siete clases
# ------------------------------------------------------------------------------

parametros_7 <- extraer_parametros_mplus(
  archivo_bch_7
)

wald_7 <- extraer_wald_mplus(
  archivo_bch_7
)

perfiles_7 <- tibble(
  clase = 1:7,
  perfil = c(
    "Migracion laboral adulta sin documento",
    "Migracion laboral adulta documentada por oferta",
    "Migracion joven transnacional documentada",
    "Migracion laboral juvenil sin documento",
    "Migracion familiar de mayor edad",
    "Migracion juvenil documentada por oferta",
    "Migracion juvenil temporal por trabajo o estudio"
  )
)

tabla_retorno_7 <- parametros_7 |>
  filter(
    grepl(
      "^RS[1-7]$",
      parametro
    )
  ) |>
  mutate(
    clase = as.integer(
      sub(
        "^RS",
        "",
        parametro
      )
    ),
    retorno_ajustado_porcentaje =
      100 * estimacion,
    ic95_inferior_porcentaje =
      100 * ic95_inferior,
    ic95_superior_porcentaje =
      100 * ic95_superior
  ) |>
  left_join(
    perfiles_7,
    by = "clase"
  ) |>
  arrange(
    clase
  ) |>
  select(
    clase,
    perfil,
    estimacion,
    retorno_ajustado_porcentaje,
    error_estandar,
    ic95_inferior,
    ic95_superior,
    ic95_inferior_porcentaje,
    ic95_superior_porcentaje,
    p_valor
  )

if (nrow(tabla_retorno_7) != 7L) {
  stop(
    "No se recuperaron las siete probabilidades RS1-RS7."
  )
}

write_csv(
  tabla_retorno_7,
  file.path(
    ruta_final,
    "Tabla_3_retorno_ajustado_7_clases.csv"
  )
)

efecto_periodo_7 <- parametros_7 |>
  filter(
    parametro %in%
      c(
        "OR20",
        "OR22"
      )
  ) |>
  mutate(
    comparacion = case_when(
      parametro == "OR20" ~
        "2020-2021 frente a 2018-2019",
      parametro == "OR22" ~
        "2022-2023 frente a 2018-2019"
    )
  ) |>
  select(
    parametro,
    comparacion,
    estimacion,
    error_estandar,
    ic95_inferior,
    ic95_superior,
    p_valor
  )

write_csv(
  efecto_periodo_7,
  file.path(
    ruta_final,
    "Tabla_4_efecto_periodo_retorno.csv"
  )
)

# ------------------------------------------------------------------------------
# 9. Tabla principal 5: sensibilidad de seis frente a siete clases
# ------------------------------------------------------------------------------

parametros_6 <- extraer_parametros_mplus(
  archivo_bch_6
)

wald_6 <- extraer_wald_mplus(
  archivo_bch_6
)

perfiles_6 <- tibble(
  clase_6 = 1:6,
  perfil_6 = c(
    "Migracion joven transnacional documentada",
    "Migracion laboral adulta sin documento",
    "Migracion adulta documentada o familiar",
    "Migracion juvenil documentada por oferta",
    "Migracion juvenil temporal por trabajo o estudio",
    "Migracion laboral juvenil sin documento"
  ),
  correspondencia_7 = c(
    "Clase 3",
    "Clase 1",
    "Clases 2 y 5",
    "Clase 6",
    "Clase 7",
    "Clase 4"
  )
)

tabla_retorno_6 <- parametros_6 |>
  filter(
    grepl(
      "^RS[1-6]$",
      parametro
    )
  ) |>
  mutate(
    clase_6 = as.integer(
      sub(
        "^RS",
        "",
        parametro
      )
    ),
    retorno_ajustado_6_porcentaje =
      100 * estimacion,
    ic95_inferior_6_porcentaje =
      100 * ic95_inferior,
    ic95_superior_6_porcentaje =
      100 * ic95_superior
  ) |>
  left_join(
    perfiles_6,
    by = "clase_6"
  ) |>
  arrange(
    clase_6
  )

if (nrow(tabla_retorno_6) != 6L) {
  stop(
    "No se recuperaron las seis probabilidades RS1-RS6."
  )
}

comparacion_6_7 <- tabla_retorno_6 |>
  transmute(
    clase_6,
    perfil_6,
    correspondencia_7,
    retorno_ajustado_6_porcentaje,
    ic95_inferior_6_porcentaje,
    ic95_superior_6_porcentaje,
    retorno_correspondiente_7_porcentaje =
      case_when(
        clase_6 == 1L ~
          tabla_retorno_7$retorno_ajustado_porcentaje[
            tabla_retorno_7$clase == 3L
          ],
        clase_6 == 2L ~
          tabla_retorno_7$retorno_ajustado_porcentaje[
            tabla_retorno_7$clase == 1L
          ],
        clase_6 == 3L ~
          mean(
            tabla_retorno_7$retorno_ajustado_porcentaje[
              tabla_retorno_7$clase %in%
                c(
                  2L,
                  5L
                )
            ]
          ),
        clase_6 == 4L ~
          tabla_retorno_7$retorno_ajustado_porcentaje[
            tabla_retorno_7$clase == 6L
          ],
        clase_6 == 5L ~
          tabla_retorno_7$retorno_ajustado_porcentaje[
            tabla_retorno_7$clase == 7L
          ],
        clase_6 == 6L ~
          tabla_retorno_7$retorno_ajustado_porcentaje[
            tabla_retorno_7$clase == 4L
          ]
      ),
    diferencia_absoluta_puntos_porcentuales =
      retorno_ajustado_6_porcentaje -
      retorno_correspondiente_7_porcentaje
  )

write_csv(
  comparacion_6_7,
  file.path(
    ruta_final,
    "Tabla_5_sensibilidad_6_vs_7_clases.csv"
  )
)

# ------------------------------------------------------------------------------
# 10. Tablas suplementarias
# ------------------------------------------------------------------------------

ruta_caracterizacion <- primer_archivo_existente(
  c(
    file.path(
      ruta_tablas,
      "caracterizacion_posterior_ponderada_7_perfiles.csv"
    )
  ),
  obligatorio = FALSE
)

if (!is.na(ruta_caracterizacion)) {
  
  caracterizacion <- read_csv(
    ruta_caracterizacion,
    show_col_types = FALSE
  )
  
  write_csv(
    caracterizacion,
    file.path(
      ruta_final,
      "Tabla_S1_caracterizacion_posterior_7_perfiles.csv"
    )
  )
}

ruta_cox_intervalos <- primer_archivo_existente(
  c(
    file.path(
      ruta_tablas,
      "HR_retorno_por_intervalo_y_perfil.csv"
    )
  ),
  obligatorio = FALSE
)

if (!is.na(ruta_cox_intervalos)) {
  
  cox_intervalos <- read_csv(
    ruta_cox_intervalos,
    show_col_types = FALSE
  )
  
  write_csv(
    cox_intervalos,
    file.path(
      ruta_final,
      "Tabla_S2_Cox_por_intervalos.csv"
    )
  )
}

ruta_pruebas_cox <- primer_archivo_existente(
  c(
    file.path(
      ruta_tablas,
      "pruebas_globales_Cox_por_intervalos.csv"
    )
  ),
  obligatorio = FALSE
)

if (!is.na(ruta_pruebas_cox)) {
  
  pruebas_cox <- read_csv(
    ruta_pruebas_cox,
    show_col_types = FALSE
  )
  
  write_csv(
    pruebas_cox,
    file.path(
      ruta_final,
      "Tabla_S3_pruebas_globales_Cox.csv"
    )
  )
}

# ------------------------------------------------------------------------------
# 11. Figura 1: prevalencia poblacional de los siete perfiles
# ------------------------------------------------------------------------------

columna_prevalencia <- intersect(
  c(
    "porcentaje_posterior_ponderado",
    "prevalencia_posterior_ponderada",
    "porcentaje_modal_ponderado"
  ),
  names(
    tabla_prevalencia
  )
)

if (length(columna_prevalencia) == 0L) {
  stop(
    "No se encontro una columna de prevalencia ponderada."
  )
}

columna_prevalencia <- columna_prevalencia[1]

datos_prevalencia <- tabla_prevalencia |>
  mutate(
    prevalencia_grafico =
      .data[[columna_prevalencia]],
    perfil_grafico = if (
      "perfil" %in%
      names(tabla_prevalencia)
    ) {
      perfil
    } else if (
      "perfil_7" %in%
      names(tabla_prevalencia)
    ) {
      as.character(
        perfil_7
      )
    } else {
      paste0(
        "Clase ",
        clase
      )
    },
    perfil_grafico = reorder(
      perfil_grafico,
      prevalencia_grafico
    )
  )

figura_prevalencia <- ggplot(
  datos_prevalencia,
  aes(
    x = prevalencia_grafico,
    y = perfil_grafico
  )
) +
  geom_col(
    width = 0.7
  ) +
  geom_text(
    aes(
      label = paste0(
        round(
          prevalencia_grafico,
          1
        ),
        "%"
      )
    ),
    hjust = -0.1,
    size = 3.5
  ) +
  scale_x_continuous(
    labels = label_percent(
      scale = 1,
      accuracy = 1
    ),
    expand = expansion(
      mult = c(
        0,
        0.15
      )
    )
  ) +
  labs(
    x = "Prevalencia poblacional estimada",
    y = NULL,
    title = "Prevalencia de los siete perfiles migratorios"
  ) +
  theme_minimal(
    base_size = 11
  )

ggsave(
  filename = file.path(
    ruta_final,
    "Figura_1_prevalencia_7_perfiles.png"
  ),
  plot = figura_prevalencia,
  width = 10,
  height = 6.5,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 12. Figura 2: probabilidad ajustada de retorno
# ------------------------------------------------------------------------------

datos_retorno_figura <- tabla_retorno_7 |>
  mutate(
    perfil = factor(
      perfil,
      levels = rev(
        perfil
      )
    )
  )

figura_retorno <- ggplot(
  datos_retorno_figura,
  aes(
    x = retorno_ajustado_porcentaje,
    y = perfil
  )
) +
  geom_errorbarh(
    aes(
      xmin = ic95_inferior_porcentaje,
      xmax = ic95_superior_porcentaje
    ),
    height = 0.2,
    linewidth = 0.6
  ) +
  geom_point(
    size = 2.7
  ) +
  scale_x_continuous(
    labels = label_percent(
      scale = 1,
      accuracy = 1
    ),
    limits = c(
      0,
      max(
        datos_retorno_figura$ic95_superior_porcentaje,
        na.rm = TRUE
      ) * 1.08
    )
  ) +
  labs(
    x = "Probabilidad ajustada de retorno",
    y = NULL,
    title = "Retorno a Mexico según perfil migratorio",
    subtitle = paste0(
      "BCH manual, diseño complejo y estandarizacion ",
      "por periodo de salida"
    )
  ) +
  theme_minimal(
    base_size = 11
  )

ggsave(
  filename = file.path(
    ruta_final,
    "Figura_2_retorno_ajustado_7_perfiles.png"
  ),
  plot = figura_retorno,
  width = 10,
  height = 6.5,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 13. Figura 3: comparación de sensibilidad
# ------------------------------------------------------------------------------

datos_sensibilidad <- bind_rows(
  tabla_retorno_6 |>
    transmute(
      modelo = "6 clases",
      perfil = perfil_6,
      probabilidad = retorno_ajustado_6_porcentaje,
      inferior = ic95_inferior_6_porcentaje,
      superior = ic95_superior_6_porcentaje
    ),
  tabla_retorno_7 |>
    transmute(
      modelo = "7 clases",
      perfil = perfil,
      probabilidad = retorno_ajustado_porcentaje,
      inferior = ic95_inferior_porcentaje,
      superior = ic95_superior_porcentaje
    )
)

figura_sensibilidad <- ggplot(
  datos_sensibilidad,
  aes(
    x = probabilidad,
    y = reorder(
      perfil,
      probabilidad
    ),
    shape = modelo
  )
) +
  geom_errorbarh(
    aes(
      xmin = inferior,
      xmax = superior
    ),
    height = 0.15,
    linewidth = 0.5
  ) +
  geom_point(
    size = 2.4
  ) +
  scale_x_continuous(
    labels = label_percent(
      scale = 1,
      accuracy = 1
    )
  ) +
  labs(
    x = "Probabilidad ajustada de retorno",
    y = NULL,
    shape = "Solucion",
    title = "Sensibilidad del retorno: soluciones de seis y siete clases"
  ) +
  theme_minimal(
    base_size = 10.5
  )

ggsave(
  filename = file.path(
    ruta_final,
    "Figura_3_sensibilidad_6_vs_7_clases.png"
  ),
  plot = figura_sensibilidad,
  width = 10,
  height = 8,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 14. Resumen analitico final
# ------------------------------------------------------------------------------

efecto_periodo_6 <- parametros_6 |>
  filter(
    parametro %in%
      c(
        "OR20",
        "OR22"
      )
  ) |>
  select(
    parametro,
    estimacion,
    ic95_inferior,
    ic95_superior,
    p_valor
  )

resumen <- c(
  "CIERRE ANALITICO DEL ESTUDIO",
  "",
  "Modelo principal:",
  "- solucion de siete clases latentes;",
  "- seleccionada por ajuste, calidad de clasificacion e interpretabilidad;",
  "- solucion de seis clases conservada como sensibilidad.",
  "",
  paste0(
    "Prueba global del retorno, siete clases: Wald chi-cuadrado(",
    wald_7$grados_libertad,
    ")=",
    round(
      wald_7$estadistico_wald,
      3
    ),
    "; p=",
    format(
      wald_7$p_valor,
      scientific = TRUE,
      digits = 3
    ),
    "."
  ),
  paste0(
    "Prueba global del retorno, seis clases: Wald chi-cuadrado(",
    wald_6$grados_libertad,
    ")=",
    round(
      wald_6$estadistico_wald,
      3
    ),
    "; p=",
    format(
      wald_6$p_valor,
      scientific = TRUE,
      digits = 3
    ),
    "."
  ),
  "",
  paste0(
    "Menor retorno en siete clases: ",
    tabla_retorno_7$perfil[
      which.min(
        tabla_retorno_7$retorno_ajustado_porcentaje
      )
    ],
    " (",
    round(
      min(
        tabla_retorno_7$retorno_ajustado_porcentaje
      ),
      1
    ),
    "%)."
  ),
  paste0(
    "Mayor retorno en siete clases: ",
    tabla_retorno_7$perfil[
      which.max(
        tabla_retorno_7$retorno_ajustado_porcentaje
      )
    ],
    " (",
    round(
      max(
        tabla_retorno_7$retorno_ajustado_porcentaje
      ),
      1
    ),
    "%)."
  ),
  "",
  "Sensibilidad:",
  "- el orden sustantivo de los perfiles se conserva con seis clases;",
  "- el perfil adulto documentado/familiar se divide en dos clases en",
  "  la solucion principal de siete clases;",
  "- el efecto del periodo de salida es practicamente identico.",
  "",
  "Analisis temporal secundario:",
  "- el supuesto de riesgos proporcionales no se cumplio;",
  "- se estimaron HR separados para 0-12, >12-24 y >24 meses;",
  "- las mayores diferencias entre perfiles ocurren durante el primer ano.",
  "",
  "Archivos finales disponibles en:",
  paste0(
    "- ",
    ruta_final
  )
)

writeLines(
  resumen,
  file.path(
    ruta_final,
    "Resumen_analitico_final.txt"
  )
)

# ------------------------------------------------------------------------------
# 15. Inventario de productos
# ------------------------------------------------------------------------------

inventario <- tibble(
  archivo = list.files(
    ruta_final,
    full.names = FALSE
  ),
  ruta = list.files(
    ruta_final,
    full.names = TRUE
  )
) |>
  mutate(
    tamano_kb = round(
      file.info(
        ruta
      )$size /
        1024,
      1
    )
  )

write_csv(
  inventario,
  file.path(
    ruta_final,
    "Inventario_resultados_finales.csv"
  )
)

cat("\n")
cat("============================================================\n")
cat("CONSOLIDACION FINAL TERMINADA CORRECTAMENTE\n\n")
cat(
  "Productos guardados en: ",
  ruta_final,
  "\n",
  sep = ""
)
cat(
  "Tablas principales: 5.\n"
)
cat(
  "Figuras nuevas: 3.\n"
)
cat(
  "Resumen analitico: Resumen_analitico_final.txt\n"
)
cat("============================================================\n")