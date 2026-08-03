# ==============================================================================
# PROYECTO: Perfiles migratorios Mexico-Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 12D: Diagnostico PH corregido con pesos normalizados
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
  "tibble",
  "survival"
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
library(tibble)
library(survival)

# ------------------------------------------------------------------------------
# 2. Rutas
# ------------------------------------------------------------------------------

ruta_procesados <- "04_datos_procesados"
ruta_tablas <- "05_tablas"
ruta_figuras <- "06_figuras"
ruta_modelos <- "07_modelos"
ruta_documentacion <- "09_documentacion"

ruta_octubre <- file.path(
  ruta_procesados,
  "base_supervivencia_octubre2023.rds"
)

ruta_septiembre <- file.path(
  ruta_procesados,
  "base_supervivencia_septiembre2023.rds"
)

if (!file.exists(ruta_octubre) || !file.exists(ruta_septiembre)) {
  stop(
    "Faltan las bases de supervivencia. Ejecute primero el script 12B."
  )
}

base_octubre <- readRDS(ruta_octubre)
base_septiembre <- readRDS(ruta_septiembre)

# ------------------------------------------------------------------------------
# 3. Preparacion
# ------------------------------------------------------------------------------

preparar_base <- function(datos) {
  
  datos |>
    mutate(
      perfil_num = relevel(
        factor(
          perfil_num,
          levels = paste0(
            "Clase ",
            1:7
          )
        ),
        ref = "Clase 4"
      ),
      
      # FAC_HOG se normaliza para que la media sea 1.
      # Esto evita que cox.zph interprete la escala poblacional como
      # una multiplicacion artificial del tamano muestral.
      peso_normalizado =
        fac_hog_num /
        mean(
          fac_hog_num,
          na.rm = TRUE
        )
    )
}

base_octubre <- preparar_base(base_octubre)
base_septiembre <- preparar_base(base_septiembre)

auditoria_pesos <- bind_rows(
  base_octubre |>
    summarise(
      escenario = "Octubre 2023",
      n = n(),
      peso_original_min = min(fac_hog_num),
      peso_original_media = mean(fac_hog_num),
      peso_original_max = max(fac_hog_num),
      peso_normalizado_min = min(peso_normalizado),
      peso_normalizado_media = mean(peso_normalizado),
      peso_normalizado_max = max(peso_normalizado)
    ),
  base_septiembre |>
    summarise(
      escenario = "Septiembre 2023",
      n = n(),
      peso_original_min = min(fac_hog_num),
      peso_original_media = mean(fac_hog_num),
      peso_original_max = max(fac_hog_num),
      peso_normalizado_min = min(peso_normalizado),
      peso_normalizado_media = mean(peso_normalizado),
      peso_normalizado_max = max(peso_normalizado)
    )
)

print(auditoria_pesos)

write_csv(
  auditoria_pesos,
  file.path(
    ruta_tablas,
    "auditoria_pesos_diagnostico_PH.csv"
  )
)

# ------------------------------------------------------------------------------
# 4. Ajustes auxiliares para cox.zph
# ------------------------------------------------------------------------------

ajustar_modelos_diagnostico <- function(
    datos,
    tiempo
) {
  
  formula_modelo <- as.formula(
    paste0(
      "Surv(",
      tiempo,
      ", evento_retorno) ~ perfil_num"
    )
  )
  
  modelo_ponderado <- coxph(
    formula = formula_modelo,
    data = datos,
    weights = peso_normalizado,
    cluster = upm_dis_num,
    robust = TRUE,
    ties = "efron",
    x = TRUE,
    y = TRUE,
    model = TRUE
  )
  
  modelo_no_ponderado <- coxph(
    formula = formula_modelo,
    data = datos,
    cluster = upm_dis_num,
    robust = TRUE,
    ties = "efron",
    x = TRUE,
    y = TRUE,
    model = TRUE
  )
  
  list(
    ponderado = modelo_ponderado,
    no_ponderado = modelo_no_ponderado
  )
}

modelos_octubre <- ajustar_modelos_diagnostico(
  base_octubre,
  "tiempo_octubre_analisis"
)

modelos_septiembre <- ajustar_modelos_diagnostico(
  base_septiembre,
  "tiempo_septiembre_analisis"
)

# ------------------------------------------------------------------------------
# 5. Pruebas de Schoenfeld
# ------------------------------------------------------------------------------

crear_diagnosticos <- function(
    modelo,
    escenario,
    ponderacion
) {
  
  prueba_termino <- cox.zph(
    modelo,
    transform = "km",
    terms = TRUE,
    singledf = FALSE,
    global = TRUE
  )
  
  prueba_coeficientes <- cox.zph(
    modelo,
    transform = "km",
    terms = FALSE,
    global = TRUE
  )
  
  list(
    termino = prueba_termino,
    coeficientes = prueba_coeficientes,
    escenario = escenario,
    ponderacion = ponderacion
  )
}

diagnosticos <- list(
  crear_diagnosticos(
    modelos_octubre$ponderado,
    "Octubre 2023",
    "Peso normalizado"
  ),
  crear_diagnosticos(
    modelos_octubre$no_ponderado,
    "Octubre 2023",
    "Sin ponderacion"
  ),
  crear_diagnosticos(
    modelos_septiembre$ponderado,
    "Septiembre 2023",
    "Peso normalizado"
  ),
  crear_diagnosticos(
    modelos_septiembre$no_ponderado,
    "Septiembre 2023",
    "Sin ponderacion"
  )
)

extraer_zph <- function(
    objeto,
    nivel
) {
  
  tabla <- as.data.frame(
    objeto[[nivel]]$table
  )
  
  tabla$termino <- rownames(tabla)
  rownames(tabla) <- NULL
  
  as_tibble(tabla) |>
    transmute(
      escenario = objeto$escenario,
      ponderacion = objeto$ponderacion,
      nivel = nivel,
      termino = termino,
      chi_cuadrado = chisq,
      grados_libertad = df,
      p_valor = p
    )
}

tabla_zph <- bind_rows(
  lapply(
    diagnosticos,
    extraer_zph,
    nivel = "termino"
  ),
  lapply(
    diagnosticos,
    extraer_zph,
    nivel = "coeficientes"
  )
)

print(tabla_zph)

write_csv(
  tabla_zph,
  file.path(
    ruta_tablas,
    "diagnostico_PH_pesos_normalizados.csv"
  )
)

# ------------------------------------------------------------------------------
# 6. Comparacion de coeficientes
# ------------------------------------------------------------------------------

extraer_coeficientes <- function(
    modelo,
    escenario,
    ponderacion
) {
  
  beta <- coef(modelo)
  se <- sqrt(diag(vcov(modelo)))
  
  tibble(
    escenario = escenario,
    ponderacion = ponderacion,
    contraste = names(beta),
    log_hazard_ratio = as.numeric(beta),
    error_estandar_robusto = as.numeric(se),
    hazard_ratio = exp(beta),
    ic95_inferior = exp(
      beta -
        qnorm(0.975) * se
    ),
    ic95_superior = exp(
      beta +
        qnorm(0.975) * se
    )
  )
}

tabla_coeficientes <- bind_rows(
  extraer_coeficientes(
    modelos_octubre$ponderado,
    "Octubre 2023",
    "Peso normalizado"
  ),
  extraer_coeficientes(
    modelos_octubre$no_ponderado,
    "Octubre 2023",
    "Sin ponderacion"
  ),
  extraer_coeficientes(
    modelos_septiembre$ponderado,
    "Septiembre 2023",
    "Peso normalizado"
  ),
  extraer_coeficientes(
    modelos_septiembre$no_ponderado,
    "Septiembre 2023",
    "Sin ponderacion"
  )
)

write_csv(
  tabla_coeficientes,
  file.path(
    ruta_tablas,
    "modelos_auxiliares_diagnostico_PH.csv"
  )
)

# ------------------------------------------------------------------------------
# 7. Graficos de residuos
# ------------------------------------------------------------------------------

guardar_grafico <- function(
    prueba,
    archivo
) {
  
  png(
    filename = archivo,
    width = 3000,
    height = 2200,
    res = 300
  )
  
  par(
    mfrow = c(2, 3),
    mar = c(4, 4, 2, 1)
  )
  
  plot(prueba)
  
  dev.off()
}

guardar_grafico(
  diagnosticos[[1]]$coeficientes,
  file.path(
    ruta_figuras,
    "diagnostico_PH_octubre_peso_normalizado.png"
  )
)

guardar_grafico(
  diagnosticos[[3]]$coeficientes,
  file.path(
    ruta_figuras,
    "diagnostico_PH_septiembre_peso_normalizado.png"
  )
)

# ------------------------------------------------------------------------------
# 8. Dictamen
# ------------------------------------------------------------------------------

globales <- tabla_zph |>
  filter(
    nivel == "termino",
    termino == "GLOBAL"
  ) |>
  select(
    escenario,
    ponderacion,
    p_valor
  )

print(globales)

globales_ponderados <- globales |>
  filter(
    ponderacion == "Peso normalizado"
  )

globales_no_ponderados <- globales |>
  filter(
    ponderacion == "Sin ponderacion"
  )

evidencia_ponderada <- any(
  globales_ponderados$p_valor < 0.05
)

evidencia_no_ponderada <- any(
  globales_no_ponderados$p_valor < 0.05
)

dictamen <- case_when(
  evidencia_ponderada &
    evidencia_no_ponderada ~
    paste0(
      "La evidencia de no proporcionalidad es consistente en los ",
      "diagnosticos ponderados normalizados y no ponderados."
    ),
  
  evidencia_ponderada |
    evidencia_no_ponderada ~
    paste0(
      "La evidencia de no proporcionalidad es mixta entre los ",
      "diagnosticos auxiliares."
    ),
  
  TRUE ~
    paste0(
      "No se detecto evidencia global de no proporcionalidad en ",
      "los diagnosticos auxiliares."
    )
)

informe <- c(
  "DIAGNOSTICO PH CORREGIDO TERMINADO",
  "",
  "Los pesos FAC_HOG fueron normalizados a media 1.",
  "Se repitio cox.zph con pesos normalizados y sin ponderacion.",
  "Las varianzas de los modelos auxiliares se calcularon por UPM.",
  "",
  paste0(
    "Dictamen: ",
    dictamen
  ),
  "",
  "IMPORTANTE:",
  "cox.zph no es una prueba plenamente ajustada al diseno complejo.",
  "Se usa como diagnostico auxiliar. Si confirma no proporcionalidad,",
  "el siguiente modelo debe permitir efectos del perfil variables en el tiempo.",
  "",
  "Archivos:",
  "- 05_tablas/auditoria_pesos_diagnostico_PH.csv",
  "- 05_tablas/diagnostico_PH_pesos_normalizados.csv",
  "- 05_tablas/modelos_auxiliares_diagnostico_PH.csv",
  "- 06_figuras/diagnostico_PH_octubre_peso_normalizado.png",
  "- 06_figuras/diagnostico_PH_septiembre_peso_normalizado.png"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_12D_diagnostico_PH_corregido.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")