# ==============================================================================
# PROYECTO: Perfiles migratorios Mexico-Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 12E: Cox por intervalos con efectos del perfil variables en el tiempo
# ==============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  survey.lonely.psu = "adjust"
)

# ------------------------------------------------------------------------------
# 1. Paquetes
# ------------------------------------------------------------------------------

paquetes <- c(
  "dplyr",
  "readr",
  "tibble",
  "tidyr",
  "survival",
  "survey",
  "ggplot2"
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
library(tidyr)
library(survival)
library(survey)
library(ggplot2)

# ------------------------------------------------------------------------------
# 2. Rutas e importacion
# ------------------------------------------------------------------------------

ruta_procesados <- "04_datos_procesados"
ruta_tablas <- "05_tablas"
ruta_figuras <- "06_figuras"
ruta_modelos <- "07_modelos"
ruta_documentacion <- "09_documentacion"

dir.create(ruta_tablas, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_figuras, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_modelos, recursive = TRUE, showWarnings = FALSE)

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
# 3. Definiciones
# ------------------------------------------------------------------------------

clases_comparadas <- c(1L, 2L, 3L, 5L, 6L, 7L)

etiquetas_perfiles <- c(
  "1" = "Laboral adulta sin documento",
  "2" = "Laboral adulta documentada",
  "3" = "Joven transnacional documentada",
  "4" = "Laboral juvenil sin documento",
  "5" = "Familiar de mayor edad",
  "6" = "Juvenil documentada por oferta",
  "7" = "Juvenil temporal por trabajo o estudio"
)

etiquetas_intervalos <- c(
  "0-12 meses",
  ">12-24 meses",
  ">24 meses"
)

# ------------------------------------------------------------------------------
# 4. Division del seguimiento
# ------------------------------------------------------------------------------

preparar_datos_intervalos <- function(
    datos,
    variable_tiempo,
    escenario
) {
  
  formula_surv <- as.formula(
    paste0(
      "Surv(",
      variable_tiempo,
      ", evento_retorno) ~ ."
    )
  )
  
  datos_divididos <- survSplit(
    formula = formula_surv,
    data = datos,
    cut = c(12, 24),
    start = "tstart",
    end = "tstop",
    event = "evento_intervalo",
    episode = "episodio",
    zero = 0
  ) |>
    mutate(
      escenario = escenario,
      intervalo = factor(
        episodio,
        levels = 1:3,
        labels = etiquetas_intervalos
      ),
      i2 = as.integer(episodio == 2L),
      i3 = as.integer(episodio == 3L),
      
      p1 = as.integer(clase_modal == 1L),
      p2 = as.integer(clase_modal == 2L),
      p3 = as.integer(clase_modal == 3L),
      p5 = as.integer(clase_modal == 5L),
      p6 = as.integer(clase_modal == 6L),
      p7 = as.integer(clase_modal == 7L),
      
      p1_i2 = p1 * i2,
      p2_i2 = p2 * i2,
      p3_i2 = p3 * i2,
      p5_i2 = p5 * i2,
      p6_i2 = p6 * i2,
      p7_i2 = p7 * i2,
      
      p1_i3 = p1 * i3,
      p2_i3 = p2 * i3,
      p3_i3 = p3 * i3,
      p5_i3 = p5 * i3,
      p6_i3 = p6 * i3,
      p7_i3 = p7 * i3,
      
      duracion_intervalo = tstop - tstart
    )
  
  if (
    anyNA(datos_divididos$tstart) ||
    anyNA(datos_divididos$tstop) ||
    any(datos_divididos$tstop <= datos_divididos$tstart)
  ) {
    stop(
      paste0(
        "Se detectaron intervalos temporales invalidos en ",
        escenario,
        "."
      )
    )
  }
  
  datos_divididos
}

split_octubre <- preparar_datos_intervalos(
  base_octubre,
  "tiempo_octubre_analisis",
  "Censura en octubre de 2023"
)

split_septiembre <- preparar_datos_intervalos(
  base_septiembre,
  "tiempo_septiembre_analisis",
  "Censura en septiembre de 2023"
)

saveRDS(
  split_octubre,
  file.path(
    ruta_procesados,
    "base_supervivencia_intervalos_octubre2023.rds"
  )
)

saveRDS(
  split_septiembre,
  file.path(
    ruta_procesados,
    "base_supervivencia_intervalos_septiembre2023.rds"
  )
)

# ------------------------------------------------------------------------------
# 5. Auditoria por intervalo y perfil
# ------------------------------------------------------------------------------

resumir_intervalos <- function(datos) {
  
  datos |>
    group_by(
      escenario,
      intervalo,
      clase_modal
    ) |>
    summarise(
      personas_en_riesgo = n_distinct(id),
      filas_intervalo = n(),
      eventos = sum(evento_intervalo),
      eventos_ponderados = sum(
        fac_hog_num * evento_intervalo
      ),
      persona_meses_ponderados = sum(
        fac_hog_num * duracion_intervalo
      ),
      .groups = "drop"
    ) |>
    mutate(
      perfil = unname(
        etiquetas_perfiles[
          as.character(clase_modal)
        ]
      ),
      tasa_por_1000_persona_meses =
        1000 *
        eventos_ponderados /
        persona_meses_ponderados
    ) |>
    select(
      escenario,
      intervalo,
      clase_modal,
      perfil,
      personas_en_riesgo,
      eventos,
      persona_meses_ponderados,
      tasa_por_1000_persona_meses
    )
}

auditoria_intervalos <- bind_rows(
  resumir_intervalos(split_octubre),
  resumir_intervalos(split_septiembre)
)

print(auditoria_intervalos, n = Inf)

write_csv(
  auditoria_intervalos,
  file.path(
    ruta_tablas,
    "auditoria_eventos_por_intervalo_y_perfil.csv"
  )
)

# ------------------------------------------------------------------------------
# 6. Disenos complejos y modelos Cox por intervalos
# ------------------------------------------------------------------------------

# No se pasa ties = "efron" a svycoxph().
# En esta combinación de versiones, el argumento puede ser interpretado por
# model.frame como si fuera una variable. El método Efron ya es el
# predeterminado del ajuste Cox subyacente.

crear_diseno <- function(datos) {
  
  svydesign(
    ids = ~upm_dis_num,
    strata = ~est_dis_num,
    weights = ~fac_hog_num,
    nest = TRUE,
    data = datos
  )
}

diseno_octubre <- crear_diseno(split_octubre)
diseno_septiembre <- crear_diseno(split_septiembre)

formula_modelo <- Surv(
  tstart,
  tstop,
  evento_intervalo
) ~
  p1 + p2 + p3 + p5 + p6 + p7 +
  p1_i2 + p2_i2 + p3_i2 + p5_i2 + p6_i2 + p7_i2 +
  p1_i3 + p2_i3 + p3_i3 + p5_i3 + p6_i3 + p7_i3 +
  strata(intervalo)

modelo_octubre <- svycoxph(
  formula_modelo,
  design = diseno_octubre
)

modelo_septiembre <- svycoxph(
  formula_modelo,
  design = diseno_septiembre
)

saveRDS(
  modelo_octubre,
  file.path(
    ruta_modelos,
    "modelo_cox_intervalos_octubre2023.rds"
  )
)

saveRDS(
  modelo_septiembre,
  file.path(
    ruta_modelos,
    "modelo_cox_intervalos_septiembre2023.rds"
  )
)

# ------------------------------------------------------------------------------
# 7. Funciones para contrastes robustos
# ------------------------------------------------------------------------------

crear_vector_contraste <- function(
    nombres_coeficientes,
    clase,
    episodio
) {
  
  contraste <- setNames(
    rep(
      0,
      length(nombres_coeficientes)
    ),
    nombres_coeficientes
  )
  
  principal <- paste0(
    "p",
    clase
  )
  
  if (!principal %in% nombres_coeficientes) {
    stop(
      paste0(
        "No se encontro el coeficiente ",
        principal,
        "."
      )
    )
  }
  
  contraste[principal] <- 1
  
  if (episodio == 2L) {
    interaccion <- paste0(
      principal,
      "_i2"
    )
    contraste[interaccion] <- 1
  }
  
  if (episodio == 3L) {
    interaccion <- paste0(
      principal,
      "_i3"
    )
    contraste[interaccion] <- 1
  }
  
  contraste
}

estimar_contraste <- function(
    beta,
    matriz_varianza,
    contraste
) {
  
  estimacion <- sum(
    contraste * beta
  )
  
  varianza <- as.numeric(
    t(contraste) %*%
      matriz_varianza %*%
      contraste
  )
  
  if (varianza < 0 && abs(varianza) < 1e-10) {
    varianza <- 0
  }
  
  if (varianza < 0) {
    stop(
      "Se obtuvo una varianza negativa para un contraste."
    )
  }
  
  error_estandar <- sqrt(varianza)
  z <- estimacion / error_estandar
  p <- 2 * pnorm(
    abs(z),
    lower.tail = FALSE
  )
  
  tibble(
    log_hazard_ratio = estimacion,
    error_estandar = error_estandar,
    hazard_ratio = exp(estimacion),
    ic95_inferior = exp(
      estimacion -
        qnorm(0.975) * error_estandar
    ),
    ic95_superior = exp(
      estimacion +
        qnorm(0.975) * error_estandar
    ),
    z = z,
    p_valor = p
  )
}

extraer_hr_intervalos <- function(
    modelo,
    escenario
) {
  
  beta <- coef(modelo)
  matriz_varianza <- vcov(modelo)
  nombres_coeficientes <- names(beta)
  
  resultados <- list()
  contador <- 1L
  
  for (episodio in 1:3) {
    
    for (clase in clases_comparadas) {
      
      contraste <- crear_vector_contraste(
        nombres_coeficientes,
        clase,
        episodio
      )
      
      resultados[[contador]] <- estimar_contraste(
        beta,
        matriz_varianza,
        contraste
      ) |>
        mutate(
          escenario = escenario,
          episodio = episodio,
          intervalo = etiquetas_intervalos[episodio],
          clase = clase,
          perfil = unname(
            etiquetas_perfiles[
              as.character(clase)
            ]
          ),
          referencia = unname(
            etiquetas_perfiles["4"]
          )
        )
      
      contador <- contador + 1L
    }
  }
  
  bind_rows(resultados) |>
    mutate(
      q_FDR_BH = p.adjust(
        p_valor,
        method = "BH"
      ),
      significativa_FDR_0_05 =
        q_FDR_BH < 0.05
    ) |>
    select(
      escenario,
      episodio,
      intervalo,
      clase,
      perfil,
      referencia,
      log_hazard_ratio,
      error_estandar,
      hazard_ratio,
      ic95_inferior,
      ic95_superior,
      z,
      p_valor,
      q_FDR_BH,
      significativa_FDR_0_05
    )
}

hr_intervalos <- bind_rows(
  extraer_hr_intervalos(
    modelo_octubre,
    "Censura en octubre de 2023"
  ),
  extraer_hr_intervalos(
    modelo_septiembre,
    "Censura en septiembre de 2023"
  )
)

print(hr_intervalos, n = Inf)

write_csv(
  hr_intervalos,
  file.path(
    ruta_tablas,
    "HR_retorno_por_intervalo_y_perfil.csv"
  )
)

# ------------------------------------------------------------------------------
# 8. Pruebas Wald globales
# ------------------------------------------------------------------------------

prueba_wald_matriz <- function(
    modelo,
    matriz_contrastes,
    escenario,
    prueba
) {
  
  beta <- coef(modelo)
  matriz_varianza <- vcov(modelo)
  
  estimaciones <- as.numeric(
    matriz_contrastes %*% beta
  )
  
  varianza_contrastes <-
    matriz_contrastes %*%
    matriz_varianza %*%
    t(matriz_contrastes)
  
  solucion <- qr.solve(
    varianza_contrastes,
    estimaciones
  )
  
  estadistico <- as.numeric(
    crossprod(
      estimaciones,
      solucion
    )
  )
  
  grados_libertad <- qr(
    matriz_contrastes
  )$rank
  
  tibble(
    escenario = escenario,
    prueba = prueba,
    estadistico_wald = estadistico,
    grados_libertad = grados_libertad,
    p_valor = pchisq(
      estadistico,
      df = grados_libertad,
      lower.tail = FALSE
    )
  )
}

crear_matriz_intervalo <- function(
    modelo,
    episodio
) {
  
  nombres_coeficientes <- names(
    coef(modelo)
  )
  
  do.call(
    rbind,
    lapply(
      clases_comparadas,
      function(clase) {
        crear_vector_contraste(
          nombres_coeficientes,
          clase,
          episodio
        )
      }
    )
  )
}

crear_matriz_interacciones <- function(modelo) {
  
  nombres_coeficientes <- names(
    coef(modelo)
  )
  
  nombres_interacciones <- c(
    paste0(
      "p",
      clases_comparadas,
      "_i2"
    ),
    paste0(
      "p",
      clases_comparadas,
      "_i3"
    )
  )
  
  matriz <- matrix(
    0,
    nrow = length(nombres_interacciones),
    ncol = length(nombres_coeficientes),
    dimnames = list(
      nombres_interacciones,
      nombres_coeficientes
    )
  )
  
  for (i in seq_along(nombres_interacciones)) {
    matriz[
      i,
      nombres_interacciones[i]
    ] <- 1
  }
  
  matriz
}

extraer_pruebas_modelo <- function(
    modelo,
    escenario
) {
  
  pruebas_intervalo <- bind_rows(
    lapply(
      1:3,
      function(episodio) {
        
        prueba_wald_matriz(
          modelo,
          crear_matriz_intervalo(
            modelo,
            episodio
          ),
          escenario,
          paste0(
            "Diferencias entre perfiles en ",
            etiquetas_intervalos[episodio]
          )
        )
      }
    )
  )
  
  prueba_interaccion <- prueba_wald_matriz(
    modelo,
    crear_matriz_interacciones(modelo),
    escenario,
    paste0(
      "Interaccion global perfil por intervalo; ",
      "cambio de los HR a traves del tiempo"
    )
  )
  
  bind_rows(
    pruebas_intervalo,
    prueba_interaccion
  )
}

pruebas_wald <- bind_rows(
  extraer_pruebas_modelo(
    modelo_octubre,
    "Censura en octubre de 2023"
  ),
  extraer_pruebas_modelo(
    modelo_septiembre,
    "Censura en septiembre de 2023"
  )
)

print(pruebas_wald, n = Inf)

write_csv(
  pruebas_wald,
  file.path(
    ruta_tablas,
    "pruebas_globales_Cox_por_intervalos.csv"
  )
)

# ------------------------------------------------------------------------------
# 9. Sensibilidad octubre frente a septiembre
# ------------------------------------------------------------------------------

sensibilidad <- hr_intervalos |>
  select(
    escenario,
    intervalo,
    clase,
    perfil,
    hazard_ratio,
    ic95_inferior,
    ic95_superior
  ) |>
  tidyr::pivot_wider(
    names_from = escenario,
    values_from = c(
      hazard_ratio,
      ic95_inferior,
      ic95_superior
    ),
    names_sep = "__"
  )

write_csv(
  sensibilidad,
  file.path(
    ruta_tablas,
    "sensibilidad_HR_intervalos_octubre_septiembre.csv"
  )
)

# ------------------------------------------------------------------------------
# 10. Figura de HR por intervalo
# ------------------------------------------------------------------------------

figura_datos <- hr_intervalos |>
  filter(
    escenario ==
      "Censura en octubre de 2023"
  ) |>
  mutate(
    intervalo = factor(
      intervalo,
      levels = etiquetas_intervalos
    )
  )

figura <- ggplot(
  figura_datos,
  aes(
    x = intervalo,
    y = hazard_ratio,
    group = perfil,
    linetype = perfil
  )
) +
  geom_hline(
    yintercept = 1,
    linetype = 2,
    linewidth = 0.5
  ) +
  geom_errorbar(
    aes(
      ymin = ic95_inferior,
      ymax = ic95_superior
    ),
    width = 0.08,
    linewidth = 0.5
  ) +
  geom_line(
    linewidth = 0.7
  ) +
  geom_point(
    size = 2
  ) +
  scale_y_log10() +
  labs(
    x = "Tiempo desde la salida",
    y = "Hazard ratio de retorno, escala logaritmica",
    linetype = "Perfil",
    title = "Asociacion entre perfil migratorio y retorno a traves del tiempo",
    subtitle = paste0(
      "Referencia: migracion laboral juvenil sin documento; ",
      "censura en octubre de 2023"
    )
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    legend.position = "right"
  )

ggsave(
  filename = file.path(
    ruta_figuras,
    "HR_retorno_por_intervalo_7_perfiles.png"
  ),
  plot = figura,
  width = 11,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 11. Informe
# ------------------------------------------------------------------------------

informe <- c(
  "MODELO COX CON EFECTOS VARIABLES EN EL TIEMPO TERMINADO",
  "",
  "Intervalos:",
  "- 0 a 12 meses.",
  "- Mas de 12 a 24 meses.",
  "- Mas de 24 meses.",
  "",
  "El modelo:",
  "- utiliza formato de proceso de conteo;",
  "- permite una razon de riesgos diferente por perfil e intervalo;",
  "- conserva FAC_HOG, estratos y UPM;",
  "- usa la clase 4 como referencia;",
  "- compara octubre con septiembre como sensibilidad.",
  "",
  "IMPORTANTE:",
  "Este sigue siendo un analisis secundario basado en la clase modal.",
  "El BCH ajustado por periodo conserva el papel de analisis principal.",
  "",
  "Archivos:",
  "- 05_tablas/auditoria_eventos_por_intervalo_y_perfil.csv",
  "- 05_tablas/HR_retorno_por_intervalo_y_perfil.csv",
  "- 05_tablas/pruebas_globales_Cox_por_intervalos.csv",
  "- 05_tablas/sensibilidad_HR_intervalos_octubre_septiembre.csv",
  "- 06_figuras/HR_retorno_por_intervalo_7_perfiles.png",
  "- 07_modelos/modelo_cox_intervalos_octubre2023.rds",
  "- 07_modelos/modelo_cox_intervalos_septiembre2023.rds"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_12E_Cox_intervalos.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")