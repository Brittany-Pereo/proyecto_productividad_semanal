library(DBI)
library(duckdb)
library(dplyr)
library(officer)
library(flextable)
library(lubridate)
library(scales)
Sys.setlocale("LC_TIME", "Spanish_Mexico")
hoy <- Sys.Date()

fecha_corte <- hoy - dplyr::if_else(
  (lubridate::wday(hoy) - 4) %% 7 == 0,
  7,
  (lubridate::wday(hoy) - 4) %% 7
)
# -------------------------------------------------------------------------
#Bases
# -------------------------------------------------------------------------
# Catalogos ---------------------------------------------------------------
catalogos_clues <- arrow::read_parquet(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/CLUES/clues.parquet"
) %>% 
  mutate(entidad = case_when(
    entidad == "MICHOACAN DE OCAMPO" ~ "Michoacan",
    entidad == "VERACRUZ DE IGNACIO DE LA LLAVE" ~ "Veracruz",
    entidad == "HRAES" ~ "HRAES",
    TRUE  ~ stringr::str_to_title(entidad)
  )) %>% 
  filter(!entidad %in% c("Yucatán", "Guanajuato"))

catalogo_metas <-  readxl::read_xlsx(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Metas/2026/Metas de productividad por unidad medica 2026.xlsx"
)%>% 
  mutate(entidad = case_when(
    entidad == "MICHOACAN DE OCAMPO" ~ "Michoacan",
    entidad == "VERACRUZ DE IGNACIO DE LA LLAVE" ~ "Veracruz",
    entidad == "HRAES" ~ "HRAES",
    TRUE  ~ stringr::str_to_title(entidad)
  )) %>% 
  filter(!entidad %in% c("Yucatán", "Guanajuato"))
# Productividad 2020 a 2023 -----------------------------------------------
con <- dbConnect(duckdb::duckdb())

DBI::dbWriteTable(
  con,
  "catalogos_clues_tmp",
  catalogos_clues %>%
    dplyr::select(clues_ssa_y_sme, clues_imb) %>%
    dplyr::mutate(
      clues_ssa_y_sme = as.character(clues_ssa_y_sme),
      clues_imb = as.character(clues_imb)),
  temporary = TRUE,
  overwrite = TRUE)

df_2020_2023 <- dbGetQuery(con, "
SELECT
  CASE
    WHEN regexp_matches(c.clues, 'IMB') THEN c.clues
    WHEN regexp_matches(c.clues, 'SSA') THEN cat.clues_imb
    ELSE cat.clues_imb
  END AS clues,

  CAST(c.fecha AS DATE) AS fecha,
  c.consultas_totales,
  c.consultas_generales,
  c.consultas_de_especialidad,
  c.procedimientos_quirurgicos,
  c.egresos

FROM read_parquet(
  'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Productividad - Cubos/Productividad de Cubos 2020-2024/Cubos_completos_2020_2024.parquet'
) AS c

LEFT JOIN catalogos_clues_tmp AS cat
  ON CAST(c.clues AS VARCHAR) = cat.clues_ssa_y_sme

WHERE c.anio != 2024
  AND CASE
        WHEN regexp_matches(c.clues, 'IMB') THEN c.clues
        WHEN regexp_matches(c.clues, 'SSA') THEN cat.clues_imb
        ELSE cat.clues_imb
      END IS NOT NULL
")
# Productividad 2024 ------------------------------------------------------
df_2024_consultas <- dbGetQuery(con, "
WITH base AS (
  SELECT
    clues,
    CAST(fecha_consulta AS DATE) AS fecha,
    CAST(CAST(fecha_insert AS VARCHAR) AS DATE) AS fecha_insert,
    LOWER(tipo_consulta) AS tipo_consulta
  FROM read_parquet(
    [
      'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/consulta_externa_01_01_2024_a_31_12_2024.parquet',
      'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/planificacion_familiar_01_01_2024_a_31_12_2024.parquet',
      'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/salud_bucal_01_01_2024_a_31_12_2024.parquet',
      'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/salud_mental_01_01_2024_a_31_12_2024.parquet'
    ],
    union_by_name = true
  )
),

resumen_diario AS (
  SELECT
    clues,
    fecha,
    fecha_insert,

    COUNT(*) AS consultas_totales,

    SUM(CASE 
          WHEN tipo_consulta IN ('general', 'generales') THEN 1
          ELSE 0
        END) AS consultas_generales,

    SUM(CASE 
          WHEN tipo_consulta IN ('especialidad', 'especialidades') THEN 1
          ELSE 0
        END) AS consultas_de_especialidad

  FROM base

  GROUP BY
    clues,
    fecha,
    fecha_insert
)

SELECT *
FROM resumen_diario
ORDER BY
  clues,
  fecha,
  fecha_insert
")

df_2024_pq <- dbGetQuery(con, "
SELECT
  clues,
  CAST(fecha_egreso AS DATE) AS fecha,
  CAST(CAST(fecha_insert AS VARCHAR) AS DATE) AS fecha_insert,

  COUNT(*) AS procedimientos_quirurgicos

FROM read_parquet(
  'C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales procedimientos/quirurgicos 2024 nuevo.parquet'
)

GROUP BY
  clues,
  CAST(fecha_egreso AS DATE),
  CAST(CAST(fecha_insert AS VARCHAR) AS DATE)

ORDER BY
  clues,
  fecha,
  fecha_insert
")


df_2024_egresos <- dbGetQuery(con, "
SELECT
  clues,
  CAST(fecha_egreso AS DATE) AS fecha,
  CAST(CAST(fecha_insert AS VARCHAR) AS DATE) AS fecha_insert,

  COUNT(*) AS egresos

FROM read_parquet(
  'C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales egresos/egresos 2024 nuevo.parquet'
)

GROUP BY
  clues,
  CAST(fecha_egreso AS DATE),
  CAST(CAST(fecha_insert AS VARCHAR) AS DATE)

ORDER BY
  clues,
  fecha,
  fecha_insert
")

df_2024 <- full_join(df_2024_consultas, df_2024_pq,
                     by = c("clues", "fecha","fecha_insert")) %>% 
  full_join(df_2024_egresos, by = c("clues", "fecha","fecha_insert"))
# Productividad 2025 ------------------------------------------------------
df_2025_consultas <- dbGetQuery(con, "
SELECT
  clues,
  CAST(fecha_consulta AS DATE) AS fecha,
  CAST(CAST(fecha_insert AS VARCHAR) AS DATE) AS fecha_insert,

  COUNT(*) AS consultas_totales,

  SUM(
    CASE
      WHEN LOWER(tipo_consulta) IN ('general', 'generales')
      THEN 1 ELSE 0
    END
  ) AS consultas_generales,

  SUM(
    CASE
      WHEN LOWER(tipo_consulta) IN ('especialidad', 'especialidades')
      THEN 1 ELSE 0
    END
  ) AS consultas_de_especialidad

FROM read_parquet(
  [
    'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/consulta_externa_01_01_2025_a_31_12_2025.parquet',
    'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/planificacion_familiar_01_01_2025_a_31_12_2025.parquet',
    'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/salud_bucal_01_01_2025_a_31_12_2025.parquet',
    'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/salud_mental_01_01_2025_a_31_12_2025.parquet'
  ],
  union_by_name = true
)

GROUP BY
  clues,
  CAST(fecha_consulta AS DATE),
  CAST(CAST(fecha_insert AS VARCHAR) AS DATE)

ORDER BY
  clues,
  fecha,
  fecha_insert
")

df_2025_pq <- dbGetQuery(con, "
SELECT
  clues,
  CAST(fecha_egreso AS DATE) AS fecha,
  CAST(CAST(fecha_insert AS VARCHAR) AS DATE) AS fecha_insert,

  COUNT(*) AS procedimientos_quirurgicos

FROM read_parquet(
  'C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales procedimientos/quirurgicos 2025 nuevo.parquet'
)

GROUP BY
  clues,
  CAST(fecha_egreso AS DATE),
  CAST(CAST(fecha_insert AS VARCHAR) AS DATE)

ORDER BY
  clues,
  fecha,
  fecha_insert
")

df_2025_egresos <- dbGetQuery(con, "
SELECT
  clues,
  CAST(fecha_egreso AS DATE) AS fecha,
  CAST(CAST(fecha_insert AS VARCHAR) AS DATE) AS fecha_insert,

  COUNT(*) AS egresos

FROM read_parquet(
  'C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales egresos/egresos 2025 nuevo.parquet'
)

GROUP BY
  clues,
  CAST(fecha_egreso AS DATE),
  CAST(CAST(fecha_insert AS VARCHAR) AS DATE)

ORDER BY
  clues,
  fecha,
  fecha_insert
")

df_2025 <- full_join(df_2025_consultas, df_2025_pq,
                     by = c("clues", "fecha", "fecha_insert")) %>% 
  full_join(df_2025_egresos, by = c("clues", "fecha", "fecha_insert"))
# Productividad 2026 ------------------------------------------------------
fecha_corte <- as.Date(fecha_corte)
df_2026_consultas <- dbGetQuery(con, glue::glue("
  SELECT
    clues,
    CAST(fecha_consulta AS DATE) AS fecha,
    CAST(fecha_insert AS DATE) AS fecha_insert,

    COUNT(*) AS consultas_totales,

    SUM(CASE 
          WHEN LOWER(tipo_consulta) = 'general' 
          THEN 1 ELSE 0 
        END) AS consultas_generales,

    SUM(CASE 
          WHEN LOWER(tipo_consulta) = 'especialidad' 
          THEN 1 ELSE 0 
        END) AS consultas_de_especialidad

  FROM read_parquet(
    'C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/78_transicion sistemas prod/data/consultas_con_ECE_2026.parquet'
  )

  WHERE CAST(fecha_insert AS DATE) <= DATE '{fecha_corte}'

  GROUP BY
    clues,
    CAST(fecha_consulta AS DATE),
    CAST(fecha_insert AS DATE)

  ORDER BY
    clues,
    fecha,
    fecha_insert
"))

df_2026_pq <- dbGetQuery(con, glue::glue("
  SELECT
    clues,
    CAST(fecha_egreso AS DATE) AS fecha,
    CAST(fecha_insert AS DATE) AS fecha_insert,
    COUNT(*) AS procedimientos_quirurgicos

  FROM read_parquet(
    'C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/78_transicion sistemas prod/data/proc_qx_con_ECE_2026.parquet'
  )

  WHERE CAST(fecha_insert AS DATE) <= DATE '{fecha_corte}'

  GROUP BY
    clues,
    CAST(fecha_egreso AS DATE),
    CAST(fecha_insert AS DATE)

  ORDER BY
    clues,
    fecha,
    fecha_insert
"))

df_2026_egresos <- dbGetQuery(con, glue::glue("
  SELECT
    clues,
    CAST(fecha_egreso AS DATE) AS fecha,
    CAST(CAST(fecha_insert AS VARCHAR) AS DATE) AS fecha_insert,
    COUNT(*) AS egresos

  FROM read_parquet(
    'C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/78_transicion sistemas prod/data/egresos_con_ECE_2026.parquet'
  )

  WHERE CAST(CAST(fecha_insert AS VARCHAR) AS DATE) <= DATE '{fecha_corte}'

  GROUP BY
    clues,
    CAST(fecha_egreso AS DATE),
    CAST(CAST(fecha_insert AS VARCHAR) AS DATE)

  ORDER BY
    clues,
    fecha,
    fecha_insert
"))

df_2026 <- full_join(df_2026_consultas, df_2026_pq,
                     by = c("clues", "fecha", "fecha_insert")) %>% 
  full_join(df_2026_egresos, by = c("clues", "fecha", "fecha_insert"))
# BASES JUNTAS ------------------------------------------------------------
df_final <- bind_rows(df_2020_2023, df_2024, df_2025, df_2026
) %>% 
  mutate(across(
    where(is.numeric),~ tidyr::replace_na(.x, 0))) %>% 
  filter(!is.na(fecha))

df_final_curp <- dbGetQuery(con, "
SELECT
  anio_insert,
  id,

  SUM(CASE WHEN tipo_procedimiento = 'consulta total' THEN procedimientos ELSE 0 END) AS procedimientos_consulta_total,
  SUM(CASE WHEN tipo_procedimiento = 'general' THEN procedimientos ELSE 0 END) AS procedimientos_general,
  SUM(CASE WHEN tipo_procedimiento = 'especialidad' THEN procedimientos ELSE 0 END) AS procedimientos_especialidad,
  SUM(CASE WHEN tipo_procedimiento = 'qx' THEN procedimientos ELSE 0 END) AS procedimientos_qx,
  SUM(CASE WHEN tipo_procedimiento = 'egresos' THEN procedimientos ELSE 0 END) AS procedimientos_egresos,

  SUM(CASE WHEN tipo_procedimiento = 'consulta total' THEN personas ELSE 0 END) AS personas_consulta_total,
  SUM(CASE WHEN tipo_procedimiento = 'general' THEN personas ELSE 0 END) AS personas_general,
  SUM(CASE WHEN tipo_procedimiento = 'especialidad' THEN personas ELSE 0 END) AS personas_especialidad,
  SUM(CASE WHEN tipo_procedimiento = 'qx' THEN personas ELSE 0 END) AS personas_qx,
  SUM(CASE WHEN tipo_procedimiento = 'egresos' THEN personas ELSE 0 END) AS personas_egresos

FROM read_parquet(
  'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/conteos con ece/fecha_insert_al_corte_todos.parquet'
)

WHERE anio_insert IS NOT NULL
  AND id = 'NACIONAL'

GROUP BY
  anio_insert,
  id

ORDER BY
  anio_insert
")
dbDisconnect(con, shutdown = TRUE)
# Estimaciones de modelo profet -------------------------------------------
modelo_profet <- readxl::read_xlsx(
  "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/66_Productividad Nacional 2026/Data/profet/nowcast_todes.xlsx"
) %>% 
  transmute(fecha = as.Date(dia), tipo_consulta,
            nowcast, observadas) %>% 
  filter(fecha >= "2026-01-01") %>% 
  tidyr::pivot_wider(names_from = tipo_consulta,
                     values_from = nowcast,
                     values_fill = 0) %>% 
  group_by(fecha) %>% 
  summarise(consultas_totales = sum(general, especialidad),
            consultas_generales = sum(general),
            consultas_de_especialidad = sum(especialidad),
            procedimientos_quirurgicos = sum(qx),
            egresos = sum(egresos))

modelo_profet_entidad <- readxl::read_xlsx(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/conteos con ece/nowcast_todes_estados.xlsx",
  sheet = "Sheet 1"
) %>% 
  transmute(fecha = as.Date(dia), tipo_consulta,
            observadas, nowcast, entidad) %>% 
  filter(fecha >= "2026-01-01",
         fecha <= fecha_corte) %>% 
  tidyr::pivot_wider(names_from = tipo_consulta,
                     values_from = c(observadas, nowcast),
                     names_glue = "{tipo_consulta}_{.value}",
                     values_fill = 0) %>% 
  group_by(entidad) %>% 
  summarise(
    consultas_totales_observadas = sum(general_observadas, especialidad_observadas),
    consultas_generales_observadas = sum(general_observadas),
    consultas_de_especialidad_observadas = sum(especialidad_observadas),
    procedimientos_quirurgicos_observadas = sum(qx_observadas),
    egresos_observadas = sum(egresos_observadas),
    
    consultas_totales_nowcast = sum(general_nowcast, especialidad_nowcast),
    consultas_generales_nowcast = sum(general_nowcast),
    consultas_de_especialidad_nowcast = sum(especialidad_nowcast),
    procedimientos_quirurgicos_nowcast = sum(qx_nowcast),
    egresos_nowcast = sum(egresos_nowcast)) %>% 
  mutate(
    entidad = case_when(
      entidad == "MICHOACAN DE OCAMPO" ~ "Michoacan",
      entidad == "VERACRUZ DE IGNACIO DE LA LLAVE" ~ "Veracruz",
      entidad == "HRAES" ~ "HRAES",
      TRUE ~ stringr::str_to_title(entidad)),
    consultas_totales_diferencias = consultas_de_especialidad_nowcast- consultas_totales_observadas,
    consultas_generales_diferencias = consultas_generales_nowcast - consultas_generales_observadas,
    consultas_de_especialidad_diferencias = consultas_de_especialidad_nowcast - consultas_de_especialidad_observadas,
    procedimientos_quirurgicos_diferencias = procedimientos_quirurgicos_nowcast - procedimientos_quirurgicos_observadas,
    egresos_diferencias = egresos_nowcast -egresos_observadas
  ) %>% 
  filter(!entidad %in% c(
    "Iniems", "Guanajuato",
    "Yucatán", "Yucatan"))

modelo_profet_completo_nowcast <- readxl::read_xlsx(
  "C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/conteos con ece/nowcast_todes_estados.xlsx",
  sheet = "Sheet 1"
) %>% 
  transmute(fecha = as.Date(dia), tipo_consulta,
            observadas, nowcast, entidad) %>% 
  filter(fecha >= "2026-01-01") %>% 
  tidyr::pivot_wider(names_from = tipo_consulta,
                     values_from = nowcast,
                     values_fill = 0) %>% 
  group_by(fecha) %>% 
  summarise(consultas_totales = sum(general, especialidad),
            consultas_generales = sum(general),
            consultas_de_especialidad = sum(especialidad),
            procedimientos_quirurgicos = sum(qx),
            egresos = sum(egresos)) 
# -------------------------------------------------------------------------
#Objetos y vectores
# -------------------------------------------------------------------------
# Parámetros generales del reporte ----------------------------------------
fecha_corte_archivo <- format(fecha_corte, "%d_%m_%Y")
#fecha_corte_archivo <- "01_04_2026"
mes_actual <- lubridate::month(Sys.Date())
# fechas equivalentes años anteriores
fecha_2025 <- fecha_corte %m-% years(1)
fecha_2024 <- fecha_corte %m-% years(2)
fecha_2023 <- fecha_corte %m-% years(3)

# número de semana del año
num_semana <- (lubridate::isoweek(fecha_corte)-1)
inicio_semana <- fecha_corte - 6
fecha_portada <- if (format(inicio_semana, "%m") == format(fecha_corte, "%m")) {
  paste0(
    "Semana del ",
    format(inicio_semana, "%d"),
    " al ",
    format(fecha_corte, "%d de %B"),
    " (semana ", num_semana, ")")} else {
      paste0(
        "Semana del ",
        format(inicio_semana, "%d de %B"),
        " al ",
        format(fecha_corte, "%d de %B"),
        " (semana ", num_semana, ")")}

fecha_valuebox <- paste0(
  "Al corte de ",
  format(fecha_corte, "%d de %B %Y"))

fecha_ini <- list(
  `2024` = as.Date("2024-01-01"),
  `2025` = as.Date("2025-01-01"),
  `2026` = as.Date("2026-01-01"))

fechas_fin <- list(
  `2024` = as.Date(fecha_2024),
  `2025` = as.Date(fecha_2025),
  `2026` = as.Date(fecha_corte))

dias_desde_miercoles <- (lubridate::wday(hoy) - 4) %% 7
miercoles_mas_reciente <- hoy - dias_desde_miercoles

fecha_txt <- paste0(
  "Al ",
  stringr::str_to_sentence(format(miercoles_mas_reciente, "%d de %B %Y")))

fecha_block <- block_list(
  fpar(
    ftext(
      fecha_txt,
      fp_text(font.size = 18, bold = TRUE, color = "#111827")
    ),
    fp_p = fp_par(
      text.align = "left",
      padding.left = 0,
      padding.right = 0,
      line_spacing = 1)))

hbc_bajos <- readxl::read_xlsx(
  "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/66_Productividad Nacional 2026/Data raw/cluster_11_rutas_geo_rutas_sencillo_VF23022026.xlsx"
) %>%
  dplyr::pull(clues_imb_hbc)
# Metas y totales de años previos --------------------------------------------------------------
meta_hoy <- num_semana / 52

productividad_ct_2024 <- sum(df_2024$consultas_totales, na.rm = TRUE)
productividad_cg_2024 <- sum(df_2024$consultas_generales, na.rm = TRUE)
productividad_ce_2024 <- sum(df_2024$consultas_de_especialidad, na.rm = TRUE)
productividad_pq_2024 <- sum(df_2024$procedimientos_quirurgicos, na.rm = TRUE)
productividad_egreso_2024 <- sum(df_2024$egresos, na.rm = TRUE)

productividad_ct_2025 <- sum(df_2025$consultas_totales, na.rm = TRUE)
productividad_cg_2025 <- sum(df_2025$consultas_generales, na.rm = TRUE)
productividad_ce_2025 <- sum(df_2025$consultas_de_especialidad, na.rm = TRUE)
productividad_pq_2025 <- sum(df_2025$procedimientos_quirurgicos, na.rm = TRUE)
productividad_egreso_2025 <- sum(df_2025$egresos, na.rm = TRUE)

productividad_ct_2026 <- 60000000
productividad_cg_2026 <- 52500000
productividad_ce_2026 <- 7500000
productividad_pq_2026 <- 1100000
productividad_egreso_2026 <- 1600000

meta_semanal_totales <- productividad_ct_2026/ 52
meta_semanal_general <- productividad_cg_2026/ 52
meta_semanal_especialidad <-productividad_ce_2026/ 52
meta_semanal_procedimientos_quirurgicos <- productividad_pq_2026/ 52
meta_semanal_egreso <- productividad_egreso_2026/ 52
# Colores y meses -----------------------------------------------------------------
col_verde   <- "#1E5B4F"   # IMSS verde
col_guinda  <- "#611232"   # guinda
col_dorado  <- "#A57F2C"   # dorado texto número
col_texto   <- "#111827"   # gris casi negro
col_muted   <- "#6B7280"   # gris texto secundario
col_borde   <- "#D1D5DB"   # borde suave
col_blanco  <- "#FFFFFF"
col_bg_ok   <- "#DFF3E8"  # verde pastel
col_bg_bad  <- "#FBE4E6"  # rojo/rosa pastel
col_bar     <- "#374151"  # barra lateral (tu gris)
col_number  <- "#B08D2A"  # dorado número
col_muted   <- "#6B7280"  # textos pequeños
col_up      <- "#16A34A"  # % positivo
col_down    <- "#DC2626"  # % negativo
col_verde_pastel   <- "#FFFFFF"  # AJUSTE DE FONDO
col_amarillo_chillon <- "#FEF3C7"
col_amarillo_chillon <- "#FFFFFF" # amarillo suave (tipo warning)  # AJUSTE DE FONDO
col_rojo_chillon   <- "#FFFFFF"  # rojo “chillón” (tailwind red-500) # AJUSTE DE FONDO
col_borde_suave  <- "#CBD5E1"
col_barra_gris   <- "#374151"
col_dorado       <- "#a57f2c"

estilo_valuebox <- list(
  transparencia= 90,
  ancho_borde= 12700,
  tamano_titulo= 16,
  tamano_valor= 28,
  tamano_subtitulo = 14,
  color_titulo = "#a57f2c",
  color_valor = "#a57f2c",
  color_subtitulo = "#a57f2c",
  negrita_titulo = TRUE,
  negrita_valor = TRUE,
  negrita_subtitulo = FALSE,
  italica_titulo = FALSE,
  italica_valor = FALSE,
  italica_subtitulo = TRUE)

estilo_verde  <- c(list(color_fondo = "#15803D", color_borde = "#15803D"), estilo_valuebox)
estilo_guinda <- c(list(color_fondo = "#B91C1C", color_borde = "#B91C1C"), estilo_valuebox)

nombres_meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
                   "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

# -------------------------------------------------------------------------
# GRAFICAS 
# -------------------------------------------------------------------------
# Valiu box ---------------------------------------------------------------
datos_vb_reales <- df_final %>% 
  mutate(
    anio = lubridate::year(fecha),
    fecha_corte_anio = as.Date(
      paste0(anio, "-", format(fecha_corte, "%m-%d"))
    )
  ) %>% 
  filter(fecha <= fecha_corte_anio) %>% 
  group_by(anio) %>% 
  summarise(
    consultas_totales = sum(consultas_totales, na.rm = TRUE),
    consultas_generales = sum(consultas_generales, na.rm = TRUE),
    consultas_de_especialidad = sum(consultas_de_especialidad, na.rm = TRUE),
    procedimientos_quirurgicos = sum(procedimientos_quirurgicos, na.rm = TRUE),
    egresos = sum(egresos, na.rm = TRUE),
    .groups = "drop"
  )

datos_vb_reales_curp <- df_final_curp %>% 
  group_by(anio = anio_insert) %>% 
  summarise(
    total_curps_distintas = sum(personas_consulta_total, na.rm = TRUE),
    curps_distintas_generales = sum(personas_general, na.rm = TRUE),
    curps_distintas_especialidad = sum(personas_especialidad, na.rm = TRUE),
    total_curps_distintas_pq = sum(personas_qx, na.rm = TRUE),
    total_curps_distintas_egresos = sum(personas_egresos, na.rm = TRUE),
    .groups = "drop"
  )

modelo_2026 <- modelo_profet %>% 
  summarise(
    consultas_totales = sum(consultas_totales, na.rm = TRUE),
    consultas_generales = sum(consultas_generales, na.rm = TRUE),
    consultas_de_especialidad = sum(consultas_de_especialidad, na.rm = TRUE),
    procedimientos_quirurgicos = sum(procedimientos_quirurgicos, na.rm = TRUE),
    egresos = sum(egresos, na.rm = TRUE)
  ) %>% 
  mutate(anio = 2026)

curps <- datos_vb_reales_curp %>%
  mutate(anio = as.character(anio)) %>%
  select(
    anio,
    total_curps_distintas,
    curps_distintas_generales,
    curps_distintas_especialidad,
    total_curps_distintas_pq,
    total_curps_distintas_egresos
  )

datos_vb <- bind_rows(
  datos_vb_reales %>%
    filter(anio != 2026),
  
  modelo_2026
) %>%
  mutate(anio = as.character(anio)) %>%
  left_join(curps, by = "anio") %>%
  arrange(anio)

tasa_crecimiento <- function(valor_2026, valor_ref) {
  round(((valor_2026 - valor_ref) / valor_2026) * 100, 0)
}

datos_variacion <- datos_vb %>% 
  summarise(
    # Productividad normal
    var_2026_vs_2025_total_consultas =
      tasa_crecimiento(consultas_totales[anio == 2026], consultas_totales[anio == 2025]),
    var_2026_vs_2024_total_consultas =
      tasa_crecimiento(consultas_totales[anio == 2026], consultas_totales[anio == 2024]),
    
    var_2026_vs_2025_consultas_generales =
      tasa_crecimiento(consultas_generales[anio == 2026], consultas_generales[anio == 2025]),
    var_2026_vs_2024_consultas_generales =
      tasa_crecimiento(consultas_generales[anio == 2026], consultas_generales[anio == 2024]),
    
    var_2026_vs_2025_consultas_especialidad =
      tasa_crecimiento(consultas_de_especialidad[anio == 2026], consultas_de_especialidad[anio == 2025]),
    var_2026_vs_2024_consultas_especialidad =
      tasa_crecimiento(consultas_de_especialidad[anio == 2026], consultas_de_especialidad[anio == 2024]),
    
    var_2026_vs_2025_pq =
      tasa_crecimiento(procedimientos_quirurgicos[anio == 2026], procedimientos_quirurgicos[anio == 2025]),
    var_2026_vs_2024_pq =
      tasa_crecimiento(procedimientos_quirurgicos[anio == 2026], procedimientos_quirurgicos[anio == 2024]),
    
    var_2026_vs_2025_egresos =
      tasa_crecimiento(egresos[anio == 2026], egresos[anio == 2025]),
    var_2026_vs_2024_egresos =
      tasa_crecimiento(egresos[anio == 2026], egresos[anio == 2024]),
    
    # CURP distintas
    var_2026_vs_2025_curps_totales =
      tasa_crecimiento(total_curps_distintas[anio == 2026], total_curps_distintas[anio == 2025]),
    var_2026_vs_2024_curps_totales =
      tasa_crecimiento(total_curps_distintas[anio == 2026], total_curps_distintas[anio == 2024]),
    
    var_2026_vs_2025_curps_generales =
      tasa_crecimiento(curps_distintas_generales[anio == 2026], curps_distintas_generales[anio == 2025]),
    var_2026_vs_2024_curps_generales =
      tasa_crecimiento(curps_distintas_generales[anio == 2026], curps_distintas_generales[anio == 2024]),
    
    var_2026_vs_2025_curps_especialidad =
      tasa_crecimiento(curps_distintas_especialidad[anio == 2026], curps_distintas_especialidad[anio == 2025]),
    var_2026_vs_2024_curps_especialidad =
      tasa_crecimiento(curps_distintas_especialidad[anio == 2026], curps_distintas_especialidad[anio == 2024]),
    
    var_2026_vs_2025_curps_pq =
      tasa_crecimiento(total_curps_distintas_pq[anio == 2026], total_curps_distintas_pq[anio == 2025]),
    var_2026_vs_2024_curps_pq =
      tasa_crecimiento(total_curps_distintas_pq[anio == 2026], total_curps_distintas_pq[anio == 2024]),
    
    var_2026_vs_2025_curps_egresos =
      tasa_crecimiento(total_curps_distintas_egresos[anio == 2026], total_curps_distintas_egresos[anio == 2025]),
    var_2026_vs_2024_curps_egresos =
      tasa_crecimiento(total_curps_distintas_egresos[anio == 2026], total_curps_distintas_egresos[anio == 2024])
  )

valuebox_total <- crear_card_institucional(
  numero = datos_vb %>% 
    filter(anio == 2026) %>% 
    pull(consultas_totales),
  titulo = "Consultas totales",
  var_vs_2025 = datos_variacion$var_2026_vs_2025_total_consultas,
  var_vs_2024 = datos_variacion$var_2026_vs_2024_total_consultas,
  acento = elige_acento(
    datos_variacion$var_2026_vs_2025_total_consultas,
    datos_variacion$var_2026_vs_2024_total_consultas))

valuebox_general <- crear_card_institucional(
  numero = datos_vb %>% 
    filter(anio == 2026) %>% 
    pull(consultas_generales),
  titulo = "Consulta general",
  var_vs_2025 = datos_variacion$var_2026_vs_2025_consultas_generales,
  var_vs_2024 = datos_variacion$var_2026_vs_2024_consultas_generales,
  acento = elige_acento(
    datos_variacion$var_2026_vs_2025_consultas_generales,
    datos_variacion$var_2026_vs_2024_consultas_generales))

valuebox_especialidad <- crear_card_institucional(
  numero = datos_vb %>% 
    filter(anio == 2026) %>% 
    pull(consultas_de_especialidad),
  titulo = "Especialidad",
  var_vs_2025 = datos_variacion$var_2026_vs_2025_consultas_especialidad,
  var_vs_2024 = datos_variacion$var_2026_vs_2024_consultas_especialidad,
  acento = elige_acento(
    datos_variacion$var_2026_vs_2025_consultas_especialidad,
    datos_variacion$var_2026_vs_2024_consultas_especialidad))

valuebox_pq <- crear_card_institucional(
  numero = datos_vb %>% 
    filter(anio == 2026) %>% 
    pull(procedimientos_quirurgicos),
  titulo = "Procedimientos quirúrgicos",
  var_vs_2025 = datos_variacion$var_2026_vs_2025_pq,
  var_vs_2024 = datos_variacion$var_2026_vs_2024_pq,
  acento = elige_acento(
    datos_variacion$var_2026_vs_2025_pq,
    datos_variacion$var_2026_vs_2024_pq))

valuebox_egresos <- crear_card_institucional(
  numero = datos_vb %>% 
    filter(anio == 2026) %>% 
    pull(egresos),
  titulo = "Egresos",
  var_vs_2025 = datos_variacion$var_2026_vs_2025_egresos,
  var_vs_2024 = datos_variacion$var_2026_vs_2024_egresos,
  acento = elige_acento(
    datos_variacion$var_2026_vs_2025_egresos,
    datos_variacion$var_2026_vs_2024_egresos))

valuebox_curps_total <- crear_card_institucional(
  numero = datos_vb %>% filter(anio == 2026) %>% pull(total_curps_distintas),
  titulo = "Consultas totales",
  var_vs_2025 = datos_variacion$var_2026_vs_2025_curps_totales,
  var_vs_2024 = datos_variacion$var_2026_vs_2024_curps_totales,
  acento = elige_acento(
    datos_variacion$var_2026_vs_2025_curps_totales,
    datos_variacion$var_2026_vs_2024_curps_totales))

valuebox_curps_general <- crear_card_institucional(
  numero = datos_vb %>% filter(anio == 2026) %>% pull(curps_distintas_generales),
  titulo = "Consultas generales",
  var_vs_2025 = datos_variacion$var_2026_vs_2025_curps_generales,
  var_vs_2024 = datos_variacion$var_2026_vs_2024_curps_generales,
  acento = elige_acento(
    datos_variacion$var_2026_vs_2025_curps_generales,
    datos_variacion$var_2026_vs_2024_curps_generales))

valuebox_curps_especialidad <- crear_card_institucional(
  numero = datos_vb %>% filter(anio == 2026) %>% pull(curps_distintas_especialidad),
  titulo = "Consultas especialidad",
  var_vs_2025 = datos_variacion$var_2026_vs_2025_curps_especialidad,
  var_vs_2024 = datos_variacion$var_2026_vs_2024_curps_especialidad,
  acento = elige_acento(
    datos_variacion$var_2026_vs_2025_curps_especialidad,
    datos_variacion$var_2026_vs_2024_curps_especialidad))

valuebox_curps_pq <- crear_card_institucional(
  numero = datos_vb %>% filter(anio == 2026) %>% pull(total_curps_distintas_pq),
  titulo = "Intervenidas",
  var_vs_2025 = datos_variacion$var_2026_vs_2025_curps_pq,
  var_vs_2024 = datos_variacion$var_2026_vs_2024_curps_pq,
  acento = elige_acento(
    datos_variacion$var_2026_vs_2025_curps_pq,
    datos_variacion$var_2026_vs_2024_curps_pq))

valuebox_curps_egresos <- crear_card_institucional(
  numero = datos_vb %>% filter(anio == 2026) %>% pull(total_curps_distintas_egresos),
  titulo = "Egresadas",
  var_vs_2025 = datos_variacion$var_2026_vs_2025_curps_egresos,
  var_vs_2024 = datos_variacion$var_2026_vs_2024_curps_egresos,
  acento = elige_acento(
    datos_variacion$var_2026_vs_2025_curps_egresos,
    datos_variacion$var_2026_vs_2024_curps_egresos))

p_2024_g  <- obtener_total_tabla(datos_vb, consultas_generales, 2024)
p_2025_g  <- obtener_total_tabla(datos_vb, consultas_generales, 2025)
p_2026_gm <- obtener_total_tabla(datos_vb, consultas_generales, 2026)

p_2024_e  <- obtener_total_tabla(datos_vb, consultas_de_especialidad, 2024)
p_2025_e  <- obtener_total_tabla(datos_vb, consultas_de_especialidad, 2025)
p_2026_em <- obtener_total_tabla(datos_vb, consultas_de_especialidad, 2026)

avance_2024_t <- obtener_total_tabla(datos_vb, consultas_totales, 2024)
avance_2025_t <- obtener_total_tabla(datos_vb, consultas_totales, 2025)
avance_2026_t <- obtener_total_tabla(datos_vb, consultas_totales, 2026)

p_2024_pq  <- obtener_total_tabla(datos_vb, procedimientos_quirurgicos, 2024)
p_2025_pq  <- obtener_total_tabla(datos_vb, procedimientos_quirurgicos, 2025)
p_2026_pqm <- obtener_total_tabla(datos_vb, procedimientos_quirurgicos, 2026)

egreso_2024 <- obtener_total_tabla(datos_vb, egresos, 2024)
egreso_2025 <- obtener_total_tabla(datos_vb, egresos, 2025)
egreso_2026 <- obtener_total_tabla(datos_vb, egresos, 2026)
# Graficas de avance por entidad ------------------------------------------
# Meta al corte
metas_entidad <- catalogo_metas %>% 
  mutate(clues_imb = as.character(clues_imb)) %>% 
  group_by(entidad) %>% 
  summarise(
    meta_total = sum(meta_general_anual, meta_especialidad_anual, na.rm = TRUE),
    meta_cg = sum(meta_general_anual, na.rm = TRUE),
    meta_ce = sum(meta_especialidad_anual, na.rm = TRUE),
    meta_pq = sum(meta_cirugia_anual, na.rm = TRUE),
    meta_egresos = sum(meta_egresos_anual, na.rm = TRUE),
    .groups = "drop")
#Avance
avance_entidad <- left_join(modelo_profet_entidad,
                            metas_entidad, by = "entidad")

limpiar_data_avance <- function(df, col_avance, col_modelo, col_meta) {
  df %>% 
    transmute(
      entidad,
      avance_total = {{ col_avance }},
      avance_modelo = {{ col_modelo }},
      meta_total = {{ col_meta }},
      pct_avance_entidad = if_else(
        !is.na(meta_total) & meta_total > 0,
        avance_total / meta_total,
        NA_real_),
      pct_modelo_entidad = if_else(
        !is.na(meta_total) & meta_total > 0,
        avance_modelo / meta_total,
        NA_real_)) %>% 
    filter(
      !is.na(entidad),
      !is.na(pct_avance_entidad),
      is.finite(pct_avance_entidad))}

xmax_seguro <- function(df, col = pct_modelo_entidad, suma = 0.05) {
  x <- max(df %>% pull({{ col }}), na.rm = TRUE)
  if (!is.finite(x)) meta_hoy + 0.10 else round(x + suma, 1)
}

data_cg <- limpiar_data_avance(avance_entidad, consultas_generales_observadas, consultas_generales_nowcast, meta_cg)

data_esp <- limpiar_data_avance(avance_entidad, consultas_de_especialidad_observadas, consultas_de_especialidad_nowcast, meta_ce)

data_pq_entidad <- limpiar_data_avance(avance_entidad, procedimientos_quirurgicos_observadas,procedimientos_quirurgicos_nowcast, meta_pq)

data_egresos <- limpiar_data_avance(avance_entidad, egresos_observadas, egresos_nowcast, meta_egresos)

grafica_avance_cgen <- grafica_avance_entidades(
  data_cg,
  meta_linea = meta_hoy,
  x_max = xmax_seguro(data_cg, suma = 0.06),
  breaks_by = 0.05,
  extra_derecha = 0.05,
  size_pct = 4,
  size_meta = 3,
  size_ejes = 10,
  size_meta_txt = 4)

grafica_avance_cesp <- grafica_avance_entidades(
  data_esp,
  meta_linea = meta_hoy,
  x_max = xmax_seguro(data_esp, suma = 0.05),
  breaks_by = 0.05,
  size_pct = 4,
  size_meta = 3,
  size_ejes = 10,
  size_meta_txt = 4,
  extra_derecha = 0.05)

grafica_avance_pq <- grafica_avance_entidades(
  data_pq_entidad,
  meta_linea = meta_hoy,
  x_max = xmax_seguro(data_pq_entidad, suma = 0.0638),
  breaks_by = 0.05,
  size_pct = 4,
  size_meta = 3,
  size_ejes = 10,
  size_meta_txt = 4,
  extra_derecha = 0.05)

grafica_avance_egresos <- grafica_avance_entidades(
  data_egresos,
  meta_linea = meta_hoy,
  x_max = xmax_seguro(data_egresos, suma = 0.05),
  breaks_by = 0.05,
  size_pct = 4,
  size_meta = 3,
  size_ejes = 10,
  size_meta_txt = 4,
  extra_derecha = 0.05)

# Valiu box semanal -------------------------------------------------------
valuebox_0 <- do.call(
  crear_valuebox_forma_noto14_small,
  c(list(
    icono = "",
    valor = mk_subtitulo_texto_2lineas(
      datos_variacion$var_2026_vs_2025_total_consultas,
      datos_variacion$var_2026_vs_2024_total_consultas
    )
  ), estilo_verde)
)

valuebox_1 <- do.call(
  crear_valuebox_forma_noto14_small,
  c(list(
    icono = "",
    valor = mk_subtitulo_texto_2lineas(
      datos_variacion$var_2026_vs_2025_consultas_generales,
      datos_variacion$var_2026_vs_2024_consultas_generales
    )
  ), estilo_verde)
)

valuebox_2 <- do.call(
  crear_valuebox_forma_noto14_small,
  c(list(
    icono = "",
    valor = mk_subtitulo_texto_2lineas(
      datos_variacion$var_2026_vs_2025_consultas_especialidad,
      datos_variacion$var_2026_vs_2024_consultas_especialidad
    )
  ), estilo_verde)
)

valuebox_3 <- do.call(
  crear_valuebox_forma_noto14_small,
  c(list(
    icono = "",
    valor = mk_subtitulo_texto_2lineas(
      datos_variacion$var_2026_vs_2025_pq,
      datos_variacion$var_2026_vs_2024_pq
    )
  ), estilo_verde)
)

valuebox_egreso <- do.call(
  crear_valuebox_forma_noto14_small,
  c(list(
    icono = "",
    valor = mk_subtitulo_texto_2lineas(
      datos_variacion$var_2026_vs_2025_egresos,
      datos_variacion$var_2026_vs_2024_egresos
    )
  ), estilo_verde)
)

# Graficas semanales ------------------------------------------------------
bases_semanales_reales <- df_final %>% 
  mutate(anio = lubridate::year(fecha),
         semana = lubridate::isoweek(fecha)) %>% 
  filter(anio %in% c(2024, 2025, 2026)) %>% 
  group_by(anio, semana) %>% 
  summarise(
    Totales = sum(consultas_totales, na.rm = TRUE),
    Generales = sum(consultas_generales, na.rm = TRUE),
    Especialidad = sum(consultas_de_especialidad, na.rm = TRUE),
    `Procedimientos quirúrgicos` = sum(procedimientos_quirurgicos, na.rm = TRUE),
    Egresos = sum(egresos, na.rm = TRUE),
    .groups = "drop")

bases_semanales_modelo <- modelo_profet %>% 
  mutate(anio = lubridate::year(fecha),
         semana = lubridate::isoweek(fecha)) %>% 
  group_by(anio, semana) %>% 
  summarise(
    Totales = sum(consultas_totales),
    Generales = sum(consultas_generales),
    Especialidad = sum(consultas_de_especialidad),
    `Procedimientos quirúrgicos` = sum(procedimientos_quirurgicos),
    Egresos = sum(egresos)
  )

bases_semanales <- bases_semanales_reales %>% 
  # quitar 2026 real
  filter(anio != 2026) %>% 
  # agregar 2026 modelado
  bind_rows(bases_semanales_modelo) %>% 
  arrange(anio, semana)

bases_todas_totales <- bases_semanales %>% 
  transmute(anio, semana, procedimiento = "Totales",
            total = Totales)

grafica_semanal_tot <- grafica_semanal_procedimiento(
  bases_todas_totales %>% filter(semana <= num_semana),
  procedimiento_sel = "Totales",
  meta_semanal = meta_semanal_totales,
  guardar_svg = FALSE)

bases_todas_cg <- bases_semanales %>% 
  transmute(anio, semana, procedimiento = "Generales",
            total = Generales)

grafica_semanal_cg <- grafica_semanal_procedimiento(
  bases_todas_cg %>% filter(semana <= num_semana),
  procedimiento_sel = "Generales",
  meta_semanal = meta_semanal_general,
  guardar_svg = FALSE)

bases_todas_esp <- bases_semanales %>% 
  transmute( anio, semana, procedimiento = "Especialidad",
             total = Especialidad)

grafica_semanal_esp <- grafica_semanal_procedimiento(
  bases_todas_esp %>% filter(semana <= num_semana),
  procedimiento_sel = "Especialidad",
  meta_semanal = meta_semanal_especialidad,
  guardar_svg = FALSE)

bases_todas_pq <- bases_semanales %>% 
  transmute(anio, semana, procedimiento = "Procedimientos quirúrgicos",
            total = `Procedimientos quirúrgicos`)

grafica_semanal_pq <- grafica_semanal_procedimiento(
  bases_todas_pq %>% filter(semana <= num_semana),
  procedimiento_sel = "Procedimientos quirúrgicos",
  meta_semanal = meta_semanal_procedimientos_quirurgicos,
  guardar_svg = FALSE)

bases_todas_egresos <- bases_semanales %>% 
  transmute(anio, semana, procedimiento = "Egresos",
            total = Egresos)

grafica_semanal_egresos <- grafica_semanal_procedimiento(
  bases_todas_egresos %>% filter(semana <= num_semana),
  procedimiento_sel = "Egresos",
  meta_semanal = meta_semanal_egreso,
  guardar_svg =FALSE)
# Graficas acumuladas -----------------------------------------------------
mes_actual <- lubridate::month(fecha_corte)
mes_actual_nombre <- nombres_meses[mes_actual]

mes_grafica <- lubridate::month(fecha_corte)
mes_grafica_nombre <- nombres_meses[mes_grafica]

fecha_ini_mes <- as.Date(sprintf("2026-%02d-01", mes_grafica))
fecha_fin_mes <- fecha_corte

df_final_resumen <- df_final %>% 
  mutate(
    anio = lubridate::year(fecha),
    fecha_insert = if_else(
      is.na(fecha_insert),
      fecha,
      fecha_insert)) %>% 
  group_by(anio, fecha, fecha_insert) %>% 
  summarise(
    consultas_totales = sum(consultas_totales, na.rm = TRUE),
    consultas_generales = sum(consultas_generales, na.rm = TRUE),
    consultas_de_especialidad = sum(consultas_de_especialidad, na.rm = TRUE),
    procedimientos_quirurgicos = sum(procedimientos_quirurgicos, na.rm = TRUE),
    egresos = sum(egresos, na.rm = TRUE),
    .groups = "drop")

graficas_totales <- generar_graficas_productividad(consultas_totales, "Consultas totales")
graficas_generales <- generar_graficas_productividad(consultas_generales, "Consultas generales")
graficas_especialidad <- generar_graficas_productividad(consultas_de_especialidad, "Consultas de especialidad")
graficas_pq <- generar_graficas_productividad(procedimientos_quirurgicos, "Procedimientos quirúrgicos")
graficas_egresos <- generar_graficas_productividad(egresos, "Egresos")

graficas_totales$grafica_acumulado
graficas_totales$grafica_mes
graficas_totales$titulo_acumulado
graficas_totales$titulo_mes
# Tablas de consultas generales -------------------------------------------
pct_al_dia <- (num_semana * 100) / 52

data_cg_t <- data_cg %>%
  transmute(
    entidad,
    avance_cg = round(avance_total),
    meta_cg = meta_total,
    pct_cg = if_else(meta_cg > 0, round((avance_cg / meta_cg) * 100, 0), 0)
  )

data_ce_t <- data_esp %>%
  transmute(
    entidad,
    avance_ce = round(avance_total),
    meta_ce = meta_total,
    pct_ce = if_else(meta_ce > 0, round((avance_ce / meta_ce) * 100, 0), 0)
  )

data_pq_t <- data_pq_entidad %>%
  transmute(
    entidad,
    avance_pq = round(avance_total),
    meta_pq = meta_total,
    pct_pq = if_else(meta_pq > 0, round((avance_pq / meta_pq) * 100, 0), 0)
  )

tabla_final <- full_join(data_cg_t, data_ce_t, by = "entidad") %>% 
  full_join(data_pq_t, by = "entidad") %>%
  filter(entidad != "Yucatan") %>% 
  mutate(
    pct_cg_tope = pmin(pct_cg, pct_al_dia, na.rm = TRUE),
    pct_ce_tope = pmin(pct_ce, pct_al_dia, na.rm = TRUE),
    pct_pq_tope = pmin(pct_pq, pct_al_dia, na.rm = TRUE),
    avance_global = round(
      rowMeans(cbind(pct_cg_tope, pct_ce_tope, pct_pq_tope), na.rm = TRUE),
      0
    ),
    avance_global = ifelse(is.nan(avance_global), 0, avance_global)
  ) %>% 
  arrange(desc(avance_global)) %>%
  transmute(
    entidad,
    consultas_generales = if_else(is.na(avance_cg), "—", scales::comma(avance_cg)),
    pct_cg,
    consultas_especialidad = if_else(is.na(avance_ce), "—", scales::comma(avance_ce)),
    pct_ce,
    procedimientos_quirurgicos = if_else(is.na(avance_pq), "—", scales::comma(avance_pq)),
    pct_pq,
    avance_global)

n_por_slide <- ceiling(nrow(tabla_final) / 2)
tabla_1 <- dplyr::slice(tabla_final, 1:n_por_slide)
tabla_2 <- dplyr::slice(tabla_final, (n_por_slide + 1):dplyr::n())

# Menor rendimiento en cirugias -------------------------------------------
tabla_pq <- df_final %>%
  filter(lubridate::year(fecha) == 2026) %>% 
  group_by(clues) %>%
  summarise(
    avance = sum(procedimientos_quirurgicos, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    catalogos_clues %>% 
      select(clues = clues_imb, nombre_comercial, entidad),
    by = "clues"
  ) %>%
  left_join(
    catalogo_metas %>% 
      select(clues = clues_imb, meta_cirugia_anual),
    by = "clues"
  ) %>%
  filter(
    meta_cirugia_anual > 0,
    !clues %in% hbc_bajos
  ) %>%
  mutate(
    meta_actual = meta_cirugia_anual * (num_semana / 52),
    cumplimiento_num_2s = avance / meta_actual,
    entidad = case_when(
      entidad == "VERACRUZ DE IGNACIO DE LA LLAVE" ~ "Veracruz",
      entidad == "MICHOACAN DE OCAMPO" ~ "Michoacán",
      TRUE ~ stringr::str_to_title(entidad)
    ),
    nombre_comercial = nombre_comercial %>%
      stringr::str_to_title() %>%
      stringr::str_replace_all("\\bHg\\b", "HG") %>%
      stringr::str_replace_all("\\bImss\\b", "IMSS")
  ) %>%
  arrange(cumplimiento_num_2s) %>%
  transmute(
    `#` = row_number(),
    CLUES = clues,
    `Nombre de la unidad` = nombre_comercial,
    Entidad = entidad,
    Avance = avance,
    `Meta acumulada` = round(meta_actual),
    `Cumplimiento %` = round(cumplimiento_num_2s * 100, 1)
  )

tabla_3 <- tabla_pq %>% 
  filter(Avance == 0)

n_por_slide_1 <- 10
n_por_slide_2 <- 7

tabla_3_1 <- dplyr::slice(tabla_3, 1:n_por_slide_1)
tabla_3_2 <- dplyr::slice(tabla_3, (n_por_slide_1 + 1):dplyr::n())

ft_3_1 <- ft_estilo_menor(tabla_3_1) %>%
  flextable::fontsize(size = 14, part = "all") %>%   # letras más grandes
  flextable::width(width = c(0.5, 1.6, 4.5, 1.5, 1, 1.4, 1.6))  # columnas más anchas

ft_3_2 <- ft_estilo_menor(tabla_3_2) %>%
  flextable::fontsize(size = 14, part = "all") %>%   # letras más grandes
  flextable::width(width = c(0.5, 1.6, 4.5, 1.5, 1, 1.4, 1.6))  # columnas más anchas

# Graficas semanales por fecha insert -------------------------------------
# Bases semanales por fecha de registro ----------------------------------
avance_entidad <- df_final %>% 
  mutate(
    anio = lubridate::year(fecha),
    anio_insert = lubridate::year(fecha_insert),
    
    fecha_corte_insert = lubridate::ymd(
      paste0(anio_insert, "-", format(fecha_corte, "%m-%d"))
    )
  ) %>% 
  filter(
    anio %in% c(2024, 2025, 2026),
    anio_insert == anio,
    fecha_insert <= fecha_corte_insert
  ) %>% 
  left_join(
    catalogo_metas %>% 
      mutate(clues_imb = as.character(clues_imb)) %>% 
      select(clues = clues_imb, entidad),
    by = "clues"
  ) %>% 
  group_by(anio, entidad) %>% 
  summarise(
    consultas_generales = sum(consultas_generales, na.rm = TRUE),
    consultas_de_especialidad = sum(consultas_de_especialidad, na.rm = TRUE),
    procedimientos_quirurgicos = sum(procedimientos_quirurgicos, na.rm = TRUE),
    egresos = sum(egresos, na.rm = TRUE),
    .groups = "drop") %>% 
  left_join(metas_entidad, by = "entidad")

limpiar_data_avance_2 <- function(df, col_avance, col_meta) {
  df %>% 
    transmute(
      entidad,
      avance_total = {{ col_avance }},
      meta_total = {{ col_meta }},
      pct_avance_entidad = if_else(
        !is.na(meta_total) & meta_total > 0,
        avance_total / meta_total,
        NA_real_)) %>% 
    filter(
      !is.na(entidad),
      !is.na(pct_avance_entidad),
      is.finite(pct_avance_entidad))}

xmax_seguro <- function(df, col = pct_modelo_entidad, suma = 0.05) {
  x <- max(df %>% pull({{ col }}), na.rm = TRUE)
  if (!is.finite(x)) meta_hoy + 0.10 else round(x + suma, 1)
}

data_cg <- limpiar_data_avance_2(
  avance_entidad %>% filter(anio == 2026),
  consultas_generales,
  meta_cg)

data_esp <- limpiar_data_avance_2(
  avance_entidad %>% filter(anio == 2026),
  consultas_de_especialidad,
  meta_ce)

data_pq_entidad <- limpiar_data_avance_2(
  avance_entidad %>% filter(anio == 2026),
  procedimientos_quirurgicos,
  meta_pq
)

data_egresos <- limpiar_data_avance_2(
  avance_entidad %>% filter(anio == 2026),
  egresos,
  meta_egresos
)

datos_vb_retraso <- df_final %>% 
  filter(
    !is.na(fecha_insert)
  ) %>% 
  mutate(
    anio = lubridate::year(fecha),
    anio_insert = lubridate::year(fecha),
    fecha_corte_insert_txt = case_when(
      !is.na(anio_insert) ~ paste0(anio_insert, "-", format(fecha_corte, "%m-%d")),
      TRUE ~ NA_character_
    ),
    fecha_corte_insert = lubridate::ymd(fecha_corte_insert_txt)
  ) %>% 
  filter(
    anio %in% c(2024, 2025, 2026),
    !is.na(fecha_corte_insert),
    anio_insert == anio,
    fecha_insert <= fecha_corte_insert
  ) %>% 
  mutate(semana = lubridate::isoweek(fecha)) %>% 
  group_by(anio, semana) %>% 
  summarise(
    consultas_totales = sum(consultas_totales, na.rm = TRUE),
    consultas_generales = sum(consultas_generales, na.rm = TRUE),
    consultas_de_especialidad = sum(consultas_de_especialidad, na.rm = TRUE),
    procedimientos_quirurgicos = sum(procedimientos_quirurgicos, na.rm = TRUE),
    egresos = sum(egresos, na.rm = TRUE),
    .groups = "drop")

bases_todas_totales_retraso <- crear_base_rezago(
  datos_vb_retraso, consultas_totales, "Totales")

bases_todas_generales_retraso <- crear_base_rezago(
  datos_vb_retraso, consultas_generales, "Generales")

bases_todas_especialidad_retraso <- crear_base_rezago(
  datos_vb_retraso, consultas_de_especialidad, "Especialidad")

bases_todas_pq_retraso <- crear_base_rezago(
  datos_vb_retraso, procedimientos_quirurgicos, "Procedimientos quirúrgicos")

bases_todas_egresos_retraso <- crear_base_rezago(
  datos_vb_retraso, egresos, "Egresos")

grafica_semanal_tot_rez <- grafica_semanal_procedimiento(
  bases_todas_totales_retraso %>% filter(semana <= num_semana),
  procedimiento_sel = "Totales",
  meta_semanal = meta_semanal_totales,
  guardar_svg = FALSE)

grafica_semanal_cg_rez <- grafica_semanal_procedimiento(
  bases_todas_generales_retraso %>% filter(semana <= num_semana),
  procedimiento_sel = "Generales",
  meta_semanal = meta_semanal_general,
  guardar_svg = FALSE)

grafica_semanal_esp_rez <- grafica_semanal_procedimiento(
  bases_todas_especialidad_retraso %>% filter(semana <= num_semana),
  procedimiento_sel = "Especialidad",
  meta_semanal = meta_semanal_especialidad,
  guardar_svg = FALSE)

grafica_semanal_pq_rez <- grafica_semanal_procedimiento(
  bases_todas_pq_retraso %>% filter(semana <= num_semana),
  procedimiento_sel = "Procedimientos quirúrgicos",
  meta_semanal = meta_semanal_procedimientos_quirurgicos,
  guardar_svg = FALSE)

grafica_semanal_egresos_rez <- grafica_semanal_procedimiento(
  bases_todas_egresos_retraso %>% filter(semana <= num_semana),
  procedimiento_sel = "Egresos",
  meta_semanal = meta_semanal_egreso,
  guardar_svg = FALSE)

datos_vb_avance <- df_final %>% 
  mutate(
    anio = lubridate::year(fecha),
    fecha_corte_anio = lubridate::ymd(
      paste0(anio, "-", format(fecha_corte, "%m-%d"))
    ),
    al_corte = fecha <= fecha_corte_anio
  ) %>% 
  filter(
    anio %in% c(2024, 2025, 2026),
    !is.na(fecha),
    !is.na(fecha_corte_anio)
  ) %>% 
  group_by(anio) %>% 
  summarise(
    total_consultas_totales = sum(consultas_totales, na.rm = TRUE),
    total_consultas_generales = sum(consultas_generales, na.rm = TRUE),
    total_consultas_de_especialidad = sum(consultas_de_especialidad, na.rm = TRUE),
    total_procedimientos_quirurgicos = sum(procedimientos_quirurgicos, na.rm = TRUE),
    total_egresos = sum(egresos, na.rm = TRUE),
    
    avance_consultas_totales = sum(consultas_totales[al_corte], na.rm = TRUE),
    avance_consultas_generales = sum(consultas_generales[al_corte], na.rm = TRUE),
    avance_consultas_de_especialidad = sum(consultas_de_especialidad[al_corte], na.rm = TRUE),
    avance_procedimientos_quirurgicos = sum(procedimientos_quirurgicos[al_corte], na.rm = TRUE),
    avance_egresos = sum(egresos[al_corte], na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    pct_avance_consultas_totales = avance_consultas_totales / total_consultas_totales,
    pct_avance_consultas_generales = avance_consultas_generales / total_consultas_generales,
    pct_avance_consultas_de_especialidad = avance_consultas_de_especialidad / total_consultas_de_especialidad,
    pct_avance_procedimientos_quirurgicos = avance_procedimientos_quirurgicos / total_procedimientos_quirurgicos,
    pct_avance_egresos = avance_egresos / total_egresos
  )

avance_2024_t_insert <- graficas_totales$datos_acumulado %>% filter(anio == 2024) %>% pull(valor_fecha_insert)
avance_2025_t_insert <- graficas_totales$datos_acumulado %>% filter(anio == 2025) %>% pull(valor_fecha_insert)
avance_2026_t_insert <- graficas_totales$datos_acumulado %>% filter(anio == 2026) %>% pull(valor_fecha_insert)

p_2024_g_insert <- graficas_generales$datos_acumulado %>% filter(anio == 2024) %>% pull(valor_fecha_insert)
p_2025_g_insert <- graficas_generales$datos_acumulado %>% filter(anio == 2025) %>% pull(valor_fecha_insert)
p_2026_g_insert <- graficas_generales$datos_acumulado %>% filter(anio == 2026) %>% pull(valor_fecha_insert)

p_2024_e_insert <- graficas_especialidad$datos_acumulado %>% filter(anio == 2024) %>% pull(valor_fecha_insert)
p_2025_e_insert <- graficas_especialidad$datos_acumulado %>% filter(anio == 2025) %>% pull(valor_fecha_insert)
p_2026_e_insert <- graficas_especialidad$datos_acumulado %>% filter(anio == 2026) %>% pull(valor_fecha_insert)

p_2024_pq_insert <- graficas_pq $datos_acumulado %>% filter(anio == 2024) %>% pull(valor_fecha_insert)
p_2025_pq_insert <- graficas_pq$datos_acumulado %>% filter(anio == 2025) %>% pull(valor_fecha_insert)
p_2026_pq_insert <- graficas_pq$datos_acumulado %>% filter(anio == 2026) %>% pull(valor_fecha_insert)

egreso_2024_insert <- graficas_egresos$datos_acumulado %>% filter(anio == 2024) %>% pull(valor_fecha_insert)
egreso_2025_insert <- graficas_egresos$datos_acumulado %>% filter(anio == 2025) %>% pull(valor_fecha_insert)
egreso_2026_insert <- graficas_egresos$datos_acumulado %>% filter(anio == 2026) %>% pull(valor_fecha_insert)

datos_variacion_retraso <- datos_vb_avance %>% 
  summarise(
    var_2026_vs_2025_total_consultas = tasa_crecimiento(
      avance_consultas_totales[anio == 2026],
      avance_consultas_totales[anio == 2025]
    ),
    var_2026_vs_2024_total_consultas = tasa_crecimiento(
      avance_consultas_totales[anio == 2026],
      avance_consultas_totales[anio == 2024]
    ),
    
    var_2026_vs_2025_consultas_generales = tasa_crecimiento(
      avance_consultas_generales[anio == 2026],
      avance_consultas_generales[anio == 2025]
    ),
    var_2026_vs_2024_consultas_generales = tasa_crecimiento(
      avance_consultas_generales[anio == 2026],
      avance_consultas_generales[anio == 2024]
    ),
    
    var_2026_vs_2025_consultas_especialidad = tasa_crecimiento(
      avance_consultas_de_especialidad[anio == 2026],
      avance_consultas_de_especialidad[anio == 2025]
    ),
    var_2026_vs_2024_consultas_especialidad = tasa_crecimiento(
      avance_consultas_de_especialidad[anio == 2026],
      avance_consultas_de_especialidad[anio == 2024]
    ),
    
    var_2026_vs_2025_pq = tasa_crecimiento(
      avance_procedimientos_quirurgicos[anio == 2026],
      avance_procedimientos_quirurgicos[anio == 2025]
    ),
    var_2026_vs_2024_pq = tasa_crecimiento(
      avance_procedimientos_quirurgicos[anio == 2026],
      avance_procedimientos_quirurgicos[anio == 2024]
    ),
    
    var_2026_vs_2025_egresos = tasa_crecimiento(
      avance_egresos[anio == 2026],
      avance_egresos[anio == 2025]
    ),
    var_2026_vs_2024_egresos = tasa_crecimiento(
      avance_egresos[anio == 2026],
      avance_egresos[anio == 2024]
    )
  )
valuebox_4 <- do.call(
  crear_valuebox_forma_noto14_small,
  c(list(
    icono = "",
    valor = mk_subtitulo_texto_2lineas(
      datos_variacion_retraso$var_2026_vs_2025_total_consultas,
      datos_variacion_retraso$var_2026_vs_2024_total_consultas
    )
  ), estilo_verde)
)

valuebox_5 <- do.call(
  crear_valuebox_forma_noto14_small,
  c(list(
    icono = "",
    valor = mk_subtitulo_texto_2lineas(
      datos_variacion_retraso$var_2026_vs_2025_consultas_generales,
      datos_variacion_retraso$var_2026_vs_2024_consultas_generales
    )
  ), estilo_verde)
)

valuebox_6 <- do.call(
  crear_valuebox_forma_noto14_small,
  c(list(
    icono = "",
    valor = mk_subtitulo_texto_2lineas(
      datos_variacion_retraso$var_2026_vs_2025_consultas_especialidad,
      datos_variacion_retraso$var_2026_vs_2024_consultas_especialidad
    )
  ), estilo_verde)
)

valuebox_7 <- do.call(
  crear_valuebox_forma_noto14_small,
  c(list(
    icono = "",
    valor = mk_subtitulo_texto_2lineas(
      datos_variacion_retraso$var_2026_vs_2025_pq,
      datos_variacion_retraso$var_2026_vs_2024_pq
    )
  ), estilo_verde)
)

valuebox_8 <- do.call(
  crear_valuebox_forma_noto14_small,
  c(list(
    icono = "",
    valor = mk_subtitulo_texto_2lineas(
      datos_variacion_retraso$var_2026_vs_2025_egresos,
      datos_variacion_retraso$var_2026_vs_2024_egresos
    )
  ), estilo_verde)
)
# -------------------------------------------------------------------------
#PRESENTACION
# -------------------------------------------------------------------------
pptx <- read_pptx("C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/66_Productividad Nacional 2026/Data raw/Master presentación nacional.pptx")

pptx <- pptx %>%
  add_slide(layout = "Portada 3", master = "Tema de Office") %>%
  ph_with(
    "Reporte nacional de productividad médica",
    location = ph_location_label("Título 1")) %>%
  ph_with(fecha_portada,
          location = ph_location_label("Marcador de contenido 2"))

pptx <- pptx %>%
  add_slide(layout = "1_valueboxes", master = "Tema de Office") %>%
  ph_with("Productividad IMSS Bienestar", ph_location_label("Título 1")) %>%
  ph_with(value = fecha_block, location = ph_location_label("fecha")) %>%
  ph_with(value = valuebox_total, location = ph_location_label("arriba 1")) %>%
  ph_with(value = valuebox_general, location = ph_location_label("arriba 2")) %>%
  ph_with(value = valuebox_especialidad, location = ph_location_label("arriba 3")) %>%
  ph_with(value = valuebox_pq, location = ph_location_label("arriba 4")) %>%
  ph_with(value = valuebox_egresos, location = ph_location_label("arriba 5")) %>%
  ph_with(value = valuebox_curps_total, location = ph_location_label("abajo 1")) %>%
  ph_with(value = valuebox_curps_general, location = ph_location_label("abajo 2")) %>%
  ph_with(value = valuebox_curps_especialidad, location = ph_location_label("abajo 3")) %>%
  ph_with(value = valuebox_curps_pq, location = ph_location_label("abajo 4")) %>%
  ph_with(value = valuebox_curps_egresos, location = ph_location_label("abajo 5"))

pptx <- pptx %>%
  add_slide(layout = "Graficas de semaforo", master = "Tema de Office") %>%
  ph_with("Consultas por entidad", ph_location_label("Título 1")) %>%
  ph_with(value = fecha_block, location = ph_location_label("fecha")) %>%
  ph_with("Generales", ph_location_label("Etiquetas 1")) %>%
  ph_with("Especialidad", ph_location_label("Etiquetas 2")) %>%
  ph_with(rvg::dml(ggobj = grafica_avance_cgen), ph_location_label("Grafica 1")) %>%
  ph_with(rvg::dml(ggobj = grafica_avance_cesp), ph_location_label("Grafica 2"))

pptx <- pptx %>%
  add_slide(layout = "Graficas de semaforo", master = "Tema de Office") %>%
  ph_with(value = "Procedimientos por entidad", location = ph_location_label("Título 1")) %>%
  ph_with(value = fecha_block, location = ph_location_label("fecha")) %>%
  ph_with(value = "Procedimientos quirúrgicos", location = ph_location_label("Etiquetas 1")) %>%
  ph_with(value = "Egresos", location = ph_location_label("Etiquetas 2")) %>%
  ph_with(value = rvg::dml(ggobj = grafica_avance_pq), location = ph_location_label("Grafica 1")) %>%
  ph_with(value = rvg::dml(ggobj = grafica_avance_egresos), location = ph_location_label("Grafica 2"))

pptx <- pptx %>%
  add_slide(layout = "Graficas semanal", master = "Tema de Office") %>%
  ph_with(value = "Consultas totales", location = ph_location_label("Título 1")) %>%
  ph_with(value = "60,000,000", location = ph_location_label("Meta anual")) %>%
  ph_with(value = scales::percent(meta_hoy), location = ph_location_label("Objetivo pct")) %>%
  ph_with(
    ft_resumen_avance(
      ancho_tabla = 3.0,
      alto_columna = 0.32,
      avance_2024 = scales::comma(avance_2024_t),
      avance_2025 = scales::comma(avance_2025_t),
      avance_2026 = scales::comma(avance_2026_t),
      pct_2024 = scales::percent(avance_2024_t / productividad_ct_2024),
      pct_2025 = scales::percent(avance_2025_t / productividad_ct_2025),
      pct_2026 = scales::percent(avance_2026_t / productividad_ct_2026)),
    location = ph_location_label("tabla_1")) %>%
  ph_with(value = rvg::dml(ggobj = grafica_semanal_tot$plot), location = ph_location_label("Grafica")) %>%
  ph_with(value = valuebox_0, location = ph_location_label("value"))

pptx <- pptx %>%
  add_slide(layout = "Una grafica", master = "Tema de Office") %>%
  ph_with( value = graficas_totales$titulo_acumulado,
           location = ph_location_label("Título 1")) %>%
  ph_with(value = rvg::dml(ggobj = graficas_totales$grafica_acumulado),
          location = ph_location_label("ft"))

pptx <- pptx %>%
  add_slide(layout = "Una grafica", master = "Tema de Office") %>%
  ph_with(value = graficas_totales$titulo_mes,
          location = ph_location_label("Título 1")) %>%
  ph_with(value = rvg::dml(ggobj = graficas_totales$grafica_mes),
          location = ph_location_label("ft"))

pptx <- pptx %>%
  add_slide(layout = "Graficas semanal", master = "Tema de Office") %>%
  ph_with(value = "Consultas generales", location = ph_location_label("Título 1")) %>%
  ph_with(value = "52,500,000", location = ph_location_label("Meta anual")) %>%
  ph_with(value = scales::percent(meta_hoy), location = ph_location_label("Objetivo pct")) %>%
  ph_with(
    ft_resumen_avance(
      ancho_tabla = 3.0,
      alto_columna = 0.32,
      avance_2024 = scales::comma(p_2024_g),
      avance_2025 = scales::comma(p_2025_g),
      avance_2026 = scales::comma(p_2026_gm),
      pct_2024 = scales::percent(p_2024_g / productividad_cg_2024),
      pct_2025 = scales::percent(p_2025_g / productividad_cg_2025),
      pct_2026 = scales::percent(p_2026_gm / productividad_cg_2026)
    ),
    location = ph_location_label("tabla_1")
  ) %>%
  ph_with(value = rvg::dml(ggobj = grafica_semanal_cg$plot), location = ph_location_label("Grafica")) %>%
  ph_with(value = valuebox_1, location = ph_location_label("value"))

pptx <- pptx %>%
  add_slide(layout = "Una grafica", master = "Tema de Office") %>%
  ph_with(value = graficas_generales$titulo_acumulado,
          location = ph_location_label("Título 1")) %>%
  ph_with(value = rvg::dml(ggobj = graficas_generales$grafica_acumulado),
          location = ph_location_label("ft"))

pptx <- pptx %>%
  add_slide(layout = "Una grafica", master = "Tema de Office") %>%
  ph_with(value = graficas_generales$titulo_mes,
          location = ph_location_label("Título 1")) %>%
  ph_with(value = rvg::dml(ggobj = graficas_generales$grafica_mes),
          location = ph_location_label("ft"))

pptx <- pptx %>%
  add_slide(layout = "Graficas semanal", master = "Tema de Office") %>%
  ph_with(value = "Consultas de especialidad", location = ph_location_label("Título 1")) %>%
  ph_with(value = "7,500,000", location = ph_location_label("Meta anual")) %>%
  ph_with(value = scales::percent(meta_hoy), location = ph_location_label("Objetivo pct")) %>%
  ph_with(
    ft_resumen_avance(
      ancho_tabla = 3,
      alto_columna = 0.32,
      avance_2024 = scales::comma(p_2024_e),
      avance_2025 = scales::comma(p_2025_e),
      avance_2026 = scales::comma(p_2026_em),
      pct_2024 = scales::percent(p_2024_e / productividad_ce_2024),
      pct_2025 = scales::percent(p_2025_e / productividad_ce_2025),
      pct_2026 = scales::percent(p_2026_em / productividad_ce_2026)
    ),
    location = ph_location_label("tabla_1")
  ) %>%
  ph_with(value = rvg::dml(ggobj = grafica_semanal_esp$plot), location = ph_location_label("Grafica")) %>%
  ph_with(value = valuebox_2, location = ph_location_label("value"))

pptx <- pptx %>%
  add_slide(layout = "Una grafica", master = "Tema de Office") %>%
  ph_with(value = graficas_especialidad$titulo_acumulado,
          location = ph_location_label("Título 1")) %>%
  ph_with(value = rvg::dml(ggobj = graficas_especialidad$grafica_acumulado),
          location = ph_location_label("ft"))

pptx <- pptx %>%
  add_slide(layout = "Una grafica", master = "Tema de Office") %>%
  ph_with(value = graficas_especialidad$titulo_mes,
          location = ph_location_label("Título 1")) %>%
  ph_with(value = rvg::dml(ggobj = graficas_especialidad$grafica_mes),
          location = ph_location_label("ft"))

pptx <- pptx %>%
  add_slide(layout = "Graficas semanal", master = "Tema de Office") %>%
  ph_with(value = "Procedimientos quirúrgicos", location = ph_location_label("Título 1")) %>%
  ph_with(value = "1,100,000", location = ph_location_label("Meta anual")) %>%
  ph_with(value = scales::percent(meta_hoy), location = ph_location_label("Objetivo pct")) %>%
  ph_with(
    ft_resumen_avance(
      ancho_tabla = 3.0,
      alto_columna = 0.32,
      avance_2024 = scales::comma(p_2024_pq),
      avance_2025 = scales::comma(p_2025_pq),
      avance_2026 = scales::comma(p_2026_pqm),
      pct_2024 = scales::percent(p_2024_pq / productividad_pq_2024),
      pct_2025 = scales::percent(p_2025_pq / productividad_pq_2025),
      pct_2026 = scales::percent(p_2026_pqm / productividad_pq_2026)
    ),
    location = ph_location_label("tabla_1")
  ) %>%
  ph_with(value = rvg::dml(ggobj = grafica_semanal_pq$plot), location = ph_location_label("Grafica")) %>%
  ph_with(value = valuebox_3, location = ph_location_label("value"))

pptx <- pptx %>%
  add_slide(layout = "Una grafica", master = "Tema de Office") %>%
  ph_with(value = graficas_pq$titulo_acumulado,
          location = ph_location_label("Título 1")) %>%
  ph_with(value = rvg::dml(ggobj = graficas_pq$grafica_acumulado),
          location = ph_location_label("ft"))

pptx <- pptx %>%
  add_slide(layout = "Una grafica", master = "Tema de Office") %>%
  ph_with(value = graficas_pq$titulo_mes,
          location = ph_location_label("Título 1")) %>%
  ph_with(value = rvg::dml(ggobj = graficas_pq$grafica_mes),
          location = ph_location_label("ft"))

pptx <- pptx %>%
  add_slide(layout = "Graficas semanal", master = "Tema de Office") %>%
  ph_with(value = "Egresos", location = ph_location_label("Título 1")) %>%
  ph_with(value = "1,600,000", location = ph_location_label("Meta anual")) %>%
  ph_with(value = scales::percent(meta_hoy), location = ph_location_label("Objetivo pct")) %>%
  ph_with(
    ft_resumen_avance(
      ancho_tabla = 3.0,
      alto_columna = 0.32,
      avance_2024 = scales::comma(egreso_2024),
      avance_2025 = scales::comma(egreso_2025),
      avance_2026 = scales::comma(egreso_2026),
      pct_2024 = scales::percent(egreso_2024 / productividad_egreso_2024),
      pct_2025 = scales::percent(egreso_2025 / productividad_egreso_2025),
      pct_2026 = scales::percent(egreso_2026 / productividad_egreso_2026)
    ),
    location = ph_location_label("tabla_1")
  ) %>%
  ph_with(value = rvg::dml(ggobj = grafica_semanal_egresos$plot), location = ph_location_label("Grafica")) %>%
  ph_with(value = valuebox_egreso, location = ph_location_label("value"))

med <- layout_properties(
  pptx,
  layout = "Una grafica",
  master = "Tema de Office"
) %>%
  dplyr::filter(ph_label == "ft")

W_PH <- med$cx[[1]]
H_PH <- med$cy[[1]]

ft_1 <- ft_base_ajustada(df = tabla_1, W_PH = W_PH, H_PH = H_PH, pct_al_dia = pct_al_dia)
ft_2 <- ft_base_ajustada(df = tabla_2, W_PH = W_PH, H_PH = H_PH, pct_al_dia = pct_al_dia)

pptx <- pptx %>%
  add_slide(layout = "Una grafica", master = "Tema de Office") %>%
  ph_with(
    "Avance semanal en el registro de la productividad global por entidad (1 de 2)",
    ph_location_label("Título 1")) %>%
  ph_with(ft_1, ph_location_label("ft")) 

pptx <- pptx %>%
  add_slide(layout = "Una grafica", master = "Tema de Office") %>%
  ph_with(
    "Avance semanal en el registro de la productividad global por entidad (2 de 2)",
    ph_location_label("Título 1")) %>%
  ph_with(ft_2, ph_location_label("ft")) 

pptx <- pptx %>%
  add_slide(layout = "Una grafica", master = "Tema de Office") %>%
  ph_with("Unidades médicas con menor rendimiento en cirugías",
          ph_location_label("Título 1")) %>%
  ph_with(value = ft_3_1,
          location = ph_location(
            left   = 0.55,
            top    = 1.62,
            width  = 16.85,
            height = 10.45))

pptx <- pptx %>%
  add_slide(layout = "Una grafica", master = "Tema de Office") %>%
  ph_with("Unidades médicas con menor rendimiento en cirugías",
          ph_location_label("Título 1")) %>%
  ph_with(value = ft_3_2,
          location = ph_location(
            left   = 0.55,
            top    = 1.62,
            width  = 16.85,
            height = 10.45))


pptx <- pptx %>%
  add_slide(layout = "Anexo",
            master = "Tema de Office") %>%
  ph_with("Anexo", ph_location_label("Anexo"))

pptx <- pptx %>%
  add_slide(layout = "Graficas semanal", master = "Tema de Office") %>%
  ph_with(value = "Consultas totales con rezago",
          location = ph_location_label("Título 1")) %>%
  ph_with(
    value = "60,000,000",
    location = ph_location_label("Meta anual")) %>%
  ph_with(value = scales::percent(meta_hoy),
          location = ph_location_label("Objetivo pct")) %>%
  ph_with(
    ft_resumen_avance(
      ancho_tabla = 3.0,
      alto_columna = 0.32,
      avance_2024 = scales::comma(avance_2024_t_insert),
      avance_2025 = scales::comma(avance_2025_t_insert),
      avance_2026 = scales::comma(avance_2026_t_insert),
      pct_2024 = scales::percent(avance_2024_t_insert / productividad_cg_2024),
      pct_2025 = scales::percent(avance_2025_t_insert/ productividad_cg_2025),
      pct_2026 = scales::percent(avance_2026_t_insert / productividad_cg_2026)
    ),
    location = ph_location_label("tabla_1")) %>%
  ph_with(value = rvg::dml(ggobj = grafica_semanal_tot_rez$plot),
          location = ph_location_label("Grafica")) %>%
  ph_with(value = valuebox_4, location = ph_location_label("value"))

pptx <- pptx %>%
  add_slide(layout = "Graficas semanal", master = "Tema de Office") %>%
  ph_with(value = "Consultas generales con rezago",
          location = ph_location_label("Título 1")) %>%
  ph_with(value = "52,500,000", location = ph_location_label("Meta anual")) %>%
  ph_with(value = percent(meta_hoy), location = ph_location_label("Objetivo pct")) %>%
  ph_with(
    ft_resumen_avance(
      ancho_tabla = 3.0,
      alto_columna = 0.32,
      avance_2024 = comma(p_2024_g_insert),
      avance_2025 = comma(p_2025_g_insert),
      avance_2026 = comma(p_2026_g_insert),
      pct_2024 = percent(p_2024_g_insert / productividad_cg_2024),
      pct_2025 = percent(p_2025_g_insert / productividad_cg_2025),
      pct_2026 = percent(p_2026_g_insert / productividad_cg_2026)
    ),
    location = ph_location_label("tabla_1")
  ) %>%
  ph_with(
    value = rvg::dml(ggobj = grafica_semanal_cg_rez$plot),
    location = ph_location_label("Grafica")
  ) %>%
  ph_with(
    value = valuebox_5,
    location = ph_location_label("value")
  )

pptx <- pptx %>%
  add_slide(layout = "Graficas semanal", master = "Tema de Office") %>%
  ph_with(
    value = "Consultas de especialidad con rezago",
    location = ph_location_label("Título 1")
  ) %>%
  ph_with(
    value = "7,500,000",
    location = ph_location_label("Meta anual")
  ) %>%
  ph_with(
    value = percent(meta_hoy),
    location = ph_location_label("Objetivo pct")
  ) %>%
  ph_with(
    ft_resumen_avance(
      ancho_tabla = 3.0,
      alto_columna = 0.32,
      avance_2024 = comma(p_2024_e_insert),
      avance_2025 = comma(p_2025_e_insert),
      avance_2026 = comma(p_2026_e_insert),
      pct_2024 = percent(p_2024_e_insert / productividad_ce_2024),
      pct_2025 = percent(p_2025_e_insert / productividad_ce_2025),
      pct_2026 = percent(p_2026_e_insert / productividad_ce_2026)
    ),
    location = ph_location_label("tabla_1")
  ) %>%
  ph_with(
    value = rvg::dml(ggobj = grafica_semanal_esp_rez$plot),
    location = ph_location_label("Grafica")
  ) %>%
  ph_with(
    value = valuebox_7,
    location = ph_location_label("value")
  )

pptx <- pptx %>%
  add_slide(layout = "Graficas semanal", master = "Tema de Office") %>%
  ph_with(
    value = "Procedimientos quirúrgicos con rezago",
    location = ph_location_label("Título 1")
  ) %>%
  ph_with(
    value = "1,100,000",
    location = ph_location_label("Meta anual")
  ) %>%
  ph_with(
    value = percent(meta_hoy),
    location = ph_location_label("Objetivo pct")
  ) %>%
  ph_with(
    ft_resumen_avance(
      ancho_tabla = 3.0,
      alto_columna = 0.32,
      avance_2024 = comma(p_2024_pq_insert),
      avance_2025 = comma(p_2025_pq_insert),
      avance_2026 = comma(p_2026_pq_insert),
      pct_2024 = percent(p_2024_pq_insert / productividad_pq_2024),
      pct_2025 = percent(p_2025_pq_insert / productividad_pq_2025),
      pct_2026 = percent(p_2026_pq_insert / productividad_pq_2026)
    ),
    location = ph_location_label("tabla_1")
  ) %>%
  ph_with(
    value = rvg::dml(ggobj = grafica_semanal_pq_rez$plot),
    location = ph_location_label("Grafica")
  ) %>%
  ph_with(
    value = valuebox_8,
    location = ph_location_label("value")
  )

pptx <- pptx %>%
  add_slide(layout = "Graficas semanal", master = "Tema de Office") %>%
  ph_with(
    value = "Egresos con rezago",
    location = ph_location_label("Título 1")
  ) %>%
  ph_with(
    value = "1,600,000",
    location = ph_location_label("Meta anual")
  ) %>%
  ph_with(
    value = scales::percent(meta_hoy),
    location = ph_location_label("Objetivo pct")
  ) %>%
  ph_with(
    ft_resumen_avance(
      ancho_tabla = 3.0,
      alto_columna = 0.32,
      avance_2024 = scales::comma(egreso_2024_insert),
      avance_2025 = scales::comma(egreso_2025_insert),
      avance_2026 = scales::comma(egreso_2026_insert),
      pct_2024 = scales::percent(egreso_2024_insert / productividad_egreso_2024),
      pct_2025 = scales::percent(egreso_2025_insert / productividad_egreso_2025),
      pct_2026 = scales::percent(egreso_2026_insert / productividad_egreso_2026)),
    location = ph_location_label("tabla_1")) %>%
  ph_with(
    value = rvg::dml(ggobj = grafica_semanal_egresos_rez$plot),
    location = ph_location_label("Grafica")) %>%
  ph_with(
    value = valuebox_8,
    location = ph_location_label("value"))

print(pptx, target = paste0(
  "C:/Users/brittany.pereo/Downloads/Reporte Nacional 2026",
  " (semana ",
  num_semana,")",".pptx"))

