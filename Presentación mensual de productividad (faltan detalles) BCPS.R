library(dplyr)
library(lubridate)
library(officer)
library(tidyr)
library(grid)
library(ggplot2)
library(DBI)
library(duckdb)
# Objetos -----------------------------------------------------------------
fecha_corte <- as.Date("2026-06-30")
fecha_portada <- format(fecha_corte, "%d de %B de %Y")
mes_corte <- month(fecha_corte)
mes_nombre <- stringr::str_to_title(format(fecha_corte, "%B"))
dia_corte <- day(fecha_corte)
# -------------------------------------------------------------------------
# Bases 
# -------------------------------------------------------------------------
# Productividad 2020 - 2023 -----------------------------------------------
con <- dbConnect(duckdb::duckdb())

#Solo correr en caso de querer modificar algo a la base
# df_2020_2023 <- DBI::dbGetQuery(con,"
# SELECT CAST(cubos.anio AS VARCHAR) AS anio,
#        CAST(cubos.fecha AS DATE) AS fecha,
#        SUM(cubos.consultas_totales) AS consultas_totales,
#        SUM(cubos.consultas_generales) AS consultas_generales,
#        SUM(cubos.consultas_de_especialidad) AS consultas_de_especialidad,
#        SUM(cubos.procedimientos_quirurgicos) AS procedimientos_quirurgicos,
#        SUM(cubos.egresos) AS egresos
# 
# FROM read_parquet('C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/66_Productividad Nacional 2026/Data raw/Cubos_completos_2020_2025.parquet') cubos
# 
# LEFT JOIN read_parquet('C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/CLUES/clues.parquet') cat
# ON cubos.clues=cat.clues_ssa_y_sme
# 
# WHERE COALESCE(cat.clues_imb,
#                CASE WHEN cubos.clues LIKE '%IMB%' THEN cubos.clues END) IS NOT NULL
# AND CAST(cubos.fecha AS DATE)<=DATE '2026-01-01'
# AND CAST(cubos.anio AS VARCHAR) NOT IN ('2024','2025')
# 
# GROUP BY CAST(cubos.anio AS VARCHAR), CAST(cubos.fecha AS DATE)
# 
# ORDER BY anio, fecha
# ")

# arrow::write_parquet(df_2020_2023,
#                      "C:/Users/brittany.pereo/GitHub/proyecto_productividad_semanal/df_2020_2023.parquet")

df_2020_2023 <- arrow::read_parquet(
  "C:/Users/brittany.pereo/GitHub/proyecto_productividad_semanal/df_2020_2023.parquet"
)

# Productividad 2024 ------------------------------------------------------
#SOLO CORRER EN CASO DE QUERER MODIFICAR ALGO A LAS BASE
# df_2024 <- DBI::dbGetQuery(con,"
# WITH consultas AS (
#   SELECT clues,
#          CAST(fecha_consulta AS DATE) AS fecha,
#          CAST(CAST(fecha_insert AS TIMESTAMP) AS DATE) AS fecha_insert,
#          COUNT(*) AS consultas_totales,
#          SUM(CASE WHEN LOWER(tipo_consulta) IN ('general','generales') THEN 1 ELSE 0 END) AS consultas_generales,
#          COUNT(DISTINCT CASE WHEN LOWER(tipo_consulta) IN ('general','generales') THEN curp_hash32 END) AS curps_distintas_generales,
#          SUM(CASE WHEN LOWER(tipo_consulta) IN ('especialidad','especialidades') THEN 1 ELSE 0 END) AS consultas_de_especialidad,
#          COUNT(DISTINCT CASE WHEN LOWER(tipo_consulta) IN ('especialidad','especialidades') THEN curp_hash32 END) AS curps_distintas_especialidad,
#          COUNT(DISTINCT curp_hash32) AS total_curps_distintas
#   FROM read_parquet([
#     'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/consulta_externa_01_01_2024_a_31_12_2024.parquet',
#     'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/planificacion_familiar_01_01_2024_a_31_12_2024.parquet',
#     'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/salud_bucal_01_01_2024_a_31_12_2024.parquet',
#     'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/salud_mental_01_01_2024_a_31_12_2024.parquet'
#   ], union_by_name=true)
#   GROUP BY clues, CAST(fecha_consulta AS DATE), CAST(CAST(fecha_insert AS TIMESTAMP) AS DATE)
# ),
# 
# pq AS (
#   SELECT clues,
#          CAST(fecha_egreso AS DATE) AS fecha,
#          CAST(CAST(fecha_insert AS TIMESTAMP) AS DATE) AS fecha_insert,
#          COUNT(*) AS procedimientos_quirurgicos,
#          COUNT(DISTINCT curp_hash32) AS total_curps_distintas_pq
#   FROM read_parquet('C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales procedimientos/quirurgicos 2024 nuevo.parquet')
#   GROUP BY clues, CAST(fecha_egreso AS DATE), CAST(CAST(fecha_insert AS TIMESTAMP) AS DATE)
# ),
# 
# egresos AS (
#   SELECT clues,
#          CAST(fecha_egreso AS DATE) AS fecha,
#          CAST(CAST(fecha_insert AS TIMESTAMP) AS DATE) AS fecha_insert,
#          COUNT(*) AS egresos,
#          COUNT(DISTINCT curp_hash32) AS total_curps_distintas_egresos
#   FROM read_parquet('C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales egresos/egresos 2024 nuevo.parquet')
#   GROUP BY clues, CAST(fecha_egreso AS DATE), CAST(CAST(fecha_insert AS TIMESTAMP) AS DATE)
# ),
# 
# base AS (
#   SELECT COALESCE(c.clues,p.clues,e.clues) AS clues,
#          COALESCE(c.fecha,p.fecha,e.fecha) AS fecha,
#          COALESCE(c.fecha_insert,p.fecha_insert,e.fecha_insert) AS fecha_insert,
#          c.consultas_totales,
#          c.consultas_generales,
#          c.curps_distintas_generales,
#          c.consultas_de_especialidad,
#          c.curps_distintas_especialidad,
#          c.total_curps_distintas,
#          p.procedimientos_quirurgicos,
#          p.total_curps_distintas_pq,
#          e.egresos,
#          e.total_curps_distintas_egresos
#   FROM consultas c
#   FULL JOIN pq p USING(clues,fecha,fecha_insert)
#   FULL JOIN egresos e USING(clues,fecha,fecha_insert)
# )
# 
# SELECT '2024' AS anio,
#        fecha,
#        SUM(COALESCE(consultas_totales,0)) AS consultas_totales,
#        SUM(COALESCE(consultas_generales,0)) AS consultas_generales,
#        SUM(COALESCE(curps_distintas_generales,0)) AS curps_distintas_generales,
#        SUM(COALESCE(consultas_de_especialidad,0)) AS consultas_de_especialidad,
#        SUM(COALESCE(curps_distintas_especialidad,0)) AS curps_distintas_especialidad,
#        SUM(COALESCE(total_curps_distintas,0)) AS total_curps_distintas,
#        SUM(COALESCE(procedimientos_quirurgicos,0)) AS procedimientos_quirurgicos,
#        SUM(COALESCE(total_curps_distintas_pq,0)) AS total_curps_distintas_pq,
#        SUM(COALESCE(egresos,0)) AS egresos,
#        SUM(COALESCE(total_curps_distintas_egresos,0)) AS total_curps_distintas_egresos
# FROM base
# GROUP BY fecha
# ORDER BY fecha
# ")
# 
# arrow::write_parquet(df_2024,
#                      "C:/Users/brittany.pereo/GitHub/proyecto_productividad_semanal/df_2024.parquet")

df_2024 <- arrow::read_parquet(
  "C:/Users/brittany.pereo/GitHub/proyecto_productividad_semanal/df_2024.parquet"
)

# Productividad 2025 ------------------------------------------------------
#solo correr en caso de modificar las bases originales
# df_2025 <- DBI::dbGetQuery(con,"
# WITH consultas AS (
#   SELECT clues,
#          CAST(fecha_consulta AS DATE) AS fecha,
#          CAST(CAST(fecha_insert AS TIMESTAMP) AS DATE) AS fecha_insert,
#          COUNT(*) AS consultas_totales,
#          COUNT(DISTINCT curp_hash32) AS total_curps_distintas,
#          SUM(CASE WHEN LOWER(tipo_consulta) IN ('general','generales') THEN 1 ELSE 0 END) AS consultas_generales,
#          COUNT(DISTINCT CASE WHEN LOWER(tipo_consulta) IN ('general','generales') THEN curp_hash32 END) AS curps_distintas_generales,
#          SUM(CASE WHEN LOWER(tipo_consulta) IN ('especialidad','especialidades') THEN 1 ELSE 0 END) AS consultas_de_especialidad,
#          COUNT(DISTINCT CASE WHEN LOWER(tipo_consulta) IN ('especialidad','especialidades') THEN curp_hash32 END) AS curps_distintas_especialidad
#   FROM read_parquet([
#     'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/consulta_externa_01_01_2025_a_31_12_2025.parquet',
#     'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/planificacion_familiar_01_01_2025_a_31_12_2025.parquet',
#     'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/salud_bucal_01_01_2025_a_31_12_2025.parquet',
#     'C:/Users/brittany.pereo/OneDrive - IMSS-BIENESTAR/División de Procesamiento de información - Repositorio de Datos/Productividad/Bases originales/historicos/salud_mental_01_01_2025_a_31_12_2025.parquet'
#   ], union_by_name=true)
#   GROUP BY clues, CAST(fecha_consulta AS DATE), CAST(CAST(fecha_insert AS TIMESTAMP) AS DATE)
# ),
# 
# pq AS (
#   SELECT clues,
#          CAST(fecha_egreso AS DATE) AS fecha,
#          CAST(CAST(fecha_insert AS TIMESTAMP) AS DATE) AS fecha_insert,
#          COUNT(*) AS procedimientos_quirurgicos,
#          COUNT(DISTINCT curp_hash32) AS total_curps_distintas_pq
#   FROM read_parquet('C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales procedimientos/quirurgicos 2025 nuevo.parquet')
#   GROUP BY clues, CAST(fecha_egreso AS DATE), CAST(CAST(fecha_insert AS TIMESTAMP) AS DATE)
# ),
# 
# egresos AS (
#   SELECT clues,
#          CAST(fecha_egreso AS DATE) AS fecha,
#          CAST(CAST(fecha_insert AS TIMESTAMP) AS DATE) AS fecha_insert,
#          COUNT(*) AS egresos,
#          COUNT(DISTINCT curp_hash32) AS total_curps_distintas_egresos
#   FROM read_parquet('C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/89_correciones_parquets_dn/finales egresos/egresos 2025 nuevo.parquet')
#   GROUP BY clues, CAST(fecha_egreso AS DATE), CAST(CAST(fecha_insert AS TIMESTAMP) AS DATE)
# ),
# 
# base AS (
#   SELECT COALESCE(c.clues,p.clues,e.clues) AS clues,
#          COALESCE(c.fecha,p.fecha,e.fecha) AS fecha,
#          COALESCE(c.fecha_insert,p.fecha_insert,e.fecha_insert) AS fecha_insert,
#          c.consultas_totales,
#          c.total_curps_distintas,
#          c.consultas_generales,
#          c.curps_distintas_generales,
#          c.consultas_de_especialidad,
#          c.curps_distintas_especialidad,
#          p.procedimientos_quirurgicos,
#          p.total_curps_distintas_pq,
#          e.egresos,
#          e.total_curps_distintas_egresos
#   FROM consultas c
#   FULL JOIN pq p USING(clues,fecha,fecha_insert)
#   FULL JOIN egresos e USING(clues,fecha,fecha_insert)
# )
# 
# SELECT '2025' AS anio,
#        fecha,
#        SUM(COALESCE(consultas_totales,0)) AS consultas_totales,
#        SUM(COALESCE(total_curps_distintas,0)) AS total_curps_distintas,
#        SUM(COALESCE(consultas_generales,0)) AS consultas_generales,
#        SUM(COALESCE(curps_distintas_generales,0)) AS curps_distintas_generales,
#        SUM(COALESCE(consultas_de_especialidad,0)) AS consultas_de_especialidad,
#        SUM(COALESCE(curps_distintas_especialidad,0)) AS curps_distintas_especialidad,
#        SUM(COALESCE(procedimientos_quirurgicos,0)) AS procedimientos_quirurgicos,
#        SUM(COALESCE(total_curps_distintas_pq,0)) AS total_curps_distintas_pq,
#        SUM(COALESCE(egresos,0)) AS egresos,
#        SUM(COALESCE(total_curps_distintas_egresos,0)) AS total_curps_distintas_egresos
# FROM base
# GROUP BY fecha
# ORDER BY fecha
# ")
# 
# arrow::write_parquet(df_2025,
#                      "C:/Users/brittany.pereo/GitHub/proyecto_productividad_semanal/df_2025.parquet")

df_2025 <- arrow::read_parquet(
  "C:/Users/brittany.pereo/GitHub/proyecto_productividad_semanal/df_2025.parquet"
)
# Productividad 2026 ------------------------------------------------------
base_2026 <- readxl::read_excel(
  "C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/66_Productividad Nacional 2026/Data/profet/nowcast_todes_estados.xlsx"
) %>% 
  janitor::clean_names() %>% 
  mutate(fecha=as.Date(dia),
         anio=lubridate::year(fecha)) %>% 
  filter(anio==2026, fecha<= fecha_corte) %>% 
  group_by(fecha,tipo_consulta) %>% 
  summarise(nowcast=sum(nowcast,na.rm=TRUE),.groups="drop") %>% 
  tidyr::pivot_wider(names_from=tipo_consulta,
                     values_from=nowcast,
                     values_fill=list(nowcast=0)) %>% 
  mutate(mes=lubridate::floor_date(fecha,"month")) %>% 
  group_by(mes) %>% 
  summarise(general=sum(general,na.rm=TRUE),
            especialidad=sum(especialidad,na.rm=TRUE),
            qx=sum(qx,na.rm=TRUE),
            egresos=sum(egresos,na.rm=TRUE),
            .groups="drop") %>% 
  transmute(anio="2026 modelo",
            fecha=mes+lubridate::days(14),
            consultas_totales=general+especialidad,
            consultas_generales=general,
            consultas_de_especialidad=especialidad,
            procedimientos_quirurgicos=qx,
            egresos=egresos)

reales <- DBI::dbGetQuery(con,"
WITH reales_curp AS (
  SELECT tipo_consulta AS tipo_procedimiento,
         '2026' AS anio,
         COUNT(*) AS procedimientos
  FROM read_parquet('C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/78_transicion sistemas prod/data/consultas_con_ECE_2026.parquet')
  GROUP BY tipo_consulta

  UNION ALL

  SELECT 'consulta total' AS tipo_procedimiento,
         '2026' AS anio,
         COUNT(*) AS procedimientos
  FROM read_parquet('C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/78_transicion sistemas prod/data/consultas_con_ECE_2026.parquet')

  UNION ALL

  SELECT 'qx' AS tipo_procedimiento,
         '2026' AS anio,
         COUNT(*) AS procedimientos
  FROM read_parquet('C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/78_transicion sistemas prod/data/proc_qx_con_ECE_2026.parquet')

  UNION ALL

  SELECT 'egresos' AS tipo_procedimiento,
         '2026' AS anio,
         COUNT(*) AS procedimientos
  FROM read_parquet('C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/78_transicion sistemas prod/data/egresos_con_ECE_2026.parquet')
)

SELECT anio,
       SUM(CASE WHEN tipo_procedimiento='consulta total' THEN procedimientos ELSE 0 END) AS consultas_totales,
       SUM(CASE WHEN tipo_procedimiento='general' THEN procedimientos ELSE 0 END) AS consultas_generales,
       SUM(CASE WHEN tipo_procedimiento='especialidad' THEN procedimientos ELSE 0 END) AS consultas_de_especialidad,
       SUM(CASE WHEN tipo_procedimiento='qx' THEN procedimientos ELSE 0 END) AS procedimientos_quirurgicos,
       SUM(CASE WHEN tipo_procedimiento='egresos' THEN procedimientos ELSE 0 END) AS egresos
FROM reales_curp
GROUP BY anio
")

cols_historicos <- c("anio","fecha","consultas_totales","consultas_generales",
                     "consultas_de_especialidad","procedimientos_quirurgicos","egresos")

historicos <- bind_rows(
  df_2020_2023 %>% select(all_of(cols_historicos)),
  df_2024 %>% select(all_of(cols_historicos)),
  df_2025 %>% select(all_of(cols_historicos)),
  base_2026 %>% select(all_of(cols_historicos))
)

# -------------------------------------------------------------------------
# Presentación 
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
ruta_funciones <- "C:/Users/brittany.pereo/GitHub/proyecto_productividad_semanal/Funciones presentacion nacional mensual.R"
source(ruta_funciones)

pptx <- read_pptx("C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/84_presentacion_clues/data raw/Master presentación.pptx")
# Portada diapo 1 -------------------------------------------------------
pptx <- pptx %>%
  add_slide(layout = "Portada 3", master = "Tema de Office") %>%
  ph_with("Reporte nacional de\n productividad médica",
          location = ph_location_label("Título 1")) %>%
  ph_with(paste0("Cierre de mes: ", mes_nombre, " 2026"),
          location = ph_location_label("Marcador de contenido 2"))

# Value box diapo 2 ------------------------------------------------------
cols_metricas <- c("consulta_gral","consulta_esp","qx","total_consultas","egresos")

agregar_vars_2026 <- function(df, cols=cols_metricas, refs=c(2024,2025)){
  for(col in cols){
    for(ref in refs){
      valor_2026 <- df[[col]][df$anio==2026]
      valor_ref  <- df[[col]][df$anio==ref]
      df[[paste0("var_2026_vs_",ref,"_",col)]] <- ifelse(
        df$anio==2026 & length(valor_ref)>0 & !is.na(valor_ref) & valor_ref!=0,
        round((valor_2026-valor_ref)/valor_ref*100,0),
        0
      )
    }
  }
  df
}

datos_consulta <- historicos %>% 
  mutate(anio=as.integer(stringr::str_extract(as.character(anio),"\\d{4}")),
         fecha_corte_anual=lubridate::make_date(anio,lubridate::month(fecha_corte),15)) %>% 
  filter(fecha<=fecha_corte_anual) %>% 
  group_by(anio) %>% 
  summarise(consulta_gral=sum(consultas_generales,na.rm=TRUE),
            consulta_esp=sum(consultas_de_especialidad,na.rm=TRUE),
            qx=sum(procedimientos_quirurgicos,na.rm=TRUE),
            total_consultas=sum(consultas_totales,na.rm=TRUE),
            egresos=sum(egresos,na.rm=TRUE),
            .groups="drop") %>% 
  arrange(anio) %>% 
  agregar_vars_2026()

datos_curps <- DBI::dbGetQuery(con,"
SELECT anio,
       SUM(CASE WHEN origen='general' THEN total_curps ELSE 0 END) AS consulta_gral,
       SUM(CASE WHEN origen='especialidad' THEN total_curps ELSE 0 END) AS consulta_esp,
       SUM(CASE WHEN origen='qx' THEN total_curps ELSE 0 END) AS qx,
       SUM(CASE WHEN origen='consulta total' THEN total_curps ELSE 0 END) AS total_consultas,
       SUM(CASE WHEN origen='egresos' THEN total_curps ELSE 0 END) AS egresos
FROM (
  SELECT anio_insert AS anio,
         LOWER(tipo_procedimiento) AS origen,
         SUM(personas) AS total_curps
  FROM read_parquet('C:/Users/brittany.pereo/IMSS-BIENESTAR/División de Procesamiento de información - Proyectos/84_presentacion_clues/data raw/procedimientos_personas_junio.parquet')
  GROUP BY anio_insert, LOWER(tipo_procedimiento)
)
GROUP BY anio
ORDER BY anio
") %>% 
  agregar_vars_2026()

vbox_consultas <- crear_valueboxes_2026(
  datos_consulta,
  c(total_consultas="Consultas totales",
    consulta_gral="Consulta general",
    consulta_esp="Especialidad",
    qx="Procedimientos quirúrgicos",
    egresos="Egresos")
)

vbox_curps <- crear_valueboxes_2026(
  datos_curps,
  c(total_consultas="Consultas totales",
    consulta_gral="Consulta general",
    consulta_esp="Especialidad",
    qx="Intervenidas",
    egresos="Egresadas")
)

pptx <- pptx %>%
  add_slide(layout="10_valueboxes", master="Tema de Office") %>%
  ph_with("Productividad IMSS Bienestar", ph_location_label("Título 1")) %>%
  ph_with(paste0("Del 1 al 30 de ",stringr::str_to_sentence(mes_nombre)," 2026"),
          ph_location_label("fecha")) %>%
  ph_with(vbox_consultas$total_consultas, ph_location_label("arriba 1")) %>%
  ph_with(vbox_consultas$consulta_gral, ph_location_label("arriba 2")) %>%
  ph_with(vbox_consultas$consulta_esp, ph_location_label("arriba 3")) %>%
  ph_with(vbox_consultas$qx, ph_location_label("arriba 4")) %>%
  ph_with(vbox_consultas$egresos, ph_location_label("arriba 5")) %>%
  ph_with(vbox_curps$total_consultas, ph_location_label("abajo 1")) %>%
  ph_with(vbox_curps$consulta_gral, ph_location_label("abajo 2")) %>%
  ph_with(vbox_curps$consulta_esp, ph_location_label("abajo 3")) %>%
  ph_with(vbox_curps$qx, ph_location_label("abajo 4")) %>%
  ph_with(vbox_curps$egresos, ph_location_label("abajo 5"))
# Barras diapo 3 ---------------------------------------------------------
grafica_consultas <- crear_grafica_real_modelo(
  datos_consulta=datos_consulta,
  reales=reales,
  col_modelo=total_consultas,
  col_real=consultas_totales,
  titulo="Consultas totales",
  etiqueta_modelo="Meta mayo"
)

grafica_qx <- crear_grafica_real_modelo(
  datos_consulta=datos_consulta,
  reales=reales,
  col_modelo=qx,
  col_real=procedimientos_quirurgicos,
  titulo="Procedimientos quirúrgicos",
  etiqueta_modelo="Meta junio"
)

ft_consultas <- crear_tabla_ft(
  df=datos_consulta,
  indicadores=c("consulta_gral","consulta_esp"),
  etiquetas=c("Consultas generales","Consultas de especialidad*"),
  mes_nombre=mes_nombre
)

ft_proc <- crear_tabla_ft(
  df=datos_consulta,
  indicadores=c("qx","egresos"),
  etiquetas=c("Procedimientos quirúrgicos","Egresos"),
  mes_nombre=mes_nombre
)

pptx <- pptx %>%
  add_slide(layout="Historico consultas y procedimientos", master="Tema de Office") %>%
  ph_with("Productividad IMSS Bienestar", ph_location_label("Título 1")) %>%
  ph_with(rvg::dml(ggobj=grafica_consultas), ph_location_label("Grafica 1")) %>%
  ph_with(rvg::dml(ggobj=grafica_qx), ph_location_label("Grafica 2")) %>%
  ph_with(ft_consultas, ph_location_label("tabla_1"), use_loc_size=TRUE) %>% 
ph_with(ft_proc, ph_location_label("tabla_2"), use_loc_size=TRUE)
# Graf temporal diapo 4 --------------------------------------------------
serie_mensual_consultas <- historicos %>%
  mutate(fecha=lubridate::floor_date(fecha,"month")) %>%
  filter(!is.na(fecha), !is.na(consultas_totales)) %>%
  summarise(consultas_totales=sum(consultas_totales,na.rm=TRUE),
            .by=fecha) %>%
  arrange(fecha)

g_periodos_consulta <- grafica_consultas_periodos(
  df=serie_mensual_consultas,
  fecha_inicio="2022-08-01",
  fecha_fin=lubridate::floor_date(fecha_corte,"month"),
  titulo=paste0(
    "Consultas totales del IMSS Bienestar (agosto 2022 – ",
    stringr::str_to_lower(mes_nombre)," 2026)"
  )
)

pptx <- pptx %>%
  add_slide(layout="Una grafica", master="Tema de Office") %>%
  ph_with(paste0("Consultas totales por mes (2022-",lubridate::year(fecha_corte),")"),
          ph_location_label("Título 1")) %>%
  ph_with(rvg::dml(ggobj=g_periodos_consulta),
          ph_location_label("ft"))
# Graf temporal diapo 5 --------------------------------------------------
serie_mensual_pq <- historicos %>%
  mutate(fecha=lubridate::floor_date(fecha,"month")) %>%
  filter(!is.na(fecha), !is.na(procedimientos_quirurgicos)) %>%
  summarise(consultas_totales=sum(procedimientos_quirurgicos,na.rm=TRUE),
            .by=fecha) %>%
  arrange(fecha)

g_periodos_pq <- grafica_consultas_periodos(
  df=serie_mensual_pq,
  fecha_inicio="2022-08-01",
  fecha_fin=lubridate::floor_date(fecha_corte,"month"),
  titulo=paste0(
    "Procedimientos quirúrgicos del IMSS Bienestar (agosto 2022 – ",
    stringr::str_to_lower(mes_nombre)," 2026)"
  )
)

pptx <- pptx %>%
  add_slide(layout="Una grafica", master="Tema de Office") %>%
  ph_with("Procedimientos quirúrgicos por mes (2022-2026)",
          ph_location_label("Título 1")) %>%
  ph_with(rvg::dml(ggobj=g_periodos_pq),
          ph_location_label("ft"))
# IMPRESIÓN ---------------------------------------------------------------
print(pptx, target = "C:/Users/brittany.pereo/Downloads/nacional_fin1_de_mes_.pptx"
)
