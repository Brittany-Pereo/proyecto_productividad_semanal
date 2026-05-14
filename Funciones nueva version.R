# FUNCIONES DE AYUDA CON FORMATO ------------------------------------------
fmt_delta <- function(x) {
  if (is.na(x)) return(list(label = "s/d", col = col_muted, icon = ""))
  if (x > 0)    return(list(label = paste0("+", x, "%"), col = col_verde,  icon = "▲ "))
  if (x < 0)    return(list(label = paste0(x, "%"),      col = col_guinda, icon = "▼ "))
  list(label = "0%", col = col_muted, icon = "• ")
}

fmt_num <- function(x) scales::comma(as.integer(x))

fmt_si <- scales::label_number(
  scale_cut = scales::cut_short_scale(),
  accuracy = 0.1,
  trim = TRUE)

fmt_var_texto <- function(x, anio_ref) {
  if (is.na(x) || is.infinite(x)) {
    return(paste0("s/d que en ", anio_ref))
  }
  
  valor <- round(abs(x), 1)
  
  if (x > 0) {
    paste0(valor, "% más que ", anio_ref)
  } else if (x < 0) {
    paste0(valor, "% menos que ", anio_ref)
  } else {
    paste0("igual que en ", anio_ref)
  }
}

formato_miles_compacto <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    TRUE ~ paste0(formatC(x / 1e3, format = "f", digits = 1), " mil")
  )
}

mk_subtitulo_texto_2lineas <- function(var_2025, var_2024) {
  paste(
    fmt_var_texto(var_2025, 2025),
    fmt_var_texto(var_2024, 2024),
    sep = "\n"
  )
}

resumen_periodo_insert <- function(df, variable, fecha_ini, fecha_fin, anios = 2023:2026) {
  
  df %>% 
    mutate(
      anio = lubridate::year(fecha),
      fecha_ini_anio = as.Date(paste0(anio, "-", format(fecha_ini, "%m-%d"))),
      fecha_fin_anio = as.Date(paste0(anio, "-", format(fecha_fin, "%m-%d")))
    ) %>% 
    filter(
      anio %in% anios,
      fecha >= fecha_ini_anio,
      fecha <= fecha_fin_anio
    ) %>% 
    group_by(anio) %>% 
    summarise(
      valor = sum({{ variable }}, na.rm = TRUE),
      valor_fecha_insert = sum(
        if_else(
          is.na(fecha_insert) | fecha_insert <= fecha_fin_anio,
          {{ variable }},
          0
        ),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>% 
    mutate(
      valor_fecha_insert = if_else(anio == 2023, valor, valor_fecha_insert),
      diferencia = valor - valor_fecha_insert
    )
}

# FUNCIONES AUXILIARES TEMPORALES -----------------------------------------
sumar_meses <- function(df1, df2) {
  full_join(df1, df2, by = "anio", suffix = c("_1", "_2")) %>%
    transmute(
      anio,
      valor = (valor_1 + valor_2),
      valor_fecha_insert = (valor_fecha_insert_1 + valor_fecha_insert_2),
      diferencia = valor - valor_fecha_insert
    )
}

sumar_lista_meses <- function(lista_df) {
  reduce(lista_df, sumar_meses)
}

crear_semanas <- function(X,fecha_inicio) {
  if(year(fecha_inicio)=="2024") {
    
    crear_semanas2 <- function(X) {(yday(X) - 4)%/%7 + 1} 
  } else if (year(fecha_inicio)=="2025") {
    crear_semanas2 <- function(X) {(yday(X) - 1)%/%7 + 1} 
  }else {
    crear_semanas2 <- function(X) {(yday(X) - 1)%/%7 + 1} 
  }
  resultado <- crear_semanas2(X)
  return(resultado)
}

# FUNCIONES PARA AGRUPAR DATOS --------------------------------------------
# Función base
resumen_periodo_insert <- function(df, variable, fecha_ini, fecha_fin, anios = 2023:2026) {
  
  df %>% 
    mutate(
      anio = lubridate::year(fecha),
      fecha_ini_anio = as.Date(paste0(anio, "-", format(fecha_ini, "%m-%d"))),
      fecha_fin_anio = as.Date(paste0(anio, "-", format(fecha_fin, "%m-%d")))
    ) %>% 
    filter(
      anio %in% anios,
      fecha >= fecha_ini_anio,
      fecha <= fecha_fin_anio
    ) %>% 
    group_by(anio) %>% 
    summarise(
      valor = sum({{ variable }}, na.rm = TRUE),
      valor_fecha_insert = sum(
        if_else(
          is.na(fecha_insert) | fecha_insert <= fecha_fin_anio,
          {{ variable }},
          0
        ),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>% 
    mutate(diferencia = valor - valor_fecha_insert)
}

crear_base_rezago <- function(df, columna, procedimiento_txt) {
  df %>% 
    transmute(
      anio,
      semana,
      procedimiento = procedimiento_txt,
      total = {{ columna }})}
# FUNCIONES PARA VALUE BOXES/CARDS ----------------------------------------
estilo_valuebox <- list(
  transparencia    = 90,
  ancho_borde      = 12700,
  tamano_titulo    = 16,
  tamano_valor     = 28,
  tamano_subtitulo = 14,
  color_titulo     = "#a57f2c",
  color_valor      = "#a57f2c",
  color_subtitulo  = "#a57f2c",
  negrita_titulo   = TRUE,
  negrita_valor    = TRUE,
  negrita_subtitulo = FALSE,
  italica_titulo   = FALSE,
  italica_valor    = FALSE,
  italica_subtitulo = TRUE)

crear_valuebox_forma_noto14_small <- function(
    icono = "",
    titulo = "",
    valor = "",
    subtitulo = NULL,
    posicion = 1,
    
    # --- shape size (pulgadas)
    width_in  = 2.6,
    height_in = 1.1,
    
    color_fondo = "#006657",
    color_borde = "",
    ancho_borde = 0,
    transparencia = 0,
    radio_esquina = 20000,
    
    color_valor = "#A77A22",
    color_titulo = "#FFFFFF",
    color_subtitulo = "#FFFFFF",
    
    tamano_titulo = 16,
    tamano_valor = 16,
    tamano_subtitulo = 12,
    
    negrita_valor = TRUE,
    italica_valor = FALSE,
    negrita_titulo = TRUE,
    italica_titulo = FALSE,
    negrita_subtitulo = FALSE,
    italica_subtitulo = FALSE,
    
    # --- fuentes
    fuente_valor = "Noto Sans",
    fuente_titulo = "Noto Sans",
    fuente_subtitulo = "Noto Sans",
    
    #control de interlineado
    interlineado = 1.5,
    espacio_despues_pt = 3
) {
  
  # --- coerciones seguras
  icono <- if (length(icono) > 0) as.character(icono[[1]]) else ""
  titulo <- if (length(titulo) > 0) as.character(titulo[[1]]) else ""
  valor  <- if (length(valor)  > 0) as.character(valor[[1]])  else ""
  if (!is.null(subtitulo) && length(subtitulo) > 0) {
    subtitulo <- as.character(subtitulo[[1]])
  }
  
  # --- helpers
  inch_to_emu <- function(x) as.integer(x * 914400)
  
  # PowerPoint:
  # 100% = 100000
  interlineado_pct <- as.integer(interlineado * 100000)
  
  # espacio antes/después en puntos * 100
  espacio_despues_xml <- as.integer(espacio_despues_pt * 100)
  
  # --- tamaño del shape
  cx <- inch_to_emu(width_in)
  cy <- inch_to_emu(height_in)
  
  alpha <- as.integer((transparencia / 100) * 100000)
  
  # --- tamaños de fuente
  tamano_titulo_emu    <- as.integer(tamano_titulo * 100)
  tamano_subtitulo_emu <- as.integer(tamano_subtitulo * 100)
  
  # --- forzar valor a 14 pt
  tamano_valor_emu <- 14L * 100L
  
  xml_parts <- list()
  
  xml_parts$header <- sprintf(
    "<p:sp xmlns:a=\"http://schemas.openxmlformats.org/drawingml/2006/main\"
          xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\"
          xmlns:p=\"http://schemas.openxmlformats.org/presentationml/2006/main\">
       <p:nvSpPr>
         <p:cNvPr id=\"%d\" name=\"ValueBox %d\"/>
         <p:cNvSpPr/>
         <p:nvPr/>
       </p:nvSpPr>
       <p:spPr>",
    posicion + 1000, posicion
  )
  
  xml_parts$transform <- sprintf(
    "         <a:xfrm>
           <a:off x=\"0\" y=\"0\"/>
           <a:ext cx=\"%d\" cy=\"%d\"/>
         </a:xfrm>",
    cx, cy
  )
  
  xml_parts$geometry <- sprintf(
    "         <a:prstGeom prst=\"roundRect\">
           <a:avLst>
             <a:gd name=\"adj\" fmla=\"val %d\"/>
           </a:avLst>
         </a:prstGeom>",
    radio_esquina
  )
  
  if (alpha > 0) {
    xml_parts$fill <- sprintf(
      "         <a:solidFill>
           <a:srgbClr val=\"%s\">
             <a:alpha val=\"%d\"/>
           </a:srgbClr>
         </a:solidFill>",
      substring(color_fondo, 2), 100000 - alpha
    )
  } else {
    xml_parts$fill <- sprintf(
      "         <a:solidFill>
           <a:srgbClr val=\"%s\"/>
         </a:solidFill>",
      substring(color_fondo, 2)
    )
  }
  
  if (!is.null(color_borde) && color_borde != "") {
    xml_parts$border <- sprintf(
      "         <a:ln w=\"%d\">
           <a:solidFill>
             <a:srgbClr val=\"%s\"/>
           </a:solidFill>
         </a:ln>",
      ancho_borde, substring(color_borde, 2)
    )
  } else {
    xml_parts$border <- "         <a:ln w=\"0\">
           <a:noFill/>
         </a:ln>"
  }
  
  xml_parts$close_spPr <- "       </p:spPr>"
  
  crear_formato_texto <- function(tamano_emu, color_hex, negrita = FALSE, italica = FALSE, fuente = NULL) {
    color_rgb <- substring(color_hex, 2)
    
    formatos <- c(
      if (negrita) "b=\"1\"" else "b=\"0\"",
      if (italica) "i=\"1\"" else "i=\"0\""
    )
    
    fuente_xml <- ""
    if (!is.null(fuente) && nzchar(fuente)) {
      fuente_xml <- sprintf(
        "\n               <a:latin typeface=\"%s\"/>\n               <a:cs typeface=\"%s\"/>",
        fuente, fuente
      )
    }
    
    sprintf(
      "<a:rPr lang=\"es-MX\" sz=\"%d\" %s>
               <a:solidFill>
                 <a:srgbClr val=\"%s\"/>
               </a:solidFill>%s
             </a:rPr>",
      tamano_emu, paste(formatos, collapse = " "), color_rgb, fuente_xml
    )
  }
  
  ppr_xml <- sprintf(
    "           <a:pPr>
             <a:lnSpc>
               <a:spcPct val=\"%d\"/>
             </a:lnSpc>
             <a:spcAft>
               <a:spcPts val=\"%d\"/>
             </a:spcAft>
           </a:pPr>",
    interlineado_pct,
    espacio_despues_xml
  )
  
  # --- texto sin subtítulo
  xml_parts$text <- sprintf(
    "       <p:txBody>
         <a:bodyPr rtlCol=\"0\" anchor=\"ctr\"/>
         <a:lstStyle/>
         <a:p>
%s
           <a:r>
             %s
             <a:t>%s</a:t>
           </a:r>
           <a:br/>
           <a:r>
             %s
             <a:t>%s</a:t>
           </a:r>
           <a:br/>
           <a:r>
             %s
             <a:t>%s</a:t>
           </a:r>
         </a:p>
       </p:txBody>
     </p:sp>",
    ppr_xml,
    crear_formato_texto(tamano_valor_emu,  color_valor,  negrita_valor,  italica_valor,  fuente_valor),  icono,
    crear_formato_texto(tamano_valor_emu,  color_valor,  negrita_valor,  italica_valor,  fuente_valor),  valor,
    crear_formato_texto(tamano_titulo_emu, color_titulo, negrita_titulo, italica_titulo, fuente_titulo), titulo
  )
  
  # --- texto con subtítulo
  if (!is.null(subtitulo) && subtitulo != "") {
    xml_parts$text <- sprintf(
      "       <p:txBody>
         <a:bodyPr rtlCol=\"0\" anchor=\"ctr\"/>
         <a:lstStyle/>
         <a:p>
%s
           <a:r>
             %s
             <a:t>%s</a:t>
           </a:r>
           <a:br/>
           <a:r>
             %s
             <a:t>%s</a:t>
           </a:r>
           <a:br/>
           <a:r>
             %s
             <a:t>%s</a:t>
           </a:r>
           <a:br/>
           <a:r>
             %s
             <a:t>%s</a:t>
           </a:r>
         </a:p>
       </p:txBody>
     </p:sp>",
      ppr_xml,
      crear_formato_texto(tamano_valor_emu,  color_valor,  negrita_valor,  italica_valor,  fuente_valor),  icono,
      crear_formato_texto(tamano_valor_emu,  color_valor,  negrita_valor,  italica_valor,  fuente_valor),  valor,
      crear_formato_texto(tamano_titulo_emu, color_titulo, negrita_titulo, italica_titulo, fuente_titulo), titulo,
      crear_formato_texto(tamano_subtitulo_emu, color_subtitulo, negrita_subtitulo, italica_subtitulo, fuente_subtitulo), subtitulo
    )
  }
  
  shape_xml <- paste(
    xml_parts$header,
    xml_parts$transform,
    xml_parts$geometry,
    xml_parts$fill,
    xml_parts$border,
    xml_parts$close_spPr,
    xml_parts$text,
    sep = "\n"
  )
  
  xml2::as_xml_document(shape_xml)
}

crear_card_institucional <- function(
    numero,
    titulo,
    var_vs_2025,
    var_vs_2024,
    acento = col_verde) {
  d25 <- fmt_delta(var_vs_2025)
  d24 <- fmt_delta(var_vs_2024)
  
  rvg::dml(code = {
    grid::grid.newpage()
    
    # Card base blanca
    grid::grid.roundrect(
      x = 0.5, y = 0.5, width = 0.98, height = 0.98,
      r = grid::unit(10, "pt"),
      gp = grid::gpar(fill = "white", col = col_borde, lwd = 1)
    )
    
    # Franja lateral con color condicional
    grid::grid.rect(
      x = 0.035, y = 0.5,
      width = 0.020, height = 0.90,
      gp = grid::gpar(fill = acento, col = NA)
    )
    
    # Número
    grid::grid.text(
      fmt_num(numero),
      x = 0.07, y = 0.73,
      just = c("left","center"),
      gp = grid::gpar(
        col = col_dorado,
        fontsize = 30,
        fontface = "bold",
        fontfamily = "Calibri"
      )
    )
    
    # Título
    grid::grid.text(
      titulo,
      x = 0.07, y = 0.44,
      just = c("left","center"),
      gp = grid::gpar(
        col = col_texto,
        fontsize = 13,
        fontface = "bold",
        fontfamily = "Calibri"
      )
    )
    
    # ----- Línea vs 2025
    label_25 <- paste0(d25$icon, "vs 2025 ")
    x0 <- grid::unit(0.07, "npc")
    y1 <- grid::unit(0.22, "npc")
    
    grid::grid.text(
      label_25,
      x = x0, y = y1,
      just = c("left","center"),
      gp = grid::gpar(
        col = col_muted,
        fontsize = 10.5,
        fontfamily = "Calibri"
      )
    )
    
    x_pct_25 <- x0 + grid::stringWidth(label_25)
    
    grid::grid.text(
      d25$label,
      x = x_pct_25, y = y1,
      just = c("left","center"),
      gp = grid::gpar(
        col = d25$col,
        fontsize = 10.5,
        fontface = "bold",
        fontfamily = "Calibri"
      )
    )
    
    # ----- Línea vs 2024
    label_24 <- paste0(d24$icon, "vs 2024 ")
    y2 <- grid::unit(0.11, "npc")
    
    grid::grid.text(
      label_24,
      x = x0, y = y2,
      just = c("left","center"),
      gp = grid::gpar(
        col = col_muted,
        fontsize = 10.5,
        fontfamily = "Calibri"
      )
    )
    
    x_pct_24 <- x0 + grid::stringWidth(label_24)
    
    grid::grid.text(
      d24$label,
      x = x_pct_24, y = y2,
      just = c("left","center"),
      gp = grid::gpar(
        col = d24$col,
        fontsize = 10.5,
        fontface = "bold",
        fontfamily = "Calibri"
      )
    )
  })
}

elige_acento <- function(var_2025, var_2024,
                         verde    = "#00B050",
                         amarillo = "#FFC000",
                         rojo     = "#FF0000") {
  
  v25_neg <- !is.na(var_2025) && var_2025 < 0
  v24_neg <- !is.na(var_2024) && var_2024 < 0
  
  if (v25_neg && v24_neg) return(rojo)
  if (xor(v25_neg, v24_neg)) return(amarillo)
  return(verde)
}

# FUNCIONES TABLAS PARA PPT -----------------------------------------------
ft_base_ajustada <- function(df, W_PH, H_PH,
                             pct_al_dia,
                             font_family = "Noto Sans",
                             size_header = 14,
                             size_body   = 13) {
  
  df <- as.data.frame(df)
  
  n_body   <- nrow(df)
  h_header <- 0.38
  h_body   <- max(0.28, min(0.55, (H_PH - h_header) / max(n_body, 1)))
  
  base_w <- c(
    entidad                    = 0.95,
    consultas_generales        = 1.45,
    pct_cg                     = 0.72,
    consultas_especialidad     = 1.45,
    pct_ce                     = 0.72,
    procedimientos_quirurgicos = 1.45,
    pct_pq                     = 0.72,
    avance_global              = 0.95
  )
  
  base_w <- base_w[names(base_w) %in% names(df)]
  escala <- W_PH / sum(base_w)
  w <- base_w * escala
  
  ft <- flextable::flextable(df) %>%
    flextable::set_header_labels(
      entidad                    = "Entidad",
      consultas_generales        = "Consultas generales",
      pct_cg                     = "% avance",
      consultas_especialidad     = "Consultas especialidad",
      pct_ce                     = "% avance",
      procedimientos_quirurgicos = "Procedimientos quirúrgicos",
      pct_pq                     = "% avance",
      avance_global              = "Avance global"
    ) %>%
    
    flextable::colformat_num(
      j = intersect(c("pct_cg", "pct_ce", "pct_pq", "avance_global"), names(df)),
      digits = 0,
      suffix = "%"
    ) %>%
    
    flextable::font(part = "header", fontname = font_family) %>%
    flextable::font(part = "body",   fontname = font_family) %>%
    flextable::fontsize(part = "header", size = size_header) %>%
    flextable::fontsize(part = "body",   size = size_body) %>%
    
    flextable::bold(part = "header") %>%
    flextable::bg(part = "header", bg = "#0B5D4A") %>%
    flextable::color(part = "header", color = "white") %>%
    flextable::padding(part = "all", padding = 3) %>%
    
    flextable::align(j = "entidad", align = "left", part = "body") %>%
    flextable::align(j = "entidad", align = "center", part = "header") %>%
    flextable::align(
      j = intersect(c("consultas_generales",
                      "consultas_especialidad",
                      "procedimientos_quirurgicos"), names(df)),
      align = "right",
      part = "body"
    ) %>%
    flextable::align(
      j = intersect(c("pct_cg", "pct_ce", "pct_pq", "avance_global"), names(df)),
      align = "center",
      part = "all"
    ) %>%
    flextable::valign(valign = "center", part = "all")
  
  cols_pct <- intersect(c("pct_cg", "pct_ce", "pct_pq", "avance_global"), names(df))
  
  if (length(cols_pct) > 0) {
    ft <- ft %>%
      flextable::bold(j = cols_pct, bold = TRUE, part = "body")
  }
  
  # Solo colorear avance_global
  if ("avance_global" %in% names(df)) {
    ft <- ft %>%
      flextable::bg(
        i = ~ avance_global < pct_al_dia * 0.50,
        j = "avance_global",
        bg = "#F34949"
      ) %>%
      flextable::bg(
        i = ~ avance_global >= pct_al_dia * 0.50 & avance_global < pct_al_dia * 0.75,
        j = "avance_global",
        bg = "#F5DD61"
      ) %>%
      flextable::bg(
        i = ~ avance_global >= pct_al_dia * 0.75 & avance_global < pct_al_dia,
        j = "avance_global",
        bg = "#A9D18E"
      ) %>%
      flextable::bg(
        i = ~ avance_global >= pct_al_dia,
        j = "avance_global",
        bg = "#006657"
      ) %>%
      flextable::color(
        i = ~ avance_global < pct_al_dia * 0.50 | avance_global >= pct_al_dia,
        j = "avance_global",
        color = "white"
      ) %>%
      flextable::color(
        i = ~ avance_global >= pct_al_dia * 0.50 & avance_global < pct_al_dia,
        j = "avance_global",
        color = "black"
      )
  }
  
  ft <- ft %>%
    flextable::border_remove() %>%
    flextable::border_outer(
      border = officer::fp_border(color = "white", width = 1)
    ) %>%
    flextable::border_inner_h(
      border = officer::fp_border(color = "white", width = 0.7)
    ) %>%
    flextable::border_inner_v(
      border = officer::fp_border(color = "white", width = 0.7)
    ) %>%
    flextable::width(j = names(w), width = as.numeric(w)) %>%
    flextable::height(part = "header", height = h_header) %>%
    flextable::height_all(part = "body", height = h_body) %>%
    flextable::set_table_properties(layout = "fixed")
  
  return(ft)
}
ft_estilo_menor <- function(x){
  ft <- if (inherits(x, "flextable")) x else flextable::flextable(as.data.frame(x))
  
  ft %>%
    bg(bg = "#0F8F7A", part = "header") %>%
    color(color = "white", part = "header") %>%
    bold(part = "header") %>%
    align(align = "center", part = "header") %>%
    valign(valign = "center", part = "all") %>%
    
    fontsize(size = 10, part = "body") %>%
    fontsize(size = 11, part = "header") %>%
    align(j = c("#","CLUES","Avance","Meta acumulada","Cumplimiento %"), align = "center", part = "body") %>%
    align(j = "Nombre de la unidad", align = "left", part = "body") %>%
    align(j = "Entidad", align = "center", part = "body") %>%
    
    border_remove() %>%
    border_outer(border = fp_border(color = "white", width = 1)) %>%
    border_inner_h(border = fp_border(color = "white", width = 0.7)) %>%
    border_inner_v(border = fp_border(color = "white", width = 0.7)) %>%
    
    colformat_num(j = "Meta acumulada", digits = 5) %>%
    colformat_num(j = "Cumplimiento %", digits = 1, suffix = "%") %>%
    
    width(j = "#", width = 0.35) %>%
    width(j = "CLUES", width = 1.05) %>%
    width(j = "Nombre de la unidad", width = 3.10) %>%
    width(j = "Entidad", width = 1.10) %>%
    width(j = "Avance", width = 0.70) %>%
    width(j = "Meta acumulada", width = 0.85) %>%
    width(j = "Cumplimiento %", width = 1.05) %>%
    
    height_all(height = 0.30, part = "body") %>%
    height(height = 0.35, part = "header") %>%
    autofit()
}

ft_resumen_avance <- function(
    ancho_tabla = 2.0,
    alto_columna = 0.32,
    avance_2024 = "1,258,822",
    avance_2025 = "1,258,822",
    avance_2026 = "1,258,822",
    pct_2024    = "3.2%",
    pct_2025    = "3.2%",
    pct_2026    = "3.2%"
) {
  
  # Helper: "2.3%" -> "2%", "5.6%" -> "6%", "3%" -> "3%"
  pct_redondeado <- function(x) {
    if (length(x) == 0 || is.na(x)) return("")
    # extrae el número (acepta 3.2, 3, 3.2%, etc.)
    num <- suppressWarnings(as.numeric(gsub(",", ".", gsub("[^0-9,\\.\\-]", "", x))))
    if (is.na(num)) return(as.character(x))
    paste0(round(num), "%")
  }
  
  df <- data.frame(
    Año   = c("Avance", "Porcentaje de avance"),
    `2024`= c(avance_2024, pct_redondeado(pct_2024)),
    `2025`= c(avance_2025, pct_redondeado(pct_2025)),
    `2026`= c(avance_2026, pct_redondeado(pct_2026)),
    check.names = FALSE
  )
  
  ft <- flextable(df)
  
  # Header (guinda, blanco)
  ft <- ft |>
    font(part = "all", fontname = "Noto Sans")
  
  ft <- ft |>
    bold(part = "header") |>
    align(part = "header", align = "center") |>
    fontsize(part = "header", size = 14) |>
    bg(part = "header", bg = "white") |>
    color(part = "header", color = "#611232")
  
  # Body base
  ft <- ft |>
    align(part = "body", align = "center") |>
    fontsize(part = "body", size = 14) |>
    padding(part = "all", padding = 4)
  
  # Fila Avance (1) guinda completo
  ft <- ft |>
    bg(i = 1, part = "body", bg = "#611232") |>
    color(i = 1, part = "body", color = "white") |>
    bold(i = 1, part = "body")
  
  # Fila porcentaje (2) gris claro
  ft <- ft |>
    bg(i = 2, part = "body", bg = "#E9ECEF") |>
    color(i = 2, part = "body", color = "black") |>
    bold(i = 2, j = 1, part = "body") |>
    align(i = 2, j = 1, part = "body", align = "left")
  
  # Bordes suaves
  ft <- ft |>
    border_remove() |>
    border_outer(part = "all", border = fp_border(color = "#C9CED6", width = 1)) |>
    border_inner_h(part = "all", border = fp_border(color = "#C9CED6", width = 1)) |>
    border_inner_v(part = "all", border = fp_border(color = "#C9CED6", width = 1))
  
  # Anchos / altos (ajusta si quieres que "llene" más el placeholder)
  ft <- ft |>
    width(j = 1, width = ancho_tabla) |>
    width(j = 2:4, width = 1.25) |>
    height_all(height = alto_columna)
  
  # Evita auto-reflow raro
  ft <- set_table_properties(ft, layout = "fixed")
  
  ft
}

obtener_total_tabla <- function(df, variable, anio_sel) {
  df %>% 
    filter(anio == anio_sel) %>% 
    pull({{ variable }}) %>% 
    sum(na.rm = TRUE)
}
# FUNCIONES DE LAS GRAFICAS PRINCIPALES -----------------------------------
grafica_avance_entidades <- function(
    df,
    col_pct = "pct_avance_entidad",
    meta_linea = meta_hoy,
    x_max = 0.25,
    breaks_by = 0.25,
    extra_derecha = 0.1,
    verde_fuerte = "#14532D",
    verde_claro  = "#87A922",
    amarillo = "#F5DD61",
    rojo     = "#B91C1C",
    gris_fondo = "#FFFFFF",
    color_meta = "#8B1E3F",
    size_pct   = 5.2,
    size_meta  = 4.6,
    size_ejes  = 13,
    size_meta_txt = 5,
    umbral_pct_fuera = 0.001,
    meta_x_nudge = 0.01,
    meta_y_nudge = 1.1
) {
  
  q1 <- meta_linea * 0.5
  q2 <- meta_linea * 0.625
  q3 <- meta_linea * 0.75
  q4 <- meta_linea * 1.00
  
  df_plot <- df %>%
    dplyr::mutate(
      pct = pmax(0, pmin(.data[[col_pct]], 1)),
      color = dplyr::case_when(
        pct <  q1 ~ "rojo",
        pct <  q2 ~ "amarillo",
        pct <  q4 ~ "verde_claro",
        TRUE      ~ "verde_fuerte"
      ),
      etiqueta_pct  = scales::percent(pct, accuracy = 1),
      etiqueta_meta = paste0("(", fmt_si(avance_total), "/", fmt_si(meta_total), ")"),
      x_pct = dplyr::if_else(
        pct <= umbral_pct_fuera,
        pmax(0.012, pct + 0.006),
        pmin(pct, x_max - 0.01)
      ),
      hjust_pct = dplyr::if_else(pct <= umbral_pct_fuera, 0, 1),
      color_pct_txt = dplyr::if_else(
        color == "verde_fuerte" & pct > umbral_pct_fuera, "white", "black"
      ),
      x_ratio = x_max + 0.01
    ) %>%
    dplyr::arrange(desc(pct)) %>%
    dplyr::mutate(entidad = factor(entidad, levels = rev(entidad)))
  
  n_ent <- length(levels(df_plot$entidad))
  
  ggplot2::ggplot(df_plot, ggplot2::aes(y = entidad)) +
    ggplot2::geom_col(ggplot2::aes(x = x_max), fill = gris_fondo, width = 0.78) +
    ggplot2::geom_col(ggplot2::aes(x = pct, fill = color), width = 0.78) +
    ggplot2::scale_fill_manual(
      values = c(rojo = rojo, amarillo = amarillo, verde_claro = verde_claro, verde_fuerte = verde_fuerte),
      guide = "none"
    ) +
    ggplot2::scale_color_identity() +
    ggplot2::geom_vline(xintercept = meta_linea, linetype = "dashed", linewidth = 1, color = color_meta) +
    ggplot2::annotate(
      "label",
      x = meta_linea + meta_x_nudge,
      y = n_ent + meta_y_nudge,
      label = paste0("Meta actual: ", scales::percent(meta_linea, accuracy = 1)),
      color = color_meta,
      fontface = "bold",
      size = size_meta_txt,
      hjust = 0,
      label.size = NA,
      fill = "white"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x_pct, label = etiqueta_pct, hjust = hjust_pct, color = color_pct_txt),
      fontface = "bold", size = size_pct, show.legend = FALSE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x_ratio, label = etiqueta_meta),
      hjust = 0, size = size_meta, color = "black"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, x_max + extra_derecha),
      breaks = seq(0, x_max, by = breaks_by),
      labels = scales::percent_format(accuracy = 1),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.02, 0.16))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = size_ejes) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = size_ejes, face = "bold"),
      axis.text.x = ggplot2::element_text(size = size_ejes),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.background   = ggplot2::element_rect(fill = NA, colour = NA),
      plot.background    = ggplot2::element_rect(fill = NA, colour = NA),
      legend.background  = ggplot2::element_rect(fill = NA, colour = NA),
      legend.box.background = ggplot2::element_rect(fill = NA, colour = NA),
      panel.border = ggplot2::element_blank()
    )
}

grafica_semanal_procedimiento <- function(
    bases_todas,
    procedimiento_sel = c("Generales", "Especialidad", "Totales",
                          "Procedimientos quirúrgicos", "Egresos"),
    anio_ref = 2026,
    meta_semanal = NULL,
    meta_texto = NULL,
    out_dir = ".",
    guardar_svg = FALSE,
    width = 12,
    height = 6,
    fondo_verde = "#EAF7EF"
) {

  procedimiento_sel <- match.arg(procedimiento_sel)

  base_grafica <- bases_todas %>%
    dplyr::filter(procedimiento == procedimiento_sel) %>%
    dplyr::rename(week = semana, consultas = total)

  # ---- Meses (labels abajo) ----
  jan1_ref <- as.Date(sprintf("%d-01-01", anio_ref))
  meses_df <- tibble::tibble(week = 1:52) %>%
    dplyr::mutate(
      fecha_inicio = jan1_ref + lubridate::days((week - 1) * 7),
      mes_num = lubridate::month(fecha_inicio)
    ) %>%
    dplyr::group_by(mes_num) %>%
    dplyr::summarise(
      week_ini = min(week),
      week_fin = max(week),
      week_mid = (week_ini + week_fin) / 2,
      .groups = "drop"
    )

  meses_es <- c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic")
  meses_df <- meses_df %>% dplyr::mutate(mes_lab = meses_es[mes_num])

  # ---- Labels fin de cada año ----
  labels_fin <- base_grafica %>%
    dplyr::filter(!is.na(consultas)) %>%
    dplyr::group_by(anio) %>%
    dplyr::slice_max(order_by = week, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  # ✅ Ajuste inteligente: solo separar si se enciman
  rng <- diff(range(base_grafica$consultas, na.rm = TRUE))
  if (!is.finite(rng) || rng == 0) rng <- 1

  min_gap <- 0.06 * rng   # separación mínima entre etiquetas (ajusta 0.03–0.06)

  # Ordena por y (de abajo hacia arriba) y empuja SOLO cuando hay choque
  labels_fin <- labels_fin %>%
    dplyr::mutate(
      x_lab = week + 0.8,
      y_lab = consultas
    ) %>%
    dplyr::arrange(y_lab) %>%
    dplyr::mutate(y_lab = {
      y <- y_lab
      if (length(y) >= 2) {
        for (i in 2:length(y)) {
          if (y[i] - y[i-1] < min_gap) {
            y[i] <- y[i-1] + min_gap
          }
        }
      }
      y
    }) %>%
    dplyr::arrange(anio)  # opcional: mantiene orden estable

  slug <- tolower(gsub("[^a-z0-9]+", "_", procedimiento_sel))
  file_svg <- file.path(out_dir, sprintf("grafica_%s_semanal_2024_2026.svg", slug))

  # ---- Plot ----
  p <- ggplot2::ggplot(
    base_grafica,
    ggplot2::aes(x = week, y = consultas, color = factor(anio), group = anio)
  )

  # 1) Fondo verde + línea de meta
  if (!is.null(meta_semanal)) {
    if (is.null(meta_texto)) {
      meta_texto <- paste0("Meta semanal: ", scales::comma(meta_semanal))
    }

    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = -Inf, xmax = Inf,
        ymin = meta_semanal, ymax = Inf,
        fill = fondo_verde,
        alpha = 1
      ) +
      ggplot2::geom_hline(
        yintercept = meta_semanal,
        color = "#C9A227",
        linewidth = 0.9
      )
  }

  # 2) Líneas por año
  p <- p + ggplot2::geom_line(linewidth = 1.3, na.rm = TRUE)

  # 3) Labels de año (NO se enciman)
  p <- p +
    ggplot2::geom_label(
      data = labels_fin,
      ggplot2::aes(x = x_lab, y = y_lab, label = as.character(anio), color = factor(anio)),
      inherit.aes = FALSE,
      hjust = 0,
      fontface = "bold",
      size = 4,
      label.size = 1,
      fill = scales::alpha("white", 0.65)
    )

  # 4) Texto de meses
  p <- p +
    ggplot2::geom_text(
      data = meses_df,
      ggplot2::aes(x = week_mid, y = -Inf, label = mes_lab),
      inherit.aes = FALSE,
      vjust = -0.6,
      fontface = "bold",
      size = 3.6,
      color = "#264653"
    )

  # 5) Texto de meta
  if (!is.null(meta_semanal)) {
    # Centrar respecto al eje X real (con tus límites)
    x_center <- mean(c(1, 53))  # porque usas limits = c(1,53)

    yrng <- diff(range(base_grafica$consultas, na.rm = TRUE))
    p <- p + ggplot2::annotate(
      "text",
      x = x_center,
      y = meta_semanal + 0.02 * yrng,
      label = meta_texto,
      hjust = 0.5,
      color = "#5A0F14",
      fontface = "bold",
      size = 4
    )
  }

  # 6) Escalas / tema
  p <- p +
    ggplot2::scale_x_continuous(
      breaks = c(1, 10, 20, 30, 40, 52),
      limits = c(1, 53),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0.22, 0.18))
    ) +
    ggplot2::scale_color_manual(
      values = c("2024" = "#8E8E8E", "2025" = "#006657", "2026" = "#7A1F2B")
    ) +
    ggplot2::labs(
      x = "Semana de consulta",
      y = paste0("Número de ", tolower(procedimiento_sel))
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "#D6E4EC", linewidth = 0.6),
      panel.grid.major.x = ggplot2::element_line(color = "#D6E4EC", linewidth = 0.4),
      legend.position = "none",
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text  = ggplot2::element_text(color = "#264653"),
      panel.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.background  = ggplot2::element_rect(fill = NA, colour = NA),
      legend.background = ggplot2::element_rect(fill = NA, colour = NA),
      legend.box.background = ggplot2::element_rect(fill = NA, colour = NA),
      plot.margin = ggplot2::margin(t = 18, r = 60, b = 30, l = 10)
    ) +
    ggplot2::coord_cartesian(clip = "off")

  # ---- Guardar ----
  if (isTRUE(guardar_svg)) {
    if (!requireNamespace("svglite", quietly = TRUE)) {
      stop("No tienes instalado `svglite`. Instala con install.packages('svglite').")
    }
    ggplot2::ggsave(
      filename = file_svg,
      plot = p,
      device = svglite::svglite,
      width = width,
      height = height,
      bg = "transparent"
    )
  }

  list(plot = p, archivo_svg = file_svg, data = base_grafica)
}

grafica_incremento_anual_apilada_limpia <- function(
    df,
    col_x = "anio",
    col_total = "valor",
    col_observado = "valor_fecha_insert",
    col_modelo = "diferencia",
    idx_base = 3,
    idx_comp = 4,
    titulo = NULL,
    subtitulo = NULL,
    anio_modelo = 2026,
    nota_pie = "",
    color_observado = "#154F45",
    color_diferencia = "#C9A227",
    color_modelo_2026 = "#8FA1B3",
    color_flecha = "#a57f2c",
    x_flecha_ini = 3.0,
    x_flecha_fin = 3.8,
    y_flecha_mult = 0.86,
    y_texto_mult = 0.90,
    offset_total_mult = 0.035
) {
  
  library(ggplot2)
  library(dplyr)
  library(scales)
  library(grid)
  
  nombre_observado   <- "Observado"
  nombre_diferencia  <- "Registro extemporáneo"
  nombre_modelo_2026 <- "Proyección 2026"
  
  df <- df %>%
    mutate(
      x = factor(as.character(.data[[col_x]]),
                 levels = as.character(.data[[col_x]])),
      total = as.numeric(.data[[col_total]]),
      observado = as.numeric(.data[[col_observado]]),
      superior = pmax(as.numeric(.data[[col_modelo]]), 0),
      componente_superior = if_else(
        as.character(.data[[col_x]]) == as.character(anio_modelo),
        nombre_modelo_2026,
        nombre_diferencia
      )
    )
  
  v1 <- df$total[idx_base]
  v2 <- df$total[idx_comp]
  pct <- round((v2 / v1 - 1) * 100)
  
  y_max <- max(df$total, na.rm = TRUE) * 1.22
  offset_total <- max(df$total, na.rm = TRUE) * offset_total_mult
  
  etiquetas_total <- df %>%
    transmute(
      x = x,
      y = total + offset_total,
      label = scales::comma(total)
    )
  
  etiquetas_observado <- df %>%
    filter(observado > 0) %>%
    transmute(
      x = x,
      y = observado / 2,
      label = scales::comma(observado)
    )
  
  ggplot(df, aes(x = x)) +
    
    # Barra base: observado
    geom_col(
      aes(y = observado, fill = nombre_observado),
      width = 0.78
    ) +
    
    # Barra superior: se dibuja encima del observado
    geom_rect(
      data = df %>%
        mutate(
          xmin = as.numeric(x) - 0.39,
          xmax = as.numeric(x) + 0.39,
          ymin = observado,
          ymax = observado + superior
        ),
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        fill = componente_superior
      ),
      inherit.aes = FALSE,
      color = NA,
      linewidth = 0.2
    ) +
    
    geom_text(
      data = etiquetas_total,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      fontface = "bold",
      size = 4.0
    ) +
    
    geom_text(
      data = etiquetas_observado,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      fontface = "bold",
      size = 3.9,
      color = "white"
    ) +
    
    annotate(
      "segment",
      x = x_flecha_ini,
      xend = x_flecha_fin,
      y = y_max * y_flecha_mult,
      yend = y_max * y_flecha_mult,
      linewidth = 1.1,
      colour = color_flecha,
      arrow = arrow(type = "closed", length = unit(0.22, "cm"))
    ) +
    
    annotate(
      "text",
      x = (x_flecha_ini + x_flecha_fin) / 2,
      y = y_max * y_texto_mult,
      label = paste0(pct, "% de incremento anual"),
      fontface = "bold",
      size = 4.3
    ) +
    
    scale_fill_manual(
      values = c(
        "Observado" = color_observado,
        "Registro extemporáneo" = color_diferencia,
        "Proyección 2026" = color_modelo_2026
      ),
      breaks = c(
        "Observado",
        "Registro extemporáneo",
        "Proyección 2026"
      ),
      name = NULL
    ) +
    
    scale_y_continuous(
      labels = comma,
      expand = expansion(mult = c(0, 0.22))
    ) +
    
    labs(
      title = titulo,
      subtitle = subtitulo,
      x = NULL,
      y = NULL,
      caption = nota_pie
    ) +
    
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      axis.text.x = element_text(face = "bold", size = 12),
      axis.text.y = element_text(size = 11),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 11),
      plot.caption = element_text(size = 9, hjust = 0, color = "gray35"),
      plot.margin = margin(10, 18, 10, 18)
    )
}

generar_graficas_productividad <- function(variable, nombre_titulo) {
  
  var <- rlang::ensym(variable)
  var_chr <- rlang::as_string(var)
  
  # -----------------------------------------------------------------------
  # META 2026 AL CORTE
  # -----------------------------------------------------------------------
  
  meta_2026 <- metas_entidad %>% 
    dplyr::summarise(meta = sum(.data[[var_chr]], na.rm = TRUE)) %>% 
    dplyr::pull(meta)
  
  meta_2026_corte <- meta_2026 *
    as.numeric(fecha_corte - as.Date("2026-01-01") + 1) / 365
  
  meta_2026_mes <- meta_2026 / 12
  
  # -----------------------------------------------------------------------
  # OBSERVADO ACUMULADO POR FECHA INSERT
  # -----------------------------------------------------------------------
  
  acumulado_fecha_insert <- df_final_resumen %>% 
    dplyr::mutate(
      anio = lubridate::year(fecha),
      anio_insert = lubridate::year(fecha_insert),
      fecha_corte_insert = lubridate::ymd(
        paste0(anio_insert, "-", format(fecha_corte, "%m-%d"))
      )
    ) %>% 
    dplyr::filter(
      anio %in% c(2023, 2024, 2025, 2026),
      !is.na(fecha_insert),
      !is.na(fecha_corte_insert),
      anio_insert == anio,
      fecha_insert <= fecha_corte_insert
    ) %>% 
    dplyr::group_by(anio) %>% 
    dplyr::summarise(
      valor_fecha_insert = sum(.data[[var_chr]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # -----------------------------------------------------------------------
  # TOTAL HISTÓRICO AL MISMO CORTE
  # -----------------------------------------------------------------------
  
  acumulado_total <- df_final_resumen %>% 
    dplyr::mutate(
      anio = lubridate::year(fecha),
      fecha_corte_anio = lubridate::ymd(
        paste0(anio, "-", format(fecha_corte, "%m-%d"))
      )
    ) %>% 
    dplyr::filter(
      anio %in% c(2023, 2024, 2025),
      fecha <= fecha_corte_anio
    ) %>% 
    dplyr::group_by(anio) %>% 
    dplyr::summarise(
      valor_total = sum(.data[[var_chr]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # -----------------------------------------------------------------------
  # BASE ACUMULADA
  # -----------------------------------------------------------------------
  
  acumulado_totales_graf <- tibble::tibble(
    anio = c(2023, 2024, 2025, 2026)
  ) %>% 
    dplyr::left_join(acumulado_fecha_insert, by = "anio") %>% 
    dplyr::left_join(acumulado_total, by = "anio") %>% 
    dplyr::mutate(
      
      valor_fecha_insert = dplyr::coalesce(valor_fecha_insert, 0),
      
      valor_total = dplyr::case_when(
        anio == 2026 ~ meta_2026_corte,
        TRUE ~ valor_total
      ),
      
      valor_total = dplyr::coalesce(valor_total, valor_fecha_insert),
      
      superior_graf = dplyr::case_when(
        anio == 2023 ~ 0,
        anio %in% c(2024, 2025) ~ valor_total - valor_fecha_insert,
        anio == 2026 ~ meta_2026_corte - valor_fecha_insert,
        TRUE ~ 0
      ),
      
      superior_graf = dplyr::if_else(
        superior_graf < 0,
        0,
        superior_graf
      ),
      
      total_graf = valor_fecha_insert + superior_graf,
      
      tipo_superior = dplyr::case_when(
        anio %in% c(2024, 2025) ~ "Registro extemporáneo",
        anio == 2026 ~ "Meta 2026",
        TRUE ~ NA_character_
      ),
      
      x = dplyr::case_when(
        anio == 2023 ~ 1,
        anio == 2024 ~ 2,
        anio == 2025 ~ 3,
        anio == 2026 ~ 4
      )
    )
  
  valor_2025_acum <- acumulado_totales_graf %>% 
    dplyr::filter(anio == 2025) %>% 
    dplyr::pull(total_graf)
  
  valor_2026_acum <- acumulado_totales_graf %>% 
    dplyr::filter(anio == 2026) %>% 
    dplyr::pull(total_graf)
  
  pct_incremento_acum <- round(
    ((valor_2026_acum / valor_2025_acum) - 1) * 100
  )
  
  y_flecha_acum <- max(
    acumulado_totales_graf$total_graf,
    na.rm = TRUE
  ) * 1.08
  
  # -----------------------------------------------------------------------
  # GRÁFICA ACUMULADA
  # -----------------------------------------------------------------------
  
  grafica_acumulado <- ggplot2::ggplot(
    acumulado_totales_graf,
    ggplot2::aes(x = x)
  ) +
    
    ggplot2::geom_col(
      ggplot2::aes(y = total_graf, fill = tipo_superior),
      width = 0.65
    ) +
    
    ggplot2::geom_col(
      ggplot2::aes(y = valor_fecha_insert, fill = "Observado"),
      width = 0.65
    ) +
    
    ggplot2::geom_text(
      ggplot2::aes(
        y = valor_fecha_insert / 2,
        label = scales::comma(round(valor_fecha_insert))
      ),
      color = "white",
      fontface = "bold",
      size = 4
    ) +
    
    ggplot2::geom_text(
      ggplot2::aes(
        y = total_graf + max(total_graf, na.rm = TRUE) * 0.03,
        label = scales::comma(round(total_graf))
      ),
      fontface = "bold",
      size = 4
    ) +
    
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 3.05,
        xend = 3.95,
        y = y_flecha_acum,
        yend = y_flecha_acum
      ),
      arrow = ggplot2::arrow(
        length = grid::unit(0.18, "cm")
      ),
      linewidth = 1.2,
      color = "#A87918"
    ) +
    
    ggplot2::annotate(
      "text",
      x = 3.5,
      y = y_flecha_acum * 1.03,
      label = paste0(
        pct_incremento_acum,
        "% de incremento anual"
      ),
      fontface = "bold",
      size = 4
    ) +
    
    ggplot2::scale_x_continuous(
      breaks = c(1, 2, 3, 4),
      labels = c("2023", "2024", "2025", "2026")
    ) +
    
    ggplot2::scale_fill_manual(
      values = c(
        "Observado" = "#154F45",
        "Registro extemporáneo" = "#C9A227",
        "Meta 2026" = "#9CAFC0"
      ),
      breaks = c(
        "Observado",
        "Registro extemporáneo",
        "Meta 2026"
      ),
      na.translate = FALSE
    ) +
    
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    
    ggplot2::theme_minimal(base_size = 14) +
    
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # -----------------------------------------------------------------------
  # OBSERVADO MES POR FECHA INSERT
  # -----------------------------------------------------------------------
  
  mes_fecha_insert <- df_final_resumen %>% 
    dplyr::mutate(
      anio = lubridate::year(fecha),
      anio_insert = lubridate::year(fecha_insert),
      
      fecha_corte_insert = dplyr::case_when(
        
        anio == 2023 ~ lubridate::ymd(
          paste0(
            "2023-",
            stringr::str_pad(mes_grafica, 2, pad = "0"),
            "-15"
          )
        ),
        
        TRUE ~ lubridate::ymd(
          paste0(
            anio_insert,
            "-",
            format(fecha_corte, "%m-%d")
          )
        )
      )
    ) %>% 
    
    dplyr::filter(
      anio %in% c(2023, 2024, 2025, 2026),
      lubridate::month(fecha) == mes_grafica,
      !is.na(fecha_insert),
      !is.na(fecha_corte_insert),
      anio_insert == anio,
      fecha_insert <= fecha_corte_insert
    ) %>% 
    
    dplyr::group_by(anio) %>% 
    
    dplyr::summarise(
      valor_fecha_insert = sum(.data[[var_chr]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # -----------------------------------------------------------------------
  # TOTAL MES HISTÓRICO
  # -----------------------------------------------------------------------
  
  mes_total <- df_final_resumen %>% 
    
    dplyr::mutate(
      
      anio = lubridate::year(fecha),
      
      fecha_corte_mes = dplyr::case_when(
        
        anio == 2023 ~ lubridate::ymd(
          paste0(
            "2023-",
            stringr::str_pad(mes_grafica, 2, pad = "0"),
            "-15"
          )
        ),
        
        TRUE ~ as.Date("2999-12-31")
      )
    ) %>% 
    
    dplyr::filter(
      anio %in% c(2023, 2024, 2025),
      lubridate::month(fecha) == mes_grafica,
      fecha <= fecha_corte_mes
    ) %>% 
    
    dplyr::group_by(anio) %>% 
    
    dplyr::summarise(
      valor_total = sum(.data[[var_chr]], na.rm = TRUE),
      .groups = "drop"
    )
  
  # -----------------------------------------------------------------------
  # BASE MES
  # -----------------------------------------------------------------------
  
  mes_graf <- tibble::tibble(
    anio = c(2023, 2024, 2025, 2026)
  ) %>% 
    
    dplyr::left_join(
      mes_fecha_insert,
      by = "anio"
    ) %>% 
    
    dplyr::left_join(
      mes_total,
      by = "anio"
    ) %>% 
    
    dplyr::mutate(
      
      valor_fecha_insert = dplyr::coalesce(
        valor_fecha_insert,
        0
      ),
      
      valor_total = dplyr::case_when(
        anio == 2026 ~ meta_2026_mes,
        TRUE ~ valor_total
      ),
      
      valor_total = dplyr::coalesce(
        valor_total,
        valor_fecha_insert
      ),
      
      superior_graf = dplyr::case_when(
        anio == 2023 ~ 0,
        anio %in% c(2024, 2025) ~ valor_total - valor_fecha_insert,
        anio == 2026 ~ meta_2026_mes - valor_fecha_insert,
        TRUE ~ 0
      ),
      
      superior_graf = dplyr::if_else(
        superior_graf < 0,
        0,
        superior_graf
      ),
      
      total_graf = valor_fecha_insert + superior_graf,
      
      tipo_superior = dplyr::case_when(
        anio %in% c(2024, 2025) ~ "Registro extemporáneo",
        anio == 2026 ~ "Meta 2026",
        TRUE ~ NA_character_
      ),
      
      x = dplyr::case_when(
        anio == 2023 ~ 1,
        anio == 2024 ~ 2,
        anio == 2025 ~ 3,
        anio == 2026 ~ 4
      )
    )
  
  valor_2025 <- mes_graf %>% 
    dplyr::filter(anio == 2025) %>% 
    dplyr::pull(total_graf)
  
  valor_2026 <- mes_graf %>% 
    dplyr::filter(anio == 2026) %>% 
    dplyr::pull(total_graf)
  
  pct_incremento_mes <- round(
    ((valor_2026 / valor_2025) - 1) * 100
  )
  
  y_flecha_mes <- max(
    mes_graf$total_graf,
    na.rm = TRUE
  ) * 1.08
  
  # -----------------------------------------------------------------------
  # GRÁFICA MES
  # -----------------------------------------------------------------------
  
  grafica_mes <- ggplot2::ggplot(
    mes_graf,
    ggplot2::aes(x = x)
  ) +
    
    ggplot2::geom_col(
      ggplot2::aes(y = total_graf, fill = tipo_superior),
      width = 0.65
    ) +
    
    ggplot2::geom_col(
      ggplot2::aes(y = valor_fecha_insert, fill = "Observado"),
      width = 0.65
    ) +
    
    ggplot2::geom_text(
      ggplot2::aes(
        y = valor_fecha_insert / 2,
        label = scales::comma(round(valor_fecha_insert))
      ),
      color = "white",
      fontface = "bold",
      size = 4
    ) +
    
    ggplot2::geom_text(
      ggplot2::aes(
        y = total_graf + max(total_graf, na.rm = TRUE) * 0.03,
        label = scales::comma(round(total_graf))
      ),
      fontface = "bold",
      size = 4
    ) +
    
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 3.05,
        xend = 3.95,
        y = y_flecha_mes,
        yend = y_flecha_mes
      ),
      arrow = ggplot2::arrow(
        length = grid::unit(0.18, "cm")
      ),
      linewidth = 1.2,
      color = "#A87918"
    ) +
    
    ggplot2::annotate(
      "text",
      x = 3.5,
      y = y_flecha_mes * 1.03,
      label = paste0(
        pct_incremento_mes,
        "% de incremento anual"
      ),
      fontface = "bold",
      size = 4
    ) +
    
    ggplot2::scale_x_continuous(
      breaks = c(1, 2, 3, 4),
      labels = c("2023", "2024", "2025", "2026")
    ) +
    
    ggplot2::scale_fill_manual(
      values = c(
        "Observado" = "#154F45",
        "Registro extemporáneo" = "#C9A227",
        "Meta 2026" = "#9CAFC0"
      ),
      breaks = c(
        "Observado",
        "Registro extemporáneo",
        "Meta 2026"
      ),
      na.translate = FALSE
    ) +
    
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    
    ggplot2::theme_minimal(base_size = 14) +
    
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  list(
    grafica_acumulado = grafica_acumulado,
    grafica_mes = grafica_mes,
    datos_mes = mes_graf,
    titulo_acumulado = paste0(nombre_titulo, " acumulado"),
    titulo_mes = paste0(nombre_titulo, " - ", mes_grafica_nombre),
    datos_acumulado = acumulado_totales_graf
  )
}
