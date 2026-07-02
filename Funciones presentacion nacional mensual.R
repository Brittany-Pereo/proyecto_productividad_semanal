library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(tibble)
library(stringr)
library(flextable)
library(officer)
library(grid)
library(rvg)
library(purrr)
library(rlang)

# ============================================================
# Colores institucionales
# ============================================================

col_verde   <- "#1E5B4F"
col_muted   <- "#6B7280"
col_borde   <- "#D1D5DB"
col_dorado  <- "#A57F2C"
col_texto   <- "#111827"
col_guinda  <- "#611232"

col_verde_pastel     <- "#00B050"
col_amarillo_chillon <- "#FFC000"
col_rojo_chillon     <- "#FF0000"

# ============================================================
# Helpers valueboxes
# ============================================================

fmt_num <- function(x){
  scales::comma(as.integer(x))
}

fmt_delta <- function(x){
  if (is.na(x)) return(list(label="s/d", col=col_muted, icon=""))
  if (x > 0) return(list(label=paste0("+",x,"%"), col=col_verde, icon="▲ "))
  if (x < 0) return(list(label=paste0(x,"%"), col=col_guinda, icon="▼ "))
  list(label="0%", col=col_muted, icon="• ")
}

elige_acento <- function(var_2025,var_2024,
                         verde=col_verde_pastel,
                         amarillo=col_amarillo_chillon,
                         rojo=col_rojo_chillon){
  
  v25_neg <- !is.na(var_2025) && var_2025 < 0
  v24_neg <- !is.na(var_2024) && var_2024 < 0
  
  if (v25_neg && v24_neg) return(rojo)
  if (xor(v25_neg,v24_neg)) return(amarillo)
  verde
}

crear_card_institucional <- function(numero,titulo,var_vs_2025,var_vs_2024,acento=col_verde){
  
  d25 <- fmt_delta(var_vs_2025)
  d24 <- fmt_delta(var_vs_2024)
  
  rvg::dml(code={
    grid::grid.newpage()
    
    grid::grid.roundrect(
      x=0.5,y=0.5,width=0.98,height=0.98,
      r=grid::unit(10,"pt"),
      gp=grid::gpar(fill="white",col=col_borde,lwd=1)
    )
    
    grid::grid.rect(
      x=0.035,y=0.5,width=0.020,height=0.90,
      gp=grid::gpar(fill=acento,col=NA)
    )
    
    grid::grid.text(
      fmt_num(numero),
      x=0.07,y=0.73,
      just=c("left","center"),
      gp=grid::gpar(col=col_dorado,fontsize=30,fontface="bold",fontfamily="Calibri")
    )
    
    grid::grid.text(
      titulo,
      x=0.07,y=0.44,
      just=c("left","center"),
      gp=grid::gpar(col=col_texto,fontsize=13,fontface="bold",fontfamily="Calibri")
    )
    
    label_25 <- paste0(d25$icon,"vs 2025 ")
    label_24 <- paste0(d24$icon,"vs 2024 ")
    
    x0 <- grid::unit(0.07,"npc")
    
    grid::grid.text(
      label_25,
      x=x0,y=grid::unit(0.22,"npc"),
      just=c("left","center"),
      gp=grid::gpar(col=col_muted,fontsize=10.5,fontfamily="Calibri")
    )
    
    grid::grid.text(
      d25$label,
      x=x0+grid::stringWidth(label_25),
      y=grid::unit(0.22,"npc"),
      just=c("left","center"),
      gp=grid::gpar(col=d25$col,fontsize=10.5,fontface="bold",fontfamily="Calibri")
    )
    
    grid::grid.text(
      label_24,
      x=x0,y=grid::unit(0.11,"npc"),
      just=c("left","center"),
      gp=grid::gpar(col=col_muted,fontsize=10.5,fontfamily="Calibri")
    )
    
    grid::grid.text(
      d24$label,
      x=x0+grid::stringWidth(label_24),
      y=grid::unit(0.11,"npc"),
      just=c("left","center"),
      gp=grid::gpar(col=d24$col,fontsize=10.5,fontface="bold",fontfamily="Calibri")
    )
  })
}

crear_valueboxes_2026 <- function(df_3anios,mapa_titulos,sufijo=""){
  
  d26 <- df_3anios %>% 
    filter(anio==2026)
  
  purrr::imap(mapa_titulos,function(titulo,var){
    crear_card_institucional(
      numero=d26 %>% pull(!!sym(var)),
      titulo=titulo,
      var_vs_2025=d26 %>% pull(!!sym(paste0("var_2026_vs_2025_",var))),
      var_vs_2024=d26 %>% pull(!!sym(paste0("var_2026_vs_2024_",var))),
      acento=elige_acento(
        d26 %>% pull(!!sym(paste0("var_2026_vs_2025_",var))),
        d26 %>% pull(!!sym(paste0("var_2026_vs_2024_",var)))
      )
    )
  }) %>% 
    rlang::set_names(paste0(names(mapa_titulos),sufijo))
}

# ============================================================
# Tablas diapo 3
# ============================================================

armar_tabla_dinamica <- function(df,indicadores,etiquetas,mes_nombre){
  
  df_base <- df %>% 
    mutate(anio=as.integer(stringr::str_extract(as.character(anio),"\\d{4}"))) %>% 
    filter(anio %in% c(2025,2026))
  
  purrr::map2_dfr(indicadores,etiquetas,function(indicador,etiqueta){
    
    valor_2025 <- df_base %>% 
      filter(anio==2025) %>% 
      summarise(valor=sum(.data[[indicador]],na.rm=TRUE)) %>% 
      pull(valor)
    
    valor_2026 <- df_base %>% 
      filter(anio==2026) %>% 
      summarise(valor=sum(.data[[indicador]],na.rm=TRUE)) %>% 
      pull(valor)
    
    crecimiento <- ifelse(
      !is.na(valor_2025) && valor_2025!=0,
      round((valor_2026-valor_2025)/valor_2025*100,0),
      NA_real_
    )
    
    tibble::tibble(
      Indicador=etiqueta,
      !!paste0("Enero-\n",stringr::str_to_sentence(mes_nombre)," 2025") := valor_2025,
      !!paste0("Enero-\n",stringr::str_to_sentence(mes_nombre)," 2026") := valor_2026,
      "Crecimiento\nanual"=paste0(ifelse(crecimiento>0,"+",""),crecimiento," %")
    )
  })
}

ft_planeacion <- function(tabla,w1=2.75,w2=0.95,w3=0.95,w4=0.95){
  flextable::flextable(tabla) %>% 
    flextable::set_table_properties(layout="fixed") %>% 
    flextable::colformat_num(j=2:3,big.mark=",",digits=0) %>% 
    flextable::align(align="center",part="all") %>% 
    flextable::align(j=1,align="left",part="all") %>% 
    flextable::bold(part="header") %>% 
    flextable::color(color="white",part="header") %>% 
    flextable::bg(bg="#333333",part="header") %>% 
    flextable::fontsize(size=7,part="all") %>% 
    flextable::fontsize(size=7,part="header") %>% 
    flextable::border_outer(border=officer::fp_border(color="#9CA3AF",width=0.6)) %>% 
    flextable::border_inner_h(border=officer::fp_border(color="#9CA3AF",width=0.5)) %>% 
    flextable::border_inner_v(border=officer::fp_border(color="#9CA3AF",width=0.5)) %>% 
    flextable::width(j=1,width=w1) %>% 
    flextable::width(j=2,width=w2) %>% 
    flextable::width(j=3,width=w3) %>% 
    flextable::width(j=4,width=w4) %>% 
    flextable::height_all(height=0.24) %>% 
    flextable::padding(padding=1,part="all") %>% 
    flextable::valign(valign="center",part="all")
}

crear_tabla_ft <- function(df,indicadores,etiquetas,mes_nombre){
  armar_tabla_dinamica(
    df=df,
    indicadores=indicadores,
    etiquetas=etiquetas,
    mes_nombre=mes_nombre
  ) %>% 
    ft_planeacion(w1=2.75,w2=0.95,w3=0.95,w4=0.95)
}

# ============================================================
# Barras diapo 3
# ============================================================

crear_grafica_real_modelo <- function(datos_consulta,reales,col_modelo,col_real,
                                      titulo,etiqueta_modelo="Meta junio"){
  
  col_modelo <- rlang::ensym(col_modelo)
  col_real   <- rlang::ensym(col_real)
  
  total_observado_2026 <- reales %>% 
    pull(!!col_real)
  
  total_modelo_2026 <- datos_consulta %>% 
    filter(anio==2026 | anio=="2026") %>% 
    pull(!!col_modelo)
  
  modelo_adicional <- total_modelo_2026-total_observado_2026
  
  df_plot <- datos_consulta %>%
    select(anio,valor=!!col_modelo) %>%
    mutate(anio=as.factor(anio),
           tipo=ifelse(anio==2026 | anio=="2026","Observado 2026","Histórico")) %>%
    filter(tipo=="Histórico") %>%
    bind_rows(
      tibble(anio=factor("2026",levels=levels(.$anio)),
             tipo="Observado 2026",
             valor=total_observado_2026),
      tibble(anio=factor("2026",levels=levels(.$anio)),
             tipo=etiqueta_modelo,
             valor=modelo_adicional)
    ) %>% 
    mutate(tipo=factor(tipo,levels=c(etiqueta_modelo,"Observado 2026","Histórico")))
  
  df_totales <- df_plot %>%
    group_by(anio) %>%
    summarise(total=sum(valor,na.rm=TRUE),.groups="drop")
  
  colores_fill <- c("Histórico"="#D9D2BE","Observado 2026"="#2F6F63")
  colores_fill[etiqueta_modelo] <- "#9F2241"
  
  ggplot(df_plot,aes(x=anio,y=valor,fill=tipo)) +
    geom_col(width=0.82) +
    geom_text(data=df_totales,
              aes(x=anio,y=total,label=scales::comma(round(total,0))),
              inherit.aes=FALSE,
              vjust=-0.35,
              fontface="bold",
              size=4) +
    geom_text(data=df_plot %>% filter(anio=="2026",tipo=="Observado 2026"),
              aes(label=scales::comma(round(valor,0))),
              position=position_stack(vjust=0.5),
              color="white",
              fontface="bold",
              size=4) +
    geom_text(data=df_plot %>% filter(anio=="2026",tipo==etiqueta_modelo) %>% left_join(df_totales,by="anio"),
              aes(x=anio,y=total,label=etiqueta_modelo),
              inherit.aes=FALSE,
              vjust=-1.6,
              color="#9F2241",
              fontface="bold",
              size=4) +
    scale_fill_manual(values=colores_fill) +
    scale_y_continuous(labels=scales::comma,expand=expansion(mult=c(0,.22))) +
    labs(title=titulo,x=NULL,y=NULL,fill=NULL) +
    theme_minimal(base_size=12) +
    theme(
      legend.position="none",
      panel.grid=element_blank(),
      axis.title=element_blank(),
      axis.text.x=element_text(face="bold",color="#6B7280"),
      axis.text.y=element_text(color="#6B7280"),
      plot.title=element_text(hjust=0.5,face="bold",color="#6B7280",size=18),
      plot.background=element_rect(fill="white",color=NA),
      panel.background=element_rect(fill="white",color=NA)
    )
}

# ============================================================
# Gráficas temporales diapos 4 y 5
# ============================================================
grafica_consultas_periodos <- function(df,fecha_inicio="2022-08-01",fecha_fin="2026-06-01",
                                       titulo="Consultas totales del IMSS Bienestar",
                                       color_linea="#6B6B6B",verde_punto="#1F5B50",
                                       fill_2223="#EFEFEF",fill_2024="#E9DDCC",
                                       fill_2025="#F4F0EA",fill_2026="#E9DDCC",
                                       fill_valuebox="#B99C6D"){
  
  df <- df %>% 
    mutate(fecha=as.Date(fecha)) %>% 
    filter(fecha>=as.Date(fecha_inicio),fecha<=as.Date(fecha_fin)) %>% 
    arrange(fecha)
  
  ymax <- max(df$consultas_totales,na.rm=TRUE)
  ymin <- min(df$consultas_totales,na.rm=TRUE)
  
  bandas <- tibble::tribble(
    ~xmin,~xmax,~fill,~label,~y_label,
    "2022-08-01","2023-12-31",fill_2223,"2022 – 2023\nAños de transición",ymax*0.92,
    "2024-01-01","2024-12-31",fill_2024,"2024\nPrimer año de operación",ymax*0.91,
    "2025-01-01","2025-12-31",fill_2025,"2025\nSegundo año\nde operación",ymax*0.94,
    "2026-01-01","2026-06-30",fill_2026,"2026\nTercer año\nde operación",ymax*0.86
  ) %>% 
    mutate(xmin=as.Date(xmin),xmax=as.Date(xmax))
  
  puntos_anuales <- df %>% 
    filter(month(fecha)==6,fecha!=max(df$fecha)) %>% 
    mutate(label=paste0(scales::comma(round(consultas_totales)),"\njunio ",year(fecha)),
           nudge_y=case_when(year(fecha)==2023~ymax*0.09,
                             year(fecha)==2024~ymax*0.10,
                             year(fecha)==2025~ymax*0.18,
                             TRUE~ymax*0.10),
           nudge_x=case_when(year(fecha)==2025~-18,
                             TRUE~0))  
  ultimo_punto <- df %>% 
    filter(fecha==max(fecha,na.rm=TRUE)) %>% 
    slice(1) %>% 
    mutate(label=paste0(scales::comma(round(consultas_totales)),"\njunio ",year(fecha)))
  
  ggplot(df,aes(x=fecha,y=consultas_totales)) +
    geom_rect(data=bandas,aes(xmin=xmin,xmax=xmax,ymin=-Inf,ymax=Inf),
              inherit.aes=FALSE,fill=bandas$fill,color=NA) +
    geom_line(color=color_linea,linewidth=1.1) +
    geom_point(color=color_linea,size=2) +
    geom_point(data=puntos_anuales,aes(x=fecha,y=consultas_totales),
               inherit.aes=FALSE,color=verde_punto,size=5.5) +
    geom_segment(data=puntos_anuales,
                 aes(x=fecha,xend=fecha+nudge_x,
                     y=consultas_totales+ymax*0.02,
                     yend=consultas_totales+nudge_y-ymax*0.015),
                 inherit.aes=FALSE,
                 linetype="dotted",linewidth=0.5,color="#111827") +
    geom_text(data=puntos_anuales,
              aes(x=fecha+nudge_x,
                  y=consultas_totales+nudge_y,
                  label=label),
              inherit.aes=FALSE,
              fontface="bold",size=3.4,lineheight=0.95) +    geom_point(data=ultimo_punto,aes(x=fecha,y=consultas_totales),
               inherit.aes=FALSE,color=verde_punto,size=5.5) +
    geom_label(data=ultimo_punto,
               aes(x=fecha,y=consultas_totales,label=label),
               inherit.aes=FALSE,
               fill=fill_valuebox,color="white",
               fontface="bold",label.size=0,size=4.1,
               label.padding=unit(0.35,"lines"),
               nudge_x=-20,nudge_y=ymax*0.18) +
    geom_text(data=bandas,
              aes(x=xmin+(xmax-xmin)/2,y=y_label,label=label),
              inherit.aes=FALSE,fontface="bold",size=3.5,lineheight=0.95) +
    annotate("segment",x=as.Date("2022-08-15"),xend=as.Date("2022-08-15"),
             y=ymin*0.95,yend=ymax*1.03,linewidth=1.1,colour=verde_punto,
             arrow=arrow(length=unit(0.25,"cm"))) +
    annotate("text",x=as.Date("2022-09-15"),y=ymin*1.08,
             label="Decreto de creación\ndel IMSS Bienestar",
             hjust=0,fontface="bold",size=3.2) +
    scale_y_continuous(labels=scales::comma,expand=expansion(mult=c(0,0.20))) +
    scale_x_date(date_breaks="1 month",date_labels="%b-%y",expand=expansion(mult=c(0,0.02))) +
    labs(title=titulo,x=NULL,y=NULL) +
    coord_cartesian(clip="off") +
    theme_minimal(base_size=12) +
    theme(panel.grid.major.x=element_blank(),
          panel.grid.minor=element_blank(),
          plot.title=element_text(face="bold",size=18),
          axis.text.x=element_text(angle=45,hjust=1),
          plot.background=element_rect(fill="white",color=NA),
          panel.background=element_rect(fill="white",color=NA))
}
