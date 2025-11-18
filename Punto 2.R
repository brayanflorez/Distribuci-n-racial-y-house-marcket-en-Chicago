######################################################################
#  Taller 2: Economía Urbana
#  Punto 2: Distribución Racial en Chicago
#  Estudiantes: David Florez, Daniel Hernandez
#####################################################################

rm(list = ls()) #limpiamos el entorno
require("pacman") 

p_load(
 sf,  dplyr, ggplot2, scales, patchwork, tidyr, haven, cowplot, fixest,
 stargazer, tidyverse, tmap, corrplot, kableExtra)

######################
# RUTAS DE TRABAJO
######################

# Ruta principal
workfile <- "D:/OneDrive - Oficina de Normalización Previsional/Daniel/03. Msc Economía/01. Uniandes/05. Clases/02. Segundo Semestre/02. Economía Urbana/98. Talleres/02. Taller 02"

# Subrutas
pro <- file.path(workfile, "01. Programas")
bds <- file.path(workfile, "02. Bases")
res <- file.path(workfile, "03. Resultados")

#################################
# 1. Carga de datos
#################################

# Datos censo
datos_geo <- st_read(file.path(bds,"Boundaries - Census Tracts - 2010/geo_export_a4ade1ed-743c-4dd8-ad1a-89b46b222cec.shp"))

# Datos poblaciones
datos_pob <- read_dta(file.path(bds,"Combined_data_Panel.dta"))

#Inspeccionar base de datos
glimpse(datos_geo);glimpse(datos_pob)
identical(datos_pob$FIPS, datos_pob$cod_census_track) # son lo mismo
table(datos_geo$countyfp10,datos_geo$statefp10)
table(datos_pob$State,datos_pob$County)
summary(datos_geo);summary(datos_pob)
st_crs(datos_geo) #sistema coordenadas
#plot(datos_geo)

#################################
# Ejercicio 2.1
#################################

vars <- c("White_Pop", "Black_Pop", "Hispanic_Pop", "Asian_Pop", 
          "Native_Pop", "Islander_Pop", "Mix_Pop")

for (v in vars) {
  datos_pob[[paste0("prop_", v)]] <- datos_pob[[v]]/datos_pob$Total_Pop
}

#cruce de información
workfile<-left_join(datos_geo,datos_pob,by = c("geoid10" = "cod_census_track"), keep = FALSE)

#verificando: contador por grupo geoid10
workfile$dup <- ave(rep(1, nrow(workfile)), workfile$geoid10, FUN = length)

#ordenar base de datos
workfile <- workfile[order(workfile$geoid10, workfile$year), ]

# Años de interés
years_interest <- c(2000, 2015, 2020)

# Listas para almacenar gráficos
mapas_blancos <- list()
mapas_negros <- list()
mapas_hispanos <- list()

# Paletas de colores (definidas una vez)
paleta_white <- c("#deebf7", "#9ecae1", "#4292c6", "#2171b5", "#084594")
paleta_black <- c("#fee5d9", "#fcae91", "#fb6a4a", "#de2d26", "#a50f15")
paleta_hispa <- c("#fff7bc", "#fee391", "#fec44f", "#fe9929", "#d95f0e")

# Definir breaks y labels
breaks_vals <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
labels_vals <- c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%")

# Bucle principal por año
for (year_val in years_interest) {
  
  # Filtrar datos para el año actual
  workfile_year <- workfile %>% 
    filter(year == year_val)
  
  # Crear categorías para las variables
  workfile_year <- workfile_year %>%
    mutate(
      cat_white = cut(prop_White_Pop, breaks = breaks_vals,
                      labels = labels_vals, include.lowest = TRUE),
      cat_black = cut(prop_Black_Pop, breaks = breaks_vals,
                      labels = labels_vals, include.lowest = TRUE),
      cat_hispa = cut(prop_Hispanic_Pop, breaks = breaks_vals,
                      labels = labels_vals, include.lowest = TRUE)
    )
  
  # Función MEJORADA para crear mapas sin leyenda individual
  crear_mapa_sin_leyenda <- function(data, variable, titulo, paleta) {
    ggplot(data) +
      geom_sf(aes(fill = {{variable}}), color = "gray20", size = 0.02) + # Contornos más finos
      scale_fill_manual(
        values = paleta,
        drop = FALSE,
        na.value = "gray90"
      ) +
      theme_void() +
      ggtitle(titulo) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                                  margin = margin(b = 5, t = 5)),
        legend.position = "none",  # ELIMINAR LEYENDA INDIVIDUAL
        panel.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(2, 2, 2, 2)  # Márgenes mínimos
      )
  }
  
  # Crear mapas SIN leyenda individual
  mapas_blancos[[as.character(year_val)]] <- crear_mapa_sin_leyenda(
    workfile_year, cat_white, year_val, paleta_white
  )
  
  mapas_negros[[as.character(year_val)]] <- crear_mapa_sin_leyenda(
    workfile_year, cat_black, year_val, paleta_black
  )
  
  mapas_hispanos[[as.character(year_val)]] <- crear_mapa_sin_leyenda(
    workfile_year, cat_hispa, year_val, paleta_hispa
  )
  
  cat("Mapas generados para el año:", year_val, "\n")
}

# ===============================================================
# COMBINAR MAPAS POR GRUPO ÉTNICO (evolución temporal)
# ===============================================================

library(patchwork)

# Función MODIFICADA para crear panel SIN TÍTULO
crear_panel_leyenda_unica <- function(lista_mapas, paleta) {
  
  # Crear mapa temporal solo para extraer la leyenda
  mapa_leyenda <- ggplot(workfile_year) +
    geom_sf(aes(fill = cat_white), color = NA) +  # Variable dummy
    scale_fill_manual(
      values = paleta,
      name = "Proporción",
      drop = FALSE,
      na.value = "gray90",
      guide = guide_legend(
        keyheight = unit(0.4, "cm"),  # REDUCIDO
        keywidth = unit(0.4, "cm"),   # REDUCIDO
        title.position = "top",
        nrow = 1,  # Leyenda horizontal para ahorrar espacio
        title.hjust = 0.5
      )
    ) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_text(size = 9, face = "bold"),  # REDUCIDO
      legend.text = element_text(size = 8)  # REDUCIDO
    )
  
  # Extraer solo la leyenda
  leyenda <- cowplot::get_legend(mapa_leyenda)
  
  # Crear panel sin leyendas individuales
  panel_mapa <- wrap_plots(
    lista_mapas[["2000"]],
    lista_mapas[["2015"]], 
    lista_mapas[["2020"]],
    ncol = 3,
    widths = c(1, 1, 1)
  ) 
  
  # Combinar mapas + leyenda (SIN TÍTULO)
  diseño_final <- wrap_plots(
    panel_mapa,
    leyenda,
    ncol = 1,
    heights = c(12, 1)  # MÁS ESPACIO para mapas (92%), menos para leyenda (8%)
  ) +
    plot_annotation(
      # TÍTULO ELIMINADO - se pondrá como caption en Overleaf
      theme = theme(
        plot.background = element_rect(fill = "white", color = NA)
      )
    )
  
  return(diseño_final)
}

# Crear paneles SIN TÍTULO
panel_blancos <- crear_panel_leyenda_unica(mapas_blancos, paleta_white)
panel_negros <- crear_panel_leyenda_unica(mapas_negros, paleta_black)
panel_hispanos <- crear_panel_leyenda_unica(mapas_hispanos, paleta_hispa)

# Mostrar paneles
print(panel_blancos)
print(panel_negros)
print(panel_hispanos)

#======================
# Guardar base graficos
#======================

# Configuración MEJORADA para Overleaf
config_overleaf <- list(
  ancho = 20,    # cm - MÁS ANCHO para aprovechar espacio
  alto = 10,     # cm - REDUCIDO porque no hay título
  dpi = 600
)

# Función optimizada para guardar
guardar_optimizado <- function(plot_obj, filename) {
  ggsave(
    filename = file.path(res, filename),
    plot = plot_obj,
    width = config_overleaf$ancho,
    height = config_overleaf$alto,
    units = "cm",
    device = cairo_pdf,
    dpi = config_overleaf$dpi,
    bg = "white"
  )
  cat("✅ Archivo guardado:", filename, "\n")
}

# Guardar todos los paneles
guardar_optimizado(panel_blancos, "pob_blanca.pdf")
guardar_optimizado(panel_negros, "pob_afro.pdf")
guardar_optimizado(panel_hispanos, "pob_hisp.pdf")

###DISTRIBUCION DE INGRESOS

hist(workfile$Median_Inc)

graph_black<-ggplot(workfile, aes(x = prop_Black_Pop, y = workfile$Median_Inc)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~year) +
  labs(x = "Proporción Afroamericanos",
       y = "Ingreso Mediano") +
  scale_y_continuous(labels = dollar)

ggsave(file.path(res,"grafico_black_inc.pdf") , plot = graph_black, width = 12, height = 8,
       units = "in", dpi = 300)
#Relación: Proporción Blancos vs Ingreso Mediano

graph_white<-ggplot(workfile, aes(x = prop_White_Pop, y = workfile$Median_Inc)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~year) +
  labs(x = "Proporción Blancos", y = "Ingreso Mediano") +
  scale_y_continuous(labels = dollar)

ggsave(file.path(res,"grafico_white_inc.pdf"), plot=graph_white, width = 12, height = 8,
       units = "in", dpi = 300)

graph_hispa<-ggplot(workfile, aes(x = prop_Hispanic_Pop, y = workfile$Median_Inc)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~year) +
  labs(title = "Relación: Proporción Hispanos vs Ingreso Mediano",
       x = "Proporción Hispanos",
       y = "Ingreso Mediano") +
  scale_y_continuous(labels = dollar)

#################################
# Ejercicio 2.2 - Segregación
#################################

# Filtrar años únicos para el análisis
years_analisis <- unique(workfile$year)
resultados_segregacion <- data.frame()

# Calcular índices para cada año
for (year_val in years_analisis) {
  
  # Filtrar datos para el año actual
  datos_ano <- workfile %>% filter(year == year_val)
  
  # Calcular totales poblacionales a nivel ciudad
  total_white <- sum(datos_ano$White_Pop, na.rm = TRUE)
  total_black <- sum(datos_ano$Black_Pop, na.rm = TRUE)
  total_hispanic <- sum(datos_ano$Hispanic_Pop, na.rm = TRUE)
  
  # ======================
  # ÍNDICE DE DISIMILITUD
  # ======================
  
  # Afroamericanos vs Blancos
  datos_temp_bw <- datos_ano %>%
    mutate(
      prop_black_tract = Black_Pop / total_black,
      prop_white_tract = White_Pop / total_white,
      diff_abs = abs(prop_black_tract - prop_white_tract)
    )
  
  dissimilarity_bw <- 0.5 * sum(datos_temp_bw$diff_abs, na.rm = TRUE)
  
  # Hispanos vs Blancos
  datos_temp_hw <- datos_ano %>%
    mutate(
      prop_hispanic_tract = Hispanic_Pop / total_hispanic,
      prop_white_tract = White_Pop / total_white,
      diff_abs = abs(prop_hispanic_tract - prop_white_tract)
    )
  
  dissimilarity_hw <- 0.5 * sum(datos_temp_hw$diff_abs, na.rm = TRUE)
  
  # ====================
  # ÍNDICE DE AISLAMIENTO
  # ====================
  
  # Para Afroamericanos
  datos_temp_ib <- datos_ano %>%
    mutate(
      peso_black = Black_Pop / total_black,
      proporcion_black_local = Black_Pop / Total_Pop
    )
  
  isolation_black <- sum(datos_temp_ib$peso_black * datos_temp_ib$proporcion_black_local, na.rm = TRUE)
  
  # Para Hispanos
  datos_temp_ih <- datos_ano %>%
    mutate(
      peso_hispanic = Hispanic_Pop / total_hispanic,
      proporcion_hispanic_local = Hispanic_Pop / Total_Pop
    )
  
  isolation_hispanic <- sum(datos_temp_ih$peso_hispanic * datos_temp_ih$proporcion_hispanic_local, na.rm = TRUE)
  
  # Almacenar resultados
  resultados_segregacion <- rbind(resultados_segregacion, data.frame(
    year = year_val,
    dissimilarity_black_white = dissimilarity_bw,
    dissimilarity_hispanic_white = dissimilarity_hw,
    isolation_black = isolation_black,
    isolation_hispanic = isolation_hispanic
  ))
}

# Mostrar resultados
print("RESULTADOS DE ÍNDICES DE SEGREGACIÓN:")
print(resultados_segregacion)

#Tabla overleaf

# Datos proporcionados
resultados_segregacion <- data.frame(
  year = c(2000, 2015, 2020),
  dissimilarity_black_white = c(0.8546048, 0.8256911, 0.8178321),
  dissimilarity_hispanic_white = c(0.5919528, 0.6061684, 0.5941507),
  isolation_black = c(0.8375966, 0.7832517, 0.7536916),
  isolation_hispanic = c(0.5901238, 0.6102414, 0.5829960)
)

# Redondear valores a 3 decimales
resultados_segregacion[, -1] <- round(resultados_segregacion[, -1], 3)

# Crear tabla LaTeX con formato académico
tabla_latex <- resultados_segregacion %>%
  rename(
    Año = year,
    `Disimilitud\\newline(Afro-Blancos)` = dissimilarity_black_white,
    `Disimilitud\\newline(Hispano-Blancos)` = dissimilarity_hispanic_white,
    `Aislamiento\\newline(Afroamericanos)` = isolation_black,
    `Aislamiento\\newline(Hispanos)` = isolation_hispanic
  ) %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("c", "c", "c", "c", "c"),
    caption = "Evolución de los Índices de Segregación Residencial en Chicago (2000-2020)",
    label = "tab:segregacion",
    linesep = ""  # Eliminar líneas adicionales entre filas
  ) %>%
  kable_styling(
    latex_options = c("hold_position", "striped", "scale_down"),
    font_size = 9,
    stripe_color = "gray!10"
  ) %>%
  add_header_above(c(" " = 1, "Índice de Disimilitud" = 2, "Índice de Aislamiento" = 2)) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(
    general = "Nota: El índice de disimilitud mide la distribución desigual de grupos poblacionales (0 = integración perfecta, 1 = segregación completa). El índice de aislamiento mide la probabilidad de interacción intraminoría (0 = integración, 1 = aislamiento total).",
    general_title = "",
    escape = FALSE,
    threeparttable = TRUE
  )

# Mostrar código LaTeX
cat(tabla_latex)

#Interpretación de resultados

#Indice de disimilitud: Proporción de ciudadanos que necesitan mudarse 
#para que su población sea igual. 

#Indice de aislamiento: mide el grado en el que cierto grupo esta expuesto
#a tener contacto solo con ellos mismos. 

#################################
# Ejercicio 2.3 - tipping points
#################################

#Considera a tods las etnias menos la Blanca
workfile$Minotiry_Pop<-workfile$Total_Pop-workfile$White_Pop
workfile$prop_Minority <- workfile$Minotiry_Pop/workfile$Total_Pop

# Función para calcular tipping points
calcular_tipping_points <- function(datos, grupo, year_val) {
  
  datos_ano <- datos %>% filter(year == year_val)
  
  # Ordenar por proporción del grupo
  datos_ordenados <- datos_ano %>%
    arrange(!!sym(grupo)) %>%
    mutate(
      cum_poblacion = cumsum(Total_Pop),
      total_poblacion = sum(Total_Pop),
      prop_cum_poblacion = cum_poblacion / total_poblacion,
      # Identificar el tract donde se cruza el 50% de la población
      tipping_candidate = prop_cum_poblacion >= 0.5
    )
  
  # Encontrar el tipping point (primer tract que contiene el percentil 50)
  tipping_index <- which(datos_ordenados$tipping_candidate)[1]
  
  if(length(tipping_index) > 0) {
    tipping_point <- datos_ordenados[[grupo]][tipping_index]
    return(tipping_point)
  } else {
    return(NA)
  }
}

# Calcular tipping points para cada grupo y año
grupos <- c("prop_Black_Pop", "prop_Hispanic_Pop", "prop_Minority")
years_tipping <- c(2000, 2015, 2020)

resultados_tipping <- expand.grid(
  year = years_tipping,
  grupo = grupos,
  tipping_point = NA
)

for(i in 1:nrow(resultados_tipping)) {
  year_val <- resultados_tipping$year[i]
  grupo_val <- as.character(resultados_tipping$grupo[i])
  
  resultados_tipping$tipping_point[i] <- calcular_tipping_points(
    workfile, grupo_val, year_val
  )
}

# Mostrar resultados
print("TIPPING POINTS POR GRUPO Y AÑO:")
print(resultados_tipping)

#Elaborar cuadro overleaf 

#############################
# Analisis del cambio de los tipping point
#############################

# Calcular cambios para cada grupo
for(grupo in grupos) {
  datos_grupo <- resultados_tipping %>% filter(grupo == !!grupo)
  
  cambio_2000_2015 <- datos_grupo$tipping_point[2] - datos_grupo$tipping_point[1]
  cambio_2015_2020 <- datos_grupo$tipping_point[3] - datos_grupo$tipping_point[2]
  cambio_total <- datos_grupo$tipping_point[3] - datos_grupo$tipping_point[1]
  
  nombre_grupo <- case_when(
    grupo == "prop_Black_Pop" ~ "Afroamericanos",
    grupo == "prop_Hispanic_Pop" ~ "Hispanos", 
    grupo == "prop_Minority" ~ "Minorías en General"
  )
  
  cat(sprintf("\n%s:\n", nombre_grupo))
  cat(sprintf("  2000: %.3f\n", datos_grupo$tipping_point[1]))
  cat(sprintf("  2015: %.3f (cambio: %+.3f)\n", datos_grupo$tipping_point[2], cambio_2000_2015))
  cat(sprintf("  2020: %.3f (cambio: %+.3f)\n", datos_grupo$tipping_point[3], cambio_2015_2020))
  cat(sprintf("  Cambio total 2000-2020: %+.3f\n", cambio_total))
}

# Función para crear mapas de tipping points
crear_mapa_tipping <- function(datos, grupo, year_val, tipping_val, nombre_grupo) {
  
  datos_ano <- datos %>% filter(year == year_val)
  
  # Clasificar tracts según tipping point
  datos_ano <- datos_ano %>%
    mutate(
      lado_tipping = case_when(
        !!sym(grupo) < tipping_val ~ "Por debajo del tipping point",
        !!sym(grupo) >= tipping_val ~ "Por encima del tipping point",
        TRUE ~ "Sin datos"
      ),
      lado_tipping = factor(lado_tipping, 
                            levels = c("Por debajo del tipping point", 
                                       "Por encima del tipping point"))
    )
  
  # Crear mapa
  mapa <- ggplot(datos_ano) +
    geom_sf(aes(fill = lado_tipping), color = "gray40", size = 0.1) +
    scale_fill_manual(
      values = c("Por debajo del tipping point" = "#FDE725",
                 "Por encima del tipping point" = "#440154"),
      name = paste0("Tipping Point: ", round(tipping_val, 3)),
      drop = FALSE,
      na.value = "gray90"
    ) +
    labs(
      title = paste0("Distribución Geográfica - ", nombre_grupo, " (", year_val, ")"),
      subtitle = paste0("Tipping point: ", round(tipping_val, 3))
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  
  return(mapa)
}

# Generar mapas para cada grupo y año
mapas_tipping <- list()

for(year_val in years_tipping) {
  for(grupo in grupos) {
    
    nombre_grupo <- case_when(
      grupo == "prop_Black_Pop" ~ "Afroamericanos",
      grupo == "prop_Hispanic_Pop" ~ "Hispanos",
      grupo == "prop_Minority" ~ "Minorías"
    )
    
    tipping_val <- resultados_tipping %>%
      filter(year == year_val, grupo == !!grupo) %>%
      pull(tipping_point)
    
    if(length(tipping_val) > 0 && !is.na(tipping_val)) {
      mapa_nombre <- paste0("mapa_", grupo, "_", year_val)
      
      mapas_tipping[[mapa_nombre]] <- crear_mapa_tipping(
        workfile, grupo, year_val, tipping_val, nombre_grupo
      )
      
      # Guardar mapa individual
      ggsave(
        filename = file.path(res, paste0("tipping_", grupo, "_", year_val, ".pdf")),
        plot = mapas_tipping[[mapa_nombre]],
        width = 12, height = 8, units = "in", dpi = 300
      )
    }
  }
}

