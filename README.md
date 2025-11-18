# Taller 2 — Economía Urbana  
## Punto 2: Distribución Racial y Segregación en Chicago (2000–2020)

**Autores:** David Flórez • Daniel Hernández  
**Curso:** Economía Urbana — Universidad de los Andes  
**Año:** 2024–2025  

Este repositorio contiene todo el código, mapas, gráficos y análisis utilizados para estudiar la evolución espacial de la composición racial y la segregación residencial en Chicago entre 2000 y 2020. El objetivo es replicar y extender herramientas clásicas de medición de segregación urbana, utilizando datos censales georreferenciados y métodos modernos de análisis espacial en R.

---

## 📁 Contenido del repositorio

### `R/Chicago_Analysis.R`
Script principal que desarrolla todo el ejercicio del Punto 2, incluyendo:

---

## 🗺️ **1. Mapas de composición racial (2000, 2015, 2020)**

El código:

- Carga datos geoespaciales de *census tracts* (shapefile).
- Une la geometría con datos demográficos panel.
- Calcula proporciones por tracto de:
  - población blanca  
  - población afroamericana  
  - población hispana  
- Crea clasificaciones por intervalos porcentuales (0–20%, 20–40%, ...).
- Genera paneles de mapas para cada grupo racial con:
  - leyenda única por grupo,
  - diseño homogéneo tipo paper,
  - colores consistentes por grupo racial (paletas White/Black/Hispanic).

Estos mapas permiten visualizar cómo cambia la distribución racial dentro de la ciudad.

---

## ✨ **2. Relaciones raza–ingreso**

Incluye tres gráficos de dispersión (2000, 2015, 2020):

- proporción afroamericana vs ingreso mediano,  
- proporción blanca vs ingreso mediano,  
- proporción hispana vs ingreso mediano.

Cada gráfico incorpora:

- puntos por tracto,  
- una línea de tendencia (modelo lineal),  
- facetado por año.

Esto permite observar patrones persistentes de correlación entre composición racial e ingreso.

---

## 📊 **3. Índices de segregación residencial**

Se calculan dos métricas clásicas:

### **Índice de Disimilitud**
Mide qué proporción de la población tendría que reubicarse para que la distribución racial fuera uniforme entre tracts.

Se calcula para:

- Afroamericanos vs blancos  
- Hispanos vs blancos  

### **Índice de Aislamiento**
Captura la probabilidad de que un miembro de un grupo se encuentre con personas del mismo grupo en su tracto.

Se calcula para:

- Afroamericanos  
- Hispanos  

Los resultados se presentan en una tabla en formato LaTeX lista para Overleaf.

---

## 🔄 **4. Tipping Points (puntos de inflexión)**

El script implementa un procedimiento que:

1. Ordena los tracts por proporción racial.  
2. Calcula el percentil poblacional acumulado.  
3. Identifica el tracto donde se cruza el 50% de la población total.  
4. Define el valor de proporción racial en ese tracto como *tipping point*.

Esto se calcula para:

- población afroamericana  
- población hispana  
- población minoritaria total  
- en los años 2000, 2015 y 2020  

Además, se generan mapas que muestran:

- tracts “por debajo del tipping point”,  
- tracts “por encima del tipping point”,  
- con colores contrastantes y leyenda informativa.

---

## 📂 `Resultados/`
Aquí se guardan automáticamente todos los productos gráficos:

- `pob_blanca.pdf` — mapas de población blanca  
- `pob_afro.pdf` — mapas de población afroamericana  
- `pob_hisp.pdf` — mapas de población hispana  
- `grafico_black_inc.pdf` — dispersión raza/ingreso (afroamericanos)  
- `grafico_white_inc.pdf` — dispersión raza/ingreso (blancos)  
- `tipping_prop_*.pdf` — mapas de tipping points  
- tabla LaTeX con índices de segregación  

Todos están listos para incluir en informes o papers.

---

## 🧰 Paquetes utilizados

- `sf` — análisis geoespacial  
- `tidyverse` — manipulación de datos  
- `ggplot2`, `cowplot`, `patchwork` — visualización avanzada  
- `tmap` — soporte cartográfico  
- `haven` — carga de datos Stata  
- `fixest` — regresiones y manipulación  
- `corrplot` — matrices de correlación  
- `kableExtra` — tablas LaTeX tipo paper  

---

## 🎯 Objetivo del ejercicio

Analizar cómo cambia la composición racial de Chicago a lo largo del tiempo y cuantificar el grado de segregación residencial mediante mapas, métricas formales e identificación de puntos de inflexión. El análisis combina geografía urbana, demografía, economía urbana y técnicas de visualización reproducible en R.

---

## 📄 Licencia y uso

Este repositorio es de uso académico exclusivamente.  
La reutilización para fines educativos está permitida con atribución.

