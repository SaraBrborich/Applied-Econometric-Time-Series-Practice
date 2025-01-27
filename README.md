# 📊 Simulación y Análisis de Datos en R - Applied Econometric Time Series

Este repositorio contiene ejercicios prácticos basados en el libro _"Applied Econometric Time Series"_ de **Walter Enders**. Incluye simulaciones de procesos autorregresivos, exploración y manipulación de datos, generación de series de tiempo y análisis de convergencia en **R**.

---

## 📖 Descripción
El notebook aborda una variedad de temas en **R**, desde la configuración del entorno hasta la simulación de series de tiempo, integrando conceptos clave de econometría aplicada.

### **Temas Abordados**

1. **Configuración del Entorno**:
   - Limpieza del workspace.
   - Instalación y carga de paquetes (`tidyverse`, `magrittr`, `stargazer`).
   - Configuración del directorio de trabajo.

2. **Exploración de Datos**:
   - Estadísticas descriptivas utilizando `stargazer`.
   - Tablas de frecuencia.

3. **Edición de Datos**:
   - Selección, eliminación y creación de variables.

4. **Estructuras de Control**:
   - Introducción a `for` loops para generar series de tiempo manualmente.

5. **Simulación de Series de Tiempo**:
   - Generación de datos aleatorios modificables para analizar condiciones de convergencia.
   - Simulaciones basadas en el modelo autorregresivo AR(1) y AR(2)

6. **Variación del Parámetro $a_1$**:
   - Simulaciones con valores como:
     - **$a_1 = 0.9$** → Proceso altamente persistente.
     - **$a_1 = 0.5$** → Proceso con menor dependencia del pasado.
     - **$a_1 = -0.5$** → Proceso oscilatorio.
     - **$a_1 = 1$** → Random Walk sin tendencia.
     - **$a_1 = 1.2$** → Proceso explosivo.
     - **$a_1 = -1.2$** → Proceso oscilatorio y explosivo.

7. **Análisis de Ergodicidad en la Media**:
   - Se analiza la convergencia en media de la serie generada, observando si la media muestral converge a la media teórica en distintas realizaciones de la serie de tiempo.
   - Para un proceso AR(1), la media teórica está dada por:

   $$E[y_t] = \frac{a_0}{1 - a_1}, \quad \text{si } |a_1| < 1$$


---

## 📂 Contenido del Repositorio
Este repositorio incluye los siguientes archivos:

- **`PracticeSession1.ipynb`**: Notebook con ejercicios en R.
- **`PracticeSession1.R`**: Versión en script R del notebook.

---

## 🚀 Cómo Usar este Proyecto

1. **Descargar los archivos** del repositorio.
2. **Abrir el archivo** `PracticeSession1.ipynb` en Jupyter Notebook o Google Colab, o el script `PracticeSession1.R` en un entorno de RStudio.
3. **Instalar los paquetes necesarios en R** ejecutando el siguiente código:

```r
install.packages(c("tidyverse", "magrittr", "stargazer", "ggplot2", "gridExtra"))
