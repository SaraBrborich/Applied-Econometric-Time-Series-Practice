# 📊 Simulación de Series de Tiempo - Applied Econometric Time Series

Este repositorio contiene ejercicios prácticos basados en el libro _"Applied Econometric Time Series"_ de **Walter Enders**. Se presentan simulaciones de procesos autorregresivos en **R**, con visualización de series de tiempo y análisis de convergencia.

---

## 📖 Descripción
Este notebook realiza simulaciones de series de tiempo **AR** y compara el comportamiento de trayectorias con distintos valores del parámetro $a_1$.

### **Modelo Autorregresivo AR(1)**
Las simulaciones se basan en el siguiente proceso:

$$ y_t = a_1 y_{t-1} + \varepsilon_t $$

Donde:
- $y_t$ es la serie de tiempo generada.
- $a_1$ es el coeficiente de persistencia.
- $\varepsilon_t \sim N(0, \sigma^2)$ es un ruido blanco normal con media cero y varianza $\sigma^2$.

---

### **Variación del Parámetro $a_1$**
Se presentan simulaciones con distintos valores de $a_1$, incluyendo:

- **$a_1 = 0.9$** → Proceso altamente persistente.
- **$a_1 = 0.5$** → Proceso con menor dependencia del pasado.
- **$a_1 = -0.5$** → Proceso oscilatorio.
- **$a_1 = 1$** → Paseo aleatorio sin tendencia.
- **$a_1 = 1.2$** → Proceso explosivo.
- **$a_1 = -1.2$** → Proceso oscilatorio y explosivo.

---

## 📂 Contenido del Repositorio
Este repositorio contiene los siguientes archivos:

- **`PracticeSession1.ipynb`**: Notebook con la simulación en R.
- **`README.md`**: Descripción del proyecto y ecuaciones matemáticas.

---

## 🚀 Cómo Usar este Notebook

1. **Descargar el archivo** `PracticeSession1.ipynb`.
2. **Abrirlo en Jupyter Notebook o Google Colab**.
3. **Instalar los paquetes necesarios en R** ejecutando el siguiente código:

```r
install.packages(c("tidyverse", "magrittr", "stargazer", "ggplot2", "gridExtra"))
