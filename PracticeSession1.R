# # Sesión Práctica 1
#
# En esta sesión exploraremos:
# - Configuración del entorno (limpieza, instalación de paquetes, carga de datos).
# - Exploración y edición de datos (`stargazer`, `tidyverse`, filtrado y manipulación).
# - Generación de series de tiempo con bucles.
# - Simulación de series de tiempo y análisis de convergencia.


# ## 1. Configuración y Manipulación de Datos
# Uno de los paquetes más usados para manipular datos es **tidyverse**. Para instalarlo y cargarlo en R, usamos:


# Instalar y cargar tidyverse
PackageNames <- c("tidyverse", "stargazer", "magrittr", "moments")
for (i in PackageNames) {
    if (!require(i, character.only = T)) {
        install.packages(i, dependencies = T)
        require(i, character.only = T)
    }
}


# ### Cargar y visualizar datos
# Ejemplo con la base de datos integrada `mtcars`:


# Primeras filas del dataset
head(mtcars)


# Estadísticas descriptivas de todas las variables
stargazer(mtcars, type = "text")


# Estadísticas descriptivas de las variables mpg y hp
stargazer(mtcars[, c("mpg", "hp")], type = "text")


# Cambiar el nombre de variables
mtcars <- rename(
    mtcars,
    mil_per_gal = mpg,
    horse_pow = hp # primero se pone el nombre nuevo y luego el antiguo
)


# Estadísticas descriptivas de las variables mpg y hp con nombres cambiados
stargazer(select(
    mtcars,
    mil_per_gal, horse_pow
), type = "text")


# ## 2. Series de Tiempo
#
# ### 2.1 Crear manualmente una serie de tiempo
# Podemos crear una serie de tiempo con la función `ts()`:


# Crear una serie de tiempo simple
datos <- c(100, 105, 110, 120, 130, 125, 140)
serie <- ts(datos, start = c(2020, 1), frequency = 12) # Datos mensuales
plot(serie, main = "Ejemplo de Serie de Tiempo", col = "blue")


# ### 2.2 Cargar una serie de tiempo Real
# Podemos cargar una serie de tiempo económica desde R:


# Cargar datos de series de tiempo de AirPassengers
data(AirPassengers)
plot(AirPassengers, main = "Número de Pasajeros Aéreos", col = "red")


# ### 2.2.1 Descomposición de la serie de tiempo
# Podemos descomponer una serie de tiempo en tendencia, estacionalidad y componente irregular:


# Descomposición de la serie AirPassengers
descomposicion <- decompose(AirPassengers)
plot(descomposicion)


# ### 2.3 Generación de series de tiempo con bucles
# Consideremos el siguiente proceso autoregresivo (Ecuación en diferencia de 1er orden):
# $$ y_t = a_0 + a_1 y_{t-1} + \varepsilon_t \tag{1}$$
#
# Sabemos que la solución está dada por:
# $$ y_t = a_0 \sum_{i=0}^{t-1} a_1^i + a_1^t y_0 + \sum_{i=0}^{t-1} a_1^i \varepsilon_{t-i} $$
#
# Donde:
# - $a_0$ es el intercepto
# - $a_1$ es el coeficiente de persistencia.
# - $\varepsilon_t \sim N(0, \sigma^2) $ es el término de error.


# Simulemos una serie de tiempo AR(1)*, es decir, la ecuación (1):


set.seed(123) # Para reproducibilidad
n <- 100 # Número de observaciones
a0 <- 1 # Intercepto
a1 <- 0.8 # Persistencia

y <- numeric(n)
epsilon <- rnorm(n, mean = 0, sd = 1) # Errores aleatorios con distribución normal

# Inicializamos la serie
y[1] <- epsilon[1]

# Generamos la serie con un loop
for (t in 2:n) {
    y[t] <- a0 + a1 * y[t - 1] + epsilon[t]
}

# Graficamos la serie generada
plot(y,
    type = "l",
    main = expression(y[t] == a[0] + a[1] * y[t - 1] + epsilon[t]),
    col = "blue",
    xlab = "Tiempo",
    ylab = "y"
)


# ### 2.3.1 Modificación de parámetros en la simulación
# Podemos cambiar el valor de $a_1 $ para ver cómo afecta la convergencia de la serie.
#
# - Si $ |a_1| < 1 $, la serie es **estacionaria** y converge a la media.
# - Si $ |a_1| > 1 $, la serie es **explosiva**.
# - Si $ a_1 = 1 $, la serie es un **random walk**.


# #### Panel (a): $y_t = 0.9 y_{t-1} + \varepsilon_t$
#
# Si $a_0 = 0$ y $a_1 = 0.9$ en la ecuación (1)
#


# Simulación de un proceso AR(1) con y_0 = 1
set.seed(123) # Para reproducibilidad
n <- 30 # Número de observaciones
a1 <- 0.9 # Parámetro autoregresivo
y <- numeric(n)
y2 <- numeric(n)
epsilon <- rnorm(n, mean = 0, sd = 0.1) # Errores aleatorios con media 0
epsilon2 <- rnorm(n, mean = 0, sd = 0)

# Inicializamos las series con y_0 = 1
y[1] <- 1
y2[1] <- 1

# Generamos la secuencia de y_t
for (t in 2:n) {
    y[t] <- a1 * y[t - 1] + epsilon[t]
    y2[t] <- a1 * y2[t - 1] + epsilon2[t]
}

# Graficamos ambas series
plot(1:n, y,
    type = "l", lwd = 2, col = "blue",
    main = expression(y[t] == 0.9 * y[t - 1] + epsilon[t]),
    xlab = "Tiempo", ylab = "yt",
    ylim = c(-0.4, 1)
)

# Segunda serie en línea roja discontinua
lines(1:n, y2, col = "red", lwd = 2, lty = 1)

# Agregar puntos a cada serie
points(1:n, y, pch = 19, col = "blue")
points(1:n, y2, pch = 19, col = "red")

# Línea de referencia en 0
abline(h = 0, col = "black", lty = 1)

# Agregar leyenda para diferenciar las series
legend("topright",
    legend = c("epsilon = 0.1", "epsilon = 0"),
    col = c("blue", "red"), lwd = 2, lty = c(1, 2)
)



# ### Panel (b): $y_t = 0.5 y_{t-1} + \varepsilon_t$
#
# Si $a_0 = 0$ y $a_1 = 0.5$ en la ecuación (1)


# Simulación de un proceso AR(1) con y_0 = 1
set.seed(123) # Para reproducibilidad
n <- 30 # Número de observaciones
a1 <- 0.5 # Parámetro autoregresivo
y <- numeric(n)
y2 <- numeric(n)

# Errores aleatorios distribuidos normalmente
epsilon <- rnorm(n, mean = 0, sd = 0.1)
epsilon2 <- rnorm(n, mean = 0, sd = 0)

# Inicializamos las series con y_0 = 1
y[1] <- 1
y2[1] <- 1

# Generamos la secuencia de y_t
for (t in 2:n) {
    y[t] <- a1 * y[t - 1] + epsilon[t]
    y2[t] <- a1 * y2[t - 1] + epsilon2[t]
}

# Graficamos ambas series
plot(1:n, y,
    type = "l", lwd = 2, col = "blue",
    main = expression(y[t] == 0.5 * y[t - 1] + epsilon[t]),
    xlab = "Tiempo", ylab = "yt",
    ylim = c(-0.4, 1)
)

# Segunda serie en línea roja
lines(1:n, y2, col = "red", lwd = 2, lty = 1)

# Agregar puntos a cada serie
points(1:n, y, pch = 19, col = "blue")
points(1:n, y2, pch = 19, col = "red")

# Línea de referencia en 0
abline(h = 0, col = "black", lty = 1)

# Agregar leyenda para diferenciar las series
legend("topright",
    legend = c("epsilon = 0.1", "epsilon = 0"),
    col = c("blue", "red"), lwd = 2, lty = c(1, 2)
)



# Ambos gráficos juntos:
#
# $y_t = 0.9 y_{t-1} + \varepsilon_t$
#
# $y_t = 0.5 y_{t-1} + \varepsilon_t$


# Comparación de series con diferentes valores de a1
a1_values <- c(0.9, 0.5)
series_list <- list()
series_list2 <- list()

for (a in a1_values) {
    y <- numeric(n)
    y2 <- numeric(n)

    y[1] <- 1
    y2[1] <- 1

    for (t in 2:n) {
        y[t] <- a * y[t - 1] + epsilon[t]
        y2[t] <- a * y2[t - 1] + epsilon2[t]
    }
    series_list[[as.character(a)]] <- y
    series_list2[[as.character(a)]] <- y2
}

# Ajustar tamaño del gráfico
options(repr.plot.width = 16, repr.plot.height = 8)

# Configurar gráficos en 1 fila y 2 columnas
par(mfrow = c(1, 2))

for (a in a1_values) {
    # Crear título
    titulo <- substitute(y[t] == a * y[t - 1] + epsilon[t], list(a = a))

    # Graficar serie 1
    plot(1:n, series_list[[as.character(a)]],
        type = "l",
        main = titulo,
        col = "blue",
        lwd = 2,
        xlab = "Tiempo",
        ylab = "y_t",
        ylim = c(-0.4, 1)
    )

    # Graficar serie 2
    lines(1:n, series_list2[[as.character(a)]], col = "red", lwd = 2, lty = 1)

    # Agregar puntos a cada serie
    points(1:n, series_list[[as.character(a)]], pch = 19, col = "blue")
    points(1:n, series_list2[[as.character(a)]], pch = 19, col = "red")

    # Línea horizontal en y = 0
    abline(h = 0, col = "gray", lty = 1, lwd = 2)

    # Agregar leyenda
    legend("topright",
        legend = c("epsilon = 0.1", "epsilon = 0"),
        col = c("blue", "red"), lwd = 2, lty = c(1, 1)
    )
}




# ### Panel (c): $y_t = -0.5 y_{t-1} + \varepsilon_t$
#
# Si $a_0 = 0$ y $a_1 = -0.5$ en la ecuación (1)


# Simulación de un proceso AR(1) con y_0 = 1
set.seed(123) # Para reproducibilidad
n <- 30 # Número de observaciones
a1 <- -0.5 # Parámetro autoregresivo
y <- numeric(n)
y2 <- numeric(n)
epsilon <- rnorm(n, mean = 0, sd = 0.1) # Errores aleatorios con media 0
epsilon2 <- rnorm(n, mean = 0, sd = 0)

# Inicializamos las series con y_0 = 1
y[1] <- 1
y2[1] <- 1

# Generamos la secuencia de y_t
for (t in 2:n) {
    y[t] <- a1 * y[t - 1] + epsilon[t]
    y2[t] <- a1 * y2[t - 1] + epsilon2[t]
}

# Ajustar tamaño del gráfico
options(repr.plot.width = 7, repr.plot.height = 6)

# Graficamos ambas series
plot(1:n, y,
    type = "l", lwd = 2, col = "blue",
    main = expression(y[t] == -0.5 * y[t - 1] + epsilon[t]),
    xlab = "Tiempo", ylab = "yt",
    ylim = c(-0.75, 1)
)

# Segunda serie en línea roja discontinua
lines(1:n, y2, col = "red", lwd = 2, lty = 1)

# Agregar puntos a cada serie
points(1:n, y, pch = 19, col = "blue")
points(1:n, y2, pch = 19, col = "red")

# Línea de referencia en 0
abline(h = 0, col = "black", lty = 1)

# Agregar leyenda para diferenciar las series
legend("topright",
    legend = c("epsilon = 0.1", "epsilon = 0"),
    col = c("blue", "red"), lwd = 2, lty = c(1, 2)
)



# ### Panel (d): $y_t = y_{t-1} + \varepsilon_t$
#
# Si $a_0 = 0$ y $a_1 = 1$ en la ecuación (1)


# Simulación de un proceso AR(1) con y_0 = 1
set.seed(123) # Para reproducibilidad
n <- 30 # Número de observaciones
a1 <- 1 # Parámetro autoregresivo
y <- numeric(n)
y2 <- numeric(n)
epsilon <- rnorm(n, mean = 0, sd = 0.1) # Errores aleatorios con media 0
epsilon2 <- rnorm(n, mean = 0, sd = 0)

# Inicializamos las series con y_0 = 1
y[1] <- 1
y2[1] <- 1

# Generamos la secuencia de y_t
for (t in 2:n) {
    y[t] <- a1 * y[t - 1] + epsilon[t]
    y2[t] <- a1 * y2[t - 1] + epsilon2[t]
}

# Ajustar tamaño del gráfico
options(repr.plot.width = 7, repr.plot.height = 6)

# Graficamos ambas series
plot(1:n, y,
    type = "l", lwd = 2, col = "blue",
    main = expression(y[t] == y[t - 1] + epsilon[t]),
    xlab = "Tiempo", ylab = "yt"
)

# Agregar puntos a cada serie
points(1:n, y, pch = 19, col = "blue")

# Línea de referencia en 0
abline(h = 0, col = "black", lty = 1)

# Agregar label
legend("topright",
    legend = c("epsilon = 0.1", "epsilon = 0"),
    col = c("blue", "red"), lwd = 2, lty = c(1, 2)
)



# ### Panel (e): $y_t = 1.2 y_{t-1} + \varepsilon_t$
#
# Si $a_0 = 0$ y $a_1 = 1.2 $ en la ecuación (1)
#
# ### Panel (f): $y_t = -1.2 y_{t-1} + \varepsilon_t$
#
# Si $a_0 = 0$ y $a_1 = -1.2 $ en la ecuación (1)


# Comparación de series con diferentes valores de a1
a1_values <- c(1.2, -1.2)
series_list <- list()
series_list2 <- list()

for (a in a1_values) {
    y <- numeric(n)
    y2 <- numeric(n)

    y[1] <- 1
    y2[1] <- 1

    for (t in 2:n) {
        y[t] <- a * y[t - 1] + epsilon[t]
        y2[t] <- a * y2[t - 1] + epsilon2[t]
    }
    series_list[[as.character(a)]] <- y
    series_list2[[as.character(a)]] <- y2
}

# Ajustar tamaño del gráfico
options(repr.plot.width = 16, repr.plot.height = 8)

# Configurar gráficos en 1 fila y 2 columnas
par(mfrow = c(1, 2))

for (a in a1_values) {
    # Crear título
    titulo <- substitute(y[t] == a * y[t - 1] + epsilon[t], list(a = a))

    # Graficar serie 1
    plot(1:n, series_list[[as.character(a)]],
        type = "l",
        main = titulo,
        col = "blue",
        lwd = 2,
        xlab = "Tiempo",
        ylab = "y_t"
    )

    # Graficar serie 2
    lines(1:n, series_list2[[as.character(a)]], col = "red", lwd = 2, lty = 1)

    # Agregar puntos a cada serie
    points(1:n, series_list[[as.character(a)]], pch = 19, col = "blue")
    points(1:n, series_list2[[as.character(a)]], pch = 19, col = "red")

    # Línea horizontal en y = 0
    abline(h = 0, col = "gray", lty = 1, lwd = 2)

    # Agregar leyenda
    legend("topright",
        legend = c("epsilon = 0.1", "epsilon = 0"),
        col = c("blue", "red"), lwd = 2, lty = c(1, 1)
    )
}
