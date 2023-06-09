


# Validación del modelo de área

## Lectura de librerías

```r
library(tidyverse)
library(survey)
library(srvyr)
library(TeachingSampling)
library(stringr)
library(magrittr)
library(sae)
library(ggplot2)
library(emdi)
library(patchwork)
library(reshape2)
library(haven)
library(BayesSAE)
library(mice)
library(rgdal)
library(spdep)
library(tmap)
library(sp)
library(sf)
library(dplyr)
library(magrittr)
select <- dplyr::select
```

## Leer bases de datos 

El código lee varios archivos y realiza algunas transformaciones en una base de datos de estimaciones de la tasa de informalidad en diferentes dominios geográficos.

Primero, se lee un archivo Shapefile llamado `DOM.shp` que contiene la información geográfica de los polígonos de los diferentes dominios. Luego, se lee una base de datos completa de donde se extraen las estimaciones de la tasa de informalidad en los diferentes dominios.

A continuación, se lee un modelo FH (Fay-Herriot) con transformación Arcoseno, que es utilizado para estimar las tasas de informalidad en los diferentes dominios geográficos. También se leen las estimaciones del modelo FH con transformación Arcoseno y se calculan algunas medidas de error como el RMSE (Root Mean Squared Error), RRMSE (Relative Root Mean Squared Error) y la desviación estándar del error de la estimación Directa.


```r
# Shapefile
poligonos_dominio <- read_sf("../shapefiles2010/DOM.shp" )
# Base de datos completa 
base_completa <- readRDS('../Data/base_completa.Rds')
# Modelo FH con transformación Arcoseno 
fh_arcsin <-  readRDS("../Data/fh_arcsin.Rds")
# Estimaciones del modelo FH con transformación Arcoseno 
estimaciones <- readRDS('../Data/estimaciones.Rds') %>%
  mutate(rrmse_FH = sqrt(FH_MSE) / FH,
         rmse_FH = sqrt(FH_MSE),
         Direct_ee = sqrt(Direct_MSE))
```

## Análisis de residuales y efectos aleatorios 

El código muestra cómo calcular y graficar los residuos estandarizados de un modelo de FH de datos transformados con la función arcoseno.

En primer lugar, se calculan los residuos estandarizados del modelo, utilizando la fórmula (valor observado - valor predicho) / desviación estándar de los residuos. Luego, se calculan los residuos estandarizados de los efectos aleatorios del modelo, utilizando la misma fórmula pero con los efectos aleatorios en lugar de los residuos.

Finalmente, se grafican los residuos estandarizados de los datos utilizando `ggplot2`. Se muestra un gráfico de puntos de los residuos estandarizados en función del índice de observación. También se traza una línea horizontal en el valor cero para facilitar la identificación de puntos que estén lejos de cero.


```r
#--- Residuales estandarizados ---#
residuals <- fh_arcsin$model$real_residuals
std_residuals <- (residuals - mean(residuals)) / sd(residuals)
#--- Residuales estandarizados de los efectos aleatorios ---#
rand.eff <- fh_arcsin$model$random_effects
srand.eff <- (rand.eff - mean(rand.eff)) / sd(rand.eff)

#--- Gráfico de residuales estandarizados ---#
theme_set(theme_bw())
ggplot(data.frame(Residuals = fh_arcsin$model$std_real_residuals)) +
  geom_point(aes(y = Residuals, x = 1:length(Residuals))) +
  labs(y = "Residuales estandarizados", x = "") +
  geom_hline(yintercept = 0, col = "blue")
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-3-1.svg" width="672" />

### QQ plots 
Este código genera dos gráficos QQ (quantile-quantile) para evaluar si los residuos y los efectos aleatorios del modelo FH con transformación Arcoseno siguen una distribución normal.

Primero, se calculan los residuos estandarizados y los efectos aleatorios estandarizados dividiéndolos por su desviación estándar y restando su media. Luego, se crea el primer gráfico QQ utilizando `ggplot2`, que muestra una comparación de la distribución empírica de los residuos estandarizados con una distribución normal teórica. El segundo gráfico QQ realiza la misma comparación, pero para los efectos aleatorios estandarizados.

Finalmente, se utiliza el operador | para unir los dos gráficos QQ en una única visualización.

```r
p1 <- ggplot(data.frame(Residuals = std_residuals),
             aes(sample = Residuals)) +
  stat_qq() + stat_qq_line(col = "blue") +
  ggtitle("qqplot - residuales estandarizados")

p2 <- ggplot(data.frame(Residuals = srand.eff),
             aes(sample = Residuals)) +
  stat_qq() + stat_qq_line(col = "blue") +
  ggtitle("qqplot - efectos aleatorios")

p1 | p2
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-4-1.svg" width="672" />

### Densidades
Este código genera dos gráficos de densidad para los residuales estandarizados y los efectos aleatorios estandarizados del modelo FH con transformación Arcoseno. En cada gráfico se utiliza la función ggplot para crear un objeto de gráfico y se establecen los datos, el eje x y los colores de las líneas y relleno de los gráficos de densidad.

Para el gráfico de densidad de los residuales estandarizados (p3), se establece una distribución normal teórica (con la función stat_function) y se define un rango de -4 a 4 en el eje x (con xlim). El color de relleno y línea se establece como azul claro (lightblue3) con un nivel de transparencia de 0.4 (alpha).

En el gráfico de densidad de los efectos aleatorios estandarizados (p4), se establece también una distribución normal teórica y un rango de -4 a 4 en el eje x, pero se utiliza el mismo color de relleno y línea que en el gráfico de densidad de los residuales estandarizados (color[2]). Ambos gráficos se combinan utilizando el operador "|" para presentarse en la misma línea.


```r
color = c("blue", "lightblue3")

p3 <- ggplot(
  data.frame(Residuals = std_residuals),
  aes(x = Residuals),
  fill = color[2],
  color = color[2]
) +
  geom_density(fill = color[2],
               color = color[2],
               alpha = 0.4) +
  stat_function(fun = dnorm) + xlim(-4, 4) +
  ggtitle("Densidad - residuales estandarizados")

p4 <- ggplot(
  data.frame(Residuals = srand.eff),
  aes(x = Residuals),
  fill = color[2],
  color = color[2]
) +
  geom_density(fill = color[2],
               color = color[2],
               alpha = 0.4) +
  stat_function(fun = dnorm) + xlim(-4, 4) +
  ggtitle("Densidad - efectos aleatorios")

p3 | p4
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-5-1.svg" width="672" />



```r
a <- summary(fh_arcsin)
a$normality %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Skewness </th>
   <th style="text-align:right;"> Kurtosis </th>
   <th style="text-align:right;"> Shapiro_W </th>
   <th style="text-align:right;"> Shapiro_p </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Standardized_Residuals </td>
   <td style="text-align:right;"> 0.2087 </td>
   <td style="text-align:right;"> 2.7168 </td>
   <td style="text-align:right;"> 0.9890 </td>
   <td style="text-align:right;"> 0.6659 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Random_effects </td>
   <td style="text-align:right;"> 0.2712 </td>
   <td style="text-align:right;"> 2.8440 </td>
   <td style="text-align:right;"> 0.9902 </td>
   <td style="text-align:right;"> 0.7491 </td>
  </tr>
</tbody>
</table>

## Coeficiente de determinación
Este fragmento de código ajusta un modelo FH y calcula el coeficiente de determinación (R2) del modelo.

Primero, filtra las observaciones de `estimaciones` que no tienen valores nulos en la columna `Gamma`. Luego, une los datos filtrados con la base de datos completa `base_completa` mediante una operación de `inner_join()`.

A continuación, se obtienen los coeficientes del modelo ajustado `fh_arcsin` y se almacenan en una matriz llamada `Betas`. También se crea una matriz `XS` que contiene las variables independientes de la regresión para cada observación de `base_dir`.

  El código luego calcula los residuos del modelo utilizando la matriz `XS` y `Betas`. El error cuadrático medio (MSE) se estima a partir de los residuos y se almacena en `S2Beta`.

La variable `su2` contiene la varianza residual del modelo FH ajustado y se utiliza para calcular el coeficiente de determinación (`R2`) del modelo. Finalmente, el código muestra el valor de `R2` calculado.


```r
dom_obs <- estimaciones %>% filter(!is.na(Gamma)) 
base_dir <- base_completa %>% inner_join(x = dom_obs)

#--- Coeficientes del modelo ajustado ---#

Betas <-  as.matrix(fh_arcsin$model$coefficients[, 1], ncol = 1)
rownames(Betas) <- rownames(fh_arcsin$model$coefficients)
XS <- cbind(as.matrix(base_dir %>% 
                        dplyr::select(rownames(Betas))))

residuos <- XS %*% Betas - c(colMeans(XS) %*% Betas)
D <- dim(XS)[1]
q <- dim(XS)[2]
S2Beta <- sum(residuos ^ 2) / (D - 1)
su2 <- fh_arcsin$model$variance
(R2 <- 1 - (su2 / (((D - q) / (D - 1)) * su2 + S2Beta)))
```

```
## [1] 0.9927712
```

## Validaciones en relación al tamaño de la muestra 

Este código genera un gráfico que compara las estimaciones obtenidas a partir de dos métodos diferentes ("Directo" y "Fay Herriot") en función del tamaño muestral de los dominios.

Primero, se define el número de dominios como la cantidad de filas en `base_completa`. Luego, se utiliza `ggplot()` para generar el gráfico. Se utiliza `geom_line()` para trazar una línea para cada uno de los dos métodos, especificando los colores correspondientes ("Directo" en azul y "Fay Herriot" en rojo) en `scale_color_manual()`. El eje x se etiqueta con el tamaño muestral de los dominios y el eje y se etiqueta como "Formalidad".

La escala del eje x se personaliza para que los intervalos sean de 10 unidades, con las etiquetas correspondientes obtenidas de `estimaciones$n`.


```r
N_dominios <- nrow(base_completa)

ggplot(estimaciones[order(estimaciones$n), ],
       aes(x = 1:N_dominios)) +
  geom_line(aes(y = Direct, color = "Directo")) +
  geom_line(aes(y = FH, color = "Fay Herriot")) +
  labs(y = "Formalidad",
       x = "Tamaño muestral", color = "") +
  scale_x_continuous(breaks = seq(1, N_dominios, by = 10),
                     labels = estimaciones$n[order(estimaciones$n)][seq(1, N_dominios, by = 10)]) +
  scale_color_manual(values = c("Directo" = "Blue", "Fay Herriot" = "red"))
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-8-1.svg" width="672" />

Estimación directa vs Fay-Herriot

Este código crea un gráfico de dispersión con una línea de ajuste lineal para comparar las estimaciones obtenidas mediante la estimación directa y la estimación de Fay-Herriot

  -   La función `ggplot()` recibe como argumento el conjunto de datos `estimaciones` y define la variable `Direct` para el eje x y la variable `FH` para el eje y.
  -   `geom_point()` agrega los puntos correspondientes a cada par de estimaciones para ambas variables.
  -   `geom_smooth()` agrega una línea de ajuste lineal a los puntos utilizando el método "lm" (mínimos cuadrados) por defecto de `ggplot()`.
  -   `ggtitle` agrega un título al gráfico indicando la comparación de las estimaciones.


```r
ggplot(estimaciones, aes(Direct, FH)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  ggtitle("Comparación de estimaciones")
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-9-1.svg" width="672" />

### Distancias de Cook

El código calcula la contribución de cada dominio a la varianza del estimador directo en un diseño de muestreo complejo usando el método de Fay-Herriot.

Primero, se define el tamaño de la población `D` y se crea un vector `CD` de longitud `D` con valores iniciales igual a cero. Luego, se extraen los coeficientes del modelo de regresión lineal ajustado usando el método de Fay-Herriot y se almacenan en `Betas`.

  A continuación, se crea una matriz diagonal `V` con los valores de la varianza de los errores de muestreo obtenidos del modelo de Fay-Herriot ajustado. El ciclo `for` itera a través de cada dominio `i` y ajusta un modelo Fay-Herriot sin ese dominio. Luego, se calculan los coeficientes ajustados del modelo sin el dominio `i` y se almacenan en `Betas_i`. Se calcula la diferencia entre los coeficientes ajustados de `Betas` y `Betas_i` y se almacena en `betaDiff`.

Finalmente, se calcula la contribución del dominio `i` a la varianza del estimador directo multiplicando `betaDiff` por la matriz `t(XS) %*% V %*% XS` y dividiendo el resultado por `(q - 1)`, donde `XS` es la matriz de diseño, `V` es la matriz de varianza, `q` es el número de variables explicativas y `betaDiff` es la diferencia entre los coeficientes ajustados del modelo completo y el modelo sin el dominio `i`. La contribución se almacena en la posición `i` del vector `CD`.






```r
D <- dim(base_completa)[1]
CD = numeric(D)
Betas <-  as.matrix(fh_arcsin$model$coefficients[, 1], ncol = 1)
V <- diag(1 / (su2 + 1 / (4 * base_dir$n_eff_FGV)))

#--- Ciclo para DC ---#

for (i in 1:D) {
  print(i)
  
  BetaModelo <-
    fh(formula(fh_arcsin$call$fixed),
      vardir = "hat_var",
      combined_data = base_completa[-i, ] %>% data.frame(),
      domains = "id_dominio",
      method = "reml",
      transformation = "arcsin",
      backtransformation = "bc",
      eff_smpsize = "n_eff_FGV",
      MSE = FALSE,
      mse_type = "NULL"
    )
  #--- Coeficientes del modelo ajustado ---#
  
  Betas_i = as.matrix(BetaModelo$model$coefficients[, 1], ncol = 1)
  betaDiff <- Betas - Betas_i
  CD[i] = (1 / (q - 1)) * t(betaDiff) %*% (t(XS) %*% V %*% XS) %*% betaDiff
}
```
lectura de la distancias de Cook calculadas previamente 


```r
CD <- readRDS("../Data/cookDis_2021.rds")
data.frame(cookDis = CD, dominios = base_completa$DES_MUNICIPIO) %>%
filter(cookDis > 0.1) %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> cookDis </th>
   <th style="text-align:left;"> dominios </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.2207 </td>
   <td style="text-align:left;"> JIMANI </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.6132 </td>
   <td style="text-align:left;"> HIGÜEY </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.2151 </td>
   <td style="text-align:left;"> LAS TERRENAS </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1.9380 </td>
   <td style="text-align:left;"> SANTO DOMINGO NORTE </td>
  </tr>
</tbody>
</table>
Gráfico de Distancias de Cook 

```r
data.frame(cookDis = CD, dominios = base_completa$DES_MUNICIPIO) %>%
  ggplot(aes(y = cookDis, x = 1:N_dominios)) +
  geom_point(col = "blue") + 
  geom_text(aes(label = ifelse(cookDis > 0.1,
                               as.character(dominios), '')), 
            hjust = 0, vjust = 0) +
  labs(y = "Distancia de Cook", x = "Municipios")
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-12-1.svg" width="672" />

## Análisis gráfico 

###  Coeficiente de variación y RRMSE

Este código genera un gráfico de líneas comparando los coeficientes de variación (CV) del estimador directo y del estimador FH en función del tamaño muestral de los dominios. El eje x muestra el tamaño muestral y el eje y los valores de los coeficientes de variación. El gráfico tiene dos líneas: una para el CV del estimador directo y otra para el RRMSE del estimador FH. Los puntos se conectan mediante una línea recta para una mejor visualización.

El código comienza filtrando los datos de estimaciones para ordenarlos por el tamaño muestral `n`. Luego, se utiliza la función `ggplot()` para crear el objeto de la gráfica. Se especifican los datos y las variables que se utilizarán en la gráfica `(aes(x = 1:N_dominios)` y `aes(y = Direct_CV, color = "CV")` y `aes(y = rrmse_FH, color = "RRMSE"))`. Se añaden dos líneas con `geom_line()` correspondientes a cada estimador, y se establece el título del gráfico con `ggtitle()`.

Se establecen las etiquetas de los ejes x e y mediante `labs()` y se definen los valores que aparecerán en el eje x con `scale_x_continuous()`. Finalmente, se establecen los colores de las líneas con `scale_color_manual()`.


```r
ggplot(estimaciones %>% 
         arrange(n), aes(x = 1:N_dominios)) +
  geom_line(aes(y = Direct_CV, color = "CV")) +
  geom_line(aes(y = rrmse_FH, color = "RRMSE")) +
  labs(y = "Coeficientes de variación", 
       x = "Tamaño muestral", color = "") +
  scale_x_continuous(breaks = seq(1, N_dominios, by = 10),
                     labels = estimaciones$n[order(estimaciones$n)][seq(1, N_dominios, by = 10)]) +
   scale_color_manual(values = c("CV" = "Blue", "RRMSE" = "red"))
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-13-1.svg" width="672" />

### Error estándar y RRMSE


```r
ggplot(estimaciones %>% 
         arrange(n), aes(x = 1:N_dominios)) +
  geom_line(aes(y = Direct_ee, color = "Directo")) +
  geom_line(aes(y = rmse_FH, color = "Fay-Herriot")) +
  labs(y = "Error estándar", 
       x = "Tamaño muestral", color = "") +
  scale_x_continuous(breaks = seq(1, N_dominios, by = 10),
                     labels = estimaciones$n[order(estimaciones$n)][seq(1, N_dominios, by = 10)]) +
  scale_color_manual(values = c("Directo" = "Blue", "Fay-Herriot" = "red"))
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-14-1.svg" width="672" />

### Boxplot: CV Directo 


```r
estimaciones %>% 
  ggplot(aes(y =Direct_CV)) + 
  geom_boxplot()
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-15-1.svg" width="672" />

### Boxplot: CV Directo y RRMSE FH (observados)


```r
melted <- estimaciones %>% filter(!is.na(Direct_CV)) %>% 
  dplyr::select(Direct_CV, rrmse_FH) %>% 
  melt()

ggplot(melted, aes(factor(variable), value)) + 
  geom_boxplot()  +
  labs(y = "Coeficiente de variación", x = "")
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-16-1.svg" width="672" />

### Boxplot: CV Directo y RRMSE FH 


```r
melted <- estimaciones %>% 
  dplyr::select(Direct_CV, rrmse_FH) %>% 
  melt()

ggplot(melted, aes(factor(variable), value)) + 
  geom_boxplot()  +
  labs(y = "Coeficiente de variación", x = "")
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-17-1.svg" width="672" />

### Boxplot: Error estándar Directo y RMSE FH (observados)


```r
melted <- estimaciones %>% filter(!is.na(Direct_CV)) %>% 
  dplyr::select(Direct_ee, rmse_FH) %>% 
  melt()

ggplot(melted, 
       aes(factor(variable), value)) + 
  geom_boxplot() +
  labs(y = "Errores estándar", x = "")
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-18-1.svg" width="672" />

### Boxplot: Error estándar Directo y RMSE FH


```r
melted <- estimaciones %>% 
  dplyr::select(Direct_ee, rmse_FH) %>% 
  melt()

ggplot(melted, 
       aes(factor(variable), value)) + 
  geom_boxplot() +
  labs(y = "Errores estándar", x = "")
```

<img src="07-fh_emdi_files/figure-html/unnamed-chunk-19-1.svg" width="672" />

## Matriz vecinos cercanos

```r
#--- Construyendo la matriz de vecinos de una lista de polígonos ---#
lista_comvecinos <-
  spdep::poly2nb(pl = poligonos_dominio, queen = FALSE)
```

Cargando: Matriz de vecinos


```r
lista_comvecinos <-
  readRDS("../Data/lista_comvecinos.rds")

#--- Matrices de ponderaciones espaciales para matriz de vecinos ---#

W <- nb2mat(lista_comvecinos, zero.policy = T)

#--- Nombrando las filas y columnas de la matriz de ponderaciones espaciales --#

row.names(W) <- paste0(poligonos_dominio$id_dominio)
colnames(W) <- paste0(poligonos_dominio$id_dominio)

#--- Ordenando las filas y columnas de la matriz de ponderaciones espaciales --#

W <- W[order(row.names(W)),]
W <- W[, order(colnames(W))]

#----  Comunas que se encuentran en la matriz de pesos W y de estimaciones  ---#

fil <- row.names(W) %in%
  estimaciones$id_dominio[!is.na(estimaciones$FH)]
W2 <- W[fil, fil]

#--- Pruebas de correlación espacial de Moran's I y Geary's C ---#


 spatialcor.tests(estimaciones$FH[fil], W2) %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Statistics </th>
   <th style="text-align:right;"> Value </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Moran's I </td>
   <td style="text-align:right;"> 0.0992 </td>
   <td style="text-align:right;"> 0.0209 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Geary's C </td>
   <td style="text-align:right;"> 0.8567 </td>
   <td style="text-align:right;"> 0.0055 </td>
  </tr>
</tbody>
</table>

