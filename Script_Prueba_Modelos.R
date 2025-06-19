# Script para Probar Modelos:
rm(list = ls())

# Paquetes:
library(forecast)
library(tidyverse)
library(tseries)
library(forecast)
library(urca)
library(lmtest)
library(gridExtra)
library(seasonal)
library(readxl)
library(knitr)
library(kableExtra)
library(broom)
library(tseries)
library(gtsummary)
library(latex2exp) # Permite usar TeX() para incluir elementos LaTeX en ggplots.
library(tsoutliers) # Permite chequear outliers mediante funciones.

# Chunk 1: Cargar Serie
df <- read_excel("series_bcu.xlsx" , sheet = "cantidad_personas_deuda_vigente") %>%
  select(fecha, cantidad_clientes, tipoinstitucion) %>%
  filter(tipoinstitucion == "Santander")
cantidad_clientes_ts <- ts(df$cantidad_clientes, start = c(2018, 12), end = c(2024, 12), frequency = 12)
cantidad_clientes_ts_completa <- ts(df$cantidad_clientes, start = c(2018, 12), frequency = 12) 
# Se deja 3 observaciones para predicción.

# Chunk 2: Plot inicial de la Serie
autoplot(cantidad_clientes_ts_completa) + 
  labs(x = "Fecha", y = "Cantidad 
       de personas", title = "Serie de Cantidad de Personas con Deuda en Santander") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank()) 

# Chunk 3: Logs de la Serie
cantidad_clientes_ts_log <- log(cantidad_clientes_ts)
autoplot(log(cantidad_clientes_ts_completa)) + 
  labs(x = "Fecha", y = TeX(r"($\log($Cantidad$)$)"), title = "Serie del Logaritmo de la Cantidad de Personas
       con Deuda en Santander") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())
# No parece tener el efecto de homogeneizar la Varianza de la Serie.

# Chunk 4: Estadisticos Descriptivos
cantidad_clientes_ts %>%  
  summary() %>%  
  enframe(name = "Estadística", value = "Valor") %>%  
  kable(caption = "Estadísticas Descriptivas de la Serie de Cantidad de Personas con Deuda") 

# Chunk 5: FAC y FACP
clientes_acf <- ggAcf(cantidad_clientes_ts, lag.max = 24, type = "correlation") + 
  labs(x = "k (Rezago)", 
       y = TeX(r"($\hat{\rho_{k}}$)"), title = "Función de Autocorrelación (FAC)") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())
clientes_pacf <- ggAcf(cantidad_clientes_ts, lag.max = 24, type = "partial") + 
  labs(x = "k (Rezago)", 
       y = TeX(r"($\hat{\alpha_{k}}$)"), title = "Función de Autocorrelación Parcial (FACP)") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())
clientes_acf
clientes_pacf

# Chunk 6: Dominio de Frecuencias
# Espectro de la Serie:
Espectro_Serie <- spectrum(cantidad_clientes_ts, method = c("ar"), 
                           freq = seq(from = 0, to = 0.5, length.out = 1000), plot = FALSE) 
# Se transforma las frecuencias para el intervalo [0,\pi]:
Espectro_Serie_Tibble <- tibble(Frecuencias = Espectro_Serie$freq*pi/max(Espectro_Serie$freq),
                                Espectro = Espectro_Serie$spec)
# Plot:
Plot_Espectro_Suavizado <- ggplot(Espectro_Serie_Tibble) +
  geom_line(aes(Frecuencias, Espectro)) +
  labs(x = TeX(r"($\omega$ (Frecuencias))"), 
       y = TeX(r"($S_{X}$(\omega))"),
       title = TeX(r"(Periodograma Suavizado de la Serie)"),
       subtitle = "Método: AR(1)",
       color = "") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  guides(color = guide_legend(position = "bottom"))
Plot_Espectro_Suavizado  

# Chunk 7: Test de Raices Unitarias (DF y KPSS)
DF_Serie_Original <- ur.df(y = cantidad_clientes_ts, type = "trend", lags = 2)
plot(DF_Serie_Original)
summary(DF_Serie_Original)
KPSS_Serie_Original <- ur.kpss(y = cantidad_clientes_ts, type = "tau")
# plot(KPSS_Serie_Original)
summary(KPSS_Serie_Original)
# Se debe aplicar primera diferencia.

# Chunk 8: Primera Diferencia Regular
diff_cantidad_clientes_ts <- diff(cantidad_clientes_ts, differences = 1)
autoplot(diff_cantidad_clientes_ts) + 
  labs(x = "Fecha", y = "Cantidad de 
       Personas", 
       title = "Serie de Cantidad de Personas con Deuda en Santander",
       subtitle = "Primera Diferencia Regular") +
  geom_hline(aes(yintercept = mean(diff_cantidad_clientes_ts)), colour = "red") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())
ggseasonplot(diff_cantidad_clientes_ts) +
  labs(x = "Fecha", y = "Cantidad de 
       Personas", 
       title = "Serie de Cantidad de Personas con Deuda en Santander",
       subtitle = "Primera Diferencia Regular") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())
# Posibles Outliers, pueden tener que ser tratados.
ggsubseriesplot(diff_cantidad_clientes_ts)
# Se puede plantear la Estacionalidad Mensual (período 12) o Trimestral (período 3).

# Chunk 9:
diff_clientes_acf <- ggAcf(diff_cantidad_clientes_ts, lag.max = 24, type = "correlation") +
  labs(x = "k (Rezago)", 
       y = TeX(r"($\hat{\rho_{k}}$)"), title = "FAC de la Serie Diferenciada") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())
diff_clientes_pacf <- ggAcf(diff_cantidad_clientes_ts, lag.max = 24, type = "partial") +
  labs(x = "k (Rezago)", 
       y = TeX(r"($\hat{\alpha_{k}}$)"), title = "FACP de la Serie Diferenciada") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())
grid.arrange(diff_clientes_acf, diff_clientes_pacf)
# De observar la FAC se puede teorizar que los modelos adecuados pueden ser:
# SARIMA(3,1,0)(0,0,0)
# SARIMA(3,1,0)(0,1,1)[12]
# SARIMA(0,1,0)(3,1,0)[3]

# Chunk 10:
# Espectro de la Serie:
Espectro_Serie_Diferenciada <- spectrum(diff_cantidad_clientes_ts, method = c("ar"), 
                                        freq = seq(from = 0, to = 0.5, length.out = 1000), plot = F) 
# Se transforma las frecuencias para el intervalo [0,\pi]:
Espectro_Serie_Diferenciada_Tibble <- tibble(Frecuencias = Espectro_Serie_Diferenciada$freq*pi/max(Espectro_Serie_Diferenciada$freq),
                                             Espectro = Espectro_Serie_Diferenciada$spec)
# Plot:
Plot_Espectro_Serie_Diferenciada_Suavizado <- ggplot(Espectro_Serie_Diferenciada_Tibble) +
  geom_line(aes(Frecuencias, Espectro)) +
  labs(x = TeX(r"($\omega$ (Frecuencias))"), 
       y = TeX(r"($S_{X}$(\omega))"),
       title = TeX(r"(Periodograma Suavizado de la Serie Diferenciada)"),
       subtitle = "Método: AR(3)",
       color = "") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) +
  guides(color = guide_legend(position = "bottom"))
Plot_Espectro_Serie_Diferenciada_Suavizado  
# Son muy importantes los ciclos de período 3, como se evidencia en la FAC.

# Chunk 11: Contrastes de RU en la Primera Diferencia Regular
DF_Serie_Diferenciada <- ur.df(y = diff_cantidad_clientes_ts, type = "drift", selectlags = "BIC")
plot(DF_Serie_Diferenciada)
summary(DF_Serie_Diferenciada)
KPSS_Serie_Diferenciada <- ur.kpss(y = diff_cantidad_clientes_ts, type = "mu")
# plot(KPSS_Serie_Original)
summary(KPSS_Serie_Diferenciada)
# DF rechaza Nula, no se tiene raíz unitaria.
# KPSS no se rechaza que sea I(0).
# No hay RUs adicionales.

# Chunk 12: Diferencia Estacional en 3
diff_estacional_3 <- diff(diff_cantidad_clientes_ts, lag = 3)
autoplot(diff_estacional_3) +
  labs(x = "Fecha", y = "Cantidad de 
       Personas", 
       title = "Serie de Cantidad de Personas con Deuda en Santander",
       subtitle = "Primera Diferencia Regular y Primera Diferencia Estacional (Trimestral)") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())
# FAC y FACP
diff_estacional_acf <- ggAcf(diff_estacional_3, lag.max = 24, type = "correlation") +
  labs(x = "k (Rezago)", 
       y = TeX(r"($\hat{\rho_{k}}$)"), title = "FAC de la Serie Diferenciada (Regular y Estacional)") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())

diff_estacional_pacf <- ggAcf(diff_estacional_3, lag.max = 24, type = "partial") +
  labs(x = "k (Rezago)", 
       y = TeX(r"($\hat{\alpha_{k}}$)"), title = "FACP de la Serie Diferenciada (Regular y Estacional)") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())
grid.arrange(diff_estacional_acf, diff_estacional_pacf)
# Puede plantearse un orden 1 en la Parte Estacional, ya sea por AR o MA.

# Chunk 13: Diferencia Estacional en 12
diff_estacional_anual <- diff(diff_cantidad_clientes_ts, lag = 12)
## Gráfico de la Serie Diferenciada Estacional Anual
autoplot(diff_estacional_anual) +
  labs(x = "Fecha", y = "Cantidad de 
       Personas", 
       title = "Serie de Cantidad de Personas con Deuda en Santander",
       subtitle = "Primera Diferencia Regular y Primera Diferencia Estacional (Anual)") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())
## FAC y FACP de la Serie Diferenciada (Regular y Estacional Anual)
diff_estacional_anual_acf <- ggAcf(diff_estacional_anual, lag.max = 24, type = "correlation") +
  labs(x = "k (Rezago)", 
       y = TeX(r"($\hat{\rho_{k}}$)"), title = "FAC de la Serie Diferenciada (Regular y Estacional Anual)") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())
diff_estacional_anual_pacf <- ggAcf(diff_estacional_anual, lag.max = 24, type = "partial") +
  labs(x = "k (Rezago)", 
       y = TeX(r"($\hat{\alpha_{k}}$)"), title = "FACP de la Serie Diferenciada (Regular y Estacional Anual)") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5)) + 
  theme(panel.grid.minor = element_blank())
grid.arrange(diff_estacional_anual_acf, diff_estacional_anual_pacf)
# Se puede plantear un Orden 1 (AR o MA) en la Diferencia Estacional de Período 12.

################################################################################
# Modelos SARIMA(3,1,0)(0,0,0)
modelo_arima_310 <- Arima(y = cantidad_clientes_ts,
                          order = c(3, 1, 0),
                          method = "ML")
coeftest(modelo_arima_310) # Se fija ar1 y ar2 en 0, lo que va de la mano con la FAC.
modelo_arima_310 <- Arima(y = cantidad_clientes_ts,
                          order = c(3, 1, 0),
                          fixed = c(0, 0, NA),
                          method = "ML")
coeftest(modelo_arima_310)

# Métricas AIC, AICc y BIC:
model_arima_310_metrics <- data.frame(
  AIC = modelo_arima_310$aic,
  AICc = modelo_arima_310$aicc,
  BIC = modelo_arima_310$bic
)

# Diagnósticos:
# Primero se observa los Residuos:
residuos_arima_310 <- residuals(modelo_arima_310)
residuos_arima_310_acf <- ggAcf(residuos_arima_310, lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC de los Residuos") +
  theme_minimal()
residuos_arima_310_pacf <- ggAcf(residuos_arima_310, lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP de los Residuos") +
  theme_minimal()
grid.arrange(residuos_arima_310_acf, residuos_arima_310_pacf)
# Residuos estandarizados:
residuos_estandarizados <- residuos_arima_310/sqrt(modelo_arima_310$sigma2)
# Plot:
autoplot(residuos_estandarizados) +
  labs(x = "Año",
       y = "Residuos Estandarizados") +
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 3, color = "red") +
  geom_hline(yintercept = -3, color = "red")
# Puede haber algún Outlier.
# Ljung-Box (fitdf = p + q del Modelo ARIMA(p,d,q))
ljung_box_df <- tibble()
for(i in 3:24){
  ljung_box_df <- rbind(ljung_box_df, (Box.test(
    residuos_arima_310, lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
# Hay algunos rechazos a la Hipótesis Nula de Residuos Incorrelacionados.

# Segundo se observa la Normalidad:
# QQ-plot
ggplot(data.frame(residuos = residuos_arima_310), aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Cuantiles teóricos", y = "Cuantiles de la muestra", 
       title = "QQ-plot de los Residuos") +
  theme_minimal()
# Histograma
ggplot(data.frame(residuos = residuos_arima_310)) +
  geom_histogram(aes(x = residuos, y = ..density..), bins = 30) +
  stat_function(fun = dnorm, args = list(mean = mean(residuos_arima_310), 
                                         sd = sd(residuos_arima_310)),
                col = "red", size = 1) +
  labs(x = "Residuos", y = "Densidad", title = "Histograma de los Residuos") +
  theme_minimal()
# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result <- shapiro.test(residuos_arima_310) %>% tidy()
jarque_bera_test_result <- jarque.bera.test(residuos_arima_310) %>% tidy()
shapiro_test_result
jarque_bera_test_result
# Se rechaza la Normalidad de los Residuos.

# Esperanza Nula de los Residuos (una vez chequeada la No Autocorrelación de los mismos).
# No se hace por falla del supuesto anterior.

# Homoscedasticidad (Varianza Constante):
# FAC y FACP del Cuadrado de los Residuos.
# Ljung-Box para los Cuadrados de los Residuos.
# No se hace por falla del supuesto anterior.

################################################################################
# Intervenir Outliers:
# De los Residuos Estandarizados uno puede teorizar sobre outliers, 
#también tomar como referencia el gráfico de la serie. Sino, se tiene esta
#función:
outlier_m1 <- tso(cantidad_clientes_ts, 
                  cval = 2.5, 
                  types = c("AO", "LS", "TC"), 
                  tsmethod = "arima", 
                  args.tsmethod = list(order = c(3, 1, 0), 
                                       seasonal = c(0, 0, 0), 
                                       include.mean= FALSE)) 
puntos_raros <- outlier_m1$outliers
puntos_raros$time
puntos_raros

# Se incluye los atípicos:
AO_2019_09 <- tsoutliers::outliers(type = "AO", ind = 10)
AO_2019_09 <- tsoutliers::outliers.effects(AO_2019_09, length(cantidad_clientes_ts)) # Grafico RE.

AO_2023_10 <- tsoutliers::outliers(type = "AO", ind = 59)
AO_2023_10 <- tsoutliers::outliers.effects(AO_2023_10, length(cantidad_clientes_ts)) # Grafico RE.

AO_2023_11 <- tsoutliers::outliers(type = "AO", ind = 60)
AO_2023_11 <- tsoutliers::outliers.effects(AO_2023_11, length(cantidad_clientes_ts)) # Grafico RE.

TC_2019_12 <- tsoutliers::outliers(type = "TC", ind = 13)
TC_2019_12 <- tsoutliers::outliers.effects(TC_2019_12, length(cantidad_clientes_ts))

LS_2021_12 <- tsoutliers::outliers(type = "LS", ind = 37)
LS_2021_12 <- tsoutliers::outliers.effects(LS_2021_12, length(cantidad_clientes_ts)) # Grafico RE.

TC_2021_12 <- tsoutliers::outliers(type = "TC", ind = 37)
TC_2021_12 <- tsoutliers::outliers.effects(TC_2021_12, length(cantidad_clientes_ts)) # Grafico RE.

LS_2022_12 <- tsoutliers::outliers(type = "TC", ind = 49)
LS_2022_12 <- tsoutliers::outliers.effects(LS_2022_12, length(cantidad_clientes_ts))

TC_2023_10 <- tsoutliers::outliers(type = "TC", ind = 59)
TC_2023_10 <- tsoutliers::outliers.effects(TC_2023_10, length(cantidad_clientes_ts)) # Gráfico RE.

TC_2024_06 <- tsoutliers::outliers(type = "TC", ind = 67)
TC_2024_06 <- tsoutliers::outliers.effects(TC_2024_06, length(cantidad_clientes_ts))

LS_2024_12 <- tsoutliers::outliers(type = "TC", ind = 73)
LS_2024_12 <- tsoutliers::outliers.effects(LS_2024_12, length(cantidad_clientes_ts))

xreg <- cbind(AO_2019_09, 
              TC_2019_12, 
              #LS_2021_12#,
              TC_2021_12,
              #LS_2022_12,
              TC_2023_10#,
              #AO_2023_10,
              #AO_2023_11#,
              #TC_2024_06,
              #LS_2024_12
)
# AO_2019_09 y TC_2021_12 son respaldados también por el gráfico de la Serie Original.

# Chequeos para que no se rompa nada:
dim(xreg)
length(cantidad_clientes_ts)
dim(xreg)[2]/length(cantidad_clientes_ts)*100 # En lo posible menor a 5.

# Se reestima incluyendo atípicos:
Modelo_2 <- Arima(y = cantidad_clientes_ts, # Datos para estimar.
                  order = c(3, 1, 0), # Orden del modelo.
                  seasonal = c(0, 0, 0), # Parte estacional
                  fixed = c(0, 0, NA, 
                            NA, NA, NA, NA, NA
                            ), # 3 Parámetros AR, 3 Intervenciones.
                  lambda = NULL, 
                  method = "ML",
                  xreg = xreg,
                  biasadj = TRUE)
coeftest(Modelo_2)

# Diagnósticos:
# Primero se observa los Residuos:
residuos_modelo_2 <- residuals(Modelo_2)
residuos_modelo_2_acf <- ggAcf(residuos_modelo_2, lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC de los Residuos") +
  theme_minimal()
residuos_modelo_2_pacf <- ggAcf(residuos_modelo_2, lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP de los Residuos") +
  theme_minimal()
grid.arrange(residuos_modelo_2_acf, residuos_modelo_2_pacf)
# Residuos estandarizados:
residuos_estandarizados_modelo_2 <- residuos_modelo_2/sqrt(Modelo_2$sigma2)
# Plot:
autoplot(residuos_estandarizados_modelo_2) +
  labs(x = "Año",
       y = "Residuos Estandarizados") +
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 3, color = "red") +
  geom_hline(yintercept = -3, color = "red")
# Puede haber algún Outlier.
# Ljung-Box (fitdf = p + q del Modelo ARIMA(p,d,q))
ljung_box_df_modelo_2 <- tibble()
for(i in 3:24){
  ljung_box_df_modelo_2 <- rbind(ljung_box_df_modelo_2, (Box.test(
    residuos_modelo_2, lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
ljung_box_df_modelo_2
# Hay algunos rechazos a la Hipótesis Nula de Residuos Incorrelacionados.
# Aunque la FAC y FACP se comportan bien.

# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result_Modelo_2 <- shapiro.test(residuos_modelo_2) %>% tidy()
jarque_bera_test_result_Modelo_2 <- jarque.bera.test(residuos_modelo_2) %>% tidy()
shapiro_test_result_Modelo_2
jarque_bera_test_result_Modelo_2

################################################################################
# Modelar Estacionalidad:

# Modelos SARIMA(3,1,0)(0,1,1)[12]
Modelo_3 <- Arima(y = cantidad_clientes_ts,
                  order = c(3, 1, 0),
                  seasonal = list(order = c(3,1,0), period = 2),
                  fixed = c(
                    0, 0, NA,
                    NA, NA, NA,
                    NA, NA, NA, NA),
                  xreg = xreg,
                  method = "ML")
coeftest(Modelo_3) # Se fija ar1 y ar2 de la Estacionalidad en 0, lo que va de la mano con la FAC.

# Métricas AIC, AICc y BIC:
Metricas_Modelo_3 <- data.frame(
  AIC = Modelo_3$aic,
  AICc = Modelo_3$aicc,
  BIC = Modelo_3$bic
)

# Diagnósticos:
# Primero se observa los Residuos:
Residuos_Modelo_3 <- residuals(Modelo_3)
residuos_Modelo_3_acf <- ggAcf(Residuos_Modelo_3, lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC de los Residuos") +
  theme_minimal()
residuos_Modelo_3_pacf <- ggAcf(Residuos_Modelo_3, lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP de los Residuos") +
  theme_minimal()
grid.arrange(residuos_Modelo_3_acf, residuos_Modelo_3_pacf)
# Residuos estandarizados:
residuos_estandarizados_Modelo_3 <- Residuos_Modelo_3/sqrt(Modelo_3$sigma2)
# Plot:
autoplot(residuos_estandarizados_Modelo_3) +
  labs(x = "Año",
       y = "Residuos Estandarizados") +
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 3, color = "red") +
  geom_hline(yintercept = -3, color = "red")
# Puede haber algún Outlier.
# Ljung-Box (fitdf = p + q del Modelo ARIMA(p,d,q))
ljung_box_df_Modelo_3 <- tibble()
for(i in 3:24){
  ljung_box_df_Modelo_3 <- rbind(ljung_box_df_Modelo_3, (Box.test(
    Residuos_Modelo_3, lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
ljung_box_df_Modelo_3

# Segundo se observa la Normalidad:
# QQ-plot
ggplot(data.frame(residuos = Residuos_Modelo_3), aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Cuantiles teóricos", y = "Cuantiles de la muestra", 
       title = "QQ-plot de los Residuos") +
  theme_minimal()
# Histograma
ggplot(data.frame(residuos = Residuos_Modelo_3)) +
  geom_histogram(aes(x = residuos, y = ..density..), bins = 30) +
  stat_function(fun = dnorm, args = list(mean = mean(Residuos_Modelo_3), 
                                         sd = sd(Residuos_Modelo_3)),
                col = "red", size = 1) +
  labs(x = "Residuos", y = "Densidad", title = "Histograma de los Residuos") +
  theme_minimal()
# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result_Modelo_3 <- shapiro.test(Residuos_Modelo_3) %>% tidy()
jarque_bera_test_result_Modelo_3 <- jarque.bera.test(Residuos_Modelo_3) %>% tidy()
shapiro_test_result_Modelo_3
jarque_bera_test_result_Modelo_3
# Se rechaza la Normalidad de los Residuos.

# Esperanza Nula de los Residuos (una vez chequeada la No Autocorrelación de los mismos).
# No se hace por falla del supuesto anterior.

# Homoscedasticidad (Varianza Constante):
# FAC y FACP del Cuadrado de los Residuos.
# Ljung-Box para los Cuadrados de los Residuos.
# No se hace por falla del supuesto anterior.

################################################################################
# Predicción:
xreg_prediccion <- tibble(
  AO10 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 10), (length(cantidad_clientes_ts) + 12)),
  TC13 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 13), length(cantidad_clientes_ts) + 12),
  TC37 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 37), length(cantidad_clientes_ts) + 12),
  TC59 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 59), length(cantidad_clientes_ts) + 12)
)

Predicciones_Modelo_3 <- forecast(Modelo_3, fan = TRUE, xreg = as.matrix(xreg_prediccion[74:85,]), h = 12)

autoplot(object = Predicciones_Modelo_3) +
  labs(x = "Fecha",
       y = "Deudores",
       title = "") +
  theme_minimal()

Comparacion_Prediccion_Realidad <- tibble(
  Fecha = c(1,2,3),
  Predicha = Predicciones_Modelo_3$mean[1:3],
  Realidad = cantidad_clientes_ts_completa[(length(cantidad_clientes_ts_completa)-2):length(cantidad_clientes_ts_completa)]) %>%
  pivot_longer(cols = c(Predicha, Realidad),
               names_to = "Serie",
               values_to = "Valores")

ggplot(Comparacion_Prediccion_Realidad) +
  geom_line(aes(x = Fecha, y = Valores, color = Serie)) +
  labs(color = "Predicción")

# Errores de Predicción dentro de la Muestra:
accuracy(Modelo_3)

# Predicción afuera de la Muestra:
# Muestra de entrenamiento ("training set") hasta 2022 inclusive
train_clientes <- window(cantidad_clientes_ts_completa, end = c(2023,12))
length(train_clientes)

# Dejamos los datos de 2023 como conjunto de prueba ("test set")
test_clientes <- window(cantidad_clientes_ts_completa, start = 2024)
n <- length(test_clientes)

# Modelo 3
Modelo_3_train <- Arima(y = train_clientes,
                       order = c(3, 1, 0),
                       seasonal = list(order = c(3,1,0), period = 2),
                       fixed = c(
                         0, 0, NA,
                         NA, NA, NA,
                         NA, NA, NA, NA),
                       xreg = xreg[1:61,],
                       method = "ML")

# Predecimos fuera de la muestra (el horizonte de predicción
# será igual al largo del test set)

# Modelo 1
Pred_3_test <- forecast(Modelo_3_train, h = n, 
                        xreg = as.matrix(xreg_prediccion[62:(dim(xreg_prediccion)[1]),]))
accuracy(Pred_3_test, test_clientes)

################################################################################
# Modelos SARIMA(0,1,0)(1,1,0)[3]
AO_2023_08 <- tsoutliers::outliers(type = "AO", ind = 52)
AO_2023_08 <- tsoutliers::outliers.effects(AO_2023_08, length(cantidad_clientes_ts))

AO_2024_12 <- tsoutliers::outliers(type = "AO", ind = 73)
AO_2024_12 <- tsoutliers::outliers.effects(AO_2024_12, length(cantidad_clientes_ts))

xreg <- cbind(AO_2019_09, 
              TC_2019_12, 
              #LS_2021_12#,
              TC_2021_12,
              #LS_2022_12,
              AO_2023_08,
              #TC_2023_10#,
              AO_2023_10,
              AO_2023_11,
              #TC_2024_06,
              AO_2024_12
)
6/73*(100)

Modelo_4 <- Arima(y = cantidad_clientes_ts,
                  order = c(0, 1, 0),
                  seasonal = list(order = c(1,1,0), period = 3),
                  fixed = c(
                    #NA, NA, NA,
                    NA, NA, NA,
                    NA, NA, NA, NA, NA),
                  xreg = xreg,
                  method = "ML")
coeftest(Modelo_4)

# Métricas AIC, AICc y BIC:
Metricas_Modelo_4 <- data.frame(
  AIC = Modelo_4$aic,
  AICc = Modelo_4$aicc,
  BIC = Modelo_4$bic
)

# Diagnósticos:
# Primero se observa los Residuos:
Residuos_Modelo_4 <- residuals(Modelo_4)
residuos_Modelo_4_acf <- ggAcf(Residuos_Modelo_4, lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC de los Residuos") +
  theme_minimal()
residuos_Modelo_4_pacf <- ggAcf(Residuos_Modelo_4, lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP de los Residuos") +
  theme_minimal()
grid.arrange(residuos_Modelo_4_acf, residuos_Modelo_4_pacf)
# Residuos estandarizados:
residuos_estandarizados_Modelo_4 <- Residuos_Modelo_4/sqrt(Modelo_4$sigma2)
# Plot:
autoplot(residuos_estandarizados_Modelo_4) +
  labs(x = "Año",
       y = "Residuos Estandarizados") +
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 3, color = "red") +
  geom_hline(yintercept = -3, color = "red")
# Puede haber algún Outlier.
# Ljung-Box (fitdf = p + q del Modelo ARIMA(p,d,q))
ljung_box_df_Modelo_4 <- tibble()
for(i in 3:24){
  ljung_box_df_Modelo_4 <- rbind(ljung_box_df_Modelo_4, (Box.test(
    Residuos_Modelo_4, lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
ljung_box_df_Modelo_4

# Segundo se observa la Normalidad:
# QQ-plot
ggplot(data.frame(residuos = Residuos_Modelo_4), aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Cuantiles teóricos", y = "Cuantiles de la muestra", 
       title = "QQ-plot de los Residuos") +
  theme_minimal()
# Histograma
ggplot(data.frame(residuos = Residuos_Modelo_4)) +
  geom_histogram(aes(x = residuos, y = ..density..), bins = 30) +
  stat_function(fun = dnorm, args = list(mean = mean(Residuos_Modelo_3), 
                                         sd = sd(Residuos_Modelo_3)),
                col = "red", size = 1) +
  labs(x = "Residuos", y = "Densidad", title = "Histograma de los Residuos") +
  theme_minimal()
# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result_Modelo_4 <- shapiro.test(Residuos_Modelo_4) %>% tidy()
jarque_bera_test_result_Modelo_4 <- jarque.bera.test(Residuos_Modelo_4) %>% tidy()
shapiro_test_result_Modelo_4
jarque_bera_test_result_Modelo_4
# Se rechaza la Normalidad de los Residuos.

# Esperanza Nula de los Residuos (una vez chequeada la No Autocorrelación de los mismos).
# No se hace por falla del supuesto anterior.

# Homoscedasticidad (Varianza Constante):
# FAC y FACP del Cuadrado de los Residuos.
# Ljung-Box para los Cuadrados de los Residuos.
# No se hace por falla del supuesto anterior.