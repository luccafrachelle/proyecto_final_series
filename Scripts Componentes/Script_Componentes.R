# Script para Probar RJDemetra en el Proyecto de Series Cronológicas:
rm(list = ls())

################################################################################
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
library(rJava)
library(RJDemetra)
# ADVERTENCIA: En MacBook RJDemetra me crashea el Rstudio, puede tener que
#ver con la instalación de JAVA. En Windows 10 funciona lo más bien.

################################################################################
# Cargar Serie:
df <- read_excel("series_bcu.xlsx" , sheet = "cantidad_personas_deuda_vigente") %>%
  select(fecha, cantidad_clientes, tipoinstitucion) %>%
  filter(tipoinstitucion == "Santander")
cantidad_clientes_ts <- ts(df$cantidad_clientes, start = c(2018, 12), end = c(2024, 12), frequency = 12)
cantidad_clientes_ts_completa <- ts(df$cantidad_clientes, start = c(2018, 12), frequency = 12) 

# Se define las specs del Modelo:
spec_x13 <- x13_spec(spec = "RSA5c", # La metodología, podría explorarse otras.
                     x11.fcasts = -1, # 1 año.
                     # x11.mode = "Multiplicative", # Mantiene el modo en Aditivo por alguna razón.
                     automdl.enabled = FALSE, # Se desactiva la modelización automática.
                     easter.enabled = FALSE, # No incide pascuas. 
                     outlier.enabled = FALSE, # Los outliers ya los tenemos definidos, se desactiva su búsqueda.
                     tradingdays.option = "None", # No.
                     transform.function = "None", # No Logaritmo.
                     arima.coefEnabled = T, # Se fija los coeficientes SARIMA(2,1,0)(1,1,0).
                               arima.p = 2, 
                               arima.d = 1,
                               arima.q = 0,
                               arima.bp = 1,
                               arima.bd = 1,
                               arima.bq = 0,
                     arima.coefType = c("Fixed", "Undefined", "Undefined"), # Se fija el ar1 en 0 de la parte regular.
                     arima.coef = c(1/(2^512), NA, NA), # El 0 equivale a NA por alguna razón, usar un número muy próximo al mismo para lograr resultados similares.
                     usrdef.outliersEnabled = TRUE, # Outliers definidos por el usuario.
                     usrdef.outliersDate = c("2019-09-01",
                                             "2019-12-01",
                                             "2021-12-01",
                                             "2023-02-01",
                                             "2023-08-01",
                                             "2023-10-01",
                                             "2024-06-01"),
                     usrdef.outliersType = c("AO", "TC", "TC", "AO", "AO", "TC", "TC"),
                        )

# Se ejecuta el Modelo:
Modelo <- x13(cantidad_clientes_ts, # Serie.
              spec = spec_x13 # Specs.
              )

# Nótese la significación y que el ar2 y sar1 tienen los signos cambiados
#respecto a la función ARIMA .Los polinomios se definen con 
#signos distintos: help(regarima)
Modelo$regarima %>% summary

# Tipo de Descomposición:
Modelo$decomposition$mode # Aditiva. Parece sensato siendo una cantidad.

# Se guarda las series, así como su descomposición:
Series_X13 <- Modelo$final$series
# Series estimadas:
Series_X13

################################################################################
# Se grafica las series:
Series_X13_DF <- data.frame(Fecha = seq(from = as.Date("2018-12-01"),
                                      to = as.Date("2024-12-01"),
                                      by = "month"),
                          y = Series_X13[,1], # Serie Original.
                          sa = Series_X13[,2], # Serie Desestacionalizada.
                          t = Series_X13[,3], # Serie Componente Tendencial.
                          s = Series_X13[,4], # Serie Componente Estacional.
                          i = Series_X13[,5] # Serie Componente Irregular.
                          ) %>% 
  pivot_longer(cols = c(y, sa, t, s, i),
               names_to = "Serie",
               values_to = "Valor")

# Serie Original
Plot_1_Serie_Original <- Series_X13_DF %>% 
  filter(Serie %in% c("y")) %>% 
  ggplot() +
  geom_line(aes(x = Fecha,
                y = Valor,
                color = Serie)) +
  labs(title = "Serie Original") +
  theme_minimal()
Plot_1_Serie_Original

# Serie Desestacionalizada:
Plot_2_Serie_Desestacionalizada <- Series_X13_DF %>% 
  filter(Serie %in% c("sa")) %>% 
  ggplot() +
  geom_line(aes(x = Fecha,
                y = Valor,
                color = Serie)) +
  labs(title = "Serie Desestacionalizada") +
  theme_minimal()
Plot_2_Serie_Desestacionalizada

# Serie Original, Desestacionalizada y Componente Tendencial
Plot_3_Multiple <- Series_X13_DF %>% 
  filter(Serie %in% c("y", "sa", "t")) %>% 
  ggplot() +
  geom_line(aes(x = Fecha,
                y = Valor,
                color = Serie)) +
  labs(title = "Serie Original, Desestacionalizada y Componente Tendencial") +
  theme_minimal()
Plot_3_Multiple

# Serie Desestacionalizada y Componente Tendencial
Plot_4_SA_T <- Series_X13_DF %>% 
  filter(Serie %in% c("sa", "t")) %>% 
  ggplot() +
  geom_line(aes(x = Fecha,
                y = Valor,
                color = Serie)) +
  labs(title = "Serie Desestacionalizada y Componente Tendencial") +
  theme_minimal()
Plot_4_SA_T

grid.arrange(Plot_1_Serie_Original, Plot_2_Serie_Desestacionalizada)
grid.arrange(Plot_3_Multiple, Plot_4_SA_T)

# Componente Estacional e Irregular:
Plot_5_Estacional <- Series_X13_DF %>% 
  filter(Serie %in% c("s")) %>% 
  ggplot() +
  geom_line(aes(x = Fecha,
                y = Valor)) +
  labs(title = "Componente Estacional") +
  theme_minimal()
Plot_5_Estacional

Plot_6_Irregular <- Series_X13_DF %>% 
  filter(Serie %in% c("i")) %>% 
  ggplot() +
  geom_line(aes(x = Fecha,
                y = Valor)) +
  labs(title = "Componente Irregular") + 
  theme_minimal()
Plot_6_Irregular

# Gráfica de los Componentes Estacional e Irregular:
grid.arrange(Plot_5_Estacional, Plot_6_Irregular)

################################################################################
# Predicciones Puntuales de cada componente:
Series_X13_Prediccion <- Modelo$final$forecasts
Series_X13_Prediccion

# Gráficos de las predicciones:
# Serie original: Coincide con lo hecho con forecast.
Serie_Original <- Series_X13[,1]
Prediccion_Serie <- Series_X13_Prediccion[,1]
Plot_7_Prediccion_Serie <- autoplot(Serie_Original) +
  autolayer(Prediccion_Serie, show.legend = FALSE) +
  labs(x = "Fecha",
       y = "Cantidad",
       title = "Serie Original con Predicción de 2025") +
  theme_minimal()
Plot_7_Prediccion_Serie

# Serie Desestacionalizada:
Serie_Desestacionalizada <- Series_X13[,2]
Prediccion_Serie_Desestacionalizada <- Series_X13_Prediccion[,2]
Plot_8_Prediccion_Serie_Desestacionalizada <- autoplot(Serie_Desestacionalizada) +
  autolayer(Prediccion_Serie_Desestacionalizada, show.legend = FALSE) +
  labs(x = "Fecha",
       y = "Cantidad",
       title = "Serie Desestacionalizada con Predicción de 2025") +
  theme_minimal()
Plot_8_Prediccion_Serie_Desestacionalizada

# Componente Tendencial:
Serie_Componente_Tendencia <- Series_X13[,3]
Prediccion_Componente_Tendencia <- Series_X13_Prediccion[,3]
Plot_9_Componente_Tendencia <- autoplot(Serie_Componente_Tendencia) +
  autolayer(Prediccion_Componente_Tendencia, show.legend = FALSE) +
  labs(x = "Fecha",
       y = "Cantidad",
       title = "Componente Tendencial con Predicción de 2025") + 
  theme_minimal()
Plot_9_Componente_Tendencia

grid.arrange(Plot_7_Prediccion_Serie, 
             Plot_8_Prediccion_Serie_Desestacionalizada, 
             Plot_9_Componente_Tendencia)

################################################################################
# Se guarda los resultados:
save.image(file = "./Salidas_Script_Componentes.Rdata")







