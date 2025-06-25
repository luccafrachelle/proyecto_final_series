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
                     x11.fcasts = 3, # 3 meses, 3 observaciones.
                     # x11.mode = "Multiplicative", # Mantiene el modo en Aditivo por alguna razón.
                     automdl.enabled = TRUE, # Se activa la modelización automática.
                     easter.enabled = TRUE, # Se permite que incida pascuas.
                     outlier.enabled = TRUE, # Ahora se permite que se sugieran outliers.
                     tradingdays.option = "None", # No.
                     transform.function = "Auto", # Automático.
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

# Y se toma las Métricas de Predicción de los primeros tres meses de 2025:
Series_X13_Prediccion <- Modelo$final$forecasts
Series_X13_Prediccion

# Serie original: Coincide con lo hecho con forecast.
Serie_Original <- Series_X13[,1]
Prediccion_Serie <- Series_X13_Prediccion[,1]

# Métricas:
test_clientes <- window(cantidad_clientes_ts_completa, start = 2025)
acc_x13 <- round(accuracy(Prediccion_Serie, test_clientes), 4)
acc_x13
# kable(acc_x13, caption = r"(Métricas de Predicción del Modelo sugerido por X13 - SARIMA$(0,1,0)(1,0,1)$)")

Plot_Prediccion_Serie <- autoplot(window(Serie_Original, start = 2024)) +
  autolayer(Prediccion_Serie, show.legend = FALSE) +
  autolayer(test_clientes) +
  labs(x = "Fecha",
       y = "Cantidad",
       title = "Serie Original con Predicción de 2025",
       subtitle = TeX(r"(Modelo x13: SARIMA$(0,1,0)(1,0,1)$)"),
       color = "") +
  theme_minimal()
  
Plot_Prediccion_Serie

save.image("./Resultados_X13_Automatic_Modelling.Rdata")
