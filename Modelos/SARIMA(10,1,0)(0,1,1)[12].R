outlier_m3 <- tso(cantidad_clientes_ts, 
                  cval = 2.5, 
                  types = c("AO", "LS", "TC"), 
                  tsmethod = "arima", 
                  args.tsmethod = list(order = c(3, 1, 0), 
                                       seasonal = list(order = c(0, 1, 1), period = 12), 
                                       include.mean= FALSE))
outlier_m3$outliers

# Se incluye los atípicos:
AO_2019_09 <- tsoutliers::outliers(type = "AO", ind = 10)
AO_2019_09 <- tsoutliers::outliers.effects(AO_2019_09, length(cantidad_clientes_ts)) # Grafico RE.

TC_2019_12 <- tsoutliers::outliers(type = "TC", ind = 13)
TC_2019_12 <- tsoutliers::outliers.effects(TC_2019_12, length(cantidad_clientes_ts))

AO_2021_12 <- tsoutliers::outliers(type = "AO", ind = 37)
AO_2021_12 <- tsoutliers::outliers.effects(AO_2021_12, length(cantidad_clientes_ts)) # Grafico RE.

AO_2023_08 <- tsoutliers::outliers(type = "AO", ind = 57)
AO_2023_08 <- tsoutliers::outliers.effects(AO_2023_08, length(cantidad_clientes_ts))

TC_2023_10 <- tsoutliers::outliers(type = "TC", ind = 59)
TC_2023_10 <- tsoutliers::outliers.effects(TC_2023_10, length(cantidad_clientes_ts)) # Gráfico RE.

TC_2024_06 <- tsoutliers::outliers(type = "TC", ind = 67)
TC_2024_06 <- tsoutliers::outliers.effects(TC_2024_06, length(cantidad_clientes_ts)) 


xreg <- cbind(AO_2019_09, 
              TC_2019_12, 
              AO_2021_12,
              AO_2023_08,
              TC_2023_10,
              TC_2024_06)

Modelo_3 <- Arima(y = cantidad_clientes_ts,
                  order = c(10, 1, 0),
                  seasonal = list(order = c(0,1,1), period = 12),
                  fixed = c(
                    0, NA, 0, 0, 0, 0, 0, NA, 0, NA, # originalmente era 2,1,0, pero usando lungbox se paso a 8 y luego a 10.
                    NA,
                    NA, NA, NA, NA, NA, NA),
                  xreg = xreg,
                  method = "ML")
coeftest(Modelo_3) # Se fija ar1 y ar2 de la Estacionalidad en 0, lo que va de la mano con la FAC.

# Métricas AIC, AICc y BIC:
Metricas_Modelo_3 <- data.frame(
  AIC = Modelo_3$aic,
  AICc = Modelo_3$aicc,
  BIC = Modelo_3$bic
)
Metricas_Modelo_3

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
Residuos_Modelo_3_Cuadradado <- Residuos_Modelo_3^2
residuos_cuadrado_Modelo_3_acf <- ggAcf(Residuos_Modelo_3_Cuadradado, 
                                        lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC del Cuadrado de los Residuos") +
  theme_minimal()
residuos_cuadrado_Modelo_3_pacf <- ggAcf(Residuos_Modelo_3_Cuadradado, 
                                         lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP del Cuadrado de los Residuos") +
  theme_minimal()
grid.arrange(residuos_cuadrado_Modelo_3_acf, residuos_cuadrado_Modelo_3_pacf)

# Ljung-Box para los Cuadrados de los Residuos.
ljung_box_df_Modelo_3_residuos_cuadrado <- tibble()
for(i in 3:24){
  ljung_box_df_Modelo_3_residuos_cuadrado <- rbind(
    ljung_box_df_Modelo_3_residuos_cuadrado, 
    (Box.test(Residuos_Modelo_3_Cuadradado, 
              lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
ljung_box_df_Modelo_3_residuos_cuadrado

################################################################################
################################################################################
# Predicción:
cbind(AO_2019_09, 
      TC_2019_12, 
      AO_2021_12,
      AO_2023_08,
      TC_2023_10,
      TC_2024_06)


xreg_prediccion <- tibble(
  AO10 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 10), (length(cantidad_clientes_ts) + 12)),
  TC13 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 13), length(cantidad_clientes_ts) + 12),
  AO37 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 37), length(cantidad_clientes_ts) + 12),
  #AO51 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 51), (length(cantidad_clientes_ts) + 12)),
  AO57 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 57), (length(cantidad_clientes_ts) + 12)),   
  TC59 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 59), length(cantidad_clientes_ts) + 12),
  TC67 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 67), (length(cantidad_clientes_ts) + 12))
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
                        order = c(10, 1, 0),
                        seasonal = list(order = c(0,1,1), period = 12),
                        fixed = c(
                          0, NA, 0, 0, 0, 0, 0, NA, 0, NA, # originalmente era 2,1,0, pero usando lungbox se paso a 8 y luego a 10.
                          NA,
                          NA, NA, NA, NA, NA#, NA
                          ), # Un outlier vendría después.
                        xreg = xreg[1:61,1:5],
                        method = "ML")

# Predecimos fuera de la muestra (el horizonte de predicción
# será igual al largo del test set)

# Modelo 3
Pred_3_test <- forecast(Modelo_3_train, h = n, 
                        xreg = as.matrix(xreg_prediccion[62:(dim(xreg_prediccion)[1]),1:5]))
accuracy(Pred_3_test, test_clientes)

autoplot(object = Pred_3_test) +
  labs(x = "Fecha",
       y = "Deudores",
       title = "") +
  theme_minimal()

Predicciones_Dentro_Muestra_Tibble <- tibble(
  Fecha = seq(1:14),
  Predicha = Pred_3_test$mean[1:14],
  Realidad = cantidad_clientes_ts_completa[(length(cantidad_clientes_ts_completa)-13):length(cantidad_clientes_ts_completa)]) %>%
  pivot_longer(cols = c(Predicha, Realidad),
               names_to = "Serie",
               values_to = "Valores")

ggplot(Predicciones_Dentro_Muestra_Tibble) +
  geom_line(aes(x = Fecha, y = Valores, color = Serie)) +
  labs(color = "Predicción")
# Diferencia explicada por el atípico que no logramos capturar en el período.









