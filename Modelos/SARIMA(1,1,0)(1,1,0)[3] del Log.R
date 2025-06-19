# Se toma el SARIMA(2,1,0)(0,1,1).
# Orden del AR de la FAC y FACP de la Serie Diferenciada.
# Orden Estacional del MA de la Serie Diferenciada en 3. Puede ser un AR también.
# Se interviene por sus Outliers.
# Se modela estacionalidad en rezago 3.
cantidad_clientes_ts <- log(cantidad_clientes_ts)
cantidad_clientes_ts_completa <- log(cantidad_clientes_ts_completa)

Modelo_1 <- Arima(y = cantidad_clientes_ts,
                  order = c(2, 1, 0),
                  seasonal = list(order = c(0,1,1), period = 3),
                  method = "ML")
coeftest(Modelo_1)
Modelo_1 <- Arima(y = cantidad_clientes_ts,
                  order = c(1, 1, 0),
                  seasonal = list(order = c(0,1,1), period = 3),
                  method = "ML")
coeftest(Modelo_1)
Modelo_1 <- Arima(y = cantidad_clientes_ts,
                  order = c(1, 1, 0),
                  seasonal = list(order = c(1,1,0), period = 3),
                  method = "ML")
coeftest(Modelo_1) # Se va con AR en ambas partes.

# Métricas AIC, AICc y BIC:
Metricas_Modelo_1 <- data.frame(
  AIC = Modelo_1$aic,
  AICc = Modelo_1$aicc,
  BIC = Modelo_1$bic
)
Metricas_Modelo_1

outlier_m1 <- tso(cantidad_clientes_ts, 
                  cval = 2.5, 
                  types = c("AO", "LS", "TC"), 
                  tsmethod = "arima", 
                  args.tsmethod = list(order = c(1, 1, 0),
                                       seasonal = list(order = c(1,1,0), period = 3),
                                       include.mean= FALSE)
) 
outlier_m1$outliers

# Se incluye los atípicos:
AO_2019_09 <- tsoutliers::outliers(type = "AO", ind = 10)
AO_2019_09 <- tsoutliers::outliers.effects(AO_2019_09, length(cantidad_clientes_ts)) # Grafico RE.

TC_2019_12 <- tsoutliers::outliers(type = "TC", ind = 13)
TC_2019_12 <- tsoutliers::outliers.effects(TC_2019_12, length(cantidad_clientes_ts))

TC_2021_12 <- tsoutliers::outliers(type = "TC", ind = 37)
TC_2021_12 <- tsoutliers::outliers.effects(TC_2021_12, length(cantidad_clientes_ts)) # Grafico RE.

LS_2023_03 <- tsoutliers::outliers(type = "LS", ind = 52)
LS_2023_03 <- tsoutliers::outliers.effects(LS_2023_03, length(cantidad_clientes_ts))

AO_2023_08 <- tsoutliers::outliers(type = "AO", ind = 57)
AO_2023_08 <- tsoutliers::outliers.effects(AO_2023_08, length(cantidad_clientes_ts))

TC_2023_10 <- tsoutliers::outliers(type = "TC", ind = 59)
TC_2023_10 <- tsoutliers::outliers.effects(TC_2023_10, length(cantidad_clientes_ts))

TC_2024_06 <- tsoutliers::outliers(type = "TC", ind = 67)
TC_2024_06 <- tsoutliers::outliers.effects(TC_2024_06, length(cantidad_clientes_ts)) 

xreg <- cbind(AO_2019_09, 
              TC_2019_12, 
              TC_2021_12,
              LS_2023_03,
              AO_2023_08,
              TC_2023_10,
              TC_2024_06)

# Con los atipicos:
Modelo_2 <- Arima(y = cantidad_clientes_ts,
                  order = c(1, 1, 0), # Original 1,1,0
                  seasonal = list(order = c(1,1,0), period = 3),
                  xreg = xreg,
                  fixed = c(
                    0,
                    NA,
                    NA, NA, NA, NA, NA, NA, NA
                  ),
                  method = "ML")
coeftest(Modelo_2)

Metricas_Modelo_2 <- data.frame(
  AIC = Modelo_2$aic,
  AICc = Modelo_2$aicc,
  BIC = Modelo_2$bic
)
Metricas_Modelo_1
Metricas_Modelo_2

# Primero se observa los Residuos:
Residuos_Modelo_2 <- residuals(Modelo_2)
residuos_Modelo_2_acf <- ggAcf(Residuos_Modelo_2, lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC de los Residuos") +
  theme_minimal()
residuos_Modelo_2_pacf <- ggAcf(Residuos_Modelo_2, lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP de los Residuos") +
  theme_minimal()
grid.arrange(residuos_Modelo_2_acf, residuos_Modelo_2_pacf)
# Residuos estandarizados:
residuos_estandarizados_Modelo_2 <- Residuos_Modelo_2/sqrt(Modelo_2$sigma2)
# Plot:
autoplot(residuos_estandarizados_Modelo_2) +
  labs(x = "Año",
       y = "Residuos Estandarizados") +
  theme(panel.grid.minor = element_blank()) +
  geom_hline(yintercept = 3, color = "red") +
  geom_hline(yintercept = -3, color = "red")
# Puede haber algún Outlier.
# Ljung-Box (fitdf = p + q del Modelo ARIMA(p,d,q))
ljung_box_df_Modelo_2 <- tibble()
for(i in 3:24){
  ljung_box_df_Modelo_2 <- rbind(ljung_box_df_Modelo_2, (Box.test(
    Residuos_Modelo_2, lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
ljung_box_df_Modelo_2

# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result_Modelo_2 <- shapiro.test(Residuos_Modelo_2) %>% tidy()
jarque_bera_test_result_Modelo_2 <- jarque.bera.test(Residuos_Modelo_2) %>% tidy()
shapiro_test_result_Modelo_2
jarque_bera_test_result_Modelo_2

# Esperanza Nula de los Residuos (una vez chequeada la No Autocorrelación de los mismos).
# No se hace por falla del supuesto anterior.

# Homoscedasticidad (Varianza Constante):
# FAC y FACP del Cuadrado de los Residuos.
Residuos_Modelo_2_Cuadradado <- Residuos_Modelo_2^2
residuos_cuadrado_Modelo_2_acf <- ggAcf(Residuos_Modelo_2_Cuadradado, 
                                        lag.max = 24, type = "correlation") +
  labs(x = "Rezago", y = "Autocorrelación", title = "FAC del Cuadrado de los Residuos") +
  theme_minimal()
residuos_cuadrado_Modelo_2_pacf <- ggAcf(Residuos_Modelo_2_Cuadradado, 
                                         lag.max = 24, type = "partial") +
  labs(x = "Rezago", y = "Autocorrelación parcial", title = "FACP del Cuadrado de los Residuos") +
  theme_minimal()
grid.arrange(residuos_cuadrado_Modelo_2_acf, residuos_cuadrado_Modelo_2_pacf)

# Ljung-Box para los Cuadrados de los Residuos.
ljung_box_df_Modelo_2_residuos_cuadrado <- tibble()
for(i in 3:24){
  ljung_box_df_Modelo_2_residuos_cuadrado <- rbind(
    ljung_box_df_Modelo_2_residuos_cuadrado, 
    (Box.test(Residuos_Modelo_2_Cuadradado, 
              lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
ljung_box_df_Modelo_2_residuos_cuadrado

################################################################################
# Predicción:
xreg_prediccion <- tibble(
  AO10 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 10), (length(cantidad_clientes_ts) + 12)),
  TC13 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 13), length(cantidad_clientes_ts) + 12),
  TC37 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 37), length(cantidad_clientes_ts) + 12),
  LS52 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "LS", ind = 52), (length(cantidad_clientes_ts) + 12)),
  AO57 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 57), (length(cantidad_clientes_ts) + 12)),   
  TC59 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 59), length(cantidad_clientes_ts) + 12),
  TC67 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 67), (length(cantidad_clientes_ts) + 12))
)

Predicciones_Modelo_2 <- forecast(Modelo_2, fan = TRUE, xreg = as.matrix(xreg_prediccion[74:85,]), h = 12)

autoplot(object = Predicciones_Modelo_2) +
  labs(x = "Fecha",
       y = "Deudores",
       title = "") +
  theme_minimal()

Comparacion_Prediccion_Realidad <- tibble(
  Fecha = c(1,2,3),
  Predicha = exp(Predicciones_Modelo_2$mean[1:3]),
  Realidad = exp(cantidad_clientes_ts_completa[(length(cantidad_clientes_ts_completa)-2):length(cantidad_clientes_ts_completa)])) %>%
  pivot_longer(cols = c(Predicha, Realidad),
               names_to = "Serie",
               values_to = "Valores")

ggplot(Comparacion_Prediccion_Realidad) +
  geom_line(aes(x = Fecha, y = Valores, color = Serie)) +
  labs(color = "Predicción")

# Errores de Predicción dentro de la Muestra:
accuracy(Modelo_2)

# Predicción afuera de la Muestra:
# Muestra de entrenamiento ("training set") hasta 2022 inclusive
train_clientes <- window(cantidad_clientes_ts_completa, end = c(2023,12))
length(train_clientes)

# Dejamos los datos de 2023 como conjunto de prueba ("test set")
test_clientes <- window(cantidad_clientes_ts_completa, start = 2024)
n <- length(test_clientes)

# Modelo 3
Modelo_2_train <- Arima(y = train_clientes,
                        order = c(1, 1, 0),
                        seasonal = list(order = c(1,1,0), period = 3),
                        fixed = c(
                          NA,
                          NA,
                          NA, NA, NA, NA, NA, NA), # Un outlier vendría después.
                        xreg = xreg[1:61,1:6],
                        method = "ML")

# Predecimos fuera de la muestra (el horizonte de predicción
# será igual al largo del test set)

# Modelo 3
Pred_2_test <- forecast(Modelo_2_train, h = n, 
                        xreg = as.matrix(xreg_prediccion[62:(dim(xreg_prediccion)[1]),1:6]))
accuracy(Pred_2_test, test_clientes)

autoplot(object = Pred_2_test) +
  labs(x = "Fecha",
       y = "Deudores",
       title = "") +
  theme_minimal()

ggplot(bind_rows(as.tibble(train_clientes) %>% janitor::clean_names() %>% 
                  select(point_forecast = x),
  as.tibble(Pred_2_test) %>% janitor::clean_names() %>% select(point_forecast)) %>% 
  mutate(point_forecast_anti_log = exp(point_forecast), row = row_number())
  ) +
  geom_line(aes(row, point_forecast_anti_log))

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

