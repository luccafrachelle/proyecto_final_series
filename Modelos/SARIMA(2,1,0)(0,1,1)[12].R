# Se comienza planteando el Modelo SARIMA(3,1,0)(0,0,0):
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
Metricas_Modelo_1 <- data.frame(
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
ljung_box_df
# Hay algunos rechazos a la Hipótesis Nula de Residuos Incorrelacionados. Pero
# son en los primeros Lags.

# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result <- shapiro.test(residuos_arima_310) %>% tidy()
jarque_bera_test_result <- jarque.bera.test(residuos_arima_310) %>% tidy()
shapiro_test_result
jarque_bera_test_result
# Se rechaza la Normalidad de los Residuos.

################################################################################
# Primera Intervención de Outliers:
outlier_m1 <- tso(cantidad_clientes_ts, 
                  cval = 2.5, 
                  types = c("AO", "LS", "TC"), 
                  tsmethod = "arima", 
                  args.tsmethod = list(order = c(3, 1, 0), 
                                       #seasonal = c(0, 0, 0),
                                       #fixed = c(0, 0, NA),
                                       include.mean= FALSE)) 
puntos_raros <- outlier_m1$outliers
puntos_raros

# Se incluye los atípicos:
AO_2019_09 <- tsoutliers::outliers(type = "AO", ind = 10)
AO_2019_09 <- tsoutliers::outliers.effects(AO_2019_09, length(cantidad_clientes_ts)) # Grafico RE.

TC_2019_12 <- tsoutliers::outliers(type = "TC", ind = 13)
TC_2019_12 <- tsoutliers::outliers.effects(TC_2019_12, length(cantidad_clientes_ts))

TC_2021_12 <- tsoutliers::outliers(type = "TC", ind = 37)
TC_2021_12 <- tsoutliers::outliers.effects(TC_2021_12, length(cantidad_clientes_ts)) # Grafico RE.

AO_2023_02 <- tsoutliers::outliers(type = "AO", ind = 51)
AO_2023_02 <- tsoutliers::outliers.effects(AO_2023_02, length(cantidad_clientes_ts))

AO_2023_08 <- tsoutliers::outliers(type = "AO", ind = 57)
AO_2023_08 <- tsoutliers::outliers.effects(AO_2023_08, length(cantidad_clientes_ts))

TC_2023_10 <- tsoutliers::outliers(type = "TC", ind = 59)
TC_2023_10 <- tsoutliers::outliers.effects(TC_2023_10, length(cantidad_clientes_ts)) # Gráfico RE.

TC_2024_06 <- tsoutliers::outliers(type = "TC", ind = 67)
TC_2024_06 <- tsoutliers::outliers.effects(TC_2024_06, length(cantidad_clientes_ts)) 


xreg <- cbind(AO_2019_09, 
              TC_2019_12, 
              TC_2021_12,
              AO_2023_02,
              AO_2023_08,
              TC_2023_10,
              TC_2024_06)
# Puede que sean muchas intervenciones, dada la poca cantidad de datos.
dim(xreg)[2]/length(cantidad_clientes_ts) # Casi un 10% de intervenciones.

# El Modelo 2 es un SARIMA(3,1,0)(0,0,0) con los atípicos incluídos.
Modelo_2 <- Arima(y = cantidad_clientes_ts,
                  order = c(3, 1, 0),
                  fixed = c(
                    0, 0, NA, 
                    NA, NA, NA, NA, NA, NA, NA),
                  xreg = xreg,
                  method = "ML")
coeftest(Modelo_2)

# El Modelo 2 logra significación en todos sus parámetros.
# Ratio de Log-Likelihood Test:
L <- 2*(Modelo_2$loglik - modelo_arima_310$loglik)
# El p-valor se calcula como la probabilidad de que L sea mayor al observado bajo H0 cierta
1 - pchisq(L, 1)
# El Modelo nuevo es más verosimil.

# Métricas AIC, AICc y BIC:
Metricas_Modelo_2 <- data.frame(
  AIC = Modelo_2$aic,
  AICc = Modelo_2$aicc,
  BIC = Modelo_2$bic
)
Metricas_Modelo_1
Metricas_Modelo_2 # Modelo con Outliers mejora en los Criterios de Información!

# Diagnósticos:
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

# Segundo se observa la Normalidad:
# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result_Modelo_2 <- shapiro.test(Residuos_Modelo_2) %>% tidy()
jarque_bera_test_result_Modelo_2 <- jarque.bera.test(Residuos_Modelo_2) %>% tidy()
shapiro_test_result_Modelo_2
jarque_bera_test_result_Modelo_2
# No se rechaza Normalidad.

################################################################################
# Incluir Estacionalidad:
# El Modelo 3 es un SARIMA(2,1,0)(0,1,1)[12] con los atípicos incluídos.
Modelo_3 <- Arima(y = cantidad_clientes_ts,
                  order = c(3, 1, 0),
                  seasonal = list(order = c(0,1,1), period = 12),
                  fixed = c(
                    0, NA, 0, 
                    NA,
                    NA, NA, NA, NA, NA, NA, NA),
                  xreg = xreg,
                  method = "ML")
coeftest(Modelo_3)

# El Modelo 2 logra significación en todos sus parámetros.
# Ratio de Log-Likelihood Test:
L <- 2*(Modelo_3$loglik - Modelo_2$loglik)
# El p-valor se calcula como la probabilidad de que L sea mayor al observado bajo H0 cierta
1 - pchisq(L, 1) # CHEQUEAR LOS GRADOS DE LIBERTAD DEL TEST
# El Modelo nuevo es más verosimil.

# Métricas AIC, AICc y BIC:
Metricas_Modelo_3 <- data.frame(
  AIC = Modelo_3$aic,
  AICc = Modelo_3$aicc,
  BIC = Modelo_3$bic
)
Metricas_Modelo_2
Metricas_Modelo_3 # Modelo con Estacionalidad mejora en los Criterios de Información!

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
# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result_Modelo_3 <- shapiro.test(Residuos_Modelo_3) %>% tidy()
jarque_bera_test_result_Modelo_3 <- jarque.bera.test(Residuos_Modelo_3) %>% tidy()
shapiro_test_result_Modelo_3
jarque_bera_test_result_Modelo_3
# No se rechaza Normalidad.

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
# Predicción:
xreg_prediccion <- tibble(
  AO10 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 10), (length(cantidad_clientes_ts) + 12)),
  TC13 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 13), length(cantidad_clientes_ts) + 12),
  TC37 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "TC", ind = 37), length(cantidad_clientes_ts) + 12),
  AO51 = tsoutliers::outliers.effects(tsoutliers::outliers(type = "AO", ind = 51), (length(cantidad_clientes_ts) + 12)),
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
                        order = c(2, 1, 0),
                        seasonal = list(order = c(0,1,1), period = 12),
                        fixed = c(
                          0, NA,
                          NA,
                          NA, NA, NA, NA, NA, NA), # Un outlier vendría después.
                        xreg = xreg[1:61,1:6],
                        method = "ML")

# Predecimos fuera de la muestra (el horizonte de predicción
# será igual al largo del test set)

# Modelo 3
Pred_3_test <- forecast(Modelo_3_train, h = n, 
                        xreg = as.matrix(xreg_prediccion[62:(dim(xreg_prediccion)[1]),1:6]))
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
