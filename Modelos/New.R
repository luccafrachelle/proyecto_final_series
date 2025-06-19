# Se toma el ARIMA(3,1,0) de la FAC
# Se interviene por sus Outliers
# Se modela estacionalidad en rezago 3.
Modelo_1 <- Arima(y = cantidad_clientes_ts,
                          order = c(3, 1, 0),
                          method = "ML")
coeftest(Modelo_1) # Se fija ar1 y ar2 en 0, lo que va de la mano con la FAC.
Modelo_1 <- Arima(y = cantidad_clientes_ts,
                          order = c(3, 1, 0),
                          fixed = c(0, 0, NA),
                          method = "ML")
coeftest(Modelo_1)

# Métricas AIC, AICc y BIC:
Metricas_Modelo_1 <- data.frame(
  AIC = Modelo_1$aic,
  AICc = Modelo_1$aicc,
  BIC = Modelo_1$bic
)

outlier_m1 <- tso(cantidad_clientes_ts, 
                  cval = 2.5, 
                  types = c("AO", "LS", "TC"), 
                  tsmethod = "arima", 
                  args.tsmethod = list(order = c(3, 1, 0),
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

LS_2023_12 <- tsoutliers::outliers(type = "LS", ind = 61) # Este se desprende del gráfico de residuos.
LS_2023_12 <- tsoutliers::outliers.effects(LS_2023_12, length(cantidad_clientes_ts)) 

TC_2024_06 <- tsoutliers::outliers(type = "TC", ind = 67)
TC_2024_06 <- tsoutliers::outliers.effects(TC_2024_06, length(cantidad_clientes_ts)) 


xreg <- cbind(AO_2019_09, 
              TC_2019_12, 
              TC_2021_12,
              LS_2023_12,
              TC_2024_06)

# Con los atipicos:
Modelo_2 <- Arima(y = cantidad_clientes_ts,
                  order = c(3, 1, 0),
                  fixed = c(
                    0, 0, NA,
                    NA, NA, NA, NA, NA), # Dos Outliers se descartan por significación.
                  xreg = xreg,
                  method = "ML")
coeftest(Modelo_2)

# Métricas AIC, AICc y BIC:
Metricas_Modelo_2 <- data.frame(
  AIC = Modelo_2$aic,
  AICc = Modelo_2$aicc,
  BIC = Modelo_2$bic
)
Metricas_Modelo_1
Metricas_Modelo_2

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
# El Modelo 3 es un SARIMA(3,1,0)(0,1,2)[3] con los atípicos incluídos.
Modelo_3 <- Arima(y = cantidad_clientes_ts,
                  order = c(3, 1, 0),
                  seasonal = list(order = c(0,1,2), period = 3),
                  fixed = c(
                    NA, NA, NA, 
                    NA, NA,
                    NA, NA, NA, NA, 0),
                  xreg = xreg,
                  method = "ML")
coeftest(Modelo_3)
Modelo_3 <- Arima(y = cantidad_clientes_ts,
                  order = c(3, 1, 0),
                  seasonal = list(order = c(0,1,2), period = 3),
                  fixed = c(
                    0, 0, NA, 
                    0, NA,
                    NA, NA, NA, NA, 0),
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
# Se rechaza Normalidad.

################################################################################
# Corregir Ljung-Box
# El Modelo 4 es un SARIMA(10,1,0)(0,1,2)[3] con los atípicos incluídos.
Modelo_4 <- Arima(y = cantidad_clientes_ts,
                  order = c(10, 1, 0),
                  seasonal = list(order = c(0,1,2), period = 3),
                  fixed = c(
                    NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                    NA, NA,
                    NA, NA, NA, NA, 0),
                  xreg = xreg,
                  method = "ML")
coeftest(Modelo_4)
Modelo_4 <- Arima(y = cantidad_clientes_ts,
                  order = c(10, 1, 0),
                  seasonal = list(order = c(0,1,2), period = 3),
                  fixed = c(
                    0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 
                    NA, 0,
                    NA, NA, NA, NA, 0),
                  xreg = xreg,
                  method = "ML")
coeftest(Modelo_4)

# El Modelo 2 logra significación en todos sus parámetros.
# Ratio de Log-Likelihood Test:
L <- 2*(Modelo_4$loglik - Modelo_3$loglik)
# El p-valor se calcula como la probabilidad de que L sea mayor al observado bajo H0 cierta
1 - pchisq(L, 1) # CHEQUEAR LOS GRADOS DE LIBERTAD DEL TEST
# El Modelo nuevo es más verosimil.

# Métricas AIC, AICc y BIC:
Metricas_Modelo_4 <- data.frame(
  AIC = Modelo_4$aic,
  AICc = Modelo_4$aicc,
  BIC = Modelo_4$bic
)
Metricas_Modelo_3
Metricas_Modelo_4 # Modelo con Estacionalidad mejora en los Criterios de Información!

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

# Se incluye los atípicos:
TC_2023_10 <- tsoutliers::outliers(type = "TC", ind = 59)
TC_2023_10 <- tsoutliers::outliers.effects(TC_2023_10, length(cantidad_clientes_ts)) # Grafico RE.

xreg <- cbind(AO_2019_09, 
              TC_2019_12, 
              TC_2021_12,
              TC_2023_10,
              LS_2023_12,
              TC_2024_06)
Modelo_4 <- Arima(y = cantidad_clientes_ts,
                  order = c(10, 1, 0),
                  seasonal = list(order = c(0,1,2), period = 3),
                  fixed = c(
                    0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 
                    NA, 0,
                    NA, NA, NA, NA, 0, NA),
                  xreg = xreg,
                  method = "ML")
coeftest(Modelo_4)

# Ljung-Box (fitdf = p + q del Modelo ARIMA(p,d,q))
Residuos_Modelo_4 <- residuals(Modelo_4)
ljung_box_df_Modelo_4 <- tibble()
for(i in 3:24){
  ljung_box_df_Modelo_4 <- rbind(ljung_box_df_Modelo_4, (Box.test(
    Residuos_Modelo_4, lag = i, type = "Ljung-Box", fitdf = 3) %>% tidy()))
};rm(i)
ljung_box_df_Modelo_4

# Segundo se observa la Normalidad:
# Tests de normalidad (Shapiro-Wilk, Jarque-Bera, Kolmogorov-Smirnov)
shapiro_test_result_Modelo_4 <- shapiro.test(Residuos_Modelo_4) %>% tidy()
jarque_bera_test_result_Modelo_4 <- jarque.bera.test(Residuos_Modelo_4) %>% tidy()
shapiro_test_result_Modelo_4
jarque_bera_test_result_Modelo_4
# No se rechaza Normalidad.












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










Modelo_3 <- Arima(y = cantidad_clientes_ts,
                  order = c(3, 1, 0),
                  seasonal = list(order = c(1,1,0), period = 3),
                  fixed = c(
                    NA, NA, NA,
                    NA,
                    NA, NA, NA, NA, NA),
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
ggplot(data.frame(residuos = Residuos_Modelo_4), aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(x = "Cuantiles teóricos", y = "Cuantiles de la muestra", 
       title = "QQ-plot de los Residuos") +
  theme_minimal()
# Histograma
ggplot(data.frame(residuos = Residuos_Modelo_4)) +
  geom_histogram(aes(x = residuos, y = ..density..), bins = 30) +
  stat_function(fun = dnorm, args = list(mean = mean(Residuos_Modelo_4), 
                                         sd = sd(Residuos_Modelo_4)),
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