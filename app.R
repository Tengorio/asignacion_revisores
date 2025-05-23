library(shiny)
library(shinydashboard)
library(dplyr)
library(readxl)
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(DT)

# UI - Interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "Análisis de Revisores"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen", tabName = "resumen", icon = icon("dashboard")),
      menuItem("Series de Tiempo", tabName = "series", icon = icon("chart-line")),
      menuItem("Distribución Respuestas", tabName = "respuestas", icon = icon("chart-bar")),
      menuItem("Datos Crudos", tabName = "datos", icon = icon("table")),
      menuItem("KPIs", tabName = "kpis", icon = icon("key"))
    ),
    fileInput("file", "Subir archivo Excel", accept = c(".xlsx")),
    actionButton("update", "Actualizar Análisis", icon = icon("refresh"))
  ),
  dashboardBody(
    tabItems(
      # Tab Resumen
      tabItem(tabName = "resumen",
              fluidRow(
                box(title = "Evolución de proyectos por número de revisiones", 
                    plotOutput("plot1"), width = 12)
              ),
              fluidRow(
                box(title = "Distribución de proyectos", 
                    plotOutput("plot2"), width = 6),
                box(title = "Pronóstico de proyectos con 3+ revisiones", 
                    plotOutput("plot3"), width = 6)
              )
      ),
      
      # Tab Series de Tiempo
      tabItem(tabName = "series",
              fluidRow(
                box(title = "Modelo logístico para finalización", 
                    plotOutput("plot4"), width = 12)
              ),
              fluidRow(
                box(title = "Comparación de modelos", 
                    plotOutput("plot5"), width = 12)
              ),
              fluidRow(
                box(title = "Heatmap de proyectos", 
                    plotOutput("plot6"), width = 12)
              )
      ),
      
      # Tab Distribución Respuestas
      tabItem(tabName = "respuestas",
              fluidRow(
                box(title = "Distribución de respuestas", 
                    plotOutput("plot7"), width = 6),
                box(title = "Tiempo de respuesta por tipo", 
                    plotOutput("plot8"), width = 6)
              ),
              fluidRow(
                box(title = "Tendencia de aceptaciones diarias", 
                    plotOutput("plot9"), width = 6),
                box(title = "Probabilidad acumulada de respuesta", 
                    plotOutput("plot10"), width = 6)
              )
      ),
      
      # Tab Datos Crudos
      tabItem(tabName = "datos",
              fluidRow(
                box(title = "Datos de Revisión", 
                    DTOutput("tabla_datos"), width = 12)
              ),
              fluidRow(
                box(title = "Series de Tiempo", 
                    DTOutput("tabla_series"), width = 12)
              )
      ),
      
      # Tab KPIs
      tabItem(tabName = "kpis",
              fluidRow(
                valueBoxOutput("tasa_aceptacion", width = 3),
                valueBoxOutput("tasa_rechazo", width = 3),
                valueBoxOutput("tasa_sin_respuesta", width = 3),
                valueBoxOutput("tiempo_medio", width = 3)
              ),
              fluidRow(
                valueBoxOutput("tiempo_mediano", width = 3),
                valueBoxOutput("dias_completar", width = 3),
                valueBoxOutput("fecha_finalizacion", width = 6)
              ),
              fluidRow(
                box(title = "Resumen de Tasas", 
                    tableOutput("tabla_tasas"), width = 6),
                box(title = "Resumen de Tiempos", 
                    tableOutput("tabla_tiempos"), width = 6)
              )
      )
    )
  )
)

# Server - Lógica del servidor
server <- function(input, output, session) {
  
  # Cargar y procesar datos
  datos_procesados <- eventReactive(input$update, {
    req(input$file)
    
    # Leer archivo
    datos_raw <- read_excel(input$file$datapath)
    
    # Procesar datos (igual que en tu script original)
    datos <- datos_raw %>%
      dplyr::select(
        fecha_invitacion = `FECHA DE INVITACIÓN A REVISAR`,
        fecha_aceptar = `FECHA DE ACEPTAR SER REVISOR`,
        fecha_rechazo = `FECHA DE RECHAZO COMO REVISOR`,
        id_project = `CLAVE DE LA SOLICITUD`
      ) %>%
      mutate(
        fecha_invitacion = as_datetime(fecha_invitacion),
        fecha_aceptar = as_datetime(fecha_aceptar),
        fecha_rechazo = as_datetime(fecha_rechazo),
        fecha_respuesta = case_when(
          !is.na(fecha_aceptar) ~ fecha_aceptar,
          !is.na(fecha_rechazo) ~ fecha_rechazo,
          TRUE ~ NA_POSIXct_
        ),
        dias_transcurridos = ifelse(!is.na(fecha_respuesta), 
                                    as.numeric(difftime(fecha_respuesta, fecha_invitacion, units = "days")),
                                    NA)
      )
    
    # Series de tiempo
    fecha_min <- min(datos$fecha_invitacion)
    fecha_max <- max(coalesce(datos$fecha_respuesta, Sys.time()), na.rm = TRUE)
    fechas_seq <- seq(from = date(fecha_min), to = date(fecha_max), by = "day")
    fechas_df <- data.frame(fecha = fechas_seq)
    
    calcular_estado_proyectos <- function(fecha_actual) {
      estado_proyectos <- datos %>%
        group_by(id_project) %>%
        summarise(
          revisiones_aceptadas = sum(
            !is.na(fecha_aceptar) & date(fecha_aceptar) <= fecha_actual
          )
        )
      
      conteo <- estado_proyectos %>%
        summarise(
          cero_revs = sum(revisiones_aceptadas == 0),
          una_rev = sum(revisiones_aceptadas == 1),
          dos_revs = sum(revisiones_aceptadas == 2),
          tres_revs = sum(revisiones_aceptadas >= 3)
        )
      
      return(conteo)
    }
    
    series_tiempo <- fechas_df %>%
      rowwise() %>%
      mutate(conteos = list(calcular_estado_proyectos(fecha))) %>%
      unnest(conteos)
    
    # Pronósticos
    cero_revs_ts <- ts(series_tiempo$cero_revs, frequency = 7)
    una_rev_ts <- ts(series_tiempo$una_rev, frequency = 7)
    dos_revs_ts <- ts(series_tiempo$dos_revs, frequency = 7)
    tres_revs_ts <- ts(series_tiempo$tres_revs, frequency = 7)
    
    modelo_cero <- auto.arima(cero_revs_ts)
    modelo_una <- auto.arima(una_rev_ts)
    modelo_dos <- auto.arima(dos_revs_ts)
    modelo_tres <- auto.arima(tres_revs_ts)
    
    total_proyectos <- sum(series_tiempo[1, c("cero_revs", "una_rev", "dos_revs", "tres_revs")])
    dias_pronostico <- 60
    
    pronostico_cero <- forecast(modelo_cero, h = dias_pronostico)
    pronostico_una <- forecast(modelo_una, h = dias_pronostico)
    pronostico_dos <- forecast(modelo_dos, h = dias_pronostico)
    pronostico_tres <- forecast(modelo_tres, h = dias_pronostico)
    
    pronosticos_df <- data.frame(
      dia = 1:dias_pronostico,
      fecha = max(fechas_df$fecha) + days(1:dias_pronostico),
      cero_revs = as.numeric(pronostico_cero$mean),
      una_rev = as.numeric(pronostico_una$mean),
      dos_revs = as.numeric(pronostico_dos$mean),
      tres_revs = as.numeric(pronostico_tres$mean)
    ) %>%
      mutate(across(c(cero_revs, una_rev, dos_revs), ~ifelse(. < 0, 0, .)))
    
    # Corregir la lógica de tres_revs para mantener monotonicidad
    ultimo_tres_revs <- tail(series_tiempo$tres_revs, 1)
    for (i in 1:nrow(pronosticos_df)) {
      if (i == 1) {
        pronosticos_df$tres_revs[i] <- max(pronosticos_df$tres_revs[i], ultimo_tres_revs)
      } else {
        pronosticos_df$tres_revs[i] <- max(pronosticos_df$tres_revs[i], pronosticos_df$tres_revs[i-1])
      }
    }
    
    # Ajustar para mantener el total de proyectos constante
    pronosticos_df <- pronosticos_df %>%
      mutate(
        suma = cero_revs + una_rev + dos_revs + tres_revs,
        diferencia = total_proyectos - suma
      ) %>%
      mutate(
        cero_revs = ifelse(diferencia < 0, 
                           pmax(0, cero_revs * (cero_revs + una_rev + dos_revs + diferencia) / 
                                  (cero_revs + una_rev + dos_revs)), 
                           cero_revs),
        una_rev = ifelse(diferencia < 0, 
                         pmax(0, una_rev * (cero_revs + una_rev + dos_revs + diferencia) / 
                                (cero_revs + una_rev + dos_revs)), 
                         una_rev),
        dos_revs = ifelse(diferencia < 0, 
                          pmax(0, dos_revs * (cero_revs + una_rev + dos_revs + diferencia) / 
                                 (cero_revs + una_rev + dos_revs)), 
                          dos_revs),
        tres_revs = ifelse(diferencia > 0, tres_revs + diferencia, tres_revs)
      ) %>%
      mutate(across(c(cero_revs, una_rev, dos_revs, tres_revs), ~round(.))) %>%
      mutate(
        suma = cero_revs + una_rev + dos_revs + tres_revs,
        tres_revs = tres_revs + (total_proyectos - suma)
      )
    
    # Encontrar día objetivo
    dia_objetivo <- pronosticos_df %>%
      filter(cero_revs <= 1 & una_rev <= 1 & dos_revs <= 1) %>%
      slice(1)
    
    if(nrow(dia_objetivo) == 0) {
      dia_objetivo <- pronosticos_df %>% slice(n())
    }
    
    # Combinar datos históricos y pronósticos
    datos_completos <- bind_rows(
      series_tiempo %>% dplyr::select(fecha, cero_revs, una_rev, dos_revs, tres_revs),
      pronosticos_df %>% dplyr::select(fecha, cero_revs, una_rev, dos_revs, tres_revs)
    ) %>%
      mutate(tipo = ifelse(fecha <= max(series_tiempo$fecha), "Datos históricos", "Pronóstico"))
    
    # Modelo logístico
    datos_para_regresion <- data.frame(
      dia = 1:nrow(series_tiempo),
      fecha = series_tiempo$fecha,
      proporcion_completados = series_tiempo$tres_revs / total_proyectos
    )
    
    modelo_logistico <- tryCatch({
      modelo <- nls(proporcion_completados ~ SSlogis(dia, Asym, xmid, scal), 
                    data = datos_para_regresion,
                    start = list(Asym = 1, xmid = nrow(datos_para_regresion)/2, scal = 1))
      
      dias_extra <- 1:120
      dias_total <- c(datos_para_regresion$dia, max(datos_para_regresion$dia) + dias_extra)
      
      pred_logistico <- predict(modelo, newdata = list(dia = dias_total))
      
      predicciones_logistico <- data.frame(
        dia = dias_total,
        fecha = c(datos_para_regresion$fecha, 
                  max(datos_para_regresion$fecha) + days(dias_extra)),
        proporcion_pred = pred_logistico,
        proyectos_pred = round(pred_logistico * total_proyectos)
      )
      
      fecha_estimada_95 <- predicciones_logistico %>%
        filter(proporcion_pred >= 0.95) %>%
        slice(1) %>%
        pull(fecha)
      
      fecha_estimada_99 <- predicciones_logistico %>%
        filter(proporcion_pred >= 0.99) %>%
        slice(1) %>%
        pull(fecha)
      
      list(
        modelo = modelo,
        predicciones = predicciones_logistico,
        fecha_95 = fecha_estimada_95,
        fecha_99 = fecha_estimada_99,
        exito = TRUE
      )
    }, error = function(e) {
      return(list(exito = FALSE))
    })
    
    # Tasas de respuesta
    tasa_respuesta <- datos %>%
      summarise(
        total = n(),
        aceptados = sum(!is.na(fecha_aceptar)),
        rechazados = sum(!is.na(fecha_rechazo)),
        sin_respuesta = sum(is.na(fecha_aceptar) & is.na(fecha_rechazo)),
        tasa_aceptacion = aceptados / (aceptados + rechazados) * 100,
        tasa_rechazo = rechazados / (aceptados + rechazados) * 100,
        tasa_sin_respuesta = sin_respuesta / total * 100
      )
    
    tiempo_medio_respuesta <- mean(datos$dias_transcurridos, na.rm = TRUE)
    tiempo_mediano_respuesta <- median(datos$dias_transcurridos, na.rm = TRUE)
    dias_para_completar <- as.numeric(difftime(dia_objetivo$fecha, Sys.Date(), units = "days"))
    
    # Retornar todos los datos procesados
    list(
      datos = datos,
      series_tiempo = series_tiempo,
      pronosticos_df = pronosticos_df,
      dia_objetivo = dia_objetivo,
      datos_completos = datos_completos,
      modelo_logistico = modelo_logistico,
      tasa_respuesta = tasa_respuesta,
      tiempo_medio_respuesta = tiempo_medio_respuesta,
      tiempo_mediano_respuesta = tiempo_mediano_respuesta,
      dias_para_completar = dias_para_completar,
      total_proyectos = total_proyectos
    )
  })
  
  # Gráficos
  output$plot1 <- renderPlot({
    dp <- datos_procesados()
    
    dp$datos_completos %>%
      pivot_longer(cols = c(cero_revs, una_rev, dos_revs, tres_revs),
                   names_to = "categoria", values_to = "conteo") %>%
      mutate(categoria = factor(categoria, 
                                levels = c("cero_revs", "una_rev", "dos_revs", "tres_revs"),
                                labels = c("0 revisiones", "1 revisión", "2 revisiones", "3+ revisiones"))) %>%
      ggplot(aes(x = fecha, y = conteo, color = categoria)) +
      geom_line(aes(linetype = tipo), size = 1) +
      geom_vline(xintercept = as.numeric(dp$dia_objetivo$fecha), linetype = "dashed", color = "red") +
      annotate("text", 
               x = dp$dia_objetivo$fecha + days(5), 
               y = max(dp$datos_completos$cero_revs, dp$datos_completos$una_rev, 
                       dp$datos_completos$dos_revs, dp$datos_completos$tres_revs) * 0.8, 
               label = paste("Fecha estimada:", format(dp$dia_objetivo$fecha, "%d/%m/%Y")),
               color = "red", fontface = "bold") +
      labs(title = "Pronóstico de proyectos por número de revisiones",
           subtitle = "Líneas continuas: datos históricos, líneas discontinuas: pronóstico",
           x = "Fecha", y = "Número de proyectos",
           color = "Categoría", linetype = "Tipo de dato") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$plot2 <- renderPlot({
    dp <- datos_procesados()
    
    dp$datos_completos %>%
      pivot_longer(cols = c(cero_revs, una_rev, dos_revs, tres_revs),
                   names_to = "categoria", values_to = "conteo") %>%
      mutate(categoria = factor(categoria, 
                                levels = c("tres_revs", "dos_revs", "una_rev", "cero_revs"),
                                labels = c("3+ revisiones", "2 revisiones", "1 revisión", "0 revisiones"))) %>%
      ggplot(aes(x = fecha, y = conteo, fill = categoria)) +
      geom_area() +
      geom_vline(xintercept = as.numeric(max(dp$series_tiempo$fecha)), 
                 linetype = "dashed", color = "black") +
      geom_vline(xintercept = as.numeric(dp$dia_objetivo$fecha), 
                 linetype = "dashed", color = "red") +
      annotate("text", 
               x = dp$dia_objetivo$fecha + days(2), 
               y = dp$total_proyectos * 0.95, 
               label = "Meta", 
               color = "red", fontface = "bold") +
      annotate("text", 
               x = max(dp$series_tiempo$fecha) + days(2), 
               y = dp$total_proyectos * 0.95, 
               label = "Hoy", 
               color = "black", fontface = "bold") +
      scale_fill_brewer(palette = "Set1") +
      labs(title = "Distribución de proyectos por número de revisiones a lo largo del tiempo",
           subtitle = paste("Total de proyectos:", dp$total_proyectos),
           x = "Fecha", y = "Número de proyectos",
           fill = "Estado") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$plot3 <- renderPlot({
    dp <- datos_procesados()
    
    # Extraer límites de confianza
    conf_intervals <- data.frame(
      fecha = max(dp$series_tiempo$fecha) + days(1:60),
      tres_lower = as.numeric(forecast(auto.arima(ts(dp$series_tiempo$tres_revs, frequency = 7)), h = 60)$lower[, 2]),
      tres_upper = as.numeric(forecast(auto.arima(ts(dp$series_tiempo$tres_revs, frequency = 7)), h = 60)$upper[, 2])
    )
    
    # Asegurar monotonicidad
    ensure_monotonic <- function(x, start_value) {
      result <- numeric(length(x))
      result[1] <- max(x[1], start_value)
      for (i in 2:length(x)) {
        result[i] <- max(x[i], result[i-1])
      }
      return(result)
    }
    
    ultimo_tres_revs <- tail(dp$series_tiempo$tres_revs, 1)
    conf_intervals$tres_lower <- ensure_monotonic(conf_intervals$tres_lower, ultimo_tres_revs)
    conf_intervals$tres_upper <- ensure_monotonic(conf_intervals$tres_upper, ultimo_tres_revs)
    conf_intervals$tres_upper <- pmin(conf_intervals$tres_upper, dp$total_proyectos)
    
    tres_revs_forecast <- data.frame(
      fecha = c(dp$series_tiempo$fecha, dp$pronosticos_df$fecha),
      valor = c(dp$series_tiempo$tres_revs, dp$pronosticos_df$tres_revs),
      tipo = c(rep("histórico", nrow(dp$series_tiempo)), rep("pronóstico", nrow(dp$pronosticos_df))),
      lower = c(rep(NA, nrow(dp$series_tiempo)), conf_intervals$tres_lower),
      upper = c(rep(NA, nrow(dp$series_tiempo)), conf_intervals$tres_upper)
    )
    
    ggplot(tres_revs_forecast, aes(x = fecha, y = valor)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.2) +
      geom_line(aes(color = tipo, linetype = tipo), size = 1) +
      geom_vline(xintercept = as.numeric(dp$dia_objetivo$fecha), linetype = "dashed", color = "red") +
      scale_color_manual(values = c("histórico" = "black", "pronóstico" = "blue")) +
      scale_linetype_manual(values = c("histórico" = "solid", "pronóstico" = "dashed")) +
      labs(title = "Pronóstico de proyectos con 3+ revisiones",
           subtitle = "Con intervalos de confianza del 95%",
           x = "Fecha", y = "Número de proyectos",
           color = "Tipo de dato", linetype = "Tipo de dato") +
      annotate("text", 
               x = dp$dia_objetivo$fecha, 
               y = max(tres_revs_forecast$upper, na.rm = TRUE) * 0.8, 
               label = paste("Fecha meta:", format(dp$dia_objetivo$fecha, "%d/%m/%Y")),
               color = "red", fontface = "bold") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$plot4 <- renderPlot({
    dp <- datos_procesados()
    
    if (dp$modelo_logistico$exito) {
      ggplot() +
        geom_point(data = data.frame(
          fecha = dp$series_tiempo$fecha,
          proporcion_completados = dp$series_tiempo$tres_revs / dp$total_proyectos
        ), 
        aes(x = fecha, y = proporcion_completados), 
        color = "blue") +
        geom_line(data = dp$modelo_logistico$predicciones, 
                  aes(x = fecha, y = proporcion_pred), 
                  color = "red", size = 1) +
        geom_hline(yintercept = 0.95, linetype = "dashed", color = "darkgreen") +
        geom_hline(yintercept = 0.99, linetype = "dashed", color = "purple") +
        geom_vline(xintercept = as.numeric(dp$modelo_logistico$fecha_95), 
                   linetype = "dashed", color = "darkgreen") +
        geom_vline(xintercept = as.numeric(dp$modelo_logistico$fecha_99), 
                   linetype = "dashed", color = "purple") +
        annotate("text", 
                 x = dp$modelo_logistico$fecha_95 + days(5), 
                 y = 0.96, 
                 label = paste("95%:", format(dp$modelo_logistico$fecha_95, "%d/%m/%Y")),
                 color = "darkgreen") +
        annotate("text", 
                 x = dp$modelo_logistico$fecha_99 + days(5), 
                 y = 0.92, 
                 label = paste("99%:", format(dp$modelo_logistico$fecha_99, "%d/%m/%Y")),
                 color = "purple") +
        labs(title = "Modelo logístico para la finalización de revisiones",
             subtitle = "Proporción de proyectos con 3+ revisiones",
             x = "Fecha", y = "Proporción de proyectos completados") +
        theme_minimal()
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Modelo logístico no disponible", size = 6) +
        theme_void()
    }
  })
  
  output$plot5 <- renderPlot({
    dp <- datos_procesados()
    
    if (dp$modelo_logistico$exito) {
      ultimo_dia_historico <- max(dp$series_tiempo$fecha)
      primer_dia_pronostico <- ultimo_dia_historico + days(1)
      
      proyecciones_arima <- dp$pronosticos_df %>%
        dplyr::select(fecha, tres_revs) %>%
        rename(arima_pred = tres_revs)
      
      proyecciones_logistico <- dp$modelo_logistico$predicciones %>%
        filter(fecha >= primer_dia_pronostico) %>%
        dplyr::select(fecha, proyectos_pred) %>%
        rename(logistico_pred = proyectos_pred)
      
      comparacion_modelos <- proyecciones_arima %>%
        left_join(proyecciones_logistico, by = "fecha") %>%
        filter(row_number() <= min(nrow(proyecciones_arima), nrow(proyecciones_logistico)))
      
      ggplot(comparacion_modelos, aes(x = fecha)) +
        geom_line(aes(y = arima_pred, color = "ARIMA"), size = 1) +
        geom_line(aes(y = logistico_pred, color = "Logístico"), size = 1) +
        geom_vline(xintercept = as.numeric(dp$dia_objetivo$fecha), 
                   linetype = "dashed", color = "blue") +
        geom_vline(xintercept = as.numeric(dp$modelo_logistico$fecha_99), 
                   linetype = "dashed", color = "red") +
        scale_color_manual(values = c("ARIMA" = "blue", "Logístico" = "red")) +
        labs(title = "Comparación de modelos de pronóstico",
             subtitle = "Proyectos con 3+ revisiones",
             x = "Fecha", y = "Número de proyectos",
             color = "Modelo") +
        theme_minimal() +
        theme(legend.position = "bottom")
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5, label = "Comparación no disponible", size = 6) +
        theme_void()
    }
  })
  
  output$plot6 <- renderPlot({
    dp <- datos_procesados()
    
    dp$datos_completos %>%
      mutate(
        dia_semana = wday(fecha, label = TRUE),
        semana = floor_date(fecha, "week")
      ) %>%
      filter(!is.na(semana), !is.na(dia_semana)) %>%
      ggplot(aes(x = dia_semana, y = semana, fill = tres_revs)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "darkgreen") +
      labs(title = "Heatmap de proyectos con 3+ revisiones por día de la semana",
           x = "Día de la semana", y = "Semana",
           fill = "Proyectos") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8))
  })
  
  output$plot7 <- renderPlot({
    dp <- datos_procesados()
    
    dp$datos %>%
      mutate(
        estado = case_when(
          !is.na(fecha_aceptar) ~ "Aceptado",
          !is.na(fecha_rechazo) ~ "Rechazado",
          TRUE ~ "Sin respuesta"
        )
      ) %>%
      count(estado) %>%
      mutate(porcentaje = n / sum(n) * 100) %>%
      ggplot(aes(x = estado, y = n, fill = estado)) +
      geom_col() +
      geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
                position = position_stack(vjust = 0.5)) +
      scale_fill_manual(values = c("Aceptado" = "forestgreen", 
                                   "Rechazado" = "firebrick", 
                                   "Sin respuesta" = "gray")) +
      labs(title = "Distribución de respuestas a invitaciones",
           x = NULL, y = "Número de invitaciones",
           fill = "Estado") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$plot8 <- renderPlot({
    dp <- datos_procesados()
    
    dp$datos %>%
      filter(!is.na(dias_transcurridos)) %>%
      mutate(
        tipo_respuesta = ifelse(!is.na(fecha_aceptar), "Aceptación", "Rechazo")
      ) %>%
      ggplot(aes(x = tipo_respuesta, y = dias_transcurridos, fill = tipo_respuesta)) +
      geom_boxplot(alpha = 0.7) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
      stat_summary(fun = mean, geom = "text", 
                   aes(label = paste0("Media: ", round(..y.., 1))),
                   vjust = -1) +
      scale_fill_manual(values = c("Aceptación" = "forestgreen", "Rechazo" = "firebrick")) +
      labs(title = "Tiempo de respuesta por tipo",
           subtitle = "Días transcurridos desde la invitación hasta obtener respuesta",
           x = NULL, y = "Días transcurridos",
           fill = "Tipo de respuesta") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
  
  output$plot9 <- renderPlot({
    dp <- datos_procesados()
    
    dp$datos %>%
      filter(!is.na(fecha_aceptar)) %>%
      mutate(fecha_aceptar_dia = as.Date(fecha_aceptar)) %>%
      count(fecha_aceptar_dia) %>%
      complete(fecha_aceptar_dia = seq(min(fecha_aceptar_dia), max(fecha_aceptar_dia), by = "day"), 
               fill = list(n = 0)) %>%
      ggplot(aes(x = fecha_aceptar_dia, y = n)) +
      geom_col(fill = "steelblue") +
      geom_smooth(method = "loess", color = "red", se = TRUE) +
      labs(title = "Tendencia de aceptaciones diarias",
           subtitle = "Con línea de tendencia suavizada",
           x = "Fecha", y = "Número de aceptaciones") +
      theme_minimal()
  })
  
  output$plot10 <- renderPlot({
    dp <- datos_procesados()
    
    dp$datos %>%
      filter(!is.na(dias_transcurridos)) %>%
      arrange(dias_transcurridos) %>%
      mutate(prop_acumulada = row_number() / n()) %>%
      ggplot(aes(x = dias_transcurridos, y = prop_acumulada)) +
      geom_line(color = "blue", size = 1) +
      geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
      geom_hline(yintercept = 0.9, linetype = "dashed", color = "darkgreen") +
      annotate("text", x = max(dp$datos$dias_transcurridos, na.rm = TRUE) * 0.8, y = 0.52, 
               label = "50% de respuestas", color = "red") +
      annotate("text", x = max(dp$datos$dias_transcurridos, na.rm = TRUE) * 0.8, y = 0.92, 
               label = "90% de respuestas", color = "darkgreen") +
      labs(title = "Probabilidad acumulada de obtener respuesta",
           x = "Días transcurridos", y = "Proporción acumulada") +
      theme_minimal()
  })
  
  # Tablas
  output$tabla_datos <- renderDT({
    dp <- datos_procesados()
    datatable(dp$datos, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$tabla_series <- renderDT({
    dp <- datos_procesados()
    datatable(dp$series_tiempo, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$tabla_tasas <- renderTable({
    dp <- datos_procesados()
    data.frame(
      Métrica = c("Tasa de aceptación", "Tasa de rechazo", "Tasa sin respuesta"),
      Valor = c(
        paste0(round(dp$tasa_respuesta$tasa_aceptacion, 1), "%"),
        paste0(round(dp$tasa_respuesta$tasa_rechazo, 1), "%"),
        paste0(round(dp$tasa_respuesta$tasa_sin_respuesta, 1), "%")
      )
    )
  })
  
  #%%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%%
  #%%%%%%%%%%%%%%%%
  
  output$tabla_tiempos <- renderTable({
    dp <- datos_procesados()
    data.frame(
      Métrica = c("Tiempo medio de respuesta", "Tiempo mediano de respuesta"),
      Valor = c(
        paste0(round(dp$tiempo_medio_respuesta, 1), " días"),
        paste0(round(dp$tiempo_mediano_respuesta, 1), " días")
      )
    )
  })
  
  # Value boxes
  output$tasa_aceptacion <- renderValueBox({
    dp <- datos_procesados()
    valueBox(
      value = paste0(round(dp$tasa_respuesta$tasa_aceptacion, 1), "%"),
      subtitle = "Tasa de aceptación", 
      icon = icon("thumbs-up"),
      color = "green"
    )
  })
  
  output$tasa_rechazo <- renderValueBox({
    dp <- datos_procesados()
    valueBox(
      value = paste0(round(dp$tasa_respuesta$tasa_rechazo, 1), "%"),
      subtitle = "Tasa de rechazo", 
      icon = icon("thumbs-down"),
      color = "red"
    )
  })
  
  output$tasa_sin_respuesta <- renderValueBox({
    dp <- datos_procesados()
    valueBox(
      value = paste0(round(dp$tasa_respuesta$tasa_sin_respuesta, 1), "%"),
      subtitle = "Sin respuesta", 
      icon = icon("question"),
      color = "yellow"
    )
  })
  
  output$tiempo_medio <- renderValueBox({
    dp <- datos_procesados()
    valueBox(
      value = round(dp$tiempo_medio_respuesta, 1),
      subtitle = "Días (media)", 
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  output$tiempo_mediano <- renderValueBox({
    dp <- datos_procesados()
    valueBox(
      value = round(dp$tiempo_mediano_respuesta, 1),
      subtitle = "Días (mediana)", 
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  output$dias_completar <- renderValueBox({
    dp <- datos_procesados()
    valueBox(
      value = round(dp$dias_para_completar), 
      subtitle = "Días para completar", 
      icon = icon("calendar-check"),
      color = "purple"
    )
  })
  
  output$fecha_finalizacion <- renderValueBox({
    dp <- datos_procesados()
    valueBox(
      value = format(dp$dia_objetivo$fecha, "%d/%m/%Y"), 
      subtitle = "Fecha estimada de finalización", 
      icon = icon("flag-checkered"),
      color = "purple",
      width = 6
    )
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)