packages <- c("rvest", "httr", "stringr", "purrr", "shiny", "dplyr", "ggplot2", 
              "jsonlite", "survival", "data.table", "lme4", "forecast", "lubridate")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Function to convert time string "MM:SS" to seconds
convert_to_seconds <- function(time_str) {
  sapply(strsplit(time_str, ":"), function(p) {
    if (length(p) == 2 && all(!is.na(as.numeric(p)))) {
      as.numeric(p[1]) * 60 + as.numeric(p[2])
    } else {
      NA
    }
  })
}

tyre_data <- read.csv("C:/Users/Priyanka/mangrulkarp_final_project/Tyre Usage - F1 Race Grand Prix - Data.csv", 
                      stringsAsFactors = FALSE)
weather_data <- read.csv("C:/Users/Priyanka/mangrulkarp_final_project/F1 Weather(2023-2018).csv", 
                         stringsAsFactors = FALSE)

if (!"Round.Number" %in% colnames(weather_data)) {
  stop("Column 'Round.Number' not found in weather_data")
}
weather_data$Rainfall <- as.logical(weather_data$Rainfall)
weather_data$Year <- as.integer(weather_data$Year)
round_matches <- grep("round", colnames(weather_data), ignore.case = TRUE, value = TRUE)
if (length(round_matches) > 0) {
  round_col <- round_matches[1]
  weather_data$Round <- as.integer(weather_data[[round_col]])
} else {
  stop("No column with 'round' found in weather_data")
}
if("Time" %in% colnames(weather_data)){
  weather_data$time_sec <- as.numeric(as.difftime(weather_data$Time, units = "secs"))
}

tyre_data$Year <- as.integer(tyre_data$Year)
gp_round_map <- list(
  "Bahrain" = 1, "Saudi Arabia" = 2, "Australia" = 3, "Azerbaijan" = 4, "Miami" = 5,
  "Monaco" = 6, "Spain" = 7, "Canada" = 8, "Austria" = 9, "Great Britain" = 10,
  "Hungary" = 11, "Belgium" = 12, "Netherlands" = 13, "Italy" = 14, "Singapore" = 15,
  "Japan" = 16, "Qatar" = 17, "USA" = 18, "Mexico" = 19, "Brazil" = 20,
  "Las Vegas" = 21, "Abu Dhabi" = 22
)
tyre_data$round <- vapply(tyre_data$Grand.Prix, function(gp) {
  val <- gp_round_map[[gp]]
  if (is.null(val)) return(NA_integer_)
  as.integer(val)
}, integer(1))

driver_name_map <- list(
  "Lewis Hamilton" = "hamilton", "Valtteri Bottas" = "bottas", "Max Verstappen" = "max_verstappen",
  "Sebastian Vettel" = "vettel", "Charles Leclerc" = "leclerc", "Kevin Magnussen" = "kevin_magnussen",
  "Nico Hulkenberg" = "hulkenberg", "Kimi Räikkönen" = "raikkonen", "Lance Stroll" = "stroll",
  "Daniil Kvyat" = "kvyat", "Carlos Sainz" = "sainz", "Romain Grosjean" = "grosjean",
  "Pierre Gasly" = "gasly", "Daniel Ricciardo" = "ricciardo", "George Russell" = "russell",
  "Alexander Albon" = "albon", "Sergio Perez" = "perez", "Antonio Giovinazzi" = "giovinazzi",
  "Lando Norris" = "norris", "Esteban Ocon" = "ocon", "Yuki Tsunoda" = "tsunoda",
  "Nicholas Latifi" = "latifi", "Mick Schumacher" = "mick_schumacher",
  "Guanyu Zhou" = "zhou", "Oscar Piastri" = "piastri", "Logan Sargeant" = "sargeant",
  "Nyck de Vries" = "de_vries", "Liam Lawson" = "lawson"
)

# Assume that gather_lap_data is defined externally and returns lap data for the given season and round.
sample_lap_data <- gather_lap_data(2023, 1)
lap_ids <- unique(sample_lap_data$driverId)
type_driver_names <- unique(tyre_data$Driver)
generate_driver_map <- function(tyre_drivers, ergast_ids) {
  map <- list()
  for (full_name in tyre_drivers) {
    last_name <- tolower(tail(strsplit(full_name, " ")[[1]], 1))
    match <- ergast_ids[grepl(paste0("^", last_name), ergast_ids)]
    if (length(match) == 1) map[[full_name]] <- match
  }
  map
}
fuzzy_driver_map <- generate_driver_map(type_driver_names, lap_ids)
driver_name_map <- modifyList(driver_name_map, fuzzy_driver_map)
reverse_driver_map <- setNames(names(driver_name_map), unlist(driver_name_map))
tyre_data$driverId <- sapply(tyre_data$Driver, function(name) {
  if (!is.null(driver_name_map[[name]])) driver_name_map[[name]]
  else tolower(gsub(" ", "_", name))
})
cat("\n🚫 In tyre_data but not in lap_data:\n")
print(setdiff(unique(tyre_data$driverId), lap_ids))
cat("\n❓ In lap_data but not in tyre_data:\n")
print(setdiff(lap_ids, unique(tyre_data$driverId)))

assign_tyre_compound <- function(lap_df, tyre_df, season, round) {
  df <- lap_df
  df$compound <- NA
  tyre_subset <- tyre_df[tyre_df$Year == season & tyre_df$round == round, ]
  for (i in seq_len(nrow(tyre_subset))) {
    row <- tyre_subset[i, ]
    condition <- df$driverId == row$driverId & df$lap >= row$From & df$lap <= row$To
    df$compound[condition] <- row$Tyres
  }
  df
}

gather_pit_data <- function(season, round) {
  url <- sprintf("https://ergast.com/api/f1/%d/%d/pitstops.json?limit=1000", season, round)
  res <- GET(url)
  if (status_code(res) != 200) return(NULL)
  parsed <- fromJSON(content(res, as = "text"), flatten = TRUE)
  races <- parsed$MRData$RaceTable$Races
  if (!is.data.frame(races) || nrow(races) == 0 || is.null(races$PitStops[[1]])) {
    return(NULL)
  }
  pit_df <- races$PitStops[[1]]
  pit_df$lap <- as.integer(pit_df$lap)
  pit_df$event <- 1
  pit_df$season <- season
  pit_df$round <- round
  weather_row <- weather_data %>%
    filter(Year == season & Round.Number == round) %>%
    slice(1)
  if (nrow(weather_row) == 1) {
    is_wet <- tolower(as.character(weather_row$Rainfall)) %in% c("true", "1", "yes")
    pit_df$Rainfall <- is_wet
    pit_df$TrackTemp <- weather_row$TrackTemp
    pit_df$AirTemp <- weather_row$AirTemp
    pit_df$weatherType <- ifelse(is_wet, "Wet", "Dry")
  } else {
    pit_df$Rainfall <- NA
    pit_df$TrackTemp <- NA
    pit_df$AirTemp <- NA
    pit_df$weatherType <- "Unknown"
  }
  cat("Weather for", season, "Round", round, ":", pit_df$weatherType[1], "\n")
  pit_df
}

ui <- fluidPage(
  titlePanel("F1 Lap Time Analysis Viewer"),
  sidebarLayout(
    sidebarPanel(
      numericInput("season", "Season:", value = 2023, min = 2018, max = 2023),
      numericInput("round", "Round:", value = 1, min = 1, max = 23),
      uiOutput("driver_selector")
    ),
    mainPanel(
      verbatimTextOutput("weatherDisplay"),
      tabsetPanel(
        tabPanel("Lap Trends", plotOutput("lapTrendPlot")),
        tabPanel("Lap Time Variability", plotOutput("boxPlot")),
        tabPanel("Compound Effect", 
                 plotOutput("compoundBoxplot"),
                 verbatimTextOutput("anovaSummary"),
                 verbatimTextOutput("tukeySummary")),
        tabPanel("Pit Stop Analysis",
                 plotOutput("kmPlot"),
                 verbatimTextOutput("kmSummary"),
                 plotOutput("kmWinnerPlot")),
        tabPanel("Time Degradation", plotOutput("degradationPlot")),
        tabPanel("Mixed Effects Model", verbatimTextOutput("mixedModelSummary"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  lap_data <- reactive({
    gather_lap_data(input$season, input$round)
  })
  
  lap_data_enriched <- reactive({
    df <- lap_data() %>%
      group_by(driverId) %>%
      arrange(lap) %>%
      mutate(
        lap_duration = time_sec,
        lap_start_time = cumsum(lag(lap_duration, default = 0))
      ) %>%
      ungroup()
    df
  })
  
  # Rolling join of lap data with weather data
  lap_weather <- reactive({
    lap_df <- lap_data_enriched()
    weather_df <- weather_data %>% filter(Year == input$season & Round.Number == input$round)
    setDT(lap_df)
    setDT(weather_df)
    setkey(weather_df, time_sec)
    setkey(lap_df, lap_start_time)
    weather_df[lap_df, roll = TRUE]
  })
  
  output$driver_selector <- renderUI({
    req(lap_data())
    selectInput("driverId", "Select Drivers:",
                choices = unique(lap_data()$driverId),
                selected = head(unique(lap_data()$driverId), 3),
                multiple = TRUE)
  })
  
  output$lapTrendPlot <- renderPlot({
    req(lap_data(), input$driverId)
    df <- lap_data() %>% filter(driverId %in% input$driverId)
    if (nrow(df) == 0) {
      plot.new()
      title("No data available for selected drivers.")
      return()
    }
    pit_laps <- df %>%
      group_by(driverId) %>%
      arrange(lap) %>%
      mutate(delta = abs(time_sec - lag(time_sec))) %>%
      filter(delta > 10) %>% pull(lap) %>% unique()
    ggplot(df, aes(x = lap, y = time_sec, color = driverId)) +
      geom_line(size = 1) +
      geom_vline(xintercept = pit_laps, linetype = "dashed", color = "gray") +
      labs(title = "Lap Time Trends", x = "Lap", y = "Lap Time (s)", color = "Driver") +
      theme_minimal()
  })
  
  output$boxPlot <- renderPlot({
    req(lap_data(), input$driverId)
    df <- lap_data() %>% filter(driverId %in% input$driverId)
    ggplot(df, aes(x = driverId, y = time_sec, fill = driverId)) +
      geom_boxplot() +
      labs(title = "Lap Time Variability", x = "Driver", y = "Lap Time (s)") +
      theme_minimal() + theme(legend.position = "none")
  })
  
  output$compoundBoxplot <- renderPlot({
    df <- lap_data() %>% filter(driverId %in% input$driverId)
    df <- assign_tyre_compound(df, tyre_data, input$season, input$round)
    df <- df %>% filter(!is.na(compound))
    ggplot(df, aes(x = compound, y = time_sec, fill = compound)) +
      geom_boxplot() +
      labs(title = "Lap Time by Tyre Compound", x = "Compound", y = "Lap Time (s)") +
      theme_minimal()
  })
  
  output$anovaSummary <- renderPrint({
    df <- lap_data() %>% filter(driverId %in% input$driverId)
    df <- assign_tyre_compound(df, tyre_data, input$season, input$round)
    df <- df %>% filter(!is.na(compound))
    # Check for factor level variation
    if (length(unique(df$compound)) < 2) {
      cat("Not enough tyre compound variation for this round/selection.\n")
      return()
    }
    summary(aov(time_sec ~ compound, data = df))
  })
  
  output$tukeySummary <- renderPrint({
    df <- lap_data() %>% filter(driverId %in% input$driverId)
    df <- assign_tyre_compound(df, tyre_data, input$season, input$round)
    df <- df %>% filter(!is.na(compound))
    # Check for factor level variation
    if (length(unique(df$compound)) < 2) {
      cat("Not enough tyre compound variation for this round/selection.\n")
      return()
    }
    model <- aov(time_sec ~ compound, data = df)
    TukeyHSD(model)
  })
  
  output$kmSummary <- renderPrint({
    pit_df <- gather_pit_data(input$season, input$round)
    if (is.null(pit_df) || nrow(pit_df) == 0) {
      cat("No pit stop data available.\n")
      return()
    }
    # For each driver, the event time is the lap of the first pit stop.
    df_pit <- pit_df %>%
      group_by(driverId) %>%
      summarise(
        timeToPit = min(lap, na.rm = TRUE),
        condition = first(weatherType),
        .groups = "drop"
      )
    # All drivers in df_pit have experienced a pit stop, so event = 1.
    df_pit <- df_pit %>% mutate(event = 1)
    cat("\nDEBUG: df_pit structure in kmSummary:\n")
    str(df_pit)
    cat("DEBUG: df_pit head:\n")
    print(head(df_pit))
    
    fit <- survfit(Surv(time = timeToPit, event = event) ~ condition, data = df_pit)
    summary(fit)
  })
  
  output$kmPlot <- renderPlot({
    pit_df <- gather_pit_data(input$season, input$round)
    req(!is.null(pit_df), nrow(pit_df) > 0)
    df_summary <- tryCatch({
      pit_df %>%
        group_by(driverId) %>%
        summarise(
          lap = min(lap, na.rm = TRUE),
          condition = first(weatherType),
          .groups = "drop"
        ) %>%
        filter(!is.na(lap), lap > 0, !is.na(condition))
    }, error = function(e) { return(data.frame()) })
    cat("\nDEBUG: df_summary structure in kmPlot:\n")
    str(df_summary)
    cat("\nDEBUG: df_summary head:\n")
    print(head(df_summary))
    if (nrow(df_summary) == 0) {
      plot.new()
      title("Kaplan-Meier: Pit Stops (Actual Rainfall)")
      legend("center", legend = "Invalid or empty data", bty = "n")
      return()
    }
    df_summary$condition <- factor(df_summary$condition)
    tryCatch({
      fit <- survfit(
        Surv(time = df_summary$lap, event = rep(1, nrow(df_summary))) ~ condition,
        data = df_summary
      )
      colors <- c("Dry" = "blue", "Wet" = "red", "Unknown" = "gray")
      plot_colors <- colors[levels(df_summary$condition)]
      plot(fit, col = plot_colors, lwd = 2, lty = 1:length(plot_colors),
           main = "Kaplan-Meier: Pit Stops (Actual Rainfall)",
           xlab = "Lap", ylab = "Survival Probability")
      legend("bottomleft", legend = names(plot_colors), col = plot_colors,
             lty = 1:length(plot_colors), lwd = 2, title = "Condition")
    }, error = function(e) {
      plot.new()
      title("Kaplan-Meier: Pit Stops (Actual Rainfall)")
      legend("center", legend = paste("Error:", e$message), bty = "n")
    })
  })
  
  output$kmWinnerPlot <- renderPlot({
    req(input$season, input$round)
    # Fetch results JSON from Ergast API
    url <- sprintf("https://ergast.com/api/f1/%d/%d/results.json", input$season, input$round)
    res <- GET(url)
    parsed <- fromJSON(content(res, as = "text"), flatten = TRUE)
    cat("\nDEBUG: parsed structure in kmWinnerPlot:\n")
    str(parsed)
    races <- parsed$MRData$RaceTable$Races
    cat("\nDEBUG: races structure in kmWinnerPlot:\n")
    str(races)
    if (nrow(races) == 0 || is.null(races$Results[1]) || !is.list(races$Results[[1]])) {
      cat("DEBUG: races does not contain a valid 'Results' field in the first row.\n")
      plot.new()
      title("KM: Winner vs Others Pit Stop Timing")
      legend("center", legend = "Race results not in expected format", bty = "n")
      return()
    }
    results_df <- races$Results[[1]]
    if(nrow(results_df) == 0) {
      cat("DEBUG: The 'Results' data frame is empty.\n")
      plot.new()
      title("KM: Winner vs Others Pit Stop Timing")
      legend("center", legend = "Empty race results", bty = "n")
      return()
    }
    # Use Driver.driverId directly from results_df
    winner_id <- results_df$Driver.driverId[1]
    cat("DEBUG: winner_id:", winner_id, "\n")
    pit_df <- gather_pit_data(input$season, input$round)
    if (is.null(pit_df) || nrow(pit_df) == 0) {
      plot.new()
      title("KM: Winner vs Others Pit Stop Timing")
      legend("center", legend = "No pit stop data available", bty = "n")
      return()
    }
    cat("\nDEBUG: pit_df structure in kmWinnerPlot:\n")
    str(pit_df)
    cat("DEBUG: pit_df head:\n")
    print(head(pit_df))
    pit_df$group <- ifelse(pit_df$driverId == winner_id, "Winner", "Others")
    weather_row <- weather_data %>%
      filter(Year == input$season & Round == input$round) %>%
      summarise(Rainfall = any(Rainfall))
    rain_val <- tryCatch({
      if (is.data.frame(weather_row)) weather_row$Rainfall[1] else NA
    }, error = function(e) NA)
    condition <- ifelse(isTRUE(rain_val), "Wet", "Dry")
    pit_df$condition <- condition
    if (any(is.na(pit_df$lap)) || any(is.na(pit_df$event))) {
      plot.new()
      title("KM: Winner vs Others Pit Stop Timing")
      legend("center", legend = "Invalid data for survival analysis", bty = "n")
      return()
    }
    tryCatch({
      fit <- survfit(Surv(time = pit_df$lap, event = pit_df$event) ~ group, data = pit_df)
      plot(fit, col = c("red", "black"), lwd = 2,
           main = paste("KM: Winner vs Others Pit Stop Timing -", condition),
           xlab = "Lap", ylab = "Survival Probability")
      legend("bottomleft", legend = levels(factor(pit_df$group)), col = c("red", "black"), lwd = 2)
    }, error = function(e) {
      plot.new()
      title("KM: Winner vs Others Pit Stop Timing")
      legend("center", legend = paste("Error:", e$message), bty = "n")
    })
  })
  
  output$degradationPlot <- renderPlot({
    df <- lap_data() %>% filter(driverId %in% input$driverId)
    df <- assign_tyre_compound(df, tyre_data, input$season, input$round)
    df <- df %>% filter(!is.na(compound))
    ggplot(df, aes(x = lap, y = time_sec, color = compound)) +
      geom_point(alpha = 0.4) +
      geom_smooth(method = "loess", se = FALSE, span = 0.4) +
      facet_wrap(~driverId) +
      labs(title = "Tyre Degradation Over Time", x = "Lap", y = "Lap Time (s)") +
      theme_minimal()
  })
  
  output$mixedModelSummary <- renderPrint({
    df <- lap_data() %>% filter(driverId %in% input$driverId)
    df <- assign_tyre_compound(df, tyre_data, input$season, input$round)
    df <- df %>% filter(!is.na(compound))
    # Check if there are enough rows and compound levels
    if (nrow(df) < 2 || length(unique(df$compound)) < 2) {
      cat("Not enough data or tyre compound variation for a mixed model.\n")
      return()
    }
    model <- lmer(time_sec ~ compound + (1 | driverId), data = df)
    summary(model)
  })
  
  output$weatherDisplay <- renderPrint({
    row <- weather_data %>% filter(Year == input$season & Round.Number == input$round)
    if (nrow(row) == 0) return("Weather data not available.")
    avg_row <- row %>% summarise(
      AirTemp = mean(AirTemp, na.rm = TRUE),
      TrackTemp = mean(TrackTemp, na.rm = TRUE),
      Humidity = mean(Humidity, na.rm = TRUE),
      WindSpeed = mean(WindSpeed, na.rm = TRUE),
      WindDirection = mean(WindDirection, na.rm = TRUE),
      Rainfall = any(Rainfall == TRUE)
    )
    condition <- ifelse(avg_row$Rainfall, "Wet", "Dry")
    paste0(
      "Weather: ", condition, "\n",
      "Track Temp: ", round(avg_row$TrackTemp, 1), " °C\n",
      "Air Temp: ", round(avg_row$AirTemp, 1), " °C\n",
      "Humidity: ", round(avg_row$Humidity, 1), "%\n",
      "Wind: ", round(avg_row$WindSpeed, 1), " km/h from ", round(avg_row$WindDirection)
    )
  })
}

shinyApp(ui = ui, server = server)

