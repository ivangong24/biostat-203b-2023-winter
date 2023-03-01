## ---------------------------
##
## Script name: app.R
##
## Purpose of script: To create a shiny app for demonstrating variable summaries
##
## Author: Yufan Gong
##
## Date Created: 2023-02-25
##
## Copyright (c) Yufan Gong, 2023
## Email: ivangong@ucla.edu
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


# Load packages -----------------------------------------------------------

library(shiny)
library(ggthemes)
library(gt)
library(rlang)
library(gtsummary)
library(tidyverse)


# Create functions --------------------------------------------------------

extreme_remove_iqr <- function(x) {
  Q = quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr = IQR(x, na.rm = TRUE)
  x = if_else(x < Q[1]-1.5*iqr | x > Q[2]+1.5*iqr, NA, x)
}

quote_all <- function(...){
  args <- ensyms(...)
  paste(map(args, as_string), sep = "")
}


# Load data and create tables/plots ---------------------------------------

icu_cohort <- read_rds("icu_cohort.rds") %>% 
  as_tibble() %>% 
  mutate_at(vars(thirty_day_mort), fct_rev)

demo_vars <- quote_all(ethnicity, language, insurance, marital_status, 
                       gender, age_at_admission)

lab_vars <- quote_all(creatinine, potassium, sodium, chloride, 
                      bicarbonate, hematocrit, white_blood_cells, glucose)

vital_vars <- quote_all(heart_rate, non_invasive_blood_pressure_mean, 
                        temperature_fahrenheit, respiratory_rate, 
                        non_invasive_blood_pressure_systolic)

list(
  list(demo_vars, lab_vars, vital_vars), 
  list("Table 1. Demographic characteristics of MIMIC IV", 
       "Table 2. Lab measurements of MIMIC IV", 
       "Table 3. Vital measurements of MIMIC IV")
  ) %>% 
  pmap(function(key_vars, title){
    icu_cohort %>% 
      select(
        all_of(key_vars), thirty_day_mort
      ) %>% 
      tbl_summary(
        by = thirty_day_mort, missing = "no"
      ) %>% 
      add_n() %>% 
      add_overall() %>% 
      modify_header(label = "**Characteristics**") %>%
      modify_spanning_header(
        starts_with("stat_") ~ "**Thirty day mortality**"
      ) %>%
      modify_caption(title) %>%
      bold_labels() %>% 
      as_gt()
  }) %>% 
  set_names("demo_table", "lab_table", "vital_table") %>% 
  list2env(envir = .GlobalEnv)


## Demographic plots
# iteration of ggplots (categorical variables)
demo_plots_1 <- quote_all(ethnicity, language, insurance, 
                          marital_status, gender) %>% 
  map(
    ~icu_cohort %>% 
      ggplot(
        aes(
          x = .data[[.x]], 
          fill = thirty_day_mort
          )
        ) + 
      geom_bar(position = "dodge") + 
      scale_fill_colorblind() + 
      labs(
        x = .x,  
        y = "Number of subjects", 
        fill = "30 day mortality"
        ) + 
      coord_flip() + 
      theme_classic()
    )

# ggplot of numeric variable (age at admission)
demo_plots_2 <- icu_cohort %>% 
  ggplot(
    aes(
      x = thirty_day_mort, 
      y = age_at_admission, 
      color = thirty_day_mort
      )
    ) + 
  geom_boxplot(na.rm = T, show.legend = FALSE) + 
  scale_color_colorblind() +
  labs(
    x = "30 day mortality",
    y = "Age at admission"
    ) +
  theme_classic()

demo_plot <- demo_plots_1 %>% 
  append(list(demo_plots_2), after = 5)


## Lab plots
lab_plot <- lab_vars %>% 
  map(
    ~icu_cohort %>% 
      mutate_if(is.numeric, extreme_remove_iqr) %>% 
      ggplot(
        aes(
          x = thirty_day_mort, 
          y = .data[[.x]], 
          color = thirty_day_mort
          )
        ) + 
      geom_boxplot(na.rm = T, show.legend = FALSE) + 
      scale_color_colorblind() + 
      labs(
        x = "30 day mortality", 
        y = .x
        ) + 
      theme_classic()
    )


## Vital plots

vital_plot <- vital_vars %>% 
  map(
    ~icu_cohort %>% 
      mutate_if(is.numeric, extreme_remove_iqr) %>% 
      ggplot(
        aes(
          x = thirty_day_mort, 
          y = .data[[.x]], 
          color = thirty_day_mort
          )
        ) + 
      geom_boxplot(na.rm = T, show.legend = FALSE) + 
      scale_color_colorblind() + 
      labs(
        x = "30 day mortality", 
        y = .x
        ) + 
      theme_classic()
    )


# Construct app UI --------------------------------------------------------

ui <- fluidPage(
  # App title ----
  titlePanel("Visualization and summary of variables"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: drop-down list for Demographic variables ----
      selectInput(
        inputId = "demo", 
        label = "Demographic characteristics", 
        choices = c(
          "ethnicity", 
          "language", 
          "insurance",
          "marital status",
          "gender",
          "age at admission"
        )
      ),
      
      # Input: drop-down list for Lab measurements ----
      selectInput(
        inputId = "lab", 
        label = "Lab measurements", 
        choices = c(
          "creatinine", 
          "potassium", 
          "sodium",
          "chloride",
          "bicarbonate",
          "hematocrit",
          "white blood cell count",
          "glucose"
          )
        ),
      
      # Input: drop-down list for Vital measurements ----
      selectInput(
        inputId = "vital",
        label = "Vital measurements",
        choices = c(
          "heart rate", 
          "mean non-invasive blood pressure", 
          "systolic non-invasive blood pressure",
          "body temperature in Fahrenheit",
          "respiratory rate"
          )
        )
      ), 
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        # Input: 
        tabPanel(
          title = "Demographics", 
          gt_output("demo_table"), 
          plotOutput("demo_plot")
          ),      
        tabPanel(
          title = "Lab Measurements", 
          gt_output("lab_table"), 
          plotOutput("lab_plot")
          ),
        tabPanel(
          title = "Vital measurements", 
          gt_output("vital_table"), 
          plotOutput("vital_plot")
          )
        )
      )
    )
  )


# Create server -----------------------------------------------------------

server <- function(input, output) {
  
  demo_plot_source <- reactive({
    switch(
      input$demo, 
      "ethnicity" = demo_plot[[1]], 
      "language" = demo_plot[[2]], 
      "insurance" = demo_plot[[3]], 
      "marital status" = demo_plot[[4]], 
      "gender" = demo_plot[[5]], 
      "age at admission" = demo_plot[[6]]
      )
    })
  
  lab_plot_source <- reactive({
    switch(
      input$lab, 
      "creatinine" = lab_plot[[1]], 
      "potassium" = lab_plot[[2]], 
      "sodium" = lab_plot[[3]], 
      "chloride" = lab_plot[[4]], 
      "bicarbonate" = lab_plot[[5]], 
      "hematocrit" = lab_plot[[6]], 
      "white blood cell count" = lab_plot[[7]], 
      "glucose" = lab_plot[[8]]
      )
    })
  
  vital_plot_source <- reactive({
    switch(
      input$vital, 
      "heart rate" = vital_plot[[1]], 
      "mean non-invasive blood pressure" = vital_plot[[2]], 
      "body temperature in Fahrenheit" = vital_plot[[3]], 
      "respiratory rate" = vital_plot[[4]], 
      "systolic non-invasive blood pressure" = vital_plot[[5]]
      )
    })
  
  output$demo_table <- render_gt(demo_table)
  output$lab_table <- render_gt(lab_table)
  output$vital_table <- render_gt(vital_table)
  
  output$demo_plot <- renderPlot({demo_plot_source()})
  output$lab_plot <- renderPlot({lab_plot_source()})
  output$vital_plot <- renderPlot({vital_plot_source()})
  
  }

shinyApp(ui = ui, server = server)
