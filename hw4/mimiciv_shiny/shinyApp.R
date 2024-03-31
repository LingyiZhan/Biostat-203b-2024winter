library(bigrquery)
library(dbplyr)
library(DBI)
library(gt)
library(gtsummary)
library(tidyverse)
library(shiny)
library(ggplot2)
library(bit64)
# path to the service account token
# change this for reproduce
satoken <- "biostat-203b-2024-winter-313290ce47a6.json"

# BigQuery authentication using service account
bq_auth(path = satoken)
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)
# dbListTables(con_bq)

##################### Figure Count  ####################
icustays_tble <- tbl(con_bq, "icustays") %>%
  collect()

icustays_clean <- icustays_tble %>%
  count(last_careunit) %>%
  rename(count = n)

icustays_Q1 <- quantile(icustays_clean$count, 0.25)
icustays_Q3 <- quantile(icustays_clean$count, 0.75)
icustays_IQR <- icustays_Q3 - icustays_Q1
icustays_low <- icustays_Q1 - 1.5 * icustays_IQR
icustays_up <- icustays_Q3 + 1.5 * icustays_IQR
icustays_clean <- icustays_clean %>% 
  filter(count >= icustays_low, count <= icustays_up) %>%
  collect()

##################### Figure Lab Events  ####################
labevents_tble <- tbl(con_bq, "labevents")
icustays_tble <- tbl(con_bq,"icustays")
specific_items <- c(50912, 50971, 50983, 50902, 50882, 51221, 51301, 50931)
final_labevents <- labevents_tble |>
  inner_join(icustays_tble, by = c("subject_id", "hadm_id")) |>
  filter(itemid %in% specific_items) |>
  group_by(subject_id, itemid) |>
  slice_max(order_by = storetime,n = 1) |>
  ungroup() |>
  pivot_wider(names_from = itemid, values_from = value) |>
  select(subject_id, stay_id, '50912', '50971', '50983', '50902', '50882', '51221', '51301', '50931') |>
  rename(
    creatinine = '50912',
    potassium = '50971',
    sodium = '50983',
    chloride = '50902',
    bicarbonate = '50882',
    hematocrit = '51221',
    white_blood_cell_count = '51301',
    glucose = '50931'
  ) |>
  group_by(subject_id, stay_id) |>
  summarise(
    creatinine = max(creatinine, na.rm = TRUE),
    potassium = max(potassium, na.rm = TRUE),
    sodium = max(sodium, na.rm = TRUE),
    chloride = max(chloride, na.rm = TRUE),
    bicarbonate = max(bicarbonate, na.rm = TRUE),
    hematocrit = max(hematocrit, na.rm = TRUE),
    white_blood_cell_count = max(white_blood_cell_count, na.rm = TRUE),
    glucose = max(glucose, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  collect()
final_labevents <- final_labevents %>%
  select(c("creatinine","potassium","sodium","chloride","bicarbonate","glucose")) %>%
  mutate(across(c(creatinine, potassium, sodium, chloride, bicarbonate, glucose),
                ~ as.numeric(replace(.x, .x == "___", NA))))

labevents_long <- pivot_longer(final_labevents,cols=everything(), names_to = "Variable", values_to = "Value") %>%
  collect()

# remove outliers
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x <- ifelse(x < lower_bound | x > upper_bound, NA, x)
  return(x)
}
final_labevents_cleaned <- final_labevents %>%
  mutate(across(where(is.numeric), remove_outliers))
labevents_long_clean <- pivot_longer(final_labevents_cleaned,cols=everything(), names_to = "Variable", values_to = "Value") %>%
  collect()


##################### Figure Vitals Events  ####################
chartevents_tble <- tbl(con_bq, "chartevents")
final_chartevents <- chartevents_tble |>
  filter(itemid %in% c(220045, 220179, 220180, 223761, 220210)) |>
  inner_join(icustays_tble, by = c("subject_id", "hadm_id","stay_id")) |>
  group_by(subject_id,hadm_id,stay_id) |>
  slice_max(order_by = storetime, n = 1) |>
  ungroup() |>
  pivot_wider(names_from = itemid, values_from = value) |>
  select(subject_id, stay_id,'220045','220179', '220180', '223761', '220210') |>
  rename(
    heartrate = '220045',
    systolic_non_invasive_blood_pressure = '220179',
    diastolic_non_invasive_blood_pressure = '220180',
    temperature_Fahrenheit = '223761',
    respiratory_rate = '220210'
  ) |>
  group_by(subject_id, stay_id)  |>
  summarise(
    heartrate = max(heartrate, na.rm = TRUE),
    systolic_non_invasive_blood_pressure = max(systolic_non_invasive_blood_pressure, na.rm = TRUE),
    diastolic_non_invasive_blood_pressure = max(diastolic_non_invasive_blood_pressure, na.rm = TRUE),
    temperature_Fahrenheit = max(temperature_Fahrenheit, na.rm = TRUE),
    respiratory_rate = max(respiratory_rate, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  collect()

final_chartevents <- final_chartevents %>%
  select(c("heartrate","systolic_non_invasive_blood_pressure","diastolic_non_invasive_blood_pressure","temperature_Fahrenheit","respiratory_rate")) %>%
  mutate(across(c(heartrate, systolic_non_invasive_blood_pressure, diastolic_non_invasive_blood_pressure, temperature_Fahrenheit, respiratory_rate),~ as.numeric(replace(.x, .x == "___", NA))))

# long
chartevents_long <- pivot_longer(final_chartevents,cols=everything(), names_to = "Variable", values_to = "Value") %>%
  collect()

final_chartevents_cleaned <- final_chartevents %>%
  mutate(across(where(is.numeric), remove_outliers))

# long clean
chartevents_long_clean <- pivot_longer(final_chartevents_cleaned,cols=everything(), names_to = "Variable", values_to = "Value") %>%
  collect()


############################################################################################################################################################

ui <- navbarPage("Shiny App",
  tabPanel("Patient characteristics",
    sidebarLayout(
      sidebarPanel(
        selectInput("variable", "Choose a Variable of Interest:",choices = c("Last care unit", "Lab Events","Vitals Events")),checkboxInput("removeOUT", "Remove outliers in IQR method for measurements?", value = FALSE)
      ),
      mainPanel(plotOutput("plot"))
    )
  ),
  tabPanel("Patient's ADT and ICU stay information",
    sidebarLayout(
      sidebarPanel(
        selectInput("patientid", "Select a Patient from ID: ",choices = c("10012055", "10012206","10012292","10012438","10012476","10012552","10012853"))
      ),
      mainPanel(plotOutput("plot2"))
    )
  )
)


server <- function(input, output, session) {
  output$plot <- renderPlot({
    if(input$variable == "Last care unit") {
      if(!input$removeOUT) {
        ggplot(icustays_tble, aes(x = last_careunit)) +
          geom_bar(stat = "count", width = 0.7) +
          coord_flip() +
          xlab("last_careunit") + ylab("Count") +
          ggtitle("Patient Count by stay or patient variable group")+
          theme_minimal() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
        }else{
          ggplot(icustays_clean, aes(x = last_careunit,y = count)) +
          geom_col(width = 0.7) +
          coord_flip() +
          xlab("last_careunit") + ylab("Count") +
          ggtitle("Patient Count by stay or patient variable group (remove outliers in IQR)")+
          theme_minimal() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"))
      }
    }else if (input$variable == "Lab Events") {
       if(!input$removeOUT) {
          ggplot(labevents_long, aes(x = Variable, y = Value)) +
          geom_boxplot() +
          coord_flip() +
          labs(title = "Boxplot for Each Variable", x = "Variable", y = "Value") +
          theme_minimal() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          )
       }else{
          ggplot(labevents_long_clean, aes(x = Variable, y = Value)) +
          geom_boxplot() +
          coord_flip() +
          labs(title = "Boxplot for Each Variable", x = "Variable", y = "Value") +
          theme_minimal() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          )
       }
    }else{
        if(!input$removeOUT) {
          ggplot(chartevents_long, aes(x = Variable, y = Value)) +
          geom_boxplot() +
          coord_flip() +
          labs(title = "Boxplot for Each Variable", x = "Variable", y = "Value") +
          theme_minimal() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          )
       }else{
          ggplot(chartevents_long_clean, aes(x = Variable, y = Value)) +
          geom_boxplot() +
          coord_flip() +
          labs(title = "Boxplot for Each Variable", x = "Variable", y = "Value") +
          theme_minimal() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          )
       }

    }
  })  
  output$plot2 <- renderPlot({
    patient_id <- as.integer64(input$patientid)
    # ADT
    patients <- tbl(con_bq, "patients")
    admissions <- tbl(con_bq, "admissions")
    transfers <- tbl(con_bq, "transfers")
    labevents <- tbl(con_bq, "labevents")
    procedures_icd <- tbl(con_bq, "procedures_icd")
    diagnoses_icd <- tbl(con_bq, "diagnoses_icd")
    d_icd_diagnoses <- tbl(con_bq, "d_icd_diagnoses") %>% collect()

    # filter 
    patient_data <- patients %>% filter(subject_id == patient_id) %>% collect()
    admission_data <- admissions %>% filter(subject_id == patient_id) %>% collect()
    transfer_data <- transfers %>% filter(subject_id == patient_id) %>% collect()
    lab_data <- labevents %>%
      filter(subject_id == patient_id) %>%
      collect()
    procedure_data <- procedures_icd %>% filter(subject_id == patient_id) %>% collect()
    diagnosis_data <- diagnoses_icd %>% filter(subject_id == patient_id) %>% collect()

    # Transform time and factor seq_num
    procedure_data$chartdate <- as.POSIXct(procedure_data$chartdate, format = "%Y/%m/%d", tz = "UTC")
    procedure_data$seq_num <- factor(procedure_data$seq_num)

    # ADT 
    # Filter out discharge events as they do not have an outtime
    adt_data <- transfer_data %>% filter(eventtype != 'discharge')

    shapes <- c(15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
    set.seed(123)
    unique_types <- unique(procedure_data$seq_num)
    shapes_sampled <- sample(shapes, length(unique_types))
    shape_mapping <- setNames(shapes_sampled, unique_types)

    # title
    formatted_title <- sprintf("Patient: %s, %s, %s years old, %s ", patient_data$subject_id[1], patient_data$gender[1], patient_data$anchor_age[1],admission_data$race[1])

    # subtitle
    joined_df <- left_join(diagnosis_data, d_icd_diagnoses, by = c("icd_code"))
    top_long_titles <- joined_df %>%
      select(long_title) %>%
      slice(1:3)
    formatted_subtitle <- paste(top_long_titles$long_title, collapse = "\n")

    ggplot() +
      labs(title = formatted_title)+
      labs(subtitle = formatted_subtitle)+
      geom_segment(data = adt_data, aes(x = intime, xend = outtime, y = 3, yend = 3, colour = careunit), size = ifelse(grepl("CU", adt_data$careunit), 4, 1)) +
      geom_point(data = lab_data , aes(x = charttime, y = 2), shape = 3, size = 3) +
      geom_point(data = procedure_data, aes(x = chartdate, y = 1, shape = seq_num),colour = "black", size = 3) +
      scale_shape_manual(values = shape_mapping) +
      scale_x_datetime(name = "Calender Time") +
      scale_y_continuous(name = "", limits = c(0, 4), breaks = c(1, 2, 3), labels = c("Procedure", "Lab", "ADT")) + 
      scale_color_discrete(name = "Care Unit")+
      scale_shape_discrete(name = "Procedure")+
      theme(
      panel.border = element_rect(color = "black", size = 1, fill = NA), 
      legend.box = "vertical"
      )+
      theme(axis.text.y = element_text(), axis.ticks.y = element_line())
  })
  
}

shinyApp(ui = ui, server = server)
