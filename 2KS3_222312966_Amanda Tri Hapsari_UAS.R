library(shiny)
library(shinydashboard)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)
library(readr)
library(corrplot)
library(car)
library(nortest)
library(lmtest)
library(sf)
library(geojsonio)
library(sp)
library(tmap)
##library(downloadHandler)
library(knitr)
library(rmarkdown)

# Load GeoJSON data
sovi_geojson <- geojson_read("https://raw.githubusercontent.com/amandatrih/projek-dashboard/main/sovi_data.geojson", what = "sp")
sovi_data <- as.data.frame(sovi_geojson@data)

# Keep distance matrix for now (if still needed)
distance_matrix <- read_csv("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv")

my_theme <- bs_theme(
  bg = "#191D24",
  fg = "#EFF7FF",
  primary = "#9EF09E",
  secondary = "#DADFE9",
  base_font = font_google("Roboto"),
  success = "#51CD37",
  danger = "#FF5005"
)

custom_css <- "
.card, .well, .panel {
  background-color: #232935 !important;
  border: none;
  color: #EFF7FF;
}

.card-title, .panel-heading {
  background-color: #232935 !important;
  color: #EFF7FF;
  font-weight: bold;
}

body, .main-header, .main-sidebar, .content-wrapper {
  background-color: #191D24 !important;
}

.skin-blue .main-sidebar {
  background-color: #232935 !important;
}

.skin-blue .sidebar-menu > li > a {
  color: #EFF7FF !important;
}

.skin-blue .sidebar-menu > li.active > a, 
.skin-blue .sidebar-menu > li:hover > a {
  background-color: #191D24 !important;
  color: #9EF09E !important;
}

.skin-blue .main-header .navbar {
  background-color: #232935 !important;
}
.skin-blue .main-header .logo {
  background-color: #232935 !important;
  color: #EFF7FF !important;
  border-bottom: none !important;
}
.skin-blue .main-header .logo:hover {
  background-color: #232935 !important;
  color: #9EF09E !important;
}
.skin-blue .main-header .navbar .sidebar-toggle {
  color: #EFF7FF !important;
}
.skin-blue .main-header .navbar .sidebar-toggle:hover {
  background-color: #191D24 !important;
  color: #9EF09E !important;
}

.download-section {
  background-color: #232935;
  padding: 15px;
  margin: 10px 0;
  border-radius: 5px;
  border-left: 4px solid #9EF09E;
}

.interpretation-box {
  background-color: #2D3748;
  padding: 15px;
  margin: 10px 0;
  border-radius: 5px;
  border-left: 4px solid #51CD37;
  color: #EFF7FF !important;
}

.metadata-card {
  background-color: #232935;
  padding: 20px;
  margin: 10px 0;
  border-radius: 8px;
  color: #EFF7FF;
}

.box {
  background-color: #232935 !important;
  border: 1px solid #2D3748 !important;
  border-radius: 8px !important;
}

.box-header {
  background-color: #2D3748 !important;
  color: #EFF7FF !important;
  border-bottom: 1px solid #2D3748 !important;
}

.box-title {
  color: #EFF7FF !important;
  font-weight: bold !important;
}

.btn-success {
  background-color: #51CD37 !important;
  border-color: #51CD37 !important;
  color: #EFF7FF !important;
}

.btn-success:hover {
  background-color: #45a02e !important;
  border-color: #45a02e !important;
  color: #EFF7FF !important;
}

.btn-primary {
  background-color: #9EF09E !important;
  border-color: #9EF09E !important;
  color: #191D24 !important;
}

.btn-primary:hover {
  background-color: #8dd88d !important;
  border-color: #8dd88d !important;
  color: #191D24 !important;
}

.btn-info {
  background-color: #92B0FF !important;
  border-color: #92B0FF !important;
  color: #191D24 !important;
}

.btn-info:hover {
  background-color: #7a9eff !important;
  border-color: #7a9eff !important;
  color: #191D24 !important;
}

.form-control {
  background-color: #2D3748 !important;
  border: 1px solid #3A4553 !important;
  color: #EFF7FF !important;
}

.form-control:focus {
  background-color: #2D3748 !important;
  border-color: #9EF09E !important;
  color: #EFF7FF !important;
  box-shadow: 0 0 0 0.2rem rgba(158, 240, 158, 0.25) !important;
}

.selectize-input {
  background-color: #2D3748 !important;
  border: 1px solid #3A4553 !important;
  color: #EFF7FF !important;
}

.selectize-dropdown {
  background-color: #2D3748 !important;
  border: 1px solid #3A4553 !important;
}

.selectize-dropdown-content .option {
  color: #EFF7FF !important;
}

.selectize-dropdown-content .option.active {
  background-color: #9EF09E !important;
  color: #191D24 !important;
}

.dataTables_wrapper {
  background-color: #232935 !important;
  color: #EFF7FF !important;
}

.dataTables_filter input {
  background-color: #2D3748 !important;
  border: 1px solid #3A4553 !important;
  color: #EFF7FF !important;
}

.dataTables_length select {
  background-color: #2D3748 !important;
  border: 1px solid #3A4553 !important;
  color: #EFF7FF !important;
}

.dataTables_info {
  color: #DADFE9 !important;
}

.dataTables_paginate .paginate_button {
  background-color: #2D3748 !important;
  border: 1px solid #3A4553 !important;
  color: #EFF7FF !important;
}

.dataTables_paginate .paginate_button:hover {
  background-color: #9EF09E !important;
  color: #191D24 !important;
}

.dataTables_paginate .paginate_button.current {
  background-color: #9EF09E !important;
  color: #191D24 !important;
}

table.dataTable {
  background-color: #232935 !important;
  color: #EFF7FF !important;
}

table.dataTable thead th {
  background-color: #2D3748 !important;
  color: #EFF7FF !important;
  border-bottom: 1px solid #3A4553 !important;
}

table.dataTable tbody td {
  border-bottom: 1px solid #3A4553 !important;
  text-align: center !important;
  vertical-align: middle !important;
}

table.dataTable thead th {
  text-align: center !important;
  vertical-align: middle !important;
  font-weight: bold !important;
}

.dataTables_wrapper .dataTable {
  border: 1px solid #3A4553 !important;
}



.dataTables_wrapper .dataTable tbody tr:last-child td {
  border-bottom: none !important;
}

.dataTables_wrapper .dataTable tbody tr:hover {
  background-color: #2D3748 !important;
}

.dataTables_wrapper .dataTable tbody tr:nth-child(even) {
  background-color: rgba(45, 55, 72, 0.3) !important;
}

.radio label, .checkbox label {
  color: #EFF7FF !important;
}

label {
  color: #DADFE9 !important;
  font-weight: 500 !important;
}

.control-label {
  color: #DADFE9 !important;
  font-weight: 500 !important;
}

h5 {
  color: #9EF09E !important;
  font-weight: 600 !important;
}

.box-title {
  color: #9EF09E !important;
  font-weight: bold !important;
}

.card-title {
  color: #9EF09E !important;
  font-weight: bold !important;
}

.panel-heading {
  color: #9EF09E !important;
  font-weight: bold !important;
}

.radio input[type='radio']:checked + label::before {
  background-color: #9EF09E !important;
  border-color: #9EF09E !important;
}

.checkbox input[type='checkbox']:checked + label::before {
  background-color: #9EF09E !important;
  border-color: #9EF09E !important;
}

.numeric-input {
  background-color: #2D3748 !important;
  border: 1px solid #3A4553 !important;
  color: #EFF7FF !important;
}

.numeric-input:focus {
  border-color: #9EF09E !important;
  box-shadow: 0 0 0 0.2rem rgba(158, 240, 158, 0.25) !important;
}

.plotly {
  background-color: #232935 !important;
}

.leaflet-container {
  background-color: #232935 !important;
}

.leaflet-popup-content-wrapper {
  background-color: #232935 !important;
  color: #EFF7FF !important;
}

.leaflet-popup-tip {
  background-color: #232935 !important;
}

hr {
  border-color: #3A4553 !important;
}

.text-muted {
  color: #DADFE9 !important;
}

p {
  color: #DADFE9 !important;
}

.box-body {
  color: #DADFE9 !important;
}

.card-body {
  color: #DADFE9 !important;
}

.panel-body {
  color: #DADFE9 !important;
}

.help-block {
  color: #92B0FF !important;
  font-style: italic !important;
}

.small {
  color: #DADFE9 !important;
}

.text-info {
  color: #92B0FF !important;
}

.text-success {
  color: #51CD37 !important;
}

.text-warning {
  color: #F5DC71 !important;
}

.text-danger {
  color: #FF5005 !important;
}

#categorization_interpretation {
  color: #EFF7FF !important;
  font-size: 14px !important;
  line-height: 1.5 !important;
}

#exploration_interpretation {
  color: #EFF7FF !important;
  font-size: 14px !important;
  line-height: 1.5 !important;
}

#assumption_interpretation {
  color: #EFF7FF !important;
  font-size: 14px !important;
  line-height: 1.5 !important;
}

#inference_interpretation {
  color: #EFF7FF !important;
  font-size: 14px !important;
  line-height: 1.5 !important;
}

#regression_interpretation {
  color: #EFF7FF !important;
  font-size: 14px !important;
  line-height: 1.5 !important;
}

.alert-success {
  background-color: rgba(81, 205, 55, 0.2) !important;
  border-color: #51CD37 !important;
  color: #EFF7FF !important;
}

.alert-danger {
  background-color: rgba(255, 80, 5, 0.2) !important;
  border-color: #FF5005 !important;
  color: #EFF7FF !important;
}

.alert-info {
  background-color: rgba(146, 176, 255, 0.2) !important;
  border-color: #92B0FF !important;
  color: #EFF7FF !important;
}

.alert-warning {
  background-color: rgba(245, 220, 113, 0.2) !important;
  border-color: #F5DC71 !important;
  color: #EFF7FF !important;
}
+/* Hover effect for Fitur Dashboard table */
+.table-dark.table-hover tbody tr:hover {
+  background-color: #2D3748 !important;
+  color: #9EF09E !important;
+}
"

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "SSA Dashboard"),
  dashboardSidebar(
    tags$head(tags$style(HTML(custom_css))),
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("Manajemen Data", tabName = "manajemen_data", icon = icon("database")),
      menuItem("Eksplorasi Data", tabName = "eksplorasi_data", icon = icon("chart-bar")),
      menuItem("Uji Asumsi Data", tabName = "uji_asumsi", icon = icon("check-circle")),
      menuItem("Statistik Inferensia", tabName = "statistik_inferensia", icon = icon("calculator")),
      menuItem("Regresi Linear Berganda", tabName = "regresi_linear", icon = icon("project-diagram"))
    )
  ),
  dashboardBody(
    theme = my_theme,
    tabItems(
      # =================== BERANDA ===================
      tabItem(tabName = "beranda",
              fluidRow(
                column(12,
                  div(class = "metadata-card",
                    h2("SELAMAT DATANG DI SELF-SERVICE ANALYTIC DASHBOARD", style = "color: #9EF09E; font-weight: bold; text-align:center;"),
                  )
                )
              ),
              fluidRow(
                valueBoxOutput("box_prov", width = 3),
                valueBoxOutput("box_kab", width = 3),
                valueBoxOutput("box_var", width = 3),
                valueBoxOutput("box_row", width = 3)
              ),
              fluidRow(
                column(12,
                  div(class = "metadata-card",
                    h4("Tujuan Dashboard:", style = "color: #51CD37; font-weight:bold;"),
                    tags$ul(
                      tags$li("Menyediakan alat bantu analisis berbasis data yang mudah digunakan oleh akademisi, peneliti, maupun pengambil kebijakan, khususnya dalam konteks kerentanan sosial."),
                      tags$li("Menyajikan pola kerentanan sosial secara visual, interaktif, dan terstruktur melalui grafik statistik, peta tematik, serta tabel eksploratif."),
                      tags$li(HTML("Mendukung proses pembelajaran dan pengembangan keterampilan dalam penerapan <b>komputasi statistik</b> dan sistem informasi berbasis data terbuka dengan studi kasus nyata dari data SOVI."))
                    )
                  )
                )
              ),
              fluidRow(
                column(12,
                  div(class = "metadata-card",
                    h4("Fitur Dashboard:", style = "color: #51CD37; font-weight:bold;"),
                    tags$table(class = "table table-bordered table-hover table-dark", style = "width:100%; background-color:#232935; color:#EFF7FF;",
                      tags$tr(tags$th("Manajemen Data:"), tags$td("Kategorisasi data kontinyu")),
                      tags$tr(tags$th("Eksplorasi Data:"), tags$td("Statistik deskriptif, visualisasi, peta")),
                      tags$tr(tags$th("Uji Asumsi:"), tags$td("Uji normalitas dan homogenitas")),
                      tags$tr(tags$th("Statistik Inferensia:"), tags$td("Uji hipotesis untuk 1 dan 2 sampel")),
                      tags$tr(tags$th("ANOVA:"), tags$td("Analisis varian satu dan dua arah")),
                      tags$tr(tags$th("Regresi Linear Berganda:"), tags$td("Analisis regresi dengan uji asumsi"))
                    )
                  )
                )
              ),
              fluidRow(
                column(12,
                  div(class = "metadata-card",
                    h4("Metadata:", style = "color: #51CD37; font-weight:bold;"),
                    tags$div(
                      h5("Informasi Dataset:", style = "color:#9EF09E; font-weight:bold;"),
                      tags$ul(
                        tags$li("Dataset: Data Indeks Kerentanan Sosial (SoVI)"),
                        tags$li(HTML('Sumber: <a href="https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv" target="_blank">sovi_data.csv</a>')),
                        tags$li(HTML('Dataset SoVI + Spasial: <a href="https://raw.githubusercontent.com/amandatrih/projek-dashboard/main/sovi_data.geojson" target="_blank">sovi_data.geojson</a>')),
                        tags$li(HTML('Matrik Penimbang Jarak: <a href="https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv" target="_blank">distance.csv</a>')),
                        tags$li(HTML('Referensi Metadata: <a href="https://www.sciencedirect.com/science/article/pii/S2352340921010180" target="_blank">link</a>'))
                      )
                    ),
                    h5("Informasi Variabel:", style = "color:#9EF09E; font-weight:bold; margin-top:15px;"),
                    DT::dataTableOutput("tabel_varinfo")
                  )
                )
              ),
              fluidRow(
                column(12,
                  div(class = "metadata-card",
                    h4("Informasi Pengembang:", style = "color: #51CD37; font-weight:bold;"),
                    fluidRow(
                      column(3,
                        tags$img(src = "amanda.jpg", style = "width:100%; max-width:180px; aspect-ratio:1/1; border-radius:50%; border:2px solid #9EF09E; object-fit:cover;", onerror = "this.src='https://ui-avatars.com/api/?name=A+T+Hapsari&background=232935&color=9EF09E&size=180'")
                      ),
                      column(9,
                        tags$div(style="margin-top:10px;",
                          tags$b("Nama : Amanda Tri Hapsari"), tags$br(),
                          "NIM : 222312966", tags$br(),
                          "Kelas : 2KS3", tags$br(),
                          "Institusi : Politeknik Statistika STIS"
                        )
                      )
                    ),
                    div(style="margin-top:15px;",
                      p("Dashboard ini dibuat untuk memenuhi UAS Mata Kuliah Komputasi Statistik"),
                      p("Program Studi DIV Komputasi Statistik – Politeknik Statistika STIS"),
                      p("Semester Genap TA. 2024/2025")
                    )
                  )
                )
              )
      ),
      
      # =================== MANAJEMEN DATA ===================
      tabItem(tabName = "manajemen_data",
              fluidRow(
                column(4,
                       box(title = "Opsi Manajemen Data", status = "primary", solidHeader = TRUE, width = NULL,
                           selectInput("var_to_categorize", "Pilih Variabel untuk Dikategorikan:",
                                       choices = NULL),
                           numericInput("num_categories", "Jumlah Kategori:", value = 3, min = 2, max = 10),
                           selectInput("categorize_method", "Metode Kategorisasi:",
                                       choices = list("Equal Intervals" = "equal",
                                                      "Quantiles" = "quantile",
                                                      "Custom Breaks" = "custom")),
                           conditionalPanel(
                             condition = "input.categorize_method == 'custom'",
                             textInput("custom_breaks", "Masukkan batas (pisahkan dengan koma):", 
                                       placeholder = "misal: 0, 10, 20, 30")
                           ),
                           selectInput("label_type", "Tipe Label:",
                                       choices = list("Numerik (1, 2, 3...)" = "numeric",
                                                      "Custom Labels" = "custom_labels")),
                           conditionalPanel(
                             condition = "input.label_type == 'custom_labels'",
                             textInput("custom_labels", "Masukkan label kustom (pisahkan dengan koma):", 
                                       placeholder = "misal: Rendah, Sedang, Tinggi")
                           ),
                           actionButton("apply_categorization", "Terapkan Kategorisasi", class = "btn-success"),
                           br(), br(),
                           h5("Pratinjau Data:"),
                           checkboxInput("show_original", "Tampilkan Data Asli", value = TRUE),
                           checkboxInput("show_modified", "Tampilkan Data Setelah Kategorisasi", value = FALSE),
                           radioButtons("display_columns", "Kolom yang Ditampilkan:",
                                       choices = list("Semua Kolom" = "all",
                                                      "Hanya Kolom Kategorisasi" = "categorized_only"),
                                       selected = "all")
                       )
                ),
                
                column(8,
                       box(title = "Tabel Data", status = "primary", solidHeader = TRUE, width = NULL,
                           DT::dataTableOutput("data_table_mgmt")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       box(title = "Hasil Kategorisasi & Interpretasi", status = "info", solidHeader = TRUE, width = NULL,
                           DT::dataTableOutput("categorization_table"),
                           br(),
                           div(class = "interpretation-box",
                               h5("Interpretasi:", style = "color: #51CD37;"),
                               textOutput("categorization_interpretation")
                           )
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       div(class = "download-section",
                           h4("Opsi Unduh", style = "color: #9EF09E;"),
                           downloadButton("download_data_mgmt", "Unduh Tabel Data", 
                                          class = "btn btn-success", style = "margin: 5px;"),
                           downloadButton("download_mgmt_report", "Unduh Laporan Manajemen", 
                                          class = "btn btn-success", style = "margin: 5px;")
                       )
                )
              )
      ),
      
      # =================== EKSPLORASI DATA ===================
      tabItem(tabName = "eksplorasi_data",
              fluidRow(
                column(3,
                       box(title = "Opsi Eksplorasi", status = "primary", solidHeader = TRUE, width = NULL,
                           selectInput("explore_var", "Pilih Variabel:",
                                       choices = NULL),
                           radioButtons("explore_type", "Tipe Analisis:",
                                        choices = list("Statistik Deskriptif" = "desc",
                                                       "Histogram" = "hist",
                                                       "Boxplot" = "box",
                                                       "Matriks Korelasi" = "corr",
                                                       "Plot Scatter" = "scatter",
                                                       "Peta Geografis" = "map")),
                           conditionalPanel(
                             condition = "input.explore_type == 'map'",
                             radioButtons("map_type", "Tipe Peta:",
                                          choices = list("Peta Variabel" = "variable",
                                                         "Peta Cluster" = "cluster"))
                           ),
                           conditionalPanel(
                             condition = "input.explore_type == 'scatter'",
                             selectInput("scatter_y", "Variabel Y:", choices = NULL)
                           ),
                           conditionalPanel(
                             condition = "input.explore_type == 'map' && input.map_type == 'cluster'",
                             selectInput("cluster_vars", "Pilih Variabel untuk Clustering:", 
                                        choices = NULL, multiple = TRUE),
                             numericInput("n_clusters", "Jumlah Cluster (k):", 
                                         value = 3, min = 2, max = 10)
                           ),
                           actionButton("generate_exploration", "Buat Analisis", class = "btn-success")
                       )
                ),
                
                column(9,
                       box(title = "Hasil Eksplorasi", status = "primary", solidHeader = TRUE, width = NULL,
                           conditionalPanel(
                             condition = "input.explore_type == 'desc'",
                             verbatimTextOutput("desc_stats")
                           ),
                           conditionalPanel(
                             condition = "input.explore_type != 'desc'",
                             plotlyOutput("exploration_plot", height = "500px")
                           ),
                           conditionalPanel(
                             condition = "input.explore_type == 'map'",
                             leafletOutput("geo_map", height = "500px")
                           )
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       box(title = "Tabel Data", status = "info", solidHeader = TRUE, width = NULL,
                           DT::dataTableOutput("exploration_table")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       div(class = "interpretation-box",
                           h5("Interpretasi:", style = "color: #51CD37;"),
                           textOutput("exploration_interpretation")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       div(class = "download-section",
                           h4("Opsi Unduh", style = "color: #9EF09E;"),
                           downloadButton("download_exploration_plot", "Unduh Plot (JPG)", 
                                          class = "btn btn-success", style = "margin: 5px;"),
                           downloadButton("download_exploration_table", "Unduh Tabel", 
                                          class = "btn btn-success", style = "margin: 5px;"),
                           downloadButton("download_exploration_report", "Unduh Laporan Eksplorasi", 
                                          class = "btn btn-success", style = "margin: 5px;"),
                           downloadButton("download_exploration_page", "Unduh Halaman Lengkap", 
                                          class = "btn btn-success", style = "margin: 5px;"),
                           conditionalPanel(
                             condition = "input.explore_type == 'map' && input.map_type == 'cluster'",
                             downloadButton("download_cluster_results", "Unduh Hasil Cluster", 
                                            class = "btn btn-success", style = "margin: 5px;")
                           )
                       )
                )
              )
      ),
      
      # =================== UJI ASUMSI DATA ===================
      tabItem(tabName = "uji_asumsi",
              fluidRow(
                column(4,
                       box(title = "Opsi Uji Asumsi", status = "primary", solidHeader = TRUE, width = NULL,
                           selectInput("assumption_var", "Pilih Variabel:",
                                       choices = NULL),
                           selectInput("assumption_group", "Variabel Grup (opsional):",
                                       choices = NULL),
                           radioButtons("test_type", "Tipe Uji:",
                                        choices = list("Uji Normalitas" = "normality",
                                                       "Uji Homogenitas" = "homogeneity")),
                           conditionalPanel(
                             condition = "input.test_type == 'normality'",
                             selectInput("normality_test", "Uji Normalitas:",
                                         choices = list("Shapiro-Wilk" = "shapiro",
                                                        "Anderson-Darling" = "ad",
                                                        "Kolmogorov-Smirnov" = "ks"))
                           ),
                           conditionalPanel(
                             condition = "input.test_type == 'homogeneity'",
                             selectInput("homogeneity_test", "Uji Homogenitas:",
                                         choices = list("Uji Levene" = "levene",
                                                        "Uji Bartlett" = "bartlett",
                                                        "Uji Fligner-Killeen" = "fligner"))
                           ),
                           actionButton("run_assumption_test", "Jalankan Uji", class = "btn-success")
                       )
                ),
                
                column(8,
                       box(title = "Hasil Uji", status = "primary", solidHeader = TRUE, width = NULL,
                           verbatimTextOutput("assumption_results"),
                           br(),
                           plotlyOutput("assumption_plot")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       div(class = "interpretation-box",
                           h5("Interpretasi:", style = "color: #51CD37;"),
                           textOutput("assumption_interpretation")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       div(class = "download-section",
                           h4("Opsi Unduh", style = "color: #9EF09E;"),
                           downloadButton("download_assumption_plot", "Unduh Plot (JPG)", 
                                          class = "btn btn-success", style = "margin: 5px;"),
                           downloadButton("download_assumption_results", "Unduh Hasil", 
                                          class = "btn btn-success", style = "margin: 5px;"),
                           downloadButton("download_assumption_page", "Unduh Halaman Lengkap", 
                                          class = "btn btn-success", style = "margin: 5px;")
                       )
                )
              )
      ),
      
      # =================== STATISTIK INFERENSIA ===================
      tabItem(tabName = "statistik_inferensia",
              fluidRow(
                column(4,
                       box(title = "Statistik Inferensia", status = "primary", solidHeader = TRUE, width = NULL,
                           selectInput("inference_type", "Tipe Analisis:",
                                       choices = list("Uji t Satu Sampel" = "one_t",
                                                      "Uji t Dua Sampel" = "two_t",
                                                      "Uji Proporsi Satu Sampel" = "one_prop",
                                                      "Uji Proporsi Dua Sampel" = "two_prop",
                                                      "Uji Variansi Satu Sampel" = "one_var",
                                                      "Uji Variansi Dua Sampel" = "two_var",
                                                      "Uji ANOVA Satu Arah" = "anova1",
                                                      "Uji ANOVA Dua Arah" = "anova2")),
                           
                           selectInput("inference_var", "Variabel Utama:",
                                       choices = NULL),
                           
                           conditionalPanel(
                             condition = "input.inference_type.indexOf('anova') != -1",
                             selectInput("inference_group", "Variabel Grup:",
                                         choices = NULL)
                           ),
                           
                           conditionalPanel(
                             condition = "input.inference_type == 'anova2'",
                             selectInput("inference_group2", "Variabel Grup Kedua:",
                                         choices = NULL)
                           ),
                           
                           conditionalPanel(
                             condition = "input.inference_type.indexOf('two') != -1",
                             selectInput("inference_var2", "Variabel Kedua:",
                                         choices = NULL)
                           ),
                           
                           conditionalPanel(
                             condition = "input.inference_type.indexOf('one') == 0",
                             numericInput("test_value", "Nilai Uji:", value = 0)
                           ),
                           
                           conditionalPanel(
                             condition = "input.inference_type.endsWith('prop')",
                             numericInput("cutpoint", "Cutpoint:", value = 0)
                           ),
                           
                           conditionalPanel(
                             condition = "input.inference_type.indexOf('two') == 0",
                             selectInput("test_cat", "Hipotesis alternatif:",
                                          choices = list("Tidak sama dengan",
                                                         "Kurang dari",
                                                         "Lebih dari"))),
                           
                           numericInput("alpha_level", "Tingkat Signifikansi:", value = 0.05, min = 0.01, max = 0.10, step = 0.01),
                           
                           actionButton("run_inference_test", "Jalankan Uji", class = "btn-success")
                       )
                ),
                
                column(8,
                       box(title = "Hasil Uji", status = "primary", solidHeader = TRUE, width = NULL,
                           verbatimTextOutput("inference_results"),
                           br(),
                           plotlyOutput("inference_plot")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       div(class = "interpretation-box",
                           h5("Interpretasi:", style = "color: #51CD37;"),
                           textOutput("inference_interpretation")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       div(class = "download-section",
                           h4("Opsi Unduh", style = "color: #9EF09E;"),
                           downloadButton("download_inference_plot", "Unduh Plot (JPG)", 
                                          class = "btn btn-success", style = "margin: 5px;"),
                           downloadButton("download_inference_results", "Unduh Hasil", 
                                          class = "btn btn-success", style = "margin: 5px;"),
                           downloadButton("download_inference_page", "Unduh Halaman Lengkap", 
                                          class = "btn btn-success", style = "margin: 5px;")
                       )
                )
              )
      ),
      
      # =================== REGRESI LINEAR BERGANDA ===================
      tabItem(tabName = "regresi_linear",
              fluidRow(
                column(4,
                       box(title = "Analisis Regresi", status = "primary", solidHeader = TRUE, width = NULL,
                           selectInput("reg_dependent", "Variabel Terikat (Y):",
                                       choices = NULL),
                           selectInput("reg_independent", "Variabel Independen (X):",
                                       choices = NULL, multiple = TRUE),
                           
                           h5("Uji Asumsi Regresi:"),
                           checkboxInput("test_linearity", "Uji Linearitas", value = TRUE),
                           checkboxInput("test_independence", "Uji Independensi", value = TRUE),
                           checkboxInput("test_normality_residuals", "Uji Normalitas Residu", value = TRUE),
                           checkboxInput("test_homoscedasticity", "Uji Homoskedastisitas", value = TRUE),
                           checkboxInput("test_multicollinearity", "Uji Multikolinearitas", value = TRUE),
                           
                           actionButton("run_regression", "Jalankan Analisis Regresi", class = "btn-success")
                       )
                ),
                
                column(8,
                       box(title = "Hasil Regresi", status = "primary", solidHeader = TRUE, width = NULL,
                           verbatimTextOutput("regression_summary"),
                           br(),
                           plotlyOutput("regression_plots")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       box(title = "Hasil Uji Asumsi Regresi", status = "info", solidHeader = TRUE, width = NULL,
                           verbatimTextOutput("regression_assumptions")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       div(class = "interpretation-box",
                           h5("Interpretasi:", style = "color: #51CD37;"),
                           textOutput("regression_interpretation")
                       )
                )
              ),
              
              fluidRow(
                column(12,
                       div(class = "download-section",
                           h4("Opsi Unduh", style = "color: #9EF09E;"),
                           downloadButton("download_regression_plot", "Unduh Plot (JPG)", 
                                          class = "btn btn-success", style = "margin: 5px;"),
                           downloadButton("download_regression_results", "Unduh Hasil", 
                                          class = "btn btn-success", style = "margin: 5px;"),
                           downloadButton("download_regression_page", "Unduh Halaman Lengkap", 
                                          class = "btn btn-success", style = "margin: 5px;")
                       )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values
  values <- reactiveValues(
    data = sovi_data,
    modified_data = NULL
  )
  
  # Update choices when data is loaded
  observe({
    numeric_vars <- names(select_if(values$data, is.numeric))
    all_vars <- names(values$data)
    factor_vars <- names(select_if(values$data, function(x) is.factor(x) || is.character(x)))
    
    updateSelectInput(session, "var_to_categorize", choices = numeric_vars)
    updateSelectInput(session, "explore_var", choices = all_vars)
    updateSelectInput(session, "scatter_y", choices = numeric_vars)
    updateSelectInput(session, "cluster_vars", choices = numeric_vars)
    updateSelectInput(session, "assumption_var", choices = numeric_vars)
    updateSelectInput(session, "assumption_group", choices = c("Tidak Ada" = "none", factor_vars))
    updateSelectInput(session, "inference_var", choices = numeric_vars)
    updateSelectInput(session, "inference_group", choices = factor_vars)
    updateSelectInput(session, "inference_group2", choices = factor_vars)
    updateSelectInput(session, "inference_var2", choices = numeric_vars)
    updateSelectInput(session, "reg_dependent", choices = numeric_vars)
    updateSelectInput(session, "reg_independent", choices = numeric_vars)
  })
  
  # =================== MANAJEMEN DATA SERVER ===================
  
  observeEvent(input$apply_categorization, {
    req(input$var_to_categorize)
    
    var_data <- values$data[[input$var_to_categorize]]
    
    if (input$categorize_method == "equal") {
      breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), 
                    length.out = input$num_categories + 1)
    } else if (input$categorize_method == "quantile") {
      breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$num_categories + 1), na.rm = TRUE)
    } else {
      breaks <- as.numeric(unlist(strsplit(input$custom_breaks, ",")))
    }
    
    # Create categorized variable
    categorized_var <- cut(var_data, breaks = breaks, include.lowest = TRUE)
    
    # Apply labels based on user choice
    if (input$label_type == "numeric") {
      # Use numeric labels (1, 2, 3, ...)
      levels(categorized_var) <- as.character(1:length(levels(categorized_var)))
    } else if (input$label_type == "custom_labels") {
      # Use custom labels
      custom_labels <- trimws(unlist(strsplit(input$custom_labels, ",")))
      if (length(custom_labels) == length(levels(categorized_var))) {
        levels(categorized_var) <- custom_labels
      } else {
        # If custom labels don't match, use numeric as fallback
        levels(categorized_var) <- as.character(1:length(levels(categorized_var)))
      }
    }
    
    values$modified_data <- values$data
    values$modified_data[[paste0(input$var_to_categorize, "_cat")]] <- categorized_var
  })
  
  output$data_table_mgmt <- DT::renderDataTable({
    if (input$show_original && input$show_modified && !is.null(values$modified_data)) {
      # Show modified data
      display_data <- values$modified_data
      
      # Filter columns based on user choice
      if (input$display_columns == "categorized_only") {
        cat_var <- paste0(input$var_to_categorize, "_cat")
        if (cat_var %in% names(display_data)) {
          display_data <- display_data[, c(input$var_to_categorize, cat_var)]
        }
      }
      
      DT::datatable(display_data, options = list(scrollX = TRUE, pageLength = 10))
    } else if (input$show_modified && !is.null(values$modified_data)) {
      # Show modified data only
      display_data <- values$modified_data
      
      # Filter columns based on user choice
      if (input$display_columns == "categorized_only") {
        cat_var <- paste0(input$var_to_categorize, "_cat")
        if (cat_var %in% names(display_data)) {
          display_data <- display_data[, c(input$var_to_categorize, cat_var)]
        }
      }
      
      DT::datatable(display_data, options = list(scrollX = TRUE, pageLength = 10))
    } else {
      # Show original data only
      display_data <- values$data
      
      # Filter columns based on user choice
      if (input$display_columns == "categorized_only") {
        # If showing original data but user wants categorized only, show just the original variable
        display_data <- display_data[, input$var_to_categorize, drop = FALSE]
      }
      
      DT::datatable(display_data, options = list(scrollX = TRUE, pageLength = 10))
    }
  })
  

  
  output$categorization_table <- DT::renderDataTable({
    if (!is.null(values$modified_data)) {
      cat_var <- paste0(input$var_to_categorize, "_cat")
      if (cat_var %in% names(values$modified_data)) {
        # Get original variable data
        var_data <- values$modified_data[[input$var_to_categorize]]
        cat_data <- values$modified_data[[cat_var]]
        
        # Create summary table
        summary_table <- data.frame()
        
        for (i in 1:length(levels(cat_data))) {
          level_name <- levels(cat_data)[i]
          level_data <- var_data[cat_data == level_name]
          
          # Get breaks information
          if (input$categorize_method == "equal") {
            breaks <- seq(min(var_data, na.rm = TRUE), max(var_data, na.rm = TRUE), 
                         length.out = input$num_categories + 1)
          } else if (input$categorize_method == "quantile") {
            breaks <- quantile(var_data, probs = seq(0, 1, length.out = input$num_categories + 1), na.rm = TRUE)
          } else {
            breaks <- as.numeric(unlist(strsplit(input$custom_breaks, ",")))
          }
          
          # Determine class interval
          if (i == 1) {
            class_interval <- paste0("[", round(breaks[i], 2), ", ", round(breaks[i+1], 2), "]")
          } else {
            class_interval <- paste0("(", round(breaks[i], 2), ", ", round(breaks[i+1], 2), "]")
          }
          
          # Calculate statistics
          row_data <- data.frame(
            Kategori = level_name,
            Kelas = class_interval,
            Banyak_Data = length(level_data),
            Nilai_Minimum = round(min(level_data, na.rm = TRUE), 2),
            Nilai_Maksimum = round(max(level_data, na.rm = TRUE), 2),
            Range = round(max(level_data, na.rm = TRUE) - min(level_data, na.rm = TRUE), 2),
            Interval_Kelas = round(breaks[i+1] - breaks[i], 2)
          )
          
          summary_table <- rbind(summary_table, row_data)
        }
        
        DT::datatable(summary_table, 
                     options = list(pageLength = 10, scrollX = TRUE),
                     caption = paste("Tabel Kategorisasi Variabel:", input$var_to_categorize))
      }
    }
  })
  
  output$categorization_interpretation <- renderText({
    if (!is.null(values$modified_data)) {
      cat_var <- paste0(input$var_to_categorize, "_cat")
      if (cat_var %in% names(values$modified_data)) {
        categories <- levels(values$modified_data[[cat_var]])
        label_info <- if (input$label_type == "numeric") {
          paste("menggunakan label", paste(categories, collapse = ", "))
        } else {
          paste("menggunakan label", paste(categories, collapse = ", "))
        }
        if (input$categorize_method == "equal") {
          paste0(
            "Variabel kontinu ", input$var_to_categorize, " telah berhasil dikategorikan menjadi ",
            input$num_categories, " kategori menggunakan metode equal intervals, ",
            label_info, ". Metode yang dipilih membagi data dengan interval yang sama panjang. Transformasi ini memungkinkan analisis kategorikal pada data asli yang kontinu."
          )
        } else if (input$categorize_method == "quantile") {
          paste0(
            "Variabel kontinu ", input$var_to_categorize, " telah berhasil dikategorikan menjadi ",
            input$num_categories, " kategori menggunakan metode quantiles, ",
            label_info, ". Metode yang dipilih membagi data dengan frekuensi yang sama banyak. Transformasi ini memungkinkan analisis kategorikal pada data asli yang kontinu."
          )
        } else {
          paste("Variabel kontinu", input$var_to_categorize, "telah berhasil dikategorikan menjadi",
                input$num_categories, "kategori menggunakan metode custom breaks,",
                label_info, ". Transformasi ini memungkinkan analisis kategorikal pada data asli yang kontinu.")
        }
      }
    }
  })
  
  # =================== EKSPLORASI DATA SERVER ===================
  
  observeEvent(input$generate_exploration, {
    req(input$explore_var)
    
    if (input$explore_type == "desc") {
      output$desc_stats <- renderPrint({
        var_data <- values$data[[input$explore_var]]
        if (is.numeric(var_data)) {
          summary(var_data)
        } else {
          table(var_data)
        }
      })
    } else if (input$explore_type == "hist") {
      output$exploration_plot <- renderPlotly({
        p <- ggplot(values$data, aes_string(x = input$explore_var)) +
          geom_histogram(bins = 30, fill = "#9EF09E", color = "#232935") +
          theme_minimal() +
          theme(
            plot.background = element_rect(fill = "#232935"),
            panel.background = element_rect(fill = "#232935"),
            panel.grid.major = element_line(color = "#3A4553"),
            panel.grid.minor = element_line(color = "#3A4553"),
            axis.text = element_text(color = "#EFF7FF"),
            axis.title = element_text(color = "#EFF7FF"),
            plot.title = element_text(color = "#9EF09E")
          ) +
          labs(title = paste("Histogram dari", input$explore_var))
        ggplotly(p)
      })
    } else if (input$explore_type == "box") {
      output$exploration_plot <- renderPlotly({
        p <- ggplot(values$data, aes_string(y = input$explore_var)) +
          geom_boxplot(fill = "#9EF09E", color = "#232935") +
          theme_minimal() +
          theme(
            plot.background = element_rect(fill = "#232935"),
            panel.background = element_rect(fill = "#232935"),
            panel.grid.major = element_line(color = "#3A4553"),
            panel.grid.minor = element_line(color = "#3A4553"),
            axis.text = element_text(color = "#EFF7FF"),
            axis.title = element_text(color = "#EFF7FF"),
            plot.title = element_text(color = "#9EF09E")
          ) +
          labs(title = paste("Boxplot dari", input$explore_var))
        ggplotly(p)
      })
    } else if (input$explore_type == "corr") {
      output$exploration_plot <- renderPlotly({
        numeric_data <- select_if(values$data, is.numeric)
        cor_matrix <- cor(numeric_data, use = "complete.obs")
        
        # Convert correlation matrix to long format for ggplot
        cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
        cor_df$value <- as.vector(cor_matrix)
        
        p <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
          geom_tile() +
          scale_fill_gradient2(low = "#FF5005", high = "#51CD37", mid = "#DADFE9", midpoint = 0) +
          theme_minimal() +
          theme(
            plot.background = element_rect(fill = "#232935"),
            panel.background = element_rect(fill = "#232935"),
            panel.grid.major = element_line(color = "#3A4553"),
            panel.grid.minor = element_line(color = "#3A4553"),
            axis.text = element_text(color = "#EFF7FF", angle = 45, hjust = 1),
            axis.title = element_text(color = "#EFF7FF"),
            plot.title = element_text(color = "#9EF09E")
          ) +
          labs(title = "Matriks Korelasi")
        ggplotly(p)
      })
    } else if (input$explore_type == "scatter") {
      req(input$scatter_y)
      output$exploration_plot <- renderPlotly({
        p <- ggplot(values$data, aes_string(x = input$explore_var, y = input$scatter_y)) +
          geom_point(color = "#9EF09E") +
          geom_smooth(method = "lm", color = "#51CD37") +
          theme_minimal() +
          theme(
            plot.background = element_rect(fill = "#232935"),
            panel.background = element_rect(fill = "#232935"),
            panel.grid.major = element_line(color = "#3A4553"),
            panel.grid.minor = element_line(color = "#3A4553"),
            axis.text = element_text(color = "#EFF7FF"),
            axis.title = element_text(color = "#EFF7FF"),
            plot.title = element_text(color = "#9EF09E")
          ) +
          labs(title = paste("Plot Scatter:", input$explore_var, "vs", input$scatter_y))
        ggplotly(p)
      })
    } else if (input$explore_type == "map") {
      if (input$map_type == "variable") {
        output$geo_map <- renderLeaflet({
          # Create color palette based on the selected variable
          var_data <- sovi_geojson@data[[input$explore_var]]
          
          # Create color palette
          pal <- colorNumeric(
            palette = c("#9EF09E", "#5566FC", "#92B0FF", "#F5DC71", "#FF9C41", "#F55077"),
            domain = var_data
          )
          
          leaflet(sovi_geojson) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~pal(var_data),
              weight = 1,
              opacity = 1,
              color = "#232935",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#9EF09E",
                dashArray = "",
                fillOpacity = 0.9,
                bringToFront = TRUE
              ),
              label = ~paste("Wilayah:", sovi_geojson@data$NAME, "<br>", 
                            input$explore_var, ":", round(var_data, 2))
            ) %>%
            addLegend(
              pal = pal,
              values = var_data,
              opacity = 0.7,
              title = input$explore_var,
              position = "bottomright"
            )
        })
      } else if (input$map_type == "cluster") {
        req(input$cluster_vars, input$n_clusters)
        
        # Perform clustering
        cluster_data <- sovi_geojson@data[, input$cluster_vars, drop = FALSE]
        cluster_data <- na.omit(cluster_data)
        
        if (nrow(cluster_data) < input$n_clusters) {
          showNotification("❌ Jumlah data tidak cukup untuk jumlah cluster yang diminta.", type = "error")
          return()
        }
        
        # Standardize data
        scaled_data <- scale(cluster_data)
        
        # Perform K-Means clustering
        set.seed(123)
        clust_result <- kmeans(scaled_data, centers = input$n_clusters)
        
        # Add cluster results to spatial data
        sovi_clustered <- sovi_geojson
        sovi_clustered@data$Cluster <- NA
        sovi_clustered@data$Cluster[as.numeric(rownames(cluster_data))] <- clust_result$cluster
        sovi_clustered@data$Cluster <- as.factor(sovi_clustered@data$Cluster)
        
        output$geo_map <- renderLeaflet({
          # Create color palette for clusters
          pal <- colorFactor(
            palette = c("#9EF09E", "#5566FC", "#92B0FF", "#F5DC71", "#FF9C41", "#F55077"),
            domain = sovi_clustered@data$Cluster
          )
          
          leaflet(sovi_clustered) %>%
            addTiles() %>%
            addPolygons(
              fillColor = ~pal(Cluster),
              weight = 1,
              opacity = 1,
              color = "#232935",
              dashArray = "3",
              fillOpacity = 0.7,
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#9EF09E",
                dashArray = "",
                fillOpacity = 0.9,
                bringToFront = TRUE
              ),
              label = ~paste("Wilayah:", sovi_clustered@data$NAME, "<br>", 
                            "Cluster:", Cluster)
            ) %>%
            addLegend(
              pal = pal,
              values = ~Cluster,
              opacity = 0.7,
              title = "Cluster",
              position = "bottomright"
            )
        })
        
        # Update interpretation for clustering
        output$exploration_interpretation <- renderText({
          paste("Pengelompokan dilakukan menggunakan metode K-Means dengan", input$n_clusters,
                "cluster berdasarkan variabel:", paste(input$cluster_vars, collapse = ", "),
                ". Setiap wilayah dikelompokkan berdasarkan kemiripan nilai pada variabel tersebut. Peta menunjukkan sebaran spasial dari kelompok-kelompok ini.")
        })
      }
    }
  })
  
  output$exploration_table <- DT::renderDataTable({
    DT::datatable(values$data, options = list(scrollX = TRUE, pageLength = 5))
  })
  
  output$exploration_interpretation <- renderText({
    paste("Analisis eksplorasi untuk variabel:", input$explore_var, 
          "- Analisis", input$explore_type, "memberikan wawasan tentang distribusi dan karakteristik data.")
  })
  
  # =================== UJI ASUMSI SERVER ===================
  
  observeEvent(input$run_assumption_test, {
    req(input$assumption_var)
    var_data <- values$data[[input$assumption_var]]
    
    if (input$test_type == "normality") {
      if (input$normality_test == "shapiro") {
        test_result <- shapiro.test(var_data)
      } else if (input$normality_test == "ad") {
        test_result <- ad.test(var_data)
      } else {
        test_result <- ks.test(var_data, "pnorm", mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE))
      }
      
      output$assumption_results <- renderPrint({
        test_result
      })
      
      output$assumption_plot <- renderPlotly({
        p <- ggplot(values$data, aes_string(sample = input$assumption_var)) +
          stat_qq() +
          stat_qq_line(color = "#51CD37") +
          theme_minimal() +
          theme(
            plot.background = element_rect(fill = "#232935"),
            panel.background = element_rect(fill = "#232935"),
            panel.grid.major = element_line(color = "#3A4553"),
            panel.grid.minor = element_line(color = "#3A4553"),
            axis.text = element_text(color = "#EFF7FF"),
            axis.title = element_text(color = "#EFF7FF"),
            plot.title = element_text(color = "#9EF09E")
          ) +
          labs(title = paste("Plot Q-Q untuk", input$assumption_var))
        ggplotly(p)
      })
      
      output$assumption_interpretation <- renderText({
        if (test_result$p.value < 0.05) {
          "Uji normalitas menunjukkan bahwa data TIDAK mengikuti distribusi normal (p < 0.05). Asumsi normalitas dilanggar."
        } else {
          "Uji normalitas menunjukkan bahwa data mengikuti distribusi normal (p >= 0.05). Asumsi normalitas terpenuhi."
        }
      })
    } else if (input$test_type == "homogeneity") {
      req(input$assumption_group)
      group_var <- input$assumption_group
      if (group_var == "none") {
        output$assumption_results <- renderPrint({
          cat("Pilih variabel grup untuk uji homogenitas.")
        })
        output$assumption_plot <- renderPlotly({NULL})
        output$assumption_interpretation <- renderText({
          "Pilih variabel grup untuk melakukan uji homogenitas."
        })
        return()
      }
      group_data <- as.factor(values$data[[group_var]])
      test_result <- NULL
      if (input$homogeneity_test == "levene") {
        test_result <- car::leveneTest(var_data, group_data)
      } else if (input$homogeneity_test == "bartlett") {
        test_result <- bartlett.test(var_data, group_data)
      } else if (input$homogeneity_test == "fligner") {
        test_result <- fligner.test(var_data, group_data)
      }
      output$assumption_results <- renderPrint({
        test_result
      })
      output$assumption_plot <- renderPlotly({
        p <- ggplot(values$data, aes_string(x = group_var, y = input$assumption_var, fill = group_var)) +
          geom_boxplot() +
          theme_minimal() +
          theme(
            plot.background = element_rect(fill = "#232935"),
            panel.background = element_rect(fill = "#232935"),
            panel.grid.major = element_line(color = "#3A4553"),
            panel.grid.minor = element_line(color = "#3A4553"),
            axis.text = element_text(color = "#EFF7FF"),
            axis.title = element_text(color = "#EFF7FF"),
            plot.title = element_text(color = "#9EF09E")
          ) +
          labs(title = paste("Boxplot", input$assumption_var, "berdasarkan", group_var))
        ggplotly(p)
      })
      output$assumption_interpretation <- renderText({
        pval <- NULL
        if (input$homogeneity_test == "levene") {
          pval <- test_result$`Pr(>F)`[1]
        } else {
          pval <- test_result$p.value
        }
        if (pval < 0.05) {
          "Uji homogenitas menunjukkan bahwa varians antar grup TIDAK homogen (p < 0.05). Asumsi homogenitas dilanggar."
        } else {
          "Uji homogenitas menunjukkan bahwa varians antar grup homogen (p >= 0.05). Asumsi homogenitas terpenuhi."
        }
      })
    }
  })
  
  # =================== STATISTIK INFERENSIA SERVER ===================
  
  observeEvent(input$run_inference_test, {
    req(input$inference_var)
    var_data <- values$data[[input$inference_var]]
    test_result <- NULL
    alternative <- if (!is.null(input$test_cat)) {
      if (input$test_cat == "Tidak sama dengan") "two.sided" else if (input$test_cat == "Kurang dari") "less" else "greater"
    } else "two.sided"
    
    if (input$inference_type == "one_t") {
      test_result <- t.test(var_data, mu = input$test_value, alternative = alternative, conf.level = 1 - input$alpha_level)
    } else if (input$inference_type == "two_t") {
      req(input$inference_var2)
      var_data2 <- values$data[[input$inference_var2]]
      test_result <- t.test(var_data, var_data2, alternative = alternative, conf.level = 1 - input$alpha_level)
    } else if (input$inference_type == "one_prop") {
      # Convert to binary based on cutpoint
      success_count <- sum(var_data > input$cutpoint, na.rm = TRUE)
      total_count <- length(var_data[!is.na(var_data)])
      test_result <- prop.test(success_count, total_count, p = 0.5, alternative = alternative, conf.level = 1 - input$alpha_level)
    } else if (input$inference_type == "two_prop") {
      req(input$inference_var2)
      var_data2 <- values$data[[input$inference_var2]]
      success_count1 <- sum(var_data > input$cutpoint, na.rm = TRUE)
      success_count2 <- sum(var_data2 > input$cutpoint, na.rm = TRUE)
      total_count1 <- length(var_data[!is.na(var_data)])
      total_count2 <- length(var_data2[!is.na(var_data2)])
      test_result <- prop.test(c(success_count1, success_count2), c(total_count1, total_count2), alternative = alternative, conf.level = 1 - input$alpha_level)
    } else if (input$inference_type == "one_var") {
      # Chi-square test for variance
      test_result <- list(
        statistic = (length(var_data) - 1) * var(var_data, na.rm = TRUE) / input$test_value^2,
        p.value = 2 * min(pchisq((length(var_data) - 1) * var(var_data, na.rm = TRUE) / input$test_value^2, df = length(var_data) - 1),
                          1 - pchisq((length(var_data) - 1) * var(var_data, na.rm = TRUE) / input$test_value^2, df = length(var_data) - 1)),
        parameter = length(var_data) - 1,
        method = "Chi-square test for variance",
        data.name = input$inference_var,
        null.value = input$test_value^2,
        alternative = "two.sided",
        conf.int = c(
          (length(var_data) - 1) * var(var_data, na.rm = TRUE) / qchisq(1 - input$alpha_level/2, df = length(var_data) - 1),
          (length(var_data) - 1) * var(var_data, na.rm = TRUE) / qchisq(input$alpha_level/2, df = length(var_data) - 1)
        )
      )
      class(test_result) <- "htest"
    } else if (input$inference_type == "two_var") {
      req(input$inference_var2)
      var_data2 <- values$data[[input$inference_var2]]
      test_result <- var.test(var_data, var_data2, alternative = alternative, conf.level = 1 - input$alpha_level)
    } else if (input$inference_type == "anova1") {
      req(input$inference_group)
      formula <- as.formula(paste(input$inference_var, "~", input$inference_group))
      test_result <- aov(formula, data = values$data)
    } else if (input$inference_type == "anova2") {
      req(input$inference_group, input$inference_group2)
      formula <- as.formula(paste(input$inference_var, "~", input$inference_group, "*", input$inference_group2))
      test_result <- aov(formula, data = values$data)
    }
    
    output$inference_results <- renderPrint({
      if (input$inference_type %in% c("anova1", "anova2")) {
        summary(test_result)
      } else {
        test_result
      }
    })
    
    # Generate appropriate plot
    output$inference_plot <- renderPlotly({
      if (input$inference_type == "one_t") {
        p <- ggplot(values$data, aes_string(x = input$inference_var)) +
          geom_histogram(bins = 30, fill = "#9EF09E", alpha = 0.7) +
          geom_vline(xintercept = input$test_value, color = "#FF5005", linetype = "dashed", size = 1) +
          geom_vline(xintercept = mean(var_data, na.rm = TRUE), color = "#51CD37", size = 1) +
          theme_minimal() +
          theme(
            plot.background = element_rect(fill = "#232935"),
            panel.background = element_rect(fill = "#232935"),
            panel.grid.major = element_line(color = "#3A4553"),
            panel.grid.minor = element_line(color = "#3A4553"),
            axis.text = element_text(color = "#EFF7FF"),
            axis.title = element_text(color = "#EFF7FF"),
            plot.title = element_text(color = "#9EF09E"),
            plot.subtitle = element_text(color = "#DADFE9")
          ) +
          labs(title = "Uji t Satu Sampel", subtitle = "Garis merah: nilai uji, Garis hijau: mean sampel")
      } else if (input$inference_type == "two_t") {
        # Create combined dataset for comparison
        data_combined <- data.frame(
          values = c(var_data, values$data[[input$inference_var2]]),
          group = c(rep("Variabel 1", length(var_data)), 
                    rep("Variabel 2", length(values$data[[input$inference_var2]])))
        )
        p <- ggplot(data_combined, aes(x = group, y = values)) +
          geom_boxplot(fill = "#9EF09E", alpha = 0.7) +
          theme_minimal() +
          theme(
            plot.background = element_rect(fill = "#232935"),
            panel.background = element_rect(fill = "#232935"),
            panel.grid.major = element_line(color = "#3A4553"),
            panel.grid.minor = element_line(color = "#3A4553"),
            axis.text = element_text(color = "#EFF7FF"),
            axis.title = element_text(color = "#EFF7FF"),
            plot.title = element_text(color = "#9EF09E")
          ) +
          labs(title = "Uji t Dua Sampel - Perbandingan Variabel", x = "Variabel", y = "Nilai")
      } else if (input$inference_type %in% c("one_prop", "two_prop")) {
        if (input$inference_type == "one_prop") {
          success_count <- sum(var_data > input$cutpoint, na.rm = TRUE)
          fail_count <- sum(var_data <= input$cutpoint, na.rm = TRUE)
          prop_data <- data.frame(
            category = c("Sukses", "Gagal"),
            count = c(success_count, fail_count)
          )
        } else {
          var_data2 <- values$data[[input$inference_var2]]
          success_count1 <- sum(var_data > input$cutpoint, na.rm = TRUE)
          fail_count1 <- sum(var_data <= input$cutpoint, na.rm = TRUE)
          success_count2 <- sum(var_data2 > input$cutpoint, na.rm = TRUE)
          fail_count2 <- sum(var_data2 <= input$cutpoint, na.rm = TRUE)
          prop_data <- data.frame(
            category = c("Var1-Sukses", "Var1-Gagal", "Var2-Sukses", "Var2-Gagal"),
            count = c(success_count1, fail_count1, success_count2, fail_count2)
          )
        }
        p <- ggplot(prop_data, aes(x = category, y = count)) +
          geom_bar(stat = "identity", fill = "#9EF09E", alpha = 0.7) +
          theme_minimal() +
          theme(
            plot.background = element_rect(fill = "#232935"),
            panel.background = element_rect(fill = "#232935"),
            panel.grid.major = element_line(color = "#3A4553"),
            panel.grid.minor = element_line(color = "#3A4553"),
            axis.text = element_text(color = "#EFF7FF"),
            axis.title = element_text(color = "#EFF7FF"),
            plot.title = element_text(color = "#9EF09E")
          ) +
          labs(title = paste("Uji Proporsi -", ifelse(input$inference_type == "one_prop", "Satu Sampel", "Dua Sampel")))
      } else if (input$inference_type %in% c("one_var", "two_var")) {
        if (input$inference_type == "one_var") {
          p <- ggplot(values$data, aes_string(x = input$inference_var)) +
            geom_histogram(bins = 30, fill = "#9EF09E", alpha = 0.7) +
            labs(title = "Distribusi untuk Uji Variansi Satu Sampel")
        } else {
          data_combined <- data.frame(
            values = c(var_data, values$data[[input$inference_var2]]),
            group = c(rep("Variabel 1", length(var_data)), 
                      rep("Variabel 2", length(values$data[[input$inference_var2]])))
          )
          p <- ggplot(data_combined, aes(x = group, y = values)) +
            geom_boxplot(fill = "#9EF09E", alpha = 0.7) +
            labs(title = "Uji Variansi Dua Sampel - Perbandingan Variabel", x = "Variabel", y = "Nilai")
        }
        p <- p + theme_minimal() +
          theme(
            plot.background = element_rect(fill = "#232935"),
            panel.background = element_rect(fill = "#232935"),
            panel.grid.major = element_line(color = "#3A4553"),
            panel.grid.minor = element_line(color = "#3A4553"),
            axis.text = element_text(color = "#EFF7FF"),
            axis.title = element_text(color = "#EFF7FF"),
            plot.title = element_text(color = "#9EF09E")
          )
      } else if (input$inference_type %in% c("anova1", "anova2")) {
        p <- ggplot(values$data, aes_string(x = input$inference_group, y = input$inference_var)) +
          geom_boxplot(fill = "#9EF09E", alpha = 0.7) +
          theme_minimal() +
          theme(
            plot.background = element_rect(fill = "#232935"),
            panel.background = element_rect(fill = "#232935"),
            panel.grid.major = element_line(color = "#3A4553"),
            panel.grid.minor = element_line(color = "#3A4553"),
            axis.text = element_text(color = "#EFF7FF"),
            axis.title = element_text(color = "#EFF7FF"),
            plot.title = element_text(color = "#9EF09E")
          ) +
          labs(title = paste("Uji ANOVA -", ifelse(input$inference_type == "anova1", "Satu Arah", "Dua Arah")))
      }
      ggplotly(p)
    })
    
    # Generate interpretation
    output$inference_interpretation <- renderText({
      alt <- if (!is.null(input$test_cat)) input$test_cat else "Tidak sama dengan"
      if (input$inference_type == "one_t") {
        if (alt == "Tidak sama dengan") {
          if (test_result$p.value < input$alpha_level) {
            paste("Uji t satu sampel menunjukkan perbedaan statistik yang signifikan dari nilai uji", 
                  input$test_value, "(p <", input$alpha_level, "). Kita menolak hipotesis nol.")
          } else {
            paste("Uji t satu sampel menunjukkan tidak ada perbedaan statistik yang signifikan dari nilai uji", 
                  input$test_value, "(p >=", input$alpha_level, "). Kita gagal menolak hipotesis nol.")
          }
        } else if (alt == "Kurang dari") {
          if (test_result$p.value < input$alpha_level) {
            paste("Uji t satu sampel menunjukkan rata-rata lebih kecil secara signifikan dari nilai uji", 
                  input$test_value, "(p <", input$alpha_level, "). Kita menolak hipotesis nol.")
          } else {
            paste("Uji t satu sampel menunjukkan rata-rata tidak lebih kecil secara signifikan dari nilai uji", 
                  input$test_value, "(p >=", input$alpha_level, "). Kita gagal menolak hipotesis nol.")
          }
        } else if (alt == "Lebih dari") {
          if (test_result$p.value < input$alpha_level) {
            paste("Uji t satu sampel menunjukkan rata-rata lebih besar secara signifikan dari nilai uji", 
                  input$test_value, "(p <", input$alpha_level, "). Kita menolak hipotesis nol.")
          } else {
            paste("Uji t satu sampel menunjukkan rata-rata tidak lebih besar secara signifikan dari nilai uji", 
                  input$test_value, "(p >=", input$alpha_level, "). Kita gagal menolak hipotesis nol.")
          }
        }
      } else if (input$inference_type == "two_t") {
        if (alt == "Tidak sama dengan") {
          if (test_result$p.value < input$alpha_level) {
            paste("Uji t dua sampel menunjukkan perbedaan statistik yang signifikan antara kedua variabel",
                  "(p <", input$alpha_level, "). Kita menolak hipotesis nol tentang rata-rata yang sama.")
          } else {
            paste("Uji t dua sampel menunjukkan tidak ada perbedaan statistik yang signifikan antara kedua variabel",
                  "(p >=", input$alpha_level, "). Kita gagal menolak hipotesis nol tentang rata-rata yang sama.")
          }
        } else if (alt == "Kurang dari") {
          if (test_result$p.value < input$alpha_level) {
            paste("Uji t dua sampel menunjukkan rata-rata variabel pertama lebih kecil secara signifikan dari variabel kedua",
                  "(p <", input$alpha_level, "). Kita menolak hipotesis nol.")
          } else {
            paste("Uji t dua sampel menunjukkan rata-rata variabel pertama tidak lebih kecil secara signifikan dari variabel kedua",
                  "(p >=", input$alpha_level, "). Kita gagal menolak hipotesis nol.")
          }
        } else if (alt == "Lebih dari") {
          if (test_result$p.value < input$alpha_level) {
            paste("Uji t dua sampel menunjukkan rata-rata variabel pertama lebih besar secara signifikan dari variabel kedua",
                  "(p <", input$alpha_level, "). Kita menolak hipotesis nol.")
          } else {
            paste("Uji t dua sampel menunjukkan rata-rata variabel pertama tidak lebih besar secara signifikan dari variabel kedua",
                  "(p >=", input$alpha_level, "). Kita gagal menolak hipotesis nol.")
          }
        }
      } else if (input$inference_type == "one_prop") {
        if (test_result$p.value < input$alpha_level) {
          paste("Uji proporsi satu sampel menunjukkan proporsi berbeda signifikan dari 0.5",
                "(p <", input$alpha_level, "). Kita menolak hipotesis nol.")
        } else {
          paste("Uji proporsi satu sampel menunjukkan proporsi tidak berbeda signifikan dari 0.5",
                "(p >=", input$alpha_level, "). Kita gagal menolak hipotesis nol.")
        }
      } else if (input$inference_type == "two_prop") {
        if (test_result$p.value < input$alpha_level) {
          paste("Uji proporsi dua sampel menunjukkan perbedaan proporsi yang signifikan",
                "(p <", input$alpha_level, "). Kita menolak hipotesis nol tentang proporsi yang sama.")
        } else {
          paste("Uji proporsi dua sampel menunjukkan tidak ada perbedaan proporsi yang signifikan",
                "(p >=", input$alpha_level, "). Kita gagal menolak hipotesis nol tentang proporsi yang sama.")
        }
      } else if (input$inference_type == "one_var") {
        if (test_result$p.value < input$alpha_level) {
          paste("Uji variansi satu sampel menunjukkan variansi berbeda signifikan dari nilai uji",
                "(p <", input$alpha_level, "). Kita menolak hipotesis nol.")
        } else {
          paste("Uji variansi satu sampel menunjukkan variansi tidak berbeda signifikan dari nilai uji",
                "(p >=", input$alpha_level, "). Kita gagal menolak hipotesis nol.")
        }
      } else if (input$inference_type == "two_var") {
        if (test_result$p.value < input$alpha_level) {
          paste("Uji variansi dua sampel menunjukkan perbedaan variansi yang signifikan",
                "(p <", input$alpha_level, "). Kita menolak hipotesis nol tentang variansi yang sama.")
        } else {
          paste("Uji variansi dua sampel menunjukkan tidak ada perbedaan variansi yang signifikan",
                "(p >=", input$alpha_level, "). Kita gagal menolak hipotesis nol tentang variansi yang sama.")
        }
      } else if (input$inference_type %in% c("anova1", "anova2")) {
        anova_summary <- summary(test_result)
        p_value <- anova_summary[[1]][["Pr(>F)"]][1]
        if (p_value < input$alpha_level) {
          paste("Hasil ANOVA menunjukkan perbedaan statistik yang signifikan antar grup",
                "(p <", input$alpha_level, "). Rata-rata grup berbeda secara signifikan.")
        } else {
          paste("Hasil ANOVA menunjukkan tidak ada perbedaan statistik yang signifikan antar grup",
                "(p >=", input$alpha_level, "). Semua rata-rata grup tidak berbeda secara signifikan.")
        }
      }
    })
  })
  
  # =================== REGRESI LINEAR SERVER ===================
  
  observeEvent(input$run_regression, {
    req(input$reg_dependent, input$reg_independent)
    
    # Create formula
    formula_str <- paste(input$reg_dependent, "~", paste(input$reg_independent, collapse = " + "))
    formula <- as.formula(formula_str)
    
    # Fit regression model
    model <- lm(formula, data = values$data)
    
    output$regression_summary <- renderPrint({
      summary(model)
    })
    
    # Generate regression plots
    output$regression_plots <- renderPlotly({
      # Create diagnostic plots
      par(mfrow = c(2, 2))
      plot(model)
      
      # For now, create a simple residuals vs fitted plot
      residuals <- residuals(model)
      fitted_values <- fitted(model)
      
      plot_data <- data.frame(
        fitted = fitted_values,
        residuals = residuals,
        standardized_residuals = rstandard(model)
      )
      
      p <- ggplot(plot_data, aes(x = fitted, y = residuals)) +
        geom_point(color = "#9EF09E", alpha = 0.7) +
        geom_hline(yintercept = 0, color = "#FF5005", linetype = "dashed") +
        geom_smooth(method = "loess", color = "#51CD37", se = FALSE) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#232935"),
          panel.background = element_rect(fill = "#232935"),
          panel.grid.major = element_line(color = "#3A4553"),
          panel.grid.minor = element_line(color = "#3A4553"),
          axis.text = element_text(color = "#EFF7FF"),
          axis.title = element_text(color = "#EFF7FF"),
          plot.title = element_text(color = "#9EF09E")
        ) +
        labs(title = "Residu vs Nilai Fitted",
             x = "Nilai Fitted",
             y = "Residu")
      
      ggplotly(p)
    })
    
    # Run assumption tests
    output$regression_assumptions <- renderPrint({
      cat("UJI ASUMSI REGRESI\n")
      cat("=" %R% 50, "\n\n")
      
      if (input$test_linearity) {
        cat("1. UJI LINEARITAS\n")
        cat("-" %R% 20, "\n")
        # Rainbow test for linearity
        if (requireNamespace("lmtest", quietly = TRUE)) {
          rainbow_test <- lmtest::raintest(model)
          print(rainbow_test)
        } else {
          cat("lmtest package diperlukan untuk uji linearitas\n")
        }
        cat("\n")
      }
      
      if (input$test_independence) {
        cat("2. UJI INDEPENDENSI (Durbin-Watson)\n")
        cat("-" %R% 35, "\n")
        if (requireNamespace("car", quietly = TRUE)) {
          dw_test <- car::durbinWatsonTest(model)
          print(dw_test)
        } else {
          cat("car package diperlukan untuk uji independensi\n")
        }
        cat("\n")
      }
      
      if (input$test_normality_residuals) {
        cat("3. NORMALITAS RESIDU (Shapiro-Wilk)\n")
        cat("-" %R% 40, "\n")
        shapiro_test <- shapiro.test(residuals(model))
        print(shapiro_test)
        cat("\n")
      }
      
      if (input$test_homoscedasticity) {
        cat("4. UJI HOMOSCEDASTISITAS (Breusch-Pagan)\n")
        cat("-" %R% 40, "\n")
        if (requireNamespace("lmtest", quietly = TRUE)) {
          bp_test <- lmtest::bptest(model)
          print(bp_test)
        } else {
          cat("lmtest package diperlukan untuk uji homoskedastisitas\n")
        }
        cat("\n")
      }
      
      if (input$test_multicollinearity) {
        cat("5. UJI MULTIKOLINEARITAS (VIF)\n")
        cat("-" %R% 32, "\n")
        if (length(input$reg_independent) > 1 && requireNamespace("car", quietly = TRUE)) {
          vif_values <- car::vif(model)
          print(vif_values)
        } else {
          cat("VIF memerlukan beberapa variabel independen dan car package\n")
        }
        cat("\n")
      }
    })
    
    # Generate interpretation
    output$regression_interpretation <- renderText({
      model_summary <- summary(model)
      r_squared <- model_summary$r.squared
      f_stat <- model_summary$fstatistic
      p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
      
      interpretation <- paste(
        "Analisis Regresi Linear Berganda - Hasil:",
        paste("- R-squared:", round(r_squared, 4), "- Ini berarti", round(r_squared * 100, 2), "% variasi dalam", input$reg_dependent, "dijelaskan oleh model."),
        paste("- F-statistik:", round(f_stat[1], 4), "dengan p-value:", round(p_value, 6)),
        if (p_value < 0.05) "- Model secara keseluruhan signifikan (p < 0.05)." else "- Model secara keseluruhan tidak signifikan (p >= 0.05).",
        "- Periksa hasil uji asumsi untuk memvalidasi kehandalan model.",
        sep = "\n"
      )
      
      interpretation
    })
  })
  
  # =================== DOWNLOAD HANDLERS ===================
  
  # Beranda Downloads
  output$download_beranda <- downloadHandler(
    filename = function() {
      paste("beranda_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Create a temporary markdown file
      temp_md <- tempfile(fileext = ".md")
      
      # Write content to markdown
      writeLines(c(
        "# DASHBOARD STATISTIK AMANDA - Beranda",
        "",
        "## Informasi Dataset",
        "- **Dataset**: Data Indeks Kerentanan Sosial (SoVI)",
        "- **Sumber**: https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv",
        "- **Matriks Jarak**: https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv",
        "- **Referensi Metadata**: https://www.sciencedirect.com/science/article/pii/S2352340921010180",
        "",
        "## Fitur Dashboard",
        "- Manajemen Data: Kategorisasi data kontinyu",
        "- Eksplorasi Data: Statistik deskriptif, visualisasi, peta",
        "- Uji Asumsi: Uji normalitas dan homogenitas",
        "- Statistik Inferensia: Uji hipotesis untuk 1 dan 2 sampel",
        "- ANOVA: Analisis varian satu dan dua arah",
        "- Regresi Linear Berganda: Analisis regresi dengan uji asumsi",
        "",
        "## Informasi Pengembang",
        "Dashboard ini dibuat untuk memenuhi UAS Mata Kuliah Komputasi Statistik",
        "Program Studi Komputasi Statistik - Politeknik Statistika STIS",
        "Semester Genap TA. 2024/2025"
      ), temp_md)
      
      # Convert to PDF using pandoc (if available)
      rmarkdown::render(temp_md, output_format = "pdf_document", output_file = file)
    }
  )
  
  # Data Management Downloads
  output$download_data_mgmt <- downloadHandler(
    filename = function() {
      paste("data_management_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(values$modified_data)) {
        write.csv(values$modified_data, file, row.names = FALSE)
      } else {
        write.csv(values$data, file, row.names = FALSE)
      }
    }
  )
  
  # Cluster Results Download
  output$download_cluster_results <- downloadHandler(
    filename = function() {
      paste0("cluster_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(input$cluster_vars, input$n_clusters)
      
      # Perform clustering again for download
      cluster_data <- sovi_geojson@data[, input$cluster_vars, drop = FALSE]
      cluster_data <- na.omit(cluster_data)
      scaled_data <- scale(cluster_data)
      set.seed(123)
      clust_result <- kmeans(scaled_data, centers = input$n_clusters)
      
      # Create result dataframe
      result_df <- sovi_geojson@data
      result_df$Cluster <- NA
      result_df$Cluster[as.numeric(rownames(cluster_data))] <- clust_result$cluster
      
      write.csv(result_df, file, row.names = FALSE)
    }
  )
  
  # =================== EKSPLORASI DATA DOWNLOAD HANDLERS ===================

# 1. Download Plot Eksplorasi (JPG)
output$download_exploration_plot <- downloadHandler(
  filename = function() {
    paste0("eksplorasi_plot_", Sys.Date(), ".jpg")
  },
  content = function(file) {
    # Simpan plot terakhir sesuai tipe eksplorasi
    if (input$explore_type == "hist") {
      p <- ggplot(values$data, aes_string(x = input$explore_var)) +
        geom_histogram(bins = 30, fill = "#9EF09E", color = "#232935") +
        theme_minimal() +
        labs(title = paste("Histogram dari", input$explore_var))
    } else if (input$explore_type == "box") {
      p <- ggplot(values$data, aes_string(y = input$explore_var)) +
        geom_boxplot(fill = "#9EF09E", color = "#232935") +
        theme_minimal() +
        labs(title = paste("Boxplot dari", input$explore_var))
    } else if (input$explore_type == "corr") {
      numeric_data <- select_if(values$data, is.numeric)
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
      cor_df$value <- as.vector(cor_matrix)
      p <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "#FF5005", high = "#51CD37", mid = "#DADFE9", midpoint = 0) +
        theme_minimal() +
        labs(title = "Matriks Korelasi")
    } else if (input$explore_type == "scatter") {
      req(input$scatter_y)
      p <- ggplot(values$data, aes_string(x = input$explore_var, y = input$scatter_y)) +
        geom_point(color = "#9EF09E") +
        geom_smooth(method = "lm", color = "#51CD37") +
        theme_minimal() +
        labs(title = paste("Plot Scatter:", input$explore_var, "vs", input$scatter_y))
    } else {
      # Untuk peta, tidak bisa diekspor langsung ke JPG, skip
      p <- NULL
    }
    if (!is.null(p)) {
      ggsave(file, plot = p, device = "jpeg", width = 8, height = 5)
    } else {
      file.create(file) # Buat file kosong jika tidak ada plot
    }
  }
)

# 2. Download Tabel Eksplorasi (CSV)
output$download_exploration_table <- downloadHandler(
  filename = function() {
    paste0("eksplorasi_tabel_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv(values$data, file, row.names = FALSE)
  }
)

# 3. Download Laporan Eksplorasi (PDF)
output$download_exploration_report <- downloadHandler(
  filename = function() {
    paste0("laporan_eksplorasi_", Sys.Date(), ".pdf")
  },
  content = function(file) {
    temp_md <- tempfile(fileext = ".md")
    temp_plot <- tempfile(fileext = ".png")
    interpretasi <- paste(
      "Analisis eksplorasi untuk variabel:", input$explore_var, 
      "- Analisis", input$explore_type, "memberikan wawasan tentang distribusi dan karakteristik data."
    )
    # Buat plot sesuai tipe eksplorasi
    p <- NULL
    if (input$explore_type == "hist") {
      p <- ggplot(values$data, aes_string(x = input$explore_var)) +
        geom_histogram(bins = 30, fill = "#9EF09E", color = "#232935") +
        theme_minimal() +
        labs(title = paste("Histogram dari", input$explore_var))
    } else if (input$explore_type == "box") {
      p <- ggplot(values$data, aes_string(y = input$explore_var)) +
        geom_boxplot(fill = "#9EF09E", color = "#232935") +
        theme_minimal() +
        labs(title = paste("Boxplot dari", input$explore_var))
    } else if (input$explore_type == "corr") {
      numeric_data <- select_if(values$data, is.numeric)
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
      cor_df$value <- as.vector(cor_matrix)
      p <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "#FF5005", high = "#51CD37", mid = "#DADFE9", midpoint = 0) +
        theme_minimal() +
        labs(title = "Matriks Korelasi")
    } else if (input$explore_type == "scatter") {
      req(input$scatter_y)
      p <- ggplot(values$data, aes_string(x = input$explore_var, y = input$scatter_y)) +
        geom_point(color = "#9EF09E") +
        geom_smooth(method = "lm", color = "#51CD37") +
        theme_minimal() +
        labs(title = paste("Plot Scatter:", input$explore_var, "vs", input$scatter_y))
    }
    # Simpan plot jika ada
    if (!is.null(p)) {
      ggsave(temp_plot, plot = p, device = "png", width = 7, height = 4)
      plot_md <- paste0("![](", temp_plot, ")")
    } else {
      plot_md <- "(Plot tidak tersedia untuk tipe ini)"
    }
    lines <- c(
      "# Laporan Eksplorasi Data",
      paste0("## Variabel: ", input$explore_var),
      paste0("## Tipe Analisis: ", input$explore_type),
      "",
      "### Statistik Deskriptif:",
      capture.output(
        if (is.numeric(values$data[[input$explore_var]])) summary(values$data[[input$explore_var]]) else table(values$data[[input$explore_var]])
      ),
      "",
      "### Grafik Hasil Eksplorasi:",
      plot_md,
      "",
      "### Interpretasi:",
      interpretasi
    )
    writeLines(lines, temp_md)
    rmarkdown::render(temp_md, output_format = "pdf_document", output_file = file)
  }
)

# 4. Download Halaman Eksplorasi Lengkap (PDF)
output$download_exploration_page <- downloadHandler(
  filename = function() {
    paste0("halaman_eksplorasi_", Sys.Date(), ".pdf")
  },
  content = function(file) {
    temp_md <- tempfile(fileext = ".md")
    temp_plot <- tempfile(fileext = ".png")
    interpretasi <- paste(
      "Analisis eksplorasi untuk variabel:", input$explore_var, 
      "- Analisis", input$explore_type, "memberikan wawasan tentang distribusi dan karakteristik data."
    )
    # Buat plot sesuai tipe eksplorasi
    p <- NULL
    if (input$explore_type == "hist") {
      p <- ggplot(values$data, aes_string(x = input$explore_var)) +
        geom_histogram(bins = 30, fill = "#9EF09E", color = "#232935") +
        theme_minimal() +
        labs(title = paste("Histogram dari", input$explore_var))
    } else if (input$explore_type == "box") {
      p <- ggplot(values$data, aes_string(y = input$explore_var)) +
        geom_boxplot(fill = "#9EF09E", color = "#232935") +
        theme_minimal() +
        labs(title = paste("Boxplot dari", input$explore_var))
    } else if (input$explore_type == "corr") {
      numeric_data <- select_if(values$data, is.numeric)
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      cor_df <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
      cor_df$value <- as.vector(cor_matrix)
      p <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "#FF5005", high = "#51CD37", mid = "#DADFE9", midpoint = 0) +
        theme_minimal() +
        labs(title = "Matriks Korelasi")
    } else if (input$explore_type == "scatter") {
      req(input$scatter_y)
      p <- ggplot(values$data, aes_string(x = input$explore_var, y = input$scatter_y)) +
        geom_point(color = "#9EF09E") +
        geom_smooth(method = "lm", color = "#51CD37") +
        theme_minimal() +
        labs(title = paste("Plot Scatter:", input$explore_var, "vs", input$scatter_y))
    }
    # Simpan plot jika ada
    if (!is.null(p)) {
      ggsave(temp_plot, plot = p, device = "png", width = 7, height = 4)
      plot_md <- paste0("![](", temp_plot, ")")
    } else {
      plot_md <- "(Plot tidak tersedia untuk tipe ini)"
    }
    # Tabel preview sesuai tipe eksplorasi
    preview_n <- 8
    if (input$explore_type == "hist" || input$explore_type == "box" || input$explore_type == "map") {
      data_preview <- data.frame(No = 1:preview_n, values$data[[input$explore_var]][1:preview_n])
      colnames(data_preview)[2] <- input$explore_var
    } else if (input$explore_type == "scatter") {
      data_preview <- data.frame(No = 1:preview_n,
                                 values$data[[input$explore_var]][1:preview_n],
                                 values$data[[input$scatter_y]][1:preview_n])
      colnames(data_preview)[2:3] <- c(input$explore_var, input$scatter_y)
    } else if (input$explore_type == "corr") {
      numeric_data <- select_if(values$data, is.numeric)
      data_preview <- data.frame(No = 1:preview_n, head(numeric_data, preview_n))
    } else {
      data_preview <- data.frame(No = 1:preview_n, values$data[[input$explore_var]][1:preview_n])
      colnames(data_preview)[2] <- input$explore_var
    }
    # Buat header dan garis bawah satu kali
    header_md <- paste0("| ", paste(names(data_preview), collapse = " | "), " |")
    garis_md  <- paste0("|", paste(rep("---", ncol(data_preview)), collapse = "|"), "|")
    baris_md  <- apply(data_preview, 1, function(row) paste("|", paste(row, collapse = " | "), "|"))
    table_md  <- paste(c(header_md, garis_md, baris_md), collapse = "\n")
    total_n <- nrow(values$data)
    keterangan <- paste0("Tabel berikut hanya menampilkan sebagian data (", preview_n, " baris pertama) dari total ", total_n, " baris.\n")
    lines <- c(
      "# Halaman Eksplorasi Data",
      paste0("## Variabel: ", input$explore_var),
      paste0("## Tipe Analisis: ", input$explore_type),
      "",
      "### Statistik Deskriptif:",
      capture.output(
        if (is.numeric(values$data[[input$explore_var]])) summary(values$data[[input$explore_var]]) else table(values$data[[input$explore_var]])
      ),
      "",
      "### Grafik Hasil Eksplorasi:",
      plot_md,
      "",
      "### Tabel Data (Preview):",
      keterangan,
      table_md,
      "",
      "### Interpretasi:",
      interpretasi
    )
    writeLines(lines, temp_md)
    rmarkdown::render(temp_md, output_format = "pdf_document", output_file = file)
  }
)
  
  # Add %R% operator definition
  `%R%` <- function(x, y) {
    paste(rep(x, y), collapse = "")
  }

  # =================== UJI ASUMSI DOWNLOAD HANDLERS ===================

  # 1. Download Plot Uji Asumsi (JPG)
  output$download_assumption_plot <- downloadHandler(
    filename = function() {
      paste0("uji_asumsi_plot_", Sys.Date(), ".jpg")
    },
    content = function(file) {
      p <- NULL
      if (input$test_type == "normality") {
        p <- ggplot(values$data, aes_string(sample = input$assumption_var)) +
          stat_qq() +
          stat_qq_line(color = "#51CD37") +
          theme_minimal() +
          labs(title = paste("Plot Q-Q untuk", input$assumption_var))
      } else if (input$test_type == "homogeneity" && input$assumption_group != "none") {
        p <- ggplot(values$data, aes_string(x = input$assumption_group, y = input$assumption_var, fill = input$assumption_group)) +
          geom_boxplot() +
          theme_minimal() +
          labs(title = paste("Boxplot", input$assumption_var, "berdasarkan", input$assumption_group))
      }
      if (!is.null(p)) {
        ggsave(file, plot = p, device = "jpeg", width = 7, height = 4)
      } else {
        file.create(file)
      }
    }
  )

  # 2. Download Hasil Uji Asumsi (TXT)
  output$download_assumption_results <- downloadHandler(
    filename = function() {
      paste0("uji_asumsi_hasil_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      temp_md <- tempfile(fileext = ".md")
      temp_plot <- tempfile(fileext = ".png")
      interpretasi <- NULL
      hasil_uji <- NULL
      p <- NULL
      if (input$test_type == "normality") {
        var_data <- values$data[[input$assumption_var]]
        if (input$normality_test == "shapiro") {
          hasil_uji <- capture.output(shapiro.test(var_data))
          test_result <- shapiro.test(var_data)
        } else if (input$normality_test == "ad") {
          hasil_uji <- capture.output(nortest::ad.test(var_data))
          test_result <- nortest::ad.test(var_data)
        } else {
          hasil_uji <- capture.output(ks.test(var_data, "pnorm", mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE)))
          test_result <- ks.test(var_data, "pnorm", mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE))
        }
        interpretasi <- if (test_result$p.value < 0.05) {
          "Uji normalitas menunjukkan bahwa data TIDAK mengikuti distribusi normal (p < 0.05). Asumsi normalitas dilanggar."
        } else {
          "Uji normalitas menunjukkan bahwa data mengikuti distribusi normal (p >= 0.05). Asumsi normalitas terpenuhi."
        }
        p <- ggplot(values$data, aes_string(sample = input$assumption_var)) +
          stat_qq() +
          stat_qq_line(color = "#51CD37") +
          theme_minimal() +
          labs(title = paste("Plot Q-Q untuk", input$assumption_var))
      } else if (input$test_type == "homogeneity" && input$assumption_group != "none") {
        var_data <- values$data[[input$assumption_var]]
        group_data <- as.factor(values$data[[input$assumption_group]])
        if (input$homogeneity_test == "levene") {
          hasil_uji <- capture.output(car::leveneTest(var_data, group_data))
          test_result <- car::leveneTest(var_data, group_data)
          pval <- test_result$`Pr(>F)`[1]
        } else if (input$homogeneity_test == "bartlett") {
          hasil_uji <- capture.output(bartlett.test(var_data, group_data))
          test_result <- bartlett.test(var_data, group_data)
          pval <- test_result$p.value
        } else if (input$homogeneity_test == "fligner") {
          hasil_uji <- capture.output(fligner.test(var_data, group_data))
          test_result <- fligner.test(var_data, group_data)
          pval <- test_result$p.value
        }
        interpretasi <- if (pval < 0.05) {
          "Uji homogenitas menunjukkan bahwa varians antar grup TIDAK homogen (p < 0.05). Asumsi homogenitas dilanggar."
        } else {
          "Uji homogenitas menunjukkan bahwa varians antar grup homogen (p >= 0.05). Asumsi homogenitas terpenuhi."
        }
        p <- ggplot(values$data, aes_string(x = input$assumption_group, y = input$assumption_var, fill = input$assumption_group)) +
          geom_boxplot() +
          theme_minimal() +
          labs(title = paste("Boxplot", input$assumption_var, "berdasarkan", input$assumption_group))
      }
      # Simpan plot jika ada
      if (!is.null(p)) {
        ggsave(temp_plot, plot = p, device = "png", width = 7, height = 4)
        plot_md <- paste0("![](", temp_plot, ")")
      } else {
        plot_md <- "(Plot tidak tersedia untuk tipe ini)"
      }
      lines <- c(
        "# Hasil Uji Asumsi Data",
        paste0("## Variabel: ", input$assumption_var),
        paste0("## Tipe Uji: ", input$test_type),
        "",
        "### Hasil Uji Statistik:",
        hasil_uji,
        "",
        "### Interpretasi:",
        interpretasi,
        "",
        "### Grafik Uji Asumsi:",
        plot_md
      )
      writeLines(lines, temp_md)
      rmarkdown::render(temp_md, output_format = "pdf_document", output_file = file)
    }
  )

  # 3. Download Halaman Uji Asumsi Lengkap (PDF)
  output$download_assumption_page <- downloadHandler(
    filename = function() {
      paste0("halaman_uji_asumsi_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      temp_md <- tempfile(fileext = ".md")
      temp_plot <- tempfile(fileext = ".png")
      interpretasi <- NULL
      hasil_uji <- NULL
      p <- NULL
      if (input$test_type == "normality") {
        var_data <- values$data[[input$assumption_var]]
        if (input$normality_test == "shapiro") {
          hasil_uji <- capture.output(shapiro.test(var_data))
          test_result <- shapiro.test(var_data)
        } else if (input$normality_test == "ad") {
          hasil_uji <- capture.output(nortest::ad.test(var_data))
          test_result <- nortest::ad.test(var_data)
        } else {
          hasil_uji <- capture.output(ks.test(var_data, "pnorm", mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE)))
          test_result <- ks.test(var_data, "pnorm", mean(var_data, na.rm = TRUE), sd(var_data, na.rm = TRUE))
        }
        interpretasi <- if (test_result$p.value < 0.05) {
          "Uji normalitas menunjukkan bahwa data TIDAK mengikuti distribusi normal (p < 0.05). Asumsi normalitas dilanggar."
        } else {
          "Uji normalitas menunjukkan bahwa data mengikuti distribusi normal (p >= 0.05). Asumsi normalitas terpenuhi."
        }
        p <- ggplot(values$data, aes_string(sample = input$assumption_var)) +
          stat_qq() +
          stat_qq_line(color = "#51CD37") +
          theme_minimal() +
          labs(title = paste("Plot Q-Q untuk", input$assumption_var))
      } else if (input$test_type == "homogeneity" && input$assumption_group != "none") {
        var_data <- values$data[[input$assumption_var]]
        group_data <- as.factor(values$data[[input$assumption_group]])
        if (input$homogeneity_test == "levene") {
          hasil_uji <- capture.output(car::leveneTest(var_data, group_data))
          test_result <- car::leveneTest(var_data, group_data)
          pval <- test_result$`Pr(>F)`[1]
        } else if (input$homogeneity_test == "bartlett") {
          hasil_uji <- capture.output(bartlett.test(var_data, group_data))
          test_result <- bartlett.test(var_data, group_data)
          pval <- test_result$p.value
        } else if (input$homogeneity_test == "fligner") {
          hasil_uji <- capture.output(fligner.test(var_data, group_data))
          test_result <- fligner.test(var_data, group_data)
          pval <- test_result$p.value
        }
        interpretasi <- if (pval < 0.05) {
          "Uji homogenitas menunjukkan bahwa varians antar grup TIDAK homogen (p < 0.05). Asumsi homogenitas dilanggar."
        } else {
          "Uji homogenitas menunjukkan bahwa varians antar grup homogen (p >= 0.05). Asumsi homogenitas terpenuhi."
        }
        p <- ggplot(values$data, aes_string(x = input$assumption_group, y = input$assumption_var, fill = input$assumption_group)) +
          geom_boxplot() +
          theme_minimal() +
          labs(title = paste("Boxplot", input$assumption_var, "berdasarkan", input$assumption_group))
      }
      # Simpan plot jika ada
      if (!is.null(p)) {
        ggsave(temp_plot, plot = p, device = "png", width = 7, height = 4)
        plot_md <- paste0("![](", temp_plot, ")")
      } else {
        plot_md <- "(Plot tidak tersedia untuk tipe ini)"
      }
      lines <- c(
        "# Halaman Uji Asumsi Data",
        paste0("## Variabel: ", input$assumption_var),
        paste0("## Tipe Uji: ", input$test_type),
        "",
        "### Hasil Uji Statistik:",
        hasil_uji,
        "",
        "### Grafik Uji Asumsi:",
        plot_md,
        "",
        "### Interpretasi:",
        interpretasi
      )
      writeLines(lines, temp_md)
      rmarkdown::render(temp_md, output_format = "pdf_document", output_file = file)
    }
  )

  # =================== BERANDA SERVER (INFO BOX & TABEL VARIABEL) ===================
  output$box_prov <- renderValueBox({
    n_prov <- length(unique(sovi_data$nmprov))
    valueBox(n_prov, "Total Provinsi", icon = icon("map-marker-alt"), color = "green")
  })
  output$box_kab <- renderValueBox({
    n_kab <- length(unique(sovi_data$nmkab))
    valueBox(n_kab, "Total Kabupaten/Kota", icon = icon("city"), color = "blue")
  })
  output$box_var <- renderValueBox({
    n_var <- ncol(sovi_data)
    valueBox(n_var, "Total Variabel", icon = icon("list"), color = "teal")
  })
  output$box_row <- renderValueBox({
    n_row <- nrow(sovi_data)
    valueBox(n_row, "Total Baris", icon = icon("table"), color = "purple")
  })

  # Tabel info variabel
  output$tabel_varinfo <- DT::renderDataTable({
    varinfo <- data.frame(
      "Nama Kolom" = c(
        "FID", "gid", "kdkab", "kdprov", "nmkab", "nmprov", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD2", "FAMILYSIZ", "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING", "DPRONE", "RENTED", "NOSWERE", "TAPWATER", "POPULATION"
      ),
      "Deskripsi" = c(
        "Nomor urut fitur spasial (internal ID)",
        "Global ID unik untuk setiap entitas wilayah",
        "Kode kabupaten/kota (biasanya 2 digit, sesuai standar BPS)",
        "Kode provinsi tempat kabupaten tersebut berada",
        "Nama kabupaten/kota",
        "Nama provinsi",
        "Persentase penduduk usia di bawah lima tahun",
        "Persentase penduduk perempuan",
        "Persentase penduduk usia ≥65 tahun dan populasi lanjut usia",
        "Persentase rumah tangga dengan kepala rumah tangga perempuan",
        "Rata-rata jumlah anggota rumah tangga dalam satu wilayah",
        "Persentase rumah tangga yang tidak menggunakan listrik sebagai sumber penerangan",
        "Persentase penduduk ≥15 tahun yang tidak tamat sekolah (berpendidikan rendah)",
        "Laju pertumbuhan penduduk (persentase perubahan jumlah penduduk)",
        "Persentase penduduk miskin",
        "Persentase penduduk yang tidak dapat membaca dan menulis",
        "Persentase rumah tangga yang tidak mengikuti pelatihan bencana",
        "Persentase rumah tangga yang tinggal di daerah rawan bencana",
        "Persentase rumah tangga yang tinggal di rumah sewa",
        "Persentase rumah tangga yang tidak memiliki sistem drainase",
        "Persentase rumah tangga yang menggunakan air leding sebagai sumber air utama",
        "Jumlah total penduduk di wilayah tersebut"
      ),
      stringsAsFactors = FALSE
    )
    DT::datatable(varinfo, options = list(dom = 't', pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  # Data Management Downloads
  output$download_mgmt_report <- downloadHandler(
    filename = function() {
      paste("laporan_manajemen_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      temp_md <- tempfile(fileext = ".md")
      # Ambil data
      orig_data <- values$data
      mod_data <- values$modified_data
      var_cat <- input$var_to_categorize
      cat_var <- paste0(var_cat, "_cat")
      # Interpretasi
      interpretasi <- NULL
      if (!is.null(mod_data) && cat_var %in% names(mod_data)) {
        categories <- levels(mod_data[[cat_var]])
        label_info <- if (input$label_type == "numeric") {
          paste("menggunakan label", paste(categories, collapse = ", "))
        } else {
          paste("menggunakan label", paste(categories, collapse = ", "))
        }
        if (input$categorize_method == "equal") {
          interpretasi <- paste0(
            "Variabel kontinu ", var_cat, " telah berhasil dikategorikan menjadi ",
            input$num_categories, " kategori menggunakan metode equal intervals, ",
            label_info, ". Metode yang dipilih membagi data dengan interval yang sama panjang. Transformasi ini memungkinkan analisis kategorikal pada data asli yang kontinu."
          )
        } else if (input$categorize_method == "quantile") {
          interpretasi <- paste0(
            "Variabel kontinu ", var_cat, " telah berhasil dikategorikan menjadi ",
            input$num_categories, " kategori menggunakan metode quantiles, ",
            label_info, ". Metode yang dipilih membagi data dengan frekuensi yang sama banyak. Transformasi ini memungkinkan analisis kategorikal pada data asli yang kontinu."
          )
        } else {
          interpretasi <- paste(
            "Variabel kontinu", var_cat, "telah berhasil dikategorikan menjadi",
            input$num_categories, "kategori menggunakan metode custom breaks,",
            label_info, ". Transformasi ini memungkinkan analisis kategorikal pada data asli yang kontinu.")
        }
      } else {
        interpretasi <- "Belum ada hasil kategorisasi yang diterapkan."
      }
      # Tabel data preview (asli & hasil kategorisasi)
      preview_n <- 10
      orig_preview <- head(orig_data, preview_n)
      if (!is.null(mod_data) && cat_var %in% names(mod_data)) {
        mod_preview <- head(mod_data[, c(var_cat, cat_var)], preview_n)
      } else {
        mod_preview <- NULL
      }
      # Markdown lines
      lines <- c(
        "# Laporan Manajemen Data",
        paste0("## Variabel yang Dikategorikan: ", var_cat),
        paste0("## Metode Kategorisasi: ", input$categorize_method),
        paste0("## Jumlah Kategori: ", input$num_categories),
        paste0("## Label: ", if (!is.null(mod_data) && cat_var %in% names(mod_data)) paste(levels(mod_data[[cat_var]]), collapse = ", ") else "-"),
        "",
        "### Interpretasi:",
        interpretasi,
        "",
        if (!is.null(mod_preview)) c("### Pratinjau Data Setelah Kategorisasi:", knitr::kable(mod_preview, format = "markdown")) else NULL
      )
      writeLines(unlist(lines), temp_md)
      rmarkdown::render(temp_md, output_format = "pdf_document", output_file = file)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)