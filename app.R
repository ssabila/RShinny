# Dashboard Climate Change Analysis - Jawa Timur
# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(leaflet)
library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(VIM)
library(forecast)
library(tseries)
library(nortest)
library(robust)
library(MASS)
library(shinycssloaders)
library(moments)

# Custom CSS theme for climate change
climate_css <- "
.content-wrapper, .right-side {
  background-color: #F0FFF0;
}
.main-header .navbar {
  background-color: #1E6B4F !important;
}
.main-header .logo {
  background-color: #1E6B4F !important;
}
.skin-blue .main-sidebar {
  background-color: #1E4D3B;
}
.skin-blue .sidebar-menu > li.active > a {
  background-color: #2E8B57;
}
.skin-blue .sidebar-menu > li:hover > a {
  background-color: #2E8B57;
}
.box.box-solid.box-primary > .box-header {
  background-color: #2E8B57;
}
.nav-tabs-custom > .nav-tabs > li.active {
  border-top-color: #2E8B57;
}
.btn-primary {
  background-color: #2E8B57;
  border-color: #1E6B4F;
}
.btn-primary:hover {
  background-color: #1E6B4F;
  border-color: #0F4C3A;
}
"

# Load and prepare data from Excel file
load_initial_data <- function(file_path = "1_DATA JATIM_GABUNG.xlsx") {
  
  # Baca data dari Excel
  raw_data <- read_excel(file_path, sheet = 1)
  
  # Bersihkan nama kolom sesuai dengan struktur asli
  colnames(raw_data) <- c(
    "Nama_Daerah",
    "Bulan", 
    "Tahun",
    "Produktivitas",
    "Curah_Hujan",
    "Suhu", 
    "Radiasi_Matahari",
    "Kelembapan",
    "NDVI",
    "CO",
    "Soil_Moisture"
  )
  
  # Konversi tipe data
  raw_data$Nama_Daerah <- as.character(raw_data$Nama_Daerah)
  raw_data$Bulan <- as.numeric(raw_data$Bulan)
  raw_data$Tahun <- as.numeric(raw_data$Tahun)
  raw_data$Produktivitas <- as.numeric(raw_data$Produktivitas)
  raw_data$Curah_Hujan <- as.numeric(raw_data$Curah_Hujan)
  raw_data$Suhu <- as.numeric(raw_data$Suhu)
  raw_data$Radiasi_Matahari <- as.numeric(raw_data$Radiasi_Matahari)
  raw_data$Kelembapan <- as.numeric(raw_data$Kelembapan)
  raw_data$NDVI <- as.numeric(raw_data$NDVI)
  raw_data$CO <- as.numeric(raw_data$CO)
  raw_data$Soil_Moisture <- as.numeric(raw_data$Soil_Moisture)
  
  # Hapus baris dengan NA values jika ada
  raw_data <- raw_data[complete.cases(raw_data), ]
  
  # Urutkan data berdasarkan daerah, tahun, dan bulan
  raw_data <- raw_data %>%
    arrange(Nama_Daerah, Tahun, Bulan) %>%
    mutate(
      # Buat ID unik untuk setiap observasi
      ID = paste(Nama_Daerah, Tahun, sprintf("%02d", Bulan), sep = "_"),
      
      # Tambahkan kategori daerah (Kota vs Kabupaten)
      Jenis_Daerah = ifelse(grepl("^Kota", Nama_Daerah), "Kota", "Kabupaten"),
      
      # Buat tanggal untuk analisis time series
      Tanggal = as.Date(paste(Tahun, Bulan, "01", sep = "-")),
      
      # Kategorisasi musim
      Musim = case_when(
        Bulan %in% c(12, 1, 2) ~ "Musim Hujan",
        Bulan %in% c(3, 4, 5) ~ "Peralihan I", 
        Bulan %in% c(6, 7, 8) ~ "Musim Kemarau",
        Bulan %in% c(9, 10, 11) ~ "Peralihan II"
      )
    )
  
  return(raw_data)
}

# UI
ui <- dashboardPage(
  title = "Climate Change Dashboard - Jawa Timur",
  
  # Header
  dashboardHeader(
    title = "🌍 Climate Change Dashboard - Jawa Timur",
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("🏠 Dashboard Overview", tabName = "overview", icon = icon("home")),
      menuItem("📖 Guide Penggunaan", tabName = "guide", icon = icon("question-circle")),
      menuItem("📊 Analisis Deskriptif", tabName = "descriptive", icon = icon("chart-bar")),
      menuItem("🔗 Analisis Inferensia", tabName = "inferential", icon = icon("project-diagram"),
               menuSubItem("Korelasi Spearman", tabName = "correlation"),
               menuSubItem("Kruskal Wallis Test", tabName = "kruskal"),
               menuSubItem("Regresi Robust", tabName = "regression")
      ),
      menuItem("📈 Time Series Analysis", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("🔮 Prediksi & Simulasi", tabName = "prediction", icon = icon("magic")),
      menuItem("📊 Visualisasi Lanjutan", tabName = "advanced_viz", icon = icon("chart-pie")),
      menuItem("🗺️ Visualisasi Spasial", tabName = "spatial", icon = icon("map")),
      menuItem("📝 Catatan Riset", tabName = "research", icon = icon("book")),
      menuItem("⬇️ Download Data", tabName = "download", icon = icon("download")),
      menuItem("⬆️ Upload Data", tabName = "upload", icon = icon("upload"))
    )
  ),
  
  # Body
  dashboardBody(
    
    tags$head(
      tags$style(HTML(climate_css))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "🌍 Climate Change Dashboard - Jawa Timur", status = "primary", solidHeader = TRUE, width = 12,
                  h4("Selamat datang di Dashboard Analisis Perubahan Iklim Jawa Timur"),
                  p("Dashboard ini menyediakan analisis komprehensif tentang data iklim dan produktivitas pertanian di 38 daerah Jawa Timur periode 2020-2024."),
                  conditionalPanel(
                    condition = "output.data_merged",
                    div(class = "alert alert-success", style = "margin: 10px 0;",
                        HTML("<strong>✅ Data Tambahan Telah Digabung!</strong><br>
                             Dashboard sekarang menggunakan data yang telah digabung dengan variabel tambahan Anda."))
                  ),
                  br(),
                  fluidRow(
                    valueBoxOutput("total_regions", width = 3),
                    valueBoxOutput("total_years", width = 3),
                    valueBoxOutput("total_observations", width = 3),
                    valueBoxOutput("variables_count", width = 3)
                  )
                )
              ),
              fluidRow(
                box(
                  title = "📊 Ringkasan Data Iklim", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("overview_climate_plot")
                ),
                box(
                  title = "🌾 Produktivitas vs Iklim", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("overview_productivity_plot")
                )
              )
      ),
      
      # Guide Tab
      tabItem(tabName = "guide",
              fluidRow(
                box(
                  title = "📖 Panduan Penggunaan Dashboard Climate Change", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("🚀 Memulai",
                             br(),
                             h4("Selamat Datang di Dashboard Climate Change Jawa Timur!"),
                             p("Dashboard ini dirancang untuk membantu Anda menganalisis data iklim dan produktivitas pertanian di Jawa Timur. Berikut adalah langkah-langkah untuk memulai:"),
                             
                             h5("1. 📊 Dashboard Overview"),
                             p("• Mulai dari tab 'Dashboard Overview' untuk melihat ringkasan data"),
                             p("• Lihat informasi dasar seperti jumlah daerah, tahun observasi, dan variabel"),
                             p("• Perhatikan grafik ringkasan untuk mendapat gambaran umum"),
                             
                             h5("2. ⬆️ Upload Data (Opsional)"),
                             p("• Jika Anda memiliki variabel tambahan, gunakan tab 'Upload Data'"),
                             p("• File harus berformat CSV/Excel dengan kolom: Nama_Daerah, Bulan, Tahun"),
                             p("• Setelah upload, data akan otomatis tersedia di semua fitur analisis"),
                             
                             h5("3. 📈 Mulai Analisis"),
                             p("• Pilih jenis analisis sesuai kebutuhan Anda"),
                             p("• Setiap tab memiliki pengaturan filter dan parameter"),
                             p("• Hasil akan ditampilkan dalam bentuk grafik, tabel, dan interpretasi"),
                             
                             div(class = "alert alert-info",
                                 HTML("<strong>💡 Tips:</strong> Mulai dengan Analisis Deskriptif untuk memahami karakteristik data, kemudian lanjut ke analisis yang lebih kompleks."))
                    ),
                    
                    tabPanel("📊 Fitur Analisis",
                             br(),
                             h4("Fitur-Fitur Analisis yang Tersedia:"),
                             
                             h5("1. 📊 Analisis Deskriptif"),
                             tags$ul(
                               tags$li("Statistik dasar (mean, median, std dev, dll.)"),
                               tags$li("Uji normalitas (Shapiro-Wilk, Anderson-Darling, KS)"),
                               tags$li("Visualisasi distribusi (histogram, boxplot)"),
                               tags$li("Filter berdasarkan tahun dan daerah")
                             ),
                             
                             h5("2. 🔗 Analisis Korelasi Spearman"),
                             tags$ul(
                               tags$li("Mengukur hubungan monoton antar variabel"),
                               tags$li("Matriks korelasi visual dan numerik"),
                               tags$li("Filter per tahun dan per daerah"),
                               tags$li("Interpretasi kekuatan korelasi")
                             ),
                             
                             h5("3. 🧪 Kruskal Wallis Test"),
                             tags$ul(
                               tags$li("Uji perbedaan median antar kelompok"),
                               tags$li("Pengelompokan berdasarkan tahun atau daerah"),
                               tags$li("Visualisasi boxplot dengan interpretasi"),
                               tags$li("Cocok untuk data non-normal")
                             ),
                             
                             h5("4. 📈 Regresi Robust"),
                             tags$ul(
                               tags$li("Model regresi tahan outlier"),
                               tags$li("Pemilihan variabel dependen dan independen"),
                               tags$li("Plot residual dan prediksi vs aktual"),
                               tags$li("Interpretasi koefisien model")
                             ),
                             
                             h5("5. 📈 Time Series Analysis"),
                             tags$ul(
                               tags$li("Analisis tren temporal data iklim"),
                               tags$li("Uji stasioneritas (ADF, KPSS)"),
                               tags$li("Model ARIMA otomatis"),
                               tags$li("Interpretasi komponen trend dan seasonal")
                             ),
                             
                             h5("6. 🔮 Prediksi & Simulasi"),
                             tags$ul(
                               tags$li("Prediksi berdasarkan input variabel iklim"),
                               tags$li("Confidence interval untuk prediksi"),
                               tags$li("Analisis sensitivitas"),
                               tags$li("Perbandingan dengan data historis")
                             )
                    ),
                    
                    tabPanel("🗺️ Visualisasi",
                             br(),
                             h4("Fitur Visualisasi yang Tersedia:"),
                             
                             h5("1. 📊 Visualisasi Lanjutan"),
                             tags$ul(
                               tags$li("Heatmap korelasi interaktif"),
                               tags$li("Scatter plot matrix"),
                               tags$li("Plot distribusi per tahun"),
                               tags$li("Radar chart profil iklim")
                             ),
                             
                             h5("2. 🗺️ Visualisasi Spasial"),
                             tags$ul(
                               tags$li("Peta interaktif Jawa Timur"),
                               tags$li("Visualisasi data per daerah"),
                               tags$li("Filter berdasarkan tahun dan bulan"),
                               tags$li("Informasi statistik regional")
                             ),
                             
                             h5("Tips Visualisasi:"),
                             div(class = "alert alert-success",
                                 tags$ul(
                                   tags$li("Gunakan filter untuk fokus pada periode atau wilayah tertentu"),
                                   tags$li("Hover mouse pada grafik untuk informasi detail"),
                                   tags$li("Peta interaktif dapat di-zoom dan di-pan"),
                                   tags$li("Semua grafik dapat di-export sebagai gambar")
                                 ))
                    ),
                    
                    tabPanel("💾 Data & Download",
                             br(),
                             h4("Mengelola Data dan Download:"),
                             
                             h5("1. ⬆️ Upload Data Tambahan"),
                             p("Format file yang didukung:"),
                             tags$ul(
                               tags$li("CSV (.csv)"),
                               tags$li("Excel (.xlsx, .xls)")
                             ),
                             p("Struktur file yang diperlukan:"),
                             tags$ul(
                               tags$li("Kolom wajib: Nama_Daerah, Bulan, Tahun"),
                               tags$li("Kolom tambahan: variabel baru yang ingin ditambahkan"),
                               tags$li("Nama daerah harus sesuai dengan data utama"),
                               tags$li("Periode (tahun-bulan) harus ada yang overlapping")
                             ),
                             
                             h5("2. ⬇️ Download Hasil"),
                             p("Anda dapat mendownload:"),
                             tags$ul(
                               tags$li("Data mentah (termasuk data yang sudah digabung)"),
                               tags$li("Hasil analisis dalam format CSV")
                             ),
                             
                             h5("3. 📝 Catatan Riset"),
                             p("Gunakan tab 'Catatan Riset' untuk:"),
                             tags$ul(
                               tags$li("Memahami metodologi yang digunakan"),
                               tags$li("Interpretasi hasil statistik"),
                               tags$li("Rujukan untuk penulisan laporan"),
                               tags$li("Implikasi kebijakan dari hasil analisis")
                             )
                    ),
                    
                    tabPanel("❓ FAQ & Tips",
                             br(),
                             h4("Frequently Asked Questions:"),
                             
                             h5("Q: Data saya tidak muncul setelah di-upload?"),
                             p("A: Pastikan file memiliki kolom Nama_Daerah, Bulan, Tahun yang sesuai format. Periksa juga apakah nama daerah cocok dengan data utama."),
                             
                             h5("Q: Uji normalitas menunjukkan data tidak normal, apa yang harus dilakukan?"),
                             p("A: Ini normal untuk data iklim. Gunakan metode non-parametrik seperti Korelasi Spearman dan Kruskal-Wallis Test yang sudah tersedia."),
                             
                             h5("Q: Model regresi memberikan hasil yang aneh?"),
                             p("A: Periksa outlier dalam data. Regresi robust sudah menangani outlier, tapi data yang ekstrem tetap bisa mempengaruhi hasil."),
                             
                             h5("Q: Time series menunjukkan data non-stasioner?"),
                             p("A: Ini wajar untuk data iklim. Model ARIMA otomatis akan melakukan differencing yang diperlukan."),
                             
                             br(),
                             h4("Tips Penggunaan Optimal:"),
                             div(class = "alert alert-info",
                                 tags$ul(
                                   tags$li("Mulai dengan analisis deskriptif untuk memahami data"),
                                   tags$li("Gunakan filter tahun/daerah untuk analisis spesifik"),
                                   tags$li("Kombinasikan beberapa jenis analisis untuk insight yang lebih dalam"),
                                   tags$li("Perhatikan interpretasi statistik di setiap hasil"),
                                   tags$li("Simpan hasil penting dengan fitur download"),
                                   tags$li("Gunakan visualisasi spasial untuk memahami pola geografis")
                                 ))
                    )
                  )
                )
              )
      ),
      
      # Descriptive Analysis Tab
      tabItem(tabName = "descriptive",
              fluidRow(
                box(
                  title = "⚙️ Pengaturan Analisis", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("desc_variable", "Pilih Variabel:", 
                              choices = c("Produktivitas", "Curah_Hujan", "Suhu", 
                                          "Radiasi_Matahari", "Kelembapan", 
                                          "NDVI", "CO", "Soil_Moisture")),
                  selectInput("desc_year", "Pilih Tahun:", choices = NULL),
                  selectInput("desc_region", "Pilih Daerah:", choices = NULL),
                  checkboxInput("normality_test", "Uji Normalitas", value = TRUE),
                  actionButton("run_descriptive", "🔍 Analisis", class = "btn-primary", width = "100%")
                ),
                box(
                  title = "📈 Statistik Deskriptif", status = "primary", solidHeader = TRUE, width = 9,
                  verbatimTextOutput("descriptive_stats"),
                  br(),
                  conditionalPanel(
                    condition = "input.normality_test",
                    h4("🧪 Hasil Uji Normalitas"),
                    verbatimTextOutput("normality_results")
                  )
                )
              ),
              fluidRow(
                box(
                  title = "📊 Distribusi Data", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("descriptive_histogram")
                ),
                box(
                  title = "📦 Boxplot Data", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("descriptive_boxplot")
                )
              )
      ),
      
      # Correlation Analysis Tab
      tabItem(tabName = "correlation",
              fluidRow(
                box(
                  title = "⚙️ Pengaturan Korelasi Spearman", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("corr_year", "Pilih Tahun:", choices = c("Semua", 2020:2024), selected = "Semua"),
                  selectInput("corr_region", "Pilih Daerah:", choices = NULL),
                  actionButton("run_correlation", "🔗 Analisis Korelasi", class = "btn-primary", width = "100%")
                ),
                box(
                  title = "🔗 Matriks Korelasi Spearman", status = "primary", solidHeader = TRUE, width = 9,
                  plotOutput("correlation_plot")
                )
              ),
              fluidRow(
                box(
                  title = "📊 Tabel Korelasi", status = "primary", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("correlation_table")
                )
              )
      ),
      
      # Kruskal Wallis Test Tab
      tabItem(tabName = "kruskal",
              fluidRow(
                box(
                  title = "⚙️ Pengaturan Kruskal Wallis", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("kw_variable", "Pilih Variabel Dependen:", 
                              choices = c("Produktivitas", "Curah_Hujan", "Suhu", 
                                          "Radiasi_Matahari", "Kelembapan", 
                                          "NDVI", "CO", "Soil_Moisture")),
                  selectInput("kw_grouping", "Kelompokkan berdasarkan:", 
                              choices = c("Tahun", "Daerah"), selected = "Tahun"),
                  conditionalPanel(
                    condition = "input.kw_grouping == 'Tahun'",
                    selectInput("kw_region_filter", "Filter Daerah:", choices = NULL)
                  ),
                  conditionalPanel(
                    condition = "input.kw_grouping == 'Daerah'",
                    selectInput("kw_year_filter", "Filter Tahun:", choices = 2020:2024, selected = 2024)
                  ),
                  actionButton("run_kruskal", "🧪 Uji Kruskal Wallis", class = "btn-primary", width = "100%")
                ),
                box(
                  title = "🧪 Hasil Kruskal Wallis Test", status = "primary", solidHeader = TRUE, width = 9,
                  verbatimTextOutput("kruskal_results"),
                  br(),
                  plotlyOutput("kruskal_plot")
                )
              )
      ),
      
      # Robust Regression Tab
      tabItem(tabName = "regression",
              fluidRow(
                box(
                  title = "⚙️ Pengaturan Regresi Robust", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("reg_dependent", "Variabel Dependen:", 
                              choices = c("Produktivitas", "Curah_Hujan", "Suhu", 
                                          "Radiasi_Matahari", "Kelembapan", 
                                          "NDVI", "CO", "Soil_Moisture"),
                              selected = "Produktivitas"),
                  checkboxGroupInput("reg_independent", "Variabel Independen:",
                                     choices = c("Curah_Hujan", "Suhu", 
                                                 "Radiasi_Matahari", "Kelembapan", 
                                                 "NDVI", "CO", "Soil_Moisture"),
                                     selected = c("Suhu", "Curah_Hujan", "NDVI")),
                  selectInput("reg_year", "Pilih Tahun:", choices = c("Semua", 2020:2024), selected = "Semua"),
                  selectInput("reg_region", "Pilih Daerah:", choices = NULL),
                  actionButton("run_regression", "📈 Analisis Regresi", class = "btn-primary", width = "100%")
                ),
                box(
                  title = "📈 Hasil Regresi Robust", status = "primary", solidHeader = TRUE, width = 9,
                  verbatimTextOutput("regression_results")
                )
              ),
              fluidRow(
                box(
                  title = "📊 Plot Residual", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("regression_residual_plot")
                ),
                box(
                  title = "🎯 Prediksi vs Aktual", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("regression_prediction_plot")
                )
              )
      ),
      
      # Time Series Analysis Tab
      tabItem(tabName = "timeseries",
              fluidRow(
                box(
                  title = "⚙️ Pengaturan Time Series", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("ts_variable", "Pilih Variabel:", 
                              choices = c("Produktivitas", "Curah_Hujan", "Suhu", 
                                          "Radiasi_Matahari", "Kelembapan", 
                                          "NDVI", "CO", "Soil_Moisture")),
                  selectInput("ts_region", "Pilih Daerah:", choices = NULL),
                  checkboxInput("ts_trend", "Tampilkan Trend", value = TRUE),
                  actionButton("run_timeseries", "📈 Analisis Time Series", class = "btn-primary", width = "100%")
                ),
                box(
                  title = "📈 Plot Time Series", status = "primary", solidHeader = TRUE, width = 9,
                  plotlyOutput("timeseries_plot")
                )
              ),
              fluidRow(
                box(
                  title = "🧪 Interpretasi Stasioneritas & Model", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6,
                           h5("📊 Hasil Uji Stasioneritas:"),
                           verbatimTextOutput("timeseries_tests")
                    ),
                    column(6,
                           h5("📈 Model ARIMA Terbaik:"),
                           verbatimTextOutput("timeseries_model")
                    )
                  ),
                  br(),
                  h5("💡 Interpretasi:"),
                  verbatimTextOutput("timeseries_interpretation")
                )
              )
      ),
      
      # Prediction Tab
      tabItem(tabName = "prediction",
              fluidRow(
                box(
                  title = "⚙️ Pengaturan Prediksi", status = "primary", solidHeader = TRUE, width = 4,
                  selectInput("pred_target", "Variabel Target:", 
                              choices = c("Produktivitas", "NDVI", "Suhu"), 
                              selected = "Produktivitas"),
                  h5("Input Nilai Variabel Prediktor:"),
                  numericInput("pred_rainfall", "Curah Hujan (mm/hari):", value = 10, min = 0, max = 20, step = 0.1),
                  numericInput("pred_suhu", "Suhu (°C):", value = 25, min = 15, max = 35, step = 0.1),
                  numericInput("pred_kelembapan", "Kelembapan (%):", value = 85, min = 60, max = 100, step = 1),
                  numericInput("pred_radiasi", "Radiasi Matahari (10⁴ j/m²):", value = 50, min = 30, max = 70, step = 1),
                  numericInput("pred_ndvi", "NDVI:", value = 0.6, min = 0.3, max = 0.9, step = 0.01),
                  numericInput("pred_co", "CO (mol/m²):", value = 0.025, min = 0.015, max = 0.04, step = 0.001),
                  numericInput("pred_soil", "Soil Moisture:", value = 0.4, min = 0.2, max = 0.6, step = 0.01),
                  actionButton("run_prediction", "🔮 Prediksi", class = "btn-success", width = "100%")
                ),
                box(
                  title = "🔮 Hasil Prediksi", status = "primary", solidHeader = TRUE, width = 8,
                  h4("Model yang Digunakan:"),
                  verbatimTextOutput("prediction_model_info"),
                  br(),
                  h4("Hasil Prediksi:"),
                  verbatimTextOutput("prediction_result"),
                  br(),
                  h4("Confidence Interval:"),
                  verbatimTextOutput("prediction_interval")
                )
              ),
              fluidRow(
                box(
                  title = "📊 Prediksi vs Data Historis", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("prediction_comparison_plot")
                ),
                box(
                  title = "🎯 Sensitivity Analysis", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("sensitivity_plot")
                )
              )
      ),
      
      # Advanced Visualization Tab
      tabItem(tabName = "advanced_viz",
              fluidRow(
                box(
                  title = "⚙️ Pengaturan Visualisasi", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("viz_type", "Jenis Visualisasi:", 
                              choices = c("Heatmap Korelasi" = "heatmap",
                                          "Scatter Matrix" = "scatter_matrix",
                                          "Distribution Plot" = "distribution",
                                          "Climate Radar" = "radar")),
                  conditionalPanel(
                    condition = "input.viz_type == 'distribution'",
                    selectInput("dist_variable", "Pilih Variabel:",
                                choices = c("Produktivitas", "Curah_Hujan", "Suhu", 
                                            "Radiasi_Matahari", "Kelembapan", 
                                            "NDVI", "CO", "Soil_Moisture"))
                  ),
                  conditionalPanel(
                    condition = "input.viz_type == 'radar'",
                    selectInput("radar_region", "Pilih Daerah:", choices = NULL)
                  ),
                  actionButton("update_viz", "🎨 Update Visualisasi", class = "btn-primary", width = "100%")
                ),
                box(
                  title = "📊 Visualisasi Utama", status = "primary", solidHeader = TRUE, width = 9,
                  plotlyOutput("advanced_plot", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = "📈 Plot Tambahan 1", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("additional_plot1")
                ),
                box(
                  title = "📊 Plot Tambahan 2", status = "primary", solidHeader = TRUE, width = 6,
                  plotlyOutput("additional_plot2")
                )
              )
      ),
      
      # Spatial Visualization Tab
      tabItem(tabName = "spatial",
              fluidRow(
                box(
                  title = "⚙️ Pengaturan Peta", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("map_variable", "Pilih Variabel:", 
                              choices = c("Produktivitas", "Curah_Hujan", "Suhu", 
                                          "Radiasi_Matahari", "Kelembapan", 
                                          "NDVI", "CO", "Soil_Moisture")),
                  selectInput("map_year", "Pilih Tahun:", choices = 2020:2024, selected = 2024),
                  selectInput("map_month", "Pilih Bulan:", 
                              choices = setNames(1:12, month.name), selected = 12),
                  radioButtons("map_aggregation", "Agregasi:", 
                               choices = c("Rata-rata" = "mean", "Median" = "median", "Maksimum" = "max"),
                               selected = "mean"),
                  actionButton("update_map", "🗺️ Perbarui Peta", class = "btn-primary", width = "100%")
                ),
                box(
                  title = "🗺️ Peta Interaktif Jawa Timur", status = "primary", solidHeader = TRUE, width = 9,
                  leafletOutput("interactive_map", height = "600px")
                )
              )
      ),
      
      # Research Notes Tab
      tabItem(tabName = "research",
              fluidRow(
                box(
                  title = "📝 Dokumentasi Penelitian", status = "primary", solidHeader = TRUE, width = 12,
                  tabsetPanel(
                    tabPanel("📊 Data yang Digunakan",
                             h4("Sumber Data"),
                             p("Data iklim dan produktivitas pertanian Jawa Timur periode 2020-2024"),
                             h4("Variabel yang Dianalisis:"),
                             tags$ul(
                               tags$li("🌾 Produktivitas: Tingkat produktivitas pertanian"),
                               tags$li("🌧️ Curah Hujan: mm per hari"),
                               tags$li("🌡️ Suhu: Derajat Celsius"),
                               tags$li("☀️ Radiasi: Puluh ribu j/m² hari"),
                               tags$li("💧 Kelembapan: Persentase"),
                               tags$li("🌱 NDVI: Normalized Difference Vegetation Index"),
                               tags$li("☁️ CO: Carbon Monoxide mol/m²"),
                               tags$li("🌍 Soil Moisture: Kelembapan tanah")
                             ),
                             h4("Cakupan Geografis:"),
                             p("38 Kabupaten/Kota di Jawa Timur dengan total 2,280 observasi")
                    ),
                    
                    tabPanel("🔄 Alur Analisis",
                             h4("Tahapan Analisis:"),
                             tags$ol(
                               tags$li("📈 Analisis Deskriptif: Statistik dasar dan uji normalitas"),
                               tags$li("🔗 Analisis Korelasi: Spearman rank correlation"),
                               tags$li("🧪 Uji Perbedaan: Kruskal Wallis test"),
                               tags$li("📊 Regresi Robust: Modeling hubungan antar variabel"),
                               tags$li("📈 Time Series: Analisis tren temporal"),
                               tags$li("🗺️ Visualisasi Spasial: Pemetaan interaktif")
                             ),
                             h4("Metodologi:"),
                             p("Menggunakan pendekatan statistik non-parametrik karena sifat data iklim yang tidak selalu terdistribusi normal.")
                    ),
                    
                    tabPanel("🎯 Hasil Penting",
                             h4("Temuan Utama:"),
                             tags$ul(
                               tags$li("📊 Variabilitas iklim antar daerah dan waktu"),
                               tags$li("🔗 Korelasi antara variabel iklim dan produktivitas"),
                               tags$li("📈 Tren perubahan iklim jangka pendek"),
                               tags$li("🗺️ Pola spasial distribusi variabel iklim")
                             ),
                             br(),
                             h4("Implikasi Kebijakan:"),
                             tags$ul(
                               tags$li("🌾 Adaptasi pertanian terhadap perubahan iklim"),
                               tags$li("💧 Manajemen sumber daya air"),
                               tags$li("🌱 Konservasi lahan dan vegetasi"),
                               tags$li("📋 Perencanaan pembangunan berkelanjutan")
                             )
                    ),
                    
                    tabPanel("📖 Interpretasi Statistika",
                             h4("Interpretasi Hasil Analisis:"),
                             br(),
                             h5("🔗 Korelasi Spearman:"),
                             p("Mengukur kekuatan hubungan monoton antar variabel. Nilai mendekati ±1 menunjukkan hubungan yang kuat."),
                             br(),
                             h5("🧪 Kruskal Wallis Test:"),
                             p("Menguji perbedaan median antar kelompok. P-value < 0.05 menunjukkan perbedaan signifikan."),
                             br(),
                             h5("📈 Regresi Robust:"),
                             p("Model yang tahan terhadap outlier, cocok untuk data iklim yang sering mengandung nilai ekstrem."),
                             br(),
                             h5("📊 Time Series:"),
                             p("Analisis komponen trend, seasonal, dan irregular untuk memahami pola temporal.")
                    )
                  )
                )
              )
      ),
      
      # Download Tab
      tabItem(tabName = "download",
              fluidRow(
                box(
                  title = "⬇️ Download Data dan Hasil", status = "primary", solidHeader = TRUE, width = 12,
                  h4("Pilih data yang ingin didownload:"),
                  br(),
                  fluidRow(
                    column(6,
                           h5("📊 Data Mentah"),
                           downloadButton("download_raw_data", "Download Data Asli", class = "btn-primary btn-block"),
                           br(), br(),
                           p("Data lengkap dalam format CSV dengan semua variabel iklim"),
                           conditionalPanel(
                             condition = "output.data_merged",
                             div(class = "alert alert-info",
                                 HTML("<strong>ℹ️ Info:</strong> Data yang akan didownload sudah termasuk variabel tambahan yang Anda upload."))
                           )
                    ),
                    column(6,
                           h5("📈 Hasil Analisis"),
                           downloadButton("download_analysis", "Download Hasil Analisis", class = "btn-success btn-block"),
                           br(), br(),
                           p("Hasil korelasi, regresi, dan uji statistik dalam format CSV")
                    )
                  )
                )
              )
      ),
      
      # Upload Tab
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "⬆️ Upload Variabel Tambahan", status = "primary", solidHeader = TRUE, width = 12,
                  h4("Upload data variabel tambahan (opsional)"),
                  p("File harus berformat CSV atau Excel dengan kolom: Nama_Daerah, Bulan, Tahun, dan variabel baru"),
                  p(tags$b("Maksimum ukuran file: 50MB")),
                  br(),
                  fileInput("upload_file", "Pilih File:",
                            accept = c(".csv", ".xlsx", ".xls"),
                            buttonLabel = "Browse...",
                            placeholder = "Belum ada file yang dipilih"),
                  br(),
                  conditionalPanel(
                    condition = "output.file_uploaded",
                    h5("Preview Data yang Diupload:"),
                    DT::dataTableOutput("uploaded_preview"),
                    br(),
                    h5("Status Penggabungan:"),
                    verbatimTextOutput("merge_status"),
                    br(),
                    actionButton("merge_data", "🔗 Gabungkan dengan Data Utama", class = "btn-success"),
                    br(), br(),
                    conditionalPanel(
                      condition = "output.data_merged",
                      div(class = "alert alert-success",
                          HTML("<strong>✅ Data Berhasil Digabung!</strong><br>
                               Variabel tambahan sekarang tersedia di semua fitur analisis. Silakan refresh halaman untuk melihat variabel baru di dropdown.")),
                      h5("Preview Data Setelah Digabung:"),
                      DT::dataTableOutput("merged_preview")
                    )
                  )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # File upload handling with enhanced merge functionality
  values <- reactiveValues(
    initial_data = NULL,
    correlation_result = NULL,
    regression_result = NULL,
    uploaded_data = NULL,
    merged_data = NULL,
    data_merged = FALSE
  )
  
  # Load initial data
  observe({
    values$initial_data <- load_initial_data()
  })
  
  # Get current data (original or merged) - FIXED
  current_data <- reactive({
    if(values$data_merged && !is.null(values$merged_data)) {
      return(values$merged_data)
    } else {
      return(values$initial_data)
    }
  })
  
  # Update region choices - FIXED to use current_data()
  observe({
    current_df <- current_data()
    if(!is.null(current_df)) {
      regions <- unique(current_df$Nama_Daerah)
      years <- unique(current_df$Tahun)
      
      # Get all numeric columns for variable choices - DYNAMIC UPDATE
      numeric_cols <- names(current_df)[sapply(current_df, is.numeric)]
      # Remove basic columns that are not variables
      variable_choices <- setdiff(numeric_cols, c("Bulan", "Tahun"))
      
      # Update choices for descriptive analysis
      updateSelectInput(session, "desc_year", 
                        choices = c("Semua" = "All", setNames(years, years)), 
                        selected = "All")
      updateSelectInput(session, "desc_region", 
                        choices = c("Semua" = "All", setNames(regions, regions)), 
                        selected = "All")
      
      # Update variable choices if new variables are added
      updateSelectInput(session, "desc_variable", choices = variable_choices)
      updateSelectInput(session, "kw_variable", choices = variable_choices)
      updateSelectInput(session, "ts_variable", choices = variable_choices)
      updateSelectInput(session, "map_variable", choices = variable_choices)
      updateSelectInput(session, "dist_variable", choices = variable_choices)
      updateSelectInput(session, "reg_dependent", choices = variable_choices)
      updateCheckboxGroupInput(session, "reg_independent", choices = variable_choices, 
                               selected = intersect(c("Suhu", "Curah_Hujan", "NDVI"), variable_choices))
      
      # Update other choices
      updateSelectInput(session, "corr_region", choices = c("Semua", regions), selected = "Semua")
      updateSelectInput(session, "kw_region_filter", choices = c("Semua", regions), selected = "Semua")
      updateSelectInput(session, "reg_region", choices = c("Semua", regions), selected = "Semua")
      updateSelectInput(session, "ts_region", choices = regions, selected = regions[1])
      updateSelectInput(session, "radar_region", choices = regions, selected = regions[1])
    }
  })
  
  output$file_uploaded <- reactive({
    return(!is.null(input$upload_file))
  })
  outputOptions(output, 'file_uploaded', suspendWhenHidden = FALSE)
  
  output$data_merged <- reactive({
    return(values$data_merged)
  })
  outputOptions(output, 'data_merged', suspendWhenHidden = FALSE)
  
  observeEvent(input$upload_file, {
    req(input$upload_file)
    
    # Check file size (50MB limit)
    file_size_mb <- input$upload_file$size / (1024 * 1024)
    if(file_size_mb > 50) {
      output$merge_status <- renderText({
        paste("ERROR: File terlalu besar (", round(file_size_mb, 1), 
              "MB). Maksimum 50MB diizinkan.")
      })
      return()
    }
    
    tryCatch({
      ext <- tools::file_ext(input$upload_file$datapath)
      
      if (ext == "csv") {
        uploaded_data <- read.csv(input$upload_file$datapath, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        uploaded_data <- read_excel(input$upload_file$datapath)
      } else {
        output$merge_status <- renderText("ERROR: Format file tidak didukung. Gunakan CSV atau Excel.")
        return()
      }
      
      # Validate required columns
      required_cols <- c("Nama_Daerah", "Bulan", "Tahun")
      missing_cols <- setdiff(required_cols, colnames(uploaded_data))
      
      if(length(missing_cols) > 0) {
        output$merge_status <- renderText({
          paste("ERROR: Kolom wajib tidak ditemukan:", paste(missing_cols, collapse = ", "),
                "\nFile harus memiliki kolom: Nama_Daerah, Bulan, Tahun")
        })
        return()
      }
      
      # Store uploaded data
      values$uploaded_data <- uploaded_data
      
      # Check data compatibility
      main_regions <- unique(values$initial_data$Nama_Daerah)
      upload_regions <- unique(uploaded_data$Nama_Daerah)
      common_regions <- intersect(main_regions, upload_regions)
      
      main_periods <- unique(paste(values$initial_data$Tahun, values$initial_data$Bulan))
      upload_periods <- unique(paste(uploaded_data$Tahun, uploaded_data$Bulan))
      common_periods <- intersect(main_periods, upload_periods)
      
      output$merge_status <- renderText({
        paste(
          "VALIDASI DATA UPLOAD:\n",
          "====================\n",
          "✓ File berhasil dibaca (", round(file_size_mb, 1), "MB)\n",
          "✓ Format file: ", toupper(ext), "\n",
          "✓ Jumlah baris: ", nrow(uploaded_data), "\n",
          "✓ Jumlah kolom: ", ncol(uploaded_data), "\n",
          "✓ Kolom tambahan: ", paste(setdiff(colnames(uploaded_data), required_cols), collapse = ", "), "\n\n",
          "KOMPATIBILITAS:\n",
          "- Daerah cocok: ", length(common_regions), "/", length(upload_regions), 
          " (", round(length(common_regions)/length(upload_regions)*100, 1), "%)\n",
          "- Periode cocok: ", length(common_periods), "/", length(upload_periods),
          " (", round(length(common_periods)/length(upload_periods)*100, 1), "%)\n\n",
          "STATUS: ", 
          ifelse(length(common_regions) > 0 && length(common_periods) > 0,
                 "✓ SIAP DIGABUNG", "✗ TIDAK KOMPATIBEL")
        )
      })
      
      output$uploaded_preview <- DT::renderDataTable({
        DT::datatable(uploaded_data, 
                      options = list(scrollX = TRUE, pageLength = 5),
                      caption = paste("Preview Data Upload -", nrow(uploaded_data), "baris"))
      })
      
    }, error = function(e) {
      output$merge_status <- renderText({
        paste("ERROR membaca file:", e$message)
      })
    })
  })
  
  observeEvent(input$merge_data, {
    req(values$uploaded_data)
    
    tryCatch({
      # Perform merge
      merged_data <- merge(values$initial_data, values$uploaded_data,
                           by = c("Nama_Daerah", "Bulan", "Tahun"),
                           all.x = TRUE, suffixes = c("", "_new"))
      
      values$merged_data <- merged_data
      values$data_merged <- TRUE
      
      # Calculate merge success statistics
      total_main <- nrow(values$initial_data)
      total_merged <- nrow(merged_data)
      new_columns <- setdiff(colnames(merged_data), colnames(values$initial_data))
      
      output$merge_status <- renderText({
        paste(
          "PENGGABUNGAN BERHASIL!\n",
          "======================\n",
          "✓ Data utama: ", total_main, " baris\n",
          "✓ Data gabungan: ", total_merged, " baris\n",
          "✓ Kolom baru ditambahkan: ", length(new_columns), "\n",
          "✓ Nama kolom baru: ", paste(new_columns, collapse = ", "), "\n\n",
          "PERHATIAN: Refresh halaman untuk melihat variabel baru di semua dropdown!\n",
          "Data siap digunakan untuk analisis!"
        )
      })
      
      output$merged_preview <- DT::renderDataTable({
        DT::datatable(values$merged_data, 
                      options = list(scrollX = TRUE, pageLength = 5),
                      caption = paste("Data Setelah Digabung -", nrow(values$merged_data), "baris,", ncol(values$merged_data), "kolom"))
      })
      
    }, error = function(e) {
      output$merge_status <- renderText({
        paste("ERROR dalam penggabungan:", e$message)
      })
    })
  })
  
  # Overview value boxes
  output$total_regions <- renderValueBox({
    valueBox(
      value = length(unique(current_data()$Nama_Daerah)),
      subtitle = "Total Daerah",
      icon = icon("map-marker-alt"),
      color = "green"
    )
  })
  
  output$total_years <- renderValueBox({
    valueBox(
      value = length(unique(current_data()$Tahun)),
      subtitle = "Periode Tahun",
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$total_observations <- renderValueBox({
    valueBox(
      value = nrow(current_data()),
      subtitle = "Total Observasi",
      icon = icon("database"),
      color = "purple"
    )
  })
  
  output$variables_count <- renderValueBox({
    valueBox(
      value = ncol(current_data()) - 3,  # Exclude Nama_Daerah, Bulan, Tahun
      subtitle = "Variabel Iklim",
      icon = icon("thermometer-half"),
      color = "orange"
    )
  })
  
  # Overview plots
  output$overview_climate_plot <- renderPlotly({
    df <- current_data() %>%
      group_by(Tahun) %>%
      summarise(
        Suhu = mean(Suhu, na.rm = TRUE),
        Curah_Hujan = mean(Curah_Hujan, na.rm = TRUE),
        Kelembapan = mean(Kelembapan, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p <- ggplot(df, aes(x = Tahun)) +
      geom_line(aes(y = Suhu, color = "Suhu (°C)"), size = 1.2) +
      geom_line(aes(y = Curah_Hujan, color = "Curah Hujan (mm/hari)"), size = 1.2) +
      geom_line(aes(y = Kelembapan, color = "Kelembapan (%)"), size = 1.2) +
      labs(title = "Tren Variabel Iklim Utama", x = "Tahun", y = "Nilai") +
      theme_minimal() +
      scale_color_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1")) +
      theme(legend.title = element_blank())
    
    ggplotly(p)
  })
  
  output$overview_productivity_plot <- renderPlotly({
    df <- current_data() %>%
      group_by(Tahun) %>%
      summarise(
        Produktivitas = mean(Produktivitas, na.rm = TRUE),
        NDVI = mean(NDVI, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p <- ggplot(df, aes(x = Tahun)) +
      geom_bar(aes(y = Produktivitas), stat = "identity", fill = "#96CEB4", alpha = 0.7) +
      geom_line(aes(y = NDVI * 10, color = "NDVI x10"), size = 1.5) +
      labs(title = "Produktivitas dan NDVI", x = "Tahun", y = "Produktivitas") +
      scale_y_continuous(sec.axis = sec_axis(~./10, name = "NDVI")) +
      theme_minimal() +
      scale_color_manual(values = "#FF6B6B") +
      theme(legend.title = element_blank())
    
    ggplotly(p)
  })
  
  # Descriptive Analysis
  observeEvent(input$run_descriptive, {
    df <- current_data()
    
    # Filter by year
    if (input$desc_year != "All") {
      df <- df[df$Tahun == as.numeric(input$desc_year), ]
    }
    
    # Filter by region
    if (input$desc_region != "All") {
      df <- df[df$Nama_Daerah == input$desc_region, ]
    }
    
    variable_data <- df[[input$desc_variable]]
    
    output$descriptive_stats <- renderText({
      stats <- paste(
        "Statistik Deskriptif untuk", input$desc_variable, "\n",
        "=====================================\n",
        "Filter: Tahun =", ifelse(input$desc_year == "All", "Semua", input$desc_year),
        ", Daerah =", ifelse(input$desc_region == "All", "Semua", input$desc_region), "\n",
        "Jumlah Observasi:", length(variable_data), "\n",
        "Mean:", round(mean(variable_data, na.rm = TRUE), 4), "\n",
        "Median:", round(median(variable_data, na.rm = TRUE), 4), "\n",
        "Std Dev:", round(sd(variable_data, na.rm = TRUE), 4), "\n",
        "Min:", round(min(variable_data, na.rm = TRUE), 4), "\n",
        "Max:", round(max(variable_data, na.rm = TRUE), 4), "\n",
        "Q1:", round(quantile(variable_data, 0.25, na.rm = TRUE), 4), "\n",
        "Q3:", round(quantile(variable_data, 0.75, na.rm = TRUE), 4), "\n",
        "Skewness:", round(moments::skewness(variable_data, na.rm = TRUE), 4), "\n",
        "Kurtosis:", round(moments::kurtosis(variable_data, na.rm = TRUE), 4)
      )
      stats
    })
    
    if (input$normality_test) {
      output$normality_results <- renderText({
        # Shapiro-Wilk test (if sample size <= 5000)
        if (length(variable_data) <= 5000 && length(variable_data) >= 3) {
          shapiro_test <- shapiro.test(variable_data)
          shapiro_result <- paste("Shapiro-Wilk Test: p-value =", round(shapiro_test$p.value, 6))
          shapiro_interp <- ifelse(shapiro_test$p.value < 0.05, "Data TIDAK normal", "Data normal")
        } else {
          shapiro_result <- "Shapiro-Wilk Test: Data terlalu besar/kecil"
          shapiro_interp <- ""
        }
        
        # Anderson-Darling test
        tryCatch({
          ad_test <- ad.test(variable_data)
          ad_result <- paste("Anderson-Darling Test: p-value =", round(ad_test$p.value, 6))
          ad_interp <- ifelse(ad_test$p.value < 0.05, "Data TIDAK normal", "Data normal")
        }, error = function(e) {
          ad_result <- "Anderson-Darling Test: Error"
          ad_interp <- ""
        })
        
        # Kolmogorov-Smirnov test
        tryCatch({
          ks_test <- ks.test(variable_data, "pnorm", mean(variable_data, na.rm = TRUE), sd(variable_data, na.rm = TRUE))
          ks_result <- paste("Kolmogorov-Smirnov Test: p-value =", round(ks_test$p.value, 6))
          ks_interp <- ifelse(ks_test$p.value < 0.05, "Data TIDAK normal", "Data normal")
        }, error = function(e) {
          ks_result <- "KS Test: Error"
          ks_interp <- ""
        })
        
        paste(
          "Uji Normalitas\n",
          "================\n",
          shapiro_result, ifelse(shapiro_interp != "", paste(" -", shapiro_interp), ""), "\n",
          ad_result, ifelse(exists("ad_interp") && ad_interp != "", paste(" -", ad_interp), ""), "\n",
          ks_result, ifelse(exists("ks_interp") && ks_interp != "", paste(" -", ks_interp), ""), "\n\n",
          "Interpretasi: p-value < 0.05 menunjukkan data tidak terdistribusi normal"
        )
      })
    }
    
    output$descriptive_histogram <- renderPlotly({
      p <- ggplot(df, aes_string(x = input$desc_variable)) +
        geom_histogram(bins = 30, fill = "#2E8B57", alpha = 0.7, color = "white") +
        labs(title = paste("Distribusi", input$desc_variable), x = input$desc_variable, y = "Frekuensi") +
        theme_minimal() +
        theme(plot.title = element_text(color = "#1E6B4F", size = 14, face = "bold"))
      
      ggplotly(p)
    })
    
    output$descriptive_boxplot <- renderPlotly({
      p <- ggplot(df, aes_string(x = "1", y = input$desc_variable)) +
        geom_boxplot(fill = "#4ECDC4", alpha = 0.7, color = "#1E6B4F") +
        labs(title = paste("Boxplot", input$desc_variable), 
             y = input$desc_variable, x = "") +
        theme_minimal() +
        theme(plot.title = element_text(color = "#1E6B4F", size = 14, face = "bold"),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
      
      ggplotly(p)
    })
  })
  
  # Correlation Analysis
  observeEvent(input$run_correlation, {
    df <- current_data()
    
    if (input$corr_year != "Semua") {
      df <- df[df$Tahun == input$corr_year, ]
    }
    if (input$corr_region != "Semua") {
      df <- df[df$Nama_Daerah == input$corr_region, ]
    }
    
    # Get numeric columns dynamically
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    numeric_cols <- setdiff(numeric_cols, c("Bulan", "Tahun"))  # Remove non-variable columns
    
    corr_data <- df[numeric_cols]
    corr_matrix <- cor(corr_data, method = "spearman", use = "complete.obs")
    
    values$correlation_result <- corr_matrix
    
    output$correlation_plot <- renderPlot({
      corrplot(corr_matrix, method = "color", type = "upper", order = "hclust",
               tl.col = "black", tl.srt = 45, tl.cex = 0.9,
               col = colorRampPalette(c("#FF6B6B", "white", "#4ECDC4"))(200),
               addCoef.col = "black", number.cex = 0.7)
      
      # Add interpretation
      mtext("Interpretasi: Warna biru = korelasi positif, Warna merah = korelasi negatif\nAngka mendekati ±1 = korelasi kuat, mendekati 0 = korelasi lemah", 
            side = 1, line = 4, cex = 0.8)
    })
    
    output$correlation_table <- DT::renderDataTable({
      corr_df <- as.data.frame(round(corr_matrix, 3))
      corr_df$Variable <- rownames(corr_df)
      corr_df <- corr_df[, c(ncol(corr_df), 1:(ncol(corr_df)-1))]
      
      DT::datatable(corr_df, 
                    options = list(scrollX = TRUE, pageLength = 10),
                    caption = paste("Korelasi Spearman - Filter: Tahun =", 
                                    ifelse(input$corr_year == "Semua", "Semua", input$corr_year),
                                    ", Daerah =", 
                                    ifelse(input$corr_region == "Semua", "Semua", input$corr_region),
                                    "\nInterpretasi: Nilai 0.7-1.0 = korelasi sangat kuat, 0.3-0.7 = kuat, 0.1-0.3 = lemah, <0.1 = sangat lemah"))
    })
  })
  
  # Kruskal Wallis Test
  observeEvent(input$run_kruskal, {
    df <- current_data()
    
    # Ensure we have enough data
    if(nrow(df) < 10) {
      output$kruskal_results <- renderText("Error: Tidak cukup data untuk analisis")
      return()
    }
    
    if (input$kw_grouping == "Tahun") {
      if (input$kw_region_filter != "Semua") {
        df <- df[df$Nama_Daerah == input$kw_region_filter, ]
      }
      grouping_var <- "Tahun"
      df$Tahun <- as.factor(df$Tahun)  # Convert to factor for proper grouping
    } else {
      df <- df[df$Tahun == input$kw_year_filter, ]
      grouping_var <- "Nama_Daerah"
      # Limit to top 10 regions if too many for visualization
      top_regions <- df %>% 
        group_by(Nama_Daerah) %>% 
        summarise(mean_val = mean(.data[[input$kw_variable]], na.rm = TRUE), .groups = 'drop') %>%
        top_n(10, mean_val) %>%
        pull(Nama_Daerah)
      df <- df[df$Nama_Daerah %in% top_regions, ]
    }
    
    # Check if we still have enough groups and data
    n_groups <- length(unique(df[[grouping_var]]))
    if(n_groups < 2) {
      output$kruskal_results <- renderText("Error: Perlu minimal 2 kelompok untuk analisis")
      return()
    }
    
    # Perform Kruskal-Wallis test
    tryCatch({
      formula_str <- paste(input$kw_variable, "~", grouping_var)
      kw_result <- kruskal.test(as.formula(formula_str), data = df)
      
      # Calculate group medians for interpretation
      group_medians <- df %>%
        group_by(.data[[grouping_var]]) %>%
        summarise(
          n = n(),
          median = median(.data[[input$kw_variable]], na.rm = TRUE),
          mean = mean(.data[[input$kw_variable]], na.rm = TRUE),
          .groups = 'drop'
        )
      
      output$kruskal_results <- renderText({
        interpretation <- if(kw_result$p.value < 0.001) {
          "Ada perbedaan SANGAT signifikan antar kelompok (p < 0.001)"
        } else if(kw_result$p.value < 0.01) {
          "Ada perbedaan SANGAT signifikan antar kelompok (p < 0.01)"
        } else if(kw_result$p.value < 0.05) {
          "Ada perbedaan signifikan antar kelompok (p < 0.05)"
        } else {
          "TIDAK ada perbedaan signifikan antar kelompok (p ≥ 0.05)"
        }
        
        group_summary <- paste(group_medians[[grouping_var]], ": median =", 
                               round(group_medians$median, 3), collapse = "\n")
        
        paste(
          "Uji Kruskal-Wallis\n",
          "==================\n",
          "Variabel:", input$kw_variable, "\n",
          "Kelompok:", grouping_var, "\n",
          "Jumlah kelompok:", n_groups, "\n",
          "Total observasi:", nrow(df), "\n\n",
          "Hasil Statistik:\n",
          "Chi-squared =", round(kw_result$statistic, 4), "\n",
          "df =", kw_result$parameter, "\n",
          "p-value =", format(kw_result$p.value, scientific = TRUE), "\n\n",
          "INTERPRETASI:\n", interpretation, "\n\n",
          "Median per Kelompok:\n", group_summary, "\n\n",
          "Kesimpulan:\n",
          if(kw_result$p.value < 0.05) {
            paste("Terdapat perbedaan yang signifikan dalam", input$kw_variable, 
                  "antar", tolower(grouping_var), ". Perlu analisis post-hoc untuk",
                  "mengetahui kelompok mana yang berbeda.")
          } else {
            paste("Tidak terdapat perbedaan yang signifikan dalam", input$kw_variable, 
                  "antar", tolower(grouping_var), ". Semua kelompok memiliki",
                  "distribusi yang relatif sama.")
          }
        )
      })
      
      output$kruskal_plot <- renderPlotly({
        p <- ggplot(df, aes_string(x = grouping_var, y = input$kw_variable, fill = grouping_var)) +
          geom_boxplot(alpha = 0.7, outlier.shape = NA) +
          geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
          stat_summary(fun = median, geom = "point", shape = 23, size = 3, 
                       fill = "white", color = "black") +
          labs(title = paste("Distribusi", input$kw_variable, "berdasarkan", grouping_var),
               subtitle = paste("p-value =", format(kw_result$p.value, digits = 4)),
               x = grouping_var, y = input$kw_variable) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "none",
                plot.title = element_text(size = 14, face = "bold"),
                plot.subtitle = element_text(size = 12, 
                                             color = ifelse(kw_result$p.value < 0.05, "red", "blue"))) +
          scale_fill_brewer(palette = "Set3")
        
        ggplotly(p, tooltip = c("x", "y"))
      })
      
    }, error = function(e) {
      output$kruskal_results <- renderText({
        paste("Error dalam analisis Kruskal-Wallis:", e$message, 
              "\nPastikan data memiliki variabilitas yang cukup dan kelompok yang valid.")
      })
    })
  })
  
  # Robust Regression
  observeEvent(input$run_regression, {
    df <- current_data()
    
    if (input$reg_year != "Semua") {
      df <- df[df$Tahun == input$reg_year, ]
    }
    if (input$reg_region != "Semua") {
      df <- df[df$Nama_Daerah == input$reg_region, ]
    }
    
    # Prepare formula
    formula_str <- paste(input$reg_dependent, "~", paste(input$reg_independent, collapse = " + "))
    
    # Fit robust regression model
    tryCatch({
      robust_model <- rlm(as.formula(formula_str), data = df, method = "M")
      values$regression_result <- robust_model
      
      output$regression_results <- renderText({
        model_summary <- summary(robust_model)
        
        coef_table <- data.frame(
          Variable = rownames(model_summary$coefficients),
          Estimate = round(model_summary$coefficients[,1], 4),
          Std_Error = round(model_summary$coefficients[,2], 4),
          t_value = round(model_summary$coefficients[,3], 4)
        )
        
        result_text <- paste(
          "Robust Regression Results\n",
          "=========================\n",
          "Model:", formula_str, "\n",
          "Sample size:", nrow(df), "\n",
          "Residual scale:", round(robust_model$s, 4), "\n\n",
          "Coefficients:\n"
        )
        
        for(i in 1:nrow(coef_table)) {
          result_text <- paste(result_text,
                               sprintf("%-20s: %8.4f (SE: %6.4f, t: %6.2f)\n", 
                                       coef_table$Variable[i], 
                                       coef_table$Estimate[i],
                                       coef_table$Std_Error[i],
                                       coef_table$t_value[i]))
        }
        
        result_text
      })
      
      # Residual plot
      output$regression_residual_plot <- renderPlotly({
        residuals_df <- data.frame(
          Fitted = fitted(robust_model),
          Residuals = residuals(robust_model)
        )
        
        p <- ggplot(residuals_df, aes(x = Fitted, y = Residuals)) +
          geom_point(alpha = 0.6, color = "#2E8B57") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
          geom_smooth(method = "loess", se = FALSE, color = "#FF6B6B") +
          labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
          theme_minimal()
        
        ggplotly(p)
      })
      
      # Prediction vs Actual plot
      output$regression_prediction_plot <- renderPlotly({
        pred_df <- data.frame(
          Actual = df[[input$reg_dependent]],
          Predicted = fitted(robust_model)
        )
        
        p <- ggplot(pred_df, aes(x = Actual, y = Predicted)) +
          geom_point(alpha = 0.6, color = "#2E8B57") +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
          geom_smooth(method = "lm", se = TRUE, color = "#4ECDC4") +
          labs(title = "Predicted vs Actual", x = "Actual Values", y = "Predicted Values") +
          theme_minimal()
        
        ggplotly(p)
      })
      
    }, error = function(e) {
      output$regression_results <- renderText({
        paste("Error in regression analysis:", e$message)
      })
    })
  })
  
  # Time Series Analysis
  observeEvent(input$run_timeseries, {
    df <- current_data()[current_data()$Nama_Daerah == input$ts_region, ]
    df <- df[order(df$Tahun, df$Bulan), ]
    
    # Check if we have enough data points
    if(nrow(df) < 12) {
      output$timeseries_plot <- renderPlotly({
        plot_ly() %>% 
          add_annotations(text = "Tidak cukup data untuk analisis time series (minimal 12 bulan)",
                          x = 0.5, y = 0.5, xref = "paper", yref = "paper")
      })
      return()
    }
    
    ts_data <- ts(df[[input$ts_variable]], start = c(min(df$Tahun), 1), frequency = 12)
    
    output$timeseries_plot <- renderPlotly({
      ts_df <- data.frame(
        Date = seq(as.Date(paste(min(df$Tahun), "-01-01", sep="")), by = "month", length.out = length(ts_data)),
        Value = as.numeric(ts_data)
      )
      
      p <- ggplot(ts_df, aes(x = Date, y = Value)) +
        geom_line(color = "#2E8B57", size = 1.2) +
        geom_point(color = "#FF6B6B", size = 1.5, alpha = 0.7) +
        labs(title = paste("Time Series:", input$ts_variable, "-", input$ts_region),
             x = "Tanggal", y = input$ts_variable) +
        theme_minimal() +
        theme(plot.title = element_text(color = "#1E6B4F", size = 14, face = "bold"))
      
      if (input$ts_trend) {
        p <- p + geom_smooth(method = "loess", color = "#4ECDC4", se = TRUE, alpha = 0.3)
      }
      
      ggplotly(p)
    })
    
    # Stationarity tests with detailed interpretation
    output$timeseries_tests <- renderText({
      tryCatch({
        # Augmented Dickey-Fuller test
        adf_test <- adf.test(ts_data)
        adf_result <- paste("ADF Test: p-value =", round(adf_test$p.value, 6))
        adf_interp <- ifelse(adf_test$p.value < 0.05, "STASIONER", "NON-STASIONER")
        
        # KPSS test
        kpss_test <- kpss.test(ts_data)
        kpss_result <- paste("KPSS Test: p-value =", round(kpss_test$p.value, 6))
        kpss_interp <- ifelse(kpss_test$p.value > 0.05, "STASIONER", "NON-STASIONER")
        
        # Overall conclusion
        overall_conclusion <- if(adf_interp == "STASIONER" && kpss_interp == "STASIONER") {
          "Data STASIONER - Dapat langsung dimodelkan"
        } else if(adf_interp == "NON-STASIONER" && kpss_interp == "NON-STASIONER") {
          "Data NON-STASIONER - Perlu differencing"
        } else {
          "Hasil uji berbeda - Perlu analisis lebih lanjut"
        }
        
        paste(
          adf_result, " → ", adf_interp, "\n",
          kpss_result, " → ", kpss_interp, "\n\n",
          "KESIMPULAN: ", overall_conclusion
        )
      }, error = function(e) {
        paste("Error dalam uji stasioneritas:", e$message)
      })
    })
    
    # ARIMA model with detailed interpretation
    output$timeseries_model <- renderText({
      tryCatch({
        auto_model <- auto.arima(ts_data, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
        
        # Extract model components
        p <- auto_model$arma[1]  # AR
        d <- auto_model$arma[6]  # I
        q <- auto_model$arma[2]  # MA
        P <- auto_model$arma[3]  # Seasonal AR
        D <- auto_model$arma[7]  # Seasonal I
        Q <- auto_model$arma[4]  # Seasonal MA
        
        seasonal_part <- if(P + D + Q > 0) {
          paste("(", P, ",", D, ",", Q, ")12", sep="")
        } else {
          ""
        }
        
        paste(
          "ARIMA(", p, ",", d, ",", q, ")", seasonal_part, "\n",
          "AIC = ", round(auto_model$aic, 2), "\n",
          "BIC = ", round(auto_model$bic, 2), "\n",
          "Sigma² = ", round(auto_model$sigma2, 6)
        )
      }, error = function(e) {
        paste("Error dalam modeling ARIMA:", e$message)
      })
    })
    
    # Model interpretation
    output$timeseries_interpretation <- renderText({
      tryCatch({
        auto_model <- auto.arima(ts_data, seasonal = TRUE, stepwise = TRUE, approximation = FALSE)
        
        # Analyze residuals
        residuals <- residuals(auto_model)
        ljung_test <- Box.test(residuals, type = "Ljung-Box")
        
        # Model adequacy
        model_adequacy <- ifelse(ljung_test$p.value > 0.05, 
                                 "Model ADEQUATE (residual white noise)",
                                 "Model perlu PERBAIKAN (residual ada pola)")
        
        # Trend analysis
        if(auto_model$arma[6] > 0) {  # d > 0
          trend_interpretation <- "Data memiliki TREND - telah di-differencing"
        } else {
          trend_interpretation <- "Data TIDAK memiliki trend"
        }
        
        # Seasonality analysis
        if(auto_model$arma[7] > 0 || auto_model$arma[3] > 0 || auto_model$arma[4] > 0) {
          seasonal_interpretation <- "Data memiliki pola MUSIMAN"
        } else {
          seasonal_interpretation <- "Data TIDAK memiliki pola musiman"
        }
        
        paste(
          "INTERPRETASI MODEL:\n",
          "==================\n",
          trend_interpretation, "\n",
          seasonal_interpretation, "\n",
          "Adequacy: ", model_adequacy, "\n",
          "Ljung-Box p-value: ", round(ljung_test$p.value, 4), "\n\n",
          "REKOMENDASI:\n",
          if(ljung_test$p.value > 0.05) {
            "Model sudah baik untuk analisis temporal. Residual bersifat random."
          } else {
            "Model perlu diperbaiki. Pertimbangkan transformasi data atau model lain."
          }
        )
      }, error = function(e) {
        paste("Error dalam interpretasi model:", e$message)
      })
    })
  })
  
  # Prediction & Simulation
  observeEvent(input$run_prediction, {
    df <- current_data()
    
    # Prepare input data for prediction
    new_data <- data.frame(
      Curah_Hujan = input$pred_rainfall,
      Suhu = input$pred_suhu,
      Kelembapan = input$pred_kelembapan,
      Radiasi_Matahari = input$pred_radiasi,
      NDVI = input$pred_ndvi,
      CO = input$pred_co,
      Soil_Moisture = input$pred_soil
    )
    
    # Build prediction model based on target variable
    if(input$pred_target == "Produktivitas") {
      predictors <- c("Curah_Hujan", "Suhu", "Kelembapan", 
                      "Radiasi_Matahari", "NDVI", "CO", "Soil_Moisture")
    } else if(input$pred_target == "NDVI") {
      predictors <- c("Curah_Hujan", "Suhu", "Kelembapan", 
                      "Radiasi_Matahari", "CO", "Soil_Moisture")
    } else {
      predictors <- c("Curah_Hujan", "Kelembapan", 
                      "Radiasi_Matahari", "NDVI", "CO", "Soil_Moisture")
    }
    
    # Remove target from predictors if it exists
    predictors <- predictors[predictors != input$pred_target]
    
    tryCatch({
      # Build robust regression model
      formula_str <- paste(input$pred_target, "~", paste(predictors, collapse = " + "))
      pred_model <- rlm(as.formula(formula_str), data = df, method = "M")
      
      # Make prediction
      prediction <- predict(pred_model, newdata = new_data[predictors])
      
      # Calculate prediction interval (more robust approach)
      model_residuals <- residuals(pred_model)
      residual_se <- mad(model_residuals)  # Median Absolute Deviation for robust SE
      conf_interval <- c(prediction - 1.96 * residual_se, prediction + 1.96 * residual_se)
      
      # Calculate model performance metrics
      fitted_values <- fitted(pred_model)
      actual_values <- df[[input$pred_target]]
      r_squared <- cor(fitted_values, actual_values)^2
      mae <- mean(abs(fitted_values - actual_values), na.rm = TRUE)
      
      output$prediction_model_info <- renderText({
        paste(
          "Model: Robust Regression (M-estimator)\n",
          "Formula:", formula_str, "\n",
          "Training samples:", nrow(df), "\n",
          "Model Performance:\n",
          "- R-squared:", round(r_squared, 4), "\n",
          "- MAE:", round(mae, 4), "\n",
          "- Robust SE:", round(residual_se, 4)
        )
      })
      
      # Determine prediction quality
      prediction_quality <- if(r_squared > 0.7) {
        "BAIK (R² > 0.7)"
      } else if(r_squared > 0.5) {
        "SEDANG (R² 0.5-0.7)"
      } else {
        "KURANG (R² < 0.5)"
      }
      
      output$prediction_result <- renderText({
        paste(
          "HASIL PREDIKSI ", input$pred_target, ":\n",
          "================================\n",
          "Nilai prediksi: ", round(prediction, 4), "\n",
          "Kualitas model: ", prediction_quality, "\n\n",
          "INPUT VARIABEL:\n",
          "- Curah Hujan: ", input$pred_rainfall, " mm/hari\n",
          "- Suhu: ", input$pred_suhu, " °C\n",
          "- Kelembapan: ", input$pred_kelembapan, " %\n",
          "- Radiasi Matahari: ", input$pred_radiasi, " x10⁴ j/m²\n",
          "- NDVI: ", input$pred_ndvi, "\n",
          "- CO: ", input$pred_co, " mol/m²\n",
          "- Soil Moisture: ", input$pred_soil
        )
      })
      
      output$prediction_interval <- renderText({
        interval_width <- diff(conf_interval)
        interval_percentage <- (interval_width / abs(prediction)) * 100
        
        paste(
          "95% CONFIDENCE INTERVAL:\n",
          "========================\n",
          "Lower bound: ", round(conf_interval[1], 4), "\n",
          "Upper bound: ", round(conf_interval[2], 4), "\n",
          "Interval width: ", round(interval_width, 4), "\n",
          "Uncertainty: ±", round(interval_percentage, 1), "%\n\n",
          "INTERPRETASI:\n",
          if(interval_percentage < 10) {
            "Prediksi sangat akurat (uncertainty < 10%)"
          } else if(interval_percentage < 20) {
            "Prediksi cukup akurat (uncertainty 10-20%)"
          } else {
            "Prediksi kurang akurat (uncertainty > 20%)"
          }
        )
      })
      
      # Comparison plot with historical data
      output$prediction_comparison_plot <- renderPlotly({
        tryCatch({
          # Get historical data statistics
          hist_mean <- mean(df[[input$pred_target]], na.rm = TRUE)
          hist_sd <- sd(df[[input$pred_target]], na.rm = TRUE)
          hist_min <- min(df[[input$pred_target]], na.rm = TRUE)
          hist_max <- max(df[[input$pred_target]], na.rm = TRUE)
          
          # Create comparison data
          comparison_data <- data.frame(
            Kategori = c("Historis Min", "Historis Mean", "Historis Max", "Prediksi"),
            Nilai = c(hist_min, hist_mean, hist_max, prediction),
            Type = c("Historical", "Historical", "Historical", "Prediction"),
            Color = c("#4ECDC4", "#2E8B57", "#4ECDC4", "#FF6B6B")
          )
          
          p <- ggplot(comparison_data, aes(x = Kategori, y = Nilai, fill = Color)) +
            geom_col(alpha = 0.8, width = 0.6) +
            geom_text(aes(label = round(Nilai, 3)), vjust = -0.5, size = 4, fontface = "bold") +
            labs(title = paste("Perbandingan Prediksi vs Data Historis -", input$pred_target),
                 subtitle = paste("Prediksi berada di",
                                  ifelse(prediction < hist_mean, "bawah", "atas"), "rata-rata historis"),
                 x = "Kategori", y = input$pred_target) +
            theme_minimal() +
            theme(legend.position = "none",
                  plot.title = element_text(size = 14, face = "bold"),
                  axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_fill_identity()
          
          ggplotly(p, tooltip = c("x", "y"))
        }, error = function(e) {
          plot_ly() %>% 
            add_annotations(text = paste("Error in comparison plot:", e$message),
                            x = 0.5, y = 0.5, xref = "paper", yref = "paper")
        })
      })
      
      # Sensitivity analysis
      output$sensitivity_plot <- renderPlotly({
        tryCatch({
          # Choose the most important predictor based on model coefficients
          model_coefs <- abs(coef(pred_model)[-1])  # Exclude intercept
          vary_var <- names(model_coefs)[which.max(model_coefs)]
          
          if(vary_var %in% predictors) {
            sensitivity_data <- data.frame()
            base_value <- new_data[[vary_var]]
            var_range <- seq(from = base_value * 0.7, 
                             to = base_value * 1.3, 
                             length.out = 20)
            
            for(val in var_range) {
              temp_data <- new_data[predictors]
              temp_data[[vary_var]] <- val
              pred_val <- predict(pred_model, newdata = temp_data)
              sensitivity_data <- rbind(sensitivity_data, 
                                        data.frame(Variable_Value = val, 
                                                   Prediction = pred_val,
                                                   Change_Pct = ((val - base_value) / base_value) * 100))
            }
            
            p <- ggplot(sensitivity_data, aes(x = Change_Pct, y = Prediction)) +
              geom_line(color = "#2E8B57", size = 1.5) +
              geom_point(color = "#FF6B6B", size = 2) +
              geom_vline(xintercept = 0, linetype = "dashed", color = "blue", alpha = 0.7) +
              geom_hline(yintercept = prediction, linetype = "dashed", color = "red", alpha = 0.7) +
              labs(title = paste("Analisis Sensitivitas:", vary_var, "vs", input$pred_target),
                   subtitle = paste("Perubahan", vary_var, "terhadap prediksi", input$pred_target),
                   x = paste("Perubahan", vary_var, "(%)"), 
                   y = paste("Prediksi", input$pred_target)) +
              theme_minimal() +
              theme(plot.title = element_text(size = 12, face = "bold"))
            
            ggplotly(p, tooltip = c("x", "y"))
          } else {
            plot_ly() %>% 
              add_annotations(text = "Sensitivity analysis tidak tersedia untuk kombinasi ini",
                              x = 0.5, y = 0.5, xref = "paper", yref = "paper")
          }
        }, error = function(e) {
          plot_ly() %>% 
            add_annotations(text = paste("Error in sensitivity analysis:", e$message),
                            x = 0.5, y = 0.5, xref = "paper", yref = "paper")
        })
      })
      
    }, error = function(e) {
      output$prediction_result <- renderText({
        paste("Error dalam prediksi:", e$message, 
              "\nPastikan semua variabel prediktor tersedia dan model dapat dibangun.")
      })
    })
  })
  
  # Advanced Visualizations
  observeEvent(input$update_viz, {
    df <- current_data()
    
    if(input$viz_type == "heatmap") {
      output$advanced_plot <- renderPlotly({
        # Get numeric columns dynamically
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        numeric_cols <- setdiff(numeric_cols, c("Bulan", "Tahun"))
        
        corr_matrix <- cor(df[numeric_cols], use = "complete.obs")
        
        plot_ly(z = corr_matrix, type = "heatmap", 
                x = colnames(corr_matrix), y = rownames(corr_matrix),
                colorscale = list(c(0, "#FF6B6B"), c(0.5, "white"), c(1, "#4ECDC4")),
                hovertemplate = "%{x} vs %{y}<br>Korelasi: %{z:.3f}<extra></extra>") %>%
          layout(title = "Heatmap Korelasi Antar Variabel Iklim",
                 annotations = list(
                   list(x = 0.5, y = -0.15, xref = "paper", yref = "paper",
                        text = "Interpretasi: Biru = korelasi positif kuat, Merah = korelasi negatif kuat, Putih = tidak berkorelasi",
                        showarrow = FALSE, font = list(size = 12))
                 ))
      })
      
    } else if(input$viz_type == "scatter_matrix") {
      output$advanced_plot <- renderPlotly({
        # Select key variables for scatter matrix
        available_vars <- names(df)[sapply(df, is.numeric)]
        available_vars <- setdiff(available_vars, c("Bulan", "Tahun"))
        key_vars <- intersect(c("Produktivitas", "Curah_Hujan", "Suhu", "NDVI"), available_vars)
        
        if(length(key_vars) >= 2) {
          scatter_data <- df[key_vars]
          
          # Create scatter plot matrix using plotly
          dimensions <- lapply(key_vars, function(var) {
            list(label = var, values = scatter_data[[var]])
          })
          
          fig <- plot_ly(type = "splom",
                         dimensions = dimensions,
                         data = scatter_data,
                         marker = list(color = "#2E8B57", size = 4, opacity = 0.6))
          
          fig %>% layout(
            title = "Scatter Plot Matrix - Hubungan Antar Variabel Utama",
            annotations = list(
              list(x = 0.5, y = -0.1, xref = "paper", yref = "paper",
                   text = "Interpretasi: Diagonal menunjukkan distribusi, off-diagonal menunjukkan hubungan antar variabel",
                   showarrow = FALSE, font = list(size = 12))
            )
          )
        } else {
          plot_ly() %>% 
            add_annotations(text = "Tidak cukup variabel untuk scatter matrix",
                            x = 0.5, y = 0.5, xref = "paper", yref = "paper")
        }
      })
      
    } else if(input$viz_type == "distribution") {
      output$advanced_plot <- renderPlotly({
        p <- ggplot(df, aes_string(x = input$dist_variable)) +
          geom_histogram(aes(y = ..density..), bins = 30, fill = "#4ECDC4", alpha = 0.7, color = "white") +
          geom_density(color = "#2E8B57", size = 1.5) +
          facet_wrap(~Tahun, scales = "free_y") +
          labs(title = paste("Distribusi", input$dist_variable, "per Tahun"),
               subtitle = "Garis menunjukkan kurva kepadatan, histogram menunjukkan frekuensi",
               x = input$dist_variable, y = "Density") +
          theme_minimal() +
          theme(plot.title = element_text(size = 14, face = "bold"))
        
        ggplotly(p) %>%
          layout(annotations = list(
            list(x = 0.5, y = -0.15, xref = "paper", yref = "paper",
                 text = "Interpretasi: Perhatikan perubahan bentuk distribusi antar tahun - indikasi perubahan iklim",
                 showarrow = FALSE, font = list(size = 12))
          ))
      })
      
    } else if(input$viz_type == "radar") {
      output$advanced_plot <- renderPlotly({
        # Create radar chart data for selected region
        available_vars <- names(df)[sapply(df, is.numeric)]
        radar_vars <- intersect(c("Produktivitas", "Curah_Hujan", "Suhu", "Kelembapan", "NDVI"), available_vars)
        
        if(length(radar_vars) >= 3) {
          radar_data <- df %>%
            filter(Nama_Daerah == input$radar_region) %>%
            summarise_at(radar_vars, mean, na.rm = TRUE)
          
          # Normalize to 0-100 scale for radar chart
          radar_normalized <- data.frame(
            Variable = radar_vars,
            Value = sapply(radar_vars, function(var) {
              (radar_data[[var]] / max(df[[var]], na.rm = TRUE)) * 100
            })
          )
          
          # Create radar chart
          fig <- plot_ly(
            type = 'scatterpolar',
            r = c(radar_normalized$Value, radar_normalized$Value[1]),  # Close the shape
            theta = c(radar_normalized$Variable, radar_normalized$Variable[1]),
            fill = 'toself',
            fillcolor = 'rgba(46, 139, 87, 0.4)',
            line = list(color = '#2E8B57', width = 3),
            marker = list(color = '#FF6B6B', size = 8)
          ) %>%
            layout(
              polar = list(
                radialaxis = list(
                  visible = TRUE,
                  range = c(0, 100),
                  tickvals = c(0, 25, 50, 75, 100),
                  ticktext = c("0%", "25%", "50%", "75%", "100%")
                )
              ),
              title = paste("Climate Profile Radar Chart -", input$radar_region),
              annotations = list(
                list(x = 0.5, y = -0.1, xref = "paper", yref = "paper",
                     text = "Interpretasi: Semakin luar = nilai relatif semakin tinggi terhadap maksimum regional",
                     showarrow = FALSE, font = list(size = 12))
              )
            )
          
          fig
        } else {
          plot_ly() %>% 
            add_annotations(text = "Tidak cukup variabel untuk radar chart",
                            x = 0.5, y = 0.5, xref = "paper", yref = "paper")
        }
      })
    }
    
    # Additional plots with interpretations
    output$additional_plot1 <- renderPlotly({
      # Enhanced boxplot by year with trend
      yearly_summary <- df %>%
        group_by(Tahun) %>%
        summarise(
          Mean_Prod = mean(Produktivitas, na.rm = TRUE),
          SD_Prod = sd(Produktivitas, na.rm = TRUE),
          .groups = 'drop'
        )
      
      p1 <- ggplot(df, aes(x = as.factor(Tahun), y = Produktivitas)) +
        geom_boxplot(aes(fill = as.factor(Tahun)), alpha = 0.7, outlier.alpha = 0.5) +
        geom_point(data = yearly_summary, aes(x = as.factor(Tahun), y = Mean_Prod), 
                   color = "red", size = 3, shape = 18) +
        geom_line(data = yearly_summary, aes(x = as.numeric(as.factor(Tahun)), y = Mean_Prod), 
                  color = "red", size = 1, alpha = 0.8, group = 1) +
        labs(title = "Produktivitas per Tahun dengan Tren", 
             subtitle = "Diamond merah = rata-rata tahunan, Garis merah = tren",
             x = "Tahun", y = "Produktivitas") +
        theme_minimal() +
        theme(legend.position = "none", plot.title = element_text(size = 12, face = "bold")) +
        scale_fill_brewer(palette = "Spectral")
      
      ggplotly(p1, tooltip = c("x", "y"))
    })
    
    output$additional_plot2 <- renderPlotly({
      # Enhanced scatter plot with regression and confidence interval
      model_simple <- lm(Produktivitas ~ Suhu, data = df)
      r_squared <- summary(model_simple)$r.squared
      
      p2 <- ggplot(df, aes(x = Suhu, y = Produktivitas)) +
        geom_point(aes(color = as.factor(Tahun)), alpha = 0.6, size = 2) +
        geom_smooth(method = "lm", se = TRUE, color = "#2E8B57", fill = "#4ECDC4", alpha = 0.3) +
        labs(title = paste("Hubungan Suhu vs Produktivitas (R² =", round(r_squared, 3), ")"),
             subtitle = ifelse(r_squared > 0.5, "Korelasi KUAT", 
                               ifelse(r_squared > 0.3, "Korelasi SEDANG", "Korelasi LEMAH")),
             x = "Suhu (°C)", y = "Produktivitas", color = "Tahun") +
        theme_minimal() +
        theme(plot.title = element_text(size = 12, face = "bold")) +
        scale_color_brewer(palette = "Set1")
      
      ggplotly(p2, tooltip = c("x", "y", "colour"))
    })
  })
  
  # Spatial Visualization
  observeEvent(input$update_map, {
    output$interactive_map <- renderLeaflet({
      df <- current_data()
      df <- df[df$Tahun == input$map_year & df$Bulan == input$map_month, ]
      
      # Check if data exists for selected period
      if(nrow(df) == 0) {
        return(leaflet() %>%
                 addProviderTiles(providers$CartoDB.Positron) %>%
                 setView(lng = 112.5, lat = -7.8, zoom = 8) %>%
                 addControl(
                   html = "<div style='background: red; color: white; padding: 10px; border-radius: 5px;'>
                    <b>Tidak ada data untuk periode yang dipilih</b><br>
                    Silakan pilih tahun dan bulan yang berbeda
                  </div>",
                   position = "topleft"
                 ))
      }
      
      # Aggregate data by region with statistics
      if (input$map_aggregation == "mean") {
        agg_data <- df %>% 
          group_by(Nama_Daerah) %>% 
          summarise(
            Value = mean(.data[[input$map_variable]], na.rm = TRUE),
            Count = n(),
            Min = min(.data[[input$map_variable]], na.rm = TRUE),
            Max = max(.data[[input$map_variable]], na.rm = TRUE),
            SD = sd(.data[[input$map_variable]], na.rm = TRUE),
            .groups = 'drop'
          )
        stat_type <- "Rata-rata"
      } else if (input$map_aggregation == "median") {
        agg_data <- df %>% 
          group_by(Nama_Daerah) %>% 
          summarise(
            Value = median(.data[[input$map_variable]], na.rm = TRUE),
            Count = n(),
            Min = min(.data[[input$map_variable]], na.rm = TRUE),
            Max = max(.data[[input$map_variable]], na.rm = TRUE),
            SD = sd(.data[[input$map_variable]], na.rm = TRUE),
            .groups = 'drop'
          )
        stat_type <- "Median"
      } else {
        agg_data <- df %>% 
          group_by(Nama_Daerah) %>% 
          summarise(
            Value = max(.data[[input$map_variable]], na.rm = TRUE),
            Count = n(),
            Min = min(.data[[input$map_variable]], na.rm = TRUE),
            Max = max(.data[[input$map_variable]], na.rm = TRUE),
            SD = sd(.data[[input$map_variable]], na.rm = TRUE),
            .groups = 'drop'
          )
        stat_type <- "Maksimum"
      }
      
      # Coordinates for East Java regions
      coordinates <- data.frame(
        Nama_Daerah = c(
          # Cities
          "Kota Batu", "Kota Blitar", "Kota Kediri", "Kota Madiun", 
          "Kota Malang", "Kota Mojokerto", "Kota Pasuruan", "Kota Probolinggo", 
          "Kota Surabaya",
          
          # Regencies
          "Kabupaten Bangkalan", "Kabupaten Banyuwangi", "Kabupaten Blitar", 
          "Kabupaten Bojonegoro", "Kabupaten Bondowoso", "Kabupaten Gresik",
          "Kabupaten Jember", "Kabupaten Jombang", "Kabupaten Kediri", 
          "Kabupaten Lamongan", "Kabupaten Lumajang", "Kabupaten Madiun", 
          "Kabupaten Magetan", "Kabupaten Malang", "Kabupaten Mojokerto", 
          "Kabupaten Nganjuk", "Kabupaten Ngawi", "Kabupaten Pacitan", 
          "Kabupaten Pamekasan", "Kabupaten Pasuruan", "Kabupaten Ponorogo", 
          "Kabupaten Probolinggo", "Kabupaten Sampang", "Kabupaten Sidoarjo", 
          "Kabupaten Situbondo", "Kabupaten Sumenep", "Kabupaten Trenggalek", 
          "Kabupaten Tuban", "Kabupaten Tulungagung"
        ),
        lat = c(
          # Cities (latitude)
          -7.8740, -8.0983, -7.8481, -7.6298, 
          -7.9826, -7.4664, -7.6458, -7.7543, 
          -7.2575,
          
          # Regencies (latitude)
          -7.0378, -8.2325, -8.0983, 
          -7.1503, -7.9138, -7.1564,
          -8.1716, -7.5460, -7.8481, 
          -7.1194, -8.1335, -7.6298, 
          -7.6431, -8.0614, -7.4664, 
          -7.6052, -7.4039, -8.1996, 
          -7.0686, -7.6393, -7.8714, 
          -7.7543, -7.1869, -7.4378, 
          -7.7122, -7.0176, -8.0614, 
          -6.8997, -8.0614
        ),
        lng = c(
          # Cities (longitude)
          112.5348, 112.1681, 112.0178, 111.5239, 
          112.6326, 112.4339, 112.9078, 113.2159, 
          112.7521,
          
          # Regencies (longitude)
          112.7378, 114.3691, 112.1681, 
          111.8814, 113.8213, 112.6544,
          113.7166, 112.2622, 112.0178, 
          112.4180, 113.2159, 111.5239, 
          111.3504, 112.6326, 112.4339, 
          111.9044, 111.4462, 111.0946, 
          113.4750, 112.9078, 111.4616, 
          113.2159, 112.9078, 112.7177, 
          113.9774, 113.8691, 111.7210, 
          111.8937, 111.9044
        ),
        stringsAsFactors = FALSE
      )
      
      # Merge data with coordinates
      map_data <- inner_join(agg_data, coordinates, by = "Nama_Daerah")
      
      # Color palette based on data range
      pal <- colorNumeric(
        palette = c("#FFE5B4", "#FFCC99", "#FF9999", "#FF6666", "#FF3333", "#CC0000"),
        domain = map_data$Value,
        na.color = "#808080"
      )
      
      # Calculate overall statistics for interpretation
      overall_mean <- mean(map_data$Value, na.rm = TRUE)
      overall_max <- max(map_data$Value, na.rm = TRUE)
      overall_min <- min(map_data$Value, na.rm = TRUE)
      
      # Categorize regions
      high_regions <- sum(map_data$Value > (overall_mean + sd(map_data$Value, na.rm = TRUE)), na.rm = TRUE)
      low_regions <- sum(map_data$Value < (overall_mean - sd(map_data$Value, na.rm = TRUE)), na.rm = TRUE)
      
      leaflet(map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 112.5, lat = -7.8, zoom = 8) %>%
        addCircleMarkers(
          lng = ~lng, lat = ~lat,
          radius = ~sqrt(pmax(Value, 0)) * 5 + 5,  # Ensure positive radius
          color = "#FFFFFF",
          fillColor = ~pal(Value),
          fillOpacity = 0.8,
          weight = 2,
          popup = ~paste(
            "<div style='font-size: 14px;'>",
            "<b>", Nama_Daerah, "</b><br>",
            "<hr style='margin: 5px 0;'>",
            "<b>", input$map_variable, " (", stat_type, "):</b> ", round(Value, 3), "<br>",
            "<b>Range:</b> ", round(Min, 3), " - ", round(Max, 3), "<br>",
            "<b>Std Dev:</b> ", round(SD, 3), "<br>",
            "<b>Jumlah Data:</b> ", Count, "<br>",
            "<hr style='margin: 5px 0;'>",
            "<i>Ranking: ", 
            ifelse(Value > overall_mean + sd(map_data$Value, na.rm = TRUE), "Tinggi",
                   ifelse(Value < overall_mean - sd(map_data$Value, na.rm = TRUE), "Rendah", "Sedang")),
            "</i>",
            "</div>"
          ),
          label = ~paste(Nama_Daerah, ":", round(Value, 3))
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = ~Value,
          title = paste(input$map_variable, "<br>", stat_type),
          opacity = 1
        ) %>%
        addControl(
          html = paste(
            "<div style='background: white; padding: 10px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.3); font-size: 12px;'>",
            "<b>Statistik Regional ", input$map_variable, "</b><br>",
            "<b>Periode:</b> ", month.name[input$map_month], " ", input$map_year, "<br>",
            "<b>Total Daerah:</b> ", nrow(map_data), "<br>",
            "<b>Rata-rata:</b> ", round(overall_mean, 3), "<br>",
            "<b>Range:</b> ", round(overall_min, 3), " - ", round(overall_max, 3), "<br>",
            "<b>Daerah Tinggi:</b> ", high_regions, " (", round(high_regions/nrow(map_data)*100, 1), "%)<br>",
            "<b>Daerah Rendah:</b> ", low_regions, " (", round(low_regions/nrow(map_data)*100, 1), "%)<br>",
            "<hr style='margin: 5px 0;'>",
            "<b>Interpretasi:</b><br>",
            ifelse(high_regions > nrow(map_data)*0.3, 
                   "Distribusi condong ke nilai tinggi", 
                   ifelse(low_regions > nrow(map_data)*0.3,
                          "Distribusi condong ke nilai rendah",
                          "Distribusi relatif merata")),
            "</div>"
          ),
          position = "topleft"
        )
    })
  })
  
  # Download handlers
  output$download_raw_data <- downloadHandler(
    filename = function() {
      paste("climate_data_jatim_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(current_data(), file, row.names = FALSE)
    }
  )
  
  output$download_analysis <- downloadHandler(
    filename = function() {
      paste("analisis_hasil_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Combine all analysis results into one file
      analysis_results <- data.frame(
        Analysis_Type = character(),
        Variable_1 = character(),
        Variable_2 = character(),
        Statistic = character(),
        Value = numeric(),
        P_Value = numeric(),
        Interpretation = character(),
        stringsAsFactors = FALSE
      )
      
      # Add correlation results if available
      if (!is.null(values$correlation_result)) {
        corr_matrix <- values$correlation_result
        for(i in 1:nrow(corr_matrix)) {
          for(j in 1:ncol(corr_matrix)) {
            if(i != j) {
              analysis_results <- rbind(analysis_results, data.frame(
                Analysis_Type = "Spearman Correlation",
                Variable_1 = rownames(corr_matrix)[i],
                Variable_2 = colnames(corr_matrix)[j],
                Statistic = "Correlation Coefficient",
                Value = corr_matrix[i,j],
                P_Value = NA,
                Interpretation = ifelse(abs(corr_matrix[i,j]) > 0.7, "Strong", 
                                        ifelse(abs(corr_matrix[i,j]) > 0.3, "Moderate", "Weak"))
              ))
            }
          }
        }
      }
      
      # Add regression results if available
      if (!is.null(values$regression_result)) {
        reg_summary <- summary(values$regression_result)
        coef_table <- reg_summary$coefficients
        for(i in 1:nrow(coef_table)) {
          analysis_results <- rbind(analysis_results, data.frame(
            Analysis_Type = "Robust Regression",
            Variable_1 = rownames(coef_table)[i],
            Variable_2 = "Coefficient",
            Statistic = "Estimate",
            Value = coef_table[i, 1],
            P_Value = NA,
            Interpretation = ifelse(abs(coef_table[i, 3]) > 2, "Significant", "Not Significant")
          ))
        }
      }
      
      if(nrow(analysis_results) == 0) {
        analysis_results <- data.frame(
          Message = "Belum ada analisis yang dijalankan. Silakan jalankan analisis terlebih dahulu."
        )
      }
      
      write.csv(analysis_results, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)