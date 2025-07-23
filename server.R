server <- function(input, output, session) {
  
  # Observer untuk menginisialisasi pilihan variabel
  observeEvent(rv_data$data, {
    req(rv_data$data)
    .update_all_select_inputs(session, rv_data$data)
  }, ignoreNULL = FALSE, once = TRUE)
  
  # Fungsi pembantu untuk memperbarui semua pilihan `selectInput`
  .update_all_select_inputs <- function(session, data_obj) {
    if (ncol(data_obj) <= 1 && names(data_obj)[1] == "Pesan") {
      return()
    }
    
    numeric_cols <- names(data_obj)[sapply(data_obj, is.numeric)]
    all_cols <- names(data_obj)
    categorical_cols <- all_cols[!sapply(data_obj, is.numeric) |
                                   sapply(data_obj, function(x) length(unique(x)) < 10)]
    
    # Update untuk semua tab
    updateSelectInput(session, "deskriptif_vars", choices = numeric_cols)
    updateSelectInput(session, "homog_bp_dep_var", choices = numeric_cols)
    updateSelectInput(session, "homog_bp_indep_vars", choices = numeric_cols)
    updateSelectInput(session, "norm_res_dep_var", choices = numeric_cols)
    updateSelectInput(session, "norm_res_indep_vars", choices = numeric_cols)
    updateSelectInput(session, "dep_var_reg", choices = numeric_cols)
    updateSelectInput(session, "indep_vars_reg", choices = numeric_cols)
    updateSelectInput(session, "anova_dep_var", choices = numeric_cols)
    updateSelectInput(session, "anova_factor1", choices = categorical_cols)
    updateSelectInput(session, "anova_factor2", choices = categorical_cols)
  }
  
  # Menampilkan waktu saat ini di Beranda
  output$jam_sekarang <- renderText({ format(Sys.time(), "%H:%M:%S") })
  
  # Metadata table (Diperbarui dengan Rentang Nilai Dinamis)
  output$metadata_table <- renderDT({
    # 1. Buat data frame metadata dasar (statis)
    metadata_df <- data.frame(
      `Nama Variabel` = c("DISTRICTCODE", "POPULATION", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER"),
      `Tipe Data` = c("Karakter", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik"),
      Deskripsi = c(
        "Kode unik untuk setiap kabupaten/kota.", "Total populasi di wilayah tersebut.",
        "Jumlah penduduk usia anak-anak.", "Jumlah penduduk berjenis kelamin perempuan.",
        "Jumlah penduduk lansia.", "Jumlah rumah tangga yang dikepalai oleh perempuan.",
        "Rata-rata jumlah anggota dalam satu rumah tangga.", "Jumlah rumah tangga tanpa akses listrik.",
        "Jumlah kepala rumah tangga dengan pendidikan rendah.", "Tingkat pertumbuhan penduduk (%).",
        "Tingkat kemiskinan (%).", "Jumlah penduduk yang buta huruf.",
        "Jumlah penduduk usia kerja yang tidak pernah mengikuti pelatihan.", "Jumlah rumah tangga yang tidak memiliki drone.",
        "Jumlah rumah tangga yang status kepemilikan rumahnya adalah sewa.", "Jumlah rumah tangga tanpa akses ke saluran pembuangan.",
        "Jumlah rumah tangga tanpa akses ke air ledeng."
      )
    )
    
    # 2. Hitung rentang nilai (Min & Max) secara dinamis dari data aktif
    rentang_df <- data.frame(
      `Nama Variabel` = names(rv_data$data),
      `Rentang Nilai (Min - Max)` = sapply(rv_data$data, function(kolom) {
        if (is.numeric(kolom)) {
          min_val <- min(kolom, na.rm = TRUE)
          max_val <- max(kolom, na.rm = TRUE)
          # Format angka agar mudah dibaca
          paste0(format(round(min_val, 2), big.mark = ","), " - ", format(round(max_val, 2), big.mark = ","))
        } else {
          "Tidak Berlaku" # Untuk kolom non-numerik seperti DISTRICTCODE
        }
      })
    )
    
    # 3. Gabungkan metadata dasar dengan data rentang yang dinamis
    # check.names=FALSE agar R tidak mengubah spasi menjadi titik
    names(metadata_df) <- c("Nama Variabel", "Tipe Data", "Deskripsi")
    names(rentang_df) <- c("Nama Variabel", "Rentang Nilai (Min - Max)")
    final_metadata <- left_join(metadata_df, rentang_df, by = "Nama Variabel")
    
    # 4. Tampilkan tabel hasil gabungan dengan pengaturan lebar kolom agar rapi
    datatable(final_metadata,
              rownames = FALSE,
              options = list(
                pageLength = 5,
                scrollX = TRUE,
                autoWidth = FALSE,
                columnDefs = list(
                  list(width = '150px', targets = 0), # Lebar kolom "Nama Variabel"
                  list(width = '100px', targets = 1), # Lebar kolom "Tipe Data"
                  list(width = '400px', targets = 2), # Lebar kolom "Deskripsi"
                  list(width = '200px', targets = 3)  # Lebar kolom "Rentang Nilai"
                ),
                language = list(search = "Cari:")
              )
    )
  })
  
  #====================================================#
  # WORD DOWNLOAD HANDLERS - MENGGUNAKAN TEMP FILE    #
  #====================================================#
  
  # Fungsi untuk membuat Word document dengan R Markdown
  create_word_with_rmd <- function(title, content_lines, output_file) {
    tryCatch({
      # Buat temporary Rmd file
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Buat konten YAML header dan content
      rmd_content <- c(
        "---",
        paste0("title: '", title, "'"),
        "author: 'Taura Davin Santosa (222313401) - Kelas 2KS3'",
        paste0("date: '", Sys.Date(), "'"),
        "output:",
        "  word_document:",
        "    reference_docx: null",
        "---",
        "",
        content_lines
      )
      
      # Tulis ke file Rmd
      writeLines(rmd_content, temp_rmd)
      
      # Render ke Word
      rmarkdown::render(
        input = temp_rmd,
        output_file = output_file,
        quiet = TRUE,
        clean = TRUE
      )
      
      # Hapus temp file
      unlink(temp_rmd)
      
      return(TRUE)
      
    }, error = function(e) {
      cat("Error creating Word with RMD:", e$message, "\n")
      return(FALSE)
    })
  }
  
  # Fungsi fallback untuk membuat file Word sederhana
  create_simple_docx <- function(title, content_lines, output_file) {
    tryCatch({
      # Coba gunakan officer
      if (requireNamespace("officer", quietly = TRUE)) {
        doc <- officer::read_docx()
        
        # Tambah judul
        doc <- doc %>% 
          officer::body_add_par(title, style = "heading 1") %>%
          officer::body_add_par("", style = "Normal") %>%
          officer::body_add_par(paste("Tanggal:", Sys.Date()), style = "Normal") %>%
          officer::body_add_par("Nama: Taura Davin Santosa", style = "Normal") %>%
          officer::body_add_par("NIM: 222313401", style = "Normal") %>%
          officer::body_add_par("Kelas: 2KS3", style = "Normal") %>%
          officer::body_add_par("", style = "Normal")
        
        # Tambah konten
        for (line in content_lines) {
          if (startsWith(line, "# ")) {
            # Header
            doc <- doc %>% officer::body_add_par(gsub("^# ", "", line), style = "heading 2")
          } else if (startsWith(line, "## ")) {
            # Sub header
            doc <- doc %>% officer::body_add_par(gsub("^## ", "", line), style = "heading 3")
          } else if (line != "") {
            # Normal text
            doc <- doc %>% officer::body_add_par(line, style = "Normal")
          } else {
            # Empty line
            doc <- doc %>% officer::body_add_par("", style = "Normal")
          }
        }
        
        # Save
        print(doc, target = output_file)
        return(TRUE)
      } else {
        return(FALSE)
      }
      
    }, error = function(e) {
      cat("Error creating Word with officer:", e$message, "\n")
      return(FALSE)
    })
  }
  
  # WORD DOWNLOAD: Beranda
  output$download_beranda_word <- downloadHandler(
    filename = function() {
      paste0("Laporan_Dashboard_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      # Konten untuk dokumen
      content_lines <- c(
        "# Ringkasan Data",
        "",
        paste("**Jumlah Observasi:** ", nrow(rv_data$data)),
        paste("**Jumlah Variabel:** ", ncol(rv_data$data)),
        "",
        "# Deskripsi",
        "",
        "Dashboard ini dibangun sebagai media interaktif untuk mengeksplorasi dan menganalisis data menggunakan metode statistik deskriptif, inferensia, dan pemodelan regresi.",
        "",
        "Disusun untuk memenuhi tugas UAS Mata Kuliah Komputasi Statistik, Politeknik Statistika STIS.",
        "",
        "# Fitur Dashboard",
        "",
        "- Manajemen Data dan Kategorisasi Variabel",
        "- Statistik Deskriptif",
        "- Visualisasi Grafik dan Peta",
        "- Uji Asumsi Klasik",
        "- Uji Statistik Inferensia",
        "- Analisis Regresi Linear Berganda"
      )
      
      # Coba buat dengan R Markdown dulu
      success_rmd <- create_word_with_rmd("Dashboard Komputasi Statistik", content_lines, file)
      
      if (!success_rmd) {
        # Jika gagal, coba dengan officer
        success_officer <- create_simple_docx("Dashboard Komputasi Statistik", content_lines, file)
        
        if (!success_officer) {
          # Fallback terakhir: buat file teks dengan ekstensi docx
          writeLines(c(
            "Dashboard Komputasi Statistik",
            "",
            paste("Tanggal:", Sys.Date()),
            "Nama: Taura Davin Santosa",
            "NIM: 222313401",
            "Kelas: 2KS3",
            "",
            paste(content_lines, collapse = "\n"),
            "",
            "CATATAN: File ini dibuat dalam format teks karena library Word tidak tersedia.",
            "Untuk membuka sebagai dokumen Word yang proper, install package 'officer' dan 'rmarkdown'."
          ), file)
        }
      }
    }
  )
  
  # WORD DOWNLOAD: Manajemen Data
  output$download_manajemen_word <- downloadHandler(
    filename = function() {
      paste0("Laporan_Manajemen_Data_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      content_lines <- c(
        "# Ringkasan Data",
        "",
        paste("**Jumlah Observasi:** ", nrow(rv_data$data)),
        paste("**Jumlah Variabel:** ", ncol(rv_data$data)),
        "",
        "# Struktur Data",
        "",
        "Dataset ini berisi informasi demografis dan sosial-ekonomi dari berbagai kabupaten/kota di Indonesia.",
        "",
        "## Variabel Utama:",
        "",
        "- **DISTRICTCODE**: Kode unik kabupaten/kota",
        "- **POPULATION**: Total populasi",
        "- **CHILDREN**: Jumlah anak-anak",
        "- **FEMALE**: Jumlah penduduk perempuan",
        "- **ELDERLY**: Jumlah penduduk lansia",
        "- **POVERTY**: Tingkat kemiskinan (%)",
        "- **GROWTH**: Tingkat pertumbuhan (%)",
        "",
        "# Kategorisasi Variabel",
        "",
        "Dashboard menyediakan fitur untuk mengkategorisasi variabel numerik menjadi beberapa kategori (2-5 kategori) berdasarkan quantile untuk memudahkan analisis."
      )
      
      success_rmd <- create_word_with_rmd("Manajemen Data & Kategorisasi Variabel", content_lines, file)
      
      if (!success_rmd) {
        success_officer <- create_simple_docx("Manajemen Data & Kategorisasi Variabel", content_lines, file)
        
        if (!success_officer) {
          writeLines(c(
            "Manajemen Data & Kategorisasi Variabel",
            "",
            paste("Tanggal:", Sys.Date()),
            "",
            paste(content_lines, collapse = "\n")
          ), file)
        }
      }
    }
  )
  
  # WORD DOWNLOAD: Statistik Deskriptif
  output$download_deskriptif_word <- downloadHandler(
    filename = function() {
      paste0("Laporan_Statistik_Deskriptif_", format(Sys.Date(), "%Y%m%d"), ".docx")
    },
    content = function(file) {
      if (is.null(input$deskriptif_vars) || length(input$deskriptif_vars) == 0) {
        content_lines <- c(
          "# Analisis Statistik Deskriptif",
          "",
          "**Status:** Tidak ada variabel yang dipilih untuk analisis.",
          "",
          "Silakan pilih variabel numerik pada dashboard untuk melakukan analisis statistik deskriptif."
        )
      } else {
        content_lines <- c(
          "# Analisis Statistik Deskriptif",
          "",
          "## Variabel yang Dianalisis:",
          "",
          paste("**Variabel:** ", paste(input$deskriptif_vars, collapse = ", ")),
          "",
          "## Statistik yang Dihitung:",
          "",
          "- **N**: Jumlah observasi",
          "- **Mean**: Rata-rata",
          "- **Median**: Nilai tengah",
          "- **SD**: Standar deviasi",
          "- **Variance**: Varians",
          "- **Q1 & Q3**: Kuartil pertama dan ketiga",
          "- **IQR**: Interquartile range",
          "- **Range**: Rentang nilai",
          "- **Skewness**: Kemencengan distribusi",
          "- **Kurtosis**: Keruncingan distribusi",
          "",
          "## Interpretasi:",
          "",
          "Statistik deskriptif memberikan gambaran umum tentang karakteristik data, termasuk ukuran pemusatan, penyebaran, dan bentuk distribusi."
        )
      }
      
      success_rmd <- create_word_with_rmd("Analisis Statistik Deskriptif", content_lines, file)
      
      if (!success_rmd) {
        success_officer <- create_simple_docx("Analisis Statistik Deskriptif", content_lines, file)
        
        if (!success_officer) {
          writeLines(c(
            "Analisis Statistik Deskriptif",
            "",
            paste("Tanggal:", Sys.Date()),
            "",
            paste(content_lines, collapse = "\n")
          ), file)
        }
      }
    }
  )
  
  # Template untuk download Word lainnya menggunakan pattern yang sama
  output$download_visualisasi_word <- downloadHandler(
    filename = function() { paste0("Laporan_Visualisasi_", format(Sys.Date(), "%Y%m%d"), ".docx") },
    content = function(file) {
      content_lines <- c(
        "# Analisis Visualisasi Data",
        "",
        paste("**Jenis Grafik:** ", ifelse(is.null(input$plot_type), "Tidak ada", input$plot_type)),
        "",
        "## Jenis Visualisasi yang Tersedia:",
        "",
        "- **Histogram**: Distribusi frekuensi variabel numerik",
        "- **Scatter Plot**: Hubungan antara dua variabel numerik",
        "- **Box Plot**: Distribusi dan outliers",
        "- **Bar Chart**: Frekuensi variabel kategorikal",
        "- **Line Chart**: Tren data",
        "- **Density Plot**: Estimasi kepadatan probabilitas",
        "- **Violin Plot**: Kombinasi box plot dan density plot",
        "- **Correlation Heatmap**: Matriks korelasi antar variabel",
        "",
        "## Manfaat Visualisasi:",
        "",
        "Visualisasi data membantu dalam:",
        "- Memahami pola dan tren dalam data",
        "- Mengidentifikasi outliers dan anomali",
        "- Melihat hubungan antar variabel",
        "- Mengkomunikasikan temuan secara efektif"
      )
      
      success_rmd <- create_word_with_rmd("Analisis Visualisasi Data", content_lines, file)
      if (!success_rmd) {
        success_officer <- create_simple_docx("Analisis Visualisasi Data", content_lines, file)
        if (!success_officer) {
          writeLines(c("Analisis Visualisasi Data", "", paste("Tanggal:", Sys.Date()), "", paste(content_lines, collapse = "\n")), file)
        }
      }
    }
  )
  
  output$download_peta_word <- downloadHandler(
    filename = function() { paste0("Laporan_Peta_", format(Sys.Date(), "%Y%m%d"), ".docx") },
    content = function(file) {
      content_lines <- c(
        "# Analisis Visualisasi Peta Sebaran Data",
        "",
        paste("**Indikator yang Dianalisis:** ", ifelse(is.null(input$peta_filter_variable), "Tidak ada", input$peta_filter_variable)),
        "",
        "## Fitur Peta Choropleth:",
        "",
        "- **Multiple tile layers**: CartoDB, OpenStreetMap, Satellite",
        "- **Popup informatif**: Detail wilayah dan nilai indikator",
        "- **Kategorisasi otomatis**: Rendah, Sedang, Tinggi",
        "- **Mini map**: Navigasi tambahan",
        "- **Scale bar**: Skala peta",
        "",
        "## Analisis Spasial:",
        "",
        "Peta choropleth menunjukkan distribusi spasial dari indikator yang dipilih across wilayah Indonesia. Analisis spasial membantu:",
        "",
        "- Mengidentifikasi pola geografis",
        "- Melihat disparitas regional",
        "- Menentukan wilayah prioritas",
        "- Mendukung pengambilan keputusan berbasis lokasi"
      )
      
      success_rmd <- create_word_with_rmd("Analisis Visualisasi Peta Sebaran Data", content_lines, file)
      if (!success_rmd) {
        success_officer <- create_simple_docx("Analisis Visualisasi Peta Sebaran Data", content_lines, file)
        if (!success_officer) {
          writeLines(c("Analisis Visualisasi Peta Sebaran Data", "", paste("Tanggal:", Sys.Date()), "", paste(content_lines, collapse = "\n")), file)
        }
      }
    }
  )
  
  # Download handlers untuk uji statistik (menggunakan pattern yang sama)
  output$download_homog_word <- downloadHandler(
    filename = function() { paste0("Uji_Homoskedastisitas_", format(Sys.Date(), "%Y%m%d"), ".docx") },
    content = function(file) {
      content_lines <- c("# Uji Homoskedastisitas (Breusch-Pagan)", "", "## Metode:", "Uji Breusch-Pagan untuk mendeteksi heteroskedastisitas dalam model regresi.", "", "## Hipotesis:", "- **H0**: Tidak ada heteroskedastisitas (homoskedastis)", "- **H1**: Ada heteroskedastisitas", "", "## Interpretasi:", "- Jika p-value < 0.05: Tolak H0 (ada heteroskedastisitas)", "- Jika p-value ≥ 0.05: Gagal tolak H0 (homoskedastis)")
      success_rmd <- create_word_with_rmd("Uji Homoskedastisitas (Breusch-Pagan)", content_lines, file)
      if (!success_rmd) {
        success_officer <- create_simple_docx("Uji Homoskedastisitas (Breusch-Pagan)", content_lines, file)
        if (!success_officer) { writeLines(c("Uji Homoskedastisitas (Breusch-Pagan)", "", paste(content_lines, collapse = "\n")), file) }
      }
    }
  )
  
  output$download_normalitas_word <- downloadHandler(
    filename = function() { paste0("Uji_Normalitas_", format(Sys.Date(), "%Y%m%d"), ".docx") },
    content = function(file) {
      content_lines <- c("# Uji Normalitas Residual (Shapiro-Wilk)", "", "## Metode:", "Uji Shapiro-Wilk untuk menguji normalitas residual dalam model regresi.", "", "## Hipotesis:", "- **H0**: Residual berdistribusi normal", "- **H1**: Residual tidak berdistribusi normal", "", "## Interpretasi:", "- Jika p-value < 0.05: Tolak H0 (residual tidak normal)", "- Jika p-value ≥ 0.05: Gagal tolak H0 (residual normal)")
      success_rmd <- create_word_with_rmd("Uji Normalitas Residual (Shapiro-Wilk)", content_lines, file)
      if (!success_rmd) {
        success_officer <- create_simple_docx("Uji Normalitas Residual (Shapiro-Wilk)", content_lines, file)
        if (!success_officer) { writeLines(c("Uji Normalitas Residual (Shapiro-Wilk)", "", paste(content_lines, collapse = "\n")), file) }
      }
    }
  )
  
  output$download_ttest_word <- downloadHandler(
    filename = function() { paste0("Uji_T_", format(Sys.Date(), "%Y%m%d"), ".docx") },
    content = function(file) {
      content_lines <- c("# Uji Beda Rata-rata (Uji T)", "", "## Metode:", "Uji T untuk menguji perbedaan rata-rata antara kelompok atau terhadap nilai hipotesis.", "", paste("**Jenis uji yang dipilih:** ", ifelse(is.null(input$ttest_main_type), "Tidak ada", input$ttest_main_type)), "", "## Jenis Uji T:", "- **Satu Sampel**: Membandingkan rata-rata sampel dengan nilai hipotesis", "- **Dua Sampel**: Membandingkan rata-rata dua kelompok")
      success_rmd <- create_word_with_rmd("Uji Beda Rata-rata (Uji T)", content_lines, file)
      if (!success_rmd) {
        success_officer <- create_simple_docx("Uji Beda Rata-rata (Uji T)", content_lines, file)
        if (!success_officer) { writeLines(c("Uji Beda Rata-rata (Uji T)", "", paste(content_lines, collapse = "\n")), file) }
      }
    }
  )
  
  output$download_proporsi_word <- downloadHandler(
    filename = function() { paste0("Uji_Proporsi_", format(Sys.Date(), "%Y%m%d"), ".docx") },
    content = function(file) {
      content_lines <- c("# Uji Proporsi", "", "## Metode:", "Uji proporsi untuk menguji perbedaan proporsi antara kelompok atau terhadap nilai hipotesis.", "", paste("**Jenis uji yang dipilih:** ", ifelse(is.null(input$prop_main_type), "Tidak ada", input$prop_main_type)), "", "## Aplikasi:", "- Menguji proporsi populasi", "- Membandingkan proporsi antar kelompok", "- Analisis data kategorikal")
      success_rmd <- create_word_with_rmd("Uji Proporsi", content_lines, file)
      if (!success_rmd) {
        success_officer <- create_simple_docx("Uji Proporsi", content_lines, file)
        if (!success_officer) { writeLines(c("Uji Proporsi", "", paste(content_lines, collapse = "\n")), file) }
      }
    }
  )
  
  output$download_varians_word <- downloadHandler(
    filename = function() { paste0("Uji_Varians_", format(Sys.Date(), "%Y%m%d"), ".docx") },
    content = function(file) {
      content_lines <- c("# Uji Varians", "", "## Metode:", "Uji F untuk menguji kesamaan varians antara dua kelompok.", "", "## Hipotesis:", "- **H0**: Varians kedua kelompok sama", "- **H1**: Varians kedua kelompok berbeda", "", "## Kegunaan:", "- Menguji homogenitas varians", "- Prasyarat untuk beberapa uji statistik", "- Analisis variabilitas data")
      success_rmd <- create_word_with_rmd("Uji Varians", content_lines, file)
      if (!success_rmd) {
        success_officer <- create_simple_docx("Uji Varians", content_lines, file)
        if (!success_officer) { writeLines(c("Uji Varians", "", paste(content_lines, collapse = "\n")), file) }
      }
    }
  )
  
  output$download_anova_word <- downloadHandler(
    filename = function() { paste0("Laporan_ANOVA_", format(Sys.Date(), "%Y%m%d"), ".docx") },
    content = function(file) {
      content_lines <- c("# Analisis ANOVA (Analysis of Variance)", "", "## Metode:", "Analysis of Variance (ANOVA) untuk menguji perbedaan rata-rata antara beberapa kelompok.", "", paste("**Jenis ANOVA yang dipilih:** ", ifelse(is.null(input$anova_type), "Tidak ada", input$anova_type)), paste("**Variabel Dependen:** ", ifelse(is.null(input$anova_dep_var), "Tidak ada", input$anova_dep_var)), paste("**Faktor 1:** ", ifelse(is.null(input$anova_factor1), "Tidak ada", input$anova_factor1)), "", "## Jenis ANOVA:", "- **One-Way ANOVA**: Satu faktor", "- **Two-Way ANOVA**: Dua faktor dengan/tanpa interaksi", "", "## Output:", "- Tabel ANOVA", "- Uji Post-Hoc (Tukey HSD)", "- Plot diagnostik", "- Visualisasi means")
      success_rmd <- create_word_with_rmd("Analisis ANOVA (Analysis of Variance)", content_lines, file)
      if (!success_rmd) {
        success_officer <- create_simple_docx("Analisis ANOVA (Analysis of Variance)", content_lines, file)
        if (!success_officer) { writeLines(c("Analisis ANOVA (Analysis of Variance)", "", paste(content_lines, collapse = "\n")), file) }
      }
    }
  )
  
  output$download_regresi_word <- downloadHandler(
    filename = function() { paste0("Laporan_Regresi_", format(Sys.Date(), "%Y%m%d"), ".docx") },
    content = function(file) {
      content_lines <- c("# Regresi Linear Berganda & Uji Asumsi Klasik", "", "## Metode:", "Regresi Linear Berganda untuk menganalisis hubungan antara variabel dependen dengan beberapa variabel independen.", "", paste("**Variabel Dependen:** ", ifelse(is.null(input$dep_var_reg), "Tidak ada", input$dep_var_reg)), paste("**Variabel Independen:** ", ifelse(is.null(input$indep_vars_reg), "Tidak ada", paste(input$indep_vars_reg, collapse = ", "))), "", "## Analisis yang Dilakukan:", "- Model OLS (Ordinary Least Squares)", "- Model Robust Regression", "- Uji Asumsi Klasik:", "  - Normalitas residual (Shapiro-Wilk)", "  - Homoskedastisitas (Breusch-Pagan)", "  - Autokorelasi (Durbin-Watson)", "  - Multikolinearitas (VIF)", "", "## Output:", "- Koefisien regresi dan interpretasi", "- Uji signifikansi (t-test dan F-test)", "- R-squared dan Adjusted R-squared", "- Plot diagnostik")
      success_rmd <- create_word_with_rmd("Regresi Linear Berganda & Uji Asumsi Klasik", content_lines, file)
      if (!success_rmd) {
        success_officer <- create_simple_docx("Regresi Linear Berganda & Uji Asumsi Klasik", content_lines, file)
        if (!success_officer) { writeLines(c("Regresi Linear Berganda & Uji Asumsi Klasik", "", paste(content_lines, collapse = "\n")), file) }
      }
    }
  )
  
  # Download handlers yang tetap (CSV, PNG, PDF, HTML) - SAMA SEPERTI SEBELUMNYA
  output$download_metadata_csv <- downloadHandler(
    filename = function() { paste("Metadata_Variabel_", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      metadata_df <- data.frame(
        Nama_Variabel = c("DISTRICTCODE", "POPULATION", "CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER"),
        Tipe_Data = c("Karakter", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik", "Numerik"),
        Deskripsi = c("Kode unik untuk setiap kabupaten/kota", "Total populasi di wilayah tersebut", "Jumlah penduduk usia anak-anak", "Jumlah penduduk berjenis kelamin perempuan", "Jumlah penduduk lansia", "Jumlah rumah tangga yang dikepalai oleh perempuan", "Rata-rata jumlah anggota dalam satu rumah tangga", "Jumlah rumah tangga tanpa akses listrik", "Jumlah kepala rumah tangga dengan pendidikan rendah", "Tingkat pertumbuhan penduduk (%)", "Tingkat kemiskinan (%)", "Jumlah penduduk yang buta huruf", "Jumlah penduduk usia kerja yang tidak pernah mengikuti pelatihan", "Jumlah rumah tangga yang tidak memiliki drone", "Jumlah rumah tangga yang status kepemilikan rumahnya adalah sewa", "Jumlah rumah tangga tanpa akses ke saluran pembuangan", "Jumlah rumah tangga tanpa akses ke air ledeng")
      )
      write.csv(metadata_df, file, row.names = FALSE)
    }
  )
  
  
  output$download_data_original_csv <- downloadHandler(
    filename = function() { paste("Data_Original_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(rv_data$data, file, row.names = FALSE) }
  )
  
  output$download_data_processed_csv <- downloadHandler(
    filename = function() { paste("Data_Terproses_", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(rv_data$data, file, row.names = FALSE) }
  )
  
  #====================================================#
  # TAB: MANAJEMEN DATA                              #
  #====================================================#
  
  output$tabel_data_manajemen <- renderDT({
    datatable(rv_data$data, options = list(scrollX = TRUE, pageLength = 5))
  })
  
  output$custom_category_labels_ui <- renderUI({
    req(input$num_categories)
    lapply(1:input$num_categories, function(i) {
      textInput(paste0("kategori_label_", i), paste0("Label Kategori ", i, ":"), value = paste0("K", i))
    })
  })
  
  observeEvent(input$apply_categorization_btn, {
    req(input$num_categories)
    num_cat <- input$num_categories
    kategori_labels <- sapply(1:num_cat, function(i) {
      input[[paste0("kategori_label_", i)]]
    })
    validate(need(all(nchar(kategori_labels) > 0), "Harap isi semua label kategori."))
    
    updated_data <- rv_data$data
    numeric_cols_to_cat <- names(updated_data)[sapply(updated_data, is.numeric)]
    
    summary_list <- list()
    for (col in numeric_cols_to_cat) {
      vec <- updated_data[[col]]
      if (length(unique(na.omit(vec))) < num_cat) {
        summary_list[[col]] <- "Diabaikan (kurang dari jumlah kategori yang diminta)."
        next
      }
      breaks <- quantile(vec, probs = seq(0, 1, length.out = num_cat + 1), na.rm = TRUE, type = 7)
      breaks <- unique(breaks)
      
      new_col_name <- paste0(col, "_cat")
      if (length(breaks) < 2) {
        summary_list[[col]] <- "Diabaikan (tidak bisa membuat interval)."
        next
      }
      
      label_to_use <- if(length(breaks)-1 == length(kategori_labels)) kategori_labels else FALSE
      updated_data[[new_col_name]] <- cut(vec, breaks = breaks, labels = label_to_use, include.lowest = TRUE)
      summary_list[[col]] <- paste0("Berhasil dibuat sebagai '", new_col_name, "'.")
    }
    
    rv_data$data <- updated_data
    
    output$categorization_summary <- renderUI({
      HTML(paste0("<h4>Ringkasan Kategorisasi:</h4><ul>",
                  paste0("<li><b>", names(summary_list), "</b>: ", unlist(summary_list), "</li>", collapse = ""),
                  "</ul>"))
    })
    
    output$tabel_kategorisasi_hasil <- renderDT({
      datatable(rv_data$data, options = list(scrollX = TRUE, pageLength = 5))
    })
    
    .update_all_select_inputs(session, rv_data$data)
  })
  
  #====================================================#
  # TAB: STATISTIK DESKRIPTIF                          #
  #====================================================#
  
  output$tabel_deskriptif <- renderDT({
    req(input$deskriptif_vars)
    
    data_selected <- rv_data$data[, input$deskriptif_vars, drop = FALSE]
    
    summary_list <- lapply(data_selected, function(x) {
      if(is.numeric(x)) {
        vec <- na.omit(x)
        data.frame(
          N = length(vec), Mean = mean(vec), Median = median(vec),
          SD = sd(vec), Variance = var(vec), Q1 = quantile(vec, 0.25),
          Q3 = quantile(vec, 0.75), IQR = IQR(vec), Range = max(vec) - min(vec),
          Skewness = skewness(vec, type = 2), Kurtosis = kurtosis(vec, type = 2)
        )
      }
    })
    
    summary_df <- bind_rows(summary_list, .id = "Variabel") %>%
      mutate(across(where(is.numeric), ~round(., 3)))
    
    datatable(summary_df, rownames = FALSE,
              options = list(scrollX = TRUE, pageLength = 10, language = list(search = "Cari:")))
  })
  
  output$interpretasi_deskriptif <- renderUI({
    req(input$deskriptif_vars)
    
    interpretations <- lapply(input$deskriptif_vars, function(var_name) {
      vec <- na.omit(rv_data$data[[var_name]])
      if(!is.numeric(vec)) return(NULL)
      
      mean_val <- mean(vec)
      median_val <- median(vec)
      sd_val <- sd(vec)
      iqr_val <- IQR(vec)
      skew_val <- skewness(vec, type = 2)
      kurt_val <- kurtosis(vec, type = 2)
      
      skew_desc <- if (abs(skew_val) < 0.5) {
        "distribusi cenderung <b>simetris</b>."
      } else if (skew_val > 0.5) {
        "distribusi <b>menjulur ke kanan (positive skew)</b>."
      } else {
        "distribusi <b>menjulur ke kiri (negative skew)</b>."
      }
      
      kurt_desc <- if (kurt_val > 1) {
        "<b>Leptokurtik</b>. Distribusi memiliki puncak yang lebih runcing."
      } else if (kurt_val < -1) {
        "<b>Platykurtik</b>. Distribusi memiliki puncak yang lebih datar."
      } else {
        "<b>Mesokurtik</b>. Keruncingan distribusi mendekati normal."
      }
      
      paste0(
        "<h4>Interpretasi untuk Variabel: <strong>", var_name, "</strong></h4>",
        "<ul>",
        "<li><b>Mean:</b> ", round(mean_val, 2), "</li>",
        "<li><b>Median:</b> ", round(median_val, 2), "</li>",
        "<li><b>Standard Deviation:</b> ", round(sd_val, 2), "</li>",
        "<li><b>Skewness:</b> ", round(skew_val, 2), " - ", skew_desc, "</li>",
        "<li><b>Kurtosis:</b> ", round(kurt_val, 2), " - ", kurt_desc, "</li>",
        "</ul>"
      )
    })
    HTML(paste(interpretations, collapse = "<hr>"))
  })
  
  #====================================================#
  # TAB: VISUALISASI GRAFIK                           #
  #====================================================#
  
  # UI dinamis untuk input grafik
  output$plot_inputs <- renderUI({
    numeric_vars <- names(rv_data$data)[sapply(rv_data$data, is.numeric)]
    categorical_vars <- names(rv_data$data)[!sapply(rv_data$data, is.numeric) |
                                              sapply(rv_data$data, function(x) length(unique(na.omit(x))) <= 10)]
    
    switch(input$plot_type,
           "Histogram" = {
             tagList(
               selectInput("hist_var", "Pilih Variabel (X):", choices = numeric_vars),
               numericInput("hist_bins", "Jumlah Bins:", value = 30, min = 5, max = 100)
             )
           },
           "Scatter Plot" = {
             tagList(
               selectInput("scatter_var_x", "Pilih Variabel (X):", choices = numeric_vars),
               selectInput("scatter_var_y", "Pilih Variabel (Y):", choices = numeric_vars,
                           selected = if(length(numeric_vars) > 1) numeric_vars[2] else NULL),
               checkboxInput("add_smooth", "Tambah Garis Regresi", value = FALSE)
             )
           },
           "Box Plot" = {
             tagList(
               selectInput("box_var_y", "Pilih Variabel Numerik (Y):", choices = numeric_vars),
               selectInput("box_var_x", "Pilih Variabel Kategorikal (X) [Opsional]:",
                           choices = c("Tidak Ada" = "", categorical_vars), selected = "")
             )
           },
           "Bar Chart" = {
             tagList(
               selectInput("bar_var", "Pilih Variabel Kategorikal:", choices = categorical_vars)
             )
           },
           "Line Chart" = {
             tagList(
               selectInput("line_var_x", "Pilih Variabel X:", choices = numeric_vars),
               selectInput("line_var_y", "Pilih Variabel Y:", choices = numeric_vars,
                           selected = if(length(numeric_vars) > 1) numeric_vars[2] else NULL)
             )
           },
           "Density Plot" = {
             tagList(
               selectInput("density_var", "Pilih Variabel Numerik:", choices = numeric_vars)
             )
           },
           "Violin Plot" = {
             tagList(
               selectInput("violin_var_y", "Pilih Variabel Numerik (Y):", choices = numeric_vars),
               selectInput("violin_var_x", "Pilih Variabel Kategorikal (X):", choices = categorical_vars)
             )
           },
           "Correlation Heatmap" = {
             tagList(
               selectInput("corr_vars", "Pilih Variabel untuk Korelasi:",
                           choices = numeric_vars, multiple = TRUE,
                           selected = if(length(numeric_vars) >= 2) numeric_vars[1:min(5, length(numeric_vars))] else numeric_vars)
             )
           }
    )
  })
  
  # Render plot utama
  output$visual_plot <- renderPlot({
    switch(input$plot_type,
           "Histogram" = {
             req(input$hist_var)
             ggplot(rv_data$data, aes_string(x = input$hist_var)) +
               geom_histogram(bins = input$hist_bins, fill = input$plot_color, color = "white", alpha = 0.8) +
               labs(title = paste("Histogram dari", input$hist_var),
                    x = input$hist_var,
                    y = "Frekuensi") +
               theme_minimal()
           },
           
           "Scatter Plot" = {
             req(input$scatter_var_x, input$scatter_var_y)
             p <- ggplot(rv_data$data, aes_string(x = input$scatter_var_x, y = input$scatter_var_y)) +
               geom_point(color = input$plot_color, alpha = 0.7, size = 2) +
               labs(title = paste("Scatter Plot antara", input$scatter_var_x, "dan", input$scatter_var_y),
                    x = input$scatter_var_x,
                    y = input$scatter_var_y) +
               theme_minimal()
             
             if (input$add_smooth) {
               p <- p + geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.3)
             }
             p
           },
           
           "Box Plot" = {
             req(input$box_var_y)
             if(input$box_var_x == "") {
               ggplot(rv_data$data, aes_string(x = '""', y = input$box_var_y)) +
                 geom_boxplot(fill = input$plot_color, alpha = 0.7, width = 0.5) +
                 labs(title = paste("Box Plot dari", input$box_var_y),
                      x = "",
                      y = input$box_var_y) +
                 theme_minimal() +
                 theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
             } else {
               ggplot(rv_data$data, aes_string(x = input$box_var_x, y = input$box_var_y)) +
                 geom_boxplot(fill = input$plot_color, alpha = 0.7) +
                 labs(title = paste("Box Plot dari", input$box_var_y, "berdasarkan", input$box_var_x),
                      x = input$box_var_x,
                      y = input$box_var_y) +
                 theme_minimal()
             }
           },
           
           "Bar Chart" = {
             req(input$bar_var)
             ggplot(rv_data$data, aes_string(x = input$bar_var)) +
               geom_bar(fill = input$plot_color, alpha = 0.8) +
               labs(title = paste("Bar Chart dari", input$bar_var),
                    x = input$bar_var,
                    y = "Frekuensi") +
               theme_minimal()
           },
           
           "Line Chart" = {
             req(input$line_var_x, input$line_var_y)
             ggplot(rv_data$data, aes_string(x = input$line_var_x, y = input$line_var_y)) +
               geom_line(color = input$plot_color, size = 1) +
               geom_point(color = input$plot_color, size = 2) +
               labs(title = paste("Line Chart antara", input$line_var_x, "dan", input$line_var_y),
                    x = input$line_var_x,
                    y = input$line_var_y) +
               theme_minimal()
           },
           
           "Density Plot" = {
             req(input$density_var)
             ggplot(rv_data$data, aes_string(x = input$density_var)) +
               geom_density(fill = input$plot_color, alpha = 0.7) +
               labs(title = paste("Density Plot dari", input$density_var),
                    x = input$density_var,
                    y = "Density") +
               theme_minimal()
           },
           
           "Violin Plot" = {
             req(input$violin_var_x, input$violin_var_y)
             ggplot(rv_data$data, aes_string(x = input$violin_var_x, y = input$violin_var_y)) +
               geom_violin(fill = input$plot_color, alpha = 0.7) +
               labs(title = paste("Violin Plot dari", input$violin_var_y, "berdasarkan", input$violin_var_x),
                    x = input$violin_var_x,
                    y = input$violin_var_y) +
               theme_minimal()
           },
           
           "Correlation Heatmap" = {
             req(input$corr_vars)
             if(length(input$corr_vars) < 2) {
               return(ggplot() +
                        annotate("text", x = 0.5, y = 0.5, label = "Pilih minimal 2 variabel untuk heatmap korelasi",
                                 size = 6) +
                        theme_void())
             }
             
             corr_data <- rv_data$data[, input$corr_vars, drop = FALSE] %>%
               cor(method = "pearson", use = "complete.obs")
             
             corr_long <- expand.grid(Var1 = rownames(corr_data), Var2 = colnames(corr_data))
             corr_long$value <- as.vector(corr_data)
             
             ggplot(corr_long, aes(Var1, Var2, fill = value)) +
               geom_tile() +
               geom_text(aes(label = round(value, 2)), color = "white", size = 3) +
               scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                    midpoint = 0, limit = c(-1,1), space = "Lab",
                                    name = "Korelasi") +
               labs(title = "Heatmap Korelasi",
                    x = "", y = "") +
               theme_minimal(base_size = 12) +
               theme(axis.text.x = element_text(angle = 45, hjust = 1))
           }
    )
  })
  
  # Interpretasi grafik
  output$plot_interpretation <- renderUI({
    interpretation <- switch(input$plot_type,
                             "Histogram" = {
                               if(is.null(input$hist_var)) return("")
                               
                               data_var <- na.omit(rv_data$data[[input$hist_var]])
                               mean_val <- mean(data_var)
                               median_val <- median(data_var)
                               skew_val <- skewness(data_var, type = 2)
                               
                               shape_desc <- if (abs(skew_val) < 0.5) {
                                 "relatif <b>simetris</b>"
                               } else if (skew_val > 0.5) {
                                 "<b>menjulur ke kanan</b> (right-skewed)"
                               } else {
                                 "<b>menjulur ke kiri</b> (left-skewed)"
                               }
                               
                               paste0("<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                                      "<h4 style='color: #2c3e50; margin-bottom: 10px;'>📊 Interpretasi Histogram</h4>",
                                      "<p><b>Histogram</b> menunjukkan distribusi frekuensi dari variabel <b>", input$hist_var, "</b>.</p>",
                                      "<ul>",
                                      "<li><b>Bentuk Distribusi:</b> Data ", shape_desc, " dengan skewness = ", round(skew_val, 3), "</li>",
                                      "<li><b>Pusat Data:</b> Mean = ", round(mean_val, 2), ", Median = ", round(median_val, 2), "</li>",
                                      "<li><b>Insight:</b> Histogram membantu mengidentifikasi normalitas data, outliers, dan pola distribusi.</li>",
                                      "</ul></div>")
                             },
                             
                             "Scatter Plot" = {
                               if(is.null(input$scatter_var_x) || is.null(input$scatter_var_y)) return("")
                               
                               x_data <- rv_data$data[[input$scatter_var_x]]
                               y_data <- rv_data$data[[input$scatter_var_y]]
                               correlation <- cor(x_data, y_data, use = "complete.obs")
                               
                               corr_strength <- if (abs(correlation) >= 0.8) {
                                 "sangat kuat"
                               } else if (abs(correlation) >= 0.6) {
                                 "kuat"
                               } else if (abs(correlation) >= 0.4) {
                                 "sedang"
                               } else if (abs(correlation) >= 0.2) {
                                 "lemah"
                               } else {
                                 "sangat lemah"
                               }
                               
                               corr_direction <- if (correlation > 0) "positif" else "negatif"
                               
                               paste0("<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
                                      "<h4 style='color: #2c3e50; margin-bottom: 10px;'>📈 Interpretasi Scatter Plot</h4>",
                                      "<p><b>Scatter Plot</b> menunjukkan hubungan antara <b>", input$scatter_var_x, "</b> dan <b>", input$scatter_var_y, "</b>.</p>",
                                      "<ul>",
                                      "<li><b>Korelasi:</b> ", round(correlation, 3), " (", corr_strength, " dan ", corr_direction, ")</li>",
                                      "<li><b>Interpretasi:</b> ",
                                      if(abs(correlation) >= 0.3) {
                                        paste0("Terdapat hubungan ", corr_strength, " antara kedua variabel.")
                                      } else {
                                        "Hubungan antara kedua variabel sangat lemah atau tidak ada pola linear yang jelas."
                                      }, "</li>",
                                      "</ul></div>")
                             },
                             
                             ""
    )
    
    HTML(interpretation)
  })
  
  #====================================================#
  # TAB: VISUALISASI PETA (FIXED)                     #
  #====================================================#
  
  # Data agregat yang diperluas dengan lebih banyak indikator
  map_data_reactive <- reactive({
    req(rv_data$data)
    
    # Cek berbagai kemungkinan nama file GeoJSON
    possible_files <- c(
      "indonesia_kab.geojson",
      "./indonesia_kab.geojson",
      file.path(getwd(), "indonesia_kab.geojson"),
      "Indonesia_kab.geojson",
      "INDONESIA_KAB.geojson"
    )
    
    geojson_file <- NULL
    for (file_path in possible_files) {
      if (file.exists(file_path)) {
        geojson_file <- file_path
        break
      }
    }
    
    if (is.null(geojson_file)) {
      # Print working directory and list files for debugging
      cat("Working directory:", getwd(), "\n")
      cat("Files in directory:", paste(list.files(pattern = "*.geojson"), collapse = ", "), "\n")
      return(NULL)
    }
    
    # Baca file GeoJSON
    tryCatch({
      indonesia_map <- st_read(geojson_file, quiet = TRUE)
      
      # Print column names for debugging
      cat("GeoJSON columns:", paste(names(indonesia_map), collapse = ", "), "\n")
      
      # Agregasi data berdasarkan DISTRICTCODE
      aggregated_data <- rv_data$data %>%
        mutate(DISTRICTCODE = as.character(DISTRICTCODE)) %>%
        group_by(DISTRICTCODE) %>%
        summarise(
          jumlah_observasi = n(),
          total_children = sum(CHILDREN, na.rm = TRUE),
          total_female = sum(FEMALE, na.rm = TRUE),
          total_elderly = sum(ELDERLY, na.rm = TRUE),
          total_fhead = sum(FHEAD, na.rm = TRUE),
          avg_familysize = mean(FAMILYSIZE, na.rm = TRUE),
          total_noelectric = sum(NOELECTRIC, na.rm = TRUE),
          total_lowedu = sum(LOWEDU, na.rm = TRUE),
          avg_growth = mean(GROWTH, na.rm = TRUE),
          avg_poverty = mean(POVERTY, na.rm = TRUE),
          total_illiterate = sum(ILLITERATE, na.rm = TRUE),
          total_notraining = sum(NOTRAINING, na.rm = TRUE),
          total_dprone = sum(DPRONE, na.rm = TRUE),
          total_rented = sum(RENTED, na.rm = TRUE),
          total_nosewer = sum(NOSEWER, na.rm = TRUE),
          total_tapwater = sum(TAPWATER, na.rm = TRUE),
          total_population = sum(POPULATION, na.rm = TRUE),
          # Tambahan indikator persentase
          pct_children = (total_children / total_population) * 100,
          pct_female = (total_female / total_population) * 100,
          pct_elderly = (total_elderly / total_population) * 100,
          pct_noelectric = (total_noelectric / jumlah_observasi) * 100,
          pct_lowedu = (total_lowedu / jumlah_observasi) * 100,
          pct_illiterate = (total_illiterate / total_population) * 100
        ) %>%
        ungroup()
      
      map_geojson <- st_read(geojson_file, quiet = TRUE)
      
      # Gabungkan data dengan peta menggunakan kolom kodeprkab
      joined_map_data <- map_geojson %>%
        left_join(aggregated_data, by = c("kodeprkab" = "DISTRICTCODE")) %>%
        mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))
      
      return(joined_map_data)
      
    }, error = function(e) {
      cat("Error reading GeoJSON:", e$message, "\n")
      return(NULL)
    })
  })
  
  # Reactive untuk statistik peta
  map_statistics <- reactive({
    req(input$peta_filter_variable)
    peta_data <- map_data_reactive()
    
    if (is.null(peta_data)) return(NULL)
    
    selected_var <- input$peta_filter_variable
    values <- peta_data[[selected_var]]
    
    list(
      min_val = min(values, na.rm = TRUE),
      max_val = max(values, na.rm = TRUE),
      mean_val = mean(values, na.rm = TRUE),
      median_val = median(values, na.rm = TRUE),
      sd_val = sd(values, na.rm = TRUE),
      total_regions = length(values[!is.na(values) & values > 0]),
      zero_regions = sum(values == 0, na.rm = TRUE)
    )
  })
  
  # Interpretasi Peta yang Komprehensif
  output$peta_interpretation <- renderUI({
    req(input$peta_filter_variable)
    
    peta_data <- map_data_reactive()
    if (is.null(peta_data)) return(HTML("<p>Data peta tidak tersedia.</p>"))
    
    selected_var <- input$peta_filter_variable
    values <- peta_data[[selected_var]]
    stats <- map_statistics()
    
    if (is.null(stats)) return(HTML("<p>Statistik tidak tersedia.</p>"))
    
    # Hitung distribusi kategori
    peta_data$kategori <- NA
    if (stats$min_val == stats$max_val) {
      peta_data$kategori <- "Seragam"
    } else {
      # Menggunakan quantile untuk kategorisasi yang lebih baik
      breaks <- quantile(values, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE)
      peta_data$kategori[values <= breaks[2]] <- "Rendah"
      peta_data$kategori[values > breaks[2] & values <= breaks[3]] <- "Sedang"
      peta_data$kategori[values > breaks[3]] <- "Tinggi"
    }
    
    peta_data$kategori[is.na(peta_data$kategori) | values == 0] <- "Tidak Ada Data"
    kategori_counts <- table(peta_data$kategori[!is.na(peta_data$kategori)])
    total_regions <- sum(kategori_counts)
    
    # Identifikasi wilayah ekstrem
    top_regions <- peta_data %>%
      arrange(desc(!!sym(selected_var))) %>%
      slice_head(n = 5) %>%
      dplyr::select(nmkab, nmprov, !!sym(selected_var))
    
    bottom_regions <- peta_data %>%
      filter(!!sym(selected_var) > 0) %>%
      arrange(!!sym(selected_var)) %>%
      slice_head(n = 5) %>%
      dplyr::select(nmkab, nmprov, !!sym(selected_var))
    
    # Label indikator
    all_choices <- c(
      "Jumlah Observasi" = "jumlah_observasi",
      "Total Populasi" = "total_population",
      "Total Anak-anak" = "total_children",
      "Total Perempuan" = "total_female",
      "Total Lansia" = "total_elderly",
      "Rata-rata Ukuran Keluarga" = "avg_familysize",
      "Rata-rata Kemiskinan (%)" = "avg_poverty",
      "Rata-rata Pertumbuhan (%)" = "avg_growth",
      "Rumah Tangga Tanpa Listrik" = "total_noelectric",
      "KRT Pendidikan Rendah" = "total_lowedu",
      "Penduduk Buta Huruf" = "total_illiterate",
      "Tanpa Pelatihan Kerja" = "total_notraining",
      "Kepala RT Perempuan" = "total_fhead",
      "Rumah Sewa" = "total_rented",
      "Tanpa Saluran Pembuangan" = "total_nosewer",
      "Tanpa Air Ledeng" = "total_tapwater",
      "% Anak-anak" = "pct_children",
      "% Perempuan" = "pct_female",
      "% Lansia" = "pct_elderly",
      "% Tanpa Listrik" = "pct_noelectric",
      "% Pendidikan Rendah" = "pct_lowedu",
      "% Buta Huruf" = "pct_illiterate"
    )
    
    selected_var_label <- names(all_choices)[all_choices == selected_var]
    if(length(selected_var_label) == 0) selected_var_label <- selected_var
    
    # Interpretasi berdasarkan jenis indikator
    interpretation_context <- switch(selected_var,
                                     "avg_poverty" = "Tingkat kemiskinan yang tinggi menunjukkan wilayah yang memerlukan perhatian khusus dalam program pengentasan kemiskinan.",
                                     "total_population" = "Distribusi populasi menunjukkan kepadatan penduduk dan dapat mempengaruhi alokasi sumber daya dan infrastruktur.",
                                     "pct_children" = "Persentase anak-anak yang tinggi menunjukkan wilayah dengan struktur demografi muda yang memerlukan investasi pendidikan dan kesehatan anak.",
                                     "pct_elderly" = "Persentase lansia yang tinggi menunjukkan wilayah dengan struktur demografi tua yang memerlukan layanan kesehatan dan sosial khusus.",
                                     "pct_noelectric" = "Persentase rumah tangga tanpa listrik menunjukkan tingkat akses infrastruktur dasar yang masih perlu ditingkatkan.",
                                     "avg_growth" = "Tingkat pertumbuhan penduduk yang tinggi dapat mengindikasikan dinamika ekonomi dan sosial yang aktif.",
                                     "Indikator ini memberikan gambaran kondisi sosial-ekonomi wilayah yang dapat digunakan untuk perencanaan pembangunan."
    )
    
    # Buat tabel top dan bottom regions
    top_table <- if(nrow(top_regions) > 0) {
      paste0(
        "<table style='width: 100%; border-collapse: collapse; margin: 10px 0; font-size: 12px;'>",
        "<tr style='background-color: #e74c3c; color: white;'>",
        "<th style='padding: 8px; border: 1px solid #ddd;'>Kabupaten/Kota</th>",
        "<th style='padding: 8px; border: 1px solid #ddd;'>Provinsi</th>",
        "<th style='padding: 8px; border: 1px solid #ddd;'>Nilai</th></tr>",
        paste0(apply(top_regions, 1, function(row) {
          paste0("<tr><td style='padding: 6px; border: 1px solid #ddd;'>", row[1],
                 "</td><td style='padding: 6px; border: 1px solid #ddd;'>", row[2],
                 "</td><td style='padding: 6px; border: 1px solid #ddd; text-align: right;'><strong>",
                 format(round(as.numeric(row[3]), 2), big.mark = ","), "</strong></td></tr>")
        }), collapse = ""),
        "</table>"
      )
    } else ""
    
    bottom_table <- if(nrow(bottom_regions) > 0) {
      paste0(
        "<table style='width: 100%; border-collapse: collapse; margin: 10px 0; font-size: 12px;'>",
        "<tr style='background-color: #27ae60; color: white;'>",
        "<th style='padding: 8px; border: 1px solid #ddd;'>Kabupaten/Kota</th>",
        "<th style='padding: 8px; border: 1px solid #ddd;'>Provinsi</th>",
        "<th style='padding: 8px; border: 1px solid #ddd;'>Nilai</th></tr>",
        paste0(apply(bottom_regions, 1, function(row) {
          paste0("<tr><td style='padding: 6px; border: 1px solid #ddd;'>", row[1],
                 "</td><td style='padding: 6px; border: 1px solid #ddd;'>", row[2],
                 "</td><td style='padding: 6px; border: 1px solid #ddd; text-align: right;'><strong>",
                 format(round(as.numeric(row[3]), 2), big.mark = ","), "</strong></td></tr>")
        }), collapse = ""),
        "</table>"
      )
    } else ""
    
    HTML(paste0(
      "<div style='background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin: 15px 0; border-left: 4px solid #3498db;'>",
      "<h3 style='color: #2c3e50; margin-top: 0; display: flex; align-items: center;'>",
      "<i class='fa fa-chart-area' style='margin-right: 10px; color: #3498db;'></i>",
      "Interpretasi Peta: ", selected_var_label, "</h3>",
      
      # Statistik Deskriptif
      "<div style='background-color: white; padding: 15px; border-radius: 5px; margin: 15px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>",
      "<h4 style='color: #2c3e50; margin-top: 0;'>📊 Statistik Deskriptif</h4>",
      "<div style='display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 10px;'>",
      "<div style='text-align: center; padding: 10px; background-color: #ecf0f1; border-radius: 5px;'>",
      "<strong style='color: #e74c3c;'>", format(round(stats$min_val, 2), big.mark = ","), "</strong><br>",
      "<small style='color: #7f8c8d;'>Minimum</small></div>",
      "<div style='text-align: center; padding: 10px; background-color: #ecf0f1; border-radius: 5px;'>",
      "<strong style='color: #f39c12;'>", format(round(stats$mean_val, 2), big.mark = ","), "</strong><br>",
      "<small style='color: #7f8c8d;'>Rata-rata</small></div>",
      "<div style='text-align: center; padding: 10px; background-color: #ecf0f1; border-radius: 5px;'>",
      "<strong style='color: #3498db;'>", format(round(stats$median_val, 2), big.mark = ","), "</strong><br>",
      "<small style='color: #7f8c8d;'>Median</small></div>",
      "<div style='text-align: center; padding: 10px; background-color: #ecf0f1; border-radius: 5px;'>",
      "<strong style='color: #27ae60;'>", format(round(stats$max_val, 2), big.mark = ","), "</strong><br>",
      "<small style='color: #7f8c8d;'>Maksimum</small></div>",
      "</div></div>",
      
      # Distribusi Kategori
      "<div style='background-color: white; padding: 15px; border-radius: 5px; margin: 15px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>",
      "<h4 style='color: #2c3e50; margin-top: 0;'>🎯 Distribusi Wilayah</h4>",
      "<ul style='list-style: none; padding: 0;'>",
      if("Rendah" %in% names(kategori_counts)) paste0("<li style='margin: 5px 0;'><span style='display: inline-block; width: 15px; height: 15px; background-color: green; margin-right: 10px; border-radius: 3px;'></span><strong>Kategori Rendah:</strong> ", kategori_counts["Rendah"], " wilayah (", round(kategori_counts["Rendah"]/total_regions*100, 1), "%)</li>") else "",
      if("Sedang" %in% names(kategori_counts)) paste0("<li style='margin: 5px 0;'><span style='display: inline-block; width: 15px; height: 15px; background-color: orange; margin-right: 10px; border-radius: 3px;'></span><strong>Kategori Sedang:</strong> ", kategori_counts["Sedang"], " wilayah (", round(kategori_counts["Sedang"]/total_regions*100, 1), "%)</li>") else "",
      if("Tinggi" %in% names(kategori_counts)) paste0("<li style='margin: 5px 0;'><span style='display: inline-block; width: 15px; height: 15px; background-color: red; margin-right: 10px; border-radius: 3px;'></span><strong>Kategori Tinggi:</strong> ", kategori_counts["Tinggi"], " wilayah (", round(kategori_counts["Tinggi"]/total_regions*100, 1), "%)</li>") else "",
      if("Tidak Ada Data" %in% names(kategori_counts)) paste0("<li style='margin: 5px 0;'><span style='display: inline-block; width: 15px; height: 15px; background-color: grey; margin-right: 10px; border-radius: 3px;'></span><strong>Tidak Ada Data:</strong> ", kategori_counts["Tidak Ada Data"], " wilayah</li>") else "",
      "</ul></div>",
      
      # Wilayah Tertinggi
      if(nrow(top_regions) > 0) paste0(
        "<div style='background-color: white; padding: 15px; border-radius: 5px; margin: 15px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>",
        "<h4 style='color: #e74c3c; margin-top: 0;'>🔺 5 Wilayah Tertinggi</h4>",
        top_table, "</div>"
      ) else "",
      
      # Wilayah Terendah
      if(nrow(bottom_regions) > 0) paste0(
        "<div style='background-color: white; padding: 15px; border-radius: 5px; margin: 15px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>",
        "<h4 style='color: #27ae60; margin-top: 0;'>🔻 5 Wilayah Terendah</h4>",
        bottom_table, "</div>"
      ) else "",
      
      # Interpretasi Kontekstual
      "<div style='background-color: white; padding: 15px; border-radius: 5px; margin: 15px 0; box-shadow: 0 2px 4px rgba(0,0,0,0.1);'>",
      "<h4 style='color: #2c3e50; margin-top: 0;'>💡 Interpretasi & Rekomendasi</h4>",
      "<p style='text-align: justify; line-height: 1.6; color: #34495e;'>", interpretation_context, "</p>",
      
      # Rekomendasi berdasarkan distribusi
      "<p style='text-align: justify; line-height: 1.6; color: #34495e;'><strong>Rekomendasi:</strong> ",
      if(length(kategori_counts) > 0 && "Tinggi" %in% names(kategori_counts) && "Rendah" %in% names(kategori_counts)) {
        if(kategori_counts["Tinggi"] > kategori_counts["Rendah"]) {
          "Mayoritas wilayah berada dalam kategori tinggi, menunjukkan perlunya intervensi yang lebih luas dan terkoordinasi."
        } else if(kategori_counts["Rendah"] > kategori_counts["Tinggi"]) {
          "Mayoritas wilayah berada dalam kategori rendah, namun perhatian khusus tetap diperlukan untuk wilayah dengan kategori tinggi."
        } else {
          "Distribusi yang relatif merata menunjukkan perlunya pendekatan yang disesuaikan dengan karakteristik masing-masing wilayah."
        }
      } else {
        "Perlu analisis lebih lanjut untuk menentukan strategi yang tepat."
      }, "</p>",
      "</div>",
      
      "</div>"
    ))
  })
  
  # Tambahan: Summary Statistics Box
  output$peta_summary_stats <- renderUI({
    req(input$peta_filter_variable)
    stats <- map_statistics()
    
    if (is.null(stats)) return(HTML("<p>Statistik tidak tersedia.</p>"))
    
    HTML(paste0(
      "<div style='display: grid; grid-template-columns: repeat(auto-fit, minmax(120px, 1fr)); gap: 10px; margin: 15px 0;'>",
      "<div style='background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 15px; border-radius: 8px; text-align: center;'>",
      "<h4 style='margin: 0; font-size: 24px;'>", stats$total_regions, "</h4>",
      "<p style='margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;'>Total Wilayah</p></div>",
      
      "<div style='background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); color: white; padding: 15px; border-radius: 8px; text-align: center;'>",
      "<h4 style='margin: 0; font-size: 20px;'>", format(round(stats$mean_val, 1), big.mark = ","), "</h4>",
      "<p style='margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;'>Rata-rata</p></div>",
      
      "<div style='background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); color: white; padding: 15px; border-radius: 8px; text-align: center;'>",
      "<h4 style='margin: 0; font-size: 20px;'>", format(round(stats$max_val, 1), big.mark = ","), "</h4>",
      "<p style='margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;'>Maksimum</p></div>",
      
      "<div style='background: linear-gradient(135deg, #43e97b 0%, #38f9d7 100%); color: white; padding: 15px; border-radius: 8px; text-align: center;'>",
      "<h4 style='margin: 0; font-size: 20px;'>", round(stats$sd_val, 1), "</h4>",
      "<p style='margin: 5px 0 0 0; font-size: 12px; opacity: 0.9;'>Std. Deviasi</p></div>",
      "</div>"
    ))
  })
  
  # Ganti bagian `output$peta_choropleth <- renderLeaflet` dengan:
  
  # Render Peta Leaflet yang Diperluas dan Interaktif
  output$peta_choropleth <- renderLeaflet({
    req(input$peta_filter_variable)
    
    peta_data <- map_data_reactive()
    
    if (is.null(peta_data)) {
      # Jika file GeoJSON tidak ada atau error, tampilkan peta kosong dengan pesan
      return(
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addPopups(lng = 118, lat = -2,
                    popup = paste0("File GeoJSON tidak ditemukan. Cek console R untuk detail.<br>",
                                   "Working directory: ", getwd(), "<br>",
                                   "Files: ", paste(list.files(pattern = "*.geojson"), collapse = ", ")))
      )
    }
    
    selected_var <- input$peta_filter_variable
    values <- peta_data[[selected_var]]
    stats <- map_statistics()
    
    if (is.null(stats)) {
      return(
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          setView(lng = 118, lat = -2, zoom = 5) %>%
          addPopups(lng = 118, lat = -2, popup = "Data statistik tidak tersedia")
      )
    }
    
    # Kategorisasi yang lebih fleksibel
    peta_data$kategori <- NA
    if (stats$min_val == stats$max_val) {
      peta_data$kategori <- "Seragam"
      color_palette <- c("purple")
    } else {
      # Menggunakan quantile untuk kategorisasi yang lebih baik
      breaks <- quantile(values, probs = c(0, 0.33, 0.67, 1), na.rm = TRUE, type = 7)
      breaks <- unique(breaks)
      peta_data$kategori[values <= breaks[2]] <- "Rendah"
      peta_data$kategori[values > breaks[2] & values <= breaks[3]] <- "Sedang"
      peta_data$kategori[values > breaks[3]] <- "Tinggi"
      color_palette <- c("green", "orange", "red")
    }
    
    peta_data$kategori[is.na(peta_data$kategori) | values == 0] <- "Tidak Ada Data"
    peta_data$kategori <- factor(peta_data$kategori,
                                 levels = c("Rendah", "Sedang", "Tinggi", "Seragam", "Tidak Ada Data"))
    
    # Palet warna yang diperbaiki
    all_colors <- c("green", "orange", "red", "purple", "grey")
    used_levels <- levels(peta_data$kategori)[levels(peta_data$kategori) %in% peta_data$kategori]
    used_colors <- all_colors[1:length(used_levels)]
    
    pal <- colorFactor(palette = used_colors, domain = used_levels)
    
    # Label indikator yang lebih lengkap
    all_choices <- c(
      "Jumlah Observasi" = "jumlah_observasi",
      "Total Populasi" = "total_population",
      "Total Anak-anak" = "total_children",
      "Total Perempuan" = "total_female",
      "Total Lansia" = "total_elderly",
      "Rata-rata Ukuran Keluarga" = "avg_familysize",
      "Rata-rata Kemiskinan (%)" = "avg_poverty",
      "Rata-rata Pertumbuhan (%)" = "avg_growth",
      "Rumah Tangga Tanpa Listrik" = "total_noelectric",
      "KRT Pendidikan Rendah" = "total_lowedu",
      "Penduduk Buta Huruf" = "total_illiterate",
      "Tanpa Pelatihan Kerja" = "total_notraining",
      "Kepala RT Perempuan" = "total_fhead",
      "Rumah Sewa" = "total_rented",
      "Tanpa Saluran Pembuangan" = "total_nosewer",
      "Tanpa Air Ledeng" = "total_tapwater",
      "% Anak-anak" = "pct_children",
      "% Perempuan" = "pct_female",
      "% Lansia" = "pct_elderly",
      "% Tanpa Listrik" = "pct_noelectric",
      "% Pendidikan Rendah" = "pct_lowedu",
      "% Buta Huruf" = "pct_illiterate"
    )
    
    selected_var_label <- names(all_choices)[all_choices == selected_var]
    if(length(selected_var_label) == 0) selected_var_label <- selected_var
    
    # Popup yang lebih informatif dan interaktif
    popup_labels <- paste0(
      "<div style='font-family: Arial, sans-serif; max-width: 300px;'>",
      "<h4 style='margin: 0 0 10px 0; color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 5px;'>",
      "<i class='fa fa-map-marker'></i> ", peta_data$nmkab, "</h4>",
      
      "<p style='margin: 5px 0; color: #7f8c8d;'><strong>Provinsi:</strong> ", peta_data$nmprov, "</p>",
      
      "<div style='background-color: #ecf0f1; padding: 10px; border-radius: 5px; margin: 10px 0;'>",
      "<h5 style='margin: 0 0 8px 0; color: #2c3e50;'>📊 ", selected_var_label, "</h5>",
      "<p style='margin: 0; font-size: 16px;'><strong style='color: #e74c3c;'>", 
      format(round(values, 2), big.mark = ","), "</strong></p>",
      "<p style='margin: 5px 0 0 0; font-size: 12px; color: #7f8c8d;'>Kategori: <strong>", 
      peta_data$kategori, "</strong></p>",
      "</div>",
      
      # Tambahan informasi kontekstual
      "<div style='font-size: 11px; color: #95a5a6; margin-top: 10px;'>",
      "<p style='margin: 2px 0;'>👥 Populasi: ", format(peta_data$total_population, big.mark = ","), "</p>",
      "<p style='margin: 2px 0;'>📈 Kemiskinan: ", round(peta_data$avg_poverty, 1), "%</p>",
      "<p style='margin: 2px 0;'>👨‍👩‍👧‍👦 Ukuran Keluarga: ", round(peta_data$avg_familysize, 1), "</p>",
      "</div>",
      "</div>"
    )
    
    # Membuat peta dengan fitur interaktif yang diperluas
    leaflet(data = peta_data) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB") %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      
      # Layer peta choropleth
      addPolygons(
        fillColor = ~pal(kategori),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        opacity = 1,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "white",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        popup = popup_labels,
        label = ~paste0(nmkab, ": ", format(round(values, 2), big.mark = ",")),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"
        ),
        group = "Choropleth"
      ) %>%
      
      # Kontrol layer
      addLayersControl(
        baseGroups = c("CartoDB", "OpenStreetMap", "Satellite"),
        overlayGroups = c("Choropleth"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      
      # Legenda yang diperbaiki
      addLegend(
        pal = pal,
        values = ~kategori,
        opacity = 0.8,
        title = HTML(paste0("<strong>", selected_var_label, "</strong><br><small>Kategori</small>")),
        position = "bottomright",
        className = "info legend"
      ) %>%
      
      # Kontrol tambahan
      addScaleBar(position = "bottomleft") %>%
      #addMiniMap(
        #tiles = providers$CartoDB.Positron,
        #toggleDisplay = TRUE,
        #minimized = TRUE
      #) %>%
      
      # Pengaturan view awal
      setView(lng = 118, lat = -2, zoom = 5)
  })
  
  #====================================================#
  # TAB: UJI ASUMSI                                    #
  #====================================================#
  
  # --- Uji Homoskedastisitas ---
  observeEvent(input$run_homog_bptest, {
    req(input$homog_bp_dep_var, input$homog_bp_indep_vars)
    formula <- as.formula(paste(input$homog_bp_dep_var, "~", paste(input$homog_bp_indep_vars, collapse = "+")))
    model <- lm(formula, data = rv_data$data)
    
    test_result <- bptest(model)
    output$homog_bptest_output <- renderPrint({ test_result })
    
    p_value <- test_result$p.value
    kesimpulan <- if (p_value < 0.05) {
      "<b>Tidak Lolos &#x274C;</b>. Nilai p < 0.05, menunjukkan adanya masalah heteroskedastisitas."
    } else {
      "<b>Lolos &#x2705;</b>. Nilai p >= 0.05, menunjukkan tidak ada bukti heteroskedastisitas."
    }
    output$homog_bptest_interpretation <- renderUI(HTML(paste("<h4>Interpretasi</h4>", kesimpulan)))
  })
  
  # --- Uji Normalitas Residual ---
  observeEvent(input$run_norm_res_test, {
    req(input$norm_res_dep_var, input$norm_res_indep_vars)
    formula <- as.formula(paste(input$norm_res_dep_var, "~", paste(input$norm_res_indep_vars, collapse = "+")))
    model <- lm(formula, data = rv_data$data)
    
    test_result <- shapiro.test(residuals(model))
    output$norm_res_output <- renderPrint({ test_result })
    
    p_value <- test_result$p.value
    kesimpulan <- if (p_value < 0.05) {
      "<b>Tidak Lolos &#x274C;</b>. Nilai p < 0.05, menunjukkan residual tidak berdistribusi normal."
    } else {
      "<b>Lolos &#x2705;</b>. Nilai p >= 0.05, menunjukkan residual berdistribusi normal."
    }
    output$norm_res_interpretation <- renderUI(HTML(paste("<h4>Interpretasi</h4>", kesimpulan)))
  })
  
  #====================================================#
  # TAB: UJI BEDA RATA-RATA (T-TEST)                   #
  #====================================================#
  
  output$ttest_inputs_ui <- renderUI({
    numeric_vars <- names(rv_data$data)[sapply(rv_data$data, is.numeric)]
    factor_2level_vars <- names(rv_data$data)[sapply(rv_data$data, function(x) length(unique(na.omit(x))) == 2)]
    
    if (input$ttest_main_type == "Satu Sampel") {
      tagList(
        selectInput("ttest_var_1s", "Pilih Variabel (Numerik):", choices = numeric_vars),
        numericInput("ttest_mu_1s", "Nilai Hipotesis Rata-rata (μ₀):", value = 0),
        radioButtons("ttest_alt_1s", "Hipotesis Alternatif:", choices = c("Two Sided" = "two.sided", "Greater" = "greater", "Less" = "less"), inline = TRUE)
      )
    } else {
      tagList(
        selectInput("ttest_dep_var_2s", "Pilih Variabel Dependen (Numerik):", choices = numeric_vars),
        selectInput("ttest_group_var_2s", "Pilih Variabel Kelompok (2 Level):", choices = factor_2level_vars),
        radioButtons("ttest_alt_2s", "Hipotesis Alternatif:", choices = c("Two Sided" = "two.sided", "Greater" = "greater", "Less" = "less"), inline = TRUE)
      )
    }
  })
  
  observeEvent(input$run_ttest, {
    if (input$ttest_main_type == "Satu Sampel") {
      req(input$ttest_var_1s)
      test_result <- t.test(rv_data$data[[input$ttest_var_1s]], mu = input$ttest_mu_1s, alternative = input$ttest_alt_1s)
      output$ttest_output <- renderPrint({ test_result })
      p_value <- test_result$p.value
      kesimpulan <- if(p_value < 0.05) "ditolak" else "gagal ditolak"
      output$ttest_interpretation <- renderUI(HTML(paste0("<h4>Interpretasi</h4><p>Dengan p-value sebesar ", round(p_value, 4), ", hipotesis nol <b>", kesimpulan, "</b> pada tingkat signifikansi 5%.</p>")))
    } else {
      req(input$ttest_dep_var_2s, input$ttest_group_var_2s)
      formula <- as.formula(paste(input$ttest_dep_var_2s, "~", input$ttest_group_var_2s))
      test_result <- t.test(formula, data = rv_data$data, alternative = input$ttest_alt_2s)
      output$ttest_output <- renderPrint({ test_result })
      p_value <- test_result$p.value
      kesimpulan <- if(p_value < 0.05) "ditolak" else "gagal ditolak"
      output$ttest_interpretation <- renderUI(HTML(paste0("<h4>Interpretasi</h4><p>Dengan p-value sebesar ", round(p_value, 4), ", hipotesis nol <b>", kesimpulan, "</b> pada tingkat signifikansi 5%.</p>")))
    }
  })
  
  #====================================================#
  # TAB: UJI PROPORSI                                  #
  #====================================================#
  
  output$prop_test_inputs_ui <- renderUI({
    all_cols <- names(rv_data$data)
    categorical_cols <- all_cols[!sapply(rv_data$data, is.numeric)]
    factor_2level_vars <- all_cols[sapply(rv_data$data, function(x) length(unique(na.omit(x))) == 2)]
    
    if (input$prop_main_type == "Satu Sampel") {
      tagList(
        selectInput("prop_var_1s", "Pilih Variabel (Kategorikal):", choices = categorical_cols),
        uiOutput("prop_success_level_ui_1s"),
        numericInput("prop_p0_1s", "Proporsi Hipotesis (p₀):", value = 0.5, min = 0, max = 1, step = 0.01)
      )
    } else {
      tagList(
        selectInput("prop_outcome_var_2s", "Pilih Variabel Outcome (2 Level):", choices = factor_2level_vars),
        selectInput("prop_group_var_2s", "Pilih Variabel Kelompok (2 Level):", choices = factor_2level_vars)
      )
    }
  })
  
  output$prop_success_level_ui_1s <- renderUI({
    req(input$prop_var_1s)
    var_levels <- unique(na.omit(rv_data$data[[input$prop_var_1s]]))
    selectInput("prop_success_level_1s", "Pilih Kategori 'Sukses':", choices = var_levels)
  })
  
  observeEvent(input$run_prop_test, {
    output$prop_test_output <- renderPrint({""}); output$prop_test_interpretation <- renderUI({""})
    if (input$prop_main_type == "Satu Sampel") {
      req(input$prop_var_1s, input$prop_success_level_1s)
      var_data <- na.omit(rv_data$data[[input$prop_var_1s]])
      x <- sum(var_data == input$prop_success_level_1s)
      n <- length(var_data)
      p0 <- input$prop_p0_1s
      test_result <- prop.test(x, n, p = p0)
      output$prop_test_output <- renderPrint({ test_result })
      p_value <- test_result$p.value
      kesimpulan <- if (p_value < 0.05) "ditolak" else "gagal ditolak"
      output$prop_test_interpretation <- renderUI(HTML(paste0("<h4>Interpretasi</h4><p>Dengan p-value ", round(p_value, 4), ", H₀ (proporsi = ", p0, ") <b>", kesimpulan, "</b>.</p>")))
    } else {
      req(input$prop_outcome_var_2s, input$prop_group_var_2s, input$prop_outcome_var_2s != input$prop_group_var_2s)
      tbl <- table(rv_data$data[[input$prop_group_var_2s]], rv_data$data[[input$prop_outcome_var_2s]])
      validate(need(all(dim(tbl) == c(2,2)), "Variabel harus memiliki tepat 2 level."))
      test_result <- prop.test(tbl)
      output$prop_test_output <- renderPrint({ test_result })
      p_value <- test_result$p.value
      kesimpulan <- if (p_value < 0.05) "ditolak" else "gagal ditolak"
      output$prop_test_interpretation <- renderUI(HTML(paste0("<h4>Interpretasi</h4><p>Dengan p-value ", round(p_value, 4), ", H₀ (tidak ada perbedaan proporsi) <b>", kesimpulan, "</b>.</p>")))
    }
  })
  
  #====================================================#
  # TAB: UJI VARIANS                                  #
  #====================================================#
  
  output$variance_test_inputs_ui <- renderUI({
    numeric_vars <- names(rv_data$data)[sapply(rv_data$data, is.numeric)]
    factor_2level_vars <- names(rv_data$data)[sapply(rv_data$data, function(x) length(unique(na.omit(x))) == 2)]
    
    variance_type_ui <- radioButtons("variance_test_type", "Pilih Jenis Uji Varians:",
                                     choices = c("Satu Kelompok" = "one_sample",
                                                 "Dua Kelompok" = "two_sample"),
                                     selected = "one_sample", inline = TRUE)
    
    conditional_ui <- conditionalPanel(
      condition = "input.variance_test_type == 'one_sample'",
      selectInput("var_test_var_1s", "Pilih Variabel (Numerik):", choices = numeric_vars),
      numericInput("var_test_sigma2_1s", "Varians Hipotesis (σ²₀):", value = 1, min = 0.01, step = 0.01)
    )
    
    conditional_ui_2s <- conditionalPanel(
      condition = "input.variance_test_type == 'two_sample'",
      selectInput("var_test_dep_var_2s", "Pilih Variabel (Numerik):", choices = numeric_vars),
      selectInput("var_test_group_var_2s", "Pilih Variabel Kelompok (2 Level):", choices = factor_2level_vars)
    )
    
    tagList(variance_type_ui, conditional_ui, conditional_ui_2s)
  })
  
  observeEvent(input$run_variance_test, {
    output$variance_test_output <- renderPrint({""})
    output$variance_test_interpretation <- renderUI({""})
    
    if (input$variance_test_type == "one_sample") {
      req(input$var_test_var_1s, input$var_test_sigma2_1s)
      
      data_var <- na.omit(rv_data$data[[input$var_test_var_1s]])
      n <- length(data_var)
      sample_var <- var(data_var)
      sigma2_0 <- input$var_test_sigma2_1s
      
      chi_square_stat <- (n - 1) * sample_var / sigma2_0
      df <- n - 1
      p_value <- 2 * min(pchisq(chi_square_stat, df), 1 - pchisq(chi_square_stat, df))
      
      output$variance_test_output <- renderPrint({
        cat("=== UJI VARIANS SATU KELOMPOK (Chi-Square Test) ===\n")
        cat("Data: ", input$var_test_var_1s, "\n")
        cat("Ukuran sampel (n): ", n, "\n")
        cat("Varians sampel (s²): ", round(sample_var, 6), "\n")
        cat("Varians hipotesis (σ²₀): ", sigma2_0, "\n\n")
        cat("Statistik Uji:\n")
        cat("χ² = (n-1)s²/σ²₀ = ", round(chi_square_stat, 6), "\n")
        cat("df = ", df, "\n")
        cat("p-value = ", format.pval(p_value, digits = 6), "\n")
      })
      
      kesimpulan <- if (p_value < 0.05) "ditolak" else "gagal ditolak"
      output$variance_test_interpretation <- renderUI({
        HTML(paste0(
          "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
          "<h4 style='color: #2c3e50; margin-bottom: 10px;'>📊 Interpretasi Uji Varians Satu Kelompok</h4>",
          "<ul>",
          "<li><b>Statistik Uji:</b> χ² = ", round(chi_square_stat, 4), " dengan df = ", df, "</li>",
          "<li><b>P-value:</b> ", format.pval(p_value, digits = 4), "</li>",
          "<li><b>Keputusan:</b> Hipotesis nol <b>", kesimpulan, "</b> pada tingkat signifikansi 5%</li>",
          "<li><b>Varians Sampel:</b> ", round(sample_var, 4), "</li>",
          "</ul>",
          "</div>"
        ))
      })
      
    } else {
      req(input$var_test_dep_var_2s, input$var_test_group_var_2s)
      
      formula <- as.formula(paste(input$var_test_dep_var_2s, "~", input$var_test_group_var_2s))
      test_result <- var.test(formula, data = rv_data$data)
      
      output$variance_test_output <- renderPrint({
        cat("=== UJI VARIANS DUA KELOMPOK (F-Test) ===\n\n")
        print(test_result)
      })
      
      p_value <- test_result$p.value
      f_stat <- test_result$statistic
      
      kesimpulan <- if (p_value < 0.05) "ditolak" else "gagal ditolak"
      
      output$variance_test_interpretation <- renderUI({
        HTML(paste0(
          "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin: 10px 0;'>",
          "<h4 style='color: #2c3e50; margin-bottom: 10px;'>📊 Interpretasi Uji Varians Dua Kelompok</h4>",
          "<ul>",
          "<li><b>Statistik Uji:</b> F = ", round(f_stat, 4), "</li>",
          "<li><b>P-value:</b> ", format.pval(p_value, digits = 4), "</li>",
          "<li><b>Keputusan:</b> Hipotesis nol (rasio varians = 1) <b>", kesimpulan, "</b> pada tingkat signifikansi 5%</li>",
          "<li><b>Kesimpulan:</b> ",
          if (p_value < 0.05) {
            "Terdapat perbedaan varians yang <b>signifikan</b> antara kedua kelompok"
          } else {
            "Tidak terdapat perbedaan varians yang signifikan antara kedua kelompok"
          }, "</li>",
          "</ul>",
          "</div>"
        ))
      })
    }
  })
  
  #====================================================#
  # TAB: UJI ANOVA                                    #
  #====================================================#
  
  observeEvent(input$run_anova, {
    req(input$anova_dep_var, input$anova_factor1)
    
    if (input$anova_type == "twoway") {
      req(input$anova_factor2)
      if (input$anova_factor1 == input$anova_factor2) {
        output$anova_output <- renderPrint({
          cat("Error: Faktor 1 dan Faktor 2 tidak boleh sama untuk ANOVA dua arah.")
        })
        return()
      }
    }
    
    data_anova <- rv_data$data
    data_anova[[input$anova_factor1]] <- as.factor(data_anova[[input$anova_factor1]])
    
    if (input$anova_type == "oneway") {
      formula <- as.formula(paste(input$anova_dep_var, "~", input$anova_factor1))
      anova_title <- "ANOVA SATU ARAH (ONE-WAY ANOVA)"
    } else {
      data_anova[[input$anova_factor2]] <- as.factor(data_anova[[input$anova_factor2]])
      
      if (input$anova_interaction) {
        formula <- as.formula(paste(input$anova_dep_var, "~", input$anova_factor1, "*", input$anova_factor2))
      } else {
        formula <- as.formula(paste(input$anova_dep_var, "~", input$anova_factor1, "+", input$anova_factor2))
      }
      anova_title <- "ANOVA DUA ARAH (TWO-WAY ANOVA)"
    }
    
    anova_model <- lm(formula, data = data_anova)
    anova_result <- anova(anova_model)
    
    output$anova_output <- renderPrint({
      cat("===", anova_title, "===\n\n")
      print(anova_result)
      cat("\n=== RINGKASAN MODEL ===\n")
      print(summary(anova_model))
    })
    
    output$anova_interpretation <- renderUI({
      if (input$anova_type == "oneway") {
        f_value <- anova_result$`F value`[1]
        p_value <- anova_result$`Pr(>F)`[1]
        df_between <- anova_result$Df[1]
        df_within <- anova_result$Df[2]
        
        kesimpulan <- if (p_value < 0.05) {
          "<b>Tolak H₀ ✅</b>. Terdapat perbedaan rata-rata yang signifikan antar kelompok."
        } else {
          "<b>Gagal Tolak H₀ ❌</b>. Tidak terdapat perbedaan rata-rata yang signifikan antar kelompok."
        }
        
        HTML(paste(
          "<h4>Interpretasi ANOVA Satu Arah</h4>",
          "<ul>",
          "<li><b>Statistik Uji:</b> F = ", round(f_value, 4), " dengan df = (", df_between, ", ", df_within, ")</li>",
          "<li><b>P-value:</b> ", format.pval(p_value, digits = 4), "</li>",
          "<li><b>Keputusan:</b> ", kesimpulan, "</li>",
          "</ul>"
        ))
        
      } else {
        n_effects <- nrow(anova_result) - 1
        
        interpretations <- list()
        
        f1 <- anova_result$`F value`[1]
        p1 <- anova_result$`Pr(>F)`[1]
        kesimpulan1 <- if (p1 < 0.05) {
          paste0("<b>Signifikan ✅</b> (F = ", round(f1, 4), ", p = ", format.pval(p1, digits = 4), ")")
        } else {
          paste0("<b>Tidak Signifikan ❌</b> (F = ", round(f1, 4), ", p = ", format.pval(p1, digits = 4), ")")
        }
        interpretations[[1]] <- paste0("<li><b>Main Effect ", input$anova_factor1, ":</b> ", kesimpulan1, "</li>")
        
        f2 <- anova_result$`F value`[2]
        p2 <- anova_result$`Pr(>F)`[2]
        kesimpulan2 <- if (p2 < 0.05) {
          paste0("<b>Signifikan ✅</b> (F = ", round(f2, 4), ", p = ", format.pval(p2, digits = 4), ")")
        } else {
          paste0("<b>Tidak Signifikan ❌</b> (F = ", round(f2, 4), ", p = ", format.pval(p2, digits = 4), ")")
        }
        interpretations[[2]] <- paste0("<li><b>Main Effect ", input$anova_factor2, ":</b> ", kesimpulan2, "</li>")
        
        if (input$anova_interaction && n_effects >= 3) {
          f_int <- anova_result$`F value`[3]
          p_int <- anova_result$`Pr(>F)`[3]
          kesimpulan_int <- if (p_int < 0.05) {
            paste0("<b>Signifikan ✅</b> (F = ", round(f_int, 4), ", p = ", format.pval(p_int, digits = 4), ")")
          } else {
            paste0("<b>Tidak Signifikan ❌</b> (F = ", round(f_int, 4), ", p = ", format.pval(p_int, digits = 4), ")")
          }
          interpretations[[3]] <- paste0("<li><b>Interaction Effect (", input$anova_factor1, " × ", input$anova_factor2, "):</b> ", kesimpulan_int, "</li>")
        }
        
        HTML(paste(
          "<h4>Interpretasi ANOVA Dua Arah</h4>",
          "<ul>",
          paste(interpretations, collapse = ""),
          "</ul>"
        ))
      }
    })
    
    if (input$anova_post_hoc) {
      significant_factors <- c()
      
      if (input$anova_type == "oneway") {
        p_value <- anova_result$`Pr(>F)`[1]
        if (p_value < 0.05) {
          significant_factors <- input$anova_factor1
        }
      } else {
        if (anova_result$`Pr(>F)`[1] < 0.05) significant_factors <- c(significant_factors, input$anova_factor1)
        if (anova_result$`Pr(>F)`[2] < 0.05) significant_factors <- c(significant_factors, input$anova_factor2)
      }
      
      if (length(significant_factors) > 0) {
        aov_model <- aov(formula, data = data_anova)
        tukey_result <- TukeyHSD(aov_model)
        
        output$posthoc_output <- renderPrint({
          cat("=== UJI POST-HOC (TUKEY HSD) ===\n\n")
          print(tukey_result)
        })
        
        output$posthoc_interpretation <- renderUI({
          interpretations <- list()
          
          for (factor_name in names(tukey_result)) {
            tukey_df <- as.data.frame(tukey_result[[factor_name]])
            significant_pairs <- tukey_df[tukey_df$`p adj` < 0.05, ]
            
            if (nrow(significant_pairs) > 0) {
              sig_pairs_text <- apply(significant_pairs, 1, function(row) {
                pair_name <- rownames(row)
                diff <- round(as.numeric(row["diff"]), 4)
                p_adj <- format.pval(as.numeric(row["p adj"]), digits = 4)
                paste0("<li><b>", pair_name, "</b>: Selisih rata-rata = ", diff, " (p-adj = ", p_adj, ")</li>")
              })
              
              interpretations[[factor_name]] <- paste(
                "<h5>", factor_name, ":</h5>",
                "<ul>", paste(sig_pairs_text, collapse = ""), "</ul>"
              )
            } else {
              interpretations[[factor_name]] <- paste0("<h5>", factor_name, ":</h5><p>Tidak ada pasangan yang berbeda signifikan setelah koreksi multiple comparisons.</p>")
            }
          }
          
          HTML(paste(
            "<h4>Interpretasi Uji Post-Hoc (Tukey HSD)</h4>",
            "<p>Uji Tukey HSD untuk faktor yang signifikan:</p>",
            paste(interpretations, collapse = "")
          ))
        })
      } else {
        output$posthoc_output <- renderPrint({
          cat("Uji Post-Hoc tidak dilakukan karena tidak ada faktor yang signifikan.")
        })
        output$posthoc_interpretation <- renderUI({
          HTML("<p>Uji Post-Hoc tidak diperlukan karena tidak ada faktor yang menunjukkan perbedaan signifikan.</p>")
        })
      }
    }
    
    output$anova_diagnostic_plots <- renderPlot({
      if (input$anova_type == "oneway") {
        par(mfrow = c(2, 2), bg = "#F5F5F5")
      } else {
        par(mfrow = c(2, 3), bg = "#F5F5F5")
      }
      
      plot(anova_model, which = 1, main = "Residuals vs Fitted\n(Cek Homoskedastisitas)")
      plot(anova_model, which = 2, main = "Normal Q-Q Plot\n(Cek Normalitas Residual)")
      
      boxplot(data_anova[[input$anova_dep_var]] ~ data_anova[[input$anova_factor1]],
              main = paste("Boxplot", input$anova_dep_var, "per", input$anova_factor1),
              xlab = input$anova_factor1,
              ylab = input$anova_dep_var,
              col = rainbow(length(levels(data_anova[[input$anova_factor1]]))))
      
      hist(residuals(anova_model),
           main = "Histogram Residual\n(Cek Normalitas)",
           xlab = "Residual",
           col = "lightblue",
           breaks = 20)
      
      if (input$anova_type == "twoway") {
        boxplot(data_anova[[input$anova_dep_var]] ~ data_anova[[input$anova_factor2]],
                main = paste("Boxplot", input$anova_dep_var, "per", input$anova_factor2),
                xlab = input$anova_factor2,
                ylab = input$anova_dep_var,
                col = heat.colors(length(levels(data_anova[[input$anova_factor2]]))))
        
        if (input$anova_interaction) {
          interaction.plot(data_anova[[input$anova_factor1]],
                           data_anova[[input$anova_factor2]],
                           data_anova[[input$anova_dep_var]],
                           main = "Interaction Plot",
                           xlab = input$anova_factor1,
                           ylab = paste("Mean of", input$anova_dep_var),
                           trace.label = input$anova_factor2)
        }
      }
    })
    
    output$anova_means_plot <- renderPlot({
      if (input$anova_type == "oneway") {
        means_data <- data_anova %>%
          group_by(!!sym(input$anova_factor1)) %>%
          summarise(
            mean_val = mean(!!sym(input$anova_dep_var), na.rm = TRUE),
            se = sd(!!sym(input$anova_dep_var), na.rm = TRUE) / sqrt(n()),
            .groups = 'drop'
          )
        
        ggplot(means_data, aes(x = !!sym(input$anova_factor1), y = mean_val)) +
          geom_col(fill = "steelblue", alpha = 0.7) +
          geom_errorbar(aes(ymin = mean_val - se, ymax = mean_val + se),
                        width = 0.2, color = "red") +
          labs(title = paste("Rata-rata", input$anova_dep_var, "per", input$anova_factor1),
               x = input$anova_factor1,
               y = paste("Rata-rata", input$anova_dep_var)) +
          theme_minimal(base_size = 14)
        
      } else {
        means_data <- data_anova %>%
          group_by(!!sym(input$anova_factor1), !!sym(input$anova_factor2)) %>%
          summarise(
            mean_val = mean(!!sym(input$anova_dep_var), na.rm = TRUE),
            se = sd(!!sym(input$anova_dep_var), na.rm = TRUE) / sqrt(n()),
            .groups = 'drop'
          )
        
        ggplot(means_data, aes(x = !!sym(input$anova_factor1), y = mean_val,
                               fill = !!sym(input$anova_factor2))) +
          geom_col(position = "dodge", alpha = 0.7) +
          geom_errorbar(aes(ymin = mean_val - se, ymax = mean_val + se),
                        position = position_dodge(width = 0.9), width = 0.2) +
          labs(title = paste("Rata-rata", input$anova_dep_var, "per Kombinasi Faktor"),
               x = input$anova_factor1,
               y = paste("Rata-rata", input$anova_dep_var),
               fill = input$anova_factor2) +
          theme_minimal(base_size = 14) +
          theme(legend.position = "bottom")
      }
    })
  })
  
  #====================================================#
  # TAB: REGRESI LINEAR BERGANDA & UJI ASUMSI          #
  #====================================================#
  
  observeEvent(input$run_regression, {
    req(input$dep_var_reg, input$indep_vars_reg)
    
    formula <- as.formula(paste(input$dep_var_reg, "~", paste(input$indep_vars_reg, collapse = "+")))
    
    model_ols <- lm(formula, data = rv_data$data)
    s_ols <- summary(model_ols)
    
    data_for_rlm <- as.data.frame(rv_data$data)
    model_robust <- MASS::rlm(formula, data = data_for_rlm)
    s_robust <- summary(model_robust)
    
    output$lm_summary <- renderPrint({ s_ols })
    output$model_interpretation <- renderUI({
      coefs <- coef(model_ols)
      eq_parts <- sapply(2:length(coefs), function(i) {
        sign <- if (coefs[i] >= 0) "+" else "-"
        paste(sign, round(abs(coefs[i]), 4), "*", names(coefs)[i])
      })
      equation <- paste(
        "<b>", input$dep_var_reg, "</b> = ",
        round(coefs[1], 4),
        paste(eq_parts, collapse = " ")
      )
      r2_interp <- paste0(
        "<b>Koefisien Determinasi (Adjusted R-squared)</b> model adalah <b>", round(s_ols$adj.r.squared * 100, 2), "%</b>. ",
        "Artinya, sebesar ", round(s_ols$adj.r.squared * 100, 2), "% variasi pada variabel dependen (",
        input$dep_var_reg, ") dapat dijelaskan oleh variabel-variabel independen dalam model."
      )
      f_stat <- s_ols$fstatistic
      f_pval <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
      f_interp <- paste0(
        "<b>Uji Signifikansi Simultan (Uji F)</b> menunjukkan p-value sebesar <b>", format.pval(f_pval, digits = 4), "</b>. ",
        if (f_pval < 0.05) {
          "Model dianggap <b>layak (fit)</b> karena secara bersama-sama, variabel independen berpengaruh signifikan."
        } else {
          "Model dianggap <b>tidak layak (not fit)</b>."
        }
      )
      partial_interp_list <- apply(s_ols$coefficients[-1, , drop = FALSE], 1, function(row) {
        var_name <- rownames(row); estimate <- row[1]; p_val <- row[4]
        direction <- if (estimate > 0) "menaikkan" else "menurunkan"
        significance <- if (p_val < 0.05) "berpengaruh signifikan" else "tidak berpengaruh signifikan"
        paste0("Variabel <b>", var_name, "</b> ", significance, " secara parsial terhadap <b>", input$dep_var_reg, "</b> (p-value = ", format.pval(p_val, digits=4), "). Setiap kenaikan satu unit akan ", direction, " ", input$dep_var_reg, " sebesar <b>", round(abs(estimate), 4), "</b>, asumsi variabel lain konstan.")
      })
      HTML(paste("<ul><li><b>Persamaan Model:</b><br>", equation, "</li><br><li><b>Interpretasi R-squared:</b><br>", r2_interp, "</li><br><li><b>Interpretasi Uji F:</b><br>", f_interp, "</li><br><li><b>Interpretasi Koefisien Parsial (Uji t):</b><ul>", paste("<li>", partial_interp_list, "</li>", collapse = ""), "</ul></li></ul>"))
    })
    
    output$rlm_summary <- renderPrint({ s_robust })
    output$rlm_interpretation <- renderUI({
      coef_robust <- s_robust$coefficients
      
      if (is.matrix(coef_robust) && ncol(coef_robust) >= 2) {
        estimates <- coef_robust[, "Value"]
        std_errors <- coef_robust[, "Std. Error"]
        t_values <- estimates / std_errors
        
        n <- nrow(data_for_rlm)
        p <- length(coef(model_robust))
        df_robust <- n - p
        
        p_values <- 2 * pt(abs(t_values), df = df_robust, lower.tail = FALSE)
        
        coefs_robust <- estimates
        if (length(coefs_robust) > 1) {
          eq_parts_robust <- sapply(2:length(coefs_robust), function(i) {
            sign <- if (coefs_robust[i] >= 0) "+" else "-"
            paste(sign, round(abs(coefs_robust[i]), 4), "*", names(coefs_robust)[i])
          })
          equation_robust <- paste(
            "<b>", input$dep_var_reg, "</b> = ", 
            round(coefs_robust[1], 4), 
            paste(eq_parts_robust, collapse = " ")
          )
        } else {
          equation_robust <- paste("<b>", input$dep_var_reg, "</b> = ", round(coefs_robust[1], 4))
        }
        
        if (length(estimates) > 1) {
          partial_interp_list <- sapply(2:length(estimates), function(i) {
            var_name <- names(estimates)[i]
            estimate <- estimates[i]
            p_val <- p_values[i]
            significance <- if (p_val < 0.05) "signifikan" else "tidak signifikan"
            
            paste0("Variabel <b>", var_name, "</b> ", significance, " dalam model robust (p-value = ",
                   format.pval(p_val, digits = 4), "). Koefisien Robust: <b>", round(estimate, 4), "</b>.")
          })
          
          interpretation_html <- paste(
            "<ul>",
            "<li><b>Persamaan Model Robust:</b><br>", equation_robust, "</li><br>",
            "<li><b>Interpretasi Koefisien Robust:</b></li>",
            "<p>Model robust regression menggunakan metode yang kurang sensitif terhadap outliers dibandingkan OLS biasa. Perbandingan koefisien antara OLS dan Robust dapat mengindikasikan adanya pengaruh outliers.</p>",
            "<ul>",
            paste("<li>", partial_interp_list, "</li>", collapse = ""),
            "</ul>",
            "</ul>"
          )
        } else {
          interpretation_html <- paste(
            "<ul>",
            "<li><b>Persamaan Model Robust:</b><br>", equation_robust, "</li>",
            "<li>Model hanya memiliki intercept.</li>",
            "</ul>"
          )
        }
        
        HTML(interpretation_html)
        
      } else {
        HTML("<p><b>Catatan:</b> Struktur output regresi robust tidak dapat diinterpretasi secara otomatis. Silakan lihat summary di atas untuk detail koefisien.</p>")
      }
    })
    
    model_residuals <- residuals(model_ols)
    
    normality_test <- shapiro.test(model_residuals)
    output$normality_test_output <- renderPrint({ normality_test })
    output$normality_interpretation <- renderUI({
      p_val <- normality_test$p.value
      hasil <- if (p_val < 0.05) "<b>Tolak H₀ &#x274C;</b>" else "<b>Gagal Tolak H₀ &#x2705;</b>"
      HTML(paste0("<b>H₀:</b> Residual berdistribusi normal.<br><b>Hasil:</b> ", hasil, " (p-value = ", round(p_val, 4), ")"))
    })
    
    homoscedasticity_test <- bptest(model_ols)
    output$homoscedasticity_test_output <- renderPrint({ homoscedasticity_test })
    output$homoscedasticity_interpretation <- renderUI({
      p_val <- homoscedasticity_test$p.value
      hasil <- if (p_val < 0.05) "<b>Tolak H₀ &#x274C;</b>" else "<b>Gagal Tolak H₀ &#x2705;</b>"
      HTML(paste0("<b>H₀:</b> Tidak ada heteroskedastisitas (homoskedastis).<br><b>Hasil:</b> ", hasil, " (p-value = ", round(p_val, 4), ")"))
    })
    
    autocorrelation_test <- dwtest(model_ols)
    output$autocorrelation_test_output <- renderPrint({ autocorrelation_test })
    output$autocorrelation_interpretation <- renderUI({
      dw_stat <- autocorrelation_test$statistic
      kesimpulan <- if (dw_stat < 1.5) "Ada indikasi autokorelasi positif." else if (dw_stat > 2.5) "Ada indikasi autokorelasi negatif." else "Tidak ada bukti autokorelasi."
      HTML(paste0("<b>Statistik D-W: ", round(dw_stat, 2), "</b>. ", kesimpulan))
    })
    
    if (length(input$indep_vars_reg) > 1) {
      vif_values <- car::vif(model_ols)
      output$multicollinearity_test_output <- renderPrint({ vif_values })
      output$multicollinearity_interpretation <- renderUI({
        hasil <- if (any(vif_values > 10)) "<b>Terdeteksi multikolinearitas &#x274C;</b>" else "<b>Tidak ada multikolinearitas &#x2705;</b>"
        HTML(paste0("<b>Aturan:</b> VIF > 10 mengindikasikan masalah.<br><b>Hasil:</b> ", hasil))
      })
    } else {
      output$multicollinearity_test_output <- renderPrint({ "Uji VIF memerlukan minimal 2 variabel independen." })
      output$multicollinearity_interpretation <- renderUI({""})
    }
    
    output$diagnostic_plots <- renderPlot({
      par(mfrow = c(1, 2), bg = "#F5F5F5")
      plot(model_ols, which = 1, main = "Residuals vs Fitted")
      plot(model_ols, which = 2, main = "Normal Q-Q")
    })
  })
}
