# Complete Enhanced R Shiny Dashboard UI with Mobile Optimization
library(shinydashboard)
library(shiny)
library(DT)
library(leaflet)
library(plotly)

dashboardPage(
  skin = "green",
  dashboardHeader(title = "Dashboard Statistik", titleWidth = 300),
  
  dashboardSidebar(
    width = 300,
    tags$head(
      # Enhanced Fonts
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Poppins:wght@300;400;500;600;700&family=Inter:wght@300;400;500;600&display=swap",
        rel = "stylesheet"
      ),
      
      # Complete Enhanced CSS with Mobile Optimization
      tags$style(HTML("
        /* === GLOBAL STYLES === */
        * {
          box-sizing: border-box;
        }
        
        .main-header, .main-sidebar, .sidebar-menu, .sidebar-menu a, .main-header .logo {
          font-family: 'Poppins', sans-serif !important;
        }
        
        body, .content-wrapper, .right-side {
          background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%) !important;
          min-height: 100vh;
        }
        
        /* === MOBILE FIRST RESPONSIVE DESIGN === */
        @media (max-width: 767px) {
          .main-sidebar {
            width: 100% !important;
            position: fixed !important;
            z-index: 1050 !important;
            transform: translateX(-100%);
            transition: transform 0.3s ease-in-out;
          }
          
          .main-sidebar.sidebar-open {
            transform: translateX(0);
          }
          
          .content-wrapper {
            margin-left: 0 !important;
            padding: 10px !important;
          }
          
          .main-header .navbar {
            margin-left: 0 !important;
          }
          
          .main-header .logo {
            width: 100% !important;
            text-align: center !important;
          }
          
          .box {
            margin-bottom: 15px !important;
          }
          
          .box-body {
            padding: 15px !important;
          }
          
          .download-buttons {
            flex-direction: column !important;
            gap: 10px !important;
          }
          
          .btn {
            width: 100% !important;
            margin-bottom: 10px !important;
            padding: 15px !important;
            font-size: 16px !important;
          }
          
          .form-control, .selectize-input {
            font-size: 16px !important;
            padding: 15px !important;
          }
          
          .box-title {
            font-size: 16px !important;
          }
          
          h1, h2 {
            font-size: 24px !important;
            text-align: center !important;
            margin-bottom: 20px !important;
          }
          
          .fluidRow {
            margin: 0 !important;
          }
          
          .col-sm-12, .col-sm-6, .col-sm-4, .col-sm-3 {
            padding: 5px !important;
          }
          
          /* Mobile Table Styling */
          .dataTables_wrapper {
            overflow-x: auto !important;
            padding: 10px !important;
          }
          
          .table {
            font-size: 12px !important;
            min-width: 600px !important;
          }
          
          /* Mobile Plot Styling */
          .plotly, .leaflet-container {
            height: 300px !important;
          }
          
          /* Mobile Navigation */
          .sidebar-menu > li > a {
            padding: 15px 20px !important;
            font-size: 14px !important;
          }
          
          .treeview-menu > li > a {
            padding: 12px 15px 12px 35px !important;
            font-size: 13px !important;
          }
        }
        
        @media (min-width: 768px) and (max-width: 991px) {
          .content-wrapper {
            padding: 15px !important;
          }
          
          .box-body {
            padding: 20px !important;
          }
          
          .download-buttons {
            flex-wrap: wrap !important;
            gap: 10px !important;
          }
          
          .btn {
            flex: 1 1 calc(50% - 5px) !important;
            min-width: 150px !important;
          }
        }
        
        /* === HEADER STYLING === */
        .skin-green .main-header .navbar { 
          background: linear-gradient(135deg, #1b5e20 0%, #2e7d32 50%, #388e3c 100%) !important;
          box-shadow: 0 4px 20px rgba(0,0,0,0.1);
          border: none;
        }
        
        .skin-green .main-header .logo {
          background: linear-gradient(135deg, #1b5e20 0%, #2e7d32 100%) !important;
          font-weight: 700;
          font-size: 22px;
          text-align: center;
          cursor: pointer;
          color: #ffffff !important;
          text-shadow: 0 2px 4px rgba(0,0,0,0.3);
          border-right: 1px solid rgba(255,255,255,0.1);
        }
        
        /* === SIDEBAR STYLING === */
        .main-sidebar {
          background: linear-gradient(180deg, #263238 0%, #37474f 100%) !important;
          box-shadow: 4px 0 20px rgba(0,0,0,0.1);
        }
        
        .sidebar-menu > li > a {
          font-size: 16px;
          padding: 18px 25px;
          color: #eceff1 !important;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          border-left: 4px solid transparent;
          position: relative;
          overflow: hidden;
        }
        
        .sidebar-menu > li > a:before {
          content: '';
          position: absolute;
          top: 0;
          left: -100%;
          width: 100%;
          height: 100%;
          background: linear-gradient(90deg, transparent, rgba(255,255,255,0.1), transparent);
          transition: left 0.5s;
        }
        
        .sidebar-menu > li > a:hover:before {
          left: 100%;
        }
        
        .skin-green .main-sidebar .sidebar .sidebar-menu > li > a:hover,
        .skin-green .sidebar-sidebar-menu > li > .treeview-menu > li > a:hover {
          background: linear-gradient(135deg, #4caf50 0%, #66bb6a 100%) !important;
          color: #ffffff !important;
          border-left: 4px solid #ff9800 !important;
          transform: translateX(5px);
          box-shadow: 0 4px 15px rgba(76, 175, 80, 0.3);
        }
        
        .skin-green .main-sidebar .sidebar .sidebar-menu > li.active > a {
          background: linear-gradient(135deg, #4caf50 0%, #66bb6a 100%) !important;
          color: #ffffff !important;
          font-weight: 600;
          border-left: 4px solid #ff9800 !important;
          box-shadow: 0 4px 15px rgba(76, 175, 80, 0.4);
          transform: translateX(5px);
        }
        
        /* === SUBMENU STYLING === */
        .treeview-menu > li > a {
          background: rgba(0,0,0,0.1) !important;
          color: #b0bec5 !important;
          padding-left: 45px !important;
          font-size: 14px;
          transition: all 0.3s ease;
        }
        
        .treeview-menu > li > a:hover {
          background: rgba(76, 175, 80, 0.2) !important;
          color: #ffffff !important;
          padding-left: 50px !important;
        }
        
        /* === BOX STYLING === */
        .box {
          border-radius: 12px !important;
          box-shadow: 0 8px 32px rgba(0,0,0,0.1) !important;
          border: none !important;
          margin-bottom: 25px;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          background: #ffffff;
          overflow: hidden;
        }
        
        .box:hover {
          transform: translateY(-5px);
          box-shadow: 0 12px 40px rgba(0,0,0,0.15) !important;
        }
        
        .box-header {
          background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%) !important;
          border-bottom: 2px solid #e3f2fd !important;
          padding: 20px !important;
          border-radius: 12px 12px 0 0 !important;
        }
        
        .box-header.with-border {
          border-bottom: 2px solid #e3f2fd !important;
        }
        
        .box-title {
          font-size: 18px !important;
          font-weight: 600 !important;
          color: #263238 !important;
          display: flex;
          align-items: center;
          gap: 10px;
        }
        
        .box-body {
          padding: 25px !important;
          background: #ffffff;
        }
        
        /* === STATUS COLORS === */
        .box.box-solid.box-primary > .box-header {
          background: linear-gradient(135deg, #2196f3 0%, #42a5f5 100%) !important;
          color: white !important;
        }
        
        .box.box-solid.box-success > .box-header {
          background: linear-gradient(135deg, #4caf50 0%, #66bb6a 100%) !important;
          color: white !important;
        }
        
        .box.box-solid.box-info > .box-header {
          background: linear-gradient(135deg, #00bcd4 0%, #26c6da 100%) !important;
          color: white !important;
        }
        
        .box.box-solid.box-warning > .box-header {
          background: linear-gradient(135deg, #ff9800 0%, #ffb74d 100%) !important;
          color: white !important;
        }
        
        /* === DOWNLOAD SECTION STYLING === */
        .download-section {
          background: linear-gradient(135deg, #f8f9fa 0%, #e3f2fd 100%) !important;
          padding: 25px;
          border-radius: 12px;
          margin: 20px 0;
          border-left: 5px solid #4caf50 !important;
          box-shadow: 0 4px 20px rgba(0,0,0,0.08);
          position: relative;
          overflow: hidden;
        }
        
        .download-section:before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 3px;
          background: linear-gradient(90deg, #4caf50, #2196f3, #ff9800, #e91e63);
        }
        
        .download-section h4 {
          color: #263238 !important;
          font-weight: 600;
          margin-bottom: 15px;
          font-size: 20px;
        }
        
        .download-buttons {
          display: flex;
          gap: 15px;
          flex-wrap: wrap;
          margin-top: 20px;
        }
        
        /* === BUTTON STYLING === */
        .btn {
          border-radius: 8px !important;
          padding: 12px 24px !important;
          font-weight: 500 !important;
          font-size: 14px !important;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1) !important;
          border: none !important;
          text-transform: none !important;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1) !important;
          position: relative;
          overflow: hidden;
          cursor: pointer;
        }
        
        .btn:before {
          content: '';
          position: absolute;
          top: 50%;
          left: 50%;
          width: 0;
          height: 0;
          background: rgba(255,255,255,0.3);
          border-radius: 50%;
          transform: translate(-50%, -50%);
          transition: width 0.6s, height 0.6s;
        }
        
        .btn:hover:before {
          width: 300px;
          height: 300px;
        }
        
        .btn:hover {
          transform: translateY(-2px) !important;
          box-shadow: 0 8px 25px rgba(0,0,0,0.2) !important;
        }
        
        .btn-success {
          background: linear-gradient(135deg, #4caf50 0%, #66bb6a 100%) !important;
        }
        
        .btn-primary {
          background: linear-gradient(135deg, #2196f3 0%, #42a5f5 100%) !important;
        }
        
        .btn-info {
          background: linear-gradient(135deg, #00bcd4 0%, #26c6da 100%) !important;
        }
        
        .btn-warning {
          background: linear-gradient(135deg, #ff9800 0%, #ffb74d 100%) !important;
        }
        
        .btn-danger {
          background: linear-gradient(135deg, #f44336 0%, #ef5350 100%) !important;
        }
        
        /* === INPUT STYLING === */
        .form-control, .selectize-input {
          border-radius: 8px !important;
          border: 2px solid #e0e0e0 !important;
          padding: 12px 16px !important;
          font-size: 14px !important;
          transition: all 0.3s ease !important;
          background: #ffffff !important;
        }
        
        .form-control:focus, .selectize-input.focus {
          border-color: #4caf50 !important;
          box-shadow: 0 0 0 3px rgba(76, 175, 80, 0.1) !important;
          outline: none !important;
        }
        
        /* === TABLE STYLING === */
        .dataTables_wrapper {
          background: #ffffff;
          border-radius: 12px;
          padding: 20px;
          box-shadow: 0 4px 20px rgba(0,0,0,0.08);
          overflow-x: auto;
        }
        
        .table {
          border-radius: 8px;
          overflow: hidden;
          width: 100%;
        }
        
        .table thead th {
          background: linear-gradient(135deg, #263238 0%, #37474f 100%) !important;
          color: #ffffff !important;
          font-weight: 600;
          border: none !important;
          padding: 15px !important;
        }
        
        .table tbody tr:hover {
          background: rgba(76, 175, 80, 0.05) !important;
          transform: scale(1.01);
          transition: all 0.2s ease;
        }
        
        /* === CONTENT STYLING === */
        .content-header h1 {
          color: #263238 !important;
          font-weight: 700 !important;
          font-size: 28px !important;
          text-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        
        /* === ANIMATION CLASSES === */
        @keyframes fadeInUp {
          from {
            opacity: 0;
            transform: translateY(30px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        
        .box {
          animation: fadeInUp 0.6s ease-out;
        }
        
        /* === CUSTOM SCROLLBAR === */
        ::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }
        
        ::-webkit-scrollbar-track {
          background: #f1f1f1;
          border-radius: 4px;
        }
        
        ::-webkit-scrollbar-thumb {
          background: linear-gradient(135deg, #4caf50, #66bb6a);
          border-radius: 4px;
        }
        
        ::-webkit-scrollbar-thumb:hover {
          background: linear-gradient(135deg, #388e3c, #4caf50);
        }
        
        /* === LOADING ANIMATION === */
        .loading {
          display: inline-block;
          width: 20px;
          height: 20px;
          border: 3px solid rgba(76, 175, 80, 0.3);
          border-radius: 50%;
          border-top-color: #4caf50;
          animation: spin 1s ease-in-out infinite;
        }
        
        @keyframes spin {
          to { transform: rotate(360deg); }
        }
        
        /* === TOOLTIP STYLING === */
        .tooltip-inner {
          background: linear-gradient(135deg, #263238 0%, #37474f 100%) !important;
          border-radius: 6px !important;
          font-size: 12px !important;
        }
        
        /* === PROGRESS BAR === */
        .progress {
          height: 8px;
          border-radius: 4px;
          background: #e0e0e0;
          overflow: hidden;
        }
        
        .progress-bar {
          background: linear-gradient(135deg, #4caf50 0%, #66bb6a 100%);
          transition: width 0.6s ease;
        }
        
        /* === CARD HOVER EFFECTS === */
        .info-box {
          border-radius: 12px !important;
          box-shadow: 0 4px 20px rgba(0,0,0,0.1) !important;
          transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
          border: none !important;
        }
        
        .info-box:hover {
          transform: translateY(-5px);
          box-shadow: 0 8px 30px rgba(0,0,0,0.15) !important;
        }
        
        /* === ENHANCED ICONS === */
        .fa, .glyphicon {
          margin-right: 8px;
          filter: drop-shadow(0 1px 2px rgba(0,0,0,0.1));
        }
        
        /* === NOTIFICATION STYLING === */
        .alert {
          border-radius: 8px !important;
          border: none !important;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1) !important;
        }
        
        /* === TAB STYLING === */
        .nav-tabs-custom > .nav-tabs > li.active > a {
          background: linear-gradient(135deg, #4caf50 0%, #66bb6a 100%) !important;
          color: #ffffff !important;
          border-radius: 8px 8px 0 0 !important;
        }
        
        .nav-tabs-custom > .nav-tabs > li > a {
          border-radius: 8px 8px 0 0 !important;
          transition: all 0.3s ease;
        }
        
        .nav-tabs-custom > .nav-tabs > li > a:hover {
          background: rgba(76, 175, 80, 0.1) !important;
        }
        
        /* === MOBILE MENU TOGGLE === */
        .mobile-menu-toggle {
          display: none;
          position: fixed;
          top: 15px;
          left: 15px;
          z-index: 1060;
          background: #4caf50;
          color: white;
          border: none;
          border-radius: 50%;
          width: 50px;
          height: 50px;
          font-size: 18px;
          box-shadow: 0 4px 15px rgba(0,0,0,0.2);
        }
        
        @media (max-width: 767px) {
          .mobile-menu-toggle {
            display: block;
          }
        }
        
        /* === PLOT RESPONSIVE === */
        .plotly, .leaflet-container, .shiny-plot-output {
          width: 100% !important;
          height: auto !important;
          min-height: 400px;
        }
        
        @media (max-width: 767px) {
          .plotly, .leaflet-container, .shiny-plot-output {
            min-height: 300px !important;
          }
        }
        
        /* === ENHANCED MOBILE INPUTS === */
        @media (max-width: 767px) {
          .selectize-input {
            min-height: 50px !important;
          }
          
          .selectize-dropdown {
            font-size: 16px !important;
          }
          
          .radio, .checkbox {
            margin: 10px 0 !important;
          }
          
          .radio label, .checkbox label {
            font-size: 16px !important;
            padding-left: 25px !important;
          }
        }
      "))
    ),
    
    sidebarMenu(
      id = "tabs",
      menuItem("🏠 Beranda", tabName = "beranda", icon = icon("home")),
      menuItem("📊 Manajemen Data", tabName = "manajemen", icon = icon("database")),
      menuItem("🔍 Eksplorasi Data", tabName = "eksplorasi", icon = icon("chart-bar"),
               menuSubItem("📈 Statistik Deskriptif", tabName = "statistik_deskriptif"),
               menuSubItem("📊 Visualisasi Grafik", tabName = "visualisasi_grafik"),
               menuSubItem("🗺️ Visualisasi Peta", tabName = "visualisasi_peta")
      ),
      menuItem("✅ Uji Asumsi", icon = icon("check-circle"),
               menuSubItem("🔬 Uji Homoskedastisitas", tabName = "uji_homogenitas"),
               menuSubItem("📊 Uji Normalitas", tabName = "uji_normalitas")
      ),
      menuItem("🧪 Statistik Inferensia", icon = icon("flask"),
               menuSubItem("📊 Uji Beda Rata-rata", tabName = "beda_rata"),
               menuSubItem("📈 Uji Proporsi", tabName = "proporsi"),
               menuSubItem("📊 Uji Varians", tabName = "varians"),
               menuSubItem("🔬 Uji ANOVA", tabName = "anova")
      ),
      menuItem("📈 Regresi Linear Berganda", tabName = "regresi", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    # Mobile menu toggle button
    tags$button(
      class = "mobile-menu-toggle",
      onclick = "toggleMobileSidebar()",
      icon("bars")
    ),
    
    # JavaScript for mobile menu
    tags$script(HTML("
      function toggleMobileSidebar() {
        var sidebar = document.querySelector('.main-sidebar');
        sidebar.classList.toggle('sidebar-open');
      }
      
      // Close sidebar when clicking outside on mobile
      document.addEventListener('click', function(event) {
        var sidebar = document.querySelector('.main-sidebar');
        var toggle = document.querySelector('.mobile-menu-toggle');
        
        if (window.innerWidth <= 767 && 
            !sidebar.contains(event.target) && 
            !toggle.contains(event.target)) {
          sidebar.classList.remove('sidebar-open');
        }
      });
    ")),
    
    tabItems(
      # === BERANDA TAB ===
      tabItem(tabName = "beranda",
              fluidRow(
                box(
                  width = 12, status = "success", solidHeader = TRUE,
                  div(style = "text-align: center; padding: 20px;",
                      tags$h1("🎯 Dashboard Komputasi Statistik", 
                              style = "background: linear-gradient(135deg, #4caf50, #2196f3); 
                                    -webkit-background-clip: text; 
                                    -webkit-text-fill-color: transparent; 
                                    font-size: 36px; font-weight: 700; margin-bottom: 10px;"),
                      tags$p("✨ Aplikasi Analisis Statistik Berbasis Web ✨", 
                             style = "font-size: 18px; color: #666; font-weight: 500;")
                  )
                )
              ),
              
              # Download Section
              fluidRow(
                box(
                  title = tagList(icon("download", style = "color: #4caf50;"), "Download Laporan"),
                  status = "success", solidHeader = TRUE, width = 12,
                  div(class = "download-section",
                      h4("📄 Download Laporan Dashboard"),
                      p("Unduh laporan lengkap dashboard dalam format Word atau data metadata dalam format CSV.", 
                        style = "color: #666; font-size: 16px; line-height: 1.6;"),
                      div(class = "download-buttons",
                          downloadButton("download_beranda_word", "📄 Download Laporan Word",
                                         class = "btn-success", icon = icon("file-word")),
                          downloadButton("download_metadata_csv", "📊 Download Metadata CSV",
                                         class = "btn-info", icon = icon("file-csv"))
                      )
                  )
                )
              ),
              
              # Metadata Section
              fluidRow(
                box(
                  title = tagList(icon("book", style = "color: #2196f3;"), "Metadata Variabel"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  p("📋 Tabel berikut menjelaskan variabel-variabel yang digunakan dalam analisis pada dashboard ini.", 
                    style = "font-size: 16px; color: #666; margin-bottom: 20px;"),
                  DTOutput("metadata_table")
                )
              ),
              
              # Description Section
              fluidRow(
                box(
                  title = tagList(icon("info-circle", style = "color: #2196f3;"), "Deskripsi Dashboard"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  div(style = "font-size: 16px; line-height: 1.8; color: #555;",
                      p("🚀 Dashboard ini dibangun sebagai media interaktif untuk mengeksplorasi dan menganalisis data menggunakan metode statistik deskriptif, inferensia, dan pemodelan regresi."),
                      p("🎓 Disusun untuk memenuhi tugas UAS Mata Kuliah Komputasi Statistik, Politeknik Statistika STIS."),
                      p("💡 Fitur utama meliputi:"),
                      tags$ul(
                        tags$li("📊 Manajemen dan kategorisasi data"),
                        tags$li("📈 Statistik deskriptif komprehensif"),
                        tags$li("🎨 Visualisasi data interaktif"),
                        tags$li("🗺️ Peta choropleth untuk analisis spasial"),
                        tags$li("🔬 Uji asumsi klasik regresi"),
                        tags$li("🧪 Berbagai uji statistik inferensia"),
                        tags$li("📈 Analisis regresi linear berganda")
                      )
                  )
                )
              ),
              
              # Info Cards
              fluidRow(
                box(
                  title = tagList(icon("user-graduate", style = "color: #ff9800;"), "Informasi Mahasiswa"),
                  status = "warning", solidHeader = TRUE, width = 4, height = "300px",
                  div(style = "padding: 15px;",
                      HTML("
                      <div style='font-size: 16px; line-height: 2;'>
                        <div style='display: flex; align-items: center; margin-bottom: 10px;'>
                          <i class='fa fa-user' style='color: #ff9800; margin-right: 10px;'></i>
                          <strong>Nama:</strong> &nbsp;Taura Davin Santosa
                        </div>
                        <div style='display: flex; align-items: center; margin-bottom: 10px;'>
                          <i class='fa fa-id-card' style='color: #ff9800; margin-right: 10px;'></i>
                          <strong>NIM:</strong> &nbsp;222313401
                        </div>
                        <div style='display: flex; align-items: center;'>
                          <i class='fa fa-graduation-cap' style='color: #ff9800; margin-right: 10px;'></i>
                          <strong>Kelas:</strong> &nbsp;2KS3
                        </div>
                      </div>
                    ")
                  )
                ),
                box(
                  title = tagList(icon("clock", style = "color: #4caf50;"), "Waktu Saat Ini"),
                  status = "success", solidHeader = TRUE, width = 4, height = "300px",
                  div(style = "text-align: center; padding: 20px;",
                      div(id = "jam_sekarang", style = "font-size: 24px; font-weight: 600; color: #4caf50; 
                          text-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-top: 30px;"),
                      tags$script(HTML("
                        setInterval(function(){
                          var now = new Date();
                          var time = now.toLocaleTimeString('id-ID');
                          var date = now.toLocaleDateString('id-ID', {
                            weekday: 'long',
                            year: 'numeric',
                            month: 'long',
                            day: 'numeric'
                          });
                          document.getElementById('jam_sekarang').innerHTML = 
                            '<div style=\"font-size: 28px; margin-bottom: 10px;\">' + time + '</div>' +
                            '<div style=\"font-size: 14px; color: #666;\">' + date + '</div>';
                        }, 1000);
                      "))
                  )
                ),
                box(
                  title = tagList(icon("quote-left", style = "color: #00bcd4;"), "Kutipan Ilmiah"),
                  status = "info", solidHeader = TRUE, width = 4, height = "300px",
                  div(style = "text-align: center; padding: 20px; display: flex; align-items: center; justify-content: center; height: 100%;",
                      div(style = "font-size: 16px; font-style: italic; color: #555; line-height: 1.6; text-align: center;",
                          '"Tanpa data, Anda hanya seseorang dengan opini."',
                          br(),
                          tags$small("– W. Edwards Deming", style = "color: #00bcd4; font-weight: 600;")
                      )
                  )
                )
              )
      ),
      
      # === MANAJEMEN DATA TAB ===
      tabItem(tabName = "manajemen",
              h2("📊 Manajemen Data & Kategorisasi Variabel", style = "color: #263238; margin-bottom: 30px;"),
              
              # Download Section
              fluidRow(
                box(
                  title = tagList(icon("download", style = "color: #4caf50;"), "Download Data"),
                  status = "success", solidHeader = TRUE, width = 12,
                  div(class = "download-section",
                      h4("📊 Download Data"),
                      p("Unduh data yang telah dikelola dalam berbagai format.", 
                        style = "color: #666; font-size: 16px;"),
                      div(class = "download-buttons",
                          downloadButton("download_data_original_csv", "📄 Data Original (CSV)",
                                         class = "btn-primary", icon = icon("file-csv")),
                          downloadButton("download_data_processed_csv", "⚙️ Data Terproses (CSV)",
                                         class = "btn-info", icon = icon("file-csv")),
                          downloadButton("download_manajemen_word", "📋 Laporan Manajemen (Word)",
                                         class = "btn-success", icon = icon("file-word"))
                      )
                  )
                )
              ),
              
              # Data Table
              fluidRow(
                box(
                  title = tagList(icon("table", style = "color: #2196f3;"), "Tabel Data"),
                  width = 12, status = "primary", solidHeader = TRUE,
                  div(style = 'overflow-x:auto;', DTOutput("tabel_data_manajemen"))
                )
              ),
              
              # Categorization Section
              fluidRow(
                box(
                  title = tagList(icon("cut", style = "color: #00bcd4;"), "Pengkategorian Variabel Numerik"),
                  status = "info", solidHeader = TRUE, width = 12,
                  p("🔧 Ubah variabel numerik menjadi kategori untuk analisis yang lebih mendalam.", 
                    style = "color: #666; margin-bottom: 20px;"),
                  fluidRow(
                    column(6,
                           numericInput("num_categories", "Jumlah Kategori (2-5):", 
                                        value = 3, min = 2, max = 5)
                    ),
                  ),
                  uiOutput("custom_category_labels_ui"),
                  actionButton("apply_categorization_btn", "🔄 Terapkan Kategorisasi", 
                               icon = icon("cut"), class = "btn-info"),
                  tags$hr(),
                  htmlOutput("categorization_summary")
                )
              ),
              
              # Categorized Data Table
              fluidRow(
                box(
                  title = tagList(icon("table", style = "color: #ff9800;"), "Hasil Kategorisasi"),
                  status = "warning", solidHeader = TRUE, width = 12,
                  div(style = 'overflow-x:auto;', DTOutput("tabel_kategorisasi_hasil"))
                )
              )
      ),
      
      # === STATISTIK DESKRIPTIF TAB ===
      tabItem(tabName = "statistik_deskriptif",
              h2("📈 Statistik Deskriptif Data Numerik", style = "color: #263238; margin-bottom: 30px;"),
              
              # Download Section
              fluidRow(
                box(
                  title = tagList(icon("download", style = "color: #4caf50;"), "Download Hasil Analisis"),
                  status = "success", solidHeader = TRUE, width = 12,
                  div(class = "download-section",
                      h4("📈 Download Statistik Deskriptif"),
                      p("Unduh hasil analisis statistik deskriptif dalam berbagai format.", 
                        style = "color: #666; font-size: 16px;"),
                      div(class = "download-buttons",
                          downloadButton("download_deskriptif_csv", "📊 Tabel Statistik (CSV)",
                                         class = "btn-info", icon = icon("file-csv")),
                          downloadButton("download_deskriptif_word", "📄 Laporan Lengkap (Word)",
                                         class = "btn-success", icon = icon("file-word"))
                      )
                  )
                )
              ),
              
              # Variable Selection
              fluidRow(
                box(
                  title = tagList(icon("list", style = "color: #00bcd4;"), "Pilih Variabel Numerik"),
                  status = "info", solidHeader = TRUE, width = 12,
                  p("🎯 Pilih satu atau lebih variabel numerik untuk analisis statistik deskriptif.", 
                    style = "color: #666; margin-bottom: 15px;"),
                  selectInput("deskriptif_vars", "Pilih Variabel:", 
                              choices = NULL, multiple = TRUE)
                  ,
                  actionButton("run_deskriptif", "🚀 Jalankan Analisis", 
                               icon = icon("play-circle"), class = "btn-primary")
                )
              ),
              
              # Results Table
              fluidRow(
                box(
                  title = tagList(icon("table", style = "color: #2196f3;"), "Tabel Statistik Deskriptif"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  DTOutput("tabel_deskriptif")
                )
              ),
              
              # Interpretation
              fluidRow(
                box(
                  title = tagList(icon("lightbulb", style = "color: #ff9800;"), "Interpretasi Statistik Deskriptif"),
                  status = "warning", solidHeader = TRUE, width = 12,
                  htmlOutput("interpretasi_deskriptif")
                )
              )
      ),
      
      # === VISUALISASI GRAFIK TAB ===
      tabItem(tabName = "visualisasi_grafik",
              h2("📊 Visualisasi Data Interaktif", style = "color: #263238; margin-bottom: 30px;"),
              
              # Download Section
              fluidRow(
                box(
                  title = tagList(icon("download", style = "color: #4caf50;"), "Download Visualisasi"),
                  status = "success", solidHeader = TRUE, width = 12,
                  div(class = "download-section",
                      h4("📊 Download Grafik dan Analisis"),
                      p("Unduh grafik dan interpretasi dalam berbagai format.", 
                        style = "color: #666; font-size: 16px;"),
                      div(class = "download-buttons",
                          downloadButton("download_plot_png", "🖼️ Grafik (PNG)",
                                         class = "btn-warning", icon = icon("image")),
                          downloadButton("download_plot_pdf", "📄 Grafik (PDF)",
                                         class = "btn-danger", icon = icon("file-pdf")),
                          downloadButton("download_visualisasi_word", "📋 Laporan Visualisasi (Word)",
                                         class = "btn-success", icon = icon("file-word"))
                      )
                  )
                )
              ),
              
              fluidRow(
                # Plot Settings
                box(
                  title = tagList(icon("cog", style = "color: #00bcd4;"), "Pengaturan Grafik"),
                  status = "info", solidHeader = TRUE, width = 4,
                  selectInput("plot_type", "🎨 Pilih Jenis Grafik:",
                              choices = c("Histogram" = "histogram", 
                                          "Scatter Plot" = "scatter", 
                                          "Box Plot" = "boxplot", 
                                          "Bar Chart" = "bar",
                                          "Line Chart" = "line", 
                                          "Density Plot" = "density", 
                                          "Violin Plot" = "violin", 
                                          "Correlation Heatmap" = "heatmap")),
                  
                  # Dynamic inputs based on plot type
                  uiOutput("plot_inputs"),
                  
                  # Appearance settings
                  conditionalPanel(
                    condition = "input.plot_type != 'heatmap'",
                    hr(),
                    h5("🎨 Pengaturan Tampilan:"),
                    textInput("plot_title", "Judul Grafik:", value = ""),
                    textInput("plot_xlabel", "Label Sumbu X:", value = ""),
                    textInput("plot_ylabel", "Label Sumbu Y:", value = ""),
                    selectInput("plot_theme", "Tema Grafik:",
                                choices = c("Minimal" = "theme_minimal",
                                            "Classic" = "theme_classic",
                                            "Light" = "theme_light",
                                            "Dark" = "theme_dark"),
                                selected = "theme_minimal"),
                    selectInput("plot_color", "Warna Utama:",
                                choices = c("Biru" = "#2196F3", "Hijau" = "#4CAF50",
                                            "Merah" = "#F44336", "Ungu" = "#9C27B0",
                                            "Orange" = "#FF9800", "Teal" = "#009688"),
                                selected = "#2196F3")
                  ),
                  
                  actionButton("create_plot", "🎨 Buat Grafik", 
                               icon = icon("chart-bar"), class = "btn-primary")
                ),
                
                # Plot Output
                box(
                  title = tagList(icon("chart-bar", style = "color: #2196f3;"), "Output Grafik"),
                  status = "primary", solidHeader = TRUE, width = 8,
                  plotlyOutput("visual_plot", height = "500px"),
                  
                  # Plot interpretation
                  conditionalPanel(
                    condition = "input.plot_type != ''",
                    hr(),
                    h4("💡 Interpretasi Grafik:"),
                    htmlOutput("plot_interpretation")
                  )
                )
              )
      ),
      
      # === VISUALISASI PETA TAB ===
      tabItem(tabName = "visualisasi_peta",
              h2("🗺️ Visualisasi Peta Sebaran Data", style = "color: #263238; margin-bottom: 30px;"),
              
              # Download Section
              fluidRow(
                box(
                  title = tagList(icon("download", style = "color: #4caf50;"), "Download Peta"),
                  status = "success", solidHeader = TRUE, width = 12,
                  div(class = "download-section",
                      h4("🗺️ Download Peta dan Analisis"),
                      p("Unduh peta dan analisis spasial dalam berbagai format.", 
                        style = "color: #666; font-size: 16px;"),
                      div(class = "download-buttons",
                          downloadButton("download_peta_html", "🌐 Peta Interaktif (HTML)",
                                         class = "btn-info", icon = icon("globe")),
                          downloadButton("download_peta_word", "📋 Laporan Peta (Word)",
                                         class = "btn-success", icon = icon("file-word"))
                      )
                  )
                )
              ),
              
              # Summary Statistics
              fluidRow(
                box(
                  title = tagList(icon("chart-bar", style = "color: #00bcd4;"), "Ringkasan Statistik"),
                  status = "info", solidHeader = TRUE, width = 12,
                  uiOutput("peta_summary_stats")
                )
              ),
              
              fluidRow(
                # Map
                box(
                  title = tagList(icon("map", style = "color: #4caf50;"), "Peta Choropleth Interaktif"),
                  status = "success", solidHeader = TRUE, width = 9,
                  p("🗺️ Peta interaktif dengan multiple layer dan popup informatif. Klik pada wilayah untuk detail lengkap.", 
                    style = "color: #666; margin-bottom: 15px;"),
                  leafletOutput("peta_choropleth", height = "650px")
                ),
                
                # Map Controls
                box(
                  title = tagList(icon("sliders", style = "color: #00bcd4;"), "Kontrol Peta"),
                  status = "info", solidHeader = TRUE, width = 3,
                  selectInput(
                    inputId = "peta_filter_variable",
                    label = "🎯 Pilih Indikator:",
                    choices = c(
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
                    ),
                    selected = "total_population"
                  ),
                  hr(),
                  h5("✨ Fitur Peta:"),
                  tags$ul(
                    tags$li("🗺️ Multiple tile layers"),
                    tags$li("📊 Popup informatif"),
                    tags$li("🎯 Kategorisasi otomatis"),
                    tags$li("📏 Scale bar")
                  ),
                  
                  actionButton("update_peta", "🔄 Update Peta", 
                               icon = icon("refresh"), class = "btn-primary")
                )
              ),
              
              # Interpretation
              fluidRow(
                box(
                  title = tagList(icon("lightbulb", style = "color: #ff9800;"), "Analisis & Interpretasi"),
                  status = "warning", solidHeader = TRUE, width = 12,
                  uiOutput("peta_interpretation")
                )
              )
      ),
      
      # === UJI HOMOSKEDASTISITAS TAB ===
      tabItem(tabName = "uji_homogenitas",
              h2("🔬 Uji Homoskedastisitas (Breusch-Pagan)", style = "color: #263238; margin-bottom: 30px;"),
              
              # Download Section
              fluidRow(
                box(
                  title = tagList(icon("download", style = "color: #4caf50;"), "Download Hasil Uji"),
                  status = "success", solidHeader = TRUE, width = 12,
                  div(class = "download-section",
                      h4("🔍 Download Hasil Uji Homoskedastisitas"),
                      p("Unduh hasil uji dan interpretasi dalam format Word.", 
                        style = "color: #666; font-size: 16px;"),
                      div(class = "download-buttons",
                          downloadButton("download_homog_word", "📄 Laporan Uji (Word)",
                                         class = "btn-success", icon = icon("file-word"))
                      )
                  )
                )
              ),
              
              # Variable Selection
              fluidRow(
                box(
                  title = tagList(icon("cog", style = "color: #00bcd4;"), "Pilih Variabel untuk Model Uji"),
                  status = "info", solidHeader = TRUE, width = 12,
                  p("🎯 Pilih variabel untuk membentuk model regresi yang akan diuji homoskedastisitasnya.", 
                    style = "color: #666; margin-bottom: 20px;"),
                  fluidRow(
                    column(6,
                           selectInput("homog_bp_dep_var", "Variabel Dependen (Y):", choices = NULL)
                    ),
                    column(6,
                           selectInput("homog_bp_indep_vars", "Variabel Independen (X):", 
                                       choices = NULL, multiple = TRUE)
                    )
                  ),
                  actionButton("run_homog_bptest", "🚀 Jalankan Uji", 
                               icon = icon("play-circle"), class = "btn-primary")
                )
              ),
              
              # Results
              fluidRow(
                box(
                  title = tagList(icon("chart-line", style = "color: #2196f3;"), "Hasil Uji Homoskedastisitas"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("homog_bptest_output"),
                  hr(),
                  h4("💡 Interpretasi Hasil:"),
                  htmlOutput("homog_bptest_interpretation")
                )
              )
      ),
      
      # === UJI NORMALITAS TAB ===
      tabItem(tabName = "uji_normalitas",
              h2("📊 Uji Normalitas Residual Regresi (Shapiro-Wilk)", style = "color: #263238; margin-bottom: 30px;"),
              
              # Download Section
              fluidRow(
                box(
                  title = tagList(icon("download", style = "color: #4caf50;"), "Download Hasil Uji"),
                  status = "success", solidHeader = TRUE, width = 12,
                  div(class = "download-section",
                      h4("📊 Download Hasil Uji Normalitas"),
                      p("Unduh hasil uji dan interpretasi dalam format Word.", 
                        style = "color: #666; font-size: 16px;"),
                      div(class = "download-buttons",
                          downloadButton("download_normalitas_word", "📄 Laporan Uji (Word)",
                                         class = "btn-success", icon = icon("file-word"))
                      )
                  )
                )
              ),
              
              # Variable Selection
              fluidRow(
                box(
                  title = tagList(icon("cog", style = "color: #00bcd4;"), "Pilih Variabel untuk Model Regresi"),
                  status = "info", solidHeader = TRUE, width = 12,
                  p("🎯 Pilih variabel untuk membentuk model regresi yang residualnya akan diuji normalitasnya.", 
                    style = "color: #666; margin-bottom: 20px;"),
                  fluidRow(
                    column(6,
                           selectInput("norm_res_dep_var", "Variabel Dependen (Y):", choices = NULL)
                    ),
                    column(6,
                           selectInput("norm_res_indep_vars", "Variabel Independen (X):", 
                                       choices = NULL, multiple = TRUE)
                    )
                  ),
                  actionButton("run_norm_res_test", "🚀 Jalankan Uji", 
                               icon = icon("play-circle"), class = "btn-primary")
                )
              ),
              
              # Results
              fluidRow(
                box(
                  title = tagList(icon("chart-line", style = "color: #2196f3;"), "Hasil Uji Normalitas Residual"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("norm_res_output"),
                  hr(),
                  h4("💡 Interpretasi Hasil:"),
                  htmlOutput("norm_res_interpretation")
                )
              )
      ),
      
      # === UJI BEDA RATA-RATA TAB ===
      tabItem(tabName = "beda_rata",
              h2("📈 Uji Beda Rata-rata (Uji T)", style = "color: #263238; margin-bottom: 30px;"),
              
              # Download Section
              fluidRow(
                box(
                  title = tagList(icon("download", style = "color: #4caf50;"), "Download Hasil Uji"),
                  status = "success", solidHeader = TRUE, width = 12,
                  div(class = "download-section",
                      h4("📈 Download Hasil Uji T"),
                      p("Unduh hasil uji dan interpretasi dalam format Word.", 
                        style = "color: #666; font-size: 16px;"),
                      div(class = "download-buttons",
                          downloadButton("download_ttest_word", "📄 Laporan Uji T (Word)",
                                         class = "btn-success", icon = icon("file-word"))
                      )
                  )
                )
              ),
              
              # Test Settings
              fluidRow(
                box(
                  title = tagList(icon("cog", style = "color: #00bcd4;"), "Pengaturan Uji T"),
                  status = "info", solidHeader = TRUE, width = 12,
                  p("🎯 Pilih jenis uji T yang ingin dilakukan.", 
                    style = "color: #666; margin-bottom: 20px;"),
                  radioButtons("ttest_main_type", "Pilih Jenis Uji T:",
                               choices = c("Satu Sampel" = "one_sample", "Dua Sampel" = "two_sample"),
                               selected = "one_sample", inline = TRUE),
                  uiOutput("ttest_inputs_ui"),
                  actionButton("run_ttest", "🚀 Jalankan Uji T", 
                               icon = icon("play-circle"), class = "btn-primary")
                )
              ),
              
              # Results
              fluidRow(
                box(
                  title = tagList(icon("chart-line", style = "color: #2196f3;"), "Hasil Uji T"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("ttest_output"),
                  hr(),
                  h4("💡 Interpretasi Hasil:"),
                  htmlOutput("ttest_interpretation")
                )
              )
      ),
      
      # === UJI PROPORSI TAB ===
      tabItem(tabName = "proporsi",
              h2("📊 Uji Proporsi", style = "color: #263238; margin-bottom: 30px;"),
              
              # Download Section
              fluidRow(
                box(
                  title = tagList(icon("download", style = "color: #4caf50;"), "Download Hasil Uji"),
                  status = "success", solidHeader = TRUE, width = 12,
                  div(class = "download-section",
                      h4("📊 Download Hasil Uji Proporsi"),
                      p("Unduh hasil uji dan interpretasi dalam format Word.", 
                        style = "color: #666; font-size: 16px;"),
                      div(class = "download-buttons",
                          downloadButton("download_proporsi_word", "📄 Laporan Uji Proporsi (Word)",
                                         class = "btn-success", icon = icon("file-word"))
                      )
                  )
                )
              ),
              
              # Test Settings
              fluidRow(
                box(
                  title = tagList(icon("cog", style = "color: #00bcd4;"), "Pengaturan Uji Proporsi"),
                  status = "info", solidHeader = TRUE, width = 12,
                  p("🎯 Pilih jenis uji proporsi yang ingin dilakukan.", 
                    style = "color: #666; margin-bottom: 20px;"),
                  radioButtons("prop_main_type", "Pilih Jenis Uji Proporsi:",
                               choices = c("Satu Sampel" = "one_sample", "Dua Sampel" = "two_sample"),
                               selected = "one_sample", inline = TRUE),
                  uiOutput("prop_test_inputs_ui"),
                  actionButton("run_prop_test", "🚀 Jalankan Uji Proporsi", 
                               icon = icon("play-circle"), class = "btn-primary")
                )
              ),
              
              # Results
              fluidRow(
                box(
                  title = tagList(icon("chart-line", style = "color: #2196f3;"), "Hasil Uji Proporsi"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("prop_test_output"),
                  hr(),
                  h4("💡 Interpretasi Hasil:"),
                  htmlOutput("prop_test_interpretation")
                )
              )
      ),
      
      # === UJI VARIANS TAB ===
      tabItem(tabName = "varians",
              h2("📊 Uji Varians Dua Sampel (Uji F)", style = "color: #263238; margin-bottom: 30px;"),
              
              # Download Section
              fluidRow(
                box(
                  title = tagList(icon("download", style = "color: #4caf50;"), "Download Hasil Uji"),
                  status = "success", solidHeader = TRUE, width = 12,
                  div(class = "download-section",
                      h4("📊 Download Hasil Uji Varians"),
                      p("Unduh hasil uji dan interpretasi dalam format Word.", 
                        style = "color: #666; font-size: 16px;"),
                      div(class = "download-buttons",
                          downloadButton("download_varians_word", "📄 Laporan Uji Varians (Word)",
                                         class = "btn-success", icon = icon("file-word"))
                      )
                  )
                )
              ),
              
              # Test Settings
              fluidRow(
                box(
                  title = tagList(icon("cog", style = "color: #00bcd4;"), "Pengaturan Uji Varians"),
                  status = "info", solidHeader = TRUE, width = 12,
                  p("🎯 Uji F untuk membandingkan varians dua kelompok data.", 
                    style = "color: #666; margin-bottom: 20px;"),
                  uiOutput("variance_test_inputs_ui"),
                  actionButton("run_variance_test", "🚀 Jalankan Uji Varians",
                               icon = icon("play-circle"), class = "btn-primary")
                )
              ),
              
              # Results
              fluidRow(
                box(
                  title = tagList(icon("chart-line", style = "color: #2196f3;"), "Hasil Uji Varians"),
                  status = "primary", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("variance_test_output"),
                  hr(),
                  h4("💡 Interpretasi Hasil:"),
                  htmlOutput("variance_test_interpretation")
                )
              )
      ),
      
      # === UJI ANOVA TAB ===
      tabItem(tabName = "anova",
              h2("🔬 Uji ANOVA (Analysis of Variance)", style = "color: #263238; margin-bottom: 30px;"),
              
              # Download Section
              fluidRow(
                box(
                  title = tagList(icon("download", style = "color: #4caf50;"), "Download Hasil ANOVA"),
                  status = "success", solidHeader = TRUE, width = 12,
                  div(class = "download-section",
                      h4("📊 Download Hasil ANOVA"),
                      p("Unduh hasil uji ANOVA, plot, dan interpretasi dalam berbagai format.", 
                        style = "color: #666; font-size: 16px;"),
                      div(class = "download-buttons",
                          downloadButton("download_anova_csv", "📊 Tabel ANOVA (CSV)",
                                         class = "btn-info", icon = icon("file-csv")),
                          downloadButton("download_anova_plots_pdf", "📈 Plot Diagnostik (PDF)",
                                         class = "btn-warning", icon = icon("chart-line")),
                          downloadButton("download_anova_word", "📄 Laporan Lengkap (Word)",
                                         class = "btn-success", icon = icon("file-word"))
                      )
                  )
                )
              ),
              
              # Test Settings
              fluidRow(
                box(
                  title = tagList(icon("cog", style = "color: #00bcd4;"), "Pengaturan Uji ANOVA"),
                  status = "info", solidHeader = TRUE, width = 12,
                  p("🎯 ANOVA digunakan untuk menguji apakah terdapat perbedaan rata-rata yang signifikan antara kelompok-kelompok.", 
                    style = "color: #666; margin-bottom: 20px;"),
                  
                  # ANOVA type selection
                  radioButtons("anova_type", "Pilih Jenis ANOVA:",
                               choices = c("ANOVA Satu Arah (One-Way)" = "oneway",
                                           "ANOVA Dua Arah (Two-Way)" = "twoway"),
                               selected = "oneway", inline = TRUE),
                  
                  fluidRow(
                    column(6,
                           selectInput("anova_dep_var", "Variabel Dependen (Numerik):", choices = NULL)
                    ),
                    column(6,
                           selectInput("anova_factor1", "Faktor 1 (Kelompok):", choices = NULL)
                    )
                  ),
                  
                  # Second factor for two-way ANOVA
                  conditionalPanel(
                    condition = "input.anova_type == 'twoway'",
                    fluidRow(
                      column(6,
                             selectInput("anova_factor2", "Faktor 2 (Kelompok):", choices = NULL)
                      ),
                      column(6,
                             checkboxInput("anova_interaction", "Sertakan Interaksi Antar Faktor", value = TRUE)
                      )
                    )
                  ),
                  
                  checkboxInput("anova_post_hoc", "Lakukan Uji Post-Hoc (Tukey HSD)", value = TRUE),
                  actionButton("run_anova", "🚀 Jalankan Uji ANOVA", 
                               icon = icon("play-circle"), class = "btn-primary")
                )
              ),
              
              # Results Tabs
              fluidRow(
                tabBox(
                  id = "anova_output_tabs",
                  width = 12,
                  tabPanel(
                    title = "📊 Hasil Uji ANOVA", icon = icon("table"),
                    verbatimTextOutput("anova_output"),
                    hr(),
                    h4("💡 Interpretasi Hasil:"),
                    htmlOutput("anova_interpretation")
                  ),
                  tabPanel(
                    title = "🔍 Uji Post-Hoc (Tukey HSD)", icon = icon("search-plus"),
                    verbatimTextOutput("posthoc_output"),
                    hr(),
                    h4("💡 Interpretasi Post-Hoc:"),
                    htmlOutput("posthoc_interpretation")
                  ),
                  tabPanel(
                    title = "📈 Plot Diagnostik", icon = icon("chart-line"),
                    plotOutput("anova_diagnostic_plots", height = "600px")
                  ),
                  tabPanel(
                    title = "📊 Visualisasi Means", icon = icon("chart-bar"),
                    plotOutput("anova_means_plot", height = "500px")
                  )
                )
              )
      ),
      
      # === REGRESI LINEAR BERGANDA TAB ===
      tabItem(tabName = "regresi",
              h2("📈 Regresi Linear Berganda & Uji Asumsi Klasik", style = "color: #263238; margin-bottom: 30px;"),
              
              # Download Section
              fluidRow(
                box(
                  title = tagList(icon("download", style = "color: #4caf50;"), "Download Hasil Regresi"),
                  status = "success", solidHeader = TRUE, width = 12,
                  div(class = "download-section",
                      h4("📈 Download Hasil Regresi"),
                      p("Unduh hasil analisis regresi, plot diagnostik, dan interpretasi dalam berbagai format.", 
                        style = "color: #666; font-size: 16px;"),
                      div(class = "download-buttons",
                          downloadButton("download_regresi_csv", "📊 Koefisien Regresi (CSV)",
                                         class = "btn-info", icon = icon("file-csv")),
                          downloadButton("download_regresi_plots_pdf", "📈 Plot Diagnostik (PDF)",
                                         class = "btn-warning", icon = icon("chart-line")),
                          downloadButton("download_regresi_word", "📄 Laporan Lengkap (Word)",
                                         class = "btn-success", icon = icon("file-word"))
                      )
                  )
                )
              ),
              
              # Variable Selection
              fluidRow(
                box(
                  title = tagList(icon("cog", style = "color: #00bcd4;"), "Pilih Variabel untuk Model Regresi"),
                  status = "info", solidHeader = TRUE, width = 12,
                  p("🎯 Pilih variabel dependen dan independen untuk analisis regresi linear berganda.", 
                    style = "color: #666; margin-bottom: 20px;"),
                  fluidRow(
                    column(6,
                           selectInput("dep_var_reg", "Variabel Dependen (Y):", choices = NULL)
                    ),
                    column(6,
                           selectInput("indep_vars_reg", "Variabel Independen (X):", 
                                       choices = NULL, multiple = TRUE)
                    )
                  ),
                  actionButton("run_regression", "🚀 Jalankan Regresi & Uji Asumsi", 
                               icon = icon("play-circle"), class = "btn-primary")
                )
              ),
              
              # Results Tabs
              fluidRow(
                tabBox(
                  id = "reg_output_tabs",
                  width = 12,
                  tabPanel(
                    title = "📊 Ringkasan & Interpretasi Model", icon = icon("list-alt"),
                    verbatimTextOutput("lm_summary"),
                    hr(),
                    h4("💡 Interpretasi Model Regresi:"),
                    htmlOutput("model_interpretation"),
                    
                    # Robust Regression Comparison
                    hr(style = "border-top: 2px solid #ccc;"),
                    h3("🔄 Perbandingan dengan Regresi Robust", align = "center"),
                    p("Regresi Robust digunakan untuk melihat hasil model yang tidak terlalu sensitif terhadap data pencilan (outliers). Jika koefisiennya sangat berbeda dengan model OLS di atas, kemungkinan ada outliers yang berpengaruh kuat pada model Anda.", 
                      style = "text-align: justify; color: #666;"),
                    verbatimTextOutput("rlm_summary"),
                    h4("💡 Interpretasi Model Regresi Robust:"),
                    htmlOutput("rlm_interpretation")
                  ),
                  tabPanel(
                    title = "✅ Uji Asumsi Klasik", icon = icon("check-double"),
                    h4("1️⃣ Uji Normalitas Residual (Shapiro-Wilk)"),
                    verbatimTextOutput("normality_test_output"),
                    htmlOutput("normality_interpretation"),
                    hr(),
                    h4("2️⃣ Uji Heteroskedastisitas (Breusch-Pagan)"),
                    verbatimTextOutput("homoscedasticity_test_output"),
                    htmlOutput("homoscedasticity_interpretation"),
                    hr(),
                    h4("3️⃣ Uji Autokorelasi (Durbin-Watson)"),
                    verbatimTextOutput("autocorrelation_test_output"),
                    htmlOutput("autocorrelation_interpretation"),
                    hr(),
                    h4("4️⃣ Uji Multikolinearitas (VIF)"),
                    verbatimTextOutput("multicollinearity_test_output"),
                    htmlOutput("multicollinearity_interpretation")
                  ),
                  tabPanel(
                    title = "📈 Plot Diagnostik", icon = icon("chart-area"),
                    plotOutput("diagnostic_plots", height = "600px"),
                    hr(),
                    h4("💡 Interpretasi Plot Diagnostik:"),
                    htmlOutput("diagnostic_plots_interpretation")
                  )
                )
              )
      )
    )
  )
)
