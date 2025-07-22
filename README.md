
### 📋 Alur Penggunaan Aplikasi

Untuk mendapatkan hasil analisis yang maksimal, disarankan untuk mengikuti alur kerja berikut:

1.  **Beranda**: Mulai di sini untuk memahami konteks proyek dan melihat **metadata** dari semua variabel yang tersedia dalam dataset. Ini akan memberikan gambaran tentang data yang akan Anda olah.

2.  **Manajemen Data**:
    * Lihat data mentah pada tabel di tab ini.
    * **Langkah Penting**: Jika Anda berencana melakukan analisis yang membutuhkan variabel kelompok (seperti Uji ANOVA), gunakan fitur **"Pengkategorian Variabel Numerik"** di sini terlebih dahulu. Fitur ini akan membuat kolom kategorikal baru dari data numerik Anda (misalnya, mengubah `POPULATION` menjadi kategori "Rendah", "Sedang", "Tinggi").

3.  **Eksplorasi Data**:
    * **Statistik Deskriptif**: Pilih variabel numerik untuk melihat ringkasan statistik lengkap dan memahami karakteristik dasar dari data Anda.
    * **Visualisasi Grafik**: Buat berbagai macam plot (Histogram, Scatter Plot, dll.) untuk melihat pola, sebaran, dan hubungan antar variabel secara visual.
    * **Visualisasi Peta**: Gunakan filter untuk memilih indikator dan lihat bagaimana data terdistribusi secara geografis di seluruh Indonesia.

4.  **Uji Statistik Inferensia & Asumsi**:
    * Gunakan tab **"Statistik Inferensia"** untuk melakukan uji hipotesis spesifik seperti Uji T, Uji Proporsi, atau Uji ANOVA. Pastikan Anda sudah membuat variabel kategorikal di langkah ke-2 jika ingin melakukan ANOVA.
    * Tab **"Uji Asumsi"** dapat digunakan untuk melakukan uji asumsi secara terpisah jika diperlukan.

5.  **Regresi Linear Berganda**:
    * Ini adalah tahap pemodelan utama. Pilih satu variabel dependen (Y) dan beberapa variabel independen (X).
    * Aplikasi akan secara otomatis menampilkan hasil regresi OLS, perbandingan dengan regresi robust, interpretasi lengkap, serta semua hasil uji asumsi klasik yang relevan dengan model Anda.
