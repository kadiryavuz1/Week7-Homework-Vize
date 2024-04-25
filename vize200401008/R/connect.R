#' CSV dosyasını SQLite veritabanına aktarır
#'
#' @param db_path Veritabanı dosyasının yolunu belirtir.
#' @param csv_file CSV dosyasının yolu.
#' @param table_name SQLite'daki tablo ismi.
#' @export
import_csv_to_sqlite <- function(db_path, csv_file, table_name) {
  # Veritabanı bağlantısını kur
  con <- connect_to_database(db_path)

  # CSV dosyasını dataframe olarak oku
  dataframe <- read.csv(csv_file)

  # Dataframe'i SQLite veritabanına tablo olarak yaz
  dbWriteTable(con, table_name, dataframe, overwrite = TRUE, row.names = FALSE)

  # Bağlantıyı kapat
  dbDisconnect(con)
}
#' Veritabanı bağlantısını kurar ve bağlantı nesnesini döndürür
#'
#' @param db_path Veritabanı dosyasının yolunu belirtir.
#' @return Veritabanı bağlantısı nesnesi.
#' @export
connect_to_database <- function(db_path) {
  library(RSQLite)
  con <- dbConnect(SQLite(), dbname = db_path)
  return(con)
}

#' Veritabanından veri çeken ve eksik değerleri bir önceki ve bir sonraki satırın ortalaması ile dolduran fonksiyon
#'
#' @param db_path SQLite veritabanı dosyasının yolu.
#' @param table_name Çekilecek veritabanı tablosunun adı.
#' @return Düzeltilmiş veri çerçevesi.
#' @export
fetch_and_fill_na <- function(db_path, table_name) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  # Veritabanından verileri çek
  data <- DBI::dbReadTable(con, table_name)

  # Eksik değerleri bir önceki ve bir sonraki satırın ortalaması ile doldur
  for (i in 1:ncol(data)) {
    for (j in 2:(nrow(data) - 1)) {
      if (is.na(data[j, i])) {
        data[j, i] <- mean(c(data[j-1, i], data[j+1, i]), na.rm = TRUE)
      }
    }
  }

  # Düzeltmeleri veritabanına yaz
  DBI::dbWriteTable(con, table_name, data, overwrite = TRUE)

  DBI::dbDisconnect(con)
  return(data)
}

#' Veritabanından veri çeken ve eksik değerleri bir önceki ve bir sonraki satırın ortalaması ile dolduran fonksiyon
#'
#' @param db_path SQLite veritabanı dosyasının yolu.
#' @param table_name Çekilecek veritabanı tablosunun adı.
#' @return Düzeltilmiş veri çerçevesi.
#' @export
fetch_and_fill_na <- function(db_path, table_name) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  # Veritabanından verileri çek
  data <- DBI::dbReadTable(con, table_name)

  # Eksik değerleri bir önceki ve bir sonraki satırın ortalaması ile doldur
  for (i in 1:ncol(data)) {
    for (j in 2:(nrow(data) - 1)) {
      if (is.na(data[j, i])) {
        data[j, i] <- mean(c(data[j-1, i], data[j+1, i]), na.rm = TRUE)
      }
    }
  }

  # Düzeltmeleri veritabanına yaz
  DBI::dbWriteTable(con, table_name, data, overwrite = TRUE)

  DBI::dbDisconnect(con)
  return(data)
}

