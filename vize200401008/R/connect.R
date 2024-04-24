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


