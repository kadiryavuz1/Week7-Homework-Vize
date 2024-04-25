library(testthat)
library(DBI)

# Eksik değerleri doldurma işlevini test etmek için bir test oluşturalım
test_that("Eksik değerlerin doğru bir şekilde doldurulduğunu kontrol edin", {
  # Örnek bir veri seti oluşturalım (örneğin bir SQLite veritabanından çekilmiş gibi)
  sample_data <- data.frame(
    Date = c("2023-04-24", "2023-04-25", "2023-04-26", "2023-04-27", "2023-04-28"),
    Open = c(27591.73, 27514.87, NA, 28428.46, 29481.01),
    High = c(27979.98, NA, 29995.84, 29871.55, 29572.79),
    Low = c(27070.85, 27207.93, 27324.55, NA, 28929.61),
    Close = c(27525.34, 28307.60, 28422.70, 29473.79, 29340.26),
    Adj.Close = c(27525.34, 28307.60, 28422.70, 29473.79, 29340.26),
    Volume = c(17703288330, 17733373139, 31854242019, NA, 17544464887)
  )

  # Örnek dataframe'i SQLite veritabanına aktar
  db_path <- tempfile(fileext = ".sqlite")
  table_name <- "sample_table"
  write.csv(sample_data, "sample_data.csv", row.names = FALSE)
  import_csv_to_sqlite(db_path, "sample_data.csv", table_name)

  # Eksik değerlerin doldurulduğunu kontrol et
  filled_data <- fetch_and_fill_na(db_path, table_name)

  # Eksik değerlerin dolu olup olmadığını kontrol et
  expect_false(any(is.na(filled_data)),
               info = "Eksik değerlerin doldurulduğunu kontrol edin.")

  # Eksik değerlerin doğru bir şekilde doldurulduğunu kontrol et
  # İlk satırı kontrol etmeye gerek yok, çünkü ilk satırı doldurmak için bir önceki satır yoktur
  for (i in 2:(nrow(filled_data) - 1)) {
    for (j in 2:ncol(filled_data)) {
      # Eksik bir değer varsa, bir önceki ve bir sonraki satırın ortalaması ile doldurulduğunu kontrol edin
      if (is.na(sample_data[i, j])) {
        expected_value <- mean(c(sample_data[i-1, j], sample_data[i+1, j]), na.rm = TRUE)
        expect_equal(filled_data[i, j], expected_value,
                     info = paste("Eksik değerin doğru bir şekilde doldurulduğunu kontrol edin (satır:", i, ", sütun:", j, ")"))
      }
    }
  }
  file.remove("sample_data.csv")
})
