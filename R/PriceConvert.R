convert_price <- function(price_range) {
  numeric_values <- stringr::str_extract(price_range, "\\d+")
  as.numeric(numeric_values[!is.na(numeric_values)])
}
