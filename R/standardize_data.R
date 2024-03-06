standardize_data = function(data, subset = TRUE) {
  sample_rate = attr(data, "sample_rate")
  HEADER_TIMESTAMP = TIME = HEADER_TIME_STAMP = X = Y = Z = NULL
  rm(list = c("HEADER_TIMESTAMP", "HEADER_TIME_STAMP", "X", "Y", "Z",
              "TIME"))
  if (is.list(data) && !is.data.frame(data)) {
    if (!"data" %in% names(data)) {
      stop(paste0(
        "data seems to be a list/AccData, but no `data` element, ",
        "pass in a data.frame"))
    } else {
      data = data$data
    }
  }
  if (is.matrix(data)) {
    if (is.numeric(data)) {
      stopifnot(ncol(data) == 3)
      data = as.data.frame(data)
      colnames(data) = c("X", "Y", "Z")
    } else {
      stop("data is a matrix and cannot be coerced to necessary structure")
    }
  }
  # uppercase
  colnames(data) = toupper(colnames(data))
  cn = colnames(data)
  if ("TIME" %in% cn && !"HEADER_TIMESTAMP" %in% cn) {
    data = data %>%
      dplyr::rename(HEADER_TIMESTAMP = TIME)
  }
  if ("HEADER_TIME_STAMP" %in% cn && !"HEADER_TIMESTAMP" %in% cn) {
    data = data %>%
      dplyr::rename(HEADER_TIMESTAMP = HEADER_TIME_STAMP)
  }
  if ("HEADER_TIMESTAMP" %in% colnames(data)) {
    if (is.unsorted(data$HEADER_TIMESTAMP)) {
      stop("Time in data must be sorted before running!")
    }
  }
  if (subset) {
    # time for this package
    data = data %>%
      dplyr::select(time = dplyr::any_of("HEADER_TIMESTAMP"), X, Y, Z)
  }
  stopifnot(all(c("X", "Y", "Z") %in% colnames(data)))
  attr(data, "sample_rate") = sample_rate

  data
}
