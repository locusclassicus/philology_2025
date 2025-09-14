# Функция crop_words: режет изображение на слова и сохраняет их как отдельные файлы
crop_words <- function(image_path,
                       out_dir = "words",
                       lang = "rus",
                       pad = 2L,
                       overwrite = FALSE) {
  
  # 1) Проверка и установка пакетов
  req_pkgs <- c("magick", "tesseract", "tidyverse")
  to_install <- req_pkgs[!req_pkgs %in% installed.packages()[, "Package"]]
  if (length(to_install)) install.packages(to_install, dependencies = TRUE)
  suppressPackageStartupMessages({
    library(magick)
    library(tesseract)
    library(tidyverse)
  })
  
  # 2) Валидация аргументов
  stopifnot(file.exists(image_path))
  stopifnot(is.numeric(pad), length(pad) == 1, pad >= 0)
  
  # 3) Подготовка
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Определяем стартовый индекс в зависимости от overwrite
  existing <- list.files(out_dir, pattern = "^word_[0-9]+\\.png$", full.names = FALSE)
  if (overwrite) {
    if (length(existing)) {
      suppressWarnings(file.remove(file.path(out_dir, existing)))
    }
    next_idx <- 1L
  } else {
    # продолжаем нумерацию после максимального существующего индекса
    if (length(existing)) {
      idx <- suppressWarnings(as.integer(sub("^word_([0-9]+)\\.png$", "\\1", existing)))
      idx <- idx[!is.na(idx)]
      next_idx <- if (length(idx)) max(idx) + 1L else 1L
    } else {
      next_idx <- 1L
    }
  }
  
  # 4) Чтение изображения и OCR-данных (с координатами слов)
  img <- image_read(image_path)
  eng <- tesseract(lang = lang)
  
  ocr_df <- tesseract::ocr_data(image_path, engine = eng)
  
  if (nrow(ocr_df) == 0 || !("bbox" %in% names(ocr_df)) || all(is.na(ocr_df$bbox))) {
    message("OCR не нашёл слов на изображении.")
    return(tibble::tibble(
      file = character(),
      word = character(),
      confidence = numeric(),
      x1 = integer(), y1 = integer(), x2 = integer(), y2 = integer(),
      w = integer(), h = integer()
    ))
  }
  
  # 5) Подготовка боксов
  boxes <- ocr_df |>
    dplyr::filter(!is.na(.data$word), nzchar(trimws(.data$word))) |>
    # В tidyselect-контексте используем имя колонки, а не .data$
    tidyr::separate_wider_delim(bbox, ",", names = c("x1", "y1", "x2", "y2")) |>
    dplyr::mutate(
      dplyr::across(c(x1, y1, x2, y2), ~ suppressWarnings(as.integer(.x)))
    ) |>
    tidyr::drop_na(x1, y1, x2, y2) |>
    dplyr::mutate(
      pad = as.integer(pad),
      x1 = pmax(0L, x1 - pad),
      y1 = pmax(0L, y1 - pad),
      x2 = x2 + pad,
      y2 = y2 + pad,
      w = pmax(1L, x2 - x1),
      h = pmax(1L, y2 - y1),
      geom = sprintf("%dx%d+%d+%d", w, h, x1, y1)
    )
  
  if (nrow(boxes) == 0) {
    message("После фильтрации не осталось валидных боксов.")
    return(tibble::tibble(
      file = character(),
      word = character(),
      confidence = numeric(),
      x1 = integer(), y1 = integer(), x2 = integer(), y2 = integer(),
      w = integer(), h = integer()
    ))
  }
  
  # 6) Кадрирование
  crops <- purrr::map(boxes$geom, ~ magick::image_crop(img, .x))
  
  # 7) Формирование путей с учётом next_idx
  n <- length(crops)
  idx_seq <- seq.int(from = next_idx, length.out = n)
  paths <- sprintf(file.path(out_dir, "word_%03d.png"), idx_seq)
  
  # 8) Защита от перезаписи, если overwrite = FALSE
  if (!overwrite) {
    exists <- file.exists(paths)
    if (any(exists)) {
      stop(
        "Целевые файлы уже существуют: ",
        paste(basename(paths[exists]), collapse = ", "),
        "\nЛибо установите overwrite = TRUE, либо очистите каталог/измените out_dir."
      )
    }
  }
  
  # 9) Сохранение
  purrr::walk2(crops, paths, magick::image_write)
  
  # 10) Возвращаем таблицу с метаданными
  tibble::tibble(
    file = paths,
    word = boxes$word,
    confidence = dplyr::coalesce(boxes$confidence, NA_real_),
    x1 = boxes$x1, y1 = boxes$y1, x2 = boxes$x2, y2 = boxes$y2,
    w = boxes$w, h = boxes$h
  )
}

# Пример использования:
# res <- crop_words("path/to/image.png", out_dir = "words", lang = "rus", pad = 2)
# View(res)