# install.packages("tesseract")
# install.packages("magick")

library(magick)
library(tesseract)
library(tidyverse)

# Конвертация PDF в изображения
my_file <- list.files("./ocr", pattern = "pdf", full.names = TRUE)[3]
pdf_convert(my_file, dpi = 300)

# Путь к изображениям
my_images <- list.files("./ocr", pattern = "png", full.names = TRUE)

# Получаем данные об OCR (для определения позиций текста)
ocr_data <- ocr_data(my_images[1], engine = tesseract("rus"))

# обрезка по прямоугольнику задаётся строкой вида "W x H + X + Y" 
# (ширина, высота, смещение от левого верхнего угла). 
# Из bbox берём x1,y1,x2,y2 → считаем W=x2−x1, H=y2−y1 и обрезаем.

boxes <-  ocr_data |> 
  separate_wider_delim(bbox, ",", names = c("x1","y1","x2","y2"))  |> 
  mutate(across(c(x1,y1,x2,y2), as.integer),
         # необязательно: небольшой отступ вокруг слова
         pad = 2L,
         x1 = pmax(0L, x1 - pad),
         y1 = pmax(0L, y1 - pad),
         x2 = x2 + pad,
         y2 = y2 + pad,
         w = pmax(1L, x2 - x1),
         h = pmax(1L, y2 - y1),
         geom = sprintf("%dx%d+%d+%d", w, h, x1, y1))

# geom = sprintf("%dx%d+%d+%d", w, h, x1, y1)` создаёт строку геометрии для ImageMagick в формате:
#   
#   **"ширина x высота + смещение_X + смещение_Y"**
# 
#   - `sprintf()` — функция форматирования строк в R (как printf в других языках)
#   - `%d` — placeholder для целого числа
#   - `x` — буквальный символ "x" между шириной и высотой  
#   - `+` — буквальные символы "+" перед смещениями
# 
# **Пример:**
#   Если w=100, h=50, x1=20, y1=30, то получится строка `"100x50+20+30"`
# 
# Это означает:
#   - Вырезать прямоугольник размером **100×50 пикселей**
#   - Начиная с точки **(20, 30)** от левого верхнего угла исходного изображения
#   


# Обрезка слов
crops <- map(boxes$geom, ~ image_crop(image1, .x))

# Сохранить по файлам
dir.create("words", showWarnings = FALSE)

walk2(crops, seq_along(crops),
      ~ image_write(.x, sprintf("words/word_%03d.png", .y)))

