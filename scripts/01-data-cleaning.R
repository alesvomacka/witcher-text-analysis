# Packages and data -------------------------------------------------------
library(tidyverse)

witcher <- read_file("data-raw/witcher.txt")

# Splitting text into books -----------------------------------------------
books <- as.list(str_split(witcher, pattern = "---", simplify = TRUE))
books <- books[-1]

# Splitting books into chapters -------------------------------------------
books <- map(books, ~as.list(str_split(.x, pattern = "&&&", simplify = TRUE)))

# Naming books ------------------------------------------------------------
book_names <- map_chr(books, ~.x[[1]])
book_names <- str_replace_all(book_names, pattern = "\\n", "")
book_names <- str_to_lower(book_names)
book_names <- str_replace_all(book_names, pattern = " ", replacement = "_")
names(books) <- book_names

# Naming chapters ---------------------------------------------------------
books <- map(books, ~.x[-1]) # This is just name of the book

chapter_names <- map(books, ~map(.x, ~str_extract(.x, pattern = ".+\\n\\n")))
chapter_names <- map(chapter_names, ~map(.x, ~str_replace_all(.x, pattern = "\\n", "")))
chapter_names <- map(chapter_names, ~map(.x, str_to_lower))
chapter_names <- map(chapter_names, ~map(.x, ~str_replace_all(.x, pattern = " ", replacement = "_")))

books <- map2(.x = books, .y = chapter_names,
              ~setNames(.x, .y))

# Text cleaning -----------------------------------------------------------
# The chapter names are still in the text, let's drop them
books <- map(books, ~map(.x, ~str_replace(.x, pattern = ".+\\n\\n", replacement = "")))

