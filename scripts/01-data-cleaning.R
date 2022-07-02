# Packages and data -------------------------------------------------------
library(tidyverse)
library(rvest)
library(tidytext)
library(hunspell)

witcher <- read_file("data-raw/witcher.txt")

# Splitting text into books -----------------------------------------------
books <- as.list(str_split(witcher, pattern = "---", simplify = TRUE))
books <- books[-1]

# Splitting books into chapters -------------------------------------------
books <- map(books, ~as.list(str_split(.x, pattern = "&&&", simplify = TRUE)))

# Naming books ------------------------------------------------------------
book_names <- map_chr(books, ~.x[[1]])
book_names <- str_replace_all(book_names, pattern = "\\n", "")
book_names <- str_squish(book_names)
names(books) <- book_names

# Naming chapters ---------------------------------------------------------
books <- map(books, ~.x[-1]) # This is just name of the book

chapter_names <- map(books, ~map(.x, ~str_extract(.x, pattern = ".+\\n\\n")))
chapter_names <- map(chapter_names, ~map(.x, ~str_replace_all(.x, pattern = "\\n", "")))

books <- map2(.x = books, .y = chapter_names,
              ~setNames(.x, .y))


# Cleanining text ---------------------------------------------------------
# The chapter names are still in the text, let's drop them
books <- map(books, ~map(.x, ~str_replace(.x, pattern = ".+\\n\\n", replacement = "")))

# sometimes two "new line" symbols appear in the text in the text, we delete them
books <- map(books, ~map(.x, ~str_replace_all(.x, pattern = "\\n\\n", replacement = " ")))


# Tokenizing --------------------------------------------------------------
books <- books %>% 
  map(~map(., as_tibble)) %>% 
  map(~map(., unnest_tokens, input = "value", output = "word")) %>% 
  map(bind_rows, .id = "chapters") %>% 
  bind_rows(.id = "book")

books <- books %>% 
  mutate(word = str_replace_all(word, "’", "'"),
         chapters = str_to_title(chapters)) %>% 
  anti_join(stop_words, "word")

books <- books %>% 
  mutate(stem = hunspell_stem(word, dict = dictionary("en_GB")),
         stem = map_chr(stem, ~if_else(length(.) > 0,
                                   .x[1],
                                   NA_character_)))

# Scrapping character names -----------------------------------------------

characters_pages <- paste0("https://witcher.fandom.com/wiki/Category:",
                           str_replace_all(book_names, pattern = " ", replacement = "_"),
                           "_characters")

character_pages <- map(characters_pages, read_html)
names(character_pages) <- book_names

character_names <- character_pages %>%
  map(html_elements, ".category-page__member-link") %>% 
  map(html_text2) %>% 
  map(as_tibble) %>% 
  bind_rows(.id = "book")

character_names <- character_names %>% 
  mutate(first_name = str_extract(value, "^[:alpha:]+")) %>% 
  rename(full_name = value) %>% 
  distinct(full_name, .keep_all = TRUE)

# Data export -------------------------------------------------------------
write_csv(character_names, "data-processed/character-names.csv")
write_csv(books, "data-processed/witcher-tokenized.csv")
