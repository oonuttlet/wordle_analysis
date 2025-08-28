library(rvest)
library(xml2)
library(tidyverse)
library(ngramr)
library(zoo)
library(ggfun)
library(ggrepel)
options(scipen = 999)

url <- "https://www.fiveforks.com/wordle/"

# This XPath selects the first text() node that is an immediate sibling
# following a <br> tag that is inside the element with id="chronlist".
xpath_selector <- '//*[@id="chronlist"]/br/following-sibling::text()[1]'

# 1. Read the page and select the specific text nodes
text_nodes <- rvest::read_html(url) |>
  html_nodes(xpath = xpath_selector)

# 2. Extract the raw text from those nodes
raw_text <- html_text(text_nodes)

# 3. Clean the text by trimming whitespace
cleaned_text <- str_trim(raw_text)

final_string <- paste(cleaned_text, collapse = "\n")

# 5. Use read.table to parse the string into a dataframe
wordle_df <- read.table(text = final_string, 
                        header = FALSE, 
                        col.names = c("Word", "Number", "Date")) |>
  mutate(Date = as_date(Date, format = "%m/%d/%y"),
         grp = ceiling(row_number()/12))

prop_from_scrabble <- words::words |> 
  filter(word_length == 5) |>
  paste(collapse = "") |>
  str_split(pattern = "") |>
  table() |>
  as.data.frame() |>
  filter(toupper(Var1) %in% LETTERS) |>
  mutate(scrabble_prop = Freq / sum(Freq),
         Var1 = toupper(Var1)) |>
  rename(Letter = Var1)

letter_frequencies <- wordle_df$Word |>
  paste(collapse = "") |>
  str_split(pattern = "") |>
  table() |>
  as.data.frame() |>
  mutate(prop = Freq / sum(Freq)) |>
  rename(Letter = Var1)

difficulties <- letter_frequencies |>
  left_join(prop_from_scrabble, by = "Letter") |>
  select(-starts_with("Freq"))

wordle_df_letters <- wordle_df |>
  separate_longer_position(cols = Word,
                           width = 1) |>
  left_join(difficulties, by = join_by("Word" == "Letter")) |>
  group_by(Number) |>
  summarize(avg_prop = mean(prop),
            avg_scrabble_prop = mean(scrabble_prop))

wordle_df_difficulty <- wordle_df |>
  left_join(wordle_df_letters, by = "Number") |>
  pivot_longer(cols = c("avg_prop", "avg_scrabble_prop")) |>
  mutate(`Letter Source` = case_when(name == 'avg_prop' ~ "Wordle",
                          TRUE ~ "Scrabble"))

ggplot(wordle_df_difficulty) +
  geom_point(data = filter(wordle_df_difficulty, name == "avg_prop"), aes(x = Date, y = value), color = "grey20", alpha = 0.3) +
  geom_smooth(aes(x = Date, y = value, group = `Letter Source`, color = `Letter Source`), method = "loess") +
  labs(title = "Difficulty of Wordle words over time",
       subtitle = "As defined by letter frequency",
       y = "Average Letter Proportion") +
  geom_label_repel(data = filter(wordle_df_difficulty, name == "avg_prop" & (value > 0.082 | value < 0.03)),
                   aes(x = Date, y = value, label = Word)) +
  theme_minimal()

