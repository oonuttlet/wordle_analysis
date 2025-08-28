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

letter_frequencies <- wordle_df$Word |>
  paste(collapse = "") |>
  str_split(pattern = "") |>
  table() |>
  as.data.frame() |>
  mutate(prop = Freq / sum(Freq)) |>
  rename(Letter = Var1)

wordle_df_letters <- wordle_df |>
  separate_longer_position(cols = Word,
                           width = 1) |>
  left_join(letter_frequencies, by = join_by("Word" == "Letter")) |>
  group_by(Number) |>
  summarize(avg_prop = mean(prop))

wordle_df_difficulty <- wordle_df |>
  left_join(wordle_df_letters, by = "Number")

ggplot(wordle_df_difficulty) +
  geom_point(aes(x = Date, y = avg_prop)) +
  geom_smooth(aes(x = Date, y = avg_prop), method = "loess") +
  labs(title = "Difficulty of Wordle words over time",
       subtitle = "As defined by letter frequency",
       y = "Average Letter Proportion") +
  geom_label_repel(data = filter(wordle_df_difficulty, avg_prop > 0.082 | avg_prop < 0.03),
                   aes(x = Date, y = avg_prop, label = Word)) +
  theme_minimal()
