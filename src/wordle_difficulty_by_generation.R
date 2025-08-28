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

wordle_df_split <- wordle_df |>
  group_by(grp) |>
  group_split()

aa <- lapply(wordle_df_split, \(x) ngramr::ngram(x$Word, year_start = 1920, year_end = 2022, smoothing = 3))
bb <- data.table::rbindlist(aa) |> as.data.frame() |>
  filter(Year >= 1920) |>
  group_by(Phrase) |>
  mutate(generation = case_when(
    between(Year, 1920, 1947) ~ "Greatest",
    between(Year, 1948, 1965) ~ "Silent",
    between(Year, 1966, 1984) ~ "Boomers",
    between(Year, 1985, 2000) ~ "Gen X",
    between(Year, 2001, 2016) ~ "Millenial",
    between(Year, 2017, 2022) ~ "Gen Z"
  ))


cc <- bb |>
  group_by(Phrase, generation) |>
  summarize(avg_freq = mean(Frequency),
            max_freq = max(Frequency),
            max_yr = Year[which.max(Frequency)],
            diff_freq = max_freq - avg_freq) |>
  left_join(wordle_df, by = join_by("Phrase" == "Word")) |>
  arrange(Date)

dd <- bb |> left_join(wordle_df, by = join_by("Phrase" == "Word")) |>
  arrange(Date) |> group_by(Date) |> summarize(avg_freq = mean(Frequency))

rolling_cc <- cc |>
  arrange(generation, Date) |>
  group_by(generation) |>
  mutate(
    # Calculate a 30-day rolling average. k=30
    # align='right' means the average is calculated using the current day and the 29 previous days.
    # fill=NA handles the first 29 days where a full window isn't available.
    `30 Day` = rollmean(avg_freq, k = 30, fill = NA, align = "right"),
    `90 Day` = rollmean(avg_freq, k = 90, fill = NA, align = "right")
  ) |>
  pivot_longer(cols = ends_with(" Day"))

ggplot(cc, aes(x = Date, y = avg_freq)) +
  geom_point(color = "grey20", alpha = 0.1) + # Make points semi-transparent
  # Use scale_y_log10 for better distribution, as you discovered
  scale_y_log10(labels = scales::label_percent()) + 
  # Add the smoothed trend line. 'method = "loess"' is a good default.
  geom_smooth(aes(color = generation, group = generation), method = "loess", se = FALSE, linewidth = 1) +
  labs(title = "Frequency Over Time with Trend Line",
       y = "Frequency (Log Scale)") +
  theme_minimal()

y_top <- max(rolling_cc$value[rolling_cc$name == "90 Day"], na.rm = TRUE)
y_bottom <- min(rolling_cc$value[rolling_cc$name == "90 Day"], na.rm = TRUE) * 1.5

ggplot(filter(rolling_cc, name == "90 Day"), aes(x = Date,
                       y = value, 
                       color = factor(generation, c("Greatest", 
                                                     "Silent",
                                                     "Boomers",
                                                     "Gen X",
                                                     "Millenial",
                                                     "Gen Z")), 
                       group = generation)) +
  
  # 1. Map aesthetics like 'linewidth' to your 'name' variable
  geom_point(data = filter(dd, avg_freq < y_top), aes(x = Date, y = avg_freq), alpha = 0.3, color = "grey60", inherit.aes = FALSE) +
  geom_smooth(method = "loess", se = FALSE, span = 0.3, linewidth = 0.8) +
  # 2. Use scale_color_manual to assign specific colors and labels
  scale_color_manual(
    name = "Generation", # This sets the title of the legend
    labels = c("Greatest" = "Greatest", 
               "Silent" = "Silent",
               "Boomers" = "Boomers",
               "Gen X" = "Gen X",
               "Millenial" = "Millenial",
               "Gen Z" = "Gen Z"),
    values = c(
      "Greatest"   = "#2c3e50",  # Dark Slate Blue
      "Silent"     = "#708090",  # Slate Gray
      "Boomers"    = "#e67e22",  # Burnt Orange
      "Gen X"      = "#1abc9c",  # Teal
      "Millenial"  = "#E5A4CB",  # Millennial Pink
      "Gen Z"      = "#f1c40f"   # Electric Yellow
    )
  ) +
  labs(
    title = "Wordle Answer Difficulty Over Time, for each generation",
    subtitle = "90-Day Rolling Averages",
    caption = "Data from Google ngram; generations assigned on\n20-year windows using birth dates from parents.com",
    y = "Smoothed Frequency",
    x = "Date"
  ) +
  annotate(
    "label", x = as.Date("2021-05-05"), y = (y_top * 0.8 + y_top)/2, 
    label = "Easier", color = "gray20", hjust = 0.5,
    angle = 90
  ) +
  annotate(
    "segment", x = as.Date("2021-06-05"), xend = as.Date("2021-06-05"),
    y = y_top * 0.8, yend = y_top,
    arrow = arrow(length = unit(2, "mm")), color = "gray20"
  ) +
  annotate(
    "label", x = as.Date("2021-05-05"), y = (y_bottom -  (y_top * 0.8 - y_top) + y_bottom )/ 2, 
    label = "Harder", color = "gray20", hjust = 0.5,
    angle = 90
  ) +
  annotate(
    "segment", x = as.Date("2021-06-05"), xend = as.Date("2021-06-05"),
    y = y_bottom -  (y_top * 0.8 - y_top) , yend = y_bottom,
    arrow = arrow(length = unit(2, "mm")), color = "gray20"
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.83, 0.83),
        legend.background = element_roundrect(fill = "white",
                                         colour = "grey20"))# Move legend to the bottom
