library(rvest)
library(xml2)
library(tidyverse)
library(ngramr)
library(zoo)
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

aa <- lapply(wordle_df_split, \(x) ngramr::ngram(x$Word, year_start = 2020, year_end = 2022, smoothing = 0))
bb <- data.table::rbindlist(aa) |> as.data.frame() |>
  group_by(Phrase) |>
  summarize(avg_freq = mean(Frequency)) |>
  left_join(wordle_df, by = join_by("Phrase" == "Word"))

ggplot(bb, aes(x = Date, y = avg_freq)) +
  geom_point(alpha = 0.5, color = "grey40") + # Make points semi-transparent
  # Use scale_y_log10 for better distribution, as you discovered
  scale_y_log10(labels = scales::label_percent()) + 
  # Add the smoothed trend line. 'method = "loess"' is a good default.
  geom_smooth(method = "loess", se = FALSE, color = "blue", linewidth = 1) +
  labs(title = "Frequency Over Time with Trend Line",
       y = "Frequency (Log Scale)") +
  theme_minimal()

df_rolling <- bb |>
  arrange(Date) |>
  mutate(
    # Calculate a 30-day rolling average. k=30
    # align='right' means the average is calculated using the current day and the 29 previous days.
    # fill=NA handles the first 29 days where a full window isn't available.
    `30 Day` = rollmean(avg_freq, k = 30, fill = NA, align = "right"),
    `90 Day` = rollmean(avg_freq, k = 90, fill = NA, align = "right")
  ) |>
  pivot_longer(cols = ends_with(" Day"))

# Plot the rolling average line
y_top <- max(df_rolling$value, na.rm = TRUE)
y_bottom <- min(df_rolling$value, na.rm = TRUE) * 1.5

ggplot(df_rolling, aes(x = Date, y = value, color = name, group = name, alpha = name)) +
  
  # 1. Map aesthetics like 'linewidth' to your 'name' variable
  geom_line(aes(linewidth = name)) +
  
  # 2. Use scale_color_manual to assign specific colors and labels
  scale_color_manual(
    name = "Trend", # This sets the title of the legend
    labels = c(`30 Day` = "30-Day Average", `90 Day` = "90-Day Average"),
    values = c(`30 Day` = "grey60", `90 Day` = "navy")
  ) +
  
  # 3. Use scale_linewidth_manual to assign specific line thicknesses
  scale_linewidth_manual(
    name = "Trend", # Use the same name to merge the legends
    labels = c(`30 Day` = "30-Day Average", `90 Day` = "90-Day Average"),
    values = c(`30 Day` = 0.9, `90 Day` = 1.2)
  ) +
  
  scale_alpha_manual(
    name = "Trend", # Use the same name to merge the legends
    labels = c(`30 Day` = "30-Day Average", `90 Day` = "90-Day Average"),
    values = c(`30 Day` = 0.4, `90 Day` = 1)
  ) +
  
  # Using the annotation code from your example (adjust positions as needed)
  # ... your annotate() calls would go here ...
  
  labs(
    title = "Wordle Answer Difficulty Over Time",
    subtitle = "30-Day and 90-Day Rolling Averages",
    y = "Smoothed Frequency",
    x = "Date"
  ) +
  # Annotation for "Easier" at the top
  annotate(
    "label", x = as.Date("2021-06-05"), y = (y_top * 0.8 + y_top)/2, 
    label = "Easier", color = "gray20", hjust = 0.5,
    angle = 90
  ) +
  annotate(
    "segment", x = as.Date("2021-07-05"), xend = as.Date("2021-07-05"),
    y = y_top * 0.8, yend = y_top,
    arrow = arrow(length = unit(2, "mm")), color = "gray20"
  ) +
  
  # Annotation for "Harder" at the bottom
  annotate(
    "label", x = as.Date("2021-06-05"), y = (y_bottom -  (y_top * 0.8 - y_top) + y_bottom )/ 2, 
    label = "Harder", color = "gray20", hjust = 0.5,
    angle = 90
  ) +
  annotate(
    "segment", x = as.Date("2021-07-05"), xend = as.Date("2021-07-05"),
    y = y_bottom -  (y_top * 0.8 - y_top) , yend = y_bottom,
    arrow = arrow(length = unit(2, "mm")), color = "gray20"
  ) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "bottom") # Move legend to the bottom
