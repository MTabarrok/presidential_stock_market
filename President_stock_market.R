library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

# Read the CSV file
data <- fread("./stock_market_data_1871_2024.csv")
data$Date = sprintf("%-7s", as.character(data$Date)) %>% str_replace(" ", "0")

# Convert Date column to Date type
data$Date <- as.Date(paste0(as.character(data$Date), ".01"), format = "%Y.%m.%d")
data = data[, c(1, 8)]
names(data) = c("Date", "Price")

# Define presidents and their terms
presidents <- read_csv("./president_timelines.csv")
presidents <- as.data.frame(lapply(presidents, function(x) gsub('"', '', x)), stringsAsFactors = FALSE)
names(presidents) = c("Index", "Name", "TermBegin", "TermEnd")

# Create a dataset for political parties
party_data <- tibble(
  Name = c("Rutherford B. Hayes", "James A. Garfield", "Chester Alan Arthur", "Grover Cleveland", 
           "Benjamin Harrison", "Grover Cleveland", "William McKinley", "Theodore Roosevelt", 
           "William Howard Taft", "Woodrow Wilson", "Warren G. Harding", "Calvin Coolidge", 
           "Herbert Hoover", "Franklin Delano Roosevelt", "Harry S. Truman", "Dwight David Eisenhower", 
           "John Fitzgerald Kennedy", "Lyndon Baines Johnson", "Richard Milhous Nixon", 
           "Gerald Rudolph Ford", "James Earl Carter", "Ronald Wilson Reagan", 
           "George Herbert Walker Bush", "William Jefferson Clinton", "George Walker Bush", 
           "Barack Obama", "Donald Trump"),
  Party = c("R", "R", "R", "D", "R", "D", "R", "R", "R", "D", "R", "R", "R", "D", "D", "R", "D", 
            "D", "R", "R", "D", "R", "R", "D", "R", "D", "R")
)

# Clean and prepare the data
data <- data %>%
  mutate(Date = as.Date(Date))

presidents[24, 2] = "Grover Cleveland 2"

presidents <- presidents %>%
  filter(as.numeric(Index) >= 19) %>%
  mutate(
    TermBegin = dmy(TermBegin),
    TermEnd = dmy(TermEnd)
  ) %>%
  left_join(party_data, by = "Name")
presidents[7, 5] = "D"
presidents = presidents[-5, ]


# Read the CSV files
president_heights <- read_csv("./president_heights.csv")
president_heights = president_heights[19:45, ]
president_ratings <- read_csv("./president_ratings.csv")
president_ratings = president_ratings[19:45, ]
presidents$age = c(54, 49, 51, 47, 55, 55, 54, 42, 51, 56, 55, 51, 54, 51, 60, 62, 43, 55, 56, 61, 52, 69, 64, 46, 54, 47, 70)
presidents$major_war = factor(c(0,0,0,0,0,0,1,0,0,1,0,0,0,1,1,0,0,1,1,0,0,0,1,0,1,1,0))
presidents$tariff = factor(c(0,0,0,0,1,0,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1))
presidents$claude_ranking = c(1,0,0,1,0,1,0,1,0,1,0,0,0,1,1,1,1,1,0,0,0,1,1,1,0,1,0)




presidents = presidents %>%
  bind_cols(president_heights[, -1]) %>% bind_cols(president_ratings[, -1])

calculate_relative_price <- function(prices, term_begin) {
  start_price <- prices$Price[1]
  prices %>%
    mutate(
      RelativePrice = (Price / start_price) * 100,
      MonthsSinceTermStart = interval(term_begin, Date) %/% months(1)
    )
}

# Calculate relative prices for each president's term
price_data <- map_df(1:nrow(presidents), function(i) {
  term_data <- data %>%
    filter(Date >= presidents$TermBegin[i], Date <= presidents$TermEnd[i]) %>%
    arrange(Date) %>%
    calculate_relative_price(presidents$TermBegin[i]) %>%
    mutate(President = presidents$Name[i],
           Party = presidents$Party[i], 
           Height = presidents$`Height (inches)`[i], 
           Greatness = presidents$Greatness[i], 
           major_war = presidents$major_war[i], 
           tariff = presidents$tariff[i], 
           claude_ranking = presidents$claude_ranking[i],
           age = presidents$age[i])
  return(term_data)
})

lines_to_label <- price_data %>%
  group_by(President) %>%
  summarize(
    MaxRelativePrice = max(RelativePrice),
    MinRelativePrice = min(RelativePrice),
    TermLength = max(MonthsSinceTermStart)
  ) %>%
  ungroup() %>%
  mutate(
    LabelHighest = rank(-MaxRelativePrice) <= 5,
    LabelLowest = rank(MinRelativePrice) <= 5,
    LabelLongest = rank(-TermLength) <= 15
  ) %>%
  filter(LabelHighest | LabelLowest | LabelLongest)

# Add labels to the price_data
price_data_labeled <- price_data %>%
  group_by(President) %>%
  mutate(IsLastObservation = row_number() == n()) %>%
  ungroup() %>%
  left_join(lines_to_label, by = "President") %>%
  mutate(
    Label = case_when(
      IsLastObservation & LabelHighest ~ President,
      IsLastObservation & LabelLowest ~ President,
      IsLastObservation & LabelLongest ~ President,
      TRUE ~ NA_character_
    )
  )

price_data_labeled <- price_data_labeled %>%
  mutate(
    Height_Group = ifelse(Height > 72, "1", "0"),
    Greatness_Group = ifelse(Greatness >= 4, "1", "0"), 
    Age_Group = ifelse(age >= 55, "1", "0")
  )


################################################################################################################################################################

################################################################################################################################################################

# Create the chart
library(ggrepel)
party_plot = ggplot(price_data_labeled, aes(x = MonthsSinceTermStart, y = RelativePrice, color = Party, group = interaction(President, Party))) +
  geom_line(aes(alpha = 0.7), linewidth = 1.2) +
  geom_label_repel(
    aes(label = Label), 
    data = filter(price_data_labeled, !is.na(Label)),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = 'grey50',
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("D" = "blue", "R" = "red")) +
  labs(title = "Relative Price Changes During Presidential Terms",
       x = "Months Since Term Start",
       y = "Relative Price (Start of Term = 100)",
       color = "Party") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(alpha = 1)),
         alpha = "none")

avg_change <- price_data_labeled %>%
  group_by(President, Party) %>%
  summarize(AvgChange = mean(last(RelativePrice) - first(RelativePrice), na.rm = TRUE)) %>%
  group_by(Party) %>%
  summarize(AvgChange = mean(AvgChange, na.rm = TRUE))%>%
  mutate(Label = sprintf("%s: %.2f%%", Party, AvgChange))

party_plot = party_plot + annotate(
  "text",
  x = -Inf, y = Inf,
  label = paste(avg_change$Label, collapse = "\n"),
  hjust = -0.1, vjust = 1.1,
  size = 3
)
party_plot

ggsave(party_plot, filename = "./party_plot.jpeg", width = 9, height = 7, units = "in", dpi = 500)

################################################################################################################################################################

################################################################################################################################################################
################################################################################################################################################################
################################################################################################################################################################

################################################################################################################################################################

date = ggplot(price_data_labeled, aes(x = Date, y = (Price), color = Party, group = interaction(President, Party))) +
  geom_line(aes(alpha = 0.7), linewidth = 1.2) +
  geom_label_repel(
    aes(label = Label), 
    data = filter(price_data_labeled, !is.na(Label)),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = 'grey50',
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("D" = "blue", "R" = "red")) +
  labs(title = "Stock Price During Presidential Terms",
       x = "Date",
       y = "Price",
       color = "Party") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(alpha = 1)),
         alpha = "none")
date
ggsave(date, filename = "./date.jpeg", width = 9, height = 7, units = "in", dpi = 500)

################################################################################################################################################################

################################################################################################################################################################
################################################################################################################################################################

################################################################################################################################################################

war_plot = ggplot(price_data_labeled, aes(x = MonthsSinceTermStart, y = RelativePrice, color = factor(major_war), group = interaction(President, Party))) +
  geom_line(aes(alpha = 1), linewidth = 1.2) +
  geom_label_repel(
    aes(label = Label), 
    data = filter(price_data_labeled, !is.na(Label)),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = 'grey50',
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("0" = "forestgreen", "1" = "red")) +
  labs(title = "Relative Price Changes During Presidential Terms",
       x = "Months Since Term Start",
       y = "Relative Price (Start of Term = 100)",
       color = "Major War") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(alpha = 1)),
         alpha = "none")

avg_change <- price_data_labeled %>%
  group_by(President, major_war) %>%
  summarize(AvgChange = mean(last(RelativePrice) - first(RelativePrice), na.rm = TRUE)) %>%
  group_by(major_war) %>%
  summarize(AvgChange = mean(AvgChange, na.rm = TRUE))%>%
  mutate(Label = sprintf("%s: %.2f%%", major_war, AvgChange))

war_plot = war_plot + annotate(
  "text",
  x = -Inf, y = Inf,
  label = paste(avg_change$Label, collapse = "\n"),
  hjust = -0.1, vjust = 1.1,
  size = 3
)
war_plot
ggsave(war_plot, filename = "./war_plot.jpeg", width = 9, height = 7, units = "in", dpi = 500)

################################################################################################################################################################

################################################################################################################################################################
################################################################################################################################################################

################################################################################################################################################################

claude_plot = ggplot(price_data_labeled, aes(x = MonthsSinceTermStart, y = RelativePrice, color = factor(claude_ranking), group = interaction(President, Party))) +
  geom_line(aes(alpha = 1), linewidth = 1.2) +
  geom_label_repel(
    aes(label = Label), 
    data = filter(price_data_labeled, !is.na(Label)),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = 'grey50',
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("1" = "forestgreen", "0" = "red")) +
  labs(title = "Relative Price Changes During Presidential Terms",
       x = "Months Since Term Start",
       y = "Relative Price (Start of Term = 100)",
       color = "Claude Good or Bad") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(alpha = 1)),
         alpha = "none")

avg_change <- price_data_labeled %>%
  group_by(President, claude_ranking) %>%
  summarize(AvgChange = mean(last(RelativePrice) - first(RelativePrice), na.rm = TRUE)) %>%
  group_by(claude_ranking) %>%
  summarize(AvgChange = mean(AvgChange, na.rm = TRUE))%>%
  mutate(Label = sprintf("%s: %.2f%%", claude_ranking, AvgChange))

claude_plot = claude_plot + annotate(
  "text",
  x = -Inf, y = Inf,
  label = paste(avg_change$Label, collapse = "\n"),
  hjust = -0.1, vjust = 1.1,
  size = 3
)
claude_plot
ggsave(claude_plot, filename = "./claude_plot.jpeg", width = 9, height = 7, units = "in", dpi = 500)

################################################################################################################################################################

################################################################################################################################################################
################################################################################################################################################################

################################################################################################################################################################

height_plot = ggplot(price_data_labeled, aes(x = MonthsSinceTermStart, y = RelativePrice, color = factor(Height_Group), group = interaction(President, Party))) +
  geom_line(aes(alpha = 1), linewidth = 1.2) +
  geom_label_repel(
    aes(label = Label), 
    data = filter(price_data_labeled, !is.na(Label)),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = 'grey50',
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("1" = "forestgreen", "0" = "red")) +
  labs(title = "Relative Price Changes During Presidential Terms",
       x = "Months Since Term Start",
       y = "Relative Price (Start of Term = 100)",
       color = "Over/Under 6 Feet") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(alpha = 1)),
         alpha = "none")

avg_change <- price_data_labeled %>%
  group_by(President, Height_Group) %>%
  summarize(AvgChange = mean(last(RelativePrice) - first(RelativePrice), na.rm = TRUE)) %>%
  group_by(Height_Group) %>%
  summarize(AvgChange = mean(AvgChange, na.rm = TRUE))%>%
  mutate(Label = sprintf("%s: %.2f%%", Height_Group, AvgChange))

height_plot = height_plot + annotate(
  "text",
  x = -Inf, y = Inf,
  label = paste(avg_change$Label, collapse = "\n"),
  hjust = -0.1, vjust = 1.1,
  size = 3
)
height_plot
ggsave(height_plot, filename = "./height_plot.jpeg", width = 9, height = 7, units = "in", dpi = 500)


################################################################################################################################################################

################################################################################################################################################################
################################################################################################################################################################

################################################################################################################################################################

age_plot = ggplot(price_data_labeled, aes(x = MonthsSinceTermStart, y = RelativePrice, color = factor(Age_Group), group = interaction(President, Party))) +
  geom_line(aes(alpha = 1), linewidth = 1.2) +
  geom_label_repel(
    aes(label = Label), 
    data = filter(price_data_labeled, !is.na(Label)),
    size = 3,
    box.padding = 0.5,
    point.padding = 0.5,
    segment.color = 'grey50',
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("0" = "forestgreen", "1" = "red")) +
  labs(title = "Relative Price Changes During Presidential Terms",
       x = "Months Since Term Start",
       y = "Relative Price (Start of Term = 100)",
       color = "Over/Under 55 at Term Start") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(override.aes = list(alpha = 1)),
         alpha = "none")

avg_change <- price_data_labeled %>%
  group_by(President, Age_Group) %>%
  summarize(AvgChange = mean(last(RelativePrice) - first(RelativePrice), na.rm = TRUE)) %>%
  group_by(Age_Group) %>%
  summarize(AvgChange = mean(AvgChange, na.rm = TRUE))%>%
  mutate(Label = sprintf("%s: %.2f%%", Age_Group, AvgChange))

age_plot = age_plot + annotate(
  "text",
  x = -Inf, y = Inf,
  label = paste(avg_change$Label, collapse = "\n"),
  hjust = -0.1, vjust = 1.1,
  size = 3
)
age_plot
ggsave(age_plot, filename = "./age_plot.jpeg", width = 9, height = 7, units = "in", dpi = 500)
