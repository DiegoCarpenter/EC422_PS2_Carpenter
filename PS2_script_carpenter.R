library(tidyquant)
library(tidyverse)

# unemp_raw <- tq_get(
#   c("UNRATE", "ORUR", "CAUR", "TXUR", "NYUR"),
#   get  = "economic.data",
#   from = "1976-01-01"
# )
# 
# head(unemp_raw)
# 
# dir.create("data", showWarnings = FALSE)
# write_rds(unemp_raw, "data/unemp_raw.rds")

unemp_raw <- read_rds("data/unemp_raw.rds")


unemp <- unemp_raw |>
  rename(unemployment_rate = price) |>
  mutate(
    region = case_when(
      symbol == "UNRATE" ~ "United States",
      symbol == "ORUR"   ~ "Oregon",
      symbol == "CAUR"   ~ "California",
      symbol == "TXUR"   ~ "Texas",
      symbol == "NYUR"   ~ "New York"
    )
  )



# Replace the missing Oct 2025 value with Sept 2025, within each region
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(
    unemployment_rate = replace(
      unemployment_rate,
      date == as.Date("2025-10-01"),
      unemployment_rate[date == as.Date("2025-09-01")]
    )
  ) |>
  ungroup()

# Quick check: should show 5 rows with no NAs
unemp |> filter(date == as.Date("2025-10-01"))

# Month-over-month change within each region
unemp <- unemp |>
  arrange(region, date) |>
  group_by(region) |>
  mutate(unemployment_rate_change = unemployment_rate - lag(unemployment_rate)) |>
  ungroup()

# 2015 onward
unemp_recent <- unemp |>
  filter(date >= as.Date("2015-01-01"))

# Summaries
unemp_summary <- unemp |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_recent_summary <- unemp_recent |>
  group_by(region) |>
  summarize(
    mean_rate = mean(unemployment_rate, na.rm = TRUE),
    min_rate  = min(unemployment_rate,  na.rm = TRUE),
    max_rate  = max(unemployment_rate,  na.rm = TRUE)
  )

unemp_top_increases <- unemp |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE) |>
  select(region, date, unemployment_rate, unemployment_rate_change)

unemp_top_decreases <- unemp |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE) |>
  select(region, date, unemployment_rate, unemployment_rate_change)

unemp_top_increases
unemp_top_decreases


unemp_no_covid <- unemp |>
  filter(year(date) != 2020)

unemp_top_increases_no_covid <- unemp_no_covid |>
  slice_max(unemployment_rate_change, n = 5, with_ties = TRUE) |>
  select(region, date, unemployment_rate, unemployment_rate_change)

unemp_top_decreases_no_covid <- unemp_no_covid |>
  slice_min(unemployment_rate_change, n = 5, with_ties = TRUE) |>
  select(region, date, unemployment_rate, unemployment_rate_change)


# 5.1 All regions on one plot
ggplot(unemp, aes(x = date, y = unemployment_rate, color = region)) +
  geom_line() +
  labs(title = "Unemployment Rate, U.S. and Selected States, 1976–Present",
       x = "Date", y = "Unemployment rate (%)", color = "Region") +
  theme_minimal()

# 5.2 One panel per region
ggplot(unemp, aes(x = date, y = unemployment_rate)) +
  geom_line() +
  facet_wrap(~ region) +
  labs(title = "Unemployment Rate by Region",
       x = "Date", y = "Unemployment rate (%)") +
  theme_minimal()

# 5.3 Bar chart of means, sorted low to high
ggplot(unemp_summary, aes(x = fct_reorder(region, mean_rate), y = mean_rate)) +
  geom_col() +
  labs(title = "Average Unemployment Rate by Region, 1976–Present",
       x = "Region", y = "Mean unemployment rate (%)") +
  theme_minimal()


unemp |>
  select(date, region, unemployment_rate) |>
  pivot_wider(names_from = region, values_from = unemployment_rate) |>
  mutate(or_gap = Oregon - `United States`) |>
  ggplot(aes(x = date, y = or_gap)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Oregon Minus U.S. Unemployment Rate",
       x = "Date", y = "Percentage points") +
  theme_minimal()


 


