library(tidyverse)
library(dbplyr)
library(fmpapi)

con <- DBI::dbConnect(RPostgres::Postgres(),dbname = 'stocks',
                      host = 'localhost',
                      port = 5432,
                      user = 'postgres',
                      password = 'postgres')

data_tbl <- tbl(con, "price_data")
company_tbl <- tbl(con, "company_info")

company_info <- collect(company_tbl)


source("R/utils.R")

df <- data_tbl %>%
  filter(
    #date == max(date)
    date == as.Date("2021-04-05")
  ) %>%
  collect()


df %>%
  filter(
    change_per_over_close_013_days_ago > 0
  ) %>%
  slice_max(atrp_ema_013, n = 40) %>%
  left_join(company_info) %>%
  ggplot(
    aes(y = fct_reorder(symbol_label, atrp_ema_013), x = atrp_ema_013, fill = sector) ) +
  geom_col()



sym_list <- df %>%
  filter(
   #change_per > 4
    volume_ema_013 > 900000,
    # smoothed_velocity > 10,
    close < 100,
    total_spikes_last_055 > 1,
    #smoothed_acceration > 0,
    change_per_over_close_034_days_ago > 10
  ) %>%
  slice_max(change_per_over_close_034_days_ago , n = 100) %>%
  select(symbol, change_per_over_close_034_days_ago ) %>%
  arrange(desc(change_per_over_close_034_days_ago)) %>% print() %>%
  pull(symbol)

dplot <-
data_tbl %>%
  filter(
    symbol %in% sym_list,
    date >= as.Date("2020-01-01")
  ) %>%
  collect() %>%
  mutate(
    symbol = factor(symbol, levels= sym_list)
  )

dplot %>%
  ggplot(
    aes(x = date, group = symbol)
  ) +
  geom_line(aes(y = close), size = 1) +
  # geom_line(aes(y = smoothed_acceration), size = 1, color = "black") +
  # geom_line(aes(y = smoothed_jerk), size = 1, color = "grey") +
  facet_wrap(~symbol, scales = "free")

dplot %>%
  filter(
    date >= as.Date("2020-12-01"),
   #date < as.Date("2021-02-01")
  ) %>%
  select(
    symbol, date, starts_with("close_ema_0"), -matches("lag")
  ) %>%
  pivot_longer(cols =  starts_with("close_ema_0")) %>%
  ggplot(
    aes(x = date, y = value, color = name)
  ) +
  geom_line() +
  facet_wrap(~symbol, scales = "free")


d <- fmp_daily_prices(c("RIOT", "TSLA", "BLNK", "SOL"), start_date = "2019-01-01") %>%
  group_by(symbol) %>%
  format_data()

d %>%
  group_by(
    symbol
  ) %>%
  mutate(
    y = nice_ema(change_per_over_close_001_days_ago, 8),
    y1 = nice_ema(change_per_over_close_034_days_ago, 55),
    #y13 = nice_ema(change_per_over_close_001_days_ago, 34),
    #y = y13-y34,
    # symbol == "BLNK",
    # date >= as.Date("2021-01-01")
  ) %>%
  filter(
    date >= as.Date("2020-01-01")
  ) %>%
  ggplot(
    aes(x = date, color = symbol)
  ) +
  geom_line(aes(y = close), size = 1) +
  #geom_line(aes(y = change_per), size = 1, color = "black", alpha = 0.5) +
  facet_wrap(~symbol, scale = "free") +
  scale_y_log10() +
  scale_x_date(breaks = "1 month", labels = scales::label_date_short())



d %>%
  filter(
    date >= as.Date("2020-01-01")
  ) %>%
ggplot(
  aes(x = date, group = symbol)
) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(y = macd_signal), color = "gold", size = 1) +
  geom_line(aes(y = macd_line), color = "purple", size = 1) +
  #geom_line(aes(y = close_ema_001), color = "black", size = 0.8) +
  # geom_line(aes(y = close_ema_008), color = "grey", size = 0.8) +
  # geom_line(aes(y = macd_line_short), color = "purple", size = 0.8, linetype = "dashed")+
  facet_wrap(~symbol, scales = "free") +
  scale_x_date(breaks = "1 month", labels = scales::label_date_short())

d %>%
  group_by(
    symbol
  ) %>%
  mutate(
    y = nice_ema(change_per_over_close_001_days_ago, 8),
    y1 = nice_ema(change_per_over_close_013_days_ago, 21),
    y2 = nice_ema(y1, 21),
    #y13 = nice_ema(change_per_over_close_001_days_ago, 34),
    #y = y13-y34,
    # symbol == "BLNK",
    # date >= as.Date("2021-01-01")
  ) %>%
  filter(
    date >= as.Date("2020-01-01")
  ) %>%
  ggplot(
    aes(x = date, color = symbol)
  ) +
  #geom_line(aes(y = mean_sorted_per), size = 1) +
  geom_line(aes(y = y1), size = 1) +
  #geom_line(aes(y = y2), size = 1, color = "red") +
  geom_hline(aes(yintercept = 0)) +
  #geom_line(aes(y = change_per), size = 1, color = "black", alpha = 0.5) +
  facet_wrap(~symbol) +
  scale_x_date(breaks = "1 month", labels = scales::label_date_short())

# d %>%
#   ggplot(
#     aes(x = date, color = symbol)
#   ) +
#   geom_line(aes(y = mean_sorted_per_34), size = 1) +
#   facet_wrap(~symbol, scales = "free")




