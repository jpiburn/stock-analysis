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


d <- data_tbl %>%
  filter(
    date == max(date)
    #date == as.Date("2021-03-31")
  ) %>%
  collect()


d %>%
  ungroup() %>%
  filter(
 # close < 8,
 # close_ema_001 > close_ema_001_lag1,
  close_ema_089 > close_ema_001,
  total_spikes_last_055 >= 1,
  volume_ema_003 > 500000,
  volume_ema_013 > 500000,
  change_per_over_close_013_days_ago < 20,
  change_per_over_close_144_days_ago > 20,
 # macd_line <= 40,
 # macd_line >= -10,
  #change_per_last_30 > 20,
  #change_per_last_30 > 0,
  # change_per_last_05 > -1,
  #macd_signal > macd_line,
  # stoch_d < 60,
  #close_ema_005 > close_ema_005_lag1,
  #close_ema_089 > close_ema_055
  #stoch_k <= 50,
  #mean_sorted_per < mean_sorted_per_34,
  # volume > 1000000,
 # atrp >= 4,
  #macd_line < 40,
 # (macd_signal - macd_line) <= 0.3,
  # lag3_macd_line < macd_line
  # macd_signal < 0,
  # obv_ema_005 > obv_ema_013
) %>%
  left_join(company_info) %>%
  arrange(desc(close_ema_001/close_ema_003)) %>%
  slice_max(close_ema_001/close_ema_003, n = 110) %>%
  pull(symbol) -> d_screen



d_data <- data_tbl %>%
  filter(
    symbol %in% d_screen,
    date >= as.Date("2021-01-01")
  ) %>%
  collect()


d_data %>%
  left_join(company_info) %>%
  mutate(
    #symbol_label = factor(symbol_label, levels = d_screen),
    symbol = factor(symbol, levels = d_screen)
  ) %>%
  # filter(
  #   symbol == "POAI"
  # ) %>%
  select(
    symbol, symbol_label, date, starts_with("close_ema_0"), -matches("lag")
  ) %>%
  pivot_longer(cols =  starts_with("close_ema_0")) %>%
  ggplot(
    aes(x = date, y = value, color = name)
  ) +
  geom_line() +
  facet_wrap(~symbol, scales = "free")



d_data %>%
  left_join(company_info) %>%
  mutate(
    #symbol_label = factor(symbol_label, levels = d_screen),
    symbol = factor(symbol, levels = d_screen)
  ) %>%
  filter(
    date >= as.Date("2021-01-01")
  ) %>%
  ggplot(
    aes(x = date, group = symbol)
  ) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(y = macd_signal), color = "gold", size = 1) +
  geom_line(aes(y = macd_line), color = "purple", size = 1)+
  #geom_line(aes(y = close_ema_001), color = "black", size = 0.8) +
  # geom_line(aes(y = close_ema_008), color = "grey", size = 0.8) +
  # geom_line(aes(y = macd_line_short), color = "purple", size = 0.8, linetype = "dashed")+
  facet_wrap(~symbol, scales = "free")


d %>%
  filter(
    symbol %in% d_screen$symbol,
    date >= as.Date("2021-01-01")
  ) %>%
  left_join(select(stock_info, symbol, company_name, symbol_label)) %>%
  mutate(
    symbol_label = factor(symbol_label, levels = d_screen$symbol_label),
    symbol = factor(symbol, levels = d_screen$symbol)
  )  %>%
  ggplot(aes(x = date, y = close)) +
  tidyquant::geom_candlestick(
    aes(open = open, high = high, low = low, close = close),
    colour_up = "#50C878",
    colour_down = "#CD5C5C",
    fill_up = "#50C878",
    fill_down = "#CD5C5C",
    #alpha = 1
  ) +
  # geom_line(
  #   aes(y =close_ema_021), size = 1
  # ) +
  # geom_line(
  #   aes(y =close_ema_008), color = "#50C878", size = 1
  # ) +
  scale_x_date(labels = scales::label_date_short()) +
  labs(y = "Closing Price", x = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~symbol, scales = "free")





d <- d %>%
  mutate(
    jump_10p_21 = nice_ema(slide_dbl(change_per, ~mean(.x >= 10, na.rm = TRUE) * 100, .before = 21), 21),
    jump_10p = nice_ema(slide_dbl(change_per, ~mean(.x >= 10, na.rm = TRUE) * 100, .before = 13), 8),
    jump_13p = nice_ema(slide_dbl(change_per, ~mean(.x >= 13, na.rm = TRUE) * 100, .before = 13), 8),
    jump_20p = nice_ema(slide_dbl(change_per, ~mean(.x >= 20, na.rm = TRUE) * 100, .before = 13), 8),
    lag3_macd_line = lag(macd_line, 3),
    lag3_macd_ratio = macd_line / lag3_macd_line
    )




dlast <-
d %>%
  group_by(symbol) %>%
  filter(
    date == max(date),
    min(date) >= as.Date("2020-03-01")
  )

d_screen <-
  dlast %>%
  filter(
  #close > 6,
 # volume_ema_013 > 1000000,
  #obv_ema_005 > obv_ema_008,
  #macd_line >= 10,
   # volume > 1000000,
  # atrp >= 6,
    #macd_line < 0,
    #macd_line <  macd_signal,
   #macd_signal < 0,
 #  obv_ema_008 > obv_ema_013,
  #close_ema_005 > close_ema_008,
  #  close_ema_008 > close_ema_013,
  #  close_ema_013 > close_ema_021,
  #close_ema_021 > close_ema_034,
    #macd_line_short > macd_signal_short
  # macd_line >= lag3_macd_line
    #lag_atrp_ema_034 >= 1,
  ) %>%
  left_join(stock_info, by = "symbol") %>%
  # filter(
  #   industry != "Oil & Gas E&P"
  # ) %>%
  select(symbol, company_name, industry, everything()) %>%
  ungroup() %>%
  arrange(desc(lag3_macd_ratio)) %>%
  print() %>%
  slice_max(lag3_macd_ratio, n = 92)





d %>%
  filter(
    symbol %in% d_screen$symbol,
    date >= as.Date("2020-12-20")
  ) %>%
  left_join(stock_info) %>%
  ggplot(
    aes(x = date, group = symbol)
  ) +
  #geom_hline(aes(yintercept = 0)) +
  geom_line(aes(y = macd_signal), color = "gold", size = 1) +
  geom_line(aes(y = macd_line), color = "purple", size = 1)+
  #geom_line(aes(y = close_ema_001), color = "black", size = 0.8) +
 # geom_line(aes(y = close_ema_008), color = "grey", size = 0.8) +
 # geom_line(aes(y = macd_line_short), color = "purple", size = 0.8, linetype = "dashed")+
  facet_wrap(~symbol, scales = "free")




d %>%
  filter(
    symbol %in% d_screen$symbol,
    date >= as.Date("2020-09-01")
  ) %>%
  left_join(stock_info) %>%
  mutate(
    symbol_label = fct_reorder(symbol_label, jump_10p, last, .desc = TRUE)
  ) %>%
  select(
    symbol, symbol_label, date, starts_with("close_ema_0"), -matches("lag")
  ) %>%
  pivot_longer(cols =  starts_with("close_ema_0")) %>%
  ggplot(
    aes(x = date, y = value, color = name)
  ) +
  geom_line() +
  facet_wrap(~symbol_label, scales = "free")





d_jumps <- d %>%
  filter(
    date >= as.Date("2020-12-01"),
    symbol %in% d_screen$symbol,
    abs(change_per) >= 12
  ) %>%
  mutate(
    direction = if_else(change_per >= 0, "up", "down")
  )


d %>%
  filter(
    symbol %in% d_screen$symbol,
    date >= as.Date("2020-12-01")
  ) %>%
  ggplot(
    aes(x = date, y = close, group = symbol)
  ) +
  geom_line() +
  geom_point(
    data = d_jumps,
    aes(colour = direction)
  ) +
  # geom_hline(aes(yintercept = 0)) +
  facet_wrap(~symbol, scales = "free")











stock_targets <- c(
  "CLNE", "CLSK", "DNN",
  "RIOT", "SOL", "UEC", "URG", "UUUU", "MOGO"
  )



d %>%
  filter(
    date >= as.Date("2020-10-01"),
    symbol %in% stock_targets,
  ) %>%
  mutate(
    tmrw_change_per = lead(change_per)
  ) %>%
  ggplot(
    aes(y = tmrw_change_per, x = change_per)
  ) +
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_point(size = 2, colour = "dark red") +
  facet_wrap(~symbol) +
  theme_minimal()



d_jumps <- d %>%
  filter(
    date >= as.Date("2020-11-01"),
    symbol %in% stock_targets,
    abs(change_per) >= 10
  ) %>%
  mutate(
    direction = if_else(change_per >= 0, "up", "down")
  )


d %>%
  filter(
    symbol %in% stock_targets,
    date >= as.Date("2020-11-01")
  ) %>%
  ggplot(
    aes(x = date, y = close, group = symbol)
  ) +
  geom_line() +
  geom_point(
    data = d_jumps,
    aes(colour = direction)
  ) +
  # geom_hline(aes(yintercept = 0)) +
  facet_wrap(~symbol, scales = "free")



# %>%
#   ungroup() %>%
#   slice_max(close200_lag50, n = 50) %>%
#   arrange(desc(atrp14))



# d_screen <-
#   dlast %>%
#   filter(
#     #  close > 6,
#     #  volume_ema_013 > 1000000,
#     # volume > 1000000,
#     # atrp >= 6,
#     macd_line < 0,
#     macd_line <  macd_signal,
#     #macd_signal < 0,
#     obv_ema_008 > obv_ema_021,
#     #macd_line_short > macd_signal_short
#     macd_line >= lag3_macd_line
#     #lag_atrp_ema_034 >= 1,
#   ) %>%
#   left_join(stock_info, by = "symbol") %>%
#   # filter(
#   #   industry != "Oil & Gas E&P"
#   # ) %>%
#   select(symbol, company_name, industry, everything()) %>%
#   ungroup() %>%
#   arrange(desc(lag3_macd_ratio)) %>%
#   print() %>%
#   slice_max(lag3_macd_ratio, n = 40)
#

