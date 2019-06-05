library(tidyverse)
library(quantmod)
library(bizdays)
library(lubridate)
library(readxl)

internet_IPOs <- read_excel("Internet-IPOs.xlsx")
symbols <- internet_IPOs$`IPO name`

load_rmetrics_calendars(1985:year(Sys.Date()) + 10)
bizdays.options$set(default.calendar = "Rmetrics/NYSE")

mutual_fund_portfolio <-
  data.frame(
    symbol = c(
      "AAIEX",
      "AVFIX",
      "AEPFX",
      "FMNDX",
      "HLMEX",
      "JVLIX",
      "MFEIX",
      "MCVIX",
      "MMIIX",
      "PRINX",
      "TEGYX",
      "VMLUX",
      "MISIX"
    ),
    pct = c(
      .0386,
      .0894,
      .0291,
      .0397,
      .2141,
      .0201,
      .0186,
      .0880,
      .0792,
      .1294,
      .0326,
      .1017,
      .1195
    ),
    stringsAsFactors = FALSE
  )

# FMIQX, FROGX, WIAEX removed temporarily, and pcts adjusted

getSymbols(symbols)
getSymbols(mutual_fund_portfolio$symbol)

clean_up_stock <- function(x) {
  df <- as.data.frame(x)
  
  symbol <- str_extract(names(df[1]), "^[A-Z]{1,5}")
  
  df <- tibble::rownames_to_column(df, var = "date")
  
  colnames(df) <-
    c("date", "open", "high", "low", "close", "volume", "adjusted")
  
  df <- df %>% mutate(symbol = symbol)
  
  df$date <- as.Date(df$date)
  
  return(df)
  
}

dates_of_interest <- function(df, n_quarters = 20) {
  ipo_date <- min(as.Date(df$date))
  
  lockup_expiration <- adjust.next(ipo_date + 180)
  
  quarter_dates <- lockup_expiration %m+% months(3 * 1:n_quarters)
  
  quarter_dates <-
    map(quarter_dates, adjust.next) %>% unlist() %>% as.Date(.)
  
  c(lockup_expiration, quarter_dates)
  
}

make_mf_investment <-
  function(investment_amt,
           as_of_date,
           mf_data. = mf_data,
           mutual_fund_portfolio. = mutual_fund_portfolio) {
    purchase_summary <- mf_data. %>%
      filter(date == as_of_date) %>%
      full_join(mutual_fund_portfolio., by = c("symbol")) %>%
      mutate(shares = (investment_amt * pct) / close,
             shares = replace_na(shares, 0)) %>%
      select(symbol, shares)
    
    return(purchase_summary)
    
  }

value_mf_investment <-
  function(df, as_of_date, mf_data. = mf_data) {
    snapshot <-
      mf_data[which(mf_data.$symbol %in% df$symbol &
                      mf_data.$date == as_of_date), ] %>%
      left_join(df, by = c("symbol")) %>%
      mutate(value = shares * close)
    
    return(sum(snapshot$value))
    
  }

combine_portfolios <- function(portfolio1, portfolio2) {
  portfolio1 %>%
    left_join(portfolio2, by = c("symbol")) %>%
    rowwise() %>%
    mutate(shares = sum(shares.x, shares.y, na.rm = TRUE))
}

sell_over_n_quarters <-
  function(df, starting_shares, n_quarters, symbol = NA) {
    shares_sold_per_quarter <- starting_shares / (n_quarters + 1)
    
    sales <- df %>%
      arrange(date) %>%
      mutate(
        selling_points = seq_along(date),
        shares_sold = ifelse(selling_points <= n_quarters + 1, shares_sold_per_quarter, 0),
        cash = shares_sold * close,
        remaining_shares = ifelse(
          shares_sold == 0,
          0,
          starting_shares - (shares_sold_per_quarter * selling_points)
        ),
        remaining_position_value = remaining_shares * close,
        mf_purchase = pmap(
          list(lag(cash), date),
          ~ make_mf_investment(investment_amt = .x, as_of_date = .y)
        ),
        mf_value = map2_dbl(mf_purchase, date, ~ value_mf_investment(.x, .y)),
        mf_purchase2 = pmap(
          list(lag(mf_value), lag(date)),
          ~ make_mf_investment(investment_amt = .x, as_of_date = .y)
        ),
        cum_portfolio = map2(mf_purchase, mf_purchase2, ~ combine_portfolios(.x, .y))
      )
    
    if (n_quarters < 19) {
      sales$cum_portfolio[(n_quarters + 3):length(sales$cum_portfolio)] <-
        sales$cum_portfolio[(n_quarters + 2)]
    }
    
    sales <- sales %>%
      mutate(
        cum_portfolio_value = map2_dbl(cum_portfolio, date, ~ value_mf_investment(.x, .y)),
        ttl_value = cash + remaining_position_value + cum_portfolio_value,
        value_if_held = starting_shares * close
      ) %>%
      select(
        date,
        cash,
        remaining_position_value,
        cum_portfolio_value,
        ttl_value,
        value_if_held
      )
    
    sales$symbol <- symbol
    
    return(list(
      value_at_20qs = sales$ttl_value[nrow(sales)],
      transaction_history = sales
    ))
    
  }

starting_value <- 1e4

stock_data <- list(mget(ls()[which(ls() %in% symbols)])) %>%
  flatten() %>%
  map(clean_up_stock) %>%
  bind_rows() %>%
  fill(-date,-symbol) %>%
  select(-high,-low,-volume,-adjusted) %>%
  group_by(symbol) %>%
  nest() %>%
  mutate(
    ipo_date = map_chr(data, ~ as.character(as.Date(
      min(.x$date), origin = "1970-01-01"))),
    dates_of_interest = map(data, dates_of_interest),
    filtered_data = pmap(list(data, dates_of_interest), function(df, x)
      df[which(df$date %in% x),]),
    shares = map_dbl(data, function(df)
      starting_value / df$open[1])
  )

mf_data <-
  list(mget(ls()[which(ls() %in% mutual_fund_portfolio$symbol)])) %>%
  flatten() %>%
  map(clean_up_stock) %>%
  bind_rows() %>%
  fill(-date,-symbol)

evaluate_strategies <- stock_data %>%
  mutate(length = map_lgl(filtered_data, ~ nrow(.x) == 21)) %>%
  filter(length == TRUE) %>%
  group_by(symbol) %>%
  slice(rep(1, each = 21)) %>%
  mutate(n_quarters = seq_along(symbol) - 1) %>%
  mutate(
    strategy_outcome = pmap(
      list(filtered_data, shares, n_quarters, symbol),
      sell_over_n_quarters),
    value_at_20qs = map_dbl(strategy_outcome, 1)
  )

stock_data %>%
  mutate(length = map_lgl(filtered_data, ~ nrow(.x) == 21)) %>%
  filter(length == TRUE) %>%
  unnest(filtered_data) %>%
  group_by(symbol) %>%
  mutate(x = seq_along(date) - 1, value = shares * close) %>%
  ggplot() +
  geom_line(aes(x = x, y = value, group = symbol), alpha = .1) +
  theme_minimal() +
  geom_hline(aes(yintercept = starting_value), colour = "red") +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 1e4)) +
  labs(x = "Quarters after lockup expires", y = "Value",
       title = "Value of $1K Investment Over Time")

evaluate_strategies %>%
  ggplot() +
  geom_line(aes(x = n_quarters, y = value_at_20qs, group = symbol), alpha = .1) +
  theme_minimal() +
  geom_hline(aes(yintercept = starting_value), colour = "red") +
  scale_y_continuous(labels = scales::dollar, limits = c(0, 5e6)) +
  labs(x = "Length of Selling Window\n(Quarters After Lockup Expires)", 
       y = "Value 20 Quarters\nafter Lockup Expires",
       title = "Liquidation Strategy Results")

evaluate_strategies %>%
  group_by(symbol) %>%
  mutate(pct_relative_to_optimal = value_at_20qs / max(value_at_20qs)) %>%
  ggplot() +
  geom_line(aes(x = n_quarters, y = pct_relative_to_optimal, group = symbol), alpha = .1) +
  geom_line(
    data = evaluate_strategies %>%
      group_by(symbol) %>%
      mutate(pct_relative_to_optimal = value_at_20qs / max(value_at_20qs)) %>%
      ungroup() %>%
      group_by(n_quarters) %>%
      summarize(med_result = median(pct_relative_to_optimal)),
    aes(x = n_quarters, y = med_result),
    colour = "red",
    size = 1
  ) +
  theme_minimal() +
  labs(x = "Length of Selling Window\n(Quarters After Lockup Expires)",
       y = NULL,
       title = "Strategy Result Relative to Optimal Strategy")
