## Loading package pacman and use its function p_load for loading all required packages
library(pacman)
p_load(timetk, tidyquant, highcharter, tidyverse, PerformanceAnalytics, quantmod, tibbletime, PortfolioAnalytics, 
       tibbletime, ROI, tidyr, estimatr, broom, ROI.plugin.glpk, ROI.plugin.quadprog)

## Create a vector with ticker symbols
symbols <- c("GOOG", "MSFT", "AMZN", "AAPL", "META", "NVDA", "TSLA")

## Import stock market prices for ticker symbols
prices <-
  getSymbols(symbols, src = 'yahoo', from = "2012-06-01", to = "2025-02-28",
             auto.assign = TRUE, warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

## Look at the prices downloaded
head(prices)

## Convert daily to monthly prices
prices_monthly <- to.monthly(prices, indexAt = "first", OHLC = FALSE)

## Calculate log returns of monthly prices
m7asset_returns_xts <- Return.calculate(prices_monthly, method = "log") %>% 
                       na.omit()

## Take a look at monthly log returns of Magnificent 7 stocks
head(m7asset_returns_xts)

## Set up portfolio criteria
port_spec <- portfolio.spec(colnames(m7asset_returns_xts))

port_spec <- add.constraint(port_spec, type = "weight_sum", min_sum = 1, max_sum = 1)
port_spec <- add.constraint(port_spec, type = "box", min = .08, max = .42)
port_spec <- add.objective(port_spec, type = "return", name = "mean")
port_spec <- add.objective(port_spec, type = "risk", name = "StdDev")

## Calculate optimum portfolio wtih respect to portfolio critera 
m7_opt <- optimize.portfolio(m7asset_returns_xts, port_spec, optimize_method = "ROI", trace = TRUE)

## Chart of optimal Magnificent 7 portfolio
chart.Weights(m7_opt)

## Extract Efficient Frontier of optimum Magnificent 7 portfolio
eff_frontier <- extractEfficientFrontier(m7_opt, match.col = "StdDev", n.portfolios = 25, risk_aversion = NULL)

## Create chart of Efficient Frontier of optimum Magnificent 7 portfolio
chart.EfficientFrontier(eff_frontier, match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL, cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier", RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legent = 0.8, chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21, cex.assets = 0.8)

extractEfficientFrontier(m7_opt)

## Create object of the optimum Magnificent 7 portfolio weights
m7_opt_w <- m7_opt$weights

## Single values
m7_opt_w

## Calculate Magnificent 7 optimum Portfolio rebalanced yearly
opt_m7_portfolio_xts <-
  Return.portfolio(m7asset_returns_xts, weights = m7_opt_w, rebalance_on = "years") %>%
  `colnames<-`("returns")

## Take a look at the optimum M7 portfolio returns
head(opt_m7_portfolio_xts)

## Convert M7 optimum portfolio from wide to long format
asset_returns_long <-
  prices %>%
  to.monthly(indexAt = "first", OHLC = FALSE) %>%
  tk_tbl(preserve_index = TRUE, rename_index = "date")%>%
  gather(asset, returns, -date) %>%
  group_by(asset) %>%
  mutate(returns = (log(returns) - log(lag(returns)))) %>%
  na.omit()

## Create a tibble object of M7 optimum portfolio
opt_m7_portfolio_tbl <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = m7_opt_w,
               col_rename = "returns",
               rebalance_on = "years")

## Extract mean and standard deviation of optimum M7 portfolio
m7_port_mean <- mean(opt_m7_portfolio_tbl$returns)
m7_port_stdev <- sd(opt_m7_portfolio_tbl$returns)

## Create a function for the Monte Carlo simulation of the optimum M7 portfolio cumulative product
m7_simulation_cumprod <- function(init_value, N, mean, stdev) {
    tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>% 
    `colnames<-`("returns") %>% 
    mutate(growth = cumprod(returns)) %>%
    select(growth)
}

## Conduct a simulation test for all three methods  
m7_simulation_test <- m7_simulation_cumprod(1, 120, m7_port_mean, m7_port_stdev)

## Take a look at the last six results
tail(m7_simulation_test)

## Calculate the optimum M7 portfolio Compounded Annual Growth Rate
m7_cagr <- ((m7_simulation_test$growth[nrow(m7_simulation_test)]^(1/10)) - 1) * 100
m7_cagr <- round(m7_cagr, 2)

## Set up M7 simulation function using cumulative production
m7_simulation_cumprod <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>% 
    `colnames<-`("returns") %>% 
    mutate(growth = cumprod(returns)) %>%
    select(growth)
}

## Set up number of simulations and names of columns
sims <- 51
starts <-
  rep(1, sims) %>%
  set_names(paste("sim", 1:sims, sep = ""))

m7_sim_51 <- map_dfc(starts, m7_simulation_cumprod, N = 120, mean =  m7_port_mean, stdev = m7_port_stdev)

tail(m7_sim_51)

## Take a look at the final six results of the simulations
tail(m7_sim_51 %>% select('growth...1', 'growth...2', 'growth...50', 'growth...51'))

m7_sim_51 <- m7_sim_51 %>% 
  mutate(month = seq(1:nrow(.))) %>% 
  select(month, everything()) %>% 
  `colnames<-`(c("month", names(starts)))
   
## Create a graph of the simulations
m7_sim_51 %>% 
  gather(sim, growth, -month) %>% 
  group_by(sim) %>% 
  ggplot(aes(month, growth, color = sim)) +
  geom_line() +
  theme(legend.position = "none")

## Create an object with statistical summarizes of the simulation
m7_mc_summary <- m7_sim_51 %>% 
                 gather(sim, growth, -month) %>% 
                 group_by(sim) %>% 
                 summarise(final = last(growth)) %>% 
                 summarise(max = max(final),
                           min = min(final),
                           median = median(final)) 

## Take a look at the last six values of each column
tail(m7_mc_summary)

##  Create a graph of the simulation
m7_sim_51 %>% gather(sim, growth, -month) %>% 
                    group_by(sim) %>% 
                    filter(last(growth) == m7_mc_summary$max ||
                           last(growth) == m7_mc_summary$median ||
                           last(growth) == m7_mc_summary$min) %>% 
                    ggplot(aes(month, growth)) +
                    geom_line(aes(color = sim))

