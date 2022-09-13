library(tidyverse)
library(imfr)
library(zoo)
library(data360r)

get_wb_tc_dataset <- function(country) {
    
    # TCData = trade data
    # GovData = governance indicators, openness transparency etc
    # 
    
    data_0 <- get_data360(site='tc', country_iso3=c(country), timeframes=c(1980:1989), output_type='wide')
    data_1 <- get_data360(site='tc', country_iso3=c(country), timeframes=c(1990:2000), output_type='wide')
    data_2 <- get_data360(site='tc', country_iso3=c(country), timeframes=c(2001:2010), output_type='wide')
    data_3 <- get_data360(site='tc', country_iso3=c(country), timeframes=c(2011:2021), output_type='wide')
    
    merge_cols <- c('Country ISO3', 'Country Name', 'Indicator', 'Subindicator Type', 'Product', 'Partner')
    merged <- data_0 %>%
        merge(data_1, by=merge_cols, all = T) %>% 
        merge(data_2, by=merge_cols, all = T) %>%
        merge(data_3, by=merge_cols, all = T)
    
    merged$`2010` <- as.numeric(merged$`2010`)
    merged$`2012` <- as.numeric(merged$`2012`)
    merged$`2014` <- as.numeric(merged$`2014`)
    merged$`2016` <- as.numeric(merged$`2016`)
    merged$`2017` <- as.numeric(merged$`2017`)
    merged$`2018` <- as.numeric(merged$`2018`)
    merged$`2020` <- as.numeric(merged$`2020`)
    merged$`2021` <- as.numeric(merged$`2021`)
    
    long <- pivot_longer(merged, -merge_cols, names_to="year")
    
    write_csv(merged, paste0(country, "-worldbank-data.csv"))
    
    return(merged)
}

get_imf_dataset <- function(country) {
    
    frequency = "Q"
    
    ids <- c(
        current_account = "BCA_BP6_USD",
        reserve_assets = "BFRA_BP6_USD",
        goods_and_services = "BFRA_BP6_USD",
        primary_income = "BIP_BP6_USD",
        secondary_income = "BIS_BP6_USD",
        capital_account = "BK_BP6_USD",
        direct_investment = "BFD_BP6_USD",
        portfolio_investment = "BFP_BP6_USD",
        financial_derivatives_otherThan_reserves_and_emp_stockoptions = "BFF_BP6_USD",
        other_investment = "BFO_BP6_USD",
        net_errors_and_omissions = "BOP_BP6_USD"
    )
    
    df <- tibble()
    df$iso2c <- ""
    df$year_quarter <- ""
    
    for(name in names(ids)) {
        id <- ids[[name]]
        print(name)
        data <- imf_data("BOP", id, country, freq=frequency) %>% 
            rename(
                !!name := .data[[id]]
            )
        
        df <- full_join(df, data, by=c("iso2c", "year_quarter"))
        Sys.sleep(1) # so IMF doesnt get mad
    }
    
    df <- df %>% mutate(
        year_quarter = as.yearqtr(year_quarter, format="%Y-Q%q")
    )
    
    df
}

graph_indicator <- function (data, indicator) {
    
    filtered <- data %>% 
        filter((Indicator == indicator)) %>%
        select(Indicator, year, value) %>% 
        mutate(year = as.numeric(year))
    
    ggplot(data=filtered, mapping=aes(x=year, y=value)) + 
        geom_line() +
        labs(title = indicator)
}

isolate_indicators <- function(data, ...) {
    
    indicators <- list(...)
    
    filtered <- data %>% 
        filter(Indicator %in% indicators) %>% 
        select(Indicator, year, value) %>% 
        mutate(year = as.numeric(year)) %>% 
        pivot_wider(names_from = Indicator, values_from = value)
    
    filtered
}

graph_indicators <- function (data, indicator_1, indicator_2) {
   
    filtered <- isolate_indicators(data, indicator_1, indicator_2)
    
    fit <- compare_indicators(data, indicator_1, indicator_2)
    slope <- coef(fit)[[sprintf("`%s`", indicator_1)]]
    intercept <- coef(fit)[["(Intercept)"]]
    pv <- summary(fit)$coef[[sprintf("`%s`", indicator_1), "Pr(>|t|)"]]
    pv2 <- summary(fit)$coef[["(Intercept)", "Pr(>|t|)"]]
    
    ggplot(data=filtered, mapping=aes_string(x=sprintf("`%s`", indicator_1), y=sprintf("`%s`", indicator_2))) + 
        geom_point() +
        geom_smooth(se=F, method="lm", color='black', na.rm = T) +
        labs(x = indicator_1, y = indicator_2, caption=sprintf("y = %.3f x + %.3f
                                                               (%.3f)      (%.3f)", 
                                                               slope, intercept, pv, pv2))
}

graph_indicators <- function (data, indicator_1, indicator_2) {
   
    filtered <- isolate_indicators(data, indicator_1, indicator_2)
    
    fit <- compare_indicators(data, indicator_1, indicator_2)
    slope <- coef(fit)[[sprintf("`%s`", indicator_1)]]
    intercept <- coef(fit)[["(Intercept)"]]
    pv <- summary(fit)$coef[[sprintf("`%s`", indicator_1), "Pr(>|t|)"]]
    pv2 <- summary(fit)$coef[["(Intercept)", "Pr(>|t|)"]]
    
    ggplot(data=filtered, mapping=aes_string(x=sprintf("`%s`", indicator_1), y=sprintf("`%s`", indicator_2))) + 
        geom_point() +
        geom_smooth(se=F, method="lm", color='black', na.rm = T) +
        labs(x = indicator_1, y = indicator_2, caption=sprintf("y = %.3f x + %.3f
                                                               (%.3f)      (%.3f)", 
                                                               slope, intercept, pv, pv2))
}

compare_indicators <- function(data, indicator_1, indicator_2) {
    
    filtered <- isolate_indicators(data, indicator_1, indicator_2)
    
    fit <- lm(sprintf("`%s` ~ `%s`", indicator_2, indicator_1), filtered)
    fit
    
}

graph_indicators_multi <- function(data, indicator_1, indicator_2) {
    
    g1 <- graph_indicators(data, indicator_1, indicator_2)
    g2 <- graph_indicator(data, indicator_1)
    g3 <- graph_indicator(data, indicator_2)
    
    g1 | (g2 / g3)
}

search_indicators <- function(database, pattern) {

    indicators <- unique(database$indicator)
    return(indicators[grepl(pattern, indicators, ignore.case = T)])
}


