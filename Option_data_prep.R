library(tidyverse)
library(readxl)

library(RQuantLib)

setwd("C:/Users/PC/OneDrive/BES_Robo/Option Data/Excels")
list.files() -> files

for (e in files) {
  print(e)
  dat <- read_csv(e,show_col_types = F)
  if (e == "LIBOR.xlsx" | e == "Option_data_prep.R" | e == "option_data_full.Rdata" ) {
    next
  }else if (grepl("1545",e)) {
    if (exists("dailye1545")) {
      daily1545 <- rbind(daily1545,dat)
    }else{ 
      daily1545 <- dat
    }
  }else{

    if (exists("dailyend")) {
      dailyend <- rbind(dailyend,dat)
    }else{ 
      dailyend <- dat
    }
  }
}


LIBOR <-read_xlsx("LIBOR.xlsx") %>% select(-1)
colnames(LIBOR) <- c("Date","LIBOR1M","LIBOR3M","LIBOR6M","LIBOR12M","SOFR1M","SOFR3M","SOFR6M","SOFR12M")

Riskfree <- LIBOR %>% mutate(Date=as.Date(Date)) %>%
  mutate(RF1M= ifelse(Date >= as.Date("2021-12-31"),SOFR1M,LIBOR1M),
         RF3M= ifelse(Date >= as.Date("2021-12-31"),SOFR3M,LIBOR3M),
         RF6M= ifelse(Date >= as.Date("2021-12-31"),SOFR6M,LIBOR6M),
         RF12M= ifelse(Date >= as.Date("2021-12-31"),SOFR12M,LIBOR12M)) %>% 
  select(Date,RF1M,RF3M,RF6M,RF12M)

setwd("C:/Users/PC/OneDrive/BES_Robo/Option Data")





resp_rf <- function(date,exp){
  ttm <- as.double(exp-date)
  col_i <- which.min(abs(c("RF1M"=30,"RF3M"=91,"RF6M"=182,"RF12M"=365) - ttm))
  row_i <- which(Riskfree$Date == date)
  res <- riskfree_mat[row_i,col_i]
  return(res)
}



dailyend %>% mutate(Date=as.Date(date,format="%m/%d/%Y"),
                    Expiration = as.Date(option_expiration,format="%m/%d/%Y")) %>% 
  select(Date,stock_price_close,Expiration,strike,call_put,style,mean_price,option_symbol) %>% 
  rename(Strike=strike,Option_price=mean_price,Stock_price=stock_price_close)  %>% distinct() -> dailyend2


Riskfree <- dailyend2 %>% distinct(Date) %>%arrange() %>%
  left_join(Riskfree,by="Date") %>% fill(RF1M:RF12M) %>%
  mutate_at(vars(RF1M:RF12M),as.numeric)
  
riskfree_mat <- Riskfree[,2:5] %>% as.matrix()


dailyend2 %>% group_by(Expiration) %>%
  summarise(count=n()) %>% mutate(yearmon=as.yearmon(Expiration)) %>%
  group_by(yearmon) %>% slice_max(count) %>% pull(Expiration) -> Expiration_dates



dailyend2  %>% filter(Date %in% Expiration_dates,Expiration %in% Expiration_dates)%>% 
  mutate(ttm = as.double(Expiration-Date)/365) %>% 
  filter(ttm > (16/365),Option_price > 0) %>% 
  mutate(rf = (map2_dbl(Date,Expiration,.f = resp_rf))/100) %>% 
  mutate(call_put = ifelse(call_put == "C","call",ifelse(call_put=="P","put",NA))) -> dailyend3

# %>% slice(rnorm(500,6000000,1000000) %>% round(0) %>%abs() %>%  unique())


adj_AmericanOptionImpliedVolatility <- function(type,value,underlying,strike,
                                                riskFreeRate,maturity,dividendYield = 0,volatility = 0.18){
  
  if (type=="call") {
    min_bound <- underlying - strike*exp(-riskFreeRate*maturity)
    max_bound <- underlying
  }else{
    min_bound <- strike - underlying
    max_bound <- strike
  }
  
  if (value < min_bound) {
    res <- NA
  }else if (value > max_bound) {
    res <- NA
  }else{
    res <- AmericanOptionImpliedVolatility(type = type,value = value,underlying = underlying,strike = strike,
                                           dividendYield = dividendYield,volatility = volatility,riskFreeRate = riskFreeRate,
                                          maturity=maturity) %>% (function(L) L[[1]] )
  }
  return(res)
  
  
}



dailyend4 <- dailyend3 %>% 
  mutate(Impvol = pmap_dbl(.l=list(call_put,Option_price,Stock_price,Strike,rf,ttm),
                           ~ adj_AmericanOptionImpliedVolatility(type=..1,value = ..2,underlying = ..3,strike = ..4,
                                                             riskFreeRate = ..5,maturity = ..6,dividendYield = 0,volatility = 0.18))) %>% 
  na.omit() %>% 
  mutate(stats = pmap(.l=list(call_put,Stock_price,Strike,rf,ttm,Impvol),~AmericanOption(type=..1,underlying = ..2,strike = ..3,riskFreeRate = ..4,
                                                                                         maturity = ..5, volatility= ..6,dividendYield = 0,engine="CrankNicolson") %>%
                        (function(L) as.list(L)) )) %>% 
  mutate(stats = map(stats,unlist))


atm_call <- dailyend4 %>%  filter(call_put =="call") %>%
  mutate(atm=abs(Stock_price - Strike),
         attm=abs(ttm-(91/365))) %>% group_by(Date) %>%
  slice_min(atm) %>% slice_min(attm) %>% slice_max(Option_price) %>% ungroup() %>% 
  select(Date,Impvol) %>% rename(atm_call_impvol=Impvol)



dailyend5 <- dailyend4 %>% left_join(.,atm_call,by="Date") %>% 
  unnest_wider(stats) %>% mutate(delta = ifelse(delta > 1,1,ifelse(delta< (-1),-1,delta))) %>% 
  select(-c(vega, theta, rho,divRho)) %>% 
  mutate(moneyness = (Strike / Stock_price) -1, moneyness_sigma = moneyness / (atm_call_impvol*sqrt(ttm)) ) %>% 
  arrange(Date)



targetdeltas <- c(0.05,0.1,0.2,0.25,0.50,0.75,0.8,0.9,0.95)
targetdeltas_NA <- rep(NA,length(targetdeltas))



get_delta <- function(delta,impvol,type){
  #browser()
  res <- window(na.locf(na.locf(na.approx(zoo(c(impvol,targetdeltas_NA),c(delta,targetdeltas)),
                                          na.rm=FALSE),na.rm=F),fromLast=T),targetdeltas) %>%
    as.vector() %>% tibble(names= paste0("iv_",substr(type[1],1,1),"_",targetdeltas*100),value=.)
  
  return(res)
}



dailyend_impvol <- dailyend5 %>% group_by(Date,call_put) %>%
  summarise(stats = get_delta(delta,Impvol,call_put),.groups = "keep") %>% 
  unnest(stats) %>% ungroup %>% select(-call_put) %>% pivot_wider(names_from = names,values_from = value)


underlyingprice <- dailyend2 %>% select(Date,Stock_price) %>%
  distinct() %>% rename(Stock_price_exp = Stock_price,Expiration = Date)

dailyend_return <-
    dailyend2 %>% left_join(.,underlyingprice,by="Expiration") %>% 
    mutate(Payoff = ifelse(call_put == "P",Strike-Stock_price_exp,
                           ifelse(call_put == "C",Stock_price_exp-Strike,NA))) %>%
    mutate(Payoff= ifelse(Payoff < 0,0,Payoff),
           Return=ifelse(Payoff == 0,0,Payoff/Option_price -1)) %>% 
    mutate(ttm = as.double(Expiration-Date)/365) %>% 
    filter(ttm > (16/365),Option_price > 0)

save(list = c("dailyend_impvol","dailyend_return","underlyingprice","dailyend5"),file = "option_data_full.Rdata")


#şimdilik buradayız

library(microbenchmark)
AmericanOption(type = "call",222,216,dividendYield = 0,
               riskFreeRate = 0.0165,volatility = 0.126,maturity =1.27,engine="CrankNicolson") -> abc
abc















