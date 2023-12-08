library(tidyverse)
setwd("C:/Users/PC/OneDrive/BES_Robo/Option Data")

load("option_data_full.Rdata")

dailyend5 %>% left_join(.,dailyend_return %>% select(Date,option_symbol,Payoff,Return),
                        by=c("option_symbol","Date")) %>%
  mutate(Return_2 = ((Payoff-Option_price)/Stock_price),
         delta =abs(delta)) %>% 
  select(-c(value,moneyness,Return)) -> dailyend6



strategy_option <- function(type,param,param_type,maturity=1/12){

  dailyend6 %>% filter(call_put ==type,
                       ttm < (maturity + 10/365),
                       ttm> (maturity - 10/365)) -> pre_param
  
  if (param_type == "moneyness") {
    
    pre_param %>% group_by(Date) %>%
      mutate(param_diff = abs(moneyness_sigma - param)) %>% arrange(desc(Option_price)) %>% 
      slice_min(param_diff,with_ties = F) -> dat
    
  }else if(param_type == "delta"){
    
    pre_param %>% group_by(Date) %>%
      mutate(param_diff = abs(delta - param)) %>% arrange(desc(Option_price)) %>% 
      slice_min(param_diff,with_ties = F) -> dat
    
  }else{
    stop("Invalid param_type.")
  }

  return(dat)
}


#strategy_option(type = "call",param = 0.5,param_type = "delta")

type = c("call","put")
deltas <- c(0.05,0.1,0.2,0.25,0.50,0.75,0.80,0.90,0.95)
moneyness <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)

for (t in type) {
  for (d in deltas) {
    dat <- strategy_option(type=t,param=d,param_type = "delta")
    assign(x = paste0(t,"_delta_",d),dat)
    
  }
}


for (t in type) {
  for (m in moneyness) {
    dat <- strategy_option(type=t,param=m,param_type = "moneyness")
    assign(x = paste0(t,"_moneyness_",m),dat)
    rm(dat)
  }
}


library(grid)
library(gridExtra)

pdf("Options.pdf")
#plots

library(zoo)
Tibble_To_Zoo<-function(df) zoo(as.data.frame(df)[,-1],as.Date(as.data.frame(df)[,1]))
TwoVarPlot<-function(plotdata,title="",leg1="",leg2=""){
  oldpar<-par()
  par(mar=c(2,3,2,3))
  plot(plotdata[,1],main=title,xlab="",ylab="",col="red",las=1)
  axis(2,col.axis="red",col="red",las=1)
  par(new=TRUE)
  plot(plotdata[,2],xaxt="n", yaxt="n",col="blue",ylab="",xlab="")
  axis(4,col.axis="blue",col="blue",las=1)
  #par(xpd=FALSE)
  legend("topleft", c(leg1,leg2), text.col=c("red","blue"),cex=0.8,bty="n")
  options(warn=-1)
  try(suppressWarnings(par(oldpar)),silent = T)
  options(warn=0)
}

##
dailyend6 %>% select(Date) %>% distinct() %>% arrange() %>% pull() -> dates
res <- tibble(Date=as.Date(character()),Type=character(),Delta=numeric(),Impvol=numeric())
for (date in dates) {
  print(date)
  for (delt in deltas) {
    for (type in c("call","put")) {
      i <-get(paste0(type,"_delta_",delt)) %>% filter(Date==as.Date(date)) %>% pull(Impvol)
      res <- rbind(res,tibble(Date=as.Date(date),Type=type,Delta=delt,Impvol=i))
    }
  }
  
}
grid.newpage()
grid.text(label = "Impvol Smiles")
for (date in dates) {
  p <- res %>% filter(Date==as.Date(date)) %>% mutate(Delta=ifelse(Type=="call",1-Delta,Delta)) %>%
    ggplot(aes(Delta,Impvol,color=Type)) + geom_line() +
    xlab("Delta for put / 1-Delta for call") + labs(title = as.Date(date),
                                                    subtitle = "x axis represents strike price, increasing")
  print(p)
}



grid.newpage()
grid.text(label = "Option Volatilities")


#option volatilities
#ATM
call_delt_vec <- ls() %>% str_subset("call_delta")
count=0
for (cd in call_delt_vec) {
  dat <- get(cd) %>% select(Date,Impvol) %>% Tibble_To_Zoo()
  if (count==0) {
    plotdat <- dat
    count= count+1
    next
  }
  plotdat <- merge.zoo(plotdat,dat)
  count= count+1
}
plot(plotdat,screens=1,col=1:length(call_delt_vec),ylab="ImpVol",xlab="Date",main="Call",
     sub="Parameter: Delta")
legend("topleft",legend = deltas,col=1:length(call_delt_vec),lty=1)


put_delt_vec <- ls() %>% str_subset("put_delta")
count=0
for (cd in put_delt_vec) {
  dat <- get(cd) %>% select(Date,Impvol) %>% Tibble_To_Zoo()
  if (count==0) {
    plotdat <- dat
    count= count+1
    next
  }
  plotdat <- merge.zoo(plotdat,dat)
  count= count+1
}
plot(plotdat,screens=1,col=1:length(put_delt_vec),ylab="ImpVol",xlab="Date",main="Put",
     sub="Parameter: Delta")
legend("topright",legend = deltas,col=1:length(put_delt_vec),lty=1)

#atm2
atmimpvol <- call_delta_0.5 %>% select(Date,Impvol) %>% left_join(.,put_delta_0.5 %>% select(Date,Impvol),
                                                     by="Date",suffix=c(".call",".put")) 
p <- atmimpvol %>%  ggplot(aes(x=Date)) + geom_line(aes(y=Impvol.call,color="Call")) +
  geom_line(aes(y=Impvol.put,color="Put")) +labs(title = "ATM",xlab="Impvol")
print(p)


#RiskReversal

rr_deltas <- c(0.05,0.1,0.2,0.25)
for (delt in rr_deltas) {
  p <-get(paste0("call_delta_",delt)) %>%
    select(Date,Impvol) %>% left_join(.,
                                      get(paste0("put_delta_",delt)) %>%
                                        select(Date,Impvol),by="Date",suffix=c(".call",".put")) %>% 
    mutate(Impvol_diff=Impvol.call-Impvol.put) %>% ggplot(aes(Date,Impvol_diff)) + geom_line() +
    labs(xlab="Impvol",title = "RiskReversal",
         subtitle = paste0("Call delta ",delt," - Put delta ",delt))
  print(p)
}







#butterFLY

for (delt in rr_deltas) {
  dat <-get(paste0("call_delta_",delt)) %>%
    select(Date,Impvol) %>% left_join(.,
                                      get(paste0("put_delta_",delt)) %>%
                                        select(Date,Impvol),by="Date",suffix=c(".call",".put")) %>% 
    left_join(.,atmimpvol,by="Date",suffix=c(".tail",".atm")) %>% 
    mutate(Impvol_diff=Impvol.call.tail+Impvol.put.tail - Impvol.call.atm-Impvol.put.atm) %>% 
    mutate(Impvol_diff_call=Impvol.call.tail-Impvol.call.atm,
           Impvol_diff_put= Impvol.put.tail- Impvol.put.atm)
  
  p <-  dat %>% ggplot(aes(Date,Impvol_diff_call)) + geom_line() +
    labs(xlab="Impvol",title="Call Spread",
         subtitle = paste0("Call ",delt,"- Call 0.5"))
  print(p)
  
  p <-  dat %>% ggplot(aes(Date,Impvol_diff_put)) + geom_line() +
    labs(xlab="Impvol",title="Put Spread",
         subtitle = paste0("Put ",delt,"- Put 0.5"))
  print(p)
  
  p <-  dat %>% ggplot(aes(Date,Impvol_diff)) + geom_line() +
    labs(xlab="Impvol",title="Butterfly",
         subtitle = paste0("Call ",delt,"- Call 0.5 - Put 0.5 ", "+ Put ",delt))
  print(p)
}


grid.newpage()
grid.text(label = "Basic Strategy Returns")

deltavec <- ls() %>% str_subset("delta_")

for (d in deltavec) {
  dat <- get(d) %>%
    ungroup %>%  na.omit %>% 
    mutate(Return=cumsum(Return_2)) %>%
    select(Date,Return,Stock_price) %>%
    Tibble_To_Zoo()
  plot <- TwoVarPlot(dat,title = d,leg1 = "Option Str. Return",leg2 = "SPY")
  print(plot)
}


moneyvec <- ls() %>% str_subset("moneyness_")
for (m in moneyvec) {
  dat <- get(m) %>%
    ungroup %>%  na.omit %>% 
    mutate(Return=cumsum(Return_2)) %>%
    select(Date,Return,Stock_price) %>%
    Tibble_To_Zoo()
  plot <- TwoVarPlot(dat,title = m,leg1 = "Option Str. Return",leg2 = "SPY")
  print(plot)
}



grid.newpage()
grid.text(label = "Complex Strategy Returns")

## returns

atm_straddle <- tibble(Date=call_delta_0.5$Date,
                       Return=call_delta_0.5$Return_2 + put_delta_0.5$Return_2)
atm_straddle_p <- atm_straddle %>% 
                    ggplot(aes(x=Date,y=Return)) + geom_line(aes(y=cumsum(Return)),size=0.9,col="salmon") +
  geom_col(aes(y=Return),alpha=0.8) +
                    labs(title="ATM Straddle",
                         subtitle = "long 0.5 Delta call + long 0.5 Delta Put")
for (d in deltas) {
  if (d >= 0.5) {
    next
  }
  date <- get(paste0("call_delta_",d)) %>% .$Date
  dat <- tibble(Date=date,
                Return=get(paste0("call_delta_",d)) %>% .$Return_2 + 
                                get(paste0("put_delta_",d)) %>% .$Return_2)
  dat_p <- dat %>% ggplot(aes(x=Date,y=Return)) + geom_line(aes(y=cumsum(Return)),size=0.9,col="salmon") +
    geom_col(aes(y=Return),alpha=0.8)+
    labs(title=paste0("Strangle ",d),
         subtitle = paste0("long ",d," Delta call + long ",d," Delta Put"))
  assign( paste0("Strangle_",d),dat)
  assign(paste0("Strangle_",d,"_p"),dat_p)
  
  
  dat <- left_join(get(paste0("Strangle_",d)),atm_straddle,by="Date",suffix=c(".delta",".atm")) %>% 
    mutate(Return=Return.delta-Return.atm) %>% select(Date,Return)
  dat_p <- dat %>% ggplot(aes(x=Date,y=Return)) + geom_line(aes(y=cumsum(Return)),size=0.9,col="salmon") +
    geom_col(aes(y=Return),alpha=0.8) + 
    labs(title=paste0("Butterfly ",d),
         subtitle = paste0("long ",d," Delta strangle + short 0.5 Delta straddle"))
  
  assign( paste0("Butterfly_",d),dat)
  assign(paste0("Butterfly_",d,"_p"),dat_p)
  
  
  dat <- tibble(Date=date,
                Return=call_delta_0.5$Return_2 - 
                                get(paste0("call_delta_",d)) %>% .$Return_2)
  dat_p <- dat %>% ggplot(aes(x=Date,y=Return)) +geom_line(aes(y=cumsum(Return)),size=0.9,col="salmon") +
    geom_col(aes(y=Return),alpha=0.8)+
    labs(title=paste0("Bull Call Spread ",d),
         subtitle = paste0("long 0.5 Delta call + short ",d," Delta call"))
  
  assign( paste0("Bull_Call_Spread_",d),dat)
  assign(paste0("Bull_Call_Spread_",d,"_p"),dat_p)
  
  dat <- tibble(Date=date,
                Return=get(paste0("put_delta_",d)) %>% .$Return_2 - 
                                put_delta_0.5$Return_2)
  dat_p <- dat %>% ggplot(aes(x=Date)) +geom_line(aes(y=cumsum(Return)),size=0.9,col="salmon") +
    geom_col(aes(y=Return),alpha=0.8) +
    labs(title=paste0("Bull Put Spread ",d),
         subtitle = paste0("short 0.5 Delta put + long ",d," Delta put"))
  
  assign( paste0("Bull_Put_Spread_",d),dat)
  assign(paste0("Bull_Put_Spread_",d,"_p"),dat_p)
  
  dat <- tibble(Date=date,
                Return=get(paste0("call_delta_",d)) %>% .$Return_2 - 
                                get(paste0("put_delta_",d)) %>% .$Return_2)
  dat_p <- dat %>% ggplot(aes(x=Date,y=Return)) + geom_line(aes(y=cumsum(Return)),size=0.9,col="salmon") +
    geom_col(aes(y=Return),alpha=0.8)+ 
    labs(title=paste0("Risk Reversal ",d),
         subtitle = paste0("long ",d," Delta call + short ",d," Delta Put"))
  assign( paste0("Risk_Reversal_",d),dat)
  assign(paste0("Risk_Reversal_",d,"_p"),dat_p)
  
  
  
  
  dat <- left_join(get(paste0("Strangle_",d)),atm_straddle,by="Date",suffix=c(".delta",".atm")) %>% 
    mutate(Return=2*Return.delta-Return.atm) %>% select(Date,Return)
  dat_p <- dat %>% ggplot(aes(x=Date,y=Return)) + geom_line(aes(y=cumsum(Return)),size=0.9,col="salmon") +
    geom_col(aes(y=Return),alpha=0.8) + 
    labs(title=paste0("Christmas Tree ",d),
         subtitle = paste0("long 2x ",d," Delta strangle + short 0.5 Delta straddle"))
  
  assign( paste0("Christmas_Tree_",d),dat)
  assign(paste0("Christmas_Tree_",d,"_p"),dat_p)
  

  
  rm(dat,dat_p)

}



p_vec <- c("atm_straddle_p","Strangle_0.05_p","Strangle_0.1_p","Strangle_0.2_p","Strangle_0.25_p",
  "Risk_Reversal_0.05_p","Risk_Reversal_0.1_p","Risk_Reversal_0.2_p","Risk_Reversal_0.25_p",
  "Bull_Call_Spread_0.05_p","Bull_Call_Spread_0.1_p","Bull_Call_Spread_0.2_p","Bull_Call_Spread_0.25_p",
  "Bull_Put_Spread_0.05_p","Bull_Put_Spread_0.1_p","Bull_Put_Spread_0.2_p","Bull_Put_Spread_0.25_p",
  "Butterfly_0.05_p","Butterfly_0.1_p","Butterfly_0.2_p","Butterfly_0.25_p",
  "Christmas_Tree_0.05_p","Christmas_Tree_0.1_p","Christmas_Tree_0.2_p","Christmas_Tree_0.25_p")
for (p in p_vec) {
  print(get(p))
}


for (d in c(0.05,0.1,0.2)){
  date <- get(paste0("call_delta_",d)) %>% .$Date
  dat <- tibble(Date=date,
                Return=get(paste0("put_delta_",d)) %>% .$Return_2 - 
                  put_delta_0.25$Return_2)
  dat_p <- dat %>% ggplot(aes(x=Date)) +geom_line(aes(y=cumsum(Return)),size=0.9,col="salmon") +
    geom_col(aes(y=Return),alpha=0.8) +
    labs(title=paste0("Bull Put Spread ",d," - 0.25"),
         subtitle = paste0("short 0.25 Delta put + long ",d," Delta put"))
  assign( paste0("Bull_Put_Spread_",d,"_0.25"),dat)
  assign(paste0("Bull_Put_Spread_",d,"_0.25","_p"),dat_p)
  
  dat <- tibble(Date=date,
                Return=call_delta_0.25$Return_2 - get(paste0("call_delta_",d)) %>% .$Return_2)
  dat_p <- dat %>% ggplot(aes(x=Date)) + geom_line(aes(y=cumsum(Return)),size=0.9,col="salmon") +
    geom_col(aes(y=Return),alpha=0.8) +
    labs(title=paste0("Bull Call Spread ",d," - 0.25"),
         subtitle = paste0("long 0.25 Delta call + short ",d," Delta call"))
  assign( paste0("Bull_Call_Spread_",d,"_0.25"),dat)
  assign(paste0("Bull_Call_Spread_",d,"_0.25","_p"),dat_p)
}

for (p in c("Bull_Call_Spread_0.05_0.25_p","Bull_Call_Spread_0.1_0.25_p","Bull_Call_Spread_0.2_0.25_p",
            "Bull_Put_Spread_0.05_0.25_p","Bull_Put_Spread_0.1_0.25_p","Bull_Put_Spread_0.2_0.25_p")) {
  print(get(p))
}


a <- left_join(put_delta_0.1 %>%
            select(Date,Return_2),
          put_delta_0.25%>%
            select(Date,Return_2),by="Date",suffix = c(".10",".25")) %>%
  left_join(.,call_delta_0.5 %>% select(Date,Return_2),by="Date") %>%
  left_join(.,put_delta_0.5 %>% select(Date,Return_2),by="Date",suffix=c(".atm_call",".atm_put")) %>%
  mutate(Return=Return_2.10-Return_2.25+Return_2.atm_call-Return_2.atm_put) %>%
  mutate(Return_bench=Return_2.atm_call-Return_2.atm_put) %>%
  select(Date,Return,Return_bench) %>%
  ggplot() + geom_line(aes(Date,cumsum(Return)),size=0.9,col="salmon") + geom_col(aes(Date,y=Return),alpha=0.8) +
  labs(title="Put_spread_0.1_0.25_s&p",
       subtitle = "Long 0.1 put, short 0.25 put, long S&P(long call atm & short put atm)")
print(a)

dev.off()
