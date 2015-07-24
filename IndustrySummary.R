portfolio_return_equal_weight <- portfolio %>%
  group_by(End, Order) %>%
  summarise(PortfolioReturnEqualWeight = mean(IndustryReturn)) %>%
  ungroup()
  
# portfolio_date <- unique(portfolio %>% select(Start, End))
# portfolio_return_value_weight <- data.frame()
# for(i in c(1:nrow(date_value))){
#   date <- portfolio_date$Start[i]
#   temp <- portfolio %>% 
#     filter(Start == date) %>%
#     select(Start, End, IndustryNameNew, Order) %>% 
#     left_join(data$ReturnDaily %>% filter(TradingDay == date), 
#               by = c("Start" =  "TradingDay", "IndustryNameNew" = "IndustryNameNew")) %>%
#     semi_join(data$IndexComponent %>% 
#                 filter(IndexInnerCode == nIndexCode, date >= InDate & date < OutDate),
#               by = c("InnerCode" = "SecuInnerCode")) %>%
#     left_join(stock_weekly_return, by = c("InnerCode", "Start", "End")) %>%
#     group_by(End, Order) %>%
#     filter(!is.na(WeeklyReturn)) %>% 
#     summarise(PortfolioReturnValueWeight = weighted.mean(WeeklyReturn, FloatMarketCap, na.rm = TRUE)) %>%
#     ungroup()
#   
#   portfolio_return_value_weight <- rbind(portfolio_return_value_weight, temp)
# }

p1 <- portfolio_return_equal_weight %>% 
  rename(Return = PortfolioReturnEqualWeight) %>% 
  rbind(index_weekly_return %>% select(End = End, Order = Name,  Return = WeeklyReturn) %>% 
          filter(End >= portfolio_date$End[1] & End <= portfolio_date$End[length(portfolio_date$End)])) %>%
  group_by(Order) %>% 
  arrange(End) %>% 
  mutate(CumualteReturn = expm1(cumsum(log1p(Return)))) %>%
  ungroup() 

ggplot(p1, aes(x = End, y = CumualteReturn, color = Order)) + geom_line() + 
  xlab(NULL) + ylab(NULL) + ggtitle(paste("等权重，Score", ScoreNumber, " 周"))

# p2 <-   portfolio_return_value_weight %>%
#   rename(Return = PortfolioReturnValueWeight) %>% 
#   rbind(index_weekly_return %>% select(End = End, Order = Name,  Return = WeeklyReturn) %>% 
#           filter(End >= portfolio_date$End[1] & End <= portfolio_date$End[length(portfolio_date$End)])) %>%
#   group_by(Order) %>% 
#   arrange(End) %>% 
#   mutate(CumualteReturn = expm1(cumsum(log1p(Return)))) %>%
#   ungroup() 
# 
# ggplot(p2, aes(x = End, y = CumualteReturn, color = Order)) + geom_line() + 
#   xlab(NULL) + ylab(NULL) + ggtitle(paste("市值权重，Score", ScoreNumber, " 周"))


p3 <-  portfolio_return_value_weight %>% 
  left_join(portfolio_return_equal_weight, by = c("End", "Order")) %>%
  rename(ValueWeight = PortfolioReturnValueWeight, EqualWeight = PortfolioReturnEqualWeight) %>%
  left_join(index_weekly_return %>% filter(InnerCode == 3145) %>% select(End, WeeklyReturn), by = "End") %>%
  rename(CSI300 = WeeklyReturn) %>%
  left_join(index_weekly_return %>% filter(InnerCode == 4982) %>% select(End, WeeklyReturn), by = "End") %>%
  rename(CSI500 = WeeklyReturn) 

temp <- p3 %>% 
  melt(id = c("End", "Order")) %>% 
  group_by(Order, variable) %>% 
  arrange(End) %>%
  mutate(CumulateReturn = expm1(cumsum(log1p(value))))%>% 
  ungroup()
ggplot(temp , aes(x = End, y = CumulateReturn, color = variable)) + 
  geom_line() + 
  facet_grid(Order ~., scales = "free") +
  xlab(NULL) + ylab(NULL) 
  
portfolio_diff300 <- p3 %>%
  mutate(ValueWeight = ValueWeight - CSI300, EqualWeight = EqualWeight - CSI300) %>% 
  select(End, Order, ValueWeight, EqualWeight) %>% 
  mutate(ValueWeight =  expm1(cumsum(log1p(ValueWeight))),
         EqualWeight = expm1(cumsum(log1p(EqualWeight)))) %>%
  melt(id = c("End", "Order"))
        
ggplot(portfolio_diff300, aes(x = End, y= value, color = Order)) + geom_line() + facet_grid(. ~ variable)
#########################


#每个组合相减的图


  portfolip_return_level_summary <- p3 %>%
  group_by(variable, Order) %>%
  summarise(Max = max(value), Min = min(value), Mean = mean(value),
            SD = sd(value), IR = YearlyIR(value, 52))


portfolip_return_level_first_minus_fifth <- portfolip_return_level %>%
  filter(Order %in% c("2", "4")) %>%
  dcast(End ~ Order, value.var = "PortfolioReturn")
names(portfolip_return_level_first_minus_fifth) <- c("Date", "First", "Fifth")
portfolip_return_level_first_minus_fifth <- portfolip_return_level_first_minus_fifth %>%
  mutate(FirstMinusFifth = First - Fifth) %>%
  arrange(Date) %>%
  mutate(CumlateRetun = expm1(cumsum(log1p(FirstMinusFifth))))
ggplot(portfolip_return_level_first_minus_fifth, aes(x = Date, y = CumlateRetun)) + geom_line() +
  ggtitle("first_minus_fifth") + xlab("Date")


portfolip_return_level_minus_mean <-  portfolip_return_level %>%
  group_by(End) %>%
  mutate(DeMeanReturn = PortfolioReturn - mean(PortfolioReturn)) %>%
  group_by(Order) %>%
  arrange(End) %>%
  mutate(CumlateRetun = expm1(cumsum(log1p(DeMeanReturn))))
ggplot(portfolip_return_level_minus_mean, aes(x = End, y = CumlateRetun, color = Order)) + geom_line() +
  ggtitle("minus_mean") + xlab("Date")

