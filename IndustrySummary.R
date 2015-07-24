portfolio_return <- portfolio %>%
  group_by(End, Order) %>%
  summarise(Return = mean(IndustryReturn)) %>%
  ungroup()

p1 <- portfolio_return %>% 
  rbind(index_weekly_return %>% select(End = End, Order = Name,  Return = WeeklyReturn) %>% 
          filter(End >= portfolio_date$End[1] & End <= portfolio_date$End[length(portfolio_date$End)])) 
print(PlotCumlateReturn_ggplot(p1) + ggtitle(paste("Score", ScoreNumber, " 周")))

year_performance <- p1 %>% 
  mutate(Year = year(End)) %>%
  group_by(Order, Year) %>% 
  summarise(IR = YearlyIR(Return, 52)) %>% 
  ungroup() %>% 
  rbind(p1 %>% group_by(Order) %>% summarise(IR = YearlyIR(Return, 52)) %>% mutate(Year = "总计")) %>%
  dcast(Year ~ Order, value.var = "IR")
print(year_performance)

#####################################################################################################
# 减去指数
p2 <-  portfolio_return %>% 
  left_join(index_weekly_return %>% filter(InnerCode == 3145) %>% select(End, WeeklyReturn), by = "End") %>%
  rename(CSI300 = WeeklyReturn) %>%
  left_join(index_weekly_return %>% filter(InnerCode == 4982) %>% select(End, WeeklyReturn), by = "End") %>%
  rename(CSI500 = WeeklyReturn) 

portfolio_diff_index <- p2 %>%
  mutate(CSI300 = Return - CSI300, CSI500 = Return - CSI500) %>% 
  select(-Return) %>% 
  group_by(Order) %>% 
  arrange(End) %>% 
  mutate(CSI300 = expm1(cumsum(log1p(CSI300))),
         CSI500 = expm1(cumsum(log1p(CSI500)))) %>%
  ungroup() %>%
  melt(id = c("End", "Order")) 
 
print(ggplot(portfolio_diff_index, aes(x = End, y= value, color = Order)) + 
        geom_line() + facet_grid(. ~ variable) + xlab(NULL) + ylab(NULL))

##############################################################################################
#每个组合相减的图

p3 <- portfolio_return %>%
  dcast(End ~ Order, value.var = "Return") %>% 
  mutate(FirstMinusFifth = First - Fifth,
         SecondMinusFourth = Second - Fourth) %>% 
  select(End, FirstMinusFifth, SecondMinusFourth) %>%
  melt(id = "End")

print(PlotCumlateReturn_ggplot(p3))

