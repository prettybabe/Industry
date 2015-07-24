# Industry Return

source('D:/working/R/MyFunction.R')
library("dplyr", lib.loc="~/R/win-library/3.1")
library("RMySQL", lib.loc="~/R/win-library/3.1")
library("TTR", lib.loc="~/R/win-library/3.1")
library("reshape2", lib.loc="~/R/win-library/3.1")
library("xlsx", lib.loc="~/R/win-library/3.1")
library("RSQLServer", lib.loc="~/R/win-library/3.1")
library("rCharts", lib.loc="~/R/win-library/3.1")
library("ggplot2", lib.loc="~/R/win-library/3.1")
load("D:/working/Data/data.RData")


#########################################################################################################
nIndexCode <- 4982  # 确认市场指数， 全流通 4088，中证500 4978， 中证800 4982， 沪深300 3145

########################################################################################################
#  确定交易时间和周期, 先尝试周度
startdate <- as.Date("2007-01-15")
enddate <- as.Date("2015-6-29")

trading_date <- data$TradingDay %>%
  filter(IfWeekEnd == 1, TradingDate >= startdate & TradingDate <= enddate) %>%
  select(TradingDate) %>%
  mutate(Start = lag(TradingDate)) %>%
  rename(End =  TradingDate) %>%
  select(Start, End) %>%
  na.omit()

#######################################################################################
# 用日度收益率试一下与现有行业指数的相关性

industry_return_daily <- data$ReturnDaily %>%
  inner_join(data$IndexComponent %>% filter(IndexInnerCode == nIndexCode),
          by = c("InnerCode" = "SecuInnerCode")) %>%
  filter(TradingDay >= InDate & TradingDay < OutDate) %>% # 选成分股
  group_by(InnerCode) %>%
  arrange(TradingDay) %>%
  mutate(LagFloatMarketCap = lag(FloatMarketCap, 1)) %>%
  filter(!is.na(LagFloatMarketCap)) %>%
  group_by(IndustryNameNew, IndustryCodeNew, TradingDay) %>%
  summarise(IndustryReturn = weighted.mean(DailyReturn, sqrt(LagFloatMarketCap))) %>%
  ungroup()
  
data$IndexQuote <- data$IndexQuote %>% 
  group_by(InnerCode) %>% 
  arrange(TradingDay) %>% 
  mutate(Lag = lag(ClosePrice)) %>% 
  mutate(Return = ClosePrice/Lag - 1) %>% 
  ungroup() %>%
  na.omit()  


#与现有行业指数比较日度收益率

corralation_daily <- industry_return_daily %>%
  mutate(IndustryCodeNew = as.numeric(IndustryCodeNew)) %>%
  inner_join(data$CorrIndexIndustry %>% filter(IndustryStandard == 24) %>% select(IndexCode, IndustryCode), 
            by = c("IndustryCodeNew" = "IndustryCode")) %>%
  inner_join(data$IndexQuote %>% select(InnerCode, TradingDay, Return), 
             by = c("IndexCode" = "InnerCode", "TradingDay")) %>%
  group_by(TradingDay) %>%
  mutate(IndustryMinusMean = IndustryReturn - mean(IndustryReturn),
         IndexMinusMean = Return - mean(Return)) %>%
  group_by(IndustryNameNew) %>%
  summarise(Corralation = cor(IndustryMinusMean, IndexMinusMean)) %>%
  arrange(Corralation)

#######################################################################################################
# 计算各行业周度收益率, 应该计算每周五的股票，以周五的流通市值计算下周的收益率
industry_return <- data.frame()
for(i in c(1:nrow(trading_date))){
  start <- trading_date[[i, 1]]
  end <- trading_date[[i, 2]]
  industry_return_temp <- data$ReturnDaily %>%
    semi_join(data$IndexComponent %>% 
                filter(IndexInnerCode == nIndexCode, start >= InDate & start < OutDate),
              by = c("InnerCode" = "SecuInnerCode")) %>%  # 挑出成分股
    filter(TradingDay >= start & TradingDay <= end) %>% # 筛选日期
    group_by(IndustryCodeNew, IndustryNameNew, TradingDay) %>% 
    group_by(InnerCode) %>%
    arrange(TradingDay) %>%
    mutate(Weight = first(FloatMarketCap), ReturnStartDate = first(TradingDay),
           ReturnLastDate = last(TradingDay)) %>% 
    filter(TradingDay != start, ReturnLastDate == end) %>%
    group_by(InnerCode, ReturnStartDate, ReturnLastDate, Weight) %>%
    summarise(WeeklyReturn = expm1(sum(log1p(DailyReturn)))) %>%
    left_join(data$ReturnDaily %>% filter(TradingDay == start) %>% select(InnerCode, IndustryCodeNew, IndustryNameNew),
              by = "InnerCode") %>% 
    group_by(ReturnStartDate, ReturnLastDate, IndustryCodeNew, IndustryNameNew) %>%
    summarise(IndustryWeeklyReturn = weighted.mean(WeeklyReturn, Weight),
              Count = n()) %>%
    ungroup()
  
  industry_return <- rbind(industry_return, industry_return_temp)
}

industry_return <- industry_return %>%
  group_by(IndustryCodeNew) %>%
  arrange(ReturnLastDate) %>%
  mutate(IndustryWeeklyCumulateReturn =  expm1(cumsum(log1p(IndustryWeeklyReturn))))

cumulate_return_plot <- ggplot(industry_return, aes(x = ReturnLastDate, y = IndustryWeeklyCumulateReturn, colour = IndustryNameNew)) +
  geom_line() + xlab(NULL) + ylab(NULL) + ggtitle("Industry Weekly Cumulate Return")
print(cumulate_return_plot)


industry_return_summary <- industry_return %>%
  group_by(IndustryNameNew) %>%
  summarise(Max = max(IndustryWeeklyReturn), Min = min(IndustryWeeklyReturn), 
            Mean = mean(IndustryWeeklyReturn), SD = sd(IndustryWeeklyReturn),
            Count = mean(Count), YearlyReturn = YearlyReturn(IndustryWeeklyReturn, 52),
            YearlySD = YearlySD(IndustryWeeklyReturn, 52), YearlyIR = YearlyIR(IndustryWeeklyReturn, 52)) %>%
  arrange(Mean)

temp <- melt(industry_return_summary, id = "IndustryNameNew")
ggplot(temp, aes(x = IndustryNameNew, y = value)) + geom_bar(stat = "identity", fill = "Blue") +
  facet_grid(variable ~ ., scale = "free_y") + ylab(NULL) + xlab(NULL) + ggtitle("Industry Weekly Return")

qplot(IndustryNameNew, IndustryWeeklyReturn, data = industry_return, geom = "boxplot") + xlab(NULL)

#
# 与现有行业指数比较周度收益率

corralation <- industry_return %>%
  ungroup() %>%
  mutate(IndustryCodeNew = as.numeric(IndustryCodeNew)) %>%
  left_join(data$CorrIndexIndustry %>% filter(IndustryStandard == 24) %>% select(IndexCode, IndustryCode), 
            by = c("IndustryCodeNew" = "IndustryCode")) %>%
  inner_join(data$IndexQuote %>% select(InnerCode, ClosePrice, TradingDay), 
             by = c("IndexCode" = "InnerCode", "ReturnStartDate" = "TradingDay")) %>%
  inner_join(data$IndexQuote %>% select(InnerCode, ClosePrice, TradingDay), 
             by = c("IndexCode" = "InnerCode", "ReturnLastDate" = "TradingDay")) %>% 
  mutate(IndexReturn = ClosePrice.y/ClosePrice.x - 1) %>% 
  group_by(ReturnLastDate) %>%
  mutate(IndustryMinusMean = IndustryWeeklyReturn - mean(IndustryWeeklyReturn),
         IndexMinusMean = IndexReturn - mean(IndexReturn)) %>%
  group_by(IndustryNameNew) %>%
  summarise(Corralation = cor(IndustryMinusMean, IndexMinusMean)) %>%
  arrange(Corralation)

# 各个行业之间的相关性及平均每期行业内股票个数
ggplot(industry_return, aes(x = IndustryNameNew))+ geom_boxplot(aes(y = Count))+ xlab(NULL) 

industry_corralation <- industry_return %>%
  group_by(ReturnLastDate) %>%
  mutate(ReturnMinusMean = IndustryWeeklyReturn - mean(IndustryWeeklyReturn)) %>%
  dcast(ReturnLastDate ~ IndustryNameNew, value.var = "ReturnMinusMean") 
industry_corralation <- as.data.frame(cor(industry_corralation[, -1], use = "na.or.complete"))

IndustryName <- names(industry_corralation)
a <- cbind(IndustryName, industry_corralation) %>%
  melt(id.var = "IndustryName")
ggplot(a , aes(IndustryName, variable, fill = value))+ geom_tile()

#######################################################################################################

#############################################################################################
 data$ForwardEPS$CON_DATE <- as.Date( data$ForwardEPS$CON_DATE)



industry_fep <- data.frame()

for(i in c(1:nrow(trading_date))){
  date <- trading_date[[i, 1]]
  temp <- data$ReturnDaily %>%
    filter(TradingDay == date) %>% # 筛选日期
    semi_join(data$IndexComponent %>% 
                filter(IndexInnerCode == nIndexCode, date >= InDate & date < OutDate),
              by = c("InnerCode" = "SecuInnerCode")) %>%  # 挑出成分股
    mutate(Date = date) 
  
  
  industry_fep_temp <- temp %>%
    inner_join(forward_ep %>% filter(CON_DATE == date, !is.na(C1)),
               by = c("SecuCode" = "STOCK_CODE", "TradingDay" = "CON_DATE")) %>%
    filter(C1 > 0 ) %>%
    filter(!is.na(IndustryNameNew)) %>%
    group_by(Date, IndustryNameNew) %>%
    summarise(FEP = weighted.mean(C1/ClosePrice, FloatMarketCap)) %>%
    ungroup()

  industry_fep <- rbind(industry_fep, industry_fep_temp)
}

 #############################################################################################
#EBIT/EV




######################################

industry_cp <- data.frame()

for(i in c(1:nrow(trading_date))){
  date <- trading_date[[i, 1]]
  temp <- data$ReturnDaily %>%
    filter(TradingDay == date) %>% # 筛选日期
    semi_join(data$IndexComponent %>% 
                filter(IndexInnerCode == nIndexCode, date >= InDate & date < OutDate),
              by = c("InnerCode" = "SecuInnerCode")) %>%  # 挑出成分股
    mutate(Date = date) 
  
  industry_cp_temp <- temp %>%
    inner_join(data$NetOperateCashFlow %>% filter(DataDate == date, !is.na(NetOperateCashFlow)),
               by = c("SecuCode" = "SecuCode", "TradingDay" = "DataDate")) %>%
    mutate(CPS = NetOperateCashFlow/MarketCap) %>%
    filter(!is.na(IndustryNameNew)) %>%
    group_by(Date, IndustryNameNew) %>%
    summarise(CP = weighted.mean(CPS, FloatMarketCap)) %>%
    ungroup()

  industry_cp <- rbind(industry_cp, industry_cp_temp)
}

##################################################################################


industry_bp <- data.frame()
for(i in c(1:nrow(trading_date))){
  date <- trading_date[[i, 1]]
  temp <- data$ReturnDaily %>%
    filter(TradingDay == date) %>% # 筛选日期
    semi_join(data$IndexComponent %>% 
                filter(IndexInnerCode == nIndexCode, date >= InDate & date < OutDate),
              by = c("InnerCode" = "SecuInnerCode")) %>%  # 挑出成分股
    mutate(Date = date) 
  
  
  temp_bp <- data$TotalShareholderEquity %>% 
    filter(InfoPublDate <= date, IfMerged == 1) %>%
    group_by(CompanyCode) %>%
    arrange(InfoPublDate, EndDate) %>%
    slice(n()) %>%
    select(CompanyCode, TotalShareholderEquity) %>%
    filter(!is.na(TotalShareholderEquity)) %>%
    ungroup()
  
  
  industry_bp_temp <- temp %>%
    inner_join(temp_bp, by = "CompanyCode") %>%
    mutate(BPS = TotalShareholderEquity/MarketCap) %>%
    filter(!is.na(IndustryNameNew)) %>%
    group_by(Date, IndustryNameNew) %>%
    summarise(BP = weighted.mean(BPS, FloatMarketCap))
  
  
  industry_bp <- rbind(industry_bp, industry_bp_temp)
}



################################################################################

industry_sp <- data.frame()

for(i in c(1:nrow(trading_date))){
  date <- trading_date[[i, 1]]
  temp <- data$ReturnDaily %>%
    filter(TradingDay == date) %>% # 筛选日期
    semi_join(data$IndexComponent %>% 
                filter(IndexInnerCode == nIndexCode, date >= InDate & date < OutDate),
              by = c("InnerCode" = "SecuInnerCode")) %>%  # 挑出成分股
    mutate(Date = date) 
  
  industry_sp_temp <- temp %>%
    inner_join(data$OperatingRevenue %>% filter(DataDate == date, !is.na(OperatingRevenue)),
               by = c("SecuCode" = "SecuCode", "TradingDay" = "DataDate")) %>%
    mutate(SPS = OperatingRevenue/MarketCap) %>%
    filter(!is.na(IndustryNameNew)) %>%
    group_by(Date, IndustryNameNew) %>%
    summarise(SP = weighted.mean(SPS, FloatMarketCap)) %>%
    ungroup()
  
  
  industry_sp <- rbind(industry_sp, industry_sp_temp)
  
}



  

 

qplot(IndustryNameNew, EP, data = industry_ep, geom = "boxplot") + xlab(NULL)
qplot(IndustryNameNew, BP, data = industry_bp, geom = "boxplot") + xlab(NULL)
qplot(IndustryNameNew, SP, data = industry_sp, geom = "boxplot") + xlab(NULL)
qplot(IndustryNameNew, CP, data = industry_cp, geom = "boxplot") + xlab(NULL)
qplot(IndustryNameNew, FEP, data = industry_fep, geom = "boxplot") + xlab(NULL)

################################################################################################################

value_signal <- industry_ep %>% 
  left_join(industry_fep, by = c("Date", "IndustryNameNew")) %>%
  left_join(industry_bp, by = c("Date", "IndustryNameNew")) %>%
  left_join(industry_cp, by = c("Date", "IndustryNameNew")) %>%
  left_join(industry_sp, by = c("Date", "IndustryNameNew")) %>%
  na.omit()

value_signal_score <- value_signal %>%
  group_by(IndustryNameNew) %>%
  arrange(Date) %>%
  mutate(EP = Score(EP, 8, IsMinusMean = 1),
         FEP = Score(FEP, 8, IsMinusMean = 1),
         BP = Score(BP, 8, IsMinusMean = 1),
         CP = Score(CP, 8, IsMinusMean = 1),
         SP = Score(SP, 8, IsMinusMean = 1)) %>%
  na.omit() %>%
  ungroup() %>%
  mutate(Value = (EP + FEP + BP + CP + SP)/5)





###############################################################################################################

signal_option <- value_signal_score %>% select(Date, IndustryNameNew,Value)
names(signal_option) <- c("Date", "IndustryNameNew", "Signal")



##############################################################################################################
signal_option_arrange <- signal_option %>%
  left_join(industry_return, by = c("Date" = "ReturnStartDate", "IndustryNameNew" = "IndustryNameNew")) %>%
  group_by(Date) %>%
  mutate(ReturnDemean = IndustryWeeklyReturn - mean(IndustryWeeklyReturn)) %>%
  mutate(Order = ifelse(percent_rank(desc(Signal)) <= 0.2, "1",
                        ifelse(percent_rank(desc(Signal)) > 0.2 & percent_rank(desc(Signal)) <= 0.4, "2",
                               ifelse(percent_rank(desc(Signal)) > 0.4 & percent_rank(desc(Signal)) <= 0.6, "3", 
                                      ifelse(percent_rank(desc(Signal)) > 0.6 & percent_rank(desc(Signal)) <= 0.8, 
                                             "4", "5"))))) %>%
  group_by(ReturnLastDate, Order) %>%
  summarise(PortfolioReturn = mean(IndustryWeeklyReturn),
            PortfolioReturnDemean = mean(ReturnDemean)) %>%
  group_by(Order) %>%
  arrange(ReturnLastDate) %>%
  mutate(CumulateReturn = expm1(cumsum(log1p(PortfolioReturn))),
         CumulateReturnDemean = expm1(cumsum(log1p(PortfolioReturnDemean)))) %>%
  ungroup() %>%
  mutate(ReturnLastDate = as.numeric(as.POSIXct(ReturnLastDate))*1000)


return_summary <- signal_option_arrange %>%
  group_by(Order) %>%
  summarise(YearlyReturn = expm1(mean(log1p(PortfolioReturn)) * 52),
            YearlySD = sd(PortfolioReturn) * sqrt(52),
            IR = YearlyReturn/YearlySD)

x2 <- hPlot(x = "ReturnLastDate", y = "CumulateReturn", data = signal_option_arrange , type = "line",
            group = "Order")
x2$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%M-%d}' ))
x2$chart(zoomType = 'xy')
x2

x3 <- hPlot(x = "ReturnLastDate", y = "CumulateReturnDemean", data = signal_option_arrange , type = "line",
            group = "Order")
x3$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%M-%d}' ))
x3$chart(zoomType = 'xy')
x3

###################### change
signal_change_option_arrange <- signal_option %>%
  group_by(IndustryNameNew) %>% 
  arrange(Date) %>%
  mutate(Lag1 = lag(Signal), Change = Signal - Lag1) %>%
  filter(!is.na(Change)) %>%
  left_join(industry_return, by = c("Date" = "ReturnStartDate", "IndustryNameNew" = "IndustryNameNew")) %>%
  group_by(Date) %>%
  mutate(ReturnDemean = IndustryWeeklyReturn - mean(IndustryWeeklyReturn)) %>%
  mutate(Order = ifelse(percent_rank(desc(Signal)) <= 0.2, "1",
                        ifelse(percent_rank(desc(Signal)) > 0.2 & percent_rank(desc(Signal)) <= 0.4, "2",
                               ifelse(percent_rank(desc(Signal)) > 0.4 & percent_rank(desc(Signal)) <= 0.6, "3", 
                                      ifelse(percent_rank(desc(Signal)) > 0.6 & percent_rank(desc(Signal)) <= 0.8, 
                                             "4", "5"))))) %>%
  group_by(ReturnLastDate, Order) %>%
  summarise(PortfolioReturn = mean(IndustryWeeklyReturn),
            PortfolioReturnDemean = mean(ReturnDemean)) %>%
  group_by(Order) %>%
  arrange(ReturnLastDate) %>%
  mutate(CumulateReturn = expm1(cumsum(log1p(PortfolioReturn))),
         CumulateReturnDemean = expm1(cumsum(log1p(PortfolioReturnDemean)))) %>%
  ungroup() %>%
  mutate(ReturnLastDate = as.numeric(as.POSIXct(ReturnLastDate))*1000) 

return_change_summary <- signal_change_option_arrange %>%
  group_by(Order) %>%
  summarise(YearlyReturn = expm1(mean(log1p(PortfolioReturn)) * 52),
            YearlySD = sd(PortfolioReturn) * sqrt(52),
            IR = YearlyReturn/YearlySD)
  

x4 <- hPlot(x = "ReturnLastDate", y = "CumulateReturn", data = signal_change_option_arrange , type = "line",
            group = "Order")
x4$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%M-%d}' ))
x4$chart(zoomType = 'xy')
x4

x5 <- hPlot(x = "ReturnLastDate", y = "CumulateReturnDemean", data = signal_change_option_arrange , type = "line",
            group = "Order")
x5$xAxis(type = 'datetime', labels = list(format = '{value:%Y-%M-%d}' ))
x5$chart(zoomType = 'xy')
x5






  

################################################################################################################
