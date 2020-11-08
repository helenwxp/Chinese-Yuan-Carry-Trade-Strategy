setwd('D:/FDU/毕业论文/正式/Empirical Tests')
library(readxl)
library(dplyr,warn.conflicts = F)                                  
library(tidyr)
library(ggplot2)

inttrend = read_excel('inttrend.xlsx')
str(inttrend)

intplot = inttrend%>%
  gather(CURRENCY,IBrate,-YM,factor_key = TRUE)

#--------------------颜色盘------------
#P50
library(RColorBrewer)
mycolors=brewer.pal(10,'Paired')
display.brewer.all()
colors()#返回所有可用颜色名称

ggplot(intplot,aes(YM,IBrate,color = CURRENCY))+
  geom_line(size = 1.2)+
  geom_vline(xintercept=c(as.POSIXct("2007-08-01"),as.POSIXct("2010-06-01"),as.POSIXct("2015-08-01")), 
             colour="black", linetype=2)+
  labs(x = 'Year',y = 'Interbank rate 3M (%)')+
  scale_colour_manual(values=mycolors)+
  theme_grey()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        plot.margin = unit(c(0.4, 0.5, 0.2, 0.5), "lines"),
        legend.position = 'left')
ggsave('inttrend.png')

usdfxtr = read_excel('usdspotvol.xlsx')
str(usdfxtr)
ggplot(usdfxtr,aes(x = year))+
  geom_line(aes(y = spotfxrate,color = "USD/CNY spot FX rate(lhs)"),size = 1.2)+
  geom_line(aes(y = volatility,color = "Historical Volatility(rhs)"),size = 1.2)+
  geom_vline(xintercept=c(as.POSIXct("2005-07-29"),as.POSIXct("2015-08-31")), 
             colour="black", linetype=2)+
  geom_vline(xintercept = c(as.POSIXct("2007-08-31"),as.POSIXct("2010-06-30")),
             colour="purple",linetype = 2)+
  labs(x = 'Year',y = 'Spot Exchange Rate')+
  scale_y_continuous(limits = c(0,9),breaks = seq(0,9,by = 1),
                     sec.axis = sec_axis(~.,breaks = seq(0,9,by = 1),name = "Historical Volatility (%)"))+
  scale_colour_manual(values = brewer.pal(3,'Set1'))+
  guides(color=guide_legend(title=NULL))+
  theme_grey()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        plot.margin = unit(c(0.4, 4, 0.2, 4), "lines"),
        legend.position = 'bottom')
ggsave('usdfxtr.png')

usdvolbf811 = read_excel('usdspotvol.xlsx',sheet = 2)
volbf = usdvolbf811$volatility
summary(volbf)
usdvolaf811 = read_excel('usdspotvol.xlsx',sheet = 3)
volaf = usdvolaf811$volatility
summary(volaf)

usdimpldvol = read_excel('usdimpldvol.xlsx')
str(usdimpldvol)
ggplot(usdimpldvol,aes(x = year))+
  geom_line(aes(y = spotfxrate,color = "USD/CNY spot FX rate(lhs)"),size = 1.2)+
  geom_line(aes(y = volatility,color = "Implied Volatility(rhs)"),size = 1.2)+
  geom_vline(xintercept=as.POSIXct("2015-08-11"), 
             colour="black", linetype=2)+
  geom_vline(xintercept = as.POSIXct("2010-06-19"),
             colour="purple",linetype = 2)+
  labs(x = 'Year',y = 'Spot Exchange Rate')+
  scale_y_continuous(limits = c(0,9),breaks = seq(0,9,by = 1),
                     sec.axis = sec_axis(~.,breaks = seq(0,9,by = 1),name = "Implied Volatility (%)"))+
  scale_colour_manual(values = brewer.pal(3,'Set1'))+
  guides(color=guide_legend(title=NULL))+
  theme_grey()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        plot.margin = unit(c(0.4, 4, 0.2, 4), "lines"),
        legend.position = 'bottom')
ggsave('usdimpldvol.png')

usdimpldvolbf811 = read_excel('usdimpldvol.xlsx',sheet = 2)
imvolbf = usdimpldvolbf811$volatility
summary(imvolbf)
usdimpldvolaf811 = read_excel('usdimpldvol.xlsx',sheet = 3)
imvolaf = usdimpldvolaf811$volatility
summary(imvolaf)

csvol = read_excel('c-svol.xlsx')
csvolplot = csvol%>%
  gather(CurrencyPair,HV,-year)

ggplot(csvolplot,aes(year,HV,color = CurrencyPair))+
  geom_line(size = 1.2)+
  geom_vline(xintercept=as.POSIXct("2015-08-30"), 
             colour="black", linetype=2)+
  geom_vline(xintercept = as.POSIXct("2010-06-30"),
             colour="purple",linetype = 2)+
  labs(x = 'Year',y = 'Historical Volatility (%)')+
  scale_colour_manual(values = brewer.pal(3,'Paired'))+
  theme_grey()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        plot.margin = unit(c(0.4, 4, 0.2, 4), "lines"),
        legend.position = 'bottom')
ggsave('c-svol.png')

hotmoney = read_excel('热钱规模计算.xlsx',sheet = 5)
ggplot(hotmoney,aes(year,hotmoney))+
  geom_line(size = 1.2)+
  scale_x_continuous(breaks=seq(2000,2019,by=2))+
  labs(x = 'Year',y = '热钱 (亿美元)')+
#  theme_grey()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10.5),
        legend.text = element_text(size = 10),
        plot.margin = unit(c(0.4, 0.5, 0.2, 0.5), "lines"),
        legend.position = 'bottom')
ggsave('hotmoney.png')
