library(readr)
library(fBasics)
library(fUnitRoots)

## 第五题
data1 <- read.csv("Desktop/time_analysis/data/E2_5.csv")
sale <- ts(data1$x, start = 2000, frequency = 12)
# 绘制时序图和样本自相关图
plot(sale, main = "The quantity of sale")
acf(sale)
# ADF检验
adfTest(sale, type = "ct")       # 进行类型三检验，发现不能拒绝原假设
adfTest(sale, type = "c")        # 进行类型二检验，发现仍然不能拒绝原假设
adfTest(sale, type = "nc")       # 进行类型一检验，发现仍不能拒绝原假设
# 纯随机性检验
for(i in 1:3)print(Box.test(sale, lag = 3*i))

## 第七题
data2 <-  read.csv("Desktop/time_analysis/data/E2_7.csv")
mortality <- ts(data2$mortality, start = 1915)
# 绘制时序图
plot(mortality, main = "The mortality of homicide")
# ADF检验
adfTest(mortality, type = "ct")      # 进行类型三检验，发现不能拒绝原假设
adfTest(mortality, type = "c")       # 进行类型二检验， 发现仍然不能拒绝原假设
adfTest(mortality, type = "nc")      # 进行类型一检验， 发现仍然不能拒绝原假设
# 一阶差分后的非平稳性
data2_1 <- diff(mortality)
plot(data2_1)
adfTest(data2_1, type = "ct")







