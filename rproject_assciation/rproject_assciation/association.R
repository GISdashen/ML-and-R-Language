#关联规则挖掘 Apriori算法
#使用超市一个月的营销数据 挖掘出一定的关联规则
install.packages("arules", repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
library(arules)

groceries <- read.transactions("groceries.csv", sep = ",") #transactions适用于事务性的稀疏矩阵

summary(groceries)

itemFrequency(groceries[,1:3]) #显gro示商品的支持度

#2可视化上平的支持度 （支持度表示一个项集在数据集中出现的频度）
itemFrequencyPlot(groceries,support= 0.1)#显示支持度至少为10%的商品

itemFrequencyPlot(groceries,topN = 20) #显示排名前二十的商品

#3可视化稀疏矩阵
image(groceries[1:5]) #表示前五次交易

image(sample(groceries,100)) #使用sample随机收取其中的一百次交易

apriori(groceries) #使用默认的支持度和置信度，无法得到任何规则，需要自己设定参数

groceriesrules <- apriori(groceries,parameter = list(support = 0.006,confidence = 0.25,minlen = 2))

groceriesrules #有463条规则

#4评估模型的性能
summary(groceriesrules) #第三列提升度lift：用来度量一类商品相对它的一般购买率，此时被购买的概率多大

inspect(groceriesrules[1:3]) #显示前三条规则

#5提升模型的性能
inspect(sort(groceriesrules, by = "lift")[1:5]) #按照lift排序 查看前5条的规则

berryrules <- subset(groceriesrules,items %in% "berries") #提取含有berries的规则

inspect(berryrules)

write(groceriesrules,file = "groceriesrules.csv",sep = ",",quote = TRUE ,row.names = FALSE ) #将结果保存到csv文件里面

groceriesrules_df <- as(groceriesrules, "data.frame") #转换为数据框格式

str(groceriesrules_df)

#总结：无监督学习使该方法可以从任何关于模式的先验知识的数据库中提取知识

