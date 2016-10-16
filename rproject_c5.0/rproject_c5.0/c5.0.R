#使用c5.0决策树确定高风险金融风险
credit <- read.csv ("credit.csv")

table(credit$checking_balance) #支票余额

table(credit$savings_balance)#储户余额

summary(credit$months_loan_duration)

summary(credit$amount);

table(credit$amount)
#创建新的训练集和测试集
set.seed(12345) #

credit_rand <- credit[order(runif(1000)),] #生成含有1000个随机数的列表

summary(credit$amount)

summary(credit_rand$amount)

head(credit$amount)

head(credit_rand$amount)

credit_train <- credit[1:900,]

credit_test <- credit[901:1000,]

prop.table(table(credit_train$default))

prop.table(table(credit_test$default))
#基于数据训练模型
install.packages("C50", repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))

library("C50")

credit_model <- C5.0(credit_train[-17], credit_train$default)



