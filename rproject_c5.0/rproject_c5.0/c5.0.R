#ʹ��c5.0������ȷ���߷��ս��ڷ���
credit <- read.csv ("credit.csv")

table(credit$checking_balance) #֧Ʊ���

table(credit$savings_balance)#�������

summary(credit$months_loan_duration)

summary(credit$amount);

table(credit$amount)
#�����µ�ѵ�����Ͳ��Լ�
set.seed(12345) #

credit_rand <- credit[order(runif(1000)),] #���ɺ���1000����������б�

summary(credit$amount)

summary(credit_rand$amount)

head(credit$amount)

head(credit_rand$amount)

credit_train <- credit[1:900,]

credit_test <- credit[901:1000,]

prop.table(table(credit_train$default))

prop.table(table(credit_test$default))
#��������ѵ��ģ��
install.packages("C50", repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))

library("C50")

credit_model <- C5.0(credit_train[-17], credit_train$default)


