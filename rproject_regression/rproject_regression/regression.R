#使用回归方法预测数值型数据
#应用线性回归预测医疗费用
insurance <- read.csv("insurance.csv",stringsAsFactors = FALSE )

str(insurance)

summary(insurance)

hist(insurance$charges)#画出收费的直方图

table(insurance$region) #对区域分析
##############################################
#1 相关系数矩阵：为变量之间的关系提供相关系数
cor(insurance[c("age","bmi","children","charges")]) #变量间的相关系数矩阵

#2  散点图矩阵
pairs(insurance[c("age","bmi","children","charges")])

##############################################
#基于数据训练模型
ins_model <- lm( charges ~ age + children + bmi + sex + smoker + region , data = insurance)#建立模型,因变量 ~ 用于模型的自变量，数据集来源

ins_pred <- predict(ins_model,insurance)#进行预测

ins_model3 <- lm(charges ~ ., data = insurance)

ins_model3 #显示每种类型的因数变化量为一时，对应每年的医疗保险成本的变化量，首项intercept为截距

##############################################
#评估模型的性能
summary(ins_model) #星号表示每个特征的预测能力

##############################################
#提升模型的性能
insurance$age2 <- insurance$age ^ 2 #创建高阶项，然后添加到模型当中

#将数值类型转换为二进制指标
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0);#大于三十 返回1 ，小于三十 返回0;添加一个bmi30特征到insurance数据集

#将前面的改进融合,得出新的模型
ins_model2 <- lm(charges ~ age + age2 +children + bmi + sex + bmi30 + smoker + region , data = insurance )

summary (ins_model2) # 通过比较R-squared 值，模型的预测准确度有所上升








