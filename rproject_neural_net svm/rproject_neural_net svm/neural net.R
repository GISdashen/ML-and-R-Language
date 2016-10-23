#神经网络：
#1混凝土数据，有8个描述混凝土成分的特征，这些特征被认为与最后的抗压强度有关系
concrete <- read.csv("concrete.csv")

#2使用标准化函数
normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
}

#对数据进行标准化
concrete_nom <- as.data.frame(lapply(concrete, normalize));
#1030x90


summary(concrete$strength)
summary(concrete_nom$strength)

#分离训练数据和测试数据
concrete_train <- concrete_nom[1:773,];
concrete_test <- concrete_nom[774:1030,];

#3基于数据训练模型
#安装neural net包
install.packages("neuralnet")

library(neuralnet)

concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train)

#将网络拓扑结构可视化  每一项特征后跟一个权重 带有1的项表示片偏差项 下面那汇报了训练的步数和一个称为误差平方和的指标
plot(concrete_model)

#4评估模型的性能
model_results <- compute(concrete_model, concrete_test[1:8]); #返回带有neurous ，net.results的分量列表

predicted_strength <- model_results$net.result 

cor(predicted_strength, concrete$strength) #获取数值向量间的相关性,结果约为72%

#5提高性能
concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age,data = concrete_train,hidden = 5 )#隐藏层节点数增加为5

plot(concrete_model2)
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strenth2 <- model_results2$net.result
cor(predicted_strenth2, concrete_test$strength) #结果提升到了７９％



###########################################################################################

#支持向量机
#一个高维空间中的“平面”，几乎适用于所有的学习任务

#预测匹配图像字符
#２准备数据
letters <- read.csv("letterdata.csv")

str(letters) #2000x17的数据集

#划分为训练和测试数据集合
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000,]

#3基于数据训练模型
install.packages("kernlab")
library(kernlab)


letter_classifirer <- ksvm(letter~.,data = letters_train , kernel = "vanilladot") #vaniladot　线性函数

letter_classifirer

#4评估模型的性能
letter_predictions <- predict(letter_classifirer, letters_test);#默认返回一个向量　包含测试数据中每一行的一预测字母

head(letter_predictions)

table(letter_predictions, letters_test$letter) #二者是长度一样的向量,返回一个２６ｘ２６的矩阵,对角线上的是预测正确的总记录数

agreement <- letter_predictions == letters_test$letter

table(agreement) #列出正确和错误的次数

prop.table(table(agreement)) #用小数来表示

#5提升模型性能,使用RBF高斯函数
letter_classifirer_rbf <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")

letter_predictions <- predict(letter_classifirer_rbf, letters_test)

agreement_rbf <- letter_predictions == letters_test$letter

table(agreement_rbf) #列出正确和错误的次数

prop.table(table(agreement_rbf)) #用小数来表示,

#结果显示 有效提升了大约9%的正确率





