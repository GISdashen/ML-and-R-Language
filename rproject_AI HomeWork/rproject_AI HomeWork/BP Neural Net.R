#训练BP神经网络
#install.packages("AMORE", repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
library('AMORE')
# 数据载入及处理
dat <- read.table('german.data-numeric.csv', header = F)
for (i in 1:25) {
    dat[, i] <- as.numeric(as.vector(dat)[, i])
}
# 划分训练集和测试集
train_dat <- dat[1:500,]  #选择前500行为训练数据
test_dat <- dat[501:1000,] #选择后500行为测试数据
# 进行训练
#参数： 包含每一层神经元数目的数值向量（24，8，2，1）；每个被训练的神经元的学习率：0.01 ；每个神经元的动量：0.5； 误差标准： 极小化我误差平方和  ； 隐藏层：       输出：          方法：动量自适应梯度下降算法 
net <- newff(n.neurons = c(24, 8, 2, 1), learning.rate.global = 1e-2, momentum.global = 0.5,
        error.criterium = "LMS", Stao = NA, hidden.layer = "tansig",
        output.layer = "purelin", method = "ADAPTgdwm") #创建一个新的多层前馈神经网络

#参数说明：  要训练的神经网络  训练集输入值 训练集输出值 评价拟合效果的标准     report:报告  show.stop:时代训练的培训功能不停止，直到数报告      n.shows:汇报的次数 
result <- train(net, train_dat[1:24], train_dat[25], error.criterium = "LMS", report = TRUE, show.step = 100, n.shows = 5)
# 进行测试
y_pre <- sim(result$net, test_dat[1:24])
y_pre[which(y_pre < 1.5)] <- 1
y_pre[which(y_pre >= 1.5)] <- 2
# 计算正确率
sum = 0
for (i in 1:500) {
    if (y[i] == test_dat[i, 25]) { #计算预测正确的个数
        sum = sum + 1
    }
}

cat("正确率", sum / 500, "n")

times <- c(1:1:5)
plot(x = times,type = 'b', y = result$Merror,xlab = "报告次数",ylab = "LMS" , main = "报告")

#绘制出还未整数化的预测集合
answer_num <- c(1:1:500)
plot(answer_num, y = y_pre, xlab = "用户", ylab = "用户的信用值",main =  "对测试集的用户信用的预测")




