#
library(AMORE)
# �������뼰����
dat <- read.table('german.data-numeric', header = F)
for (i in 1:25) {
    dat[, i] <- as.numeric(as.vector(dat)[, i])
}
# ����ѵ�����Ͳ��Լ�
train_dat <- dat[1:500,]
test_dat <- dat[501:1000,]
# ����ѵ��
net <- newff(n.neurons = c(24, 8, 2, 1), learning.rate.global = 1e-2, momentum.global = 0.5,
        error.criterium = "LMS", Stao = NA, hidden.layer = "tansig",
        output.layer = "purelin", method = "ADAPTgdwm")
result <- train(net, train_dat[1:24], train_dat[25], error.criterium = "LMS", report = TRUE, show.step = 100, n.shows = 5)
# ���в���
y <- sim(result$net, test_dat[1:24])
y[which(y < 1.5)] <- 1
y[which(y >= 1.5)] <- 2
# ������ȷ��
sum = 0
for (i in 1:500) {
    if (y[i] == test_dat[i, 25]) {
        sum = sum + 1
    }
}
cat("��ȷ��", sum / 500, "n")



