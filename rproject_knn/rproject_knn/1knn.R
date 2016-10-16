##使用knn分类学习对数据进行分类

wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE);

## 将第一列id剔除
wbcd <- wbcd[-1];

#diagnosis属性->良性/恶性,使用table生成diagonosis的一元表
table(wbcd$diagnosis) 

#对diagonisis重新编码
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malinant"));

#两个属性所占的百分比
round(prop.table(table(wbcd$diagnosis))* 100 ,digits = 1)

#举例观察三个特征
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

######################################################################
#对上面的三类数据进行标准化
#标准化函数
normalize <- function(x) {
    return ((x - min(x))/(max(x) - min(x)))
}
#对数值数据标准化
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize));

#测试标准化
summary(wbcd_n$area_mean)

######################################################################
#数据准备：将数据拆分为训练数据和测试数据
wbcd_train <- wbcd_n[1:436,];

wbcd_test <- wbcd_n[170:569,];

#将类的标签(Benign/Malinant)提取出来
wbcd_train_labels <- wbcd[1:469,1] 

wbcd_test_labels <- wbcd[170:569,1];
######################################################################
#训练模型
install.packages("class");

#knn返回一个因子向量，为测试数据集里面的每一个案例返回一个预测标签
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels,k = 21) #k为标识最临近目标的整数

######################################################################
#评估模型的性能
CrossTable(x = wbcd_test_labels,y = wbcd_test_pred,prop.chisq = FALSE )

######################################################################
#提升模型的性能
#1 使用z-score标准化:标准化后均值变为0
wbcd_z <- as.data.frame(scale(wbcd[-1]));

summary(wbcd_z$area_mean)

wbcd_train <- wbcd_z[1:436,];

wbcd_test <- wbcd_z[170:569,];

wbcd_train_labels <- wbcd[1:469, 1]

wbcd_test_labels <- wbcd[170:569, 1];

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

CrossTable(wbcd_test_labels, wbcd_test_pred,prop.chisq = FALSE)

#2测试其他的k值

