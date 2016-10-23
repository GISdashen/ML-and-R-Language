#���������ھ� Apriori�㷨
#ʹ�ó���һ���µ�Ӫ������ �ھ��һ���Ĺ�������
install.packages("arules", repos = c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
library(arules)

groceries <- read.transactions("groceries.csv", sep = ",") #transactions�����������Ե�ϡ�����

summary(groceries)

itemFrequency(groceries[,1:3]) #��groʾ��Ʒ��֧�ֶ�

#2���ӻ���ƽ��֧�ֶ� ��֧�ֶȱ�ʾһ��������ݼ��г��ֵ�Ƶ�ȣ�
itemFrequencyPlot(groceries,support= 0.1)#��ʾ֧�ֶ�����Ϊ10%����Ʒ

itemFrequencyPlot(groceries,topN = 20) #��ʾ����ǰ��ʮ����Ʒ

#3���ӻ�ϡ�����
image(groceries[1:5]) #��ʾǰ��ν���

image(sample(groceries,100)) #ʹ��sample�����ȡ���е�һ�ٴν���

apriori(groceries) #ʹ��Ĭ�ϵ�֧�ֶȺ����Ŷȣ��޷��õ��κι�����Ҫ�Լ��趨����

groceriesrules <- apriori(groceries,parameter = list(support = 0.006,confidence = 0.25,minlen = 2))

groceriesrules #��463������

#4����ģ�͵�����
summary(groceriesrules) #������������lift����������һ����Ʒ�������һ�㹺���ʣ���ʱ������ĸ��ʶ��

inspect(groceriesrules[1:3]) #��ʾǰ��������

#5����ģ�͵�����
inspect(sort(groceriesrules, by = "lift")[1:5]) #����lift���� �鿴ǰ5���Ĺ���

berryrules <- subset(groceriesrules,items %in% "berries") #��ȡ����berries�Ĺ���

inspect(berryrules)

write(groceriesrules,file = "groceriesrules.csv",sep = ",",quote = TRUE ,row.names = FALSE ) #��������浽csv�ļ�����

groceriesrules_df <- as(groceriesrules, "data.frame") #ת��Ϊ���ݿ��ʽ

str(groceriesrules_df)

#�ܽ᣺�޼ලѧϰʹ�÷������Դ��κι���ģʽ������֪ʶ�����ݿ�����ȡ֪ʶ
