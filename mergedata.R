#################prepare
print('123')
rm(list=ls())#��������ռ�
ls()#��ʾ��ǰ�����ռ����
library(readxl)
library("forecast") 
library(TTR)
library(readxl)
library(corrgram)
library('dplyr')
##################L1###########
grade=read_excel("D:/�ļ�/��Ŀ/20180708�ļ��ɼ�ͳ�Ʒ���/data/10-15CET4.xlsx",sheet = 1,col_names = T);
NEMT15=read_excel("D:/�ļ�/��Ŀ/20180708�ļ��ɼ�ͳ�Ʒ���/data/06to15�����ܱ�/2015�ܱ�.xlsx",sheet = 1,col_names = T);
NEMT14=read_excel("D:/�ļ�/��Ŀ/20180708�ļ��ɼ�ͳ�Ʒ���/data/06to15�����ܱ�/2014�ܱ�.xlsx",sheet = 1,col_names = T);
NEMT13=read_excel("D:/�ļ�/��Ŀ/20180708�ļ��ɼ�ͳ�Ʒ���/data/06to15�����ܱ�/2013�ܱ�.xlsx",sheet = 1,col_names = T);
NEMT12=read_excel("D:/�ļ�/��Ŀ/20180708�ļ��ɼ�ͳ�Ʒ���/data/06to15�����ܱ�/2012�ܱ�.xlsx",sheet = 1,col_names = T);
NEMT11=read_excel("D:/�ļ�/��Ŀ/20180708�ļ��ɼ�ͳ�Ʒ���/data/06to15�����ܱ�/2011�ܱ�.xlsx",sheet = 1,col_names = T);
NEMT10=read_excel("D:/�ļ�/��Ŀ/20180708�ļ��ɼ�ͳ�Ʒ���/data/06to15�����ܱ�/2010�ܱ�.xlsx",sheet = 1,col_names = T);
NEMT09=read_excel("D:/�ļ�/��Ŀ/20180708�ļ��ɼ�ͳ�Ʒ���/data/06to15�����ܱ�/2009�ܱ�.xlsx",sheet = 1,col_names = T);
NEMT08=read_excel("D:/�ļ�/��Ŀ/20180708�ļ��ɼ�ͳ�Ʒ���/data/06to15�����ܱ�/2008�ܱ�.xlsx",sheet = 1,col_names = T);
NEMT07=read_excel("D:/�ļ�/��Ŀ/20180708�ļ��ɼ�ͳ�Ʒ���/data/06to15�����ܱ�/2007�ܱ�.xlsx",sheet = 1,col_names = T);

i=0
temp=NEMT06
for (item in all){
	temp = rbind(temp,item)
	i = i+1
	print(i)
}

NEMT
size(all)
#####WAY  2  
	setwd("D:/�ļ�/��Ŀ/20180708�ļ��ɼ�ͳ�Ʒ���/data/")
	a = list.files("06to15�����ܱ�")#list.files���input�ļ����������ļ�������a
	dir = paste("./06to15�����ܱ�/",a,sep="")#��paste�����·������dir
	n = length(dir)#��ȡdir���ȣ�Ҳ�����ļ����µ��ļ�����
	merge.data = read_excel(dir[1],sheet = 1,col_names = T)#�����һ���ļ����ݣ����Բ����ȶ�һ��������Ϊ�˼򵥣�ʡȥ����data.frame��ʱ�䣬��ѡ���ȶ���һ���ļ���
	names(merge.data) = c("ѧ��","����","�Ա�","�������","����","Ͷ���ɼ�","����","��ѧ","����" )
	for (i in 2:n){
		print(i)
		new.data = read_excel(dir[i],sheet = 1,col_names = T);
		names(new.data) = c("ѧ��","����","�Ա�","�������","����","Ͷ���ɼ�","����","��ѧ","����" )
		new.data
		merge.data =rbind(merge.data,new.data)
	}
	merge.data[6:9]=apply(merge.data[6:9],2,as.numeric)
	summary(merge.data[,6:9])
	write.csv(merge.data)��
	write.table (merge.data, file ="merge.csv",sep =",") 
	# merge.csvΪ�����
###########cet4����allstu
	CET4=read_excel("D:/�ļ�/��Ŀ/20180708�ļ��ɼ�ͳ�Ʒ���/data/10-15CET4.xlsx",sheet = 1,col_names = T);
	CET4
	CET4[4]=apply(CET4[4],2,as.numeric)

###########ɾ������
	for (item in merge.data){
		print(length(item))	
	}


###########

hist(d1$ƽ��ѧ�ּ�,breaks=100)
res = subset(d1,grepl("2014|2015|2016|2017",d1$��ѧ����))
#����
res
hist(res$ƽ��ѧ�ּ�,breaks=100)
#δ��
res = subset(d1,!grepl("0",d1$ѧ���ܺ�))
res
hist(res$ƽ��ѧ�ּ�,breaks=100)
#����δ��
d1s = subset(d1,grepl("2014|2015|2016|2017",d1$��ѧ����)&!grepl("0",d1$ѧ���ܺ�))
hist(d1s$ƽ��ѧ�ּ�,breaks=100)
#k������Ϣȡ����
#d2s = subset(d2,grepl("2014|2015|2016|2017",d2$��ѧʱ��))
#d3s = subset(d3,grepl("2014|2015|2016|2017",d3$��ѧʱ��))
d4=rbind(d2s,d3s)
dbind=left_join(d1s,d4,by="ѧ��")
dbind$level
pstudent = subset(dbind,grepl("1|2",dbind$level))
psS = subset(pstudent,grepl("2",pstudent$level)) #��������ȥ��ѡ
psO = subset(pstudent,grepl("1",pstudent$level))#����������ȥ��ѡ
hist(pstudent$ƽ��ѧ�ּ�,breaks=100)
hist(psS $ƽ��ѧ�ּ�,breaks=100)
hist(psO $ƽ��ѧ�ּ�,breaks=100)
qqnorm(psS$ƽ��ѧ�ּ�)
qqline(psS$ƽ��ѧ�ּ�,col="red")

t.test(d1s$ƽ��ѧ�ּ�,psS$ƽ��ѧ�ּ�,var.equal=TRUE,paired=F)
t.test(d1s$ƽ��ѧ�ּ�,psO $ƽ��ѧ�ּ�,var.equal=TRUE,paired=F)
t.test(psS$ƽ��ѧ�ּ�,psO $ƽ��ѧ�ּ�,var.equal=TRUE,paired=F)





#################        ��Ŀ1
	data=read_excel("D:/�ļ�/�γ�/Ӧ��ͳ�Ʒ���/20180514�ھŴ���ҵ/data.xlsx",sheet = 2,col_names = T);
	data = data[,2:8]
	pc=princomp(data,cor=T)
	summary(pc)
	m = 2;
	pc$loadings[,1:m]
	y = pc$scores;y 
	screeplot(pc,type="lines")
	order(y[,1])
	order(y[,2])
	biplot(pc)
	lines(c(-10,10),c(0,0))
#################        ��Ŀ2      data read
	data=read_excel("D:/�ļ�/�γ�/Ӧ��ͳ�Ʒ���/20180514�ھŴ���ҵ/data.xlsx",col_names = T);
	pc=princomp(data,cor=T)
	summary(pc)
	m = 3;
	pc$loadings[,1:m]
	y = pc$scores;y 
	screeplot(pc,type="lines")
	order(y[,1])
	order(y[,2])
	order(y[,3])
	order(y[,4])
	order(y[,5])
	biplot(pc)
	lines(c(-10,10),c(0,0))