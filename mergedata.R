#################prepare
print('123')
rm(list=ls())#清除工作空间
ls()#显示当前工作空间变量
library(readxl)
library("forecast") 
library(TTR)
library(readxl)
library(corrgram)
library('dplyr')
##################L1###########
grade=read_excel("D:/文件/项目/20180708四级成绩统计分析/data/10-15CET4.xlsx",sheet = 1,col_names = T);
NEMT15=read_excel("D:/文件/项目/20180708四级成绩统计分析/data/06to15新生总表/2015总表.xlsx",sheet = 1,col_names = T);
NEMT14=read_excel("D:/文件/项目/20180708四级成绩统计分析/data/06to15新生总表/2014总表.xlsx",sheet = 1,col_names = T);
NEMT13=read_excel("D:/文件/项目/20180708四级成绩统计分析/data/06to15新生总表/2013总表.xlsx",sheet = 1,col_names = T);
NEMT12=read_excel("D:/文件/项目/20180708四级成绩统计分析/data/06to15新生总表/2012总表.xlsx",sheet = 1,col_names = T);
NEMT11=read_excel("D:/文件/项目/20180708四级成绩统计分析/data/06to15新生总表/2011总表.xlsx",sheet = 1,col_names = T);
NEMT10=read_excel("D:/文件/项目/20180708四级成绩统计分析/data/06to15新生总表/2010总表.xlsx",sheet = 1,col_names = T);
NEMT09=read_excel("D:/文件/项目/20180708四级成绩统计分析/data/06to15新生总表/2009总表.xlsx",sheet = 1,col_names = T);
NEMT08=read_excel("D:/文件/项目/20180708四级成绩统计分析/data/06to15新生总表/2008总表.xlsx",sheet = 1,col_names = T);
NEMT07=read_excel("D:/文件/项目/20180708四级成绩统计分析/data/06to15新生总表/2007总表.xlsx",sheet = 1,col_names = T);

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
	setwd("D:/文件/项目/20180708四级成绩统计分析/data/")
	a = list.files("06to15新生总表")#list.files命令将input文件夹下所有文件名输入a
	dir = paste("./06to15新生总表/",a,sep="")#用paste命令构建路径变量dir
	n = length(dir)#读取dir长度，也就是文件夹下的文件个数
	merge.data = read_excel(dir[1],sheet = 1,col_names = T)#读入第一个文件内容（可以不用先读一个，但是为了简单，省去定义data.frame的时间，我选择先读入一个文件。
	names(merge.data) = c("学号","地区","性别","考生类别","科类","投挡成绩","语文","数学","外语" )
	for (i in 2:n){
		print(i)
		new.data = read_excel(dir[i],sheet = 1,col_names = T);
		names(new.data) = c("学号","地区","性别","考生类别","科类","投挡成绩","语文","数学","外语" )
		new.data
		merge.data =rbind(merge.data,new.data)
	}
	merge.data[6:9]=apply(merge.data[6:9],2,as.numeric)
	summary(merge.data[,6:9])
	write.csv(merge.data)；
	write.table (merge.data, file ="merge.csv",sep =",") 
	# merge.csv为输出表
###########cet4合入allstu
	CET4=read_excel("D:/文件/项目/20180708四级成绩统计分析/data/10-15CET4.xlsx",sheet = 1,col_names = T);
	CET4
	CET4[4]=apply(CET4[4],2,as.numeric)

###########删除数据
	for (item in merge.data){
		print(length(item))	
	}


###########

hist(d1$平均学分绩,breaks=100)
res = subset(d1,grepl("2014|2015|2016|2017",d1$入学日期))
#当届
res
hist(res$平均学分绩,breaks=100)
#未修
res = subset(d1,!grepl("0",d1$学分总和))
res
hist(res$平均学分绩,breaks=100)
#当届未修
d1s = subset(d1,grepl("2014|2015|2016|2017",d1$入学日期)&!grepl("0",d1$学分总和))
hist(d1s$平均学分绩,breaks=100)
#k困难信息取当届
#d2s = subset(d2,grepl("2014|2015|2016|2017",d2$入学时间))
#d3s = subset(d3,grepl("2014|2015|2016|2017",d3$入学时间))
d4=rbind(d2s,d3s)
dbind=left_join(d1s,d4,by="学号")
dbind$level
pstudent = subset(dbind,grepl("1|2",dbind$level))
psS = subset(pstudent,grepl("2",pstudent$level)) #特困生出去不选
psO = subset(pstudent,grepl("1",pstudent$level))#困难生生出去不选
hist(pstudent$平均学分绩,breaks=100)
hist(psS $平均学分绩,breaks=100)
hist(psO $平均学分绩,breaks=100)
qqnorm(psS$平均学分绩)
qqline(psS$平均学分绩,col="red")

t.test(d1s$平均学分绩,psS$平均学分绩,var.equal=TRUE,paired=F)
t.test(d1s$平均学分绩,psO $平均学分绩,var.equal=TRUE,paired=F)
t.test(psS$平均学分绩,psO $平均学分绩,var.equal=TRUE,paired=F)





#################        题目1
	data=read_excel("D:/文件/课程/应用统计分析/20180514第九次作业/data.xlsx",sheet = 2,col_names = T);
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
#################        题目2      data read
	data=read_excel("D:/文件/课程/应用统计分析/20180514第九次作业/data.xlsx",col_names = T);
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