library(GEOquery)
library(ggplot2)
gset <- getGEO("GSE182920",  destdir=".")
length(gset)
gset<-gset[[1]]
gset<-exprs(gset)
str(gset)
class(gset)
attributes(gset)
dim(gset)
gset<-gset[1:20,]
attributes(gset)
dim(gset)
str(gset)
test<-gset[,1:4]
control<-gset[,5:8]
#t-test
gtest.means <- apply(test, 1, mean, na.rm=TRUE)
gcontrol.means <- apply(control, 1, mean, na.rm=TRUE)
test.summary <- summary(test)
control.summary <- summary(control)
test.gene <- t.test(x=control,y =test ,var.equal = TRUE)
test.gene
#boxplot_for_control_test
testv<-c()
controlv<-c()
for(i in test){
  testv<-append(testv,i)}
for(i in control){
  controlv<-append(controlv,i)}
genes<-data.frame(types<-rep(c("test","control"),each=80),genec<-c(testv,controlv))
ggplot(genes,aes(x=types,y=genec,fill=types))+geom_boxplot()




boxplot(test, notch=F,varwidth=T,main="boxplot", outline=TRUE,las=2,col="blue",xlab="Sample",ylab="Genes")
boxplot(control, notch=F,varwidth=T,main="boxplot", outline=TRUE,las=2,col="red",xlab="Sample",ylab="Genes")

gsms <- "11110000"
sml <- strsplit(gsms, split="")[[1]]
gs <- factor(sml)
ord <- order(gs)
groups <- make.names(c("control","test"))
palette(c("#E6AB02", "#E7298A"))
# order samples by group
boxplot(gset[,ord], boxwex=0.6, notch=F, main="boxplot", outline=FALSE, las=2, col=gs[ord],xlab="Sample",ylab="Genes")
legend("bottomleft",groups, fill=palette(), bty="n")
#scatterplot
pairs(~control+test,data = gset,main = "Scatterplot Matrix",pch=20)

# your code goes here