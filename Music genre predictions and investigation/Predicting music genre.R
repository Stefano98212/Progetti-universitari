data = read.csv("C:/Users/Stefano/Desktop/DSE/ADVANCED MULTIVARIATE STATISTICS/PROGETTO/Data.csv", header=TRUE, sep=",")
fix(data)
attach(data)
dim(data)


#new dataset removing jazz, metal e blues
data=subset(data, label!="blues" & label!="jazz"&label!="metal" & label!="country")
fix(data)
attach(data)
dim(data)




#making manova
man=manova(cbind(tempo, beats, chroma_stft, spectral_centroid, spectral_bandwidth, rolloff, zero_crossing_rate, mfcc1, mfcc2, mfcc3, mfcc4, mfcc5, mfcc6, mfcc7, mfcc8, mfcc9, mfcc10, mfcc11, mfcc12, mfcc13, mfcc14, mfcc15, mfcc16, mfcc17, mfcc18, mfcc19 )~label, data=data)
summary(man)




#creating dataset for PCA
n= c(2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28)
c = cor(data[,n])
dataPCA= data[,n]
attach(dataPCA)
fix(dataPCA)
#Making KMO test
library(psych)
KMO(dataPCA)
#the overall result is 0,85 which is good

#PCA
PCA= prcomp(dataPCA, scale. = TRUE)
PCA
names(PCA)
variance= PCA$sdev^2
pve= variance/sum(variance)
plot(pve, xlab= "principal component", ylab="percentage variance explained", ylim= c(0,1), type="b")
plot(cumsum(pve))
score= PCA$x
x=score[,1]
y=score[,2]
z=score[,3]

sum(PCA$sdev[1:3]^2)/sum(PCA$sdev^2)
#we can take 3 dimensions

#communalities
loadings=PCA$rotation
loadings= loadings[,1:3]
loadings=as.data.frame(loadings)
communalities=rep(0,nrow(loadings))
loadings["Communality"]=communalities
loadings
loadings$Communality=(loadings$PC1)^2+(loadings$PC2)^2+(loadings$PC3)^2
loadings

#plotting PCA results
library(pca3d)
pca3d(PCA, components=1:3, group= label, palette = rainbow(6), show.group.labels = FALSE, biplot=TRUE, show.scale = TRUE, show.centroids = FALSE, show.shapes = TRUE, legend="top" )




#matrix correlations
library(corrplot)
corrplot(c, method = "pie")
cf3 = 12
low_correlated= c(3, 4, 6, 10, 12, 13, 14, 15, 20, 23, 24, 25, 26, 27 ,28 )
new_c= cor(data[,low_correlated])
corrplot(new_c, method="number")
unique(data$label)




#MULTINOMIAL LOGISTIC REGRESSION
library(caret)
library(nnet)
library(car)
low_correlated2=c(3, 4, 6, 10, 12, 13, 14, 15, 20, 23, 24, 25, 26, 27 ,28, 30 )

#complete dataset for logistic regression
data_logistic= data[,low_correlated2]
fix(data_logistic)

#creating training set
set.seed(15)
training_set= sample(1:nrow(data_logistic), nrow(data_logistic)/1.2)
length(training_set)
logistic_training=data_logistic[training_set,]
attach(logistic_training)
#fix(logistic_training)
logistic_test=data_logistic[-training_set,]
attach(logistic_test)

table(data_logistic$label)

logistic_training$label=relevel(as.factor(logistic_training$label), ref = "disco")
logistic_reg= nnet::multinom(label~beats+chroma_stft+spectral_centroid+mfcc1+mfcc3+mfcc4+mfcc5+mfcc6+mfcc11+mfcc14+mfcc15+mfcc16+mfcc17+mfcc18+mfcc19, data = logistic_training)
summary(logistic_reg)
confint(logistic_reg)
predicted.class=predict(logistic_reg, newdata = logistic_test)
predicted.class
#accuracy
mean(predicted.class==logistic_test$label)
#confusion matrix
table(logistic_test$label,predicted.class)
coef(logistic_reg)

#copmputing statistic for the wald test
sommario= summary(logistic_reg)
wald_test=(sommario$coefficients/sommario$standard.errors)^2
fix(wald_test)
wald_test




#MULTIDIMENSIONAL SCALING

#scaling data
Data_Scaled= scale(dataPCA, scale = TRUE)
attach(Data_Scaled)
fix(Data_Scaled)
proximities=dist(Data_Scaled, method = "manhattan")
dim(as.matrix(proximities))
multi_dimensional_scaling= cmdscale(proximities,k=20, eig = TRUE)
multi_dimensional_scaling
cumsum((multi_dimensional_scaling$eig)^2)/sum((multi_dimensional_scaling$eig)^2)
#we can choose 2 dimensions
two_dimension= multi_dimensional_scaling$points[,1:2]

#creating a dataframe with the two dimensions
two_dimension= as.data.frame(two_dimension)
colors=rep("black", 600)
two_dimension["Colors"]=colors
two_dimension["Label"]=data$label
attach(two_dimension)
two_dimension$Colors[Label=="classical"]="orange"
two_dimension$Colors[Label=="disco"]="blue"
two_dimension$Colors[Label=="hiphop"]="green"
two_dimension$Colors[Label=="pop"]="red"
two_dimension$Colors[Label=="reggae"]="brown"
two_dimension$Colors[Label=="rock"]="black"
fix(two_dimension)
attach(two_dimension)
#fix(two_dimension)
#unique(two_dimension$Label)
library(ggplot2)

#Plot multidimensional scaling
ggplot(data=two_dimension, aes(x=V1, y=V2,label=Label, colour=Label))+
  geom_point(size=2, aes(colour=two_dimension$Label, shape=two_dimension$Label))+
  labs(colour="name")+
  theme(legend.position = "right")


