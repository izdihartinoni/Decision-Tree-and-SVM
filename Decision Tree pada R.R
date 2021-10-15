#decision tree
library(rpart)
data(iris)

#membagi data training-testing menggunakan hold out (80% training, 20%testing)
n=round(0.8*nrow(iris))
index=sample(seq_len(nrow(iris)),size=n)
traind=iris[index,]
testd=iris[-index,]

#membuat model decision tree
dt_model<-rpart(Species~.,method='class',data=traind)
summary(dt_model)

#visualisasi decision tree
win.graph()
plot(dt_model)
text(dt_model,use.n=TRUE)

#ketepatan klasifikasi
pred51<-predict(dt_model,traind[1:4],type='class')
pred52<-predict(dt_model,testd[1:4],type='class')
ak51=table(pred51,traind$Species)
ak52=table(pred52,testd$Species)
akurasi51=sum(diag(ak51))/nrow(traind)
akurasi52=sum(diag(ak52))/nrow(testd)
table(testd$Species,pred52)
table(traind$Species,pred51)
spesi51=sum(ak51[2:3,2:3])/sum(ak51[2:3,1:3])
spesi52=sum(ak52[2:3,2:3])/sum(ak52[2:3,1:3])
sensi51=sum(ak51[1,1])/sum(ak51[1,1:3])
sensi52=sum(ak52[1,1])/sum(ak52[1,1:3])
