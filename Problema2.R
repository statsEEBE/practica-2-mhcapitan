#Codigo para problema 2
iris
mis_dades <- iris
y<-mis_dades$Sepal.Length
y
x<-mis_dades$Petal.Length
x
plot(x,y)
xbar<-mean(x)
xbar#tengo que llamar a la variable despues de almacenarla

ybar<-mean(y)
ybar

m<-sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)
#al poner x ya estoy llamando a cada uno de loa valores de la lista
#la pendiente es la formula de arriba en minimos cuadrados
b<-ybar-m*xbar
b
q<-m*1.5+b#con la pendiente y la 'b' calculada encontramos la prediccion en x=1.5
q

mod<-lm(y~x)

data.frame(x=x)

ypredicted<-predict(mod, data.frame(x=x))
ypredicted
#estamos calculando todas las predicciones de 'y' para todas las x
plot(x,y)
lines(x, ypredicted)

#coeficiente de determinacion, grado de dispersion sobre 1 al rededor de la media,
#la recta de minimos cuadrados

Rsq<-sum((ypredicted-ybar)^2)/sum((y-ybar)^2)
Rsq


summary(mod)#analiza datos de 'mod' que es un sistema lineal en este caso
sqrt(Rsq)
