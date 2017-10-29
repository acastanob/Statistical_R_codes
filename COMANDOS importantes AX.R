#  ____________________________________________________________________________________________  #
#  !                             LEER DATOS                                                   !  #
#  !__________________________________________________________________________________________!  #
########--------------------------------------------de EXCEL
library(XLConnect)
wb <- loadWorkbook(file.choose(), create= FALSE)
datos <- readWorksheet(wb, sheet="nombreHoja")

######---------------------------------------------------------------------------------------------------------#######

#######--------------------------------------------Copiar y pegar
read.table("clipboard",header = TRUE)
attach(datos)

######---------------------------------------------------------------------------------------------------------#######

#######--------------------------------------------CSV
system.time(Datpo2<- fread(paste('DATA_SUIC_2.csv',sep=''), sep = "|", na="",stringsAsFactors=F,integer64=getOption("datatable.integer64"),dec="."))

######---------------------------------------------------------------------------------------------------------#######

#######--------------------------------------------de NOTEPAD
read.table(file.choose(),header = TRUE)
attach(datos)

######---------------------------------------------------------------------------------------------------------#######

#####---------------------------------------------SELECCION DATOS
datos[,c(2,4)]
subset(datos,Genero=='F', select=c(2,3))

######---------------------------------------------------------------------------------------------------------#######
#  ____________________________________________________________________________________________  #
#  !                             PROGRAMACIÓN                                                 !  #
#  !__________________________________________________________________________________________!  #
######---------------------------------------------------------------------------------------------------------#######

for( i in 1:10){...}

while(i<10){...}

if(i==w){...}else{...}

x=mat.or.vec(10,10)
cbind(v1,v2,v3)


######---------------------------------------------------------------------------------------------------------#######
#  ____________________________________________________________________________________________  #
#  !                          REGRESION Y DISEÑOS DE EXPERIMENTOS                              !  #
#  !__________________________________________________________________________________________!  #
######---------------------------------------------------------------------------------------------------------#######

##############---------------------------------REG LINEAL
MODELO<-lm(Y~X1+X2)
anova(MODELO)
confint(MODELO, level = 0.95) # Intervalo para B0 y B1
Ygorro<-fitted(MODELO)
res=residuals(MODELO)
plot(res)
abline(h=0, col="red")
shapiro.test(res)
coplot(Y~X1)
######---------------------------------------------------------------------------------------------------------#######

##############---------------------------------REG BAYESIANA
res.bay<-MCMCregress(hijo~padre+madre+genero+origen)
B1.PosterioriMarginal=res.bay[,2]
qqnorm(B1.PosterioriMarginal)
summary(res.bay)
#HIPÓTESIS
sigmas=rgamma(1000, 0.4, rate = 1)
prob.norm<-function(B1.u,B1.sd,B1.PosterioriMarginal) pnorm(0.8, mean=B1.u,sd=B1.sd)
######---------------------------------------------------------------------------------------------------------#######

##############---------------------------------REG NO LINEAL
library(proto)
library(nls2)
library(nlstools)
MODELO=nls(y~ "escribir función" ,data=datos,start=list( valores iniciales de parámetros)
overview(MODELO)
##supuestos
ygorro=fitted(MODELO)
epsilon=epsiloniduals(MODELO)/summary(MODELO)$sigma
####NORMALIDAD
qqnorm(epsilon)
shapiro.test(epsilon)
####independiencia
Rezagados=mat.or.vec(length(epsilon)-1,1)
for( i in 1:length(epsilon)-1){
   Rezagados[i]=epsilon[i+1]
}
plot(epsilon[1:length(epsilon)-1],Rezagados,xlab='residuales',ylab='Rezagados',main='residuales vs Rezagados')
######VARIANZA
par(mfrow=c(1,2))
plot(ygorro,epsilon,xlab='Y ajustados',main='residuales vs Ygorro')
plot(datos[,2],epsilon,xlab='datos',main='residuales vs x')
#prueba levene o barlet
library(nlrwr)
with(datos,leveneTest(y,as.factor(x1)))
bartlett.test(epsiloniduals(MODELO),datos[,2])
######---------------------------------------------------------------------------------------------------------#######

##############---------------------------------DISEÑOS DE EXPERIMENTOS

laboratorio<-c(rep(1,5),rep(2,5),rep(3,5))
laboratorio<-as.factor(laboratorio)
peso<-scan() 
38 43 45 36 45
31 50 37 43 38
50 38 30 41 44
boxplot(split(peso,laboratorio),xlab='Laboratorios',ylab='Peso de estaño')
anovapeso<-aov(peso~laboratorio) 
summary(aov(peso~laboratorio)) # Construye la tabla ANOVA 
res<-residuals(anovapeso) # Se calculan los residuals. 
yh<-fitted(anovapeso) 
shapiro.test(res) # Prueba de normalidad 
qqnorm(res) 
qqline(res) 
library(car) #Para poder hacer la prueba de LEVENE
leveneTest(peso,laboratorio) #Prueba de Levene homogeneidad de varianzas 
bartlett.test(peso,laboratorio) #Prueba de Bartlett homog. de varianzas


#CALCULAR n
library(pwr)
tm<-function(D,potencia,sigma2,alpha)
{
f1<-sqrt(D^2/(2*a*sigma2))
n<-pwr.anova.test (f=f1,k=4,power=potencia,sig.level=alpha)
tam.muestra<-ceiling(n[2]$n)
return(print(c(D,potencia, sigma2,alpha,tam.muestra)))
}


######---------------------------------------------------------------------------------------------------------#######
#  ____________________________________________________________________________________________  #
#  !                                  MUESTREO                                                !  #
#  !__________________________________________________________________________________________!  #
######---------------------------------------------------------------------------------------------------------#######

###Cálculo del tamaño de muestra para estimar varias proporciones simultáneamente
tamaño.max.d2<-function(m,alf) (qnorm(1-alf/(2*m)))^2*(1/m)*(1-1/m)
####Considere el caso con 6 categorías, alfa=0.05
k<-6; alfa=0.05

mm<-matrix(seq(1:k),ncol=1)
nd2<-apply(mm,1,tamaño.max.d2,alfa)
jj<-which.max(nd2)
(nd2optimo<-mm[jj])

######---------------------------------------------------------------------------------------------------------#######
#  ____________________________________________________________________________________________  #
#  !                             SERIES DE TIEMPO                                             !  #
#  !__________________________________________________________________________________________!  #
######---------------------------------------------------------------------------------------------------------#######
# gráfica de la serie
plot.ts(seriew6, type="l")

# transformación de Box-Cox.
box.cox.powers(seriew6) 
lamdaEncontrado= xxxxx
tseriew6=seriew6^ lamdaEncontrado
lseriew6=log(seriew6)

# gráfica de la serie transformada y correlogramas muestrales
plot.ts(lseriew6)
par(mfrow=c(2,1))
acf(lseriew6, lag.max=15, ci=0)
pacf(lseriew6, lag.max=15, ci=0)

# prueba de raíces unitarias sobre la serie transformada en niveles
(maxlag=round(length(seriew6)^(1/3)))
ru_w6=ur.df(lseriew6, type = c("trend"), lags=maxlag, selectlags = c("BIC"))
summary(ru_w6)
# conclusión parece que hay raíz unitaria

# prueba de raíces unitarias sobre la serie diferenciada
ru_w6d=ur.df(diff(lseriew6), type = c("drift"), lags=maxlag, selectlags = c("BIC"))
summary(ru_w6d)
# reespecificacón del modelo
ru_w6d=ur.df(diff(lseriew6), type = c("none"), lags=0)
summary(ru_w6d)
# conclusión parece que la serie diferenciada es estacionaria

# análisis para la serie transformada diferenciada una vez
plot.ts(diff(log(seriew6)), type="l")
# correlogramas muestrales acf(diff(log(seriew6)), lag.max=15, ci=0) y pacf(diff(log(seriew6)), lag.max=15, ci=0)

# estimación del modelo usando la función arima de stats
# estimación ML condicional con deriva
(mod_w6_CSS=arima(diff(log(seriew6)), c(0, 0, 1), method = c("CSS"))) 
(res_w6_CSS=residuals(mod_w6_CSS))
# estimación ML exacta con deriva
(mod_w6_ML=arima(diff(log(seriew6)), c(0, 0, 1), method = c("ML"))) 
(res_w6_ML=residuals(mod_w6_ML))
# estimación ML exacta con valores iniciales dados por la estimación condicional sin deiva
(mod_w6_CSS_ML=arima(diff(log(seriew6)), c(0, 0, 1), method = c("CSS-ML"))) 
(res_w6_CSS_ML=residuals(mod_w6_CSS_ML))



######---------------------------------------------------------------------------------------------------------#######
#  ____________________________________________________________________________________________  #
#  !                             MULTIVARIADO                                                 !  #
#  !__________________________________________________________________________________________!  #
######---------------------------------------------------------------------------------------------------------#######


#########----------------------------------------------  C L U S T E R
#mydata <- scale(datos) # estandarizar variables

############    K-MEANS

######------------------------------------ Determine numero de clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
  	 centers=i)$withinss)
plot(2:15, wss[2:15], type="b", xlab="Number of Clusters",
  ylab="Within groups sum of squares", main="22) Sum_Cuad")



######---------------------------------- K-Means Cluster Analysis
fit <- kmeans(mydata, 4) # 3 cluster solution
# get cluster means 
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
DatosKmeans <- data.frame(mydata, fit$cluster)
max(DatosKmeans$fit.cluster)
DatosKmeans[1:10,]
#lo guarda en DIR (Ruta default de R)
write.table(DatosKmeans, file = "DatosKmeansS9.txt")


######----------------------------------- JERÁRQUICO - Ward Hierarchical Clustering

#library(cluster)
d <- daisy(mydata)
fit <- hclust(d, method="ward") 
plot(fit, main="22) Dendograma") # display dendogram
rect.hclust(fit, k=2, border="red")
groups <- cutree(fit, k=2) # cut tree into 5 clusters
DatosJerarquico <- data.frame(mydata, groups)
write.table(DatosJerarquico, file = "DatosJerarquico_S22.txt")


######---------------------------------- Ward Hierarchical Clustering with Bootstrapped p values
library(pvclust)
fit <- pvclust(mydata, method.hclust="ward",
   method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)


######---------------------------------- Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
plot(fit) # plot results 
summary(fit) # display the best model

######---------------------------------------------------------------------------------------------------------#######
#########--------------------------------------Componentes Principales



library(ade4)
acp <- dudi.pca(perfil,scannf=F,nf=2)  # realiza el ACP
# acpI contiene las ayudas a la interpretacion del ACP
acpI <- inertia.dudi(acp,row.inertia=T,col.inertia=T)
# impresion de objetos de acp y de acpI con titulos
cat("\n Valores propios \n")
print(acpI$TOT,2)
plot(acp$eig)
cat("\n Vectores propios \n")
print(acp$c1)
cat("\n Coordenadas de las columnas \n")
print(acp$co)
cat("\n Contribuciones de las columnas a los ejes \n")
print(acpI$col.abs/100)
cat("\n Calidad de representacion de las columnas \n")
print(acpI$col.rel/100)
cat("\n Calidad de representacion de las columnas en el plano \n")
print(acpI$col.cum/100)
cat("\n Coordenadas de las filas \n")
print(acp$li)
cat("\n Contribuciones de las filas a los ejes \n")
print(acpI$row.abs/100)
cat("\n Calidad de representacion de las filas en los ejes \n")
print(acpI$row.rel/100)
cat("\n Calidad de representacion de las filas en el plano \n")
print(acpI$row.cum/100)

barplot(acp$eig)

par(mfrow=c(2,2)) # para 4 gr¶aficas simult¶aneas
s.corcircle(acp$co,sub="Cafe - C¶³rculo de correlaciones",possub= "bottomright")
# se define iden para tener etiquetas m¶as cortas en las gr¶aficas
iden <- c("EC","C4M","C4C","C2M","C2C","EO","O4M","O4C","O2M","O2C")
# s.label es de ade4 y coloca puntos con etiquetas en planos factoriales
s.label(acp$li,label=iden,sub="Preparaciones de caf¶e",possub= "bottomright")




######---------------------------------------------------------------------------------------------------------#######
#########--------------------------------------Correspondencia

SAS

######---------------------------------------------------------------------------------------------------------#######

#####-------------------------------------------ANALISIS DISCRIMINANTE 
#####-----------------------(mirar si una observacion pertenece a la poblacion 1 o a la 2


bd1<-read.table(file.choose(),header=T)
bd2<-read.table(file.choose(),header=T)
(medbd1=mean(bd1[,1:2]))
(varbd1<-var(bd1[,1:2]))
(medbd2=mean(bd2[,1:2]))
(varbd2<-var(bd2[,1:2]))
#b
bd2<-read.table(file.choose(),header=T)
plot(bd1[,1],bd1[,2])
plot(bd2[,1],bd2[,2])

################

#HIPOTESIS X0 PERTENECE A POBLACION_i

 #DATOS que serviran al hacer hipotesis
  t1<-read.table(file.choose(),header=T)
  t2<-read.table(file.choose(),header=T)
  (mediat1<-mean(t1[,1:2]))
  (covt1<-var(t1[,1:2]))
  (mediat2<-mean(t2[,1:2]))
  (covt2<-var(t2[,1:2]))
  (n1=length(t(t1))/length(t1))#existe una forma mas facil??
  (n2=length(t(t2))/length(t2))
  (p=length(t1))

#PROBAR IGUALDAD DE SIGMAS    si es NORMAL
  (sp=(1/(n1-1)+(n2-1))*(((n1-1)*covt1)+((n2-1)*covt2)))
  (M=(n1-1)+(n2-1)*(log(det(sp), base= exp(1)))- ((n1-1)*det(covt1)+(n2-1)*det(covt2)))
  (u=((1/((n1-1)+(n2-1)))- 1/(1/((n1-1)+(n2-1))))*(((2*p^2)+3*p -1)/(6*(p+1)*(2+1))))
  (c=(1-u)*M)
  qchisq(0.95,((1/2)*p*(p+1)*(2-1)), lower.tail=T)
  #si c > qchisq  rechazo ho, "sigmas diferentes"

  
#Observacion pertenece a poblacion 1 ; caso SIGMAS IGUALES
(Sp=(((n1-1)*covt1)+((n2-1)*covt2))/(n1+n2-2))
x0=c(380,10.05)
(ygorro=(t(mediat1-mediat2))%*%solve(Sp)%*%x0)
mediay1=(t(mediat1-mediat2))%*%solve(Sp)%*%mediat1
mediay2=(t(mediat1-mediat2))%*%solve(Sp)%*%mediat2
(m=(1/2)*(mediay1-mediay2))
#si ygorro > m entonces X0 pertenece a poblacion 1

######---------------------------------------------------------------------------------------------------------#######
