#Projekt 3
#zdefiniowanie odcinka czasowego
t=seq(0,0.5,by = 0.001)

#zdefiniowanie funkcji ruchu Browna
brown<-function(czasy)
{
  wynik<-c(0)
  tmp<-0
  for (i in 1:(length(czasy)-1)) {
    tmp=tmp+rnorm(1,mean=0,sd=sqrt(czasy[i+1]-czasy[i]))
    wynik<-c(wynik,tmp)
  }
  return(wynik)
}

#Symulacja zadanego procesu metod¹ Eulera
euler<-function(t,x0)
{
  podstawa<-brown(t)
  wynik<-c(x0)
  tmp=x0
  for (i in 1:(length(t)-1))
  {
    tmp=tmp+(3*tmp-exp(-t[i]))*(t[i+1]-t[i])+3*(podstawa[i+1]-podstawa[i])
    wynik=c(wynik,tmp)
  }
  return(wynik)
}
#u mnie metoda Millsteina bêdzie idendtyczna z metod¹ Eulera, poniewa¿ pochodna po wariancji wynosi 0

#Generowanie trajektorii
sde_euler<-euler(t,1)
layout(1:3)
plot(t,euler(t,1),type='l',main="Przyk³adowe trajektorie",xlab = "czas",ylab = 'Wartoœæ procesu')
plot(t,euler(t,1),type='l',xlab = "czas",ylab = 'Wartoœæ procesu')
plot(t,euler(t,1),type='l',xlab = "czas",ylab = 'Wartoœæ procesu')

#stworzenie 1000 trajektorii
all_traj<-replicate(1000,euler(t,1))

#Sprawdzenie funkcji œredniej
srednie<-rowMeans(all_traj)
layout(1:2)
plot(t,srednie,type='l',xlab="czas",ylab="Œrednia",main = "Wykres numerycznie wyznaczonej œredniej")
wartosc_oczekiwana<-0.25*exp(-t)+0.75*exp(3*t)
lines(t,wartosc_oczekiwana,col='red') # porownanie z wartoscia teoretyczna
plot(t,wartosc_oczekiwana,xlab="czas",ylab="Œrednia",type='l',main = "Wykres analitycznie wyznaczonej wartoœci")

#Sprawdzenie funkcji wariancji
wariancje<-apply(all_traj,1, var)
plot(t,wariancje,type = 'l',xlab="czas",ylab="Wariancja",main = "Wykres analitycznie wyznaczonej wariancji")
wariancja_teoretyczna<-1.5*(exp(6*t)-1)
lines(t,wariancja_teoretyczna,col='red')#porównanie z wartoœci¹ teoretyczn¹
plot(t,wariancja_teoretyczna,xlab="czas",ylab="Wariancja",type='l',main = "Wykres numerycznie wyznaczonej wariancji")
hist(all_traj,probability = T,xlim = c(-200,200),breaks = 1000,density = T)
range(all_traj)

#Rozklad wyznaczony analitycznie
rozklad<-matrix(nrow = 1000,ncol = length(wartosc_oczekiwana))
rozklad[1,]=rep(1,length(wartosc_oczekiwana))
for (j in 2:1000) {
  
  for (i in 1:length(wartosc_oczekiwana)) {
    
    rozklad[j,i]<-rnorm(1,mean=wartosc_oczekiwana[i],sd=sqrt(wariancja_teoretyczna[i]))
  }
}

#Porownanie gestosci
plot(density(all_traj),col='red',main='Wykres gêstoœci dla t=0.5')
lines(density(rozklad),col='blue')

