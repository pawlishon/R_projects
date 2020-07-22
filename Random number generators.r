library(tidyverse)

#Generator - t student ----------------------------
t_dist <- function(x){
  return(1/2 + (x)/(2*sqrt(x^2 + 2)))  
}

t_inv_dist <- function(u){
  return(sqrt((2*(2*u - 1)^2)/((1-(2*u - 1)^2))))
}
x=seq(0.01,1,by = 0.01)
dane=data_frame(x,y=t_dist(x),z=t_inv_dist(x))
dist_plot<-ggplot(dane) + geom_point(aes(x,y),col='red')+geom_point(aes(x,z),col='blue')

generator_t <- function(n){
  t_sample <- c()
  for(i in 1:n){  
  u <- runif(1)
  if(u < 0.5){
    t_sample[i] <- sqrt(2/(1-4*(u - 1/2)^2) - 2)
  }else{ 
    t_sample[i] <- -sqrt(2/(1-4*(u - 1/2)^2) - 2)
  }
  }
  return(t_sample)
}

#Generator - rozklad chi- kwadrat---------------------------------

#Generator rozklad normalny - Box Muller----------------------
generator_n <- function(n,mu=0,sigma=1,x_0=1,pi=3.14){
  if (sigma > 0){
    u_1 <- runif(n)
    u_2 <- runif(n)
    n_vec <-  (sqrt(-2*log(u_1))*cos(2*pi*u_2)+mu)*sigma
    return(n_vec) 
  }else{
    print('Sigma ujemna')
  }
}

generator_chi <- function(n ,k){#po co to k?
  n1 = generator_n(n)
  n2 = generator_n(n)
  n3 = generator_n(n)
  n4 = generator_n(n)
  chi_sample <- n1^2 + n2^2 + n3^2 + n4^2
  return(chi_sample)
}

#Poisson generator-------------------------------------------------
# Rozklad poissona z miedzyczasow w procesie Poissona
n2 = 1000
generator_p <- function(n, lambda){
  s = c()
  for(k in 1:n){
    www <- runif(n2)
    for(i in 1:n2){
      if(prod(www[1:i])<exp(-lambda)) break
    }
    s[k] <- i - 1
  }
  return(s)
}


##Symulacje ------------------------------

#1 Narysowanie dystrybuant----------------------------------------------------------
x_cdf <- -50:50
x_inv_cdf <- seq(0,1, 0.01)
data_t_cdf <- data.frame(p=x_cdf, dist = t_dist(x_cdf), group = "CDF")

dist <- c()
dist[1:50] <- -t_inv_dist(x_inv_cdf[1:50])
dist[51:length(x_inv_cdf)] <- t_inv_dist(x_inv_cdf[51:length(x_inv_cdf)])

data_t_inv <- data.frame(p=x_inv_cdf, dist = dist, group = "Inverted CDF")

plot_t_cdf <- ggplot(data_t_cdf, aes(x = p, y = dist)) + 
  geom_point() + geom_line() + xlab("Dystrybuanta - rozk?ad T Studenta")

plot_t_inv <- ggplot(data_t_inv, aes(x = p, y = dist)) + 
  geom_point() + geom_line() + xlab("Wykres dystrybuanty odwrotnej")

gridExtra::grid.arrange(plot_t_cdf, plot_t_inv, ncol = 2)

#2 Statystyki opisowe oraz porównanie--------------------------
#Dla T Studenta
summary(generator_t(10))
summary(rt(10,2))
summary(generator_t(100))
summary(generator_t(1000))

porownanie <- matrix(c(summary(generator_t(10)),
                       summary(generator_t(100)),
                       summary(generator_t(1000)),summary(rt(1000000,2))),
                       4,6,byrow = T)

colnames(porownanie) <- c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.' )
rownames(porownanie) <- c('Generator dla N=10','Generator dla N=100',
                          'Generator dla N=1000','Wartosc zblizona do teoretycznej')
#Dla Chi 
summary(generator_chi(10))
summary(generator_chi(100))
summary(generator_chi(1000))

porownanie_c <- matrix(c(summary(generator_chi(10)),
                       summary(generator_chi(100)),
                       summary(generator_chi(1000)),summary(rchisq(1000000,4))),
                     4,6,byrow = T)

colnames(porownanie_c) <- c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.' )
rownames(porownanie_c) <- c('Generator dla N=10','Generator dla N=100',
                          'Generator dla N=1000','Wartosc zblizona do teoretycznej')

#Dla Poisson
summary(generator_p(10))
summary(generator_p(100))
summary(generator_p(1000))

porownanie_p <- matrix(c(summary(generator_p(2,10)),
                         summary(generator_p(2,100)),
                         summary(generator_p(2,1000)),summary(rpois(1000000,2))),
                       4,6,byrow = T)

colnames(porownanie_p) <- c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.' )
rownames(porownanie_p) <- c('Generator dla N=10','Generator dla N=100',
                            'Generator dla N=1000','Wartosc zblizona do teoretycznej')

#3 Histogramy i gęstości estymowane ------------------------------
t_sample <- generator_t(10000)
p_sample <- generator_p(10000, 2)
chi_sample <- generator_chi(10000)

t_sample_th <- rt(10000, 2)
p_sample_th <- rpois(10000, 2)
chi_sample_th <- rchisq(10000, 4)

t_data <- data.frame(p = t_sample, group = "generated")
p_data <- data.frame(p = p_sample, group = "generated")
chi_data <- data.frame(p = chi_sample, group = "generated")
t_data2 <- data.frame(p = t_sample_th, group = "build-in")
p_data2 <- data.frame(p = p_sample_th, group = "build-in")
chi_data2 <- data.frame(p = chi_sample_th, group = "build-in")

t_data <-  rbind(t_data, t_data2) 
p_data <-  rbind(p_data, p_data2)
chi_data <-  rbind(chi_data, chi_data2)

plot_t_hist <- ggplot() + 
  geom_histogram(data = t_data[t_data$group == "generated", ], aes(x = p,y = stat(count / sum(count)))) +
  geom_density(data = t_data[t_data$group == "build-in", ], aes(x = p, col = "red"), alpha=0.3) +
  ylab("")+
  xlab("")+
  xlim(-10, 10) 

plot_p_hist <- ggplot() + 
  geom_histogram(data = p_data[p_data$group == "generated", ], aes(x = p, y = stat(count / sum(count)))) +
  geom_density(data = p_data[p_data$group == "build-in", ], aes(x = p, col = "red"), alpha=0.3) +
  ylab("")+
  xlab("")+
  xlim(-5, 10)

plot_chi_hist <- ggplot() + 
  geom_histogram(data = chi_data[chi_data$group == "generated", ], aes(x = p,y = stat(count / sum(count)))) +
  geom_density(data = chi_data[chi_data$group == "build-in", ], aes(x = p, col = "red"), alpha=0.3) +
  ylab("")+
  xlab("")+
  xlim(0, 25)

#4 Dystrybuanta empiryczna + teoretyczna--------------------
#T Student
x_cdf <- -50:50
emp_t_sample <- ecdf(t_sample)
emp_t_sample <- emp_t_sample(x_cdf)

data_t_emp <- data.frame(p=x_cdf, dist_emp = emp_t_sample, dist = pt(x_cdf, 2))

plot_t_cdf <- ggplot(data_t_emp) + 
  geom_point(aes(x = x_cdf, y = dist_emp, col = "empiryczna")) + 
  geom_point(aes(x = x_cdf, y = dist,col='teoretyczna'), alpha = 0.5)+
  labs(colour='Dystrybuanta')+xlab('x')+ylab('y')
plot_t_cdf

#Chi Kwadrat
emp_chi_sample <- ecdf(chi_sample)
emp_chi_sample <- emp_chi_sample(x_cdf)

data_chi_emp <- data.frame(p=x_cdf, dist_emp = emp_chi_sample, dist = pchisq(x_cdf, 4))

plot_chi_cdf <- ggplot(data_chi_emp) + 
  geom_point(aes(x = x_cdf, y = dist_emp, col = "empiryczna")) + 
  geom_point(aes(x = x_cdf, y = dist,col='teoretyczna'), alpha = 0.5)+
  labs(colour='Dystrybuanta')+xlab('x')+ylab('y')+xlim(c(0,50))
plot_chi_cdf

#Poisson
emp_p_sample <- ecdf(p_sample)
emp_p_sample <- emp_p_sample(x_cdf)

data_p_emp <- data.frame(p=x_cdf, dist_emp = emp_p_sample, dist = ppois(x_cdf, 2))

plot_p_cdf <- ggplot(data_p_emp) + 
  geom_point(aes(x = x_cdf, y = dist_emp, col = "empiryczna")) + 
  geom_point(aes(x = x_cdf, y = dist, col='teoretyczna'),alpha = 0.5)+
  labs(colour='Dystrybuanta')+xlab('x')+ylab('y')+xlim(c(0,50))
plot_p_cdf

#5 Testy dla rozkładów ciągłych-----------------------------
ks.test(chi_sample, chi_sample_th)
ks.test(t_sample, t_sample_th)
