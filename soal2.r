#No. 2
#2A
soal_2a<-function(x, n, p){
  cat("2A. Peluang terdapat",x,"pasien sembuh sebesar",dbinom(x, n, p),"\n")
}
soal_2a(4, 20, 0.2)

#2B
soal_2b<-function(n, p){
  success <- 0:n
  plot(success, dbinom(success, n, p),type='h',main="2B. Distribusi Binomial",
       ylab="Peluang",xlab="Jumlah Sembuh",lwd=5)
}
soal_2b(20, 0.2)

#2C
soal_2c<-function(n, p){
  rataan=n*p
  q=1-p
  varian=n*p*q
  cat("2C. Rataan adalah:", rataan,",dan varian adalah:", varian,"\n")
}
soal_2c(20, 0.2)
