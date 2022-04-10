#No. 5
#5A
soal_5a<-function(x,lambda){
  #Nilai x dapat diganti karena belum ada di soal
  dexp(x,rate=lambda)
}
soal_5a(5,3)

#5B
soal_5b<-function(x,lambda){
  set.seed(1)
  hist(rexp(x,rate=lambda))
}
soal_5b(10,3)
soal_5b(100,3)
soal_5b(1000,3)
soal_5b(10000,3)

#5C
soal_5c<-function(lambda){
  rataan=1/lambda
  varian=1/(lambda^2)
  cat("5C. Rataan:",rataan,", dan varian:",varian,"\n")
}
soal_5c(3)
