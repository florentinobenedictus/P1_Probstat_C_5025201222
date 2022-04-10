#No. 4
#4A
soal_4a<-function(x, v){
  dchisq(x,v)
}
soal_4a(2, 10)

#4B
soal_4b<-function(rd){
  res_rchisq <- rchisq(rd, df = 10)
  hist(res_rchisq,main="4B. Histogram Chi-Square 100 data random")
}
soal_4b(100)

#4C
soal_4c<-function(v){
  rataan=v
  varian=2*v
  cat("4C. Rataan adalah:", rataan,",dan varian adalah:", varian,"\n")
}
soal_4c(10)
