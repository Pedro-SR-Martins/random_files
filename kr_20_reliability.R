kr20 = function(x) { 
  x=na.omit(x)#tirar missing
  k = ncol(x) 
  k1 = k -1
  p = sapply(x, mean, na.rm=TRUE)#m?dia das colunas (taxa de acerto)
  q = 1 - p #(taxa de erro)
  pq = sum(p * q)
  x$sum = rowSums(x)#pra cada sujeito
  o2 = (sd(x$sum))^2#vari?ncia
  KR20 = ((k/k1)*(1-(pq/o2)))
  coeff = KR20
  percents = data.frame(error = q, accuracy = p)
  n = nrow(x)
  final = list(kr20 = coeff, n = n, percents = percents)
  print(final)
}
