iqroutlier = function(var = x, id = y, rule = z){
  require(psych)
  m = describe(var, IQR = TRUE, quant = c(.25,.75))
  IQR = m$IQR
  q1 = m$Q0.25
  q3 = m$Q0.75
  dtf = data.frame(ID=id, 
                   obs=var, outlier=var<q1-rule*IQR | var>q3+rule*IQR)
  lower = q1-rule*IQR
  upper = q3+rule*IQR
  outliern = length(which(dtf=="TRUE"))
  casenumber = ifelse(dtf$outlier == TRUE, dtf$ID, NA)
  casenumber = na.omit(casenumber)
  n = length(casenumber)
  
  result = list(q1 = q1,
                q3 = q3,
                IQR = IQR,
                multiplier = rule,
                'number of outliers detected' = outliern,
                'id number of cases detected' = casenumber[1:n])
  return(result)
}
