pacman::p_load(tidyverse, easystats, boot)
data("iris")
head(iris)
rs <- iris %>% 
  correlation() %>% .$r
names(rs) <- iris %>% 
  correlation() %>% data.frame() %>% 
  mutate(corxy = paste(Parameter1, Parameter2)) %>% .$corxy
rs


rs_fun <- function(data,indices){
  d <- data[indices, ]
  rs <- d %>% 
    correlation() %>% .$r
  names(rs) <- d %>% 
    correlation() %>% data.frame() %>% 
    mutate(corxy = paste(Parameter1, Parameter2)) %>% .$corxy
  rs
}

data(mtcars)
head(mtcars)

bootstrapped_rs <- boot(data = iris,
                        R = 1000, 
                        statistic = rs_fun)
booted_rs = matrix(ncol = 3, 
                      nrow = NROW(bootstrapped_rs$t0))

for(i in 1:NROW(bootstrapped_rs$t0)){
  tmp = boot::boot.ci(bootstrapped_rs,
                      type = "bca", index = i)
  tmp2 = c(tmp$t0, tmp$bca[,4:5])
  booted_rs[i,] = tmp2
}

rownames(booted_rs) <- names(bootstrapped_rs$t0)


colnames(booted_rs) <- c("r",
                            "Lower",
                            "upper")
BOOT_RS <- booted_rs %>% data.frame() %>% 
  rownames_to_column(var = "Variable pair") %>% 
  mutate(across(where(is.numeric), ~round(.x,2)))
BOOT_RS


# ------------

rs_fun <- function(data,indices){
  d <- data[indices, ]
  rs <- d %>% 
    correlation() %>% .$r
  names(rs) <- d %>% 
    correlation() %>% data.frame() %>% 
    mutate(corxy = paste(Parameter1, Parameter2)) %>% .$corxy
  rs
}

bootstrapped_rs <- boot(data = cars,
                        R = 1000, 
                        statistic = rs_fun)
booted_rs = matrix(ncol = 3, 
                   nrow = NROW(bootstrapped_rs$t0))

for(i in 1:NROW(bootstrapped_rs$t0)){
  tmp = boot::boot.ci(bootstrapped_rs,
                      type = "bca", index = i)
  tmp2 = c(tmp$t0, tmp$bca[,4:5])
  booted_rs[i,] = tmp2
}

rownames(booted_rs) <- names(bootstrapped_rs$t0)


colnames(booted_rs) <- c("r",
                         "Lower",
                         "upper")
BOOT_RS <- booted_rs %>% data.frame() %>% 
  rownames_to_column(var = "Variable pair") %>% 
  mutate(across(where(is.numeric), ~round(.x,2)))
BOOT_RS
