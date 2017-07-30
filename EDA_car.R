## Jenjong1
rm(list = ls())
gc()
setwd("D:/rProg/Rank_consistency")
rdata<- as.matrix( read.csv(file = "./data/racing_data.csv", header = F) )
num.vec <- rdata[,1]
pl.mat <- rdata[,2:17]
car.mat <- rdata[,18:33]

p = 43
car.mat.p <- matrix(0,p*(p-1)/2, 2)
ii <- 1
for( i in 1:(p-1))
{
  for( j in (i+1):p)
  {
    car.mat.p[ii, 1] <- i
    car.mat.p[ii, 2] <- j
    ii <- ii + 1
  }
}
car.mat.p <- cbind(1:nrow(car.mat.p), car.mat.p,0)
pl.list <-  vector(mode = 'list', length = nrow(car.mat.p))


idx.fun <- function(i1, i2, p)
{
  if (i1 == 1) 
  {
    str.v = 0
    end.v = i2-i1
    v = str.v + end.v
    return(v)
  }
  
  if (i1>1)
  {
    str.v  = ( 2*(p-1) - (i1-2) )*(i1-1)/2
    end.v = i2 - i1
    v = str.v + end.v
    return(v)
  }
}

# list 
i = 1
mat <- NULL
id.vec <- c()
for ( i in 1:length(num.vec))
{
  a1 <- num.vec[i]
  a2 <- pl.mat[i,1:a1]
  a3 <- car.mat[i,1:a1]

  for (j in 1:(length(a2)-1) )
  {
    for (k in (j+1):length(a2))
    {
      tmp <- c(a3[j], a3[k]) 
      tmp.pl <- c(a2[j], a2[k]) 
      if ( a3[j] == a3[k] ) next
      if ( a3[j] != a3[k] )
      {
        idx.min <- which.min(tmp)
        idx.max <- which.max(tmp)
        i1 = tmp[idx.min]
        i2 = tmp[idx.max]
        loc <- idx.fun(i1, i2, p )
        # add n_jk
        car.mat.p[loc,4] <- car.mat.p[loc,4] + 1
        # add player
        tmp.vec <- pl.list[[loc]]
        tmp.vec <- c(tmp.vec, tmp.pl)
        pl.list[[loc]] <- tmp.vec  
      }
      
    }
  }
  cat(i,'\n')
}
v = c()
i = 1
idx = 1
rlist = list()
rmat = c()
i.idx <- c()
for (i in 1:length(pl.list))
{
  tmp.v <- car.mat.p[i,4]
  if (tmp.v <= 6 ) next
  tmp <- pl.list[[i]]
  tv <- max(table(tmp)/tmp.v)
  if (tv == 1) 
  {
    rlist[[idx]] <- table(tmp)
    rmat <- c(rmat, tmp.v)
    idx = idx + 1
  }
  v = c(v,tv)
}

vv = c()
for ( i in 1:length(rlist)) 
{
  tmp  = rlist[[i]]
  if (length(tmp) == 2)
  {
    vv = c(vv, names(tmp))
    next
  } else {
    
    vv <- c(vv, names(tmp)[which.max(tmp)])
  }

}


