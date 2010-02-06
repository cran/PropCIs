blakerci <-
function(x,n,level,tolerance=1e-05){
  lower = 0
  upper = 1
  if (x!=0){lower = qbeta((1-level)/2, x, n-x+1)
            while (acceptbin(x, n, lower + tolerance) < (1 - level))
              lower = lower+tolerance
          }
  if (x!=n){upper = qbeta(1 - (1-level)/2, x+1, n-x)
            while (acceptbin(x, n, upper - tolerance) < (1 - level))
              upper = upper-tolerance
          }
  c(lower,upper)
}

