wald2ci <-
function(x1, n1, x2, n2, conf.level){
   p1hat = x1/n1
   p2hat = x2/n2
   z = abs(qnorm((1-conf.level)/2))
   ll = (p1hat - p2hat)  - z*sqrt((p1hat*(1-p1hat))/n1 + (p2hat*(1-p2hat))/n2)
   ul = (p1hat - p2hat)  + z*sqrt((p1hat*(1-p1hat))/n1 + (p2hat*(1-p2hat))/n2)
   c(ll,ul)
}

