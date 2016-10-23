
1 + 1e-15 -1

1 + 1e-16 -1

.Machine$double.eps

log(.Machine$double.eps)/log(2)

DiffSqrtBad(1e15) # Some value

DiffSqrtBad(1e16) # 0 (wrong) !!

DiffSqrtGood(1e16) 

DiffSqrtBad(x<-1e15) - DiffSqrtGood(x) # The error of DiffSqrtBad is quite big!
