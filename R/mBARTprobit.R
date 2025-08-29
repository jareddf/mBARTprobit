mBARTprobit=function(
    x.train,y.train, x.test=matrix(0.0,0,0),
    sigest=NA, sigdf=3, sigquant=.90,
    k=2.0,
    power=.8, base=.25, #regular bart is 2,.95
    sigmaf=NA,
    lambda=NA,
    offset = qnorm(mean(y.train)),
    ntree=200,
    ndpost=1000, nskip=100,
    mgsize=50,
    nkeeptrain=ndpost,nkeeptest=ndpost,
    nkeeptestmean=ndpost,
    nkeeptreedraws=ndpost,
    printevery=10
)
{
  ##require(Rcpp)
  #--------------------------------------------------
  nd = ndpost
  burn = nskip
  #--------------------------------------------------
  #data
  n = length(y.train)
  p = ncol(x.train)
  np = nrow(x.test)
  x = t(x.train)
  xp = t(x.test)
  # y.train = y.train-offset
  #--------------------------------------------------
  #set  nkeeps for thinning
  if((nkeeptrain!=0) & ((ndpost %% nkeeptrain) != 0)) {
    nkeeptrain=ndpost
    cat('*****nkeeptrain set to ndpost\n')
  }
  if((nkeeptest!=0) & ((ndpost %% nkeeptest) != 0)) {
    nkeeptest=ndpost
    cat('*****nkeeptest set to ndpost\n')
  }
  if((nkeeptestmean!=0) & ((ndpost %% nkeeptestmean) != 0)) {
    nkeeptestmean=ndpost
    cat('*****nkeeptestmean set to ndpost\n')
  }
  if((nkeeptreedraws!=0) & ((ndpost %% nkeeptreedraws) != 0)) {
    nkeeptreedraws=ndpost
    cat('*****nkeeptreedraws set to ndpost\n')
  }
#
#   #--------------------------------------------------
#   #sigest
#   if(is.na(sigest)) {
#     if(p < n) {
#       df = data.frame(x.train,y.train)
#       lmf = lm(y.train~.,df)
#       sigest = summary(lmf)$sigma
#     } else {
#       sigest = sd(y.train)
#     }
#   }
  #--------------------------------------------------
  #prior
  nu=sigdf
  # if(is.na(lambda)) {
  #   if(is.na(sigest)) {
  #     if(p < n) {
  #       df = data.frame(x.train,y.train)
  #       lmf = lm(y.train~.,df)
  #       sigest = summary(lmf)$sigma
  #     } else {
  #       sigest = sd(y.train)
  #     }
  #   }
  #   qchi = qchisq(1.0-sigquant,nu)
  #   lambda = (sigest*sigest*qchi)/nu #lambda parameter for sigma prior
  # }

  # if(is.na(sigmaf)) {
  #   tau=(max(y.train)-min(y.train))/(2*k*sqrt(ntree));
  # } else {
  #   tau = sigmaf/sqrt(ntree)
  # }
  # JDF EDIT as per gbart:
  tau = 3/(k*sqrt(ntree))
  tau = sqrt(1.467)*tau #adjustment for monotonic constraint
  lambda = 1
  # sigma = 1 set in .cpp file

  #--------------------------------------------------
  #call
  res = .Call("cmonbart",
              x,
              y.train,
              xp,
              tau,
              nu,
              lambda,
              base,
              power,
              offset,
              nd,
              burn,
              ntree,
              mgsize,
              nkeeptrain,
              nkeeptest,
              nkeeptestmean,
              nkeeptreedraws,
              printevery
  )
  res$yhat.train = res$yhat.train+offset
  res$prob.train = pnorm(res$yhat.train)
  res$prob.train.mean <- apply(res$prob.train, 2, mean)
  res$yhat.test = res$yhat.test+offset
  res$prob.test = pnorm(res$yhat.test)
  res$prob.test.mean <- apply(res$prob.test, 2, mean)
  res$nkeeptreedraws=nkeeptreedraws
  # res$mu=fmean
  # attr(res, 'class') <- 'wbart'
  print('new script is done')
  return(res)

}
