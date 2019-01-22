cwas_mdmr=function(modelfile,formula,subjectlist,factor2perm=NULL,nperm=499)
#AZEEZ PENN BBL 
{
library(MatchLinReg)
library(MASS)
require(psych) # fora< rtoz transformation
# load the model file  first and estimate the number of subjects 
# let the subject functional connectivity matrices  be in csv file and arrange in the same order as model
model=read.csv(modelfile,header=T)
N=nrow(model) # number of subjects
b=model.matrix(formula,model) # formular must stats with ~ eg ~x1 +x2 : interaction can be included
B=ncol(b)
# read the list of the functional matrix
subjlist=read.csv(subjectlist, header = F)
subjlist[] = lapply(subjlist, as.character)
# read the functional matrix of the first subject to determine the  number of rois
s1=read.table(subjlist[1,],header=F)
X=(ncol(s1)) # number of Regions 
 # the distance and the mdmr will be calculated  in loop # for each ROIs 
 G=matrix(NA,N*N,X)
 #G1=big.matrix(ncol = N*N,nrow = X,NA,shared = T)
 permat = matrix(NA, N, nperm)
 b2=b; b2=b2[ , -match(factor2perm,colnames(b2)) ] 
 #H <- b %*% solve(t(b) %*% b) %*% t(b)  # hat matrix 
 #H2 <- b2 %*% solve(t(b2) %*% b2) %*% t(b2)  # hat matrix 
 #H3=H-H2

 #  TOL=1e-07
 # # 
 # qrX <- qr(b, tol=TOL)
 # Q <- qr.Q(qrX)
 # H <- tcrossprod(Q[,1:qrX$rank])
 # 
 # qrX <- qr(b2, tol = TOL)
 # Q <- qr.Q(qrX)
 # H3 <- H - tcrossprod(Q[, 1:qrX$rank])
 # 
 for (k in 1:nperm) permat[ , k] = sample(N)
 #for (k in 1:nperm) permat2[ , k] = sample(N)
 #if (is.null(metric)){metric="pearson"}
 if(is.null(factor2perm)) {stop("provide the factor to regress with distance")}
for (i in 1:X) {
    x=matrix(0, nrow = X, ncol = N) # initialize x
    for (j in 1:N) {
        mat1=read.table(subjlist[j,])
        mat1=cor(mat1)
        mat1=as.matrix(mat1)
        x[,j]=mat1[i,] 
    }
    x=x[-i,] # remove the seed regions  as suggested by TS/DSB
    #x=fisherz(x) # rto z tranform as  done in CWAS-MDMR paper Shehzad  et al 2014
    x=scale(x, center = TRUE, scale = TRUE) #  scale  to mean of 0 and std of 1 Shehzad  et al 2014
    #compute distance matrix: correlation computations for this code; pearson spearman and kendall.  
    D1=cor(x,method='pearson'); D=sqrt(2*(1-D1)); #standard stranformation of distance Shezad 2014, Mardia 1979)
    
    A=-(D*D)/2; 
    II=diag(1, nrow = N, ncol = N); IV=rep(1,1,N) ;adj <- II - IV %*% t(IV)/N
    g= -0.5 * D^2 %*% (diag(N) - IV %*% t(IV)/N)
    #CC=II-IV %*% t(IV)/N; g=CC*A*CC;
    #g=gower(D)
    G[,i]=as.vector(g) #Compute Gower's centered similarity matrix G,

} 
 
 H1 = b %*% solve(t(b) %*% b) %*% t(b)
 IH = diag(N) - H1
 H2 = H1 - (b2 %*% solve(t(b2) %*% b2) %*% t(b2))
 
 Num=(crossprod(G,as.vector(H2)))/(B); Den=(crossprod(G,as.vector(IH)))/(N-B)
 Fstat=t(Num/Den)
 
 #permutation zone
 bb=b[ , -match(factor2perm,colnames(b)) ] 
 d1=b[ , match(factor2perm,colnames(b)) ] 
 ret=mlr.orthogonalize(X=bb, Z=d1, normalize = FALSE, tolerance = .Machine$double.eps^0.5)
 orthofat=ret[,1]
 b3=b;
  
  pb <- txtProgressBar(min = 0, max = nperm, style = 3)
  H3mat=Hmat=matrix(NA, N^2, nperm)
          for (k in 1:nperm) {
     	      prm = permat[ , k]
     	      b3[,match(factor2perm,colnames(b3))]=orthofat[prm]
     	      b4=b3[,-match(factor2perm,colnames(b3))]
     	      H3 = b3 %*% solve(t(b3) %*% b3) %*% t(b3)
     	      HD = diag(N) - H3
     	      H4 = H3 - (b4 %*% solve(t(b4) %*% b4) %*% t(b4))
     	      H3mat[ , k]=H4
            Hmat[ , k]= HD
            setTxtProgressBar(pb, k,title = 'HAT matrix')
          }
  close(pb)

 # Nump=((H3mat)%*%G)/(B); Denp=((Hmat)%*%G)/(N-B)
  Nump=(crossprod(H3mat,G))/B; Denp=(crossprod(Hmat,G))/(N-B)
  Fstatp=Nump/Denp
  pvalue=apply(rbind(Fstat, Fstatp), 2, function(v) sum(v[1]<=v)) / (nperm+1)

  #corrected.p=(rowSums(outer(F, maxpermF, "<="))+1) / (nperm+1)
  #maxpermF=apply(Fstatp, 1, max)
  zvals = qt(pvalue/2, Inf, lower.tail=FALSE)
  #Since a p-value of 1, gives an infinite z-stat
  # then we can adjust the input p-value to be nperms/(nperms+1)
  inf_zvals = is.infinite(zvals)
  if (any(inf_zvals))
  cat(sul./m(inf_zvals), "infinite z-value(s) found\n, setting to nperms/(nperms+1)\n") # usually none if number of perumtaions
  zvals[inf_zvals] = nperm/(nperm+1)                                                    #  if the permutation number is  high like 1000
  pandz <- list(pvalue,zvals)
  return(pandz)
}
