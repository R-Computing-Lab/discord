rmvn <- function(n,sigma) {
  Sh <- with(svd(sigma), v%*%diag(sqrt(d))%*%t(u))
  matrix(rnorm(ncol(sigma)*n),ncol=ncol(sigma))%*%Sh
}

kinsim1 <- function(
  r=c(1,.5),		# levels of relatedness, default is MZ and DZ twins
  npg=100,
  npergroup=rep(npg,length(r)),	#
  mu=0,			#intercept
  ace=c(1,1,1), #variance
  r_vector=NULL, # alternative specification, give vector of rs
  variance=FALSE, #if ACE is given as raw variance, alternative is 
  ...){
  if(variance){
    sA <- ace[1]^0.5; sC <- ace[2]^0.5; sE <- ace[3]^0.5
  }else{
    sA <- ace[1]*0+1; sC <- ace[2]*0+1; sE <- ace[3]*0+1
  }
  
  S2 <- matrix(c(0,1,1,0),2)
  datalist <- list()
  
  if(is.null(r_vector)){
    id=1:sum(npergroup)
    for(i in 1:length(r)){
      n = npergroup[i]
      
      A.r <- sA*rmvn(n,sigma=diag(2)+S2*r[i])
      C.r <- rnorm(n,sd=sC);	C.r <- cbind(C.r,C.r)
      E.r <- cbind(rnorm(n,sd=sE),rnorm(n,sd=sE))
      
      
      if(variance){
        y.r <- mu + A.r + C.r + E.r
      }else{
        y.r <- mu + ace[1]*A.r + ace[2]*C.r + ace[3]*E.r
      }
      
      r_ <- rep(r[i],n)
      
      data.r<-data.frame(A.r,C.r,E.r,y.r,r_)
      names(data.r)<-c("A1","A2","C1","C2","E1","E2","y1","y2","r")
      datalist[[i]] <- data.r
      names(datalist)[i]<-paste0("datar",r[i])
    }
    merged.data.frame = Reduce(function(...) merge(..., all=T), datalist)
    merged.data.frame$id<-id
  }else{
    id=1:length(r_vector)
    data_vector=data.frame(id,r_vector)
    data_vector$A.r1<-as.numeric(NA)
    data_vector$A.r2<-as.numeric(NA)
    unique_r= matrix(unique(r_vector))
    for(i in 1:length(unique_r)){
      n=length(r_vector[r_vector==unique_r[i]])
      A.rz <- sA*rmvn(n,sigma=diag(2)+S2*unique_r[i])
      data_vector$A.r1[data_vector$r_vector==unique_r[i]] <- A.rz[,1]
      data_vector$A.r2[data_vector$r_vector==unique_r[i]] <- A.rz[,2]
    }
    n=length(r_vector)
    A.r<-matrix(c(data_vector$A.r1,data_vector$A.r2),ncol=2)
    C.r <- rnorm(n,sd=sC);	C.r <- cbind(C.r,C.r)
    E.r <- cbind(rnorm(n,sd=sE),rnorm(n,sd=sE))
    if(variance){
      y.r <- mu + A.r + C.r + E.r
    }else{
      y.r <- mu + ace[1]*A.r + ace[2]*C.r + ace[3]*E.r
    }
    data.r<-data.frame(id,A.r,C.r,E.r,y.r,r_vector)
    names(data.r)<-c("id","A1","A2","C1","C2","E1","E2","y1","y2","r")
    datalist[[i]] <- data.r
    names(datalist)[i]<-paste0("datar",r[i])
    
    merged.data.frame = data.r
  }
  
  return(merged.data.frame)
}	




kinsim_multi <- function(
 r_all=c(1,.5),		# levels of relatedness, default is MZ and DZ twins
  npg_all=500, #number of pairs per group -- use if want the same n in all groups
  npergroup_all=rep(npg_all,length(r_all)), # number of pairs in each group
  mu_all=0,			#intercept
  variables=2,
  mu_list=rep(mu_all,variables),
 
  r_vector=NULL, # alternative specification, give vector of rs
  ace_all=c(1/3,1/3,1/3), # variance default
  ace_list=matrix(rep(ace_all,variables),byrow=TRUE,nrow=variables),
  cov_a=1, #default shared variance for genetics
  cov_c=1, #default shared variance for c
  cov_e=1, #default shared variance for e
  variance=FALSE, #if ACE is given as raw variance, alternative is given as proportions
  model="Correlated",#"Cholesky", #modeling type
  ...){
if(variance){
  sA <- ace_list[,1]^0.5; sC <- ace_list[,2]^0.5; sE <- ace_list[,3]^0.5
}else{
  sA <- ace_list[,1]*0+1; sC <- ace_list[,2]*0+1; sE <- ace_list[,3]*0+1
  }
  S2 <- diag(4)*-1+1
  
  datalist <- list()
  if(variables==1){
    data_v<-kinsim1(r=r_all,
                    npergroup=npergroup_all,	#
                    mu=mu_list[1],			#intercept
                    ace= ace_list[[1]],r_vector=r_vector,variance=variance
    )
    data_v$A1_u<-data_v$A1
    data_v$A2_u<-data_v$A2
    data_v$C1_u<-data_v$C1
    data_v$C2_u<-data_v$C2
    data_v$E1_u<-data_v$E1
    data_v$E2_u<-data_v$E2
    data_v$y1_u<-data_v$y1
    data_v$y2_u<-data_v$y2
    
  merged.data.frame =data_v
  names(merged.data.frame)[c(1,10)]<-c("id","r")
  }
  if(variables>2){  
    stop("You have tried to generate data beyond the current limitations of this program. Maximum variables 2.")
  }
 if(model=="Correlated"|model=="correlated"){

  if(is.null(r_vector)){
    id=1:sum(npergroup_all)
    for(i in 1:length(r_all)){
      n = npergroup_all[i]

     # Genetic Covariance 
      sigma_a<-diag(4)+S2*r_all[i]
      sigma_a[1,3]<-cov_a;
      sigma_a[3,1]<-cov_a;sigma_a[2,4]<-cov_a;sigma_a[4,2]<-cov_a
      sigma_a[1,4]<-cov_a*r_all[i];sigma_a[4,1]<-cov_a*r_all[i];sigma_a[3,2]<-cov_a*r_all[i];sigma_a[2,3]<-cov_a*r_all[i]
      A.r <- rmvn(n,sigma=sigma_a)
      
      A.r[,1:2]<- A.r[,1:2]*sA[1]; A.r[,3:4]<- A.r[,3:4]*sA[2]
      
      # Shared C Covariance 
      sigma_c<-diag(4)+S2*1
      sigma_c[1,3]<-cov_c;sigma_c[3,1]<-cov_c;sigma_c[2,4]<-cov_c;sigma_c[4,2]<-cov_c
      sigma_c[1,4]<-cov_c*1;sigma_c[4,1]<-cov_c*1;sigma_c[3,2]<-cov_c*1;sigma_c[2,3]<-cov_c*1
      C.r <- rmvn(n,sigma=sigma_c)
      C.r[,1:2]<- C.r[,1:2]*sC[1]; C.r[,3:4]<- C.r[,3:4]*sC[2]

      # Shared E Covariance 
      sigma_e<-diag(4)+S2*0
      sigma_e[1,3]<-cov_e;sigma_e[3,1]<-cov_e;sigma_e[2,4]<-cov_e;sigma_e[4,2]<-cov_e
      E.r <- rmvn(n,sigma=sigma_e)
      E.r[,1:2]<- E.r[,1:2]*sE[1]; E.r[,3:4]<- E.r[,3:4]*sE[2]
      if(variance){
        y.r <-  A.r + C.r + E.r
      }else{
        y.r <- A.r
        y.r[,1:2]<-A.r[,1:2]*ace_list[1,1] + C.r[,1:2]*ace_list[1,2] + E.r[,1:2]*ace_list[1,3]
        y.r[,3:4]<-A.r[,3:4]*ace_list[2,1] + C.r[,3:4]*ace_list[2,2] + E.r[,3:4]*ace_list[2,3]
      }
      
      y.r[,1:2]<-y.r[,1:2]+mu_list[1]
      y.r[,3:4]<-y.r[,3:4]+mu_list[2]
      r_ <- rep(r_all[i],n)
      
      data.r<-data.frame(A.r,C.r,E.r,y.r,r_)
      names(data.r)<-c("A1_1","A1_2","A2_1","A2_2","C1_1","C1_2","C2_1","C2_2","E1_1","E1_2","E2_1","E2_2","y1_1","y1_2","y2_1","y2_2","r")
      datalist[[i]] <- data.r
      names(datalist)[i]<-paste0("datar",r_all[i])
    }
    merged.data.frame = Reduce(function(...) merge(..., all=T), datalist)
    merged.data.frame$id<-id
  }else{
    id=1:length(r_vector)
    data_vector=data.frame(id,r_vector,matrix(rep(as.numeric(NA),length(id)*4),nrow=length(id),ncol=4))
	names(data_vector)<-c("id","r","A1_1","A1_2","A2_1","A2_2")
    unique_r= matrix(unique(r_vector))
    for(i in 1:length(unique_r)){
      n=length(r_vector[r_vector==unique_r[i]])
	  
	  # Genetic Covariance 
      sigma_a<-diag(4)+S2*unique_r[i]
      sigma_a[1,3]<-cov_a;
      sigma_a[3,1]<-cov_a;sigma_a[2,4]<-cov_a;sigma_a[4,2]<-cov_a
      sigma_a[1,4]<-cov_a*unique_r[i];sigma_a[4,1]<-cov_a*unique_r[i];sigma_a[3,2]<-cov_a*unique_r[i];sigma_a[2,3]<-cov_a*unique_r[i]
      A.r <- rmvn(n,sigma=sigma_a)
      data_vector$A1_1[data_vector$r_vector==unique_r[i]] <- A.r[,1]*sA[1]
      data_vector$A1_2[data_vector$r_vector==unique_r[i]] <- A.r[,2]*sA[1]
	  data_vector$A2_1[data_vector$r_vector==unique_r[i]] <- A.r[,3]*sA[2]
      data_vector$A2_2[data_vector$r_vector==unique_r[i]] <- A.r[,4]*sA[2]
      A.r[,1:2]<- A.r[,1:2]; A.r[,3:4]<- A.r[,3:4]*sA[2]
    }
    n=length(r_vector)
    A.r<-matrix(c(data_vector$A1_1,data_vector$A1_2,data_vector$A2_1,data_vector$A2_2),ncol=4,nrow=n)
# Shared C Covariance 
      sigma_c<-diag(4)+S2*1
      sigma_c[1,3]<-cov_c;sigma_c[3,1]<-cov_c;sigma_c[2,4]<-cov_c;sigma_c[4,2]<-cov_c
      sigma_c[1,4]<-cov_c*1;sigma_c[4,1]<-cov_c*1;sigma_c[3,2]<-cov_c*1;sigma_c[2,3]<-cov_c*1
      C.r <- rmvn(n,sigma=sigma_c)
      C.r[,1:2]<- C.r[,1:2]*sC[1]; C.r[,3:4]<- C.r[,3:4]*sC[2]

      # Shared E Covariance 
      sigma_e<-diag(4)+S2*0
      sigma_e[1,3]<-cov_e;sigma_e[3,1]<-cov_e;sigma_e[2,4]<-cov_e;sigma_e[4,2]<-cov_e
      E.r <- rmvn(n,sigma=sigma_e)
      E.r[,1:2]<- E.r[,1:2]*sE[1]; E.r[,3:4]<- E.r[,3:4]*sE[2]

      if(variance){
        y.r <-  A.r + C.r + E.r
      }else{
        y.r <- A.r
        y.r[,1:2]<-A.r[,1:2]*ace_list[1,1] + C.r[,1:2]*ace_list[1,2] + E.r[,1:2]*ace_list[1,3]
        y.r[,3:4]<-A.r[,3:4]*ace_list[2,1] + C.r[,3:4]*ace_list[2,2] + E.r[,3:4]*ace_list[2,3]
      }
      y.r[,1:2]<-y.r[,1:2]+mu_list[1]
      y.r[,3:4]<-y.r[,3:4]+mu_list[2]
    y.r <- mu + A.r + C.r + E.r
    data.r<-data.frame(A.r,C.r,E.r,y.r,r_vector,id)
    names(data.r)<-c("A1_1","A1_2","A2_1","A2_2","C1_1","C1_2","C2_1","C2_2","E1_1","E1_2","E2_1","E2_2","y1_1","y1_2","y2_1","y2_2","r","id")
    datalist[[i]] <- data.r
    names(datalist)[i]<-paste0("datar",r[i])
    merged.data.frame = data.r
}
  
 }else if(model=="Cholesky"){
   for(i in 1:variables){
   if(i==1){
     data_v<-kinsim1(r=r_all,
                     npergroup=npergroup_all,	#
                     mu=mu_list[i],			#intercept
                     ace= ace_list[[i]],r_vector=r_vector,variance=variance
     )
     data_v$A1_u<-data_v$A1
     data_v$A2_u<-data_v$A2
     data_v$C1_u<-data_v$C1
     data_v$C2_u<-data_v$C2
     data_v$E1_u<-data_v$E1
     data_v$E2_u<-data_v$E2
     data_v$y1_u<-data_v$y1
     data_v$y2_u<-data_v$y2
   }else{
     data_v<-kinsim1(r=r_all,
                     mu=mu_list[i],			#intercept
                     ace= ace_list[[i]],r_vector=datalist$v1$r,variance=variance)
     data_v$id<-NULL
     data_v$r<-NULL
     # Parse Genetic into Unique and total
     data_v$A1_u<-data_v$A1
     data_v$A2_u<-data_v$A2
     data_v$C1_u<-data_v$C1
     data_v$C2_u<-data_v$C2
     data_v$E1_u<-data_v$E1
     data_v$E2_u<-data_v$E2
     data_v$y1_u<-data_v$y1
     data_v$y2_u<-data_v$y2
     
     if(variance){
       stop("You have tried to generate data beyond the current limitations of this program. Variance method for Cholesky not yet coded.")
     }else{
       data_v$A1<- cov_a_list[i]*datalist$v1$A1_u+(1-cov_a_list[i])*data_v$A1_u
       data_v$A2<-cov_a_list[i]*datalist$v1$A2_u+(1-cov_a_list[i])*data_v$A2_u
       
       
       #Parse C into unique and total
       
       data_v$C1<-cov_c_list[i]*datalist$v1$C1_u+(1-cov_c_list[i])*data_v$C1_u
       data_v$C2<-cov_c_list[i]*datalist$v1$C2_u+(1-cov_c_list[i])*data_v$C2_u
       
       #Parse E into unique and total
       
       data_v$E1<-cov_e_list[i]*datalist$v1$E1_u+(1-cov_e_list[i])*data_v$E1_u
       data_v$E2<-cov_e_list[i]*datalist$v1$E2_u+(1-cov_e_list[i])*data_v$E2_u
     }
   } 
     data_v$y1<-data_v$A1+data_v$C1+data_v$E1+mu_list[i]
     data_v$y2<-data_v$A2+data_v$C2+data_v$E2+mu_list[i]
     
   
  datalist[[i]] <- data_v
  names(datalist)[i]<-paste0("v",i)
}

merged.data.frame =as.data.frame(datalist)
names(merged.data.frame)[c(1,10)]<-c("id","r")
 }else{
   stop(paste0("You have tried to generate data beyond the current limitations of this program. Model specification ",model," not recognized."))
 }
  
  return(merged.data.frame)
}	



