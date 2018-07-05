##############################################################################
# Application

# Package for k-NN

library(kknn)

# Selected data set

data=dataNet # Change it inside loop too

# Folds for the selected data set

folds=foldsNet

# Complete input attribute sets

finputs=c("Depth_m","qt_MPa","Rf_p","u_kPa")

# List with inputs combination sets

inpcomb=list()

# Initializing matrix with "k" values obtained for each testing case and its accucary associated

kval=matrix(0,10,2)
colnames(kval)=c("k","accuracy")

rn=c()

for(i in 1:10){
  rn=rbind(rn,as.character(i))
}

rownames(kval)=rn

# Initializing list of inputs

loinp=list()

# Initializing list with kval matrices

lokval=list()

# Initializing list with kval mean

lokvalm=list()

# Initializing list with kval standard deviation

lokvalsd=list()

# Initializing list with kval "k" values

lokvalk=list()

# Initializing list with kval mean for the best "k"

lokvalkm=list()

# Initializing list with kval standard deviation for the best "k"

lokvalksd=list()

# Applying

# For each ML technique

for(mode in c("rectangular","gaussian")){
  
  # For each classification method
  
  for(out in c("SBTn","MSBTn")){
    # For each number of input attributes
    
    for(nrows in 4:length(finputs)){
      
      # Generating combinations of input attributes
      
      inputs=combn(finputs,nrows)
      
      # Removing some combinations
      
      idxr=c()
      
      for(verif1 in 1:ncol(inputs)){
        ok=0
        for(verif2 in 1:nrow(inputs)){
          if(inputs[verif2,verif1]=="Rf_p"){
            ok=1
          }
        }
        if(ok == 0){
          idxr=append(idxr,verif1)
        }
      }
      
      if(length(idxr)!=0){
        inputs=inputs[,-idxr]
      }
      
      # Keeping it in the list
      
      inpcomb[[length(inpcomb)+1]]=inputs
      
      # For each combination
      
      for(inpCase in 1:ncol(inputs)){
        
        # Selecting input attributes
        
        inp=inputs[,inpCase]
        loinp[[length(loinp)+1]]=inp
        
        # Atualizing data set
        
        data=dataNet
        
        # Eliminating other attributes from data
        
        datancol=ncol(data)
        idxr=c()
        
        for(i in 1:datancol){
          at=colnames(data)[i]
          if(length(inp[inp==at])==0 && at!=out){
            idxr=append(idxr,i)
          }
        }
        
        data=data[,-idxr]
        
        # Defining formula
        
        form=paste(inp,collapse="+")
        form=as.formula(paste(append(out,form),collapse="~"))
        
        #10-fold cross-validation technique
        
        for(i in 1:10){
          
          # Testing case
          
          cat("Testing case:",i,"\n\n")
          
          # Test data set as matrix
          
          mtest=as.matrix(data[folds[[i]],])
          
          # Validation data set as matrix
          
          mvalid=as.matrix(data[folds[[validFolds[i]]],])
          
          # Training data set as matrix
          
          mtrain=as.matrix(data[-append(folds[[i]],folds[[validFolds[i]]]),])
          
          # Removing Robertson's (1991) class 0
          
          if(out=="SBTn"){
            mtest=mtest[mtest[,out]!=0,]
            mvalid=mvalid[mvalid[,out]!=0,]
            mtrain=mtrain[mtrain[,out]!=0,]
          }
          
          # Normalization
          
          minVal=c()
          
          for(j in 1:length(inp)){
            minVal=cbind(minVal,min(mtrain[,inp[j]]))
          }
          
          # Data frame with minimum values
          
          minVal=as.matrix(minVal)
          colnames(minVal)=inp
          
          maxVal=c()
          
          for(j in 1:length(inp)){
            maxVal=cbind(maxVal,max(mtrain[,inp[j]]))
          }
          
          # Data frame with maximum values
          
          maxVal=as.matrix(maxVal)
          colnames(maxVal)=inp
          
          # Normalizing
          
          for(j in 1:length(inp)){
            
            mini=minVal[j]
            maxi=maxVal[j]
            
            cat("Normalizing",inp[j],"\n")
            
            for(obj in 1:nrow(mtrain)){
              mtrain[obj,inp[j]]=(mtrain[obj,inp[j]]-mini)/(maxi-mini)
            }
            for(obj in 1:nrow(mvalid)){
              mvalid[obj,inp[j]]=(mvalid[obj,inp[j]]-mini)/(maxi-mini)
            }
            for(obj in 1:nrow(mtest)){
              mtest[obj,inp[j]]=(mtest[obj,inp[j]]-mini)/(maxi-mini)
            }
          }
          
          # Statistical marks for pre-filtering
          
          q1=c()
          q3=c()
          
          for(class in sort(unique(mtrain[,out]))){
            auxq1=c()
            auxq3=c()
            for(at in inp[inp!="Depth_m"]){
              
              # First quartile
              
              auxq1=append(auxq1,quantile(mtrain[mtrain[,out]==class,at])[2])
              
              # Third quartile
              
              auxq3=append(auxq3,quantile(mtrain[mtrain[,out]==class,at])[4])
              
            }
            q1=cbind(q1,auxq1)
            q3=cbind(q3,auxq3)
          }
          
          
          # Interquartil interval
          
          iq=q3-q1
          
          # Formating
          
          q1=t(as.matrix(q1))
          colnames(q1)=inp[inp!="Depth_m"]
          rownames(q1)=sort(unique(mtrain[,out]))
          
          q3=t(as.matrix(q3))
          colnames(q3)=inp[inp!="Depth_m"]
          rownames(q3)=sort(unique(mtrain[,out]))
          
          iq=t(as.matrix(iq))
          colnames(iq)=inp[inp!="Depth_m"]
          rownames(iq)=sort(unique(mtrain[,out]))
          
          # Data cleaning
          
          removed=0
          idx=c()
          
          # Pre-filtering
          
          cat("\nPre-filtering\n")
          
          for(obj in 1:nrow(mtrain)){
            class=as.character(mtrain[obj,out])
            for(at in inp[inp!="Depth_m"]){
              if(mtrain[obj,at]>q3[class,at]+1.5*iq[class,at]
                 || mtrain[obj,at]<q1[class,at]-1.5*iq[class,at]){
                if(length(idx)==0 || obj!=idx[length(idx)]){
                  idx=append(idx,obj)
                  #cat("Pre-filtered:",length(idx),"\n")
                  #cat("Object:",obj,"\n\n")
                }
              }
            }
          }
          
          # Data frame of test data set
          
          test=as.data.frame(mtest)
          
          # Data frame of validation data set
          
          valid=as.data.frame(mvalid)
          
          # Data frame of training data set
          
          train=as.data.frame(mtrain)
          
          # Converting outputs of the data frames into factors
          
          test[,out]=as.factor(test[,out])
          valid[,out]=as.factor(valid[,out])
          train[,out]=as.factor(train[,out])
          
          # Filtering
          
          cat("\nFiltering\n")
          
          filt=0
          
          for(obj in 1:length(idx)){
            fitTrain <- kknn(formula=form,
                             train=train[-idx[obj],],
                             test=train[idx[obj],],
                             k=1,
                             distance=2,
                             kernel=mode,
                             scale=FALSE)
            
            pred=fitTrain$fitted.values
            
            filt=filt+1
            
            # Remove object if it was misclassified
            
            if(pred!=train[idx[obj],out]){
              train=train[-idx[obj],]
              idx=idx-1
              removed=removed+1
              #cat("Filtered:",removed,"\n\n")
              #cat("Pre-filtered object:",filt,"\n")
            }
          }
          
          # Training data set as matrix
          
          train[,out]=as.numeric(as.character(train[,out]))
          mtrain=as.matrix(train)
          
          # Initializing sets for balanced training set
          
          mtrainb=mtrain
          mtestb=mtest
          mvalidb=mvalid
          
          # Proportion check
          
          cat("\nProportion check\n\n")
          
          prop=c()
          
          for(j in sort(unique(mtrainb[,out]))){
            prop=append(prop,nrow(mtrainb[mtrainb[,out]==j,]))
            cat("Class", j, ":", nrow(mtrainb[mtrainb[,out]==j,]),"\n")
          }
          
          cat("\nBalancing\n\n")
          
          # Number of elements in the minority class
          
          minProp=min(prop)
          
          # Total number of elements by class
          
          nTot=max(1000,2*minProp)
          
          # Balancing
          
          for(j in sort(unique(mtrainb[,out]))){
            mtrainClass=mtrainb[mtrainb[,out]==j,]
            numEl=nrow(mtrainClass)
            if(numEl>nTot){
              set.seed(1)
              objIdx=sample(nrow(mtrainClass),numEl-nTot)
              mtrainb=rbind(mtrainb[mtrainb[,out]!=j,],mtrainClass[-objIdx,])
            }
            else if(numEl<nTot){
              for(p in 1:(nTot-numEl)){
                set.seed(1)
                objIdx=sample(nrow(mtrainClass),length(inp)+1)
                obj=c()
                for(at in colnames(mtrainb)){
                  if(at==out){
                    obj=append(obj,j)
                  }else{
                    obj=append(obj,mean(mtrainClass[objIdx,at]))
                  }
                }
                mtrainb=rbind(mtrainb,obj)
              }
            }
            cat("Class", j, ":", nrow(mtrainb[mtrainb[,out]==j,]),"\n")
          }
          
          # Normalization of unbalanced training set
          
          cat("\nUnbalanced training set normalization\n\n")
          
          minVal=c()
          
          for(j in 1:length(inp)){
            minVal=cbind(minVal,min(mtrain[,inp[j]]))
          }
          
          # Data frame with minimum values
          
          minVal=as.matrix(minVal)
          colnames(minVal)=inp
          
          maxVal=c()
          
          for(j in 1:length(inp)){
            maxVal=cbind(maxVal,max(mtrain[,inp[j]]))
          }
          
          # Data frame with maximum values
          
          maxVal=as.matrix(maxVal)
          colnames(maxVal)=inp
          
          # Normalizing
          
          for(j in 1:length(inp)){
            
            mini=minVal[j]
            maxi=maxVal[j]
            
            cat("Normalizing",inp[j],"\n")
            
            for(obj in 1:nrow(mtrain)){
              mtrain[obj,inp[j]]=(mtrain[obj,inp[j]]-mini)/(maxi-mini)
            }
            for(obj in 1:nrow(mvalid)){
              mvalid[obj,inp[j]]=(mvalid[obj,inp[j]]-mini)/(maxi-mini)
            }
            for(obj in 1:nrow(mtest)){
              mtest[obj,inp[j]]=(mtest[obj,inp[j]]-mini)/(maxi-mini)
            }
          }
          
          # Normalization of balanced training set
          
          cat("\nBalanced training set normalization\n\n")
          
          minVal=c()
          
          for(j in 1:length(inp)){
            minVal=cbind(minVal,min(mtrainb[,inp[j]]))
          }
          
          # Data frame with minimum values
          
          minVal=as.matrix(minVal)
          colnames(minVal)=inp
          
          maxVal=c()
          
          for(j in 1:length(inp)){
            maxVal=cbind(maxVal,max(mtrainb[,inp[j]]))
          }
          
          # Data frame with maximum values
          
          maxVal=as.matrix(maxVal)
          colnames(maxVal)=inp
          
          # Normalizing
          
          for(j in 1:length(inp)){
            
            mini=minVal[j]
            maxi=maxVal[j]
            
            cat("Normalizing",inp[j],"\n")
            
            for(obj in 1:nrow(mtrainb)){
              mtrainb[obj,inp[j]]=(mtrainb[obj,inp[j]]-mini)/(maxi-mini)
            }
            for(obj in 1:nrow(mvalidb)){
              mvalidb[obj,inp[j]]=(mvalidb[obj,inp[j]]-mini)/(maxi-mini)
            }
            for(obj in 1:nrow(mtestb)){
              mtestb[obj,inp[j]]=(mtestb[obj,inp[j]]-mini)/(maxi-mini)
            }
          }
          
          # Data frame of test data set
          
          test=as.data.frame(mtest)
          
          # Data frame of validation data set
          
          valid=as.data.frame(mvalid)
          
          # Data frame of unbalanced training data set
          
          train=as.data.frame(mtrain)
          
          # Data frame of the data sets for balanced training set
          
          testb=as.data.frame(mtestb)
          validb=as.data.frame(mvalidb)
          trainb=as.data.frame(mtrainb)
          
          # Converting outputs of the data frames into factors
          
          test[,out]=as.factor(test[,out])
          valid[,out]=as.factor(valid[,out])
          train[,out]=as.factor(train[,out])
          
          trainb[,out]=as.factor(trainb[,out])
          validb[,out]=as.factor(validb[,out])
          testb[,out]=as.factor(testb[,out])
          
          # Pre-testing the balanced set
          
          fitValidb <- kknn(formula=form,
                            train=trainb,
                            test=testb,
                            k=3,
                            distance=2,
                            kernel=mode,
                            scale=FALSE)
          
          # Calculating accuracy (mean recall)
          
          confb=table(fitValidb$fit,testb[,out])
          
          mpb=0
          
          for(j in colnames(confb)){
            if(sum(confb[,j])!=0){
              mpb=mpb+confb[j,j]/sum(confb[,j])
            }
          }
          
          mpb=mpb/ncol(confb)
          
          cat("\nBalanced training set pre-test accuracy:",100*mpb,"%\n\n")
          
          # Pre-testing the unbalanced set
          
          fitValid <- kknn(formula=form,
                           train=train,
                           test=test,
                           k=3,
                           distance=2,
                           kernel=mode,
                           scale=FALSE)
          
          # Calculating accuracy (mean recall)
          
          conf=table(fitValid$fit,test[,out])
          
          mp=0
          
          for(j in colnames(conf)){
            if(sum(conf[,j])!=0){
              mp=mp+conf[j,j]/sum(conf[,j])
            }
          }
          
          mp=mp/ncol(conf)
          
          cat("Unbalanced training set pre-test accuracy:",100*mp,"%\n\n")
          
          if(mpb>mp){
            train=trainb
            test=testb
            valid=validb
            cat("Using balanced set!\n\n")
          }
          else{
            cat("Using unbalanced set!\n\n")
          }
          
          # Boolean indicating if the "while" loop must go on
          
          cont=1
          
          # Initializing a variable to keep the previous mean recall
          
          prevmp=0
          
          # Initializing "k" parameter of k-NN (k=2*n-1)
          
          n=1
          
          while(cont){
            
            cat("Training for k =",2*n-1,"\n")
            
            # Training
            
            fitValid <- kknn(formula=form,
                             train=train,
                             test=valid,
                             k=2*n-1,
                             distance=2,
                             kernel=mode,
                             scale=FALSE)
            
            # Calculating accuracy (mean recall)
            
            conf=table(fitValid$fit,valid[,out])
            
            mp=0
            
            for(j in colnames(conf)){
              if(sum(conf[,j])!=0){
                mp=mp+conf[j,j]/sum(conf[,j])
              }
            }
            
            mp=mp/ncol(conf)
            
            cat("Accuracy:",100*mp,"%\n\n")
            
            if(mp < prevmp){
              cont=0
              n=n-1
            }else{
              n=n+1
              prevmp=mp
            }
          }
          
          cat("Testing for k =",2*n-1,"\n")
          
          # Testing
          
          fitTest <- kknn(formula = form,
                          train = train,
                          test = test,
                          k = 2*n-1,
                          distance=2,
                          kernel=mode,
                          scale=FALSE)
          
          # Calculating accuracy (mean recall)
          
          conf=table(fitTest$fit,test[,out])
          
          mp=0
          
          for(j in colnames(conf)){
            if(sum(conf[,j])!=0){
              mp=mp+conf[j,j]/sum(conf[,j])
            }
          }
          
          mp=mp/ncol(conf)
          
          cat("Accuracy:",100*mp,"%\n\n")
          
          # Keeping "k" value for the ith testing case
          
          kval[i,1]=2*n-1
          
          # Keeping accuracy related to "k"
          
          kval[i,2]=mp*100
        }
        
        # Adding kval to the list lokval
        
        lokval[[length(lokval)+1]]=kval
        
        # Adding mean to lokvalm
        
        lokvalm[[length(lokvalm)+1]]=mean(kval[,2])
        
        # Adding standard deviation to lokvalsd
        
        lokvalsd[[length(lokvalsd)+1]]=sd(kval[,2])
        
        # Best "k"
        
        x=kval[,1]
        z=table(as.vector(x))
        bk=as.numeric(names(z)[z == max(z)])[1]
        lokvalk[[length(lokvalk)+1]]=bk
        
        # Adding mean for the best "k" to the list lokvalkm
        
        lokvalkm[[length(lokvalkm)+1]]=mean(kval[kval[,1]==bk,2])
        
        # Adding standard deviation for the best "k" to the list lokvalksd
        
        lokvalksd[[length(lokvalksd)+1]]=sd(kval[kval[,1]==bk,2])
      }
    }
  }
}
