#--------------------------------------------------------------------
#
#   Simulation of Markov Chain
#
#--------------------------------------------------------------------


#--------------------------------------------------------------------
# Simulation Function
#--------------------------------------------------------------------

#Input current status'a(I[k])', Transfer matrix 'P', State space 'I', Number of state 'n', 
#Output 'I[k+1]'
markov<-function(a,P,I,n){
  PP<-0                                    		
  r<-runif(1, min = 0, max = 1)  		 
  #generate random variable U[0,1]
  for(i in 1:n){if(a==I[i])k<-i}      
  i<-k                                       		 
  #find out 'I[k]' correspond to 'a'
  for(k in 1:n){
    PP<-PP+P[i,k]
    if(r<PP){if(r>PP-P[i,k])m<-k} 	 
    #the transfer probability corresopnding to 'r'
  }
  I[m]                                     		 
  #output I[k+1]
}


#Generate a Markov interest chain with 
#length 'k', transfer matrix 'P', state space 'I', no. of states 'n' 
markovchain<-function(k,P,I,n){			
  r<-runif(k, min = 0, max = 1)  		 
  #generate k random variable U[0,1]
  markovchain<-c(1:k)
  m<-1				
  markovchain[1]<-I[1]
  for(i in 1:k){			
    #for every item in the Markov chain
    PP<-0                                    		 
    for(t in 1:n){			
      #transfer from I[m] to another state
      PP<-PP+P[m,t]		
      if(r[i]<PP){if(r[i]>PP-P[m,t])temp<-t} 	 
      #the transfer probability corresponding to r
    }
    markovchain[i]<-I[temp]
    m<-temp				
  }
  markovchain                                    		
}



#--------------------------------------------------------------------
#
#   Simulation of Single Asset Surplus Changes
#
#--------------------------------------------------------------------


#--------------------------------------------------------------------
# Simulation Function
#--------------------------------------------------------------------


#A Function for single simulation with
#Initial surplus 'u', state space'I', transfer matrix 'P', no. of states 'n', length of time 'T'

singlesimulation<-function(u,p,I,P,n,T){
  U<-c(1:T)
  U[1]<-u			
  #The initial surplus
  interest<-markovchain(T,P,I,n) 
  #Simulate the distribution of interest
  Y<-rgamma(T,1/2,1/2)	
  #The claim amount follows Gamma(1/2,1/2)
  Z<-Y-p
  for(t in 2:T){		
    U[t]<-U[t-1]*(1+interest[t])-Z[t]	
    #Current asset status
  }
  time<-c(1:T)
  plot(time,U,type="l")			
  #Plot the curve of change
  lines(time,rep(0,T),col="red")		
  #Mark the baseline U=0
}	


#--------------------------------------------------------------------
# Application
#--------------------------------------------------------------------


#The example 1A 
#Simulation of Single Asset Surplus Changes for model 1

#Initial surplus 'u', Premium 'p', Interest rate 'interest', length of time 'T'
consinglesimulation<-function(u,p,interest,T){
  U<-c(1:T)
  U[1]<-u			
  Y<-rgamma(T,1/2,1/2)	
  #The claim amount follows Gamma(1/2,1/2)
  Z<-Y-p
  for(t in 2:T){		
    U[t]<-U[t-1]*(1+interest)-Z[t]	
    #Current asset status
  }
  time<-c(1:T)
  plot(time,U,type="l")		
  #Plot the curve of change
  lines(time,rep(0,T),col="red")		
  #Mark the baseline U=0
}


#The example 2 
#Simulation of Single Asset Surplus Changes for model 2

#For a single simulation, with initial surplus 'u', the investment amount 'alpha',
#state space 'I', tranfer matrix 'P', no. of states 'n', length of time 'T'
#the parameters for the stock market 'mu' and 'sigma'

singlesimulation1<-function(u,alpha,I,P,n,T,mu,sigma){
  U<-c(1:T)
  U[1]<-u			
  interest<-markovchain(T,P,I,n) 
  #Simulate the distribution of interest
  Y<-rexp(T,1/8)	
  #The claim amount follows exp(1/8)
  Tn<-rexp(T,1/9)	
  B<-rnorm(T,0,1)
  W<-c(1:T)
  Z<-W
  for(t in 2:T ){	
    Z[t]<-Y[t]-(0.9*Tn[t])		
    #The net loss
    W[t]<-exp(mu+(sigma*B[t]))
    U[t]<-U[t-1]*(1+interest[t])+(alpha*W[t]) - Z[t]	
    #Current asset status
  }
  time<-c(1:T)
  plot(time,U,type="o")		
  #Plot the curve of change
  lines(time,rep(0,T),col="red")		
  #Mark the baseline U=0
  U
}



#--------------------------------------------------------------------
#
#   Simulation of ruin probability
#
#--------------------------------------------------------------------


#Simulate the ruin probability
#Replace the ruin probability with the ruin frequency of 1000 assets, with length of time T units


#--------------------------------------------------------------------
# 4.5.1 The numeric example 1 
#--------------------------------------------------------------------

#Initial surplus 'u', Premium 'p', Interest rate 'interest', length of time 'T'
simulation<-function(u,p,I,P,n,T){
  N<-0				
  #N is the number of simulation
  S<-0				
  #S is the number of bankruptcies (ruin)
  for(N in 1:1000){			
    #Start a single simulation
    U<-c(1:T)
    U[1]<-u			
    #The inintial surplus u
    interest<-markovchain(T,P,I,n) 
    #Simulate the interest
    R<-0			
    #Initialize the single bankruptcy token R
    Y<-rgamma(T,1/2,1/2)	
    #The claim amount follows Gamma(1/2,1/2)
    Z<-Y-p
    for(t in 2:T){		
      U[t]<-U[t-1]*(1+interest[t])-Z[t]	
      #Current asset status
      if(U[t]<0){		
        R<-1	
        break()	
      }
    }
    S<-S+R			
    #Record bankruptcy
  }				
  RP<-S/N				
  #Calculate the ruin freq.(prob.)
  RP				
}


#--------------------------------------------------------------------
# Change of parameters and ploting
#--------------------------------------------------------------------


#Calculate the ruin probability at different levels of a parameter 
#For example we change the initial surplus u here
#And repeat the test 100 times
L1<-c(1:100)
for(i in 1:100)L1[i]<-simulation(0,1.1,I,P,3,1000)
L2<-c(1:100)
for(i in 1:100)L2[i]<-simulation(3,1.1,I,P,3,1000)
L3<-c(1:100)
for(i in 1:100)L3[i]<-simulation(5,1.1,I,P,3,1000)
L4-c(1:100)
for(i in 1:100)L4[i]<-simulation(7,1.1,I,P,3,1000)
L5<-c(1:100)
for(i in 1:100)L5[i]<-simulation(10,1.1,I,P,3,1000)
#Plot the curve of the probability of bankruptcy within each group
plot(TIME,L1,lty = 1,col = "red",ylim=c(0,1),type="l")
lines(TIME,L2,lty = 1,col = "blue",ylim=c(0,1))
lines(TIME,L3,lty = 1,col = "darkorange",ylim=c(0,1))
lines(TIME,L4,lty = 1,col = "green",ylim=c(0,1))
lines(TIME,L5,lty = 1,col = "brown",ylim=c(0,1))
#Plot the changes in the average (red line) and maximum (blue line) ruin probabilities 
#as a function of the parameter level
meanL<-c(mean(L1),mean(L2),mean(L3),mean(L4),mean(L5))
uL<-c(0,3,5,7,10)
plot(uL,meanL, type="o",ylim=c(0,1))
spmeanL<-spline(uL,meanL, n=1000)
lines(spmeanL,col="red")
maxL<-c(max(L1),max(L2),max(L3),max(L4),max(L5))
plot(uL,maxL, type="o",ylim=c(0,1))
lines(spmeanmaxL,col="blue")
lines(spmeanL,col="red",lty=2)



#--------------------------------------------------------------------
# 4.5.1.4 The ruin time and deficits
#--------------------------------------------------------------------

#Single Ruin Process Plot as well as Ruin Times and Deficit
#Initial surplus 'u', Premium 'p', Interest rate 'interest', length of time 'T'
singlesimulationtime<-function(u,p,I,P,n,T){
  U<-rep(0,T)
  U[1]<-u			
  interest<-markovchain(T,P,I,n) 
  Y<-rgamma(T,1/2,1/2)	
  Z<-Y-p
  for(t in 2:T){		
    U[t]<-U[t-1]*(1+interest[t])-Z[t]	
    if(U[t]<0) break()		
    #If ruined, break
  }
  time<-c(1:T)
  plot(time,U,type="l")		
  lines(time,rep(0,T),col="red")
  OUT<-list(time=t,deficit=U[t])		
  #Output the ruin time 'time', and the deficit 'deficit'
  return(OUT)
}


#The average ruin time and deficit in 100 simulations
simulationtime<-function(u,p,I,P,n,T){
  time<-rep(0,100)
  deficit<-time
  count<-0
  for(i in 1:100){
    U<-rep(0,T)
    U[1]<-u			
    interest<-markovchain(T,P,I,n) 
    Y<-rgamma(T,1/2,1/2)	
    Z<-Y-p
    for(t in 2:T){		
      U[t]<-U[t-1]*(1+interest[t])-Z[t]	
      if(U[t]<0) break()		
    }
    if(t<T){
      time[i]<-t		
      deficit[i]<-U[t]		
      count<-count+1		
    }
  }
  OUT<-list(time=sum(time)/count,deficit=sum(deficit)/count)
  return(OUT)		
  #Output the average ruin time and the deficit
}



#--------------------------------------------------------------------
# 4.5.2 The numeric example 2
#--------------------------------------------------------------------

#Initial surplus 'u', the investment amount 'alpha',
#state space 'I', tranfer matrix 'P', no. of states 'n', length of time 'T'
#the parameters for the stock market 'mu' and 'sigma'
simulation1<-function(u,alpha,c,I,P,n,T,mu,sigma){
  N<-0				
  #N is the number of simulation
  S<-0				
  #S is the number of ruin
  for(N in 1:1000){			
    #Start a single simulation
    U<-c(1:T)
    U[1]<-u		
    interest<-markovchain(T,P,I,n) 
    R<-0			
    #Initialize the single bankruptcy token R
    Y<-rexp(T,1/8)	
    #The claim amount follows exp(1/8)
    Tn<-rexp(T,1/9)	
    Z<-Y-c*Tn		
    #The net los
    B<-rnorm(T,0,1)
    W<-exp(mu+sigma*B)
    for(t in 2:T){		
      U[t]<-U[t-1]*(1+interest[t])+alpha*W[t]-Z[t]	
      #Current asset status
      if(U[t]<0){		
        R<-1	
        break()	
      }
    }
    S<-S+R			
  }			
  RP<-S/N				
  #Calculate the ruin freq.(prob.)
  RP				
}


#--------------------------------------------------------------------
# 4.5.2.4 Comparison with numeric example 1
#--------------------------------------------------------------------


#Simulated Ruin Probability After Adjusting Example 2 Parameters According To Example 1

#Initial surplus 'u', the investment amount 'alpha',
#state space 'I', tranfer matrix 'P', no. of states 'n', length of time 'T'
#the parameters for the stock market 'mu' and 'sigma'
consimulation<-function(u,alpha,c,I,P,n,T,mu,sigma){
  N<-0				
  #N is the number of simulation
  S<-0			
  #S is the number of ruin
  for(N in 1:1000){		
    U<-c(1:T)
    U[1]<-u			
    interest<-markovchain(T,P,I,n) 
    R<-0			
    #Initialize the single bankruptcy token R
    Y<-rgamma(T,1/2,1/2)	
    #The claim amount follows Gamma(1/2,1/2)
    Z<-Y-1.1
    B<-rnorm(T,0,1)
    W<-exp(mu+sigma*B)
    for(t in 2:T){		
      U[t]<-U[t-1]*(1+interest[t])+alpha*W[t]-Z[t]	
      #Current asset status
      if(U[t]<0){	
        #If ruined
        R<-1	
        break()	
      }
    }
    S<-S+R			
  }				
  RP<-S/N				
  #Calculate the ruin freq.(prob.)
  RP				
}
