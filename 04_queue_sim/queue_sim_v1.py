# -*- coding: utf-8 -*-
import math
import datetime
import numpy as np
import simpy
import random
import matplotlib.pyplot as plt
import plotly.plotly as py
from scipy import stats
import csv
import os


## нужно прописать корректные имена входного и выходного файлов, а также
## названия рабочей директории, где они находятся
os.chdir('C:\\Users\\akononyhin\\Canopy\\My Scripts\\')
input_file='LoadProfile_data.csv'
output_file='sim_output.csv'

iter_nums=2
weeks_cnt=5

server_cnt=3
server_capacity=[None]*server_cnt
server_capacity[0]=1       
server_capacity[1]=2         
server_capacity[2]=3           
    
arrival_interval=0.081        #interval in minutes

service_time=[None]*server_cnt
#service_time[0]=0.03       #time in minutes
#service_time_0=0.029468     #time in minutes
service_time[0]=0.029468
service_time[1]=0.05            #time in minutes
service_time[2]=0.05            #time in minutes
#N=137000         #cars number
N=137000*weeks_cnt

service_degrade=[[[] for i in range(server_cnt)] for j in range(iter_nums)]
service_degrade[0][0].append((10080*4,10080*5,0.5))
service_degrade[1][0].append((10080*4,10080*5,1.5))

                
std_1=[None]*iter_nums
mean_1=[None]*iter_nums
std_2=[None]*iter_nums
mean_2=[None]*iter_nums
std_3=[None]*iter_nums
mean_3=[None]*iter_nums
std_all=[None]*iter_nums
mean_all=[None]*iter_nums

avg_util=np.zeros((iter_nums, server_cnt ))
avg_queue=np.zeros((iter_nums, server_cnt ))

steps_number=2016*5
a_aggr=np.zeros((iter_nums, steps_number, 3))
a_aggr_all=np.zeros((steps_number, 3))

in_service_aggr=np.zeros((iter_nums, server_cnt, steps_number, 3))
in_service_aggr_all=np.zeros((server_cnt,steps_number, 3))

arr_rate=np.zeros((10080, 2))
'''
arr_time=np.zeros((10079, 2))
arr_time[:,0]=range(0,1000,1)
for i in range(len(arr_time)):
    #arr_time[i,1]=0*(math.sin(arr_time[i,0]))+arrival_interval-1
    arr_time[i,1]=arrival_interval
'''
#os.chdir('C:\\Users\\akononyhin\\Canopy\\My Scripts\\')
i=0
with open(input_file) as csvfile:
    spamreader = csv.reader(csvfile, delimiter=',', quotechar='|')
    for row in spamreader:
        arr_rate[i,0]=row[0]
        #arr_rate[i,1]=(1/8)*float(row[1])/90
        #arr_rate[i,1]=1.1479*float(row[1])/90/8
        arr_rate[i,1]=270*float(row[1])/90/8
        i+=1
      
     
for w in range(iter_nums):
    print datetime.datetime.now().time()
    print('Simulation number %d starts' % (w))
        
    a=np.zeros((N, 9))
    bcs_a=np.zeros((server_cnt,3*N, 6))
    '''
    bcs_1_a=np.zeros((3*N, 6))
    bcs_2_a=np.zeros((3*N, 6))
    bcs_3_a=np.zeros((3*N, 6))
    '''
    #print a
    
    def car(env, name, bcs, driving_time, service_time):
        #print 'hello hello'
        # Simulate driving to the BCS            
        yield env.timeout(driving_time)
        for k in range(server_cnt):

            # Request one of its charging spots
            #print('Car %s arriving at bcs_%d: %f' % (name, k, env.now))
            #print bcs[i].count
            #print len(bcs[i].queue)
            a[name,0+k*3]=env.now
            bcs_a[k,name*3,0:5]=(round(env.now,4),bcs[k].count,len(bcs[k].queue),name,0)
            '''
            bcs_1_a[name*3,0]=round(env.now,4)
            bcs_1_a[name*3,1]=bcs_1.count
            bcs_1_a[name*3,2]=len(bcs_1.queue)
            bcs_1_a[name*3,3]=name
            bcs_1_a[name*3,4]=0
            '''     
            with bcs[k].request() as req:
                yield req
        
                # Charge the battery
        #akon         print('Car %s starting to charge at bcs_%d at %f' % (name, k, env.now))
                a[name,1+k*3]=env.now
                bcs_a[k,name*3+1,0:5]=(round(env.now,4),bcs[k].count,len(bcs[k].queue),name,1)
        
                '''
                if (k==0 and env.now>=10080*(weeks_cnt-1)):
                    service_time[0]=service_time_0*1.5
                else:
                    service_time[0]=service_time_0
                '''
                    
                      
                sd1=service_degrade[w][k]    
                m=0
                flag=0
                while (m<len(sd1) and flag==0):
                    if (sd1[m][0]<=env.now and sd1[m][1]>=env.now):
                        flag=1
                    m+=1
                #print 'hi HI 1'
                #print sd1
                #print m
                #print flag
                if (flag==1):
                    #print 'hi HI 2' 
                    service_time_new=service_time[k]*sd1[m-1][2]
                else:
                    #print 'hi HI 3' 
                    service_time_new=service_time[k]
                #print 'hi HI 4'     
                #yield env.timeout(charge_duration)
                yield env.timeout(random.expovariate(1.0/service_time_new))
        #akon         print('Car %s leaving the bcs_%d at %f' % (name, k, env.now))
                a[name,2+k*3]=env.now
                bcs_a[k,name*3+2,0:5]=(round(env.now,4),bcs[k].count,len(bcs[k].queue),name,2)
          
    
    
    env = simpy.Environment()
    bcs=[None]*server_cnt
    bcs[0] = simpy.Resource(env, capacity=server_capacity[0])
    bcs[1] = simpy.Resource(env, capacity=server_capacity[1])
    bcs[2] = simpy.Resource(env, capacity=server_capacity[2])
    #We can now create the car processes and pass a reference to our resource as well as some additional parameters to them:
    
    #flag=0
    t=0
    k_prev=0
    i_last=0
    print 'env.process starts'
    for i in range(N):  #N - cars count
        #env.process(car(env, 'Car %d' % i, bcs, i*2, 5))
        
        #деградация 0-го сервера на 5-ой неделе на 0%, 1%, 4% и 9% 
        '''
        if (t>=10080*(weeks_cnt-1)):
            #service_time[0]=service_time_0*(1.0+0.02*w)
            service_time[0]=service_time_0*1.3
            #flag=1
        else:
            service_time[0]=service_time_0
        print service_time[0]
        '''
        env.process(car(env, i, bcs, t, service_time))
        
        k_next=math.floor(t/arr_rate[len(arr_rate)-1,0])
        #print t/arr_rate[len(arr_rate)-1,0]
        ta=t%arr_rate[len(arr_rate)-1,0]
        #print k_next
        
        '''
        if (i_last>0 and arr_rate[i_last-1,0]>ta):
            i_last=0
        '''    
        
        if (k_next>k_prev):
            i_last=0
            k_prev=k_next
        
        
        while arr_rate[i_last,0]<=ta:
            i_last+=1
        
        #print t,ta, i_last 
        #print arr_rate[i_last,:]
        
        #t=t+random.expovariate(1/arrival_interval)
        arrival_rate_1=arr_rate[i_last-1,1]+(ta-arr_rate[i_last-1,0])/(arr_rate[i_last,0]-arr_rate[i_last-1,0])\
            *(arr_rate[i_last,1]-arr_rate[i_last-1,1])
        #print (ta-arr_time[i_last-1,0])/(arr_time[i_last,0]-arr_time[i_last-1,0])
        #print arrival_interval_1
        #print arrival_rate_1
        t=t+random.expovariate(arrival_rate_1)
    print 'env.process ends'
    print datetime.datetime.now().time()
    env.run()
    
    temp = a.view(np.ndarray)
    a=temp[np.lexsort((temp[:, 0], ))]
    
    #for i in range((len(a))):
    #    for j in range(len(a[i,:])):
    #        print ('%5.2f' % a[i,j])
    
    #print a           
    b1=sum(a[:,1]-a[:,0])
    b2=sum(a[:,4]-a[:,3])
    b3=sum(a[:,7]-a[:,6])
    #print round(b1/N,4)
    #print round(b2/N,4)
    #print round(b3/N,4)
    
    c1=(a[:,2]-a[:,0])
    c2=(a[:,5]-a[:,3])
    c3=(a[:,8]-a[:,6])
    c_all=(a[:,8]-a[:,0])
    #c=(a[:,2]-a[:,1])+(a[:,5]-a[:,4])+(a[:,8]-a[:,7])
    ##gamma_numbers=numpy.random.gamma(1, 2, 10000)
    ##plt.hist(gamma_numbers)
    #plt.hist(c)
    #plt.title("Experimental Histogram")
    #plt.xlabel("Value")
    #plt.ylabel("Frequency")
    
    #fig1 = plt.gcf()
    
    #plot_url = py.plot_mpl(fig1, filename='mpl-basic-histogram')
    
    
    std_1[w]=np.std(c1)
    mean_1[w]=np.mean(c1)
    std_2[w]=np.std(c2)
    mean_2[w]=np.mean(c2)
    std_3[w]=np.std(c3)
    mean_3[w]=np.mean(c3)
    std_all[w]=np.std(c_all)
    mean_all[w]=np.mean(c_all)
    
    '''
    alpha=mean*mean/(std*std)
    beta=mean/alpha
    gamma_numbers=np.random.gamma(alpha,beta, N)
    '''
    
    print ('mean_1=%f' % mean_1[w])
    print ('mean_2=%f' % mean_2[w])
    print ('mean_3=%f' % mean_3[w])
    print ('mean_all=%f' % mean_all[w])
    
    #print ('std=%f' % std)
    #print ('alpha=%f' % alpha)
    #print ('beta=%f' % beta)
    #plt.hist(gamma_numbers)
    #plt.title("Gamma Histogram")
    #plt.xlabel("Value")
    #plt.ylabel("Frequency")
    #fig2 = plt.gcf()
    
    #plot_url = py.plot_mpl(fig2, filename='mpl-basic-histogram')
    
    bins = np.linspace(0, 200, 500)
    
    #inds = np.digitize([0,99,99.5,100,101], bins)
    #print inds
    #print np.histogram(c,bins)
    '''
    plt.hist(c, bins, alpha=0.5, label='simulation')
    plt.hist(gamma_numbers, bins, alpha=0.5, label='Gamma dist')
    plt.legend(loc='upper right')
    plt.show()
    '''
    
    '''
    plt.hist(c1, bins, alpha=0.5, label='1 server',histtype='step')
    plt.hist(c2, bins, alpha=0.5, label='2 server',histtype='step')
    plt.hist(c3, bins, alpha=0.5, label='3 server',histtype='step')
    plt.hist(c_all, bins, alpha=0.5, label='all servers',histtype='step')
    plt.legend(loc='upper right')
    plt.show()
    '''
    #np.random.seed(987654321)
    #print ('test')
    #param1= stats.gamma.fit(c,floc=0)
    #param2= stats.gamma.fit(c)
    #print param1
    #print param2
    #print stats.kstest(c,lambda x : stats.gamma.cdf(x, *param1))
    '''
    print stats.kstest(c,lambda x : stats.gamma.cdf(x, *param2))
    print stats.kstest(c,lambda x : stats.gamma.cdf(x, alpha,0,beta))
    '''
    #print bcs_1_a[:,1:6]
    
    #!!!!!!!!!!!!!!!!!!!!! может быть ошибка !!!!!!!!!!!!
    for k in range (server_cnt):
        temp = bcs_a[k,:,:].view(np.ndarray)
        bcs_a[k,:,:]=temp[np.lexsort((temp[:, 0], ))]

    
    
    for i in range(server_cnt):
        for j in range(len(bcs_a[i])-1):
            bcs_a[i,j+1,5]=bcs_a[i,j+1,0]-bcs_a[i,j,0]

    
    '''
    print len(bcs_1_a)
    print len(bcs_2_a)
    print len(bcs_3_a)
    '''
    
    #max_in_service[i]=int(max(bcs_a[i,:,1]))
    max_in_queue=[None]*server_cnt
    in_queue=[None]*server_cnt
    in_service=[None]*server_cnt
    for i in range(server_cnt):
        max_in_queue[i]=int(max(bcs_a[i,:,2]))
        in_queue[i]=np.zeros((max_in_queue[i]+1, 2))
        in_service[i]=np.zeros((server_capacity[i]+1, 2))
    #in_service_1=np.zeros((max_in_service_1+1, 2))
    
    
    
    
    
    '''
    print max_in_service_1
    print max_in_service_2
    print max_in_service_3
    '''
    #for i in range(server_capacity_1+1):
    #    in_service_1[i,0]=i
    for i in range(server_cnt):
        in_service[i][:,0]=range(server_capacity[i]+1)
        for k in range(len(bcs_a[i,:,:])):
            in_service[i][int(bcs_a[i,k,1]),1]+=bcs_a[i,k,5]

        
        #for i in range(max_in_queue+1):
        #    in_queue[i,0]=i
        
        in_queue[i][:,0]=range(max_in_queue[i]+1)
            
        for k in range(len(bcs_a[i,:,:])):
            in_queue[i][bcs_a[i,k,2],1]+=bcs_a[i,k,5]

    
        #print in_service
        avg_util[w,i]=sum(in_service[i][:,0]*in_service[i][:,1])/(server_capacity[i]*sum(in_service[i][:,1]))
        
        #print in_queue
        avg_queue[w,i]=sum(in_queue[i][:,0]*in_queue[i][:,1])/sum(in_queue[i][:,1])
  
        print avg_util[w,i],avg_queue[w,i]
  

    max_t=int(a[len(a)-1,8])
    step_t=int(math.ceil(max_t/steps_number))
   
    k=0
    #print w
    for j in range(0,steps_number):
        a_aggr[w,j,0]=j*step_t
        l=k
        while ((a[l,0]<=(j+1)*step_t) and l<len(a)-1):
            l+=1
        #a_aggr[w,i,1]=np.mean(a[k:l,8]-a[k:l,0])
        if (l==k):
            a_aggr[w,j,1]=0
        else:
            a_aggr[w,j,1]=sum(a[k:l,8]-a[k:l,0])/(l-k)
        #print a_aggr[w,j,1]
        a_aggr[w,j,2]=l-k
        k=l
     
    #print a_aggr
    print sum(a_aggr[w,:,1]*a_aggr[w,:,2])/sum(a_aggr[w,:,2])
    #plt.plot(a_aggr[w,:,1])
    a_aggr_all=a_aggr_all+a_aggr[w,:,:]/iter_nums  
         
    #max_t=int(bcs_1_a[len(bcs_1_a)-1,0])
    #step_t=int(math.ceil(max_t/steps_number))    

    for i in range(server_cnt):
        k=0       
        for j in range(0,steps_number):
            in_service_aggr[w,i,j,0]=j*step_t
            
            l=k
            #sum=0
            while ((bcs_a[i,l,0]<=(j+1)*step_t) and l<len(bcs_a[i,:,:])-1):
                l+=1
            #print k,l,i
            if (sum(bcs_a[i,k:l,5])>0):
                in_service_aggr[w,i,j,1]=sum(bcs_a[i,k:l,1]*bcs_a[i,k:l,5])/(server_capacity[i]*sum(bcs_a[i,k:l,5]))
            else:
                in_service_aggr[w,i,j,1]=0
            in_service_aggr[w,i,j,2]=l-k
            k=l
        
        in_service_aggr_all[i,:,:]=in_service_aggr_all[i,:,:]+in_service_aggr[w,i,:,:]/iter_nums

    #print sum(in_service_1_aggr[:,2]*in_service_1_aggr[:,1])/sum(in_service_1_aggr[:,2])
    print np.mean(in_service_aggr[w,0,:,1])

    
   

        

        
#plt.ylim(0,np.mean(a_aggr_all[:,1])*2)
'''
for w in range(iter_nums):
    plt.plot(a_aggr[w,:,0], a_aggr[w,:,1],'b-',lw=0.5)
'''

#plt.plot(a_aggr_all[:,0], a_aggr_all[:,1],'k-', lw=3)
#plt.show()
 
''' 
plt.ylim(0,1.2)
plt.xlim(0,10080*5)
plt.axhline(y=1.0, color = '0.75')
plt.plot(in_service_aggr_all[0,0:(7*10078/7),0], (1.0)*in_service_aggr_all[0,0:(7*10078/7),1],'-',color = '#006400')
plt.plot(in_service_aggr_all[1,0:(7*10078/7),0], (1.0)*in_service_aggr_all[1,0:(7*10078/7),1],'-',color = '#32CD32')
plt.plot(in_service_aggr_all[2,0:(7*10078/7),0], (1.0)*in_service_aggr_all[2,0:(7*10078/7),1],'-',color = '#00FF00')

plt.plot(a_aggr_all[0:(7*10078/7),0], (1.0/2.0)*a_aggr_all[0:(7*10078/7),1])
plt.plot((1.0/70.0)*arr_rate[0:(7*10078/7),1],'k-', lw=2)
plt.show()
'''
plt.ylim(0,2)
plt.xlim(0,10080*5)
plt.axhline(y=1.0, color = '0.75')
plt.plot(in_service_aggr[iter_nums-1,0,0:(weeks_cnt*10080/step_t),0], (1.0)*in_service_aggr[iter_nums-1,0,0:(weeks_cnt*10080/step_t),1],'-',color = '#006400')
plt.plot(in_service_aggr[iter_nums-1,1,0:(weeks_cnt*10080/step_t),0], (1.0)*in_service_aggr[iter_nums-1,1,0:(weeks_cnt*10080/step_t),1],'-',color = '#32CD32')
plt.plot(in_service_aggr[iter_nums-1,2,0:(weeks_cnt*10080/step_t),0], (1.0)*in_service_aggr[iter_nums-1,2,0:(weeks_cnt*10080/step_t),1],'-',color = '#00FF00')

plt.plot(a_aggr[iter_nums-1,0:(weeks_cnt*10080/step_t),0], (1.0/2.0)*a_aggr[iter_nums-1,0:(weeks_cnt*10080/step_t),1])
plt.plot((1.0/70.0)*arr_rate[0:10080,1],'k-', lw=2)
plt.show()


print 'all simulations end'
print datetime.datetime.now().time()


ofile  = open(output_file, "w")
#writer = csv.writer(ofile, delimiter=',')

for w in range(iter_nums):
    for i in range(steps_number):
        
        if ((i+1)%(10080/step_t)==0):
            cc=str(np.mean(arr_rate[((i*5)%10080):(((i+1)*5-1)%10080),1]))
        else:
            cc=str(np.mean(arr_rate[((i*5)%10080):(((i+1)*5)%10080),1]))
            
        row1=str(w)+','+str(int(a_aggr[w,i,0]))+','+cc+','\
        +str(float(a_aggr[w,i,1]))
        for j in range(server_cnt):
            row1=row1+','+str(float(in_service_aggr[w,j,i,1]))
        #print row1
        ofile.write(row1)
        ofile.write('\n')
    
ofile.close()
           