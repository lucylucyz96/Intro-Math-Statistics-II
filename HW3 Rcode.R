#(a)
#Generate sample size of 100 from standrd normal distribution
sample1 = rnorm(100,0,1)

#(b)
#Generate sample n=100 from standard normal with large/small points
sample2 = c(rnorm(100,0,1),rnorm(10,20,1.5),rnorm(10,-25,1.5))

#(c)
#H0: data follows standard normal distribution
#H1: data does not follow standard normal distrinbution
#For sample1:
percentage = seq(0,1,by=0.05) #divide the bins
percentage_quantiles = qnorm(percentage)#get the quantiles
for (i in sample1) {
  bins = cut(sample1, percentage_quantiles)
} #place data into bins
bins = table(bins) #make a table
chisq.test(bins) #perform chi-sq
#p value is 0.4959>0.05, we fail to reject H0.
#The data follows a standard normal distribution

#For sample2:
percentage2 = seq(0,1,by=0.05)
percentage_quantiles2 = qnorm(percentage2)
for(i in sample2){
  bins2 = cut(sample2, percentage_quantiles)
}
bins2 = table(bins2)
chisq.test(bins2)

#p value is 3.711e^-05<0.05, we reject H0.
#The data does not follow a standard normal distribution


#(d)
t_distri = rt(100,4)
qt(c(0.025,0.975),4) #determines the outliers for t distribution
#At alpha = 5%, the outlier cutoffs are -2.776445 and 2.776445
percentage_quantiles3 = qnorm(percentage)

bins3 = cut(t_distri, percentage_quantiles3)

bins3 = table(bins3)
chisq.test(bins3)

data_set = c(rt(100,4),rnorm(10,20,1.5),rnorm(10,-20,1.5))
percentage_quantiles4 = qnorm(percentage2)

bins4 = cut(data_set, percentage_quantiles4)

bins4 = table(bins4)
chisq.test(bins4)









