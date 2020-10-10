#when n=100, m = 500
#assume population variances are equal
bias = c()
bias2 = c()
estimate_1 = c()
estimate_2 = c()
true_var = (1/10)+(1/5) #since variance is 1
true_var
for (i in 1:10000){
sample_1= rnorm(10,mean = 0, sd=1)
sample_2 = rnorm(5,mean = 5, sd=1)
S_sample_1=var(sample_1)
S_sample_2 = var(sample_2)
S_square_p = (((10-1)*S_sample_1)+((5-1)*S_sample_2))/(10+5-2) #first way to calculate
estimate_1[i] = S_square_p*((1/10)+(1/5))
estimate_1 #first estimate

estimate_2[i] =(S_sample_1/10)+(S_sample_2/5)
estimate_2

}
bias = estimate_1-true_var
bias2=estimate_2-true_var
bias_val_sample_1 = (1/10000)*sum(bias)
bias_val_sample_2 = (1/10000)*sum(bias2)
bias_val_sample_1
bias_val_sample_2

var_sample_1= (1/(10000-1))*sum((estimate_1 -mean(estimate_1))^2)
var_sample_2= (1/(10000-1))*sum((estimate_2 -mean(estimate_2))^2)
var_sample_1
var_sample_2

d.varp=density(estimate_1) # The kernel smoothed density for pooled estimate
d.varu=density(estimate_2) # The kernel smoothed density for unpooled estimate
# Plot the kernel smoothed densities
plot(d.varp,main="n=10 ~N(0,1),m=5 ~N(5,1)") # the pooled estimate solid
abline(v=mean(estimate_1)) # the mean of the pooled estimate
lines(d.varu,lty=2) # add the unpooled estimate dash
abline(v=mean(estimate_2),lty=2) # the mean of the pooled estimate
abline(v=true_var,lwd=2,lty=4) # add the true parameter
legend("topright",c("Pooled estimate","Unpooled estimate","True parameter"),
       lty=c(1,2,4),lwd=c(1,1,2))