# APM Project

oldat<-read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/rioolympics.csv"))

str(oldat)

# we notice that gdp00 and gdp16 because of #N/A are read as Factors
# we pass the argument na.strings to assign NA
oldat<-read.csv(url("http://www.stats.gla.ac.uk/~tereza/rp/rioolympics.csv"),na.strings="#N/A")

# Now we transform to factor soviet, comm,muslim,oneparty,host
str(oldat)

oldat$soviet<-as.factor(oldat$soviet)
oldat$comm<-as.factor(oldat$comm)
oldat$muslim<-as.factor(oldat$muslim)
oldat$oneparty<-as.factor(oldat$oneparty)
oldat$host<-as.factor(oldat$host)
# now we check to see which countries have missing data.
oldat[!complete.cases(oldat),]$country

str(oldat)
# we notice that Afganistan, Cuba, and Syrian Arab Republic have missing data
# we also notice the dates of the missing data on Afganistan and Syrian Arab 
# Republic. They have missing data on the years of wars.
# For that reason we conclude that the missing values are not random missing
# values.

# So for the wars we chose to apply NOCB on Afganistan. Next Observation 
# Carried Backward. The decision is based on the fact that the GDP wouldnt 
# change dramatically the first year after the war.

# On Syria on the other hand the missing data (the war) are at the final years
# of our data. We cant apply NOCB

# On Cuba since we dont know any events that could caused the missing data
# we assume that its random. So we fill the missing value with linear
# interpolation based on previous GDP.

# So applying NOCB to Afganistan:
oldat$gdp00[oldat$country=="Afghanistan"]<-oldat$gdp04[oldat$country=="Afghanistan"]

# For cuba we chose to linearly extrapolate the data.
#### another solution could be to just repeat the last known value.
#### Linearly extrapolation seems more appropriate since the previous
#### 4 year gaps show a big difference in gdp.
cuba_gdp=t(as.matrix(oldat[21,3:6]))
cuba_years=as.matrix(seq(1:4))
data<-cbind(cuba_years,cuba_gdp)
data<-as.data.frame(data)
colnames(data)<-c("v1","v2")
mod_cuba<-lm(v2~v1,data)
summary(mod_cuba)
# We predict and add the value to the gdp16
oldat$gdp16[oldat$country=="Cuba"]<-predict(mod_cuba,newdata=data.frame(v1=5))[[1]]

# For Syria it doesnt make sense to predict the value so we will ignore that
# country
oldat<-oldat[-92,]

###########################################################################################

##  Explaratory Analysis

# we choose the columns that refer to 2012

newoldat12<-oldat[,c("country","country.code","gdp12","pop12","soviet","comm","muslim","oneparty","gold12","tot12","totgold12","totmedals12","altitude","athletes12","host")]

# Summary statistics
# We produce summary statistics for gdp12,pop12,gold12,tot12,altitude,athletes12
summary(newoldat12[,c("gdp12","pop12","gold12","tot12","altitude","athletes12")])
# from the summary statistics we notice potential outliers.

################################################################################################# na bgalv statistikes k gia ta 0, 1
# we take a closer look at gold12,tot12,and athletes12 through histograms
#hist(x=newoldat12$gold12,xlab="Gold Medals")
# From the histogram we see that most of the countries dont take any gold.
# While some of them take alot of them. So there seems to outlier
####################################################################################################### na koitaksv powerlaw
hist(x=newoldat12$tot12,xlab="Total Medals",main = "Histogram of total medals")
# From the histogram we see that most of the countries dont take any matals.
# While some of them take alot of them. So there seems to outlier
hist(x=newoldat12$athletes12,xlab="Athletes",main="Number of athletes per country")
# From the histogram we see that most of the countries participate with few athletes
# While some of them participate with alot of athletes.
cor(x=newoldat12$gold12,y=newoldat12$tot12)
# Now we would take a look if the proportion of gold metals compared to total.
library(ggplot2)
library(graphics)
ggplot(newoldat12,aes(x=tot12,y=gold12))+
  geom_point(size=1)+
  xlab("Total number of medals")+
  ylab("golds")+geom_smooth(method="lm",col="red")
summary(lm(gold12~tot12,newoldat12))


cooksd<-cooks.distance(lm(gold12~tot12,newoldat12))
plot(cooksd,pch="*",cex=2)
abline(h=4*mean(cooksd,na.rm=T),col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# from the graph we can see that we have outliers 104,18,80,103,51 

#removing them and creating a new dataset we get the following.
newoldat12_b<-newoldat12[-c(104,18,80,103,51),]

# Testing to see how good they predict values.

mod_new<-lm(gold12~tot12,newoldat12)
mod_new_b<-lm(gold12~tot12,newoldat12_b)

predict(mod_new,data.frame(tot12=c(91)))
predict(mod_new_b,data.frame(tot12=c(91)))

predict(mod_new,data.frame(tot12=c(10)))
predict(mod_new_b,data.frame(tot12=c(10)))


# We decided not to remove any outliers since although they pool the line upwards 
# since we are talking about proportion in smaller values that would be 
# translated to integers they dont change our predictions that much.
# for example. 3.22 vs 3.1 would both translated to 3 gold metals


ggplot(newoldat12,aes(x=tot12,y=gold12))+
  geom_point(size=1)+
  xlab("Total number of medals")+
  ylab("proportion of golds to total medals")+geom_smooth(method="lm",col="red")

# From the data above we notice that as the medals increase the gold medals
# increase linearly.
# So we will compare the variables we are interested with the total number of 
# medals


# Compare proportion of medals with number of athletes.

ggplot(newoldat12,aes(x=athletes12,y=tot12/totmedals12))+
  geom_point()+geom_smooth()
summary(lm((tot12/totmedals12)~athletes12,newoldat12))


# Compare proportion of medals with gdp
ggplot(newoldat12_b,aes(x=gdp12,y=tot12/totmedals12))+
  geom_point()
summary(lm((tot12/totmedals12)~gdp12,newoldat12))
# Check muslim countries

ggplot(newoldat12,aes(x=muslim,y=tot12))+geom_boxplot()+xlab("Muslim or not?")+
  theme(panel.background = element_rect(fill="transparent",colour = NA),
        plot.background = element_rect(fill="transparent",colour=NA),
        panel.border = element_rect(fill=NA,colour="black",size=1))
# Check ex soviet countries
ggplot(newoldat12,aes(x=soviet,y=tot12))+geom_boxplot()+xlab("Soviet or not?")+
  theme(panel.background = element_rect(fill="transparent",colour = NA),
        plot.background = element_rect(fill="transparent",colour=NA),
        panel.border = element_rect(fill=NA,colour="black",size=1))

# Check communist
ggplot(newoldat12,aes(x=comm,y=tot12))+geom_boxplot()+xlab("Socialist or not?")+
  theme(panel.background = element_rect(fill="transparent",colour = NA),
        plot.background = element_rect(fill="transparent",colour=NA),
        panel.border = element_rect(fill=NA,colour="black",size=1))
# Check host
ggplot(newoldat12,aes(x=host,y=tot12))+geom_boxplot()+xlab("Host or not?")+
  theme(panel.background = element_rect(fill="transparent",colour = NA),
        plot.background = element_rect(fill="transparent",colour=NA),
        panel.border = element_rect(fill=NA,colour="black",size=1))


ggplot(newoldat12,aes(x=host,y=gdp12))+geom_boxplot()+xlab("Host or not?")+
  theme(panel.background = element_rect(fill="transparent",colour = NA),
        plot.background = element_rect(fill="transparent",colour=NA),
        panel.border = element_rect(fill=NA,colour="black",size=1))

# Check gdp
ggplot(newoldat12,aes(x=gdp12,y=(tot12)**2))+geom_point()+geom_smooth()
###############################################################################3 deikths spearman rho

summary(lm((tot12**2)~gdp12,newoldat12))
summary(lm((tot12)~gdp12,newoldat12))

summary(lm((tot12/totmedals12)~pop12,newoldat12))
summary(lm((tot12/totmedals12)~gdp12,newoldat12))


################################################################################
# Models
################################################################################
################################################################################

# join all data up until 2012
year00<-oldat[,c("gdp00","pop00","soviet","comm","muslim","oneparty","gold00","tot00","totgold00","totmedals00","altitude","athletes00","host")]
year04<-oldat[,c("gdp04","pop04","soviet","comm","muslim","oneparty","gold04","tot04","totgold04","totmedals04","altitude","athletes04","host")]
year08<-oldat[,c("gdp08","pop08","soviet","comm","muslim","oneparty","gold08","tot08","totgold08","totmedals08","altitude","athletes08","host")]
year12<-oldat[,c("gdp12","pop12","soviet","comm","muslim","oneparty","gold12","tot12","totgold12","totmedals12","altitude","athletes12","host")]
colnames(year00)<-colnames(year04)<-colnames(year08)<-colnames(year12)<-c("gdp","pop","soviet","comm","muslim","oneparty","gold","tot","totgold","totmedals","altitude","athletes","host")

data_until12<-rbind(year00,year04,year08,year12)

###############################################
###############################################
###############################################


mod_logit<-glm(cbind(tot,totmedals-tot)~gdp+pop+soviet+comm+muslim+oneparty+altitude+athletes+host,family=binomial(link="logit"),data=data_until12)
summary(mod_logit)

mod_probit<-glm(cbind(tot,totmedals-tot)~gdp+pop+soviet+comm+muslim+oneparty+altitude+athletes+host,family=binomial(link="probit"),data=data_until12)
summary(mod_probit)

mod_cloglog<-glm(cbind(tot,totmedals-tot)~gdp+pop+soviet+comm+muslim+oneparty+altitude+athletes+host,family=binomial(link="cloglog"),data=data_until12)
summary(mod_cloglog)

# from the above we see that probit gives slightly better results
# gdp, pop, comm, muslim, oneparty, athletes, and host are important factors for the total 
# amount of medals a country gets.

mod_probit<-glm(cbind(tot,totmedals-tot)~gdp+pop+comm+muslim+oneparty+athletes+host,family=binomial(link="probit"),data=data_until12)
summary(mod_probit)


qchisq(df=428-7,p=0.95)
################### poison

mod_poison1<-glm(tot~gdp+pop+soviet+comm+muslim+oneparty+altitude+athletes+host,family=poisson,data=data_until12)
summary(mod_poison1)

mod_poison2<-glm(tot~gdp+pop+comm+muslim+oneparty+athletes+host,family=poisson,data=data_until12)
summary(mod_poison2)
qchisq(df=428-7,p=0.95)

# So its not a good fit

# We check for outliers that effect residual deviance

resp <- resid(mod_poison2, type = "pearson")
resd <- resid(mod_poison2, type = "deviance")
p1<- ggplot(mod_poison2, aes(sample = resp)) + geom_point(stat = "qq", color = "#7fc97f") +
  ylab("Pearson residuals")
p2<- ggplot(mod_poison2, aes(sample = resd)) + geom_point(stat = "qq", color = "#7fc97f") +
  ylab("Deviance residuals")
p3<- ggplot(mod_poison2, aes(x = predict(mod_poison2, type="link"), y =resd))+
  geom_point(col = "#7fc97f") +
  ylab("Deviance residuals") + xlab("Linear predictor")
grid.arrange(p1, p2, p3, nrow = 1)




# We check for overdispersion and its positive. Alpha=2.12
dispersiontest(mod_poison2,trafo=1)

# So we cannot apply poison model. Since the variance is bigger than the mean 
# (assumption of poisson model) instead of using quasi-poisson model we choose
# to use negative binomial model

library(MASS)
mod_nb1<-glm.nb(tot~gdp+pop+soviet+comm+muslim+oneparty+altitude+athletes+host,data=data_until12)
summary(mod_nb1)

# we use only the significant variable

mod_nb2<-glm.nb(tot~comm+muslim+altitude+athletes+host,data=data_until12)
summary(mod_nb2)

qchisq(df=428-5,p=0.95)


#which is an acceptable model.
library(Metrics)

actual<-oldat$tot16 # results for 2016

# Now we will compare the models binomial, poisson and negative binomial

# for binomial model mod_probit
# gdp+pop+comm+muslim+oneparty+athletes+host
predicted_mod_probit<-predict.glm(mod_probit,data.frame(gdp=oldat$gdp16,
                                                        pop=oldat$pop16,
                                                        comm=oldat$comm,
                                                        muslim=oldat$muslim,
                                                        oneparty=oldat$oneparty,
                                                        athletes=oldat$athletes16,
                                                        host=oldat$host
                                                        ))
rmse(actual,predicted_mod_probit)

# for poisson model
# gdp+pop+comm+muslim+oneparty+athletes+host
# mod_poison2

predicted_mod_poison2<-predict.glm(mod_poison2,data.frame(gdp=oldat$gdp16,
                                                          pop=oldat$pop16,
                                                          comm=oldat$comm,
                                                          muslim=oldat$muslim,
                                                          oneparty=oldat$oneparty,
                                                          athletes=oldat$athletes16,
                                                          host=oldat$host))

rmse(actual,predicted_mod_poison2)


# for negative binomial model
# comm+muslim+altitude+athletes+host
# mod_nb2


predicted_mod_nb2<-predict.glm(mod_nb2,data.frame(comm=oldat$comm,
                                         muslim=oldat$muslim,
                                         altitude=oldat$altitude,
                                         athletes=oldat$athletes16,
                                         host=oldat$host))

rmse(actual,predicted_mod_nb2)

# Negative binomial model (mod_nb2) give us a better fit.

# failures

pres.n2<-resid(mod_nb2,type="pearson")

expmedals2<-round(fitted(mod_nb2)[abs(pres.n2)>1], 2)
cbind(oldat[abs(pres.n2)>1, c('country','tot16')], expmedals2)

