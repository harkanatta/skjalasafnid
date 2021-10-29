

CB.dat <- read.table(file.choose(), header = TRUE)  #choose 'CurrentBiologyData.txt'

dim(CB.dat)
# [1] 516  12

# for the columns "no_emails_worked", "no_response", "response_no_help", "data_lost", "cant_share", and "data_received" a '1' indicates which category each manuscript falls into. The sum of a row for these columns is thus always '1' (an article cannot be in more than one category)
#this setup means that each column needs to be reversed for the logistic regressions.


names(CB.dat)

# [1] "paper"                       "year"                       
# [3] "no_emails_worked"            "no_response"                
# [5] "response_no_help"            "data_lost"                  
# [7] "cant_share"                  "data_received"              
# [9] "number_emails_paper"         "number_emails_web"          
# [11] "number_bounced_paper_emails" "number_bounced_web_emails"


#makes the 'age' variable, which goes from 2 to 22 years old

CB.dat$age <- 2013 - CB.dat$year


#***********

#examining 'no_emails_worked'

# there were 131 papers where none of the emails appeared to work
summary(factor(CB.dat$no_emails_worked))
# 0   1 
# 385 131 



# for the logistic regression of the proportion of papers per year where at least one email appeared to work, we need to reverse the 'no_emails_worked' factor so that an email getting through is a 1:

CB.dat$email <- replace(CB.dat$no_emails_worked, which(CB.dat$no_emails_worked == 0), "yes")
CB.dat$email <- replace(CB.dat$email, which(CB.dat$no_emails_worked == 1), "no")
CB.dat$email <- factor(CB.dat$email)

summary(factor(CB.dat$email))
# no yes 
# 131 385

#% papers with at least one apparently working email
385/(131+385) 
# [1] 0.746124


# a logistic model for the proportion of emails that appeared to work

em1 <- glm(email ~ age, data = CB.dat, family = "binomial")
summary(em1)

# Call:
# glm(formula = email ~ age, family = "binomial", data = CB.dat)

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -1.9363  -1.2842   0.6381   0.7991   1.0742  

# Coefficients:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  1.85409    0.21052   8.807  < 2e-16 ***
# age         -0.07302    0.01626  -4.491 7.09e-06 ***

# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

       # Null deviance: 584.68  on 515  degrees of freedom
# Residual deviance: 564.03  on 514  degrees of freedom
# AIC: 568.03

# Number of Fisher Scoring iterations: 4


#this gives the 95% confidence intervals on the fitted coefficients
exp(cbind(em1$coef, confint(em1)))
                                          # 2.5 %     97.5 %
# (Intercept) 6.3858649 4.2720542 9.7634073
# age            0.9295794 0.9001207 0.9594594



#this is the Chi-squared test for the significance of the age term
drop1(em1, test = "Chi")
# Single term deletions

# Model:
# email ~ age
       # Df Deviance    AIC    LRT  Pr(>Chi)    
# <none>      564.03 568.03                     
# age     1   584.68 586.68 20.651 5.512e-06 ***


#the below are needed for plotting Figure 1A

# this is a list of numbers running between 2 and 22
newdata.em <- data.frame(age = seq(2, 22, 0.1))

#this is the fitted line
newdata.em$phat <- predict(em1, newdata.em, type = "response")

#this generates the 95% confidence interval band
newdata.em <- cbind(newdata.em, predict(em1, newdata.em, type = "response", se.fit = TRUE))

newdata.em <- within(newdata.em, {
  LL <- fit - 1.96*se.fit
  UL <- fit + 1.96*se.fit
})


#these are the observed proportions of 'no_emails_worked' for each year (note that the code for Figure 1A plots 1-props.em)
props.em <- tapply(CB.dat$no_emails_worked, factor(CB.dat$age), mean)

pred.props.em <- data.frame(ages = seq(2, 22, 2), props.em = props.em)


# round(props.em,digits=2)
   # 2    4    6    8   10   12   14   16   18   20   22 
# 0.16 0.13 0.18 0.24 0.20 0.30 0.48 0.37 0.31 0.39 0.35 


#***************

#examining 'no_response' for the papers where at least one email appeared to work (procedure is more or less identical to 'no_emails_worked')


CB.through <- CB.dat[CB.dat$email == "yes", ]

#as above, we need to reverse 'no_response' so that getting a response is a '1'

CB.through$response <- CB.through$no_response
CB.through$response <- replace(CB.through$response, which(CB.through$response == 1), "no")
CB.through$response <- replace(CB.through$response, which(CB.through$response == 0), "yes")
CB.through$response <- factor(CB.through$response)

# there were 194 papers where we did not receive a response
summary(CB.through$response)
#  no yes 
# 194 191

# i.e. a 49.6% response rate


re1 <- glm(response ~ age, data = CB.through, family = "binomial")
summary(re1)

# Call:
# glm(formula = response ~ age, family = "binomial", data = CB.through)

# Deviance Residuals: 
   # Min      1Q  Median      3Q     Max  
# -1.182  -1.170  -1.164   1.185   1.191  

# Coefficients:
            # Estimate Std. Error z value Pr(>|z|)
# (Intercept) -0.03537    0.18364  -0.193    0.847
# age          0.00215    0.01659   0.130    0.897

# (Dispersion parameter for binomial family taken to be 1)

    # Null deviance: 533.70  on 384  degrees of freedom
# Residual deviance: 533.68  on 383  degrees of freedom
# AIC: 537.68

# Number of Fisher Scoring iterations: 3

exp(cbind(re1$coef, confint(re1)))  
# 						                    2.5 %    97.5 %
# (Intercept) 0.9652443 0.6730092 1.383830
# age            1.0021520 0.9700372 1.035352

newdata.re <- data.frame(age = seq(2, 22, 0.1))
newdata.re$phat <- predict(re1, newdata.re, type = "response")
newdata.re <- cbind(newdata.re, predict(re1, newdata.re, type = "response", se.fit = TRUE))

newdata.re <- within(newdata.re, {
  LL <- fit - 1.96*se.fit
  UL <- fit + 1.96*se.fit
})

#these are the observed proportions of 'no_response' for each year (note that the code for Figure 1A plots 1-props.re)
props.re <- tapply(CB.through$no_response, factor(CB.through$age), mean)

pred.props.re <- data.frame(ages = seq(2, 22, 2), props.re = props.re)


# round(props.re,digits=2)
   # 2    4    6    8   10   12   14   16   18   20   22 
# 0.43 0.57 0.57 0.41 0.54 0.50 0.62 0.47 0.38 0.50 0.53 



#***************

#examining 'no_useful_response' for the papers where we received a response (procedure is more or less identical to 'no_emails_worked')


CB.use <- CB.through[which(CB.through$response == "yes"), ]

CB.use$response_no_help
CB.use$help <- CB.use$response_no_help
CB.use$help <- replace(CB.use$help, which(CB.use$help == 1), "no")
CB.use$help <- replace(CB.use$help, which(CB.use$help == 0), "yes")
CB.use$help <- factor(CB.use$help)
summary(CB.use$help)
# no yes 
# 33 158

h1 <- glm(help ~ age, data = CB.use, family = "binomial")
summary(h1) 

# Call:
# glm(formula = help ~ age, family = "binomial", data = CB.use)

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -1.9119   0.5994   0.6143   0.6257   0.6296  

# Coefficients:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept) 1.504493   0.339184   4.436 9.18e-06 ***
# age         0.006724   0.030857   0.218    0.827    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

    # Null deviance: 175.82  on 190  degrees of freedom
# Residual deviance: 175.77  on 189  degrees of freedom
# AIC: 179.77

# Number of Fisher Scoring iterations: 4


exp(cbind(h1$coef, confint(h1)))
                                       # 2.5 %     97.5 %
# (Intercept) 4.501869 2.3706457 9.018685
# age           1.006747  0.9487272 1.071641

newdata.h <- data.frame(age = seq(2, 22, 0.1))
newdata.h$phat <- predict(h1, newdata.h, type = "response")
newdata.h <- cbind(newdata.h, predict(h1, newdata.h, type = "response", se.fit = TRUE))

newdata.h <- within(newdata.h, {
  LL <- fit - 1.96*se.fit
  UL <- fit + 1.96*se.fit
})

# remember to do 1-props.h in the figure
props.h <- tapply(CB.use$response_no_help, factor(CB.use$age), mean)

pred.props.h <- data.frame(ages = seq(2, 22, 2), props.h = props.h)

# round(props.h,digits=2)
   # 2    4    6    8   10   12   14   16   18   20   22 
# 0.21 0.12 0.09 0.30 0.24 0.20 0.12 0.10 0.00 0.27 0.25 


#***************

#examining 'data extant' for the papers where we received a useful response from the authors(procedure is more or less identical to above)

CB.extant <- CB.use[which(CB.use$help == "yes"), ]
names(CB.extant)
 # [1] "paper"                       "year"                        "no_emails_worked"            "no_response"                
 # [5] "response_no_help"            "data_lost"                   "cant_share"                  "data_received"              
 # [9] "number_emails_paper"         "number_emails_web"           "number_bounced_paper_emails" "number_bounced_web_emails"  
# [13] "email"                       "age"                         "response"                    "help"   

summary(factor(CB.extant$data_received))
  # 0   1 
 # 57 101 

summary(factor(CB.extant$cant_share))
  # 0   1 
# 138  20 


CB.extant$alive <- rowSums(cbind(CB.extant$data_received, CB.extant$cant_share))
summary(factor(CB.extant$alive))
#  0   1 
# 37 121

al1 <- glm(alive ~ age, data = CB.extant, family = "binomial")
summary(al1)

# Call:
# glm(formula = alive ~ age, family = "binomial", data = CB.extant)

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -2.1985   0.3686   0.4323   0.6858   1.4690  

# Coefficients:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  2.98688    0.46614   6.408 1.48e-10 ***
# age           -0.16593    0.03416  -4.857 1.19e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

    # Null deviance: 171.99  on 157  degrees of freedom
# Residual deviance: 144.00  on 156  degrees of freedom
# AIC: 148

# Number of Fisher Scoring iterations: 4


  
exp(cbind(al1$coef, confint(al1)))  
                                          # 2.5 %        97.5 %
# (Intercept) 19.8237864 8.4667072 53.3316290
# age              0.8471072 0.7894149  0.9033122

newdata.a <- data.frame(age = seq(2, 22, 0.1))
newdata.a$phat <- predict(al1, newdata.a, type = "response")
newdata.a <- cbind(newdata.a, predict(al1, newdata.a, type = "response", se.fit = TRUE))

newdata.a <- within(newdata.a, {
  LL <- fit - 1.96*se.fit
  UL <- fit + 1.96*se.fit
})


# remember to do 1-pred.props.a in the figure
props.a <- tapply(CB.extant$alive, factor(CB.extant$age), mean)

pred.props.a <- data.frame(ages = seq(2, 22, 2), props.a = props.a)


# round(props.a,digits=2)
   # 2    4    6    8   10   12   14   16   18   20   22 
# 1.00 0.78 0.81 0.93 0.85 0.67 0.86 0.78 0.53 0.12 0.33 




######Figure 1######

library(ggplot2)



# plot of emails got through
p1 <- ggplot(newdata.em, aes(x = age, y = fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) +
  geom_line(size = 1) + ylim(0, 1) +
  labs(x = "age of paper (years)", y = "P(email got through)", title = "A") +
  geom_point(aes(x = ages, y = 1-props.em), data = pred.props.em, size = 2, col = "red") +
  theme_bw()+ theme(axis.title=element_text(size=8), axis.text = element_text(size = 8))

# plot of response for emails that got through
p2 <- ggplot(newdata.re, aes(x = age, y = fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) +
  geom_line(size = 1) + ylim(0, 1) +
  labs(x = "age of paper (years)", y = "P(response|email got through)", title = "B") +
  geom_point(aes(x = ages, y = 1-props.re), data = pred.props.re, size = 2, col = "red") +
  theme_bw()+ theme(axis.title=element_text(size=8), axis.text = element_text(size = 8))

# plot of useful response given that a response was received
p3 <- ggplot(newdata.h, aes(x = age, y = fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) +
  geom_line(size = 1) + ylim(0, 1) +
  labs(x = "age of paper (years)", y = "P(useful response|response)", title = "C") +
  geom_point(aes(x = ages, y = 1-props.h), data = pred.props.h, size = 2, col = "red") +
  theme_bw()+ theme(axis.title=element_text(size=8), axis.text = element_text(size = 8))


# plot of data extant given a useful response was received
p4 <- ggplot(newdata.a, aes(x = age, y = fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) +
  geom_line(size = 1) + ylim(0, 1) +
  labs(x = "age of paper (years)", y = "P(data extant|useful response)", title = "D") +
  geom_point(aes(x = ages, y = props.a), data = pred.props.a, size = 2, col = "red") +
  theme_bw()+ theme(axis.title=element_text(size=8), axis.text = element_text(size = 8))


# Multiple plot function (from Cookbook for R: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, by.row=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#creates a 4 piece panel plot
quartz(width = 6, height = 5, pointsize = 6)
multiplot(p1, p3, p2, p4, cols = 2)


####### 
#Figure of data extant as proportion of entire dataset (rather than just those papers where the authors told us the status of their data)


CB.dat$alive_all_papers <- rowSums(cbind(CB.dat$cant_share, CB.dat$data_received))

l1 <- glm(alive_all_papers ~ age, data = CB.dat, family = "binomial")
summary(l1)

# Call:
# glm(formula = alive_all_papers ~ age, family = "binomial", data = CB.dat)

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -0.9212  -0.8112  -0.6196  -0.4665   2.1317  

# Coefficients:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.48507    0.18715  -2.592  0.00955 ** 
# age         -0.07628    0.01842  -4.141 3.46e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

    # Null deviance: 562.08  on 515  degrees of freedom
# Residual deviance: 543.32  on 514  degrees of freedom
# AIC: 547.32

# Number of Fisher Scoring iterations: 4


newdata <- data.frame(age = seq(2, 22, 0.1))
newdata$phat <- predict(l1, newdata, type = "response")
newdata <- cbind(newdata, predict(l1, newdata, type = "response", se.fit = TRUE))

newdata <- within(newdata, {
  LL <- fit - 1.96*se.fit
  UL <- fit + 1.96*se.fit
})

props <- tapply(CB.dat$alive_all_papers, factor(CB.dat$age), mean)
pred.props <- data.frame(ages = seq(2, 22, 2), props = props)


ggplot(newdata, aes(x = age, y = fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) +
  geom_line(size = 1) + ylim(0, 1) +
  labs(x = "age of paper (years)", y = "P(extant data)") +
  geom_point(aes(x = ages, y = props), data = pred.props, size = 2, col = "red") +
  theme_bw() + theme(axis.title=element_text(size=8), axis.text = element_text(size = 8))



############# Table 1 #############

# sample_size<-tapply(CB.dat$no_emails_worked, factor(CB.dat$year), length)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
  # 26   36   35   30   40   43   46   45   66   69   80 


# tapply(CB.dat$no_emails_worked, factor(CB.dat$year), sum, na.rm=TRUE)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
   # 9   14   11   11   19   13    9   11   12    9   13 
   
# round(tapply(CB.dat$no_emails_worked, factor(CB.dat$year), sum, na.rm=TRUE)/sample_size,digits=2)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
# 0.35 0.39 0.31 0.37 0.48 0.30 0.20 0.24 0.18 0.13 0.16 


   
# tapply(CB.dat$no_response, factor(CB.dat$year), sum, na.rm=TRUE)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
   # 9   11    9    9   13   15   20   14   31   34   29
   


# round(tapply(CB.dat$no_response, factor(CB.dat$year), sum, na.rm=TRUE)/sample_size,digits=2)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
# 0.35 0.31 0.26 0.30 0.32 0.35 0.43 0.31 0.47 0.49 0.36


# tapply(CB.dat$response_no_help, factor(CB.dat$year), sum, na.rm=TRUE)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
   # 2    3    0    1    1    3    4    6    2    3    8 
 
# round(tapply(CB.dat$response_no_help, factor(CB.dat$year), sum, na.rm=TRUE)/sample_size, digits=2)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
# 0.08 0.08 0.00 0.03 0.02 0.07 0.09 0.13 0.03 0.04 0.10

 
 
# tapply(CB.dat$data_lost, factor(CB.dat$year), sum, na.rm=TRUE)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
   # 4    7    7    2    1    4    2    1    4    5    0
   
# round(tapply(CB.dat$data_lost, factor(CB.dat$year), sum, na.rm=TRUE)/sample_size,digits=2)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
# 0.15 0.19 0.20 0.07 0.02 0.09 0.04 0.02 0.06 0.07 0.00 
   
   
# tapply(CB.dat$cant_share, factor(CB.dat$year), sum, na.rm=TRUE)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
   # 1    0    2    3    0    0    0    0    1    6    7 
   
# round(tapply(CB.dat$cant_share, factor(CB.dat$year), sum, na.rm=TRUE)/sample_size,digits=2)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
# 0.04 0.00 0.06 0.10 0.00 0.00 0.00 0.00 0.02 0.09 0.09 


# tapply(CB.dat$data_received, factor(CB.dat$year), sum, na.rm=TRUE)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
   # 1    1    6    4    6    8   11   13   16   12   23
   

# round(tapply(CB.dat$data_received, factor(CB.dat$year), sum, na.rm=TRUE)/sample_size,digits=2)
# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
# 0.04 0.03 0.17 0.13 0.15 0.19 0.24 0.29 0.24 0.17 0.29 



# extant_by_year<-tapply(CB.dat$cant_share, factor(CB.dat$year), sum, na.rm=TRUE)+tapply(CB.dat$data_received, factor(CB.dat$year), sum, na.rm=TRUE)

# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
   # 2    1    8    7    6    8   11   13   17   18   30 

# round(extant_by_year/sample_size,digits=2)

# 1991 1993 1995 1997 1999 2001 2003 2005 2007 2009 2011 
# 0.08 0.03 0.23 0.23 0.15 0.19 0.24 0.29 0.26 0.26 0.38 


####### Analysis of emails (Figure 2)

total_emails<-CB.dat$number_emails_paper+CB.dat$number_emails_web


### does number of emails found in the paper change through time?


ew1 <- glm(number_emails_paper ~ age, data = CB.dat, family = "poisson")
summary(ew1)

# Call:
# glm(formula = number_emails_paper ~ age, family = "poisson", 
    # data = CB.dat)

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -1.6366  -0.8563  -0.1635   0.3509   1.9056  

# Coefficients:
             # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.425116   0.081163   5.238 1.62e-07 ***
# age           -0.066481   0.008646  -7.689 1.48e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

        # Null deviance: 336.47  on 515  degrees of freedom
# Residual deviance: 270.84  on 514  degrees of freedom
# AIC: 1059

# Number of Fisher Scoring iterations: 5


drop1(ew1, test = "Chi")

# Single term deletions

# Model:
# CB.dat$number_emails_paper ~ CB.dat$age
           # Df Deviance    AIC    LRT  Pr(>Chi)    
# <none>              270.84 1059.0                     
# CB.dat$age  1   336.47 1122.7 65.625 5.454e-16 ***


#incident rate ratio exp of coeff exp(-0.066)
#[1] 0.9361309

#for each year of age the count goes down to 0.94 x count


newdata.ew1 <- data.frame(age = seq(min(CB.dat$age), max(CB.dat$age), length.out=516))
newdata.ew1$phat <- predict(ew1, newdata.ew1, type = "response")
newdata.ew1 <- cbind(newdata.ew1, predict(ew1, newdata.ew1, type = "response", se.fit = TRUE))

newdata.ew1 <- within(newdata.ew1, {
  LL <- fit - 1.96*se.fit
  UL <- fit + 1.96*se.fit
})


props.ew1 <- tapply(CB.dat$number_emails_paper, factor(CB.dat$age), mean, na.rm=TRUE)

pred.props.ew1 <- data.frame(ages = seq(2, 22, 2), props.ew1 = props.ew1)


# round(props.ew1,digits=2)
   # 2    4    6    8   10   12   14   16   18   20   22 
# 1.18 1.06 1.09 1.13 1.13 0.84 0.68 0.40 0.03 0.36 0.42 


### the proportion of emails found in the paper that appeared to work, by year

paper_email_success<-cbind(CB.dat$number_emails_paper-CB.dat$number_bounced_paper_emails,CB.dat$number_bounced_paper_emails)


ew2 <- glm(paper_email_success ~ age, data = CB.dat, family = "binomial")
summary(ew2)

# Call:
# glm(formula = paper_email_success ~ age, family = "binomial", 
    # data = CB.dat)

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -2.0417  -1.2112   0.0000   0.9625   1.7715  

# Coefficients:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.68552    0.17677   3.878 0.000105 ***
# age         -0.03912    0.01903  -2.056 0.039812 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

    # Null deviance: 513.50  on 369  degrees of freedom
# Residual deviance: 509.26  on 368  degrees of freedom
# AIC: 554.85

# Number of Fisher Scoring iterations: 4



drop1(ew2, test = "Chi")

# Single term deletions

# Model:
# paper_email_success ~ age
       # Df Deviance    AIC    LRT Pr(>Chi)  
# <none>      509.26 554.85                  
# age     1     513.50 557.09 4.2457  0.03935 *



exp(cbind(ew2$coef, confint(ew2)))
                                       # 2.5 %       97.5 %
# (Intercept) 1.984803 1.4076872 2.8173688
# age            0.961631 0.9261634 0.9980921



newdata.ew2 <- data.frame(age = seq(min(CB.dat$age), max(CB.dat$age), length.out=516))
newdata.ew2$phat <- predict(ew2, newdata.ew2, type = "response")
newdata.ew2 <- cbind(newdata.ew2, predict(ew2, newdata.ew2, type = "response", se.fit = TRUE))

newdata.ew2 <- within(newdata.ew2, {
  LL <- fit - 1.96*se.fit
  UL <- fit + 1.96*se.fit
})

paper_email_year_total<-tapply(CB.dat$number_emails_paper, factor(CB.dat$age), sum,na.rm=T)

paper_email_success_year_total<-tapply(CB.dat$number_emails_paper-CB.dat$number_bounced_paper_emails, factor(CB.dat$age), sum,na.rm=T)

props.ew2 <- paper_email_success_year_total/paper_email_year_total

pred.props.ew2 <- data.frame(ages = seq(2, 22, 2), props.ew2 = props.ew2)

# round(props.ew2,digits=2)
   # 2    4    6    8   10   12   14   16   18   20   22 
# 0.70 0.71 0.61 0.47 0.44 0.39 0.52 0.67 1.00 0.69 0.73 


### does total number of web emails change through time?

ew3 <- glm(number_emails_web ~ age, data = CB.dat, family = "poisson")
summary(ew3)

# Call:
# glm(formula = number_emails_web ~ age, family = "poisson", data = CB.dat)

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -1.4392  -1.2553   0.1147   0.2573   1.8612  

# Coefficients:
             # Estimate Std. Error z value Pr(>|z|)   
# (Intercept) -0.299164   0.091429  -3.272  0.00107 **
# age          0.015190   0.007416   2.048  0.04054 * 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for poisson family taken to be 1)

    # Null deviance: 442.37  on 515  degrees of freedom
# Residual deviance: 438.21  on 514  degrees of freedom
# AIC: 1180.9

# Number of Fisher Scoring iterations: 5



drop1(ew3, test = "Chi")

# Single term deletions

# Model:
# number_emails_web ~ age
       # Df Deviance    AIC   LRT Pr(>Chi)  
# <none>      438.21 1180.9                 
# age       1   442.37 1183.1 4.154  0.04154 *



newdata.ew3 <- data.frame(age = seq(min(CB.dat$age), max(CB.dat$age), length.out=516))
newdata.ew3$phat <- predict(ew3, newdata.ew3, type = "response")
newdata.ew3 <- cbind(newdata.ew3, predict(ew3, newdata.ew3, type = "response", se.fit = TRUE))

newdata.ew3 <- within(newdata.ew3, {
  LL <- fit - 1.96*se.fit
  UL <- fit + 1.96*se.fit
})


props.ew3 <- tapply(CB.dat$number_emails_web, factor(CB.dat$age), mean, na.rm=TRUE)
pred.props.ew3 <- data.frame(ages = seq(2, 22, 2), props.ew3 = props.ew3)


# round(props.ew3,digits=2)
   # 2    4    6    8   10   12   14   16   18   20   22 
# 0.60 0.80 0.92 0.91 0.87 0.95 0.95 1.10 1.20 0.81 0.73 





### the proportion of emails found on the web that appeared to work, by year

web_email_success<-cbind(CB.dat$number_emails_web-CB.dat$number_bounced_web_emails, CB.dat$number_bounced_web_emails)


ew4 <- glm(web_email_success ~ age, data = CB.dat, family = "binomial")
summary(ew4)

# Call:
# glm(formula = web_email_success ~ age, family = "binomial", data = CB.dat)

# Deviance Residuals: 
    # Min       1Q   Median       3Q      Max  
# -2.1725  -0.3327   0.0000   0.9066   1.5863  

# Coefficients:
            # Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  0.88088    0.20160   4.369 1.25e-05 ***
# age           -0.03402    0.01622  -2.097    0.036 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

        # Null deviance: 460.80  on 335  degrees of freedom
# Residual deviance: 456.38  on 334  degrees of freedom
# AIC: 523.94

# Number of Fisher Scoring iterations: 4


drop1(ew4, test = "Chi")

# Single term deletions

# Model:
# web_email_success ~ age
       # Df Deviance    AIC    LRT Pr(>Chi)  
# <none>      456.38 523.94                  
# age     1     460.80 526.36 4.4207  0.03551 *



exp(cbind(ew4$coef, confint(ew4)))
                                         # 2.5 %     97.5 %
# (Intercept) 2.413027 1.6333022 3.6039296
# age            0.966554 0.9361368 0.9976983


newdata.ew4 <- data.frame(age = seq(min(CB.dat$age), max(CB.dat$age), length.out=516))
newdata.ew4$phat <- predict(ew4, newdata.ew4, type = "response")
newdata.ew4 <- cbind(newdata.ew4, predict(ew4, newdata.ew4, type = "response", se.fit = TRUE))

newdata.ew4 <- within(newdata.ew4, {
  LL <- fit - 1.96*se.fit
  UL <- fit + 1.96*se.fit
})



web_email_year_total<-tapply(CB.dat$number_emails_web, factor(CB.dat$age), sum, na.rm=T)

web_email_success_year_total<-tapply(CB.dat$number_emails_web-CB.dat$number_bounced_web_emails, factor(CB.dat$age), sum)

props.ew4 <- web_email_success_year_total/web_email_year_total

pred.props.ew4 <- data.frame(ages = seq(2, 22, 2), props.ew4 = props.ew4)

# round(props.ew4,digits=2)
   # 2    4    6    8   10   12   14   16   18   20   22 
# 0.75 0.67 0.66 0.61 0.68 0.61 0.39 0.61 0.69 0.52 0.58 


###################
# Figure 2 plots

# plot of # emails from paper
em1 <- ggplot(newdata.ew1, aes(x = age, y = fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) +
  geom_line(size = 1) +
  labs(x = "age of paper (years)", y = "number of emails in paper", title = "A") +
  geom_point(aes(x = ages, y = props.ew1), data = pred.props.ew1, size = 2, col = "red") +
  theme_bw()+ theme(axis.title=element_text(size=8), axis.text = element_text(size = 8))

#plot of proportion of emails worked (as percentage)

em2 <- ggplot(newdata.ew2, aes(x = age, y = fit*100)) +
  geom_ribbon(aes(ymin = LL*100, ymax = UL*100), alpha = 0.25) +
  geom_line(size = 1) + ylim(0, 100) +
  labs(x = "age of paper (years)", y = "% of paper emails that worked", title = "B") +
  geom_point(aes(x = ages, y = props.ew2*100), data = pred.props.ew2, size = 2, col = "red") +
  theme_bw() + theme(axis.title=element_text(size=8), axis.text = element_text(size = 8))


# plot of number of emails on web
em3 <- ggplot(newdata.ew3, aes(x = age, y = fit)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = 0.25) +
  geom_line(size = 1) + ylim(0, 1.25) +
  labs(x = "age of paper (years)", y = "number of emails on web", title = "C") +
  geom_point(aes(x = ages, y = props.ew3), data = pred.props.ew3, size = 2, col = "red") +
  theme_bw()+ theme(axis.title=element_text(size=8), axis.text = element_text(size = 8))


# plot of email worked given it was on the web (as percentage)

em4 <- ggplot(newdata.ew4, aes(x = age, y = fit*100)) +
  geom_ribbon(aes(ymin = LL*100, ymax = UL*100), alpha = 0.25) +
  geom_line(size = 1) + ylim(0, 100) +
  labs(x = "age of paper (years)", y = "% of web emails that worked", title = "D") +
  geom_point(aes(x = ages, y = props.ew4*100), data = pred.props.ew4, size = 2, col = "red") +
  theme_bw()+ theme(axis.title=element_text(size=8), axis.text = element_text(size = 8))


quartz(width = 6, height = 5, pointsize = 6)
multiplot(em1, em3, em2, em4, cols = 2)
