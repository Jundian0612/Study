# FDA R Examples for Statistical School 2025
# ------------------------------------------------------------------------------
# References: 
# Ramsay, Hooker & Graves (2009)
# Functional Data Analysis with R and Matlab (Springer)
# ------------------------------------------------------------------------------
# 2025/07/16  Pai-Ling Li(Lecturer)
#             Chun-Chieh Chiou(TA)
#


 #  load the fda package

   install.packages("fda")
   library(fda)

#  display the data files associated with the fda package

   data(package='fda')


   #  -----------------------------------------------------------------------
   #  Example 1   Regression Splines: Smoothing by Regression Analysis
   #              Smoothing the growth data
   #  -----------------------------------------------------------------------      
   
   #  define the range of the ages and set up a fine mesh of ages
   
   ageRng  = c(1,18)
   age     = growth$age
   agefine = seq(1,18,len=501)
   
   #  set up order 6 spline basis with 12 basis functions for
   #  fitting the growth data so as to estimate acceleration
   
   nbasis = 12;
   norder =  6;
   heightbasis12 = create.bspline.basis(ageRng, nbasis, norder)
   
   #  fit the data by least squares
   
   basismat   = eval.basis(age, heightbasis12)
   heightmat  = growth$hgtf
   heightcoef = lsfit(basismat, heightmat, intercept=FALSE)$coef
   
   #  fit the data using function smooth_basis, which does the same thing.
   
   heightList = smooth.basis(age, heightmat, heightbasis12)
   heightfd   = heightList$fd
   height.df  = heightList$df
   height.gcv = heightList$gcv
   
   heightbasismat = eval.basis(age, heightbasis12)
   y2cMap         = solve(crossprod(heightbasismat),
                          t(heightbasismat))
   
   tt = matrix(rep(age,54),nrow = 31)
   # par(mfcol = c(2,2));
   plot(tt, heightmat,xlab = "Age",ylab="Height")
   
   #plot(tt, heightmat,type = "l")
   plot(heightList,xlab = "Age",ylab="Height")   
   

   
#  -----------------------------------------------------------------------
#  Example 2   Canadian Weather Data (Canadian average annual weather cycle)
#             log precipitation data
#  -----------------------------------------------------------------------   
   
   CanadianWeather 

#  ----------   Part 1. Some Functional Descriptive Statistics  ----------------
   
#  Statistics for the log precipitation data
#  Using 'logprec.fd' as follows: 
  
#  Step 1: Organize data to have winter in the center of the plot

  # CanadianWeather$dailyAv
  # dayOfYearShifted
  
   logprecav = CanadianWeather$dailyAv[dayOfYearShifted, , 'log10precip']

#  Step 2: Pre-smoothing step
   
#  2-1: Set up a saturated basis: as many basis functions as observations

   yearRng  = c(0,365)
   daybasis = create.fourier.basis(yearRng, 365)

#  2-2: Define the harmonic acceleration operator

   Lcoef        = c(0,(2*pi/diff(yearRng))^2,0)
   harmaccelLfd = vec2Lfd(Lcoef, yearRng)

#  2-3: Smooth data with lambda that minimizes GCV  (spline smoothing)

   lambda     = 1e6
   fdParobj   = fdPar(daybasis, harmaccelLfd, lambda)
   logprec.fd = smooth.basis(day.5, logprecav, fdParobj)$fd
   
   
#  Step 3: Caculate descriptive Statistics

#  3-1: Elementary pointwise mean and standard deviation
   
   meanlogprec   = mean.fd(logprec.fd)
   stddevlogprec = std.fd(logprec.fd)
   
   par(mfrow = c(1, 2))
   plot(meanlogprec, xlab="Day (July 1 to June 30)", ylab="Mean(log10 precip)")
   plot(stddevlogprec, xlab="Day (July 1 to June 30)", ylab="SD(log10 precip)")

# 3-2: The Bivariate Covariance Function v(s; t)

  logprecvar.bifd = var.fd(logprec.fd)

  weektime        = seq(0,365,length=53)
  logprecvar_mat  = eval.bifd(weektime, weektime,
                            logprecvar.bifd)

# 3-3: Plotting covariance function

  persp(weektime, weektime, logprecvar_mat,
        theta=-45, phi=25, r=3, expand = 0.5,
        ticktype='detailed',
        xlab="Day (July 1 to June 30)",
        ylab="Day (July 1 to June 30)",
        zlab="variance(log10 precip)")

  contour(weektime, weektime, logprecvar_mat,
          xlab="Day (July 1 to June 30)",
          ylab="Day (July 1 to June 30)")

   
# --------  Part 2. Exploring Variation: Functional Principal Analysis --------
  

  
#  Step 1: Do PCA with 2 components
  
   nharm = 2
   logprec.pcalist = pca.fd(logprec.fd, nharm)
  
   print(logprec.pcalist$values[1:4])
  
#  Step 2: Plotting the first two eigenfunctions
   
   plot.pca.fd(logprec.pcalist)
  
#  The expansion supplied by the function is too large,
#  and here we supply a smaller value, 0.5
  
   plot(logprec.pcalist, expand=.5)
   
# Step 3: Plotting the scattor plot of the first two FPC socres
   
   pcascores = logprec.pcalist$scores
   
   plot(pcascores[,1], pcascores[,2], type="p", pch="o",
        xlab="PC 1", ylab="PC 2")
   text(pcascores[,1]+0.5, pcascores[,2]+0.2, CanadianWeather$place)
  
# Step 4: Use the VRIMAX rotation algorithm
  
  logprec.rotpcalist = varmx.pca.fd(logprec.pcalist)
  
  plot.pca.fd(logprec.rotpcalist, expand=.5)
  
  rotpcascores = logprec.rotpcalist$scores
  
  plot(rotpcascores[,1], rotpcascores[,2], type="p", pch="o",
       xlab="Rotated PC 1", ylab="Rotated PC 2")
  
  text(rotpcascores[,1]+0.5, rotpcascores[,2]+0.2, CanadianWeather$place) # 特徵越像越集中
  
  
  #  -----------------------------------------------------------------------
  #  Example 3    Pinch force data
  #  -----------------------------------------------------------------------
  
  #  ------------------  input the data  --------------------
  
  pinchmat   <- pinch
  pinchtime  <- seq(0,150,len=151)/600
  pinchrange <- c(0,0.25)
  
  #  -----------  create fd object   --------------------
  #         use 31 bsplines of order 6
  
  nbasis <- 153
  norder <-   4
  pinchbasis <- create.bspline.basis(pinchrange, nbasis, norder)
  
  lambda <- 1e-6
  pinchfdPar <- fdPar(pinchbasis, 2, lambda)
  
  pinchfd <- smooth.basis(pinchtime, pinchmat, pinchfdPar)$fd
  names(pinchfd$fdnames) <- c("Arg. No.", "Replicates", "Force (N)")
  
  #  plot all the curves
  
  par(mfrow=c(1,1),pty="m")
  plot(pinchfd)
  title("Pinch Force Curves")
  
  #  plot each curve along with the data
  
  plotfit.fd(pinchmat, pinchtime, pinchfd)
  
  #  plot the residuals, with cases sorted by size of mean squared residuals
  
  plotfit.fd(pinchmat, pinchtime, pinchfd, residual=TRUE, sort=TRUE)
  
  #  ---------------  do a PCA (light smoothing, varimax rotation)  --------
  
  lambda      <- 1e-4
  pcafdPar    <- fdPar(pinchbasis, 2, lambda)
  
  pinchpca.fd <- pca.fd(pinchfd, nharm=3, pcafdPar)
  
  pinchpca.fd <- varmx.pca.fd(pinchpca.fd)
  
  plot.pca.fd(pinchpca.fd)
  
  pincheigvals <- pinchpca.fd[[2]]
  pincheigvals

  