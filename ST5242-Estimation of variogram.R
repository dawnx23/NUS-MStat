
#--------------------------------------------------------------------
#
#   variogram.R (sgeostat package)
#
#--------------------------------------------------------------------


#--------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------

library(sgeostat)
library(gstat)


#--------------------------------------------------------------------
# Load the Dataset
#--------------------------------------------------------------------

# maas:Zinc measurements as groundwater quality variable
data(maas)
# maas.bank:Coordinates of maas bank
data(maas.bank)
# Or use the dataset: meuse
# This dataset gives locations and topsoil heavy metal concentrations
# Along with a number of soil and landscape variables at the observation locations
# maas is part of meuse
data(meuse)

#dataset<-maas
dataset<-meuse


#--------------------------------------------------------------------
# dataset.point
#--------------------------------------------------------------------


# Create an object of class point from a data frame
dataset.point <- point(dataset)


# Print descriptive information about a point object
print.point(dataset.point)
# Plot the spatial locations in a point object
# Given according to the location information in the dataset, namely, "x" and "y"
plot(dataset.point)

# Optionally coloring by quantile
# We can also change the species of quantile, such as 'cadmium','copper','lead'
plot(dataset.point,v='zinc')
#plot(dataset.point,v='cadmium')
#plot(dataset.point,v='copper')
#plot(dataset.point,v='lead')

# Add a legend to the plot
plot(dataset.point,v='zinc',xlab='easting',ylab='northing',axes=TRUE,legend.pos=4)
# Plot additionally the dataset bank:
lines(maas.bank)

#--------------------------------------------------------------------
# dataset.pair
#--------------------------------------------------------------------

# Create a pair object from a point object
dataset.pair <- pair(dataset.point,num.lags=10,maxdist=2000)

# For an anisotropic pair object
dataset.pair25 <- pair(dataset.point,num.lags=10,type='anisotropic',
                    theta=25,maxdist=500)

# Print the pairs
print(dataset.pair)

# theta:an angle, measured in degrees from the horizontal x axis
# dtheta:a tolerance angle, around theta
print(dataset.pair25)
# For an anisotropic object, a pair of points will be included 
# when the angle between the positive x axis and the vector formed by the pair of points
# falls within the tolerance angle given by (theta-dtheta,theta+dtheta)


#--------------------------------------------------------------------
# Empirical Variogram Estimates
#--------------------------------------------------------------------


# Calculate empirical variogram estimates
# Here we use the 'zinc' data for example, we can also use data of other metal
dataset.v<-est.variogram(dataset.point,dataset.pair,'zinc')
#dataset.v<-est.variogram(dataset.point,dataset.pair,'cadmium')
#dataset.v<-est.variogram(dataset.point,dataset.pair,'copper')
#dataset.v<-est.variogram(dataset.point,dataset.pair,'lead')
dataset.v


#--------------------------------------------------------------------
# Fit Variogram Models
#--------------------------------------------------------------------

# Fit variogram models to empirical variogram estimates
# Automatic fit:
# c0: initial estimate for nugget effect; cg,ag,...: initial estimates
dataset.vmod<-fit.gaussian(dataset.v,c0=60000,cg=110000,ag=800,plot.it=TRUE,
                        type='c',iterations=30)
# For more points, we can adjust the num.lags in the pair function.
dataset.pair <- pair(dataset.point,num.lags=80,maxdist=4000)

# An iterative, Gauss-Newton fitting algorithm is used to fit the model here
# iterations=0, means no fit, intended for "subjective" fit
# Thus parameter values from external sources can be plugged into a variogram model object.

# Other variogram models can be also used
dataset.vmod.e<-fit.exponential(dataset.v,c0=60000,ce=110000,ae=800,plot.it=TRUE,
                           type='c',iterations=0)
dataset.vmod.s<-fit.spherical(dataset.v,c0=60000,cs=110000,as=800,plot.it=TRUE,
                              type='c',iterations=10)
dataset.vmod.w<-fit.wave(dataset.v,c0=60000,cw=110000,aw=800,plot.it=TRUE,
                            type='c',iterations=0)
dataset.vmod.l<-fit.linear(dataset.v, type='c', plot.it=TRUE,iterations=1)

# "type" here decides what kind of empirical vraiogram estimate we use here
# 'c' for classic, 'r' for robust, 'm' for median

# Plot empirical variogram estimates
# Two plots
oldpar <- par(mfrow=c(1,2))
plot(dataset.v, type='c')
plot(dataset.v,var.mod.obj=dataset.vmod)
par(oldpar)



#--------------------------------------------------------------------
#
#   variogram.R (npsp package)
#
#--------------------------------------------------------------------


#--------------------------------------------------------------------
# Load packages
#--------------------------------------------------------------------

library(geoR)
library(npsp)

#--------------------------------------------------------------------
# Local linear variogram
#--------------------------------------------------------------------

# Stationary data (data(s100) geoR)
summary(s100)
plot(s100)
# s100 is a simulated dataset, not a real data

# Empirical variogram
vario.geor <- variog(s100, max.dist=0.6) # geoR variog()
str(vario.geor)

# Local polynomial estimation of the semivariogram
# Using local polynomial kernel smoothing 
svarnp <- np.svariso(s100$coords, s100$data, h = 0.2, maxlag = 0.6)
str(svarnp)

# Graphical comparison
#oldpar <- par(mfrow=c(1,2))
plot(vario.geor,main="Empirical semivariogram")
plot(svarnp, main = "Nonparametric semivariogram")
#par(oldpar) 


#--------------------------------------------------------------------
# Bandwidth selection
#--------------------------------------------------------------------

# Discretizes the data into a regular grid (computes a binned approximation) using 
# the multivariate linear binning technique
bin <- svar.bin(s100$coords, s100$data, set.NA = TRUE)
# Selects the bandwidth of a local polynomial kernel 
# using (standart or modified) CV, GCV or MASE criteria.
# use binning approximations to the objective function values
hcv <- h.cv(bin, ncv = 2)
# ncv: 0 to GCV considering all the data, > 0 to traditional or modified cross-validation
# Setting ncv >= 2 would be recommended for sparse data
# estimates are computed by leaving out binning cells with
# indexes within the intervals [xi-ncv+1, xi+ncv-1]
hcv
# "ASE" Averaged squared error
# "ARSE" Averaged relative squared error
# "AAE" Averaged absolute error
# "ARAE" Averaged relative absolute error

svarnp1 <- np.svariso(s100$coords, s100$data, h = hcv$h, maxlag = 0.6)
plot(svarnp1, main = "Nonparametric semivariogram")


#--------------------------------------------------------------------
# Fitting Parametric Model
#--------------------------------------------------------------------

# To convert a semivariogram estimate to an object of the geoR-class variogram
svar.geor <- as.variogram(svarnp1)

# Estimate covariance parameters by fitting a parametric model to a empirical variogram
# ini: Initial values for the covariance parameters: sigma^2 (partial sill) and phi (range parameter)
# ordinary least squares
vario.ols <- variofit(svar.geor, cov.model='exponential', ini = c(1, 0.5), weights = "equal")  
vario.ols
# weighted least squares
vario.wls <- variofit(svar.geor, ini = c(1, 0.5), weights = "cressie")  
vario.wls
# The different options for 'weights' are used to define the loss function to be minimised

# Available choices for "cov.model" are:
# "matern", "exponential", "gaussian", "spherical", "circular", "cubic", "wave", "power", 
# "powered.exponential", "cauchy", "gencauchy", "gneiting", "gneiting.matern","pure.nugget"
# Default exponential

plot(svar.geor, main = "Nonparametric estimates and fitted models")
lines(vario.ols, lty = 1, max.dist = 1)
lines(vario.wls, lty = 2, max.dist = 1)
legend(0.3, 0.3, legend = c("OLS","WLS"), lty = c(1, 2))

