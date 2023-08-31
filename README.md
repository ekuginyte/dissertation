## Variable Selection in High-Dimensional Data within the Bayesian Framework 

Final thesis as part of the Master's in Applied Statistics and Data Mining from University of St Andrews 

### Abstract

This study examines challenges inherent in variable selection for linear models within the Bayesian framework, particularly in high-dimensional settings using R packages. ’High-dimensionality’ refers to contexts involving many predictors that are fewer, equal to, or greater than the number of data points. Frequentist penalised regression Lasso and elastic-net methods and the machine learning XGBoost method are initially used to establish a benchmark. Subsequent Bayesian approaches encompass Bayesian Lasso, spike-and-slab prior, spike-and-slab Lasso, horseshoe priors, and the simplified shotgun stochastic search with screening. Packages that share methodologies are juxtaposed for user-friendliness and implementation nuances. Simulated data scenarios reveal that Bayesian methods, which compute coefficients credible intervals, often outperform frequentist counterparts by adeptly integrating model uncertainty into feature selection. However, they may miss weak signals when predictors significantly outnumber data points. Methods based on point estimates tend to retain irrelevant variables, while the machine learning method underperforms in all settings. Analysing socio-economic crime data largely aligns with the synthetic data study outcomes.

For the full report, please refer to the PDF file.
    
