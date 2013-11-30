*WORK IN PROGRESS*

Summaries of testing earthquake predictions 
--------------------------------------------

Intro

Prediction of earthquakes has existed for long. 
Even non-seismic methods have been in use for long
Even experts in geophysics do not agree on possibilities of earthquake prediction 
Earthquake predictions have been assessed using ideas from statistical hypothesis testing: a test statistic is compared to its distribution under a null hypothesis
Stochastic models for earthquake occurrence do not seem like a good foundation for evaluating earthquake predictions, especially predictions of large earthquakes.

Phenomenology of earthquakes

An earthquake catalog is a list of the estimated locations, times, and magnitudes of earthquakes found by a given authority, such as the U.S. Geological Survey.
All large earthquakes occur within a few tens of kilometres of the surface.
Indeed, most large earthquakes occur in a relatively narrow band around the Pacific Ocean, the “ring of fire.”
Earthquakes also cluster in time: large earthquakes invariably have aftershocks; some have foreshocks;
The terms “foreshock” and “after- shock” are often taken to imply a causal connection to a main shock.
The most common stochastic model for seismicity takes the epicenters and times of shocks above some threshold magnitude to be a realization of a spatially inhomogeneous but temporally homogeneous Poisson process.

Here are two categories of earthquake predictions: deterministic or binary predictions and probabilistic predictions.





Test of earthquake predictions 
-------------------------------------------
The fraction of the study volume covered by alarms is v = VA/V .

Several stochastic models for seismicity are common for testing predictions.

1.	Some studies model seismicity by a homogeneous Poisson process with intensity equal to the mean historical rate in the study region 

2.	Some studies use a spatially heterogeneous but temporally homogeneous Poisson process model, with rate in the spatial region Rj equal to the historical rate j 

3.	Some studies condition on the observed locations of past events, but model the times of the events as Poisson or uniform 

4.	Some studies condition on the observed locations and the observed times, but model the times as exchangeable 
Another approach to testing is to compare the success rate of predictions with the (theoretical or empirical) success rate of random predictions that do not use any seismic information

Jackson Method 

eviews methods for testing deterministic and probabilistic predictions. The approach to testing deterministic predictions is based on a probability distribution for the number of successful predictions, in turn derived from a null hypothesis that specifies P(Sj = 1), j = 1,...,A.
Advocate a likelihood-ratio test for evaluating probabilistic forecasts.

Cosnole Method 

Addresses deterministic predictions and probabilistic forecasts. His discussion of deterministic predictions includes several statistics for comparing alternative sets of predictions.
For Console, the null hypothesis is that seismicity has a Poisson distribution. His test rejects the null hypothesis if more events occur during alarms than are expected on the assumption that seismicity has a homogeneous Poisson distribution with true rate equal to the observed rate.

Shi, Liu & Zhang

R score measures the concordance of the binned data with predictions of occurrence and of non-occurrence. Their hypothesis tests use the R-score as the test statistic. They compare the R-score of the actual predictions on the declustered catalog with the R-score of several sets of random predictions.

Some claims of successful predictions
--------------------------------------------
Wyss and Burford

Wyss and Burford claim to have predicted the earth-quake about a year before it occurred, using “seismic quiescence,” an anomalous paucity of earthquakes over some period of time. Their null hy- pothesis is that seismicity follows a homogeneous Poisson process with rate equal to the historical rate; clustering is not taken into account.

VAN predictions based on Seismic Electrical Signals

Regarding predictions made by Varotsos, Alexopoulos and Nomicos (VAN) , participants disagreed about whether the predictions were too vague to be considered predictions, whether some aspects of the predictions were adjusted post hoc, what the null hy- pothesis should be, and what tests were appropriate.

Kossobokov et al.,

Their predictions are based on two algorithms, M8 and MSc, which track the running mean of the number of main shocks; the difference between the cumulative number of main shocks and a windowed trend in the number of main shocks and the largest number of aftershocks of any event in a temporal window.

A naive predictor
------------------------------------
We exploit the empirical clustering of earthquakes in time to construct a predictor that succeeds far beyond chance according to tests that hold predictions fixed and treat seismicity as random. If the locations, magnitudes, and times of the events in the catalog are {(rj,Mj,tj)}Qj=1, we take the Q! outcomes {(rj,Mj,tpi(j))}Qj=1. We do not claim that this predictor or this null hypothesis is good. Rather, we claim that this approach to testing is misleading.

We apply the approach to the Global Centroid Moment Tensor (CMT) catalog for 2004 and for 2000–2004. We make two sets of predictions:

(i) After each earthquake of (body-wave) magnitude MT or greater, predict that there will be another earthquake of magnitude MT or greater within 21 days, and within 50 km epicentral distance.

(ii) After each earthquake of magnitude MT or greater, predict that there will be an earthquake within 21 days and within 50 km that is at least as large as any within 50 km within 21 days prior to its occurrence.

Predictor (i) tends to predict more aftershocks: large events trigger alarms that contain some of their aftershocks. Predictor (ii) prevents aftershocks from being predicted by main shocks; however, it does not prevent aftershocks with magnitude MT or larger from predicting still larger aftershocks of the same main event.

Predictors (i) and (ii) generate the same number of alarms and have the same total duration, but not the same extent of space and magnitude. Note that these alarms need not be disjoint.

Under the null hypothesis, both prediction methods (i and ii) succeed well beyond chance for CMT data from the years 2000–2004, for both values of the threshold magnitude. That is not because the prediction method is good; rather, it is because the stochastic model in the null hypothesis fails to take into account the empirical clustering of earthquakes and the dependence of the predictions on the seismicity.

Discussion
-----------------------------

To use a statistical hypothesis test, something must be assumed to be random, and its probability distribution under the null hypothesis must be known. Many studies model seismicity as random under the null hypothesis. That approach has serious drawbacks, and details of the stochastic model, such as spatial heterogeneity, independence or exchangeability, matter for testing. Most null hypotheses used in tests ignore the empirical clustering of earthquakes. It is often assumed that the resulting data represent a realization of a Poisson process, which is implausible. The standard approach to testing hold the predictions fixed while seismicity varies randomly, and the predictions would be different if the seismicity were different. The result is that simple-minded schemes, such as the “automatic alarm strategy,” succeed well beyond chance in hypothesis tests. This is not because the predictions are good: it is because the tests are bad. 


Contributers: 
Sung Hoon Choi, Jinsoo Lee






