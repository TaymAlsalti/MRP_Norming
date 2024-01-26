This is GitHub repository hosting the code and most of the material belonging to the manuscript [Using Multilevel Regression and Poststratification to Efficiently Derive Accurate Norms](https://osf.io/preprints/psyarxiv/fcm3n/). 


# Reproducibility

4 things are required to be able to rerun our code:

1. TwinLife data: Must be requested from [GESIS](https://search.gesis.org/research_data/ZA6701). Alternatively you can run the tutorial code on the synthetic sample we provide on our [OSF project](https://osf.io/2r9sy/).
2. [CFT 20-R, German version norms](https://www.testzentrale.de/shop/grundintelligenztest-skala-2-revision-cft-20-r-mit-wortschatztest-ws-und-zahlenfolgentest-zf-revision-ws-zf-r-90116.html): Means and SDs (Streuung) of "Teil-1 Max" have to be manually extracted from the manual for age groups 11;1-11;6 through 60-64. The Normalised Manual IQ values presented in Table 2 in the manuscript also have to be extracted.
3. The census tables, which we provide on our [OSF project](https://osf.io/2r9sy/).
4. To ensure the code works as expected, it’s best to have the same versions of R (4.2.2) and the packages we use.
Since brms requires the most work to set up, it's best to start there. Follow [this walk-through](https://learnb4ss.github.io/learnB4SS/articles/install-brms.html) for installing `brms` on different operating systems (take care to install the right version of RTools if you're on Windows!). Once you've done that, it's probably safe to install the proper versions of the of rest of the packages by running `renv::restore()`. If `renv::restore()` fails to install any of the packages, try `remotes::install_version()` to install that specific version of the package and run `renv::restore()` again. 


Note that all code in our tutorial document can be executed based on materials we provide on our OSF repository.


# Abstract
By itself, a raw score on a psychological test or measure has little meaning. Usually, individual scores can only be interpreted meaningfully when they are compared against a given reference population - a norm. High-quality test norms are important for accurate individual diagnostics, identifying sampling bias, and calculating standardised effect sizes that can be compared across studies. Traditionally, norms are constructed from random, representative samples from the population. However, it is becoming increasingly difficult to reach representative samples as nonresponse rates soar and traditional sampling methods like random digit dialling fail. Outside of psychology, Multilevel Regression and Poststratification (MRP) is one of the most widely used methods to correct for nonrepresentative sampling. MRP has the potential to make high-quality test norms more accessible to psychologists by substantially reducing costs of sampling. We provide this tutorial to facilitate its wide-scale adoption. MRP involves fitting a regularised prediction model to a large and diverse sample and weighting predicted scores by true population values (sourced from census data, for example). Using IQ test data from the TwinLife study (N = 10,059, Culture Fair Test, CFT 20-R) as an example, we show that MRP leads to IQ scores that differ on average by 4.4 points from those derived through traditional norming. 

