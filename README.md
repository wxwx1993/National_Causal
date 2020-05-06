# National Causal Studies
This is the data repository for public available code and data to reproduce analyses in "Evaluating the Potential Causal Impact of Long-term Exposure to Fine Particulate Matter on Mortality Among the Elderly."

<b>Code: </b><br>
[`data_process.R`](https://github.com/wxwx1993/National_Causal/blob/master/data_process.R) includes the code to extract all necessary data and prepocess data for statistical analyses.

[`statistical_models.R`](https://github.com/wxwx1993/National_Causal/blob/master/statistical_models.R) includes the code to implement all five statistical models, including two tranditional methods (i.e., Cox proportional hazard model and Cox-equvalent conditional Poisson model), and three causal inference methods (i.e., adjustment, weighting and matching by genealized propensity scores).

[`covariate_balance.R`](https://github.com/wxwx1993/National_Causal/blob/master/covariate_balance.R) includes the code to assess covariate balance for two causal inference methods (i.e., weighting and matching by genealized propensity scores).

[`death_saved.R`](https://github.com/wxwx1993/National_Causal/blob/master/death_saved.R) includes the code to calculate the total number of deaths that would be avoided among the elderly per decade if all areas were in compliance with the current World Health Organization (WHO) guidelines (i.e., 10 μg/m3 annual PM2.5 exposure).

[`pm_map.R`](https://github.com/wxwx1993/National_Causal/blob/master/pm_map.R),[`ScienceAdvances_main.R`](https://github.com/wxwx1993/National_Causal/blob/master/ScienceAdvances_main.R),[`ScienceAdvances_supp.R`](https://github.com/wxwx1993/National_Causal/blob/master/ScienceAdvances_supp.R) includes the code to generate figures in Main Text and Supplementary Materials.

[`Bootstrap`](https://github.com/wxwx1993/National_Causal/tree/master/Bootstrap) contains code to calculate the Bootstrapped Confidence Intervals for all five statistical models.

<b>Data: </b><br>
All data needed to evaluate the conclusions in the paper are present in the paper and/or the Supplementary Materials. Medicare paitent individual data are stored at the Level-3 secured data platform on Research Computing Environment, supported by the Institute for Quantitative Social Science in the Faculty of Arts and Sciences at Harvard University. Those interested in the original data can contact the corresponding author.

<b>Contact Us: </b><br>
* Email: fdominic@hsph.harvard.edu

<b>Terms of Use:</b><br>
Authors/funders retain copyright (where applicable) of code on this Github repo and the article. Anyone who wishes to share, reuse, remix, or adapt this material must obtain permission from the corresponding author. By using the contents on this Github repo and the article, you agree to cite our paper.
