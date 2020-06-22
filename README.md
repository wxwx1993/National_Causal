# National Causal Studies
This is the data repository for publicly available code and data to conduct analyses in "Evaluating the Impact of Long-term Exposure to Fine Particulate Matter on Mortality Among the Elderly."

<b>Code: </b><br>
[`data_process.R`](https://github.com/wxwx1993/National_Causal/blob/master/data_process.R) includes the code to extract all necessary data and prepocess data for statistical analyses.

[`statistical_models.R`](https://github.com/wxwx1993/National_Causal/blob/master/statistical_models.R) includes the code to implement all five statistical models, including two tranditional regression methods (i.e., Cox proportional hazard model and Cox-equvalent conditional Poisson model), and three causal inference methods (i.e., adjustment, weighting and matching by genealized propensity score). [`Cox_model.sas`](https://github.com/wxwx1993/National_Causal/blob/master/Cox_model.sas) include the SAS code to implement Cox proportional hazard model that provide a benchmark to the R code.

[`covariate_balance.R`](https://github.com/wxwx1993/National_Causal/blob/master/covariate_balance.R) includes the code to assess covariate balance for two causal inference methods (i.e., weighting and matching by genealized propensity score).

[`death_saved.R`](https://github.com/wxwx1993/National_Causal/blob/master/death_saved.R) includes the code to calculate the total number of deaths that would be avoided among the elderly per decade if all areas were in compliance with the current World Health Organization (WHO) guidelines (i.e., 10 μg/m3 annual PM2.5 exposure).

[`pm_map.R`](https://github.com/wxwx1993/National_Causal/blob/master/pm_map.R),[`ScienceAdvances_main.R`](https://github.com/wxwx1993/National_Causal/blob/master/ScienceAdvances_main.R),[`ScienceAdvances_supp.R`](https://github.com/wxwx1993/National_Causal/blob/master/ScienceAdvances_supp.R) includes the code to generate figures in Main Text and Supplementary Materials.

[`Bootstrap`](https://github.com/wxwx1993/National_Causal/tree/master/Bootstrap) contains the code to calculate the Bootstrapped Confidence Intervals for all five statistical models.

<b>Data: </b><br>
All data needed to evaluate the conclusions in the paper are present in the paper and/or the Supplementary Materials. Medicare patient individual-level data are stored at a Level-3 secured data platform on Research Computing Environment, supported by the Institute for Quantitative Social Science in the Faculty of Arts and Sciences at Harvard University. Those interested in the original data can contact the corresponding author.

<b>Terms of Use:</b><br>
Authors/funders retain copyright (where applicable) of code on this Github repo and the article. Anyone who wishes to share, reuse, remix, or adapt this material must obtain permission from the corresponding author. By using the contents on this Github repo and the article, you agree to cite our paper.

X. Wu, D. Braun, J. Schwartz, M. A. Kioumourtzoglou, F. Dominici, Evaluating the impact of long-term exposure to fine particulate matter on mortality among the elderly. Sci. Adv. 6, eaba5692 (2020).

<b>Contact Us: </b><br>
* Email: fdominic@hsph.harvard.edu
* Email: wuxiao@g.harvard.edu

<b>Acknowledgments</b><br>

We appreciate the work of M. Benjamin Sabath and Yaguang Wei for generating and processing the estimates of PM2.5 exposure. We would like to thank Lena Goodwin for editorial assistance in the preparation of this manuscript.
