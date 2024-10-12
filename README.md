# Modelling cost-effectivenss of LAAB-PrEP vs. shielding 
Modelling for cost effectiveness of shielding and LAAB-PrEP amongst immunocompromised people
* Intervention group: LAAB-PrEP + certain % of shielding
* Comparator group: shielding 
## Cost-effectivness analysis 
We randomly sampled all parameters from big ranges listed in the template and generated a big matrix containing the corresponding inputs and economic & life-year outputs. We then plotted the cost-effectivness plane and willingness-to-pay curve where cost of shielding is not considered as shown in Fig 1 and where cost of shielding is considered in Fig S4. 
 * inputs: 'LAAB_PrEP_template.csv'
 * outputs: 'LAAB_PrEP_outputsnew.csv'
 * scripts: 'script_Fig1.R'
   
## Important parameters analysis
We performed a one-way sensitivity analysis, two-way sensitivity analysis, multi-way sensitivity for incremental cost, incremental QALY and ICER when cost of shielding is considered and is not considered, respectively. We screened out the top 5 parameters in each of the three time horizons studied, e.g., 2 years, 5 years, and 10 years respectively. 

One-way sensitivity analysis
 * outputs:
   + time horizon is 2/5/10 years: 'LAAB_PrEP_template.csv'
 * outputs:
   + time horizon is 2 years: 'dataICER_min2_nocosta.rds', 'dataICER_max2_nocosta.rds', 'dataICER_mean2_nocosta.rds', 'dataCOST_min2_nocosta.rds', 'dataCOST_max2_nocosta.rds', 'dataCOST_mean10_nocosta.rds'
   + time horizon is 5 years: 'dataICER_min5_nocosta.rds', 'dataICER_max5_nocosta.rds', 'dataICER_mean5_nocosta.rds', 'dataCOST_min5_nocosta.rds', 'dataCOST_max5_nocosta.rds', 'dataCOST_mean5_nocosta.rds'
   + time horizon is 10 years: 'dataICER_min10_nocosta.rds', 'dataICER_max10_nocosta.rds', 'dataICER_mean10_nocosta.rds', 'dataCOST_min10_nocosta.rds', 'dataCOST_max10_nocosta.rds', 'dataCOST_mean10_nocosta.rds'
 * scripts:'tornado_plot_nocost.R' & 'script_Fig2.R' & 'tornado_plot_withcost.R'

Two-way sensitivity analysis
 * outputs:
   + time horizon is 2/5/10 years: 'LAAB_PrEP_template.csv'
 * outputs:
   + time horizon is 2 years: 'dataICER_min2_nocosta.rds', 'dataICER_max2_nocosta.rds', 'dataICER_mean2_nocosta.rds', 'dataCOST_min2_nocosta.rds', 'dataCOST_max2_nocosta.rds', 'dataCOST_mean10_nocosta.rds'
   + time horizon is 5 years: 'dataICER_min5_nocosta.rds', 'dataICER_max5_nocosta.rds', 'dataICER_mean5_nocosta.rds', 'dataCOST_min5_nocosta.rds', 'dataCOST_max5_nocosta.rds', 'dataCOST_mean5_nocosta.rds'
   + time horizon is 10 years: 'dataICER_min10_nocosta.rds', 'dataICER_max10_nocosta.rds', 'dataICER_mean10_nocosta.rds', 'dataCOST_min10_nocosta.rds', 'dataCOST_max10_nocosta.rds', 'dataCOST_mean10_nocosta.rds'
 * scripts:'plot_two_way_effcos.R' & 'script_FigS7.R'

## Scenario analysis
We studied three scenarios defined by $\eta$: shielding fatigue and $\rho$: reduction in shielding due to protecton with LAAB-PrEP as shown in Fig 3a.
 * inputs:
   + Low fatigue and low replacement scenario: 'LAAB_PrEP_template_s1_1.csv' where $\eta \in [0,0.15]$ and $\rho \in [0,0.1]$
   + Low fatigue and high replacement scenario: 'LAAB_PrEP_template_s2_1.csv' where $\eta \in [0,0.1]$ and $\rho \in [0.95,1]$
   + High fatigue scenario:'LAAB_PrEP_template_s3_1.csv' where $\eta \in [0.75,0.85]$
 * outputs:
   + Low fatigue and low replacement scenario: 'LAAB_PrEP_template_s1_1new.csv'
   + Low fatigue and high replacement scenario: 'LAAB_PrEP_outputs_s2_1new.csv'
   + High fatigue scenario:'LAAB_PrEP_outputs_s3_1new.csv'
 * scripts:'script_Fig3.R'
## Impact of incidence of hospitalisation
We post-analysed the modelling outputs from the big sensitivity analysis where all parameters were randdomly sampled from big ranges by plotting the willingness-to-pay curves under different intervals of incidence of hospitalisation as shown in Fig 3c. 
 * inputs:
   + LAAB_PrEP_template.csv'
* outputs:
   + LAAB_PrEP_outputsnew.csv'
 * scripts:'scritp_Fig3.R'

## Impact of time horizon
We conducted a sensitivity analysis for differen time horizon as shown in Fig 3b. 
 * inputs:
   + LAAB_PrEP_template.csv'
 * outputs:
   + LAAB_PrEP_outputs_th2new.csv'
   + LAAB_PrEP_outputs_th5new.csv'
   + LAAB_PrEP_outputs_th10new.csv'
 * scripts:'LAAB_PrEP_th2.R','LAAB_PrEP_th5.R','LAAB_PrEP_th5.R' & 'scritp_Fig3.R'
   
## Individual risk group analysis
We parameterised eight risk groups of immunocompromised people fron INFORM study [1] listed in below templates and run the model and plotted the corresponding willingness-to-pay curves as shown in Fig 3d.  
* inputs:
   + advanced HIV group: 'LAAB_PrEP_template_advancedHIVnew.csv'
   + end stage cancer: 'LAAB_PrEP_template_endstagenew.csv'
   + organ transplant:'LAAB_PrEP_template_organtransnew.csv'
   + high-dose, long-term moderate-dose corticosteroids: 'LAAB_PrEP_template_highdosenew.csv'
   + end-stage kidney disease: 'LAAB_PrEP_template_endstagenew.csv'
   + primary immunodeficiency: 'LAAB_PrEP_template_primaryimmunew.csv'
   + secondary immunodeficiency: 'LAAB_PrEP_template_secondaryimmonew.csv'
   + solid tumour ≤5 years prior: 'LAAB_PrEP_template_solidtumnew.csv'
 * outputs:
   + advanced HIV group: 'CEA_OUTPUTS_advancedHIV.csv'
   + end stage cancer: 'CEA_OUTPUTS_endstage.csv'
   + organ transplant:'CEA_OUTPUTS_organtrans.csv'
   + high-dose, long-term moderate-dose corticosteroids: 'CEA_OUTPUTS.csv'
   + end-stage kidney disease: 'CEA_OUTPUTS.csv'
   + primary immunodeficiency: 'CEA_OUTPUTS.csv'
   + secondary immunodeficiency: 'CEA_OUTPUTS.csv'
   + solid tumour ≤5 years prior: 'CEA_OUTPUTS.csv'
 * scripts:'script_Fig3.R'

## Reference
[1] Evans, Rachael A., et al. "Impact of COVID-19 on immunocompromised populations during the Omicron era: insights from the observational population-based INFORM study." The Lancet Regional Health–Europe 35 (2023).https://www.thelancet.com/journals/lanepe/article/PIIS2666-7762(23)00166-7/fulltext#tbl2](https://www.thelancet.com/journals/lanepe/article/PIIS2666-7762(23)00166-7/fulltext)
