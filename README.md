# Modelling cost-effectivenss of LAAB-PrEP vs. shielding 
Modelling for cost effectiveness of shielding and LAAB-PrEP amongst immunocompromised people
* Intervention group: LAAB-PrEP + different % of shielding
* Comparator group:100% shielding 
## Cost-effectivness analysis based on sensitivity analysis
The cost-effectivness analysis for the intervention group 
 * inputs: 'LAAB_PrEP_template.csv'
 * outputs: 'LAAB_PrEP_outputs.csv'
 * scripts: 'LAAB_PrEP_script.R'

## Scenario analysis
 * inputs:
   + Low fatigue and low replacement scenario: 'LAAB_PrEP_template_s1.csv'
   + Low fatigue and high replacement scenario: 'LAAB_PrEP_template_s2.csv'
   + High fatigue scenario:'LAAB_PrEP_template_s3.csv'
 * outputs:
   + Low fatigue and low replacement scenario: 'LAAB_PrEP_outputs_s1.csv'
   + Low fatigue and high replacement scenario: 'LAAB_PrEP_outputs_s2.csv'
   + High fatigue scenario:'LAAB_PrEP_outputs_s3.csv'
 * scripts:'LAAB_PrEP_script.R'
   
## Important parameters analysis
 * inputs:
   + time horizon is 2 years: 'LAAB_PrEP_template_th2.csv'
   + time horizon is 5 years: 'LAAB_PrEP_template_th5.csv'
   + time horizon is 10 years:'LAAB_PrEP_template_th10.csv'
 * outputs:
   + time horizon is 2 years: 'LAAB_PrEP_outputs_th2.csv'
   + time horizon is 5 years: 'LAAB_PrEP_outputs_th5.csv'
   + time horizon is 10 years:'LAAB_PrEP_outputs_th10.csv'
 * scripts:'LAAB_PrEP_script.R'

## Impact of incidence of hospitalisation
* inputs:
   + time horizon is 2 years: 'LAAB_PrEP_template.csv'
 * outputs:
   + time horizon is 10 years:'LAAB_PrEP_outputs.csv'
 * scripts:'LAAB_PrEP_script.R'
   
## Individual risk group analysis
* inputs:
   + advanced HIV group: 'LAAB_PrEP_template_advancedHIV.csv'
   + end stage cancer: 'LAAB_PrEP_template_endstage.csv'
   + organ transplant:'LAAB_PrEP_template_organtrans.csv'
   + high-dose, long-term moderate-dose corticosteroids: 'LAAB_PrEP_template_highdose.csv'
   + end-stage kidney disease: 'LAAB_PrEP_template_endstage.csv'
   + primary immunodeficiency: 'LAAB_PrEP_template_primaryimmu.csv'
   + secondary immunodeficiency: 'LAAB_PrEP_template_secondaryimmo.csv'
   + solid tumour ≤5 years prior: 'LAAB_PrEP_template_solidtum.csv'
 * outputs:
   + advanced HIV group: 'CEA_OUTPUTS_advancedHIV.csv'
   + end stage cancer: 'CEA_OUTPUTS_endstage.csv'
   + organ transplant:'CEA_OUTPUTS_organtrans.csv'
   + high-dose, long-term moderate-dose corticosteroids: 'CEA_OUTPUTS.csv'
   + end-stage kidney disease: 'CEA_OUTPUTS.csv'
   + primary immunodeficiency: 'CEA_OUTPUTS.csv'
   + secondary immunodeficiency: 'CEA_OUTPUTS.csv'
   + solid tumour ≤5 years prior: 'CEA_OUTPUTS.csv'
 * scripts:'LAAB_PrEP_script.R'

## Plot
 * scripts:'plotting_without_running_models.R'


Ref: https://www.thelancet.com/journals/lanepe/article/PIIS2666-7762(23)00166-7/fulltext#tbl2
