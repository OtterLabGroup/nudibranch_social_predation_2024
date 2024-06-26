# Social predation in a nudibranch mollusc (2024)
Kate Otter*, Saida Gamidova & Paul S. Katz

*corresponding author: kotter@umass.edu 

Data and analysis for social feeding paper (doi and citation here)
The code and csv files in this repository can be used to replicate all of the analysis and figures from the above paper. 

## Data
Raw data (behavioral videos) are available upon request.

This folder contains the following csv files:
* lab_data: group sizes measured 20 minutes after a routine feeding
* GF_data_20240403: results from all the group feeding assays (3 & 7-days food-deprived)
* 2AC_choice_data_20240414: results from all 2-alternative choice assays (3 & 7-days food-deprived)
  * blank cells are missing data because it could not be analyzed
  * labels from the paper: asw+anemone = ATW
* 2AC_latency_7_20240507: latency calculations using the 2AC_choice_data_20240414 and a basic R script (contact for example code) 
* SFID_individual_differences_data_20240403: data from the individual differences experiment, with unique identifiers for each animal (Animal_ID) that was tested multiple times

## Analysis
This folder contains the following R scripts:
* PPR_analysis: code for simulations
* PPR_analysis_functions: helper code with the functions used for the simulations

This folder contains an Rmarkdown document that reproduces all statistical analyses from the paper.

This folder also contains output csv files from the modeling scripts to facilitate ease of reproducing figures and analysis:
* FDL3_GF_simulated_data_SDM_20240509: 100,000 simulated datasets of the same size as the 3-days food-deprived GF data using the parameterized social dining model (alpha = 4.063)
* FDL3_GF_simulated_data_equal_20240509: 100,000 simulated datasets of the same size as the 3-days food-deprived GF data with equal probability of each slug selecting an anemone (null model)
* FDL7_GF_simulated_data_equal_20240509: 100,000 simulated datasets of the same size as the 7-days food-deprived GF data with equal probability of each slug selecting an anemone (null model)

## Figure Generation
This folder contains an Rmarkdown document that can be used to replicate all of the figures from the paper.
