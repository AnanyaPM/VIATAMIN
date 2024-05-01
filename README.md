# VIATAMIN

# Introduction
Maintenance therapy (MT) is the final stage in acute lymphoblastic leukemia treatment. This phase lasts 2 years and requires consistent assessment of blood counts and clinical features, and subsequent dose adjustment of drugs 6 mercaptoppurine (6MP) and methotrexate (MTX) every 1-3 weeks depending on the protocol. Globally, a practice of "treat of tolerance" has been adopted to allow for individualised optimal treatment. [1-4]

***VIATAMIN*** is a Shiny R-based app for visualisation and analysis of maintenance therapy (MT) data in acute lymphoblastic leukaemia.
Based on the functions developed using the *allMT* R package (CRAN) [5], this app allows for simple and quick visual and numeric analysis of MT data for an individual patient or a cohort of patients at any point during treatment.

# Accession
app URL: https://ananyam.shinyapps.io/VIATAMIN/

# Usage
Following the upload of MT data files, users are provided with a range of analysis functions either for an individual patient or a cohort of patients. These functions are based on user-input criteria, dependent on the respective treatment protocols. 

FUNCTIONS:

**For an individual patient**
1. To track patient’s MT progression during or after therapy.

2. To track patient’s cycle-by-cycle MT progression during or after the therapy using summary measure (SM) weighted mean (6MP*MTX) vs weighted mean ANC parameters in a scatterplot.

3. Asses hematological toxicities - neutropenia, thrombocytopenia and anemia during MT.

4. Evaluate median time to first 6MP dose increase for the cohort.

5. Evaluate real time dosing decisions (stop, reduce or increase) by the physicians during MT based on MT dosing guidelines [2]

**For a cohort of patients**

1. SM plot to analyze MT for a given cohort defined by the user. SM is evaluated for each patient and plotted together to represent the entire cohort.

2. Compare SM for two or more cohort defined by the user to evaluate MT practice. Cohort comparisons can include clinical interventions, year-wise evolution, patients treated by two different set of physicians, or as user see it fit.

3. Asses hematological toxicities - neutropenia, thrombocytopenia and anemia during MT.

4. Plot depicting median time to first 6MP dose increase for the cohort.

5. Evaluate real time dosing decisions (stop, reduce or increase) by the physicians during MT based on MT dosing guidelines [2]

*Please refer to in-app reference manual for detailed instructions and explanations*

# Authorship
Ananya Mahadevan, Dr. Tushar Mungle and Dr. Shekhar Krishnan. 

# References
[1] K. Schmiegelow, S. N. Nielsen, T. L. Frandsen, and J. Nersting, “Mercaptopurine/methotrexate maintenance therapy of childhood acute lymphoblastic leukemia: Clinical facts and fiction,” Journal of pediatric hematology/oncology, vol. 36, no. 7, p. 503, 2014.

[2] T. D. Mungle, “Modelling clinical decision processes to optimise maintenance chemotherapy in children with acute lymphoblastic leukaemia,” PhD thesis, IIT Kharagpur, 2020.

[3] N. Das et al., “Protocol for ICiCLe-ALL-14 (InPOG-ALL-15-01): A prospective, risk stratified, randomised, multicentre, open label, controlled therapeutic trial for newly diagnosed childhood acute lymphoblastic leukaemia in india,” Trials, vol. 23, no. 1, pp. 1–20, 2022.

[4] Krishnan S, Mahadevan A, Mungle T, Gogoi MP, Saha V. Maintenance Treatment in Acute Lymphoblastic Leukemia: A Clinical Primer. Indian J Pediatr. 2024 Jan;91(1):47-58. doi: 10.1007/s12098-023-04687-6.

[5] Mungle T, Mahadevan A, Krishnan S (2023). _allMT: Acute Lymphoblastic Leukemia Maintenance Therapy Analysis_. R package version 0.1.0,
  <https://CRAN.R-project.org/package=allMT>.
