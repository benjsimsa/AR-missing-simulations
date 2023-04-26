
This repository contains the simulation code, full simulation results, as well as the RMarkdown files used for the writeup of my Master's thesis at KU Leuven (topic: **The effect of missing data on the estimation bias, variance, and statistical power in multilevel autoregressive(1) models**; supervisor: Prof. Eva Ceulemans; co-supervisors: Prof. Ginette Lafit, Jordan Revol; 6/2023)

# Folder structure 

├── **code_custom_functions**   # the code with functions used for the simulations  
├── **code_plots_tables**       # Code used to create some of the figures in the paper  
├── **code_simulations**        # Code used for the simulation studies   
│  ├── **simulation_code_simulation_a.Rmd**               # Simulation study A: person-mean centering; no random slopes  
│  ├── **simulation_code_simulation_b_randomslopes.Rmd**  # Simulation study B: person-mean centering; random slopes   
│  ├── **simulation_code_simulation_c_norandomslopes_noncentered.Rmd**    # Simulation study C: no person-mean centering; no random slopes  
│  ├── **simulation_code_simulation_d_randomslopes_noncentered.Rmd**  # Simulation study D: no person-mean centering; random slopes   
├── **results**  
│  ├── **sim_a_norandomslopes**              # Simulation study A: person-mean centering; no random slopes   
│  ├── **sim_b_randomslopes**  # Simulation study B: person-mean centering; random slopes   
│  ├── **sim_c_norandomslopes_noncentered**    # Simulation study C: no person-mean centering; no random slopes   
│  ├── **sim_d_randomslopes_noncentered**  # Simulation study D: no person-mean centering; random slopes   
    # the files in the results folder are structured in the following way:  
    # misstype_realAR_compliance_Nparticipants_Ttimepoints.rds  
    # for example, the file block_03_04_20_20.Rds contains results   
    # from the simulation replicate with block missingness, real simulated fixed AR effect of 0.3, compliance of 0.4, 20 participants   
    # and 20 beeps per participant   
├── **writeup.Rmd**  # The main file used for the main text of the thesis   
└── **README.md**  

