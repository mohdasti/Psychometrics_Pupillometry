# Running Scripts Manually in RStudio

This document provides the complete list of all analysis scripts in execution order. Run each script individually in RStudio by sourcing it from the repository root.

**Important:** Set your working directory to the repository root first:
```r
setwd("/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry")
```

---

## Stage 1: Data Preparation

### Script 1: Load and Validate Data
```r
source("01_data_preparation/01_load_and_validate_data.R")
```
**Purpose:** Load pre-merged trial-level data and validate structure  
**Input:** `data/processed/ch2_triallevel_merged.csv`  
**Output:** Validated dataset (saved back to same file)

---

### Script 2: Compute Psychometric Function Parameters
```r
source("01_data_preparation/02_compute_pf_parameters.R")
```
**Purpose:** Fit psychometric functions or load existing PF parameters  
**Input:** `data/processed/ch2_triallevel_merged.csv`  
**Output:** `data/processed/ch2_pf_parameters.csv`

---

## Stage 2: Pupil Quality Control

### Script 3: Define Pupil Quality Tiers
```r
source("02_pupil_quality_control/03_pupil_quality_tiers.R")
```
**Purpose:** Define quality tiers and compute within-subject centered pupil metrics  
**Input:** `data/processed/ch2_triallevel_merged.csv`  
**Output:** Updated dataset with quality tier flags and centered pupil metrics

---

### Script 5: Missingness Diagnostic
```r
source("02_pupil_quality_control/05_missingness_diagnostic.R")
```
**Purpose:** Model pupil data missingness to test for systematic bias  
**Input:** `data/processed/ch2_triallevel_merged.csv`  
**Output:** 
- `data/qc/missingness_diagnostic.csv`
- `06_visualization/output/tables/missingness_diagnostic_effects.csv`
- `06_visualization/output/models/missingness_model.rds`

---

## Stage 3: Effort Manipulation Check

### Script 4: Effort-Pupil Manipulation Check
```r
source("03_effort_manipulation_check/04_effort_pupil_manipulation_check.R")
```
**Purpose:** Test whether High effort increases pupil metrics relative to Low effort  
**Input:** `data/processed/ch2_triallevel_merged.csv` (with quality tiers)  
**Output:**
- `06_visualization/output/tables/effort_total_auc_effects.csv`
- `06_visualization/output/tables/effort_cog_auc_effects.csv`
- `06_visualization/output/figures/effort_manipulation_total_auc.png`
- `06_visualization/output/figures/effort_manipulation_cog_auc.png`
- `06_visualization/output/models/effort_total_auc_model.rds`
- `06_visualization/output/models/effort_cog_auc_model.rds`

---

## Stage 4: Primary Analysis - Pupil-Psychometric Coupling

### Script 6: Pupil-Psychometric Coupling (PRIMARY ANALYSIS)
```r
source("04_pupil_psychometric_coupling/06_pupil_psychometric_coupling.R")
```
**Purpose:** Fit hierarchical GLMM linking phasic arousal to psychometric sensitivity  
**Key test:** Does `stimulus_intensity × pupil_cognitive_state` interaction predict choice?  
**Input:** `data/processed/ch2_triallevel_merged.csv` (with quality tiers and centered pupil metrics)  
**Output:**
- `06_visualization/output/tables/pupil_psychometric_primary_effects.csv`
- `06_visualization/output/tables/pupil_psychometric_lenient_effects.csv`
- `06_visualization/output/tables/pupil_psychometric_strict_effects.csv`
- `06_visualization/output/figures/psychometric_by_pupil_state.png`
- `06_visualization/output/figures/pupil_psychometric_interaction.png`
- `06_visualization/output/models/pupil_psychometric_primary_model.rds`
- `06_visualization/output/models/pupil_psychometric_lenient_model.rds`
- `06_visualization/output/models/pupil_psychometric_strict_model.rds`

---

## Stage 5: Subject-Level Analysis

### Script 7: Subject-Level PF-Pupil Coupling
```r
source("05_subject_level_analysis/07_pf_pupil_subject_coupling.R")
```
**Purpose:** Correlate subject-level changes in pupil metrics with changes in PF parameters  
**Input:** 
- `data/processed/ch2_triallevel_merged.csv`
- `data/processed/ch2_pf_parameters.csv`
**Output:**
- `06_visualization/output/tables/pf_pupil_coupling_correlations.csv`
- `06_visualization/output/figures/pf_pupil_coupling_threshold.png`
- `06_visualization/output/figures/pf_pupil_coupling_slope.png`
- `06_visualization/output/figures/pf_pupil_coupling_matrix.png`

---

## Stage 6: Visualization

### Script 8: Generate All Figures
```r
source("06_visualization/08_generate_figures.R")
```
**Purpose:** Create publication-ready figures integrating all analyses  
**Input:** Results from previous scripts (models, processed data)  
**Output:**
- `06_visualization/output/figures/fig1_psychometric_by_effort.png`
- `06_visualization/output/figures/fig2_effort_pupil_manipulation.png`
- `06_visualization/output/figures/fig3_psychometric_by_pupil_state.png`
- `06_visualization/output/figures/fig4_missingness_diagnostic.png`

---

## Complete Execution Order

Copy and paste this entire block into RStudio to run all scripts in order:

```r
# Set working directory to repository root
setwd("/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry")

# Stage 1: Data Preparation
source("01_data_preparation/01_load_and_validate_data.R")
source("01_data_preparation/02_compute_pf_parameters.R")

# Stage 2: Pupil Quality Control
source("02_pupil_quality_control/03_pupil_quality_tiers.R")
source("02_pupil_quality_control/05_missingness_diagnostic.R")

# Stage 3: Effort Manipulation Check
source("03_effort_manipulation_check/04_effort_pupil_manipulation_check.R")

# Stage 4: Primary Analysis
source("04_pupil_psychometric_coupling/06_pupil_psychometric_coupling.R")

# Stage 5: Subject-Level Analysis
source("05_subject_level_analysis/07_pf_pupil_subject_coupling.R")

# Stage 6: Visualization
source("06_visualization/08_generate_figures.R")
```

---

## Optional: Data Generation Script

If you need to regenerate pupil data:

```r
source("01_data_preparation/make_quick_share_v7.R")
```

**Note:** This script requires additional data sources and configuration. See `data/processed/README_data_source.md` for details.

---

## After Running All Scripts

1. **Render the Quarto report:**
   ```r
   quarto::quarto_render("07_manuscript/chapter2/chapter2_dissertation.qmd")
   ```

2. **Check outputs:**
   - Figures: `06_visualization/output/figures/`
   - Tables: `06_visualization/output/tables/`
   - Models: `06_visualization/output/models/`
   - QC files: `data/qc/`

---

## Troubleshooting

**If a script fails:**
1. Check that previous scripts have completed successfully
2. Verify required input files exist in `data/processed/`
3. Check the script output for specific error messages
4. Ensure all required R packages are installed

**Path errors:**
- Make sure you're running from the repository root
- All scripts automatically load `config/paths_config.R` for paths



