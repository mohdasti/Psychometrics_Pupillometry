# Chapter 2 Setup Summary

## ✅ Setup Complete

The Chapter 2 analysis pipeline has been successfully set up with all required components.

## Created Components

### 📁 Directory Structure

```
07_manuscript/chapter2/
├── config/
│   └── paths_config.R              ✅ Centralized path configuration
├── data/
│   ├── raw/
│   │   ├── behavioral/             ✅ Behavioral data files copied
│   │   └── pupil/                  ✅ Directory created
│   ├── processed/
│   │   └── ch2_triallevel_merged.csv  ✅ Merged data file copied
│   └── qc/                         ✅ Quality control directory
├── scripts/
│   ├── 01_load_and_validate_data.R          ✅ Data loading script
│   ├── 02_compute_pf_parameters.R           ✅ PF computation script
│   ├── 03_pupil_quality_tiers.R             ✅ Quality tier script
│   ├── 04_effort_pupil_manipulation_check.R ✅ Manipulation check script
│   ├── 05_missingness_diagnostic.R          ✅ Missingness analysis script
│   ├── 06_pupil_psychometric_coupling.R     ✅ PRIMARY ANALYSIS script
│   ├── 07_pf_pupil_subject_coupling.R       ✅ Subject-level coupling script
│   └── 08_generate_figures.R                ✅ Figure generation script
├── output/
│   ├── figures/                    ✅ Output directory
│   ├── tables/                     ✅ Output directory
│   └── models/                     ✅ Output directory
├── reports/
│   └── chap2_psychometric_pupil.qmd  ✅ Quarto report template
├── run_analysis.R                  ✅ Master execution script
└── README.md                       ✅ Documentation
```

### 📊 Data Files

**Primary processed data:**
- ✅ `data/processed/ch2_triallevel_merged.csv` (from modeling-pupil-DDM repo)
  - Pre-merged trial-level data with behavioral + pupil measures
  - Contains quality flags and pupil metrics

**Raw behavioral data files:**
- ✅ `data/raw/behavioral/bap_beh_trialdata_v2.csv` (6.0M)
  - Trial-level behavioral data (all trials)
  - Columns: subject_id, task_modality, effort, stimulus parameters, choices, RT, etc.
  
- ✅ `data/raw/behavioral/bap_beh_subjxtaskdata_v2.csv` (475K)
  - Subject-level summaries by task
  - Includes LC imaging measures, demographics, etc.

- ✅ `data/raw/behavioral/bap_beh_trialdata_v2_trials_per_subject_per_task.csv` (2.1K)
  - Trial counts per subject per task
  - Useful for QC and sample size checks

**Master spreadsheet files (LC Aging Subject Data):**
- ✅ `data/raw/behavioral/LC Aging Subject Data master spreadsheet - behavioral.csv` (24K)
  - **Important:** Contains PF parameters (thresholds, slopes) by subject, task, and effort condition
  - Includes: aud_thresh, vis_thresh, aud_slope, vis_slope (and Low/High variants)
  - Also includes accuracy measures, grip separability indices, RPF AUC measures
  - **May be used by script 02 as alternative to re-computing PF parameters**

- ✅ `data/raw/behavioral/LC Aging Subject Data master spreadsheet - behavioral data dictionary.csv` (7.2K)
  - Data dictionary explaining behavioral variables
  - Describes variables in the behavioral master spreadsheet

- ✅ `data/raw/behavioral/LC Aging Subject Data master spreadsheet - demographics.csv` (53K)
  - Demographics data (age, sex, education, etc.)
  - Session dates and order information
  - Useful for participant characterization

- ✅ `data/raw/behavioral/LC Aging Subject Data master spreadsheet - neuropsych.csv` (25K)
  - Neuropsychology battery data
  - Includes: MoCA, Word List, Boston Naming, Trail Making, etc.
  - Useful for participant characterization and potential covariates

- ✅ `data/raw/behavioral/bap_beh_trialdata_v2_report.txt` (4.6K)
  - Documentation/report about the behavioral data

### 📝 Scripts Created (8 scripts, ~1657 lines total)

1. **01_load_and_validate_data.R** - Loads and validates pre-merged trial-level data
2. **02_compute_pf_parameters.R** - Fits psychometric functions or loads existing PF parameters
   - **Note:** Can potentially load PF parameters from `LC Aging Subject Data master spreadsheet - behavioral.csv` instead of re-computing
3. **03_pupil_quality_tiers.R** - Defines quality tiers and computes within-subject centered pupil metrics
4. **04_effort_pupil_manipulation_check.R** - Tests effort effects on pupil dynamics
5. **05_missingness_diagnostic.R** - Analyzes missingness patterns
6. **06_pupil_psychometric_coupling.R** - **PRIMARY ANALYSIS**: GLMM linking pupil to psychometric sensitivity
7. **07_pf_pupil_subject_coupling.R** - Subject-level correlations between pupil and PF changes
8. **08_generate_figures.R** - Generates all publication-ready figures

### 📄 Documentation

- ✅ `README.md` - Comprehensive guide to using the pipeline
- ✅ `config/paths_config.R` - Centralized path configuration
- ✅ `reports/chap2_psychometric_pupil.qmd` - Quarto report template
- ✅ `run_analysis.R` - Master script to run all analyses in order

## Next Steps

### 1. Install Required R Packages

```r
install.packages(c(
  "tidyverse",
  "lme4",
  "broom.mixed",
  "GGally",
  "here",
  "kableExtra",
  "quarto",
  "knitr"
))
```

### 2. Run the Analysis Pipeline

**Option A: Run all scripts at once**
```r
cd 07_manuscript/chapter2
source("run_analysis.R")
```

**Option B: Run scripts individually (recommended for first run)**
```r
# In order:
source("scripts/01_load_and_validate_data.R")
source("scripts/02_compute_pf_parameters.R")
source("scripts/03_pupil_quality_tiers.R")
source("scripts/04_effort_pupil_manipulation_check.R")
source("scripts/05_missingness_diagnostic.R")
source("scripts/06_pupil_psychometric_coupling.R")
source("scripts/07_pf_pupil_subject_coupling.R")
source("scripts/08_generate_figures.R")
```

### 3. Generate Report

After running analyses:
```bash
cd reports
quarto render chap2_psychometric_pupil.qmd
```

## Key Features

### ✅ Aligned with Prospectus

All scripts follow the methodology specified in the dissertation prospectus:

- **Behavioral backbone**: PF parameter extraction/re-estimation (Script 02)
- **Effort-pupil manipulation check**: Tests High vs Low effort effects (Script 04)
- **Missingness diagnostic**: Models missingness patterns (Script 05)
- **Primary analysis**: GLMM with continuous stimulus intensity and pupil state interaction (Script 06)
- **Subject-level coupling**: Correlates Δpupil with ΔPF parameters (Script 07)

### ✅ Quality Tier System

Three quality tiers implemented:
- **Primary**: ≥0.60 validity (baseline AND cognitive windows)
- **Lenient**: ≥0.50 validity (robustness check)
- **Strict**: ≥0.70 validity (robustness check)

### ✅ Within-Subject Centering

Pupil metrics decomposed into:
- **State component**: Trial value - subject mean (within-person fluctuations)
- **Trait component**: Subject mean (between-person differences)

This allows testing whether within-person arousal fluctuations predict sensitivity changes.

### ✅ Continuous Stimulus Intensity

All analyses preserve continuous stimulus intensity (not binned into Easy/Hard), as specified in the prospectus.

## Validation Checklist

Before running analyses, verify:

- [x] Data files copied successfully (including master spreadsheet files)
- [x] Directory structure created
- [x] All scripts created
- [ ] Required R packages installed
- [ ] Data file structure matches script expectations
- [ ] Review `LC Aging Subject Data master spreadsheet - behavioral.csv` for PF parameters (may be used by script 02)
- [ ] Ready to run first script (01_load_and_validate_data.R)

## Notes

- Scripts use `here()` package for path management (relative to repository root)
- All output directories are auto-created by scripts
- Models are saved as .rds files for later inspection
- Figures are saved in publication-ready format (PNG, 300 DPI)
- The pipeline is designed to handle missing data gracefully

### Additional Data Files

**Master Spreadsheet Files:**
The `LC Aging Subject Data master spreadsheet` files provide complementary information:

- **behavioral.csv**: Contains pre-computed PF parameters (thresholds, slopes) by subject × task × effort
  - Columns include: `aud_thresh`, `vis_thresh`, `aud_thresh_low`, `vis_thresh_low`, `aud_thresh_high`, `vis_thresh_high`, etc.
  - Similar structure for slopes: `aud_slope`, `vis_slope`, with Low/High variants
  - **Consideration for Script 02:** These PF parameters may represent the "existing behavioral manuscript" PF fits mentioned in the prospectus. Script 02 should check if these exist and can be used instead of re-computing.
  
- **demographics.csv** and **neuropsych.csv**: Useful for participant characterization, but not required for core Chapter 2 analyses

- **behavioral data dictionary.csv**: Reference guide for variable meanings in the master spreadsheet

## Troubleshooting

If you encounter issues:

1. **Data file not found**: Check that `ch2_triallevel_merged.csv` exists in `data/processed/`
2. **Package errors**: Install required packages (see Next Steps section)
3. **Path errors**: Ensure you're running from the repository root or using `here()` correctly
4. **Model convergence**: May need to adjust optimizers or simplify models for small samples

See `README.md` for detailed troubleshooting guidance.

---

**Setup completed:** All components created and ready for analysis!

