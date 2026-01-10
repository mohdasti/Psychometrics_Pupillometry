# Pupil-Indexed Arousal and Psychometric Sensitivity in Older Adults

[![DOI](https://zenodo.org/badge/1124053644.svg)](https://doi.org/10.5281/zenodo.18205183) [![R](https://img.shields.io/badge/R-4.5.2-276DC3?logo=r&logoColor=white)](https://www.r-project.org/) [![Quarto](https://img.shields.io/badge/Quarto-1.7.33-0F4C75?logo=quarto&logoColor=white)](https://quarto.org/) [![Markdown](https://img.shields.io/badge/Markdown-000000?logo=markdown&logoColor=white)](https://www.markdownguide.org/)

This repository contains the analysis pipeline for investigating how physical effort–induced and pupil-indexed arousal relates to perceptual discrimination at the psychometric level in healthy older adults. The project uses a dual-task paradigm combining handgrip effort with auditory and visual perceptual discrimination, and integrates pupillometry with psychometric function analysis.

---

## Overview

### Research Question

How does physical effort–induced arousal (indexed by pupil dynamics) relate to perceptual discrimination sensitivity in older adults? This project uses hierarchical generalized linear mixed models (GLMMs) to quantify how trial-wise phasic arousal covaries with psychometric sensitivity while preserving continuous stimulus intensity and individual-level arousal granularity.

### Key Contribution

This project extends behavioral psychometric function (PF) analyses by adding trial-wise physiological coupling. Rather than collapsing trials into coarse "Easy/Hard" bins, we model continuous stimulus intensity directly and decompose arousal into within-person (state) and between-person (trait) components to test whether within-person fluctuations in arousal predict changes in psychometric sensitivity.

---

## Methodology

### Participants

Approximately 50 healthy older adults (target N ≈ 50 after QC; final N may vary modestly), aged 65+ years.

### Task Paradigm

Participants perform **same–different discrimination** tasks in two modalities while simultaneously squeezing a dynamometer:

**Effort Conditions:**
- **Low effort:** 5% of maximum voluntary contraction (MVC)
- **High effort:** 40% MVC

**Stimuli:**
- **Auditory Discrimination Task (ADT):** 1000 Hz base tones with frequency offsets of ±8, ±16, ±32, or ±64 Hz
- **Visual Discrimination Task (VDT):** Oriented Gabor patches with contrast differences of ±0.06, ±0.12, ±0.24, or ±0.48

**Trial Structure:**
Each trial follows this sequence:
1. **Pre-squeeze baseline:** 3 seconds
2. **Sustained squeeze period:** 3 seconds (Low or High effort)
3. **Standard stimulus:** 0.1 seconds
4. **Inter-stimulus interval (ISI):** 0.5 seconds
5. **Target stimulus:** 0.1 seconds
6. **Response window:** Participants indicate whether the target was "same" or "different" from the standard

Participants respond using button presses, and both choice and response time are recorded.

### Pupillometry

Pupil diameter is recorded continuously throughout each trial using an eye-tracking system. The data are processed using a MATLAB preprocessing pipeline that:

- Segments trials relative to squeeze onset
- Flags blinks and track loss events
- Computes window-specific validity metrics (proportion of valid samples)
- Implements baseline correction using pre-event windows

#### Pupil Features

Two primary features are computed on each trial:

1. **Total AUC:** Baseline-corrected area under the pupil curve over a global trial window, indexing overall arousal during concurrent physical effort. Used primarily for effort–pupil manipulation checks and subject-level coupling.

2. **Cognitive pupil metric (primary):** Baseline-corrected task-evoked measure computed in a **fixed-duration post-target window** (e.g., from 300 ms after target onset through a fixed 1 s window), designed to reduce confounding by response time and late-trial dropout. Implemented as fixed-window AUC (primary) and/or fixed-window mean dilation (secondary), with response-locked variants evaluated in sensitivity analyses.

#### Quality Tiers

Pupil-based analyses use pre-specified quality tiers based on window-specific validity:

- **Primary tier:** Baseline validity ≥ 0.60 AND cognitive window validity ≥ 0.60 (used for primary analyses)
- **Lenient tier:** Baseline validity ≥ 0.50 AND cognitive window validity ≥ 0.50 (robustness check)
- **Strict tier:** Baseline validity ≥ 0.70 AND cognitive window validity ≥ 0.70 (robustness check)

This strategy matches inclusion criteria to the physiological question and demonstrates that key inferences are robust across reasonable quality thresholds.

---

## Analysis Pipeline

### 1. Data Loading and Validation

Loads and validates pre-merged trial-level data containing both behavioral measures (choices, RT, stimulus parameters) and pupil metrics (with quality flags). Validates data structure, checks for missing values, and standardizes variable names.

**Script:** `01_data_preparation/01_load_and_validate_data.R`

### 2. Psychometric Function (PF) Parameter Estimation

Fits psychometric functions to extract thresholds and slopes per subject × task × effort combination, or loads existing PF parameters if available. Uses probit link functions (natural for psychometric modeling) with continuous stimulus intensity as the predictor.

**Script:** `01_data_preparation/02_compute_pf_parameters.R`

### 3. Pupil Quality Tiers and Within-Subject Centering

Defines quality tiers based on window validity thresholds and computes within-subject centered pupil metrics:

- **State component:** `pupil_state = trial_value - subject_mean` (within-person fluctuations)
- **Trait component:** `pupil_trait = subject_mean` (between-person differences)

This decomposition allows testing whether within-person arousal fluctuations predict sensitivity changes, independent of stable individual differences in baseline arousal.

**Script:** `02_pupil_quality_control/03_pupil_quality_tiers.R`

### 4. Effort–Pupil Manipulation Check

Tests whether High effort increases pupil metrics relative to Low effort using mixed-effects models. This provides a physiological manipulation check that physical effort effectively modulates central arousal systems (indexed by pupil dynamics).

**Script:** `03_effort_manipulation_check/04_effort_pupil_manipulation_check.R`

### 5. Missingness Diagnostic

Models pupil data missingness as an outcome to test for systematic bias. Uses logistic mixed-effects models to test whether data retention depends on effort, stimulus intensity, modality, or response time. If systematic bias is detected, results are interpreted with that in mind and robustness analyses are emphasized.

**Script:** `02_pupil_quality_control/05_missingness_diagnostic.R`

### 6. Pupil–Psychometric Coupling (Primary Analysis)

Fits hierarchical GLMMs linking phasic arousal to psychometric sensitivity. The primary model uses a probit link function:

$$\text{probit}(P(Y_{ij}=1)) = \beta_0 + \beta_1 X_{ij} + \beta_2 \text{Effort}_{ij} + \beta_3 \text{Modality}_{ij} + \beta_4 P^{(\text{state})}_{ij} + \beta_5 (X_{ij} \times P^{(\text{state})}_{ij}) + \beta_6 P^{(\text{trait})}_{j} + u_{0j} + u_{1j}X_{ij}$$

where:
- $Y_{ij}$ = binary choice (e.g., "different" = 1) on trial $i$ for subject $j$
- $X_{ij}$ = continuous stimulus intensity (frequency or contrast offset)
- $P^{(\text{state})}_{ij}$ = within-subject centered pupil metric (trial value - subject mean)
- $P^{(\text{trait})}_{j}$ = between-subject pupil metric (subject mean)

The **key interaction term** is $X_{ij} \times P^{(\text{state})}_{ij}$, which tests whether within-person fluctuations in arousal are associated with changes in psychometric sensitivity (i.e., changes in how strongly stimulus intensity predicts choice).

Results are tested for robustness across quality tiers (primary, lenient, strict).

**Script:** `04_pupil_psychometric_coupling/06_pupil_psychometric_coupling.R`

### 7. Subject-Level PF–Pupil Coupling

Computes subject-level change scores (High effort - Low effort) for both pupil metrics and PF parameters, then correlates these changes to test whether individuals with stronger effort-evoked arousal changes also show larger effort-related changes in thresholds/slopes.

**Script:** `05_subject_level_analysis/07_pf_pupil_subject_coupling.R`

### 8. Figure Generation

Generates all publication-ready figures including:
- Psychometric function plots by effort condition
- Effort–pupil manipulation check plots
- Psychometric curves by pupil state (high/medium/low tertiles)
- Subject-level coupling plots (Δpupil vs ΔPF parameters)
- Missingness diagnostic plots

**Script:** `06_visualization/08_generate_figures.R`

---

## Repository Structure

```
Psychometrics_Pupillometry/
├── 01_data_preparation/                 # Data loading, validation, PF parameters
│   ├── 01_load_and_validate_data.R
│   ├── 02_compute_pf_parameters.R
│   ├── make_quick_share_v7.R          # Pupil data generation script
│   └── output/                         # Stage-specific outputs
├── 02_pupil_quality_control/            # Quality tiers, missingness diagnostics
│   ├── 03_pupil_quality_tiers.R
│   ├── 05_missingness_diagnostic.R
│   └── output/
├── 03_effort_manipulation_check/        # Effort-pupil manipulation check
│   ├── 04_effort_pupil_manipulation_check.R
│   └── output/
├── 04_pupil_psychometric_coupling/      # PRIMARY ANALYSIS: GLMM coupling
│   ├── 06_pupil_psychometric_coupling.R
│   └── output/
├── 05_subject_level_analysis/           # Subject-level correlations
│   ├── 07_pf_pupil_subject_coupling.R
│   └── output/
├── 06_visualization/                    # Figure generation
│   ├── 08_generate_figures.R
│   └── output/
│       ├── figures/                    # All publication figures
│       ├── tables/                     # All tables
│       └── models/                     # Saved model objects
├── 07_manuscript/                       # Manuscript and reports
│   └── chapter2/
│       ├── docs/                       # Documentation files
│       │   ├── TIMESTAMP_DEFINITIONS.md
│       │   ├── AUC_CALCULATION_METHOD.md
│       │   └── CHAPTER2_SETUP_PROMPT*.md
│       └── chapter2_dissertation.qmd   # Main Quarto report
├── data/                                # Shared data directory
│   ├── raw/
│   │   ├── behavioral/                 # Raw behavioral data
│   │   └── pupil/                      # Raw pupil data
│   ├── processed/                      # Processed data files
│   └── qc/                             # Quality control outputs
├── config/                              # Configuration files
│   └── paths_config.R                  # Centralized path configuration
├── chapter2_materials/                 # Original materials (reference)
├── run_analysis.R                      # Master script to run all analyses
├── README.md                           # This file
└── .gitignore
```

---

## Requirements

### Software

- **R:** Version 4.x or higher
- **MATLAB:** R2020b or higher (for pupillometry preprocessing pipeline, if preprocessing from raw data)

### R Packages

Core dependencies:

```r
install.packages(c(
  "tidyverse",      # Data manipulation and visualization
  "lme4",           # Mixed-effects models for GLMMs
  "broom.mixed",    # Tidying mixed-effects model outputs
  "GGally",         # Correlation matrix plots
  "here",           # Path management
  "kableExtra",     # Enhanced tables (for reports)
  "quarto",         # Report generation
  "knitr"           # Report generation
))
```

### Data Files

The analysis pipeline expects the following data structure:

**Primary processed data:**
- `data/processed/ch2_triallevel_merged.csv`: Pre-merged trial-level data with behavioral + pupil measures
- `data/processed/ch2_triallevel_pupil.csv`: Pupil-processed data with AUC metrics and quality flags

**Raw behavioral data:**
Behavioral data files are sourced from `/Users/mohdasti/Documents/LC-BAP/BAP/Nov2025/` and located in:
- `07_manuscript/chapter2/data/raw/behavioral/bap_beh_trialdata_v2.csv`: Trial-level behavioral data (trial-by-trial responses)
- `07_manuscript/chapter2/data/raw/behavioral/bap_beh_subjxtaskdata_v2.csv`: Subject-level summaries
- `07_manuscript/chapter2/data/raw/behavioral/bap_beh_trialdata_v2_trials_per_subject_per_task.csv`: Trial counts per subject/task

**Master spreadsheet files:**
- `07_manuscript/chapter2/data/raw/behavioral/LC Aging Subject Data master spreadsheet - behavioral.csv`: Contains PF parameters (thresholds, slopes) by subject, task, and effort condition
- `07_manuscript/chapter2/data/raw/behavioral/LC Aging Subject Data master spreadsheet - behavioral data dictionary.csv`: Data dictionary
- `07_manuscript/chapter2/data/raw/behavioral/LC Aging Subject Data master spreadsheet - demographics.csv`: Demographics data
- `07_manuscript/chapter2/data/raw/behavioral/LC Aging Subject Data master spreadsheet - neuropsych.csv`: Neuropsychology battery data

**Documentation:**
- `07_manuscript/chapter2/docs/`: Contains documentation files for data processing, timestamps, AUC calculation methods, and setup guides
- `data/processed/README_data_source.md`: Documentation of pupil data source and processing

---

## Quick Start

### Run Complete Analysis Pipeline

From the repository root directory:

```r
# Source the master script
source("run_analysis.R")
```

Or run scripts individually by analysis stage:

```r
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

### Generate Report

After running all analyses:

```bash
cd 07_manuscript/chapter2/reports
quarto render chapter2_dissertation.qmd
```

---

## Key Methodological Features

### Continuous Stimulus Intensity

All analyses preserve continuous stimulus intensity (not binned into "Easy/Hard"), allowing for direct modeling of intensity–response relationships and interactions with arousal.

### Within-Subject Centering

Pupil metrics are decomposed into state (trial-level fluctuations) and trait (subject-level means) components, allowing tests of whether within-person arousal fluctuations predict sensitivity changes, independent of stable individual differences.

### Quality Tier Robustness

Results are tested across multiple quality thresholds (≥0.50, ≥0.60, ≥0.70) to ensure robustness to different data inclusion criteria, matching quality requirements to the specific physiological question being addressed.

### Fixed-Window Pupil Metrics

Cognitive pupil metrics are computed in fixed-duration windows (rather than response-locked) to reduce confounding by response time and late-trial dropout.

---

## Expected Outcomes

If hypotheses are supported, the analyses will show that in older adults:

1. High-effort handgrip modulates pupil dynamics (global and task-evoked), providing a physiological manipulation check.

2. Psychometric performance differs between effort conditions at the behavioral level (PF-derived parameters), and trial-wise arousal covaries with psychometric sensitivity when intensity is modeled directly, with effects that are robust across reasonable quality thresholds and pupil-window definitions.

Regardless of whether arousal primarily relates to sensitivity-like changes (intensity coupling) or criterion-like shifts (intercepts), the analyses provide a statistically rigorous characterization of how pupil-indexed arousal covaries with psychophysical decision behavior in aging.

---

## Code Organization

### Behavioral Backbone vs. Novel Analyses

- **Behavioral backbone code:** PF parameter estimation and related analyses. These provide the foundation for understanding effort-related changes in psychometric performance.

- **Novel analyses code:** Trial-level GLMMs linking pupil-indexed arousal to psychometric sensitivity, missingness diagnostics, and related robustness analyses. These represent the primary contributions of this project.

All code is clearly labeled to indicate its purpose.

---

## Outputs

### Generated Files

**Figures** (in `06_visualization/output/figures/`):
- Psychometric function plots by effort condition
- Effort–pupil manipulation check plots
- Psychometric curves by pupil state tertiles
- Subject-level coupling plots
- Missingness diagnostic plots

**Tables** (in `06_visualization/output/tables/`):
- Fixed effects from all GLMMs
- Correlation summaries
- Quality tier summaries
- Missingness diagnostic results

**Models** (in `06_visualization/output/models/`):
- Saved R model objects (.rds files) for all fitted models

**Reports:**
- `07_manuscript/chapter2/chapter2_dissertation.qmd`: Quarto report integrating all results

---

## Troubleshooting

**Data file not found:**
- Ensure `ch2_triallevel_merged.csv` exists in `data/processed/`
- Check that raw data files are in `data/raw/behavioral/`
- Behavioral data should be sourced from `/Users/mohdasti/Documents/LC-BAP/BAP/Nov2025/`

**Package errors:**
- Install required packages (see Requirements section)
- Ensure R version is 4.x or higher

**Model convergence issues:**
- Check sample sizes (may need more trials per subject)
- Try different optimizers (e.g., `bobyqa`, `Nelder_Mead`)
- Simplify model structure if needed

**Path errors:**
- Ensure scripts are run from the repository root
- All scripts use `config/paths_config.R` for centralized path management
- Check that `config/paths_config.R` is properly configured

For detailed troubleshooting, see `07_manuscript/chapter2/README.md`.

---

## License

[Specify license — e.g., MIT License is standard for academic code]

---

## Contact

**Author:** Mohammad Dastgheib  
**Institution:** University of California, Riverside  
**Department:** Department of Psychology, Cognition & Cognitive Neuroscience Area

---

## Citation

If you use this code in your research, please cite:

```bibtex
@software{psychometrics_pupillometry,
  title = {Pupil-Indexed Arousal and Psychometric Sensitivity in Older Adults},
  author = {Dastgheib, Mohammad},
  year = {2024},
  url = {https://github.com/mohdasti/Psychometrics_Pupillometry}
}
```
