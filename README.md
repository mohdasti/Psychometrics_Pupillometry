# Pupil-Indexed Arousal and Psychometric Sensitivity in Older Adults

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21215384.svg)](https://doi.org/10.5281/zenodo.21215384) [![R](https://img.shields.io/badge/R-4.5.2-276DC3?logo=r&logoColor=white)](https://www.r-project.org/) [![Quarto](https://img.shields.io/badge/Quarto-1.7.33-0F4C75?logo=quarto&logoColor=white)](https://quarto.org/) [![Markdown](https://img.shields.io/badge/Markdown-000000?logo=markdown&logoColor=white)](https://www.markdownguide.org/)

This repository contains the analysis pipeline for Chapter 2 of the dissertation: how physical effort–induced and pupil-indexed arousal relates to perceptual discrimination at the psychometric level in healthy older adults. The paradigm combines isometric handgrip with auditory and visual same–different discrimination and integrates pupillometry with trial-level GLMMs and psychometric-function modeling.

**Archived release (code + de-identified data):** [https://doi.org/10.5281/zenodo.21215384](https://doi.org/10.5281/zenodo.21215384)

---

## Overview

### Research question

How does pupil-indexed arousal relate to psychometric sensitivity in older adults when stimulus intensity is modeled continuously? The primary inferential model is a probit GLMM that decomposes pupil into within-person (state) and between-person (trait) components and tests whether momentary arousal modulates the intensity–choice relationship.

### Key features

- Continuous stimulus intensity (not binned Easy/Hard)
- Primary-tier pupil quality gating (60% valid samples in pre-squeeze and cognitive windows; lenient/strict tiers for robustness)
- TOST equivalence testing on the primary coupling interaction
- Subject-level Δpupil vs ΔPF correlations with influence diagnostics
- Quarto dissertation report with reproducible figure/table generation

---

## Quick start

### 1. Run the full analysis pipeline

From the repository root:

```bash
Rscript run_analysis.R
```

Or in R:

```r
source("run_analysis.R")
```

This runs scripts **01–08** in order (see [Analysis pipeline](#analysis-pipeline)).

### 2. Regenerate manuscript figures

Several dissertation figures are built by dedicated scripts under `07_manuscript/chapter2/scripts/` (participant flow, equivalence forest, coupling forest, effort dissociation, pupil waveforms). After the main pipeline:

```bash
Rscript 07_manuscript/chapter2/scripts/plot_participant_flow.R
Rscript 07_manuscript/chapter2/scripts/plot_equivalence_forest.R
Rscript 07_manuscript/chapter2/scripts/plot_subject_coupling_forest.R
Rscript 07_manuscript/chapter2/scripts/plot_effort_dissociation.R
Rscript 07_manuscript/chapter2/scripts/plot_ch2_pupil_waveform.R
```

Shared plotting logic lives in `R/` (e.g. `plot_equivalence_forest.R`, `plot_subject_coupling_forest.R`, `plot_effort_pupil_manipulation.R`).

### 3. Render the dissertation chapter

```bash
cd 07_manuscript/chapter2/reports
quarto render chapter2_dissertation.qmd
```

---

## Analysis pipeline

| Stage | Script | Purpose |
|-------|--------|---------|
| 1 | `01_data_preparation/01_load_and_validate_data.R` | Load and validate merged trial-level data |
| 1 | `01_data_preparation/02_compute_pf_parameters.R` | Psychometric-function parameters (threshold, slope) |
| 2 | `02_pupil_quality_control/03_pupil_quality_tiers.R` | Quality tiers; within-subject pupil centering |
| 2 | `02_pupil_quality_control/05_missingness_diagnostic.R` | Missingness / selection bias diagnostics |
| 3 | `03_effort_manipulation_check/04_effort_pupil_manipulation_check.R` | Effort → Total/Cognitive AUC manipulation check |
| 4 | `04_pupil_psychometric_coupling/06_pupil_psychometric_coupling.R` | **Primary GLMM**, quality-tier refits, TOST, model comparison |
| 4 | `04_pupil_psychometric_coupling/06b_lc_integrity_extension.R` | Optional LC-integrity extension (exploratory) |
| 5 | `05_subject_level_analysis/07_pf_pupil_subject_coupling.R` | Subject-level Δpupil vs ΔPF correlations |
| 6 | `06_visualization/08_generate_figures.R` | Core publication figures |

All scripts source `config/paths_config.R` for paths relative to the repo root.

---

## Repository structure

```
Psychometrics_Pupillometry/
├── 01_data_preparation/
│   ├── 01_load_and_validate_data.R
│   ├── 02_compute_pf_parameters.R
│   └── make_quick_share_v7.R          # Regenerate pupil trial-level CSV from MATLAB outputs
├── 02_pupil_quality_control/
│   ├── 03_pupil_quality_tiers.R
│   └── 05_missingness_diagnostic.R
├── 03_effort_manipulation_check/
│   └── 04_effort_pupil_manipulation_check.R
├── 04_pupil_psychometric_coupling/
│   ├── 06_pupil_psychometric_coupling.R
│   └── 06b_lc_integrity_extension.R
├── 05_subject_level_analysis/
│   └── 07_pf_pupil_subject_coupling.R
├── 06_visualization/
│   ├── 08_generate_figures.R
│   └── output/
│       ├── figures/                   # Generated PNG/PDF figures (gitignored)
│       ├── tables/                    # CSV summaries (gitignored)
│       └── models/                    # Saved .rds model objects (partially tracked)
├── 07_manuscript/chapter2/
│   ├── reports/
│   │   ├── chapter2_dissertation.qmd  # Main dissertation chapter
│   │   ├── references.bib
│   │   └── pupil_data_report_advisor.qmd
│   └── scripts/                       # Manuscript-specific figure/table helpers
│       ├── plot_participant_flow.R
│       ├── plot_equivalence_forest.R
│       ├── plot_subject_coupling_forest.R
│       ├── plot_effort_dissociation.R
│       └── plot_ch2_pupil_waveform.R
├── R/                                 # Shared helpers (plots, TOST, coupling, sample flow)
│   ├── plot_equivalence_forest.R
│   ├── plot_subject_coupling_forest.R
│   ├── plot_effort_pupil_manipulation.R
│   ├── plot_participant_flow.R
│   ├── tost_equivalence_helpers.R
│   ├── subject_coupling_helpers.R
│   ├── pf_analysis_helpers.R
│   ├── ch2_sample_helpers.R
│   ├── build_hypothesis_summary.R
│   └── colors_manuscript.R
├── config/
│   └── paths_config.R
├── data/                              # gitignored locally; included in Zenodo archive
│   ├── raw/behavioral/
│   ├── processed/
│   └── qc/
├── run_analysis.R                     # Master pipeline script
└── README.md
```

---

## Data

CSV data files are **gitignored** in this repository (de-identified trial-level records). They are included in the [Zenodo archive](https://doi.org/10.5281/zenodo.21215384). After download, place files under `data/` as below.

### Processed analysis files (`data/processed/`)

| File | Description |
|------|-------------|
| `ch2_triallevel_merged.csv` | Trial-level behavioral + pupil metrics + quality flags (primary analysis file) |
| `ch2_triallevel_pupil.csv` | Pupil-processed trial file (AUC metrics, validity columns) |
| `ch2_pf_parameters.csv` | Per-subject × task × effort PF thresholds and slopes |
| `README_data_source.md` | Pupil preprocessing provenance (v7 baseline alignment, AUC windows) |

### Raw behavioral files (`data/raw/behavioral/`)

| File | Description |
|------|-------------|
| `bap_beh_trialdata_v2.csv` | Trial-level behavioral responses |
| `bap_beh_subjxtaskdata_v2.csv` | Subject × task summaries |
| `bap_beh_trialdata_v2_trials_per_subject_per_task.csv` | Trial counts per subject/task |
| `LC Aging Subject Data master spreadsheet - behavioral.csv` | Master behavioral spreadsheet (PF parameters, etc.) |
| `LC Aging Subject Data master spreadsheet - behavioral data dictionary.csv` | Data dictionary |
| `LC Aging Subject Data master spreadsheet - demographics.csv` | Demographics |
| `LC Aging Subject Data master spreadsheet - neuropsych.csv` | MoCA and neuropsych battery |

### QC outputs (`data/qc/`)

Generated by the pipeline: `pupil_quality_summary.csv`, `missingness_diagnostic.csv`, `inclusion_bias_table.csv`.

---

## Outputs

### Models (`06_visualization/output/models/`)

| File | Description |
|------|-------------|
| `mod_pupil_psychometric_primary.rds` | Primary-tier probit GLMM |
| `mod_pupil_psychometric_lenient.rds` | Lenient-tier robustness refit |
| `mod_pupil_psychometric_strict.rds` | Strict-tier robustness refit |
| `mod_pupil_psychometric_slow_rt.rds` | Slow-RT sensitivity subset |
| `mod_pupil_psychometric_no_interaction.rds` | Nested model (no intensity × pupil-state interaction) |
| `mod_pupil_psychometric_effort_intensity.rds` | Expanded model with effort × intensity term |
| `mod_effort_total_auc.rds` / `mod_effort_cog_auc.rds` | Effort manipulation checks |
| `mod_missingness_diagnostic.rds` | Pupil missingness GLMM |

### Tables (`06_visualization/output/tables/`)

Key CSVs include `pupil_psychometric_primary_effects.csv`, `pupil_psychometric_tost_equivalence.csv`, `pf_pupil_coupling_correlations.csv`, `pf_pupil_coupling_sensitivity.csv`, `effort_*_effects.csv`, `pf_threshold_effort_effects.csv`, `pf_slope_effort_effects.csv`, and tiered GLMM effect files.

### Figures (`06_visualization/output/figures/`)

Key figures include `fig_equivalence_forest.png`, `fig_subject_coupling_forest.png`, `fig2_effort_pupil_manipulation.png`, `participant_flow.png`, `fig_effort_dissociation.png`, `pupil_psychometric_interaction.png`, and appendix scatter/coupling panels. PNG/PDF outputs are gitignored; regenerate via the pipeline and manuscript scripts above.

---

## Requirements

### Software

- **R** ≥ 4.x
- **Quarto** (for rendering `chapter2_dissertation.qmd`)
- **MATLAB** + Psignifit 4 (only if refitting psychometric functions from raw counts; PF parameters are also provided in `ch2_pf_parameters.csv`)

### R packages

Core dependencies used across the pipeline:

```r
install.packages(c(
  "tidyverse", "lme4", "lmerTest", "broom.mixed",
  "here", "ggplot2", "patchwork", "ggtext", "ggdist",
  "GGally", "gt", "knitr", "kableExtra", "mgcv"
))
```

---

## Methodology (summary)

### Pupil metrics

- **Total AUC:** Baseline-corrected AUC over 0–4.70 s from squeeze onset (B0 reference); indexes sustained effort-period arousal.
- **Cognitive AUC:** Baseline-corrected mean dilation in fixed window 4.85–6.05 s from squeeze onset (B1 reference); primary GLMM predictor (within-subject centered state + between-subject trait).

### Pupil quality tiers

- **Primary:** ≥ 60% valid samples in pre-squeeze baseline and cognitive windows
- **Lenient:** ≥ 50% in both windows
- **Strict:** ≥ 70% in both windows

### Primary GLMM

Probit link; random intercept and random intensity slope by participant. Primary coupling term: stimulus intensity × pupil state. Robustness includes quality-tier refits, slow-RT subset, AIC model comparison, and TOST equivalence at SESOI ± 0.10 probit units.

Full methods and equations are in `07_manuscript/chapter2/reports/chapter2_dissertation.qmd`.

---

## Troubleshooting

**Data file not found:** Download the Zenodo archive or run `01_load_and_validate_data.R` after placing raw/processed CSVs under `data/`.

**Figure not found in Quarto render:** Run `run_analysis.R`, then the manuscript figure scripts in `07_manuscript/chapter2/scripts/`.

**Path errors:** Always run from the repository root; scripts use `here()` and `config/paths_config.R`.

More detail: `07_manuscript/chapter2/README.md`.

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

If you use this code or data, please cite the Zenodo archive:

```bibtex
@software{psychometrics_pupillometry_2026,
  author  = {Dastgheib, Mohammad},
  title   = {Psychometrics\_Pupillometry: Pupil-Indexed Arousal and Psychometric Sensitivity in Older Adults},
  year    = {2026},
  url     = {https://github.com/mohdasti/Psychometrics_Pupillometry},
  doi     = {10.5281/zenodo.21215384}
}
```
