# Chapter 2: Pupil-Indexed Arousal and Psychometric Sensitivity in Older Adults

**Dissertation Title:** A Mechanistic Investigation of Physical-Cognitive Effort Interactions and Their Modulation by Aging

**Chapter:** Chapter 2 — Pupil-Indexed Arousal and Psychometric Sensitivity in Older Adults

---

## Overview

This repository contains the analysis pipeline for Chapter 2 of the dissertation, which extends an existing multi-author behavioral manuscript by adding trial-wise physiological coupling analyses. The chapter investigates how physical effort–induced and pupil-indexed arousal relates to perceptual discrimination at the psychometric level in healthy older adults (65+ years).

### Related Repository

The dataset, experiment design, behavioral files, and pupillometry data are **identical** to those used in the companion repository:

**→ [`modeling-pupil-DDM`](https://github.com/mohdasti/modeling-pupil-DDM)** (Chapter 3: Drift Diffusion Modeling)

The key difference is the **analysis focus**:
- **This repository (Chapter 2):** Psychometric function analysis and GLMM-based pupil–psychometric coupling
- **modeling-pupil-DDM (Chapter 3):** Hierarchical drift diffusion modeling (DDM) and pupil–DDM parameter correlations

Both repositories share preprocessing pipelines and data organization structures. See the "Context and Relationship to Other Work" section below for details.

### Key Contribution

While the existing behavioral manuscript provides psychometric function (PF) fits and reports how handgrip effort relates to PF-derived parameters (thresholds and slopes), this dissertation extension quantifies **how trial-wise arousal** (indexed by phasic pupil dynamics) covaries with psychometric sensitivity using hierarchical generalized linear mixed models (GLMMs) that preserve continuous stimulus intensity and individual-level arousal granularity.

---

## Context and Relationship to Other Work

### Relationship to modeling-pupil-DDM Repository

This repository shares the same underlying data and experiment design as the [`modeling-pupil-DDM`](https://github.com/mohdasti/modeling-pupil-DDM) repository (Chapter 3). Both repositories use:
- Identical behavioral data files
- Identical pupillometry preprocessing pipeline (MATLAB)
- Identical experimental paradigm (handgrip dual-task with auditory/visual discrimination)
- Identical participant sample (older adults, 65+ years)

The key distinction is the **analysis approach**:
- **This repository (Chapter 2):** Psychometric function analysis and GLMM-based pupil–psychometric coupling
- **modeling-pupil-DDM (Chapter 3):** Hierarchical drift diffusion modeling (DDM) and pupil–DDM parameter correlations

Users may reference the DDM repository for shared preprocessing pipelines and data organization structures.

### Behavioral Backbone

Chapter 2 is anchored in an existing multi-author behavioral manuscript that:
- Fits psychometric functions (PFs) to older adult dual-task data
- Reports effort-related changes in PF parameters (thresholds, slopes)
- Includes complementary confidence/metacognitive analyses

The PF outcomes from that work provide the **behavioral backbone** for this chapter. The dissertation chapter reports these PF results and (where needed) re-estimates confirmatory contrasts within a consistent statistical framework.

### Dissertation Extension

The **novel component** of Chapter 2 is a model-based integration of pupil-indexed arousal into psychometric analysis that:
- Preserves continuous stimulus intensity (rather than collapsing into "Easy/Hard" bins)
- Separates within-person (state) from between-person (trait) arousal differences
- Handles realistic pupillometry constraints (noise, missingness, unbalanced trial counts)

---

## Methodology

### Participants

Approximately 50 healthy older adults (target N ≈ 50 after QC; final N may vary modestly), aged 65+ years.

### Task Paradigm

Participants perform auditory and visual same–different discrimination while simultaneously squeezing a dynamometer at:
- **Low effort:** 5% MVC
- **High effort:** 40% MVC

**Stimuli:**
- **Auditory:** 1000 Hz base tones with ±8/16/32/64 Hz offsets
- **Visual:** Oriented Gabor patches with ±0.06/0.12/0.24/0.48 contrast differences

**Trial structure:** 3 s pre-squeeze baseline → 3 s sustained squeeze → standard (0.1 s) → ISI (0.5 s) → target (0.1 s) → response window

### Pupillometry

Pupil diameter is recorded continuously and processed using an established MATLAB preprocessing pipeline that:
- Segments trials relative to squeeze onset
- Flags blinks/track loss
- Computes window-specific validity metrics
- Implements baseline correction using pre-event windows

#### Pupil Features

Two primary features are computed on each trial:

1. **Total AUC:** Baseline-corrected area under the pupil curve over a global trial window, indexing overall arousal during concurrent physical effort. Used primarily for effort–pupil manipulation checks and subject-level coupling.

2. **Cognitive pupil metric (primary):** Baseline-corrected task-evoked measure computed in a **fixed-duration post-target window** (e.g., 300 ms after target onset through a fixed 1 s window), designed to reduce confounding by RT and late-trial dropout. Implemented as fixed-window AUC (primary) and/or fixed-window mean dilation (secondary), with response-locked variants evaluated in sensitivity analyses.

#### Quality Tiers

Pupil-based analyses use pre-specified quality tiers:

- **Primary tier:** Window validity ≥ 0.60 for baseline and cognitive windows
- **Sensitivity tiers:** Window validity ≥ 0.50 (lenient) and ≥ 0.70 (stricter)
- **Window sensitivity:** Fixed-window cognitive metric (primary) vs. response-locked cognitive metric (secondary)

---

## Analysis Pipeline

### 1. Descriptive Analyses (`01_descriptives/`)

**Effort–pupil manipulation check:** Mixed-effects models test whether Total AUC and the fixed-window cognitive pupil metric differ between Low and High effort, across modalities. This provides a manipulation check that physical effort modulates global and task-evoked pupil dynamics in older adults.

### 2. Missingness Diagnostics (`02_missingness/`)

**Missingness-as-outcome analysis:** Logistic mixed-effects models test whether pupil usability (pass/fail) depends on effort, stimulus intensity, modality, and RT. If retention is predicted by task variables, results are interpreted with that bias in mind and robustness analyses are emphasized.

### 3. Psychometric Analyses (`03_psychometrics/`)

**Behavioral backbone (PF outcomes):** Quantifies how High versus Low handgrip effort alters PF-derived thresholds and slopes in older adults, separately for auditory and visual discrimination. Uses subject-level PF fits from the existing behavioral manuscript.

### 4. Pupil–Psychometric Coupling (`04_glmm_coupling/`)

**Primary analysis:** Trial-level GLMMs linking phasic arousal to psychometric sensitivity.

The primary model treats the pupil metric as a continuous predictor using within-subject centering to separate state and trait components:

\[
P^{(\text{trait})}_{j} = \overline{P}_{j}, \qquad
P^{(\text{state})}_{ij} = P_{ij} - \overline{P}_{j}
\]

Key model structure:
- **Outcome:** Binary choice (e.g., "different" = 1)
- **Predictors:** Continuous stimulus intensity (X), Effort, Modality, within-subject centered pupil (state), between-subject pupil (trait)
- **Key interaction:** X × P^(state) tests whether within-person fluctuations in arousal are associated with changes in psychometric sensitivity

To reduce RT-related measurement confounds, models using response-locked pupil metrics include RT as a covariate.

**Secondary coupling:** Subject-level changes in pupil metrics (High–Low effort) related to subject-level changes in PF parameters (High–Low effort), testing whether individuals with stronger effort-evoked arousal changes also show larger effort-related changes in thresholds/slopes.

---

## Repository Structure

The repository structure is organized to align with the analysis pipeline stages:

```
Psychometrics_Pupillometry/
├── 01_data_preprocessing/   # Data preprocessing (shared structure with DDM repo)
│   ├── matlab/              # MATLAB preprocessing pipeline (pupillometry)
│   │   ├── segment_trials.m
│   │   └── blink_correction.m
│   └── python/              # Behavioral data preprocessing
├── 02_pupillometry_analysis/# Pupil feature extraction
│   └── feature_extraction/  # Total AUC, cognitive pupil metrics, QC tiers
├── analysis/
│   ├── 01_descriptives/     # Effort–pupil manipulation checks
│   ├── 02_missingness/      # Missingness-as-outcome diagnostic models
│   ├── 03_psychometrics/    # Behavioral backbone (PF fits)
│   └── 04_glmm_coupling/    # Primary trial-level pupil–intensity models
├── data/
│   ├── raw/                 # Raw data files (gitignored if sensitive)
│   ├── processed/           # Validated pupil windows, merged behavioral data
│   └── metadata/            # Participant logs, QC tier lists
├── figures/                 # Output for PFs and Pupil Intensity–Response plots
├── config/                  # Configuration files (paths, parameters)
├── README.md
└── .gitignore
```

*Note: Directory structure will be populated as analyses are developed. Preprocessing stages (01_data_preprocessing, 02_pupillometry_analysis) follow the same structure as the companion DDM repository, while analysis stages (03_psychometrics, 04_glmm_coupling) are specific to Chapter 2.*

---

## Requirements

### Software

- **R:** Version 4.x or higher
- **MATLAB:** R2020b or higher (for preprocessing pipeline)

### R Packages

Core dependencies include:
- `lme4` — Mixed-effects models for GLMMs
- `tidyverse` — Data manipulation and visualization
- Additional packages as specified in individual analysis scripts

*Note: While `brms` is used in the companion DDM repository (Chapter 3) for Bayesian hierarchical modeling, this repository focuses on frequentist GLMMs via `lme4`. Bayesian approaches may be used for robustness checks if needed.*

### MATLAB Toolboxes

As specified in the preprocessing pipeline documentation.

---

## Code Organization

### Backbone Code vs. Dissertation Extension Code

To avoid confusion about ownership and novelty:

- **Behavioral backbone code:** PF parameter estimation and related analyses from the existing multi-author manuscript. These analyses provide the foundation but are not the primary focus of this dissertation extension.

- **Dissertation extension code:** Trial-level GLMMs linking pupil-indexed arousal to psychometric sensitivity, missingness diagnostics, and related robustness analyses. These represent the novel contributions of this chapter.

All code is clearly labeled to indicate its source and purpose.

### Shared vs. Repository-Specific Code

- **Shared preprocessing code:** The MATLAB pupillometry preprocessing pipeline and behavioral data preprocessing steps are shared with the [`modeling-pupil-DDM`](https://github.com/mohdasti/modeling-pupil-DDM) repository. These scripts may be referenced from either repository.

- **Chapter 2-specific code:** Psychometric function fitting, GLMM coupling analyses, and missingness diagnostics are specific to this repository and represent the Chapter 2 analysis pipeline.

---

## Expected Outcomes

If hypotheses are supported, Chapter 2 will show that in older adults:

1. High-effort handgrip modulates pupil dynamics (global and task-evoked), providing a physiological manipulation check.

2. Psychometric performance differs between effort conditions at the behavioral level (PF-derived parameters), and trial-wise arousal covaries with psychometric sensitivity when intensity is modeled directly, with effects that are robust across reasonable quality thresholds and pupil-window definitions.

Regardless of whether arousal primarily relates to sensitivity-like changes (intensity coupling) or criterion-like shifts (intercepts), Chapter 2 provides a statistically rigorous characterization of how pupil-indexed arousal covaries with psychophysical decision behavior in aging.

---

## References

See the dissertation prospectus for full theoretical background and citations.

---

## License

[Specify license — e.g., MIT License is standard for academic code]

---

## Contact

**Author:** Mohammad Dastgheib  
**Institution:** University of California, Riverside  
**Department:** Department of Psychology, Cognition & Cognitive Neuroscience Area  
**Dissertation Committee:** Dr. Ilana Bennett (Chair), Dr. Aaron Seitz, Dr. John Franchak
