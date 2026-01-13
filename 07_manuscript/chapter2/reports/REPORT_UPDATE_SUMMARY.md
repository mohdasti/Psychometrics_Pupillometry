# Chapter 2 Dissertation Report Update Summary

**Date:** December 2024  
**File:** `chapter2_dissertation.qmd`

---

## Updates Made

### 1. Path Configuration Updated

**Changed paths to reflect new directory structure:**
- Data: `data/processed/` (root level, not `07_manuscript/chapter2/data/processed/`)
- Figures: `06_visualization/output/figures/` (not `07_manuscript/chapter2/output/figures/`)
- Tables: `06_visualization/output/tables/` (not `07_manuscript/chapter2/output/tables/`)
- QC files: `data/qc/` (not `07_manuscript/chapter2/data/qc/`)

**Updated `fig_path()` helper function:**
- Now searches in `06_visualization/output/figures/`
- Updated relative paths for Quarto rendering from `reports/` directory

### 2. New Data Files Loaded

**Added loading of additional analysis outputs:**
- `effort_total_auc_effects.csv` - Total AUC effort effects
- `pupil_psychometric_lenient_effects.csv` - Lenient tier results
- `pupil_psychometric_strict_effects.csv` - Strict tier results
- `missingness_diagnostic_effects.csv` - Missingness model results

### 3. New Results Sections Added

#### Data Quality Summary
- Added new section showing pupil data availability by task
- Displays quality tier counts and proportions
- Shows mean baseline and cognitive window quality

#### Behavioral Backbone
- Added figure reference: `fig1_psychometric_by_effort.png`
- Shows psychometric functions by effort condition

#### Effort–Pupil Manipulation Check
- Updated to show both Total AUC and Cognitive AUC effects
- Updated figure reference to `fig2_effort_pupil_manipulation.png`
- Table now combines results from both metrics

#### Missingness Diagnostic
- Added proper table display of missingness model results
- Added figure reference: `fig4_missingness_diagnostic.png`
- Shows which predictors (effort, stimulus intensity, task, RT) predict missingness

#### Pupil–Psychometric Coupling
- Added interaction plot figure: `pupil_psychometric_interaction.png`
- Updated figure reference to `fig3_psychometric_by_pupil_state.png`
- Added robustness checks table showing interaction effects across quality tiers

#### Subject-Level PF–Pupil Coupling
- Added figure references:
  - `pf_pupil_coupling_threshold.png`
  - `pf_pupil_coupling_slope.png`
  - `pf_pupil_coupling_matrix.png`
- Shows correlation plots and correlation matrix

---

## Available Figures

All figures are located in `06_visualization/output/figures/`:

1. **`fig1_psychometric_by_effort.png`** - Psychometric functions by effort condition
2. **`fig2_effort_pupil_manipulation.png`** - Effort–pupil manipulation check (combined Total/Cognitive AUC)
3. **`fig3_psychometric_by_pupil_state.png`** - Psychometric functions by pupil state tertiles
4. **`fig4_missingness_diagnostic.png`** - Missingness diagnostic plot
5. **`psychometric_by_pupil_state.png`** - Alternative version of pupil state plot
6. **`pupil_psychometric_interaction.png`** - Model predictions showing interaction
7. **`effort_manipulation_cog_auc.png`** - Cognitive AUC effort check
8. **`effort_manipulation_total_auc.png`** - Total AUC effort check
9. **`effort_manipulation_subject_scatter.png`** - Subject-level scatter plot
10. **`pf_pupil_coupling_threshold.png`** - Subject-level coupling: threshold
11. **`pf_pupil_coupling_slope.png`** - Subject-level coupling: slope
12. **`pf_pupil_coupling_matrix.png`** - Correlation matrix

---

## Available Tables

All tables are located in `06_visualization/output/tables/`:

1. **`effort_cog_auc_effects.csv`** - Cognitive AUC effort effects
2. **`effort_total_auc_effects.csv`** - Total AUC effort effects
3. **`missingness_diagnostic_effects.csv`** - Missingness model results
4. **`pupil_psychometric_primary_effects.csv`** - Primary GLMM results
5. **`pupil_psychometric_lenient_effects.csv`** - Lenient tier results
6. **`pupil_psychometric_strict_effects.csv`** - Strict tier results
7. **`pf_pupil_coupling_correlations.csv`** - Subject-level correlations

---

## QC Files

Located in `data/qc/`:

1. **`pupil_quality_summary.csv`** - Quality tier summaries by subject × task
2. **`missingness_diagnostic.csv`** - Missingness summary by subject × task × effort
3. **`inclusion_bias_table.csv`** - Inclusion bias analysis

---

## Rendering the Report

From the repository root:

```bash
cd 07_manuscript/chapter2/reports
quarto render chapter2_dissertation.qmd
```

Or from R:

```r
quarto::quarto_render("07_manuscript/chapter2/reports/chapter2_dissertation.qmd")
```

---

## Notes

- All paths have been updated to work with the new numbered folder structure
- The report will automatically find figures and tables in the new locations
- If any files are missing, the report will display placeholder messages
- All analysis results are now integrated into the report



