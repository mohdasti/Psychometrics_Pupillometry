# Chapter 2 Analysis Audit

## Date: 2025-01-XX
## Purpose: Systematic audit of completed vs. missing analyses

---

## ✅ COMPLETED ANALYSES

### 1. PF Parameter Computation
- **Status**: ✅ COMPLETE
- **Script**: `01_data_preparation/02_compute_pf_parameters.R`
- **Output**: `data/processed/ch2_pf_parameters.csv`
- **Details**: PF parameters (threshold, slope) computed for each subject × task × effort combination
- **Note**: Parameters exist but statistical tests comparing High vs Low effort are MISSING (see below)

### 2. Effort-Pupil Manipulation Check
- **Status**: ✅ COMPLETE
- **Script**: `03_effort_manipulation_check/04_effort_pupil_manipulation_check.R`
- **Output Files**:
  - `06_visualization/output/tables/effort_cog_auc_effects.csv`
  - `06_visualization/output/tables/effort_total_auc_effects.csv`
  - `06_visualization/output/models/mod_effort_cog_auc.rds`
  - `06_visualization/output/models/mod_effort_total_auc.rds`
- **Results**: 
  - Total AUC: High effort increases pupil (estimate = 0.857, t = 2.28)
  - Cognitive AUC: High effort decreases pupil (estimate = -0.012, t = -3.86) - **NOTE: This seems counterintuitive, may need verification**
- **Evaluation Status**: Results exist but need to be evaluated in hypothesis summary table

### 3. Missingness Diagnostic
- **Status**: ✅ COMPLETE
- **Script**: `02_pupil_quality_control/05_missingness_diagnostic.R`
- **Output**: `06_visualization/output/tables/missingness_diagnostic_effects.csv`
- **Details**: Tests whether missingness is predicted by effort, stimulus intensity, task, or RT

### 4. Pupil-Psychometric Coupling (Primary Analysis)
- **Status**: ✅ COMPLETE
- **Script**: `04_pupil_psychometric_coupling/06_pupil_psychometric_coupling.R`
- **Output Files**:
  - `06_visualization/output/tables/pupil_psychometric_primary_effects.csv`
  - `06_visualization/output/tables/pupil_psychometric_lenient_effects.csv`
  - `06_visualization/output/tables/pupil_psychometric_strict_effects.csv`
  - `06_visualization/output/models/mod_pupil_psychometric_primary.rds`
  - `06_visualization/output/models/mod_pupil_psychometric_no_interaction.rds`
- **Key Result**: Interaction term (stimulus × pupil state): estimate = 0.035, z = 1.16, p = 0.245 (non-significant)
- **Evaluation Status**: Results exist but need to be evaluated in hypothesis summary table

### 5. Subject-Level PF-Pupil Coupling
- **Status**: ✅ COMPLETE
- **Script**: `05_subject_level_analysis/07_pf_pupil_subject_coupling.R`
- **Output**: `06_visualization/output/tables/pf_pupil_coupling_correlations.csv`
- **Details**: Correlations between Δpupil and ΔPF parameters (threshold, slope)

---

## ❌ MISSING ANALYSES

### 1. **PF Parameter Statistical Tests (High vs Low Effort)**
- **Status**: ❌ MISSING
- **Hypotheses**: H1a (thresholds higher under High effort), H1b (slopes shallower under High effort)
- **What's Missing**: 
  - Mixed-effects models comparing threshold between High and Low effort
  - Mixed-effects models comparing slope between High and Low effort
  - Tests should be done separately for each task (ADT, VDT) or with task as a factor
- **Required Script**: New script needed: `01_data_preparation/03_test_pf_parameters_effort.R` or similar
- **Expected Output**: 
  - `06_visualization/output/tables/pf_threshold_effort_effects.csv`
  - `06_visualization/output/tables/pf_slope_effort_effects.csv`
  - `06_visualization/output/models/mod_pf_threshold_effort.rds`
  - `06_visualization/output/models/mod_pf_slope_effort.rds`

### 2. **Hypothesis Summary Table Evaluation**
- **Status**: ❌ INCOMPLETE
- **Issue**: The hypothesis summary table in `chapter2_dissertation.qmd` still shows placeholder text:
  - "To be evaluated from PF parameters" (for H1a, H1b)
  - "To be evaluated from effort-pupil check" (for H2a, H2b)
- **What's Needed**: 
  - Update table with actual statistical results
  - Mark hypotheses as "Supported", "Not Supported", or "Partially Supported"
  - Add p-values and effect sizes

---

## 🔍 ISSUES TO INVESTIGATE

### 1. Effort-Pupil Manipulation Check: Cognitive AUC
- **Issue**: Cognitive AUC shows a NEGATIVE effect of High effort (estimate = -0.012, t = -3.86)
- **Expected**: High effort should INCREASE pupil (positive effect)
- **Action Needed**: Verify this result is correct or investigate why cognitive AUC decreases with effort
- **Possible Explanations**:
  - Cognitive AUC window definition may capture different dynamics than Total AUC
  - Baseline correction issues
  - Window timing relative to effort period

### 2. PF Parameters Source
- **Question**: Are PF parameters from Psignifit 4 (MATLAB) or from R script?
- **Current**: `ch2_pf_parameters.csv` appears to be from R script (`02_compute_pf_parameters.R`)
- **Documentation**: Methods section mentions Psignifit 4, but R script uses probit GLM
- **Action Needed**: Clarify which method was actually used, or ensure consistency

---

## 📋 ACTION ITEMS

1. **URGENT**: Create script to test PF parameters (threshold/slope) between High vs Low effort
2. **URGENT**: Update hypothesis summary table with actual results
3. **HIGH**: Investigate cognitive AUC negative effect in effort-pupil check
4. **MEDIUM**: Verify PF parameter computation method matches documentation
5. **LOW**: Add p-values to effort-pupil effects tables (currently only has statistic)

---

## 📊 SUMMARY

**Completed**: 5/6 major analyses
**Missing**: 1 critical analysis (PF parameter effort comparison)
**Issues**: 2 items need investigation

**Next Steps**:
1. Create missing PF parameter comparison script
2. Run the script to generate results
3. Update hypothesis summary table
4. Investigate cognitive AUC anomaly

