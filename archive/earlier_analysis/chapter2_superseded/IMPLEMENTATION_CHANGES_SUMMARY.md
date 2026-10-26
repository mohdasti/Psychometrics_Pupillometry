# Summary of Changes from chapter2_materials to Dissertation QMD

**Date**: January 2026  
**Purpose**: Document all changes needed in `chapter2_dissertation.qmd` based on new materials

---

## 🔴 CRITICAL CHANGES REQUIRED

### 1. Cognitive AUC Window Definition (MAJOR CHANGE)

**CURRENT IN DISSERTATION** (Line 456):
- "Baseline-corrected task-evoked measure computed in a fixed-duration post-target window (e.g., from 300 ms after target onset through a fixed 1 s window)"

**SHOULD BE** (Based on `COGNITIVE_AUC_WINDOW_IMPLEMENTATION.md`):
- **Primary Window**: 4.85s to 6.05s (1.20s fixed window, stimulus-locked)
- **Start**: `t_target + 0.50s = 4.85s` (relative to squeeze onset)
- **End**: `t_target + 1.70s = 6.05s` (relative to squeeze onset)
- **Duration**: Fixed **1.20 seconds** (not variable)
- **Expected samples**: ~300 samples at 250 Hz

**Rationale for Change**:
- Previous 50ms window (4.65-4.70s) was too short (~10-15 samples) → unreliable AUC estimates
- New 1.20s window captures TEPR peak (~1s post-stimulus) while avoiding RT confounds
- Based on expert recommendations (Mathôt 2022, Bauer et al. 2021)

**Action Required**:
- Update line 456 to specify exact window: "4.85s to 6.05s (1.20s fixed window, stimulus-locked)"
- Remove vague "e.g., from 300 ms after target onset through a fixed 1 s window"
- Add rationale: "Captures TEPR peak (~1s post-stimulus) while avoiding early reflex components and RT confounds"

---

### 2. Primary Metric: Use `cog_mean` Instead of Raw `cog_auc`

**CURRENT IN DISSERTATION**:
- Uses `cog_auc` (raw AUC) as primary metric

**SHOULD BE** (Based on `IMPLEMENTATION_SUMMARY.md`):
- **Primary metric**: `cog_mean = cog_auc / window_duration` (mean dilation)
- **Rationale**: Removes duration confounds, preferred over raw AUC

**Action Required**:
- Update methods section to specify `cog_mean` as primary metric
- Update all analysis code to use `cog_mean` instead of `cog_auc`
- Add note: "Mean dilation (AUC/duration) is preferred over raw AUC to remove duration confounds"

---

### 3. Quality Thresholds Update

**CURRENT IN DISSERTATION** (Lines 462-464):
- Primary tier: Window validity ≥ 0.60 for baseline and cognitive windows
- Lenient tier: Window validity ≥ 0.50
- Strict tier: Window validity ≥ 0.70

**SHOULD BE** (Based on `FILTERING_GUIDE.md` and `COGNITIVE_AUC_WINDOW_IMPLEMENTATION.md`):
- **Primary tier**: 
  - `B1_quality >= 0.50` AND `cog_quality >= 0.60` (unchanged)
  - **PLUS gap-aware filters**:
    - `cog_auc_max_gap_ms <= 250` (Kret & Sjak-Shie threshold)
    - `cog_window_duration >= 0.90` (75% of 1.20s window)
    - `cog_auc_n_valid >= 240` (80% of expected ~300 samples)

**Action Required**:
- Update quality tier definitions to include gap-aware metrics
- Add explanation of gap-aware filtering rationale
- Update filtering code in analysis sections

---

### 4. Confound Mitigation: Motor/Response Screen Contamination

**CURRENT IN DISSERTATION**:
- Line 1285 mentions "fixed-duration post-target window to reduce RT confounding"
- No mention of motor/response screen contamination

**SHOULD BE** (Based on `CONFOUND_MITIGATION_STRATEGY.md`):
- **Problem**: Response screen appears at 4.70s (only 350ms after target at 4.35s)
- **Problem**: Button press overlaps with cognitive window for many trials (median RT ~0.6-0.7s → press at ~5.3-5.4s)
- **Mitigation Strategies**:
  1. **Motor buffer truncation**: Truncate primary window 150ms before button press
  2. **Pre-response window**: Decision-aligned window that excludes motor execution
  3. **RT as covariate**: Include RT in all models to control for decision state
  4. **Slow-RT sensitivity**: Test on trials where motor can't contaminate

**Action Required**:
- Add new section in Methods: "Confound Mitigation: Motor/Response Screen Contamination"
- Update primary model to include RT as covariate
- Add sensitivity analysis using slow-RT subset (`cog_win_uncontaminated_by_motor == TRUE`)
- Update limitations section to mention motor buffer truncation (when AUC computed)

---

### 5. Gap-Aware QC Metrics Documentation

**CURRENT IN DISSERTATION**:
- No mention of gap-aware QC metrics

**SHOULD BE** (Based on `FILTERING_GUIDE.md`):
- Document the 5 new gap-aware QC metrics:
  1. `cog_auc_max_gap_ms`: Maximum contiguous missing segment (ms)
  2. `cog_window_duration`: Actual duration of cognitive window (seconds)
  3. `cog_auc_n_valid`: Number of valid samples in cognitive window
  4. `cog_auc_n_segments`: Number of contiguous valid segments
  5. `cog_auc_prop_valid`: Proportion of valid samples

**Action Required**:
- Add section in Methods: "Gap-Aware Quality Control Metrics"
- Explain rationale: "Percentage-validity thresholds are necessary but not sufficient. Large contiguous gaps can distort AUC even with acceptable %-valid"
- Reference Kret & Sjak-Shie (2019) recommendation: gaps >250ms should not be interpolated

---

## 🟡 MODERATE CHANGES RECOMMENDED

### 6. Update Quality Tier Rationale

**CURRENT IN DISSERTATION** (Lines 466-468):
- Focuses on lapse-rate considerations

**SHOULD BE**:
- Keep lapse-rate rationale
- **ADD**: Gap-aware filtering rationale (distinguishing genuine low dilation from data quality artifacts)
- **ADD**: Expert-recommended thresholds based on window duration and sample count

---

### 7. Update Limitations Section

**CURRENT IN DISSERTATION** (Lines 1283-1285):
- Mentions "fixed-duration post-target window to reduce RT confounding"
- Mentions "may not capture the most relevant arousal dynamics"

**SHOULD BE**:
- **ADD**: Temporal overlap with response screen (4.70s) and button press (variable)
- **ADD**: Motor buffer truncation available but full AUC computation pending
- **ADD**: Pre-response window definitions available but full AUC computation pending
- **ADD**: RT included as covariate to control for decision state
- **ADD**: Sensitivity analysis on slow-RT subset available

---

### 8. Update Statistical Model Description

**CURRENT IN DISSERTATION** (Lines 498-503):
- Primary GLMM does not include RT as covariate

**SHOULD BE**:
- **ADD RT as covariate**:
  $$
  \text{probit}(P(Y_{ij}=1)) = \beta_0 + \beta_1 X_{ij} + \beta_2 \text{Effort}_{ij} + \beta_3 \text{Modality}_{ij} + \beta_4 P^{(\text{state})}_{ij} + \beta_5 \text{RT}_{ij} + \beta_6 (X_{ij} \times P^{(\text{state})}_{ij}) + \beta_7 P^{(\text{trait})}_{j} + u_{0j} + u_{1j}X_{ij}
  $$

**Action Required**:
- Update model equation to include RT
- Add rationale: "RT included as covariate to control for decision state and reduce RT-pupil coupling confounds"

---

## 🟢 MINOR UPDATES / CLARIFICATIONS

### 9. Update Data Quality Section

- Mention that gap-aware metrics are available for filtering
- Update sample size descriptions to reflect gap-aware filtering

### 10. Update Sensitivity Analysis Section

- Add sensitivity analysis using slow-RT subset
- Add note about motor-buffered window (when AUC computed)

---

## ❓ QUESTIONS FOR CLARIFICATION

### Question 1: Data File Location ✅ ANSWERED
- **Question**: Where is the actual data file (`ch2_triallevel.csv`) located?
- **Answer**: The dissertation loads `ch2_triallevel_merged.csv` from `data/processed/` (line 230)
- **Location**: `07_manuscript/chapter2/data/processed/ch2_triallevel_merged.csv`
- **Note**: This file needs to be updated with new columns from `chapter2_materials/data/ch2_triallevel.csv` (if it exists) or regenerated using updated `make_quick_share_v7.R`

### Question 2: Analysis Code Updates
- **Question**: Do the analysis scripts in `07_manuscript/chapter2/scripts/` need to be updated to use `cog_mean` instead of `cog_auc`?
- **Context**: The dissertation QMD file may call analysis scripts that still use old metrics.

### Question 3: Motor-Buffered AUC Computation
- **Question**: Should we mention motor-buffered AUC in the dissertation even though full computation is pending?
- **Context**: Window definitions are computed, but full AUC requires re-processing flat files.
- **Recommendation**: Mention it as available for future sensitivity analysis, but don't require it for primary analysis.

### Question 4: Pre-Response Window
- **Question**: Should pre-response window be included in primary analysis or only sensitivity?
- **Context**: Pre-response window AUC computation is pending (requires re-processing).
- **Recommendation**: Document it as available for sensitivity analysis, but primary analysis uses stimulus-locked window.

### Question 5: Quality Tier Definitions
- **Question**: Should we keep the three-tier system (lenient/primary/strict) or update to use gap-aware metrics?
- **Context**: New documentation suggests gap-aware filtering should be part of primary tier.
- **Recommendation**: Keep three-tier system but add gap-aware metrics to each tier.

### Question 6: Baseline Quality Threshold
- **Question**: Is `B1_quality >= 0.50` the correct threshold, or should it be `baseline_quality >= 0.50`?
- **Context**: Documentation mentions both `B1_quality` and `baseline_quality`.
- **Need to verify**: Which column name is actually in the data file?

### Question 7: Window Duration in Current Data ⚠️ CRITICAL
- **Question**: The documentation says current implementation uses fixed 1.20s window, but `DECISION_FRAMEWORK_VERIFICATION.md` mentions old 50ms window. Which is correct?
- **Context**: There's a discrepancy between documents. `DECISION_FRAMEWORK_VERIFICATION.md` appears to be outdated (references old 50ms window).
- **Current Status**: According to `IMPLEMENTATION_SUMMARY.md` and `COGNITIVE_AUC_WINDOW_IMPLEMENTATION.md`, the new 1.20s window (4.85-6.05s) has been implemented.
- **Action Required**: **VERIFY** by checking actual data file or re-running `make_quick_share_v7.R` to ensure data has new window implementation.

### Question 8: RT Covariate in Existing Models
- **Question**: Are existing saved models (`mod_effort_cog_auc.rds`, `mod_effort_total_auc.rds`) already including RT as covariate?
- **Context**: If models need to be re-run, this affects results section.
- **Need to verify**: Check model formulas in saved model files.

---

## 📋 IMPLEMENTATION CHECKLIST

### Methods Section Updates
- [ ] Update cognitive AUC window definition (line 456)
- [ ] Add gap-aware QC metrics section
- [ ] Add confound mitigation section
- [ ] Update quality tier definitions to include gap-aware metrics
- [ ] Update statistical model to include RT as covariate
- [ ] Update sensitivity analysis descriptions

### Results Section Updates
- [ ] Verify data quality tables reflect gap-aware filtering
- [ ] Update sample size descriptions
- [ ] Add sensitivity analysis results (slow-RT subset, if available)

### Discussion/Limitations Updates
- [ ] Update limitations section with confound mitigation discussion
- [ ] Mention motor buffer truncation and pre-response windows (when available)
- [ ] Update future directions

### Code Updates
- [ ] Update analysis code to use `cog_mean` instead of `cog_auc`
- [ ] Add gap-aware filtering to data preparation
- [ ] Add RT as covariate to models
- [ ] Add sensitivity analysis code (slow-RT subset)

---

## 📚 KEY REFERENCES TO ADD

1. **Mathôt (2022)**: Methods in cognitive pupillometry - Timing, RT/response confounds
2. **Bauer et al. (2021)**: Pupil dilation peaks ~1-2s post-stimulus
3. **Kret & Sjak-Shie (2019)**: Gap handling recommendations (≤250ms)
4. **de Gee et al. (2014)**: Decision-aligned pupil dynamics
5. **Urai et al. (2017)**: Post-choice pupil relates to decision uncertainty

---

**Last Updated**: January 2026  
**Status**: Ready for implementation after clarification of questions above
