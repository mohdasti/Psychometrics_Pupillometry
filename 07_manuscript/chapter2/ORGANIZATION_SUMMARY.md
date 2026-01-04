# Chapter 2 Project Organization Summary

**Date:** December 2024  
**Purpose:** Summary of project reorganization integrating `chapter2_materials` and behavioral data from `/Users/mohdasti/Documents/LC-BAP/BAP/Nov2025/`

---

## What Was Organized

### 1. Pupil Materials Integration

Materials from `/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry/chapter2_materials/` have been integrated into the main Chapter 2 structure:

#### Documentation Files
- **Location:** `07_manuscript/chapter2/docs/`
- **Files moved:**
  - `TIMESTAMP_DEFINITIONS.md` - Task timing definitions
  - `AUC_CALCULATION_METHOD.md` - AUC calculation methodology
  - `PUPIL_DATA_REPORT_PROMPT.md` - Data structure documentation
  - `CHAPTER2_SETUP_PROMPT.md` - Detailed setup guide
  - `CHAPTER2_SETUP_PROMPT_CONCISE.md` - Concise setup guide

#### Pupil Data Files
- **Location:** `07_manuscript/chapter2/data/processed/`
- **Files:**
  - `ch2_triallevel_pupil.csv` - Pupil-processed data with AUC metrics
  - `README_data_source.md` - Documentation of data source and processing

#### Scripts
- **Location:** `07_manuscript/chapter2/scripts/`
- **Files:**
  - `make_quick_share_v7.R` - Script to regenerate pupil data if needed

#### Reports
- **Location:** `07_manuscript/chapter2/reports/`
- **Files:**
  - `pupil_data_report_advisor.qmd` - Comprehensive data quality report

### 2. Behavioral Data Organization

Behavioral data files from `/Users/mohdasti/Documents/LC-BAP/BAP/Nov2025/` are already organized in:

**Location:** `07_manuscript/chapter2/data/raw/behavioral/`

**Files:**
- `bap_beh_trialdata_v2.csv` - Trial-level behavioral data (trial-by-trial responses)
- `bap_beh_subjxtaskdata_v2.csv` - Subject-level summaries by task
- `bap_beh_trialdata_v2_trials_per_subject_per_task.csv` - Trial counts per subject/task
- `bap_beh_trialdata_v2_report.txt` - Data report
- `LC Aging Subject Data master spreadsheet - behavioral.csv` - PF parameters (thresholds, slopes)
- `LC Aging Subject Data master spreadsheet - behavioral data dictionary.csv` - Data dictionary
- `LC Aging Subject Data master spreadsheet - demographics.csv` - Demographics
- `LC Aging Subject Data master spreadsheet - neuropsych.csv` - Neuropsychology data

---

## Updated Configuration

### Paths Configuration
- **File:** `07_manuscript/chapter2/config/paths_config.R`
- **Updates:**
  - Added paths for documentation directory (`docs_dir`)
  - Added paths for pupil data files (`pupil_trial_file`, `pupil_data_readme`)
  - Added path for pupil data generation script (`make_pupil_data_script`)
  - Added path for pupil data report (`pupil_data_report_file`)

### Documentation Updates
- **`07_manuscript/chapter2/README.md`** - Updated to reflect new directory structure and data locations
- **`README.md`** (root) - Updated repository structure section

---

## Current Directory Structure

```
07_manuscript/chapter2/
├── data/
│   ├── raw/
│   │   ├── behavioral/          # Behavioral data from LC-BAP/BAP/Nov2025/
│   │   └── pupil/               # Raw pupil data (if separate)
│   ├── processed/               # Processed data files
│   │   ├── ch2_triallevel_merged.csv
│   │   ├── ch2_triallevel_pupil.csv
│   │   └── README_data_source.md
│   └── qc/                      # Quality control outputs
├── scripts/                     # Analysis scripts (01-08 + make_quick_share_v7.R)
├── docs/                        # Documentation (from chapter2_materials)
├── output/
│   ├── figures/
│   ├── tables/
│   └── models/
├── reports/                     # Quarto reports
│   ├── chapter2_dissertation.qmd
│   └── pupil_data_report_advisor.qmd
├── config/
│   └── paths_config.R          # Updated with new paths
└── README.md                   # Updated documentation
```

---

## Data Sources

### Behavioral Data
- **Source:** `/Users/mohdasti/Documents/LC-BAP/BAP/Nov2025/`
- **Status:** ✅ Already organized in `data/raw/behavioral/`
- **Contains:** Trial-by-trial data, psychometric function parameters, demographics, neuropsych data

### Pupil Materials
- **Source:** `/Users/mohdasti/Documents/GitHub/Psychometrics_Pupillometry/chapter2_materials/`
- **Status:** ✅ Integrated into Chapter 2 structure
- **Contains:** Processed pupil data, documentation, scripts, reports

---

## Next Steps

1. **Verify data paths:** Ensure all scripts reference the correct paths using `paths_config.R`
2. **Test analysis pipeline:** Run `run_analysis.R` to verify everything works with new organization
3. **Update any hardcoded paths:** Check scripts for any hardcoded paths that need updating
4. **Documentation review:** Review updated README files for accuracy

---

## Notes

- The `chapter2_materials/` folder remains in the repository root as a reference/backup
- All behavioral data files are already in place and match the expected structure
- Paths configuration has been updated to include all new materials
- Documentation has been updated to reflect the new organization

