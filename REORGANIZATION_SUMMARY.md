# Project Reorganization Summary

**Date:** December 2024  
**Purpose:** Reorganized project structure to follow numbered folder pattern (inspired by `modeling-pupil-DDM`)

---

## New Structure

The project has been reorganized from a flat `07_manuscript/chapter2/` structure into numbered analysis stages:

```
Psychometrics_Pupillometry/
├── 01_data_preparation/          # Data loading, validation, PF parameters
├── 02_pupil_quality_control/     # Quality tiers, missingness diagnostics
├── 03_effort_manipulation_check/ # Effort-pupil manipulation check
├── 04_pupil_psychometric_coupling/ # PRIMARY ANALYSIS: GLMM coupling
├── 05_subject_level_analysis/    # Subject-level correlations
├── 06_visualization/             # Figure generation
├── 07_manuscript/                # Manuscript and reports
├── data/                         # Shared data directory (moved from chapter2/data)
├── config/                       # Configuration files (moved from chapter2/config)
└── run_analysis.R               # Master script (moved to root)
```

---

## Changes Made

### 1. Scripts Reorganized by Analysis Stage

**Before:** All scripts in `07_manuscript/chapter2/scripts/`

**After:** Scripts distributed across numbered folders:
- `01_data_preparation/`: Scripts 01-02, `make_quick_share_v7.R`
- `02_pupil_quality_control/`: Scripts 03, 05
- `03_effort_manipulation_check/`: Script 04
- `04_pupil_psychometric_coupling/`: Script 06 (primary analysis)
- `05_subject_level_analysis/`: Script 07
- `06_visualization/`: Script 08

### 2. Data Directory Moved to Root

**Before:** `07_manuscript/chapter2/data/`

**After:** `data/` at repository root (shared across all analysis stages)
- `data/raw/behavioral/` - Raw behavioral data
- `data/raw/pupil/` - Raw pupil data
- `data/processed/` - Processed data files
- `data/qc/` - Quality control outputs

### 3. Configuration Centralized

**Before:** `07_manuscript/chapter2/config/paths_config.R`

**After:** `config/paths_config.R` at repository root
- Updated to reflect new structure
- All scripts now source this file for paths
- Defines paths for all numbered folders and shared data

### 4. Output Directories

Each numbered folder now has its own `output/` directory:
- `01_data_preparation/output/`
- `02_pupil_quality_control/output/`
- `03_effort_manipulation_check/output/`
- `04_pupil_psychometric_coupling/output/`
- `05_subject_level_analysis/output/`
- `06_visualization/output/` (contains shared `figures/`, `tables/`, `models/`)

### 5. Reports and Documentation

**Before:** `07_manuscript/chapter2/reports/` and `07_manuscript/chapter2/docs/`

**After:** 
- Reports remain in `07_manuscript/chapter2/`
- Documentation in `07_manuscript/chapter2/docs/`

### 6. Master Script Updated

**Before:** `07_manuscript/chapter2/run_analysis.R`

**After:** `run_analysis.R` at repository root
- Updated to run scripts from numbered folders
- Maintains execution order by analysis stage

---

## Script Updates

All scripts have been updated to:
1. Source `config/paths_config.R` instead of defining paths locally
2. Use centralized path variables (e.g., `merged_trial_file`, `figures_dir`)
3. Work with the new directory structure

**Key path variables (from `paths_config.R`):**
- `merged_trial_file` - Main processed data file
- `pf_params_file` - PF parameters file
- `figures_dir` - Figures output (points to `06_visualization/output/figures/`)
- `tables_dir` - Tables output
- `models_dir` - Models output
- `qc_dir` - Quality control outputs

---

## Migration Notes

### For Existing Users

1. **Update your working directory:** Run scripts from repository root (not `07_manuscript/chapter2/`)
2. **Data location:** Data is now in `data/` at root (not `07_manuscript/chapter2/data/`)
3. **Script paths:** Scripts are now in numbered folders (e.g., `01_data_preparation/01_load_and_validate_data.R`)
4. **Outputs:** Check `06_visualization/output/` for final figures and tables

### Running Analyses

**From repository root:**
```r
source("run_analysis.R")
```

**Or run individual stages:**
```r
source("01_data_preparation/01_load_and_validate_data.R")
source("02_pupil_quality_control/03_pupil_quality_tiers.R")
# etc.
```

---

## Benefits of New Structure

1. **Clear analysis stages:** Each numbered folder represents a distinct analysis phase
2. **Better organization:** Related scripts grouped together by purpose
3. **Shared resources:** Data and config centralized at root
4. **Scalability:** Easy to add new analysis stages (e.g., `08_...`)
5. **Consistency:** Matches pattern from `modeling-pupil-DDM` project

---

## Files Preserved

- All original scripts (moved, not deleted)
- All data files (moved to shared `data/` directory)
- All documentation (preserved in `07_manuscript/chapter2/docs/`)
- All reports (preserved in `07_manuscript/chapter2/`)

---

## Next Steps

1. Test the pipeline: Run `source("run_analysis.R")` to verify everything works
2. Update any external references: If other projects reference this structure, update paths
3. Review outputs: Check that all outputs are generated in expected locations

---

## Questions?

If you encounter issues:
1. Check `config/paths_config.R` for path definitions
2. Ensure you're running scripts from repository root
3. Verify data files are in `data/` directory
4. Check script paths match the new structure

