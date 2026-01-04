# Effort Condition NA Fix

**Date:** December 2024  
**Issue:** NA values in effort condition variable  
**Status:** ✅ Fixed

---

## Problem

The `effort` condition variable should only contain "Low" or "High" values, but some rows had `NA` values. This could occur when:

1. **Missing behavioral data:** When merging pupil data with behavioral data, some trials might not have corresponding behavioral records, leading to missing effort information.

2. **Merge issues:** During the data merge process, if the join between behavioral and pupil data fails for some trials, effort values could be missing.

3. **Data source inconsistencies:** If effort is derived from multiple sources (e.g., `is_high_grip`, `grip_level`, `grip_targ_prop_mvc`) and these sources are inconsistent or missing.

---

## Solution

Added comprehensive effort validation and derivation logic in `01_data_preparation/01_load_and_validate_data.R`:

### 1. **Standardization**
- Convert all effort values to title case ("Low", "High")
- Handle common variations (e.g., "low" → "Low", "HIGH" → "High")

### 2. **NA Detection and Derivation**
When NAs are found, the script attempts to derive effort from alternative sources in this order:

1. **`is_high_grip`** (boolean): 
   - `TRUE` → "High"
   - `FALSE` → "Low"

2. **`grip_level`** (character):
   - "high" → "High"
   - "low" → "Low"

3. **`grip_targ_prop_mvc`** (numeric):
   - `>= 0.20` (20% MVC) → "High"
   - `< 0.20` → "Low"

### 3. **Cleanup**
- Remove any rows that still have NA effort after derivation attempts
- Remove any rows with invalid effort values (not "Low" or "High")
- Report the number of rows fixed/removed

---

## Implementation Details

The fix is implemented in the data loading script (`01_data_preparation/01_load_and_validate_data.R`) in the "DATA CLEANING" section, right after task label standardization.

**Key features:**
- ✅ Detects NAs before and after derivation attempts
- ✅ Reports how many rows were fixed using each method
- ✅ Removes rows that cannot be fixed (with warning)
- ✅ Validates final effort values are only "Low" or "High"
- ✅ Reports final effort counts

---

## Verification

After running the fix:

```r
# Check effort values
table(dat$effort, useNA = "always")
# Should show only "Low" and "High", with 0 NAs

# Verify no invalid values
sum(!dat$effort %in% c("Low", "High"))
# Should be 0
```

**Current status:** ✅ All effort values are valid (0 NAs, 0 invalid values)

---

## Prevention

To prevent NAs from occurring in the future:

1. **Ensure complete behavioral data:** When merging behavioral and pupil data, ensure all trials have corresponding behavioral records.

2. **Run validation script:** Always run `01_load_and_validate_data.R` after any data merge or update to catch and fix any NAs.

3. **Check data sources:** Verify that source files (behavioral CSV, pupil data) have complete effort information before merging.

---

## Files Modified

- `01_data_preparation/01_load_and_validate_data.R` - Added effort NA detection and derivation logic

---

## Usage

The fix runs automatically when you execute:

```r
source("01_data_preparation/01_load_and_validate_data.R")
```

The script will:
1. Load the data
2. Detect any NA effort values
3. Attempt to derive effort from alternative sources
4. Remove rows that cannot be fixed
5. Validate final effort values
6. Save the cleaned dataset

---

## Notes

- Rows with NA effort that cannot be derived are **removed** from the dataset
- This ensures data quality but may reduce sample size slightly
- The script reports how many rows were fixed/removed for transparency
- All downstream analyses will now have only valid effort values ("Low" or "High")

