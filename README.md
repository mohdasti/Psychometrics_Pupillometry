# Squeeze Harder, Say "Same": Physical Effort Shifts Older Adults' Perceptual Criterion

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21215384.svg)](https://doi.org/10.5281/zenodo.21215384)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](LICENSE)
[![Last commit](https://img.shields.io/github/last-commit/mohdasti/Psychometrics_Pupillometry)](https://github.com/mohdasti/Psychometrics_Pupillometry/commits/main)
[![R](https://img.shields.io/badge/R-4.6.1-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-1.9.38-0F4C75?logo=quarto&logoColor=white)](https://quarto.org/)
[![lme4](https://img.shields.io/badge/lme4-2.0.1-276DC3?logo=r&logoColor=white)](https://cran.r-project.org/package=lme4)
[![lmerTest](https://img.shields.io/badge/lmerTest-3.2.1-276DC3?logo=r&logoColor=white)](https://cran.r-project.org/package=lmerTest)

Chapter 2 of the dissertation. Sixty-eight older adults judged whether two tones or two visual gratings were the same or different while holding a handgrip at low (5%) or high (40%) physical effort, with pupil diameter recorded throughout. The chapter tests whether high physical effort changes the criterion for reporting a difference, the sensitivity of the judgment, or both, and whether pupil size during the grip or around the probe tracks those effects.

The chapter source is [`07_manuscript/chapter2/reports/chapter2_dissertation.qmd`](07_manuscript/chapter2/reports/chapter2_dissertation.qmd). Every reported number is read from the export `07_manuscript/chapter2/export/ch2fit_20261024_193248/`. That export is the accepted run. It is not refit when the chapter is rendered.

An earlier deposit of code and de-identified data is archived at [https://doi.org/10.5281/zenodo.21215384](https://doi.org/10.5281/zenodo.21215384). The chapter in this repository is the version on `main`.

---

## Render the chapter

From `07_manuscript/chapter2/reports`:

```bash
quarto render chapter2_dissertation.qmd --to pdf
quarto render chapter2_dissertation.qmd --to docx
```

The YAML default is the dissertation. Both versions end with References and then Supplementary Materials, which has two tiers. The manuscript tier, in both versions, holds the pupil sample and the missingness model (M5), model estimation with the time-axis rule and the random-effects steps, and the robustness analysis R1. The dissertation tier adds the optimizer check, R2, and the provenance of the pupil recordings with the time-axis table. The paper version omits the dissertation tier:

```bash
quarto render chapter2_dissertation.qmd --to pdf -M dissertation:false --output chapter2_manuscript.pdf
quarto render chapter2_dissertation.qmd --to docx -M dissertation:false --output chapter2_manuscript.docx
```

Render the paper version under its own filename so that it does not overwrite the dissertation output.

PDF uses XeLaTeX with STIX Two Text and STIX Two Math. Word supplement numbers are applied by `ch2_docx_supptables.lua` after Quarto writes the file. Pass a bare filename to `--output` and put the directory in `--output-dir`.

The render reads `values.json`, the coefficient and table CSVs, and the two figures in the export. It also needs `07_manuscript/chapter2/output/figures/Paradigm.png` and `R/ch2_report_helpers.R`.

---

## Models

Fits use lme4 2.0.1 and lmerTest 3.2.1, with bobyqa and a maximum of 200,000 evaluations. A fit is accepted only when it is not singular and the optimizer returns no convergence message. Random effects are reduced in fixed steps when that rule fails. The steps are in the supplement. M1–M4 are in the main text; M5, R1, and the estimation details are in the supplement.

| ID | Question |
|----|----------|
| M1 | Behavior. Probit model of "different" responses. Physical effort and catch × physical effort test the criterion; the sign of offset × physical effort separates the sensitivity account from the capacity account. |
| M2 | Grip-period pupil. Whether high physical effort enlarged the pupil during the grip. |
| M3 | Task-evoked pupil. Whether the TEPR differs with physical effort and with offset. |
| M4a | Pre-probe pupil and the slope of the judgment. |
| M4b | Task-evoked pupil and the slope of the judgment. |
| M5 | Missingness control, reported in the supplement. Whether a matched trial passed the task-evoked checks. |
| R1 | Robustness. M4b with the task-evoked area in place of the mean. |
| R2 | Robustness. M1's formula in the pupil subsample. This fit was not accepted. |

R1 and R2 are display labels. The saved fit IDs are S1 and S2. Formulas, windows, and coding are in the Methods of the chapter.

---

## Analysis scripts

The current chapter was built in this order. Each script takes a run id except the Phase 3 scripts, which use `ch2fit_20261024_193248`.

| Step | Script | What it does |
|------|--------|----------------|
| Pupil rebuild | `scripts/phase2a/ch2_phase2a_rebuild.R` | Rebuilds pupil features. Fits no model. |
| Time axis | `scripts/phase2a1/ch2_axis_check.R` | Read-only check of the sampling interval. |
| 250 Hz rebuild | `scripts/phase2a2/ch2_rebuild_250hz.R` | Rebuilds features on the fixed 250 Hz axis. Fits no model. |
| Analysis tables | `scripts/phase2b/01_build_analysis_data.R` | Builds the M1–M5 analysis tables. Stops if a sample size does not match. |
| Fits | `scripts/phase2b/02_fit_models.R` | Fits M1–M5, S1, and S2. Does not refit an accepted model. |
| Coding tests | `tests/test_ch2_coding.R` | Checks the contrast coding. |
| Export | `scripts/phase3/01_export.R` | Writes the chapter export from the saved fits. The only model call is `predict()` on the saved M1 fit. |
| Checks | `scripts/phase3/02_checks.R` | Checks the export, references, and render. Does not refit and does not edit the chapter. |

`scripts/phase3/01_export_chapter_values.R` is an earlier exporter for the same run. The chapter reads the export written by `scripts/phase3/01_export.R`.

Accepted model objects from the locked run are written under `output/models/ch2_phase2b/ch2fit_20261024_193248/`. Diagnostics are under `output/diagnostics/`. Those directories are local. Rendering the chapter uses the tracked export, not the model objects.

The rebuild and the fits read trial files under `data/` and, for the axis check, the processed pupil recordings. Those inputs are not all tracked. Paths inside the phase scripts point at this machine.

`archive/earlier_analysis/` holds the earlier analysis (quality tiers, equivalence tests, and subject-level correlations), including `run_analysis.R` and the numbered folders `01_data_preparation` through `06_visualization`. Those scripts were written for the repository root. They do not produce the chapter that is rendered from `ch2fit_20261024_193248`.

---

## Repository layout

```
Psychometrics_Pupillometry/
├── 07_manuscript/chapter2/
│   ├── reports/
│   │   ├── chapter2_dissertation.qmd
│   │   ├── ch2_docx_supptables.lua
│   │   ├── references.bib
│   │   └── apa-7th-edition.csl
│   ├── export/ch2fit_20261024_193248/   # values, tables, and figures the chapter reads
│   └── output/figures/Paradigm.png
├── R/                                   # current helpers below; other files belong to the earlier analysis
│   ├── ch2_report_helpers.R
│   ├── tepr_waveform_helpers.R
│   ├── trial_time_axis.R
│   └── trial_time_axis_250hz.R
├── scripts/
│   ├── phase2a/
│   ├── phase2a1/
│   ├── phase2a2/
│   ├── phase2b/
│   └── phase3/
├── tests/test_ch2_coding.R
└── archive/earlier_analysis/            # previous pipeline and working notes
```

---

## Requirements

- R 4.6.1
- Quarto 1.9.38
- XeLaTeX, STIX Two Text, and STIX Two Math, for PDF
- R packages used by the chapter and the phase scripts: `lme4`, `lmerTest`, `data.table`, `ggplot2`, `jsonlite`, `knitr`, `here`

```r
install.packages(c(
  "lme4", "lmerTest", "data.table", "ggplot2",
  "jsonlite", "knitr", "here"
))
```

---

## License

Copyright (C) 2025–2026 Mohammad Dastgheib.

This repository is licensed under the GNU General Public License v3.0. See [LICENSE](LICENSE).

---

## Contact

**Author:** Mohammad Dastgheib  
**Institution:** University of California, Riverside  
**Department:** Department of Psychology, Cognition & Cognitive Neuroscience Area

---

## Citation

If you use the archived deposit, cite:

```bibtex
@software{psychometrics_pupillometry_2026,
  author  = {Dastgheib, Mohammad},
  title   = {Psychometrics\_Pupillometry: Pupil-Indexed Arousal and Psychometric Sensitivity in Older Adults},
  year    = {2026},
  url     = {https://github.com/mohdasti/Psychometrics_Pupillometry},
  doi     = {10.5281/zenodo.21215384}
}
```
