# Chapter 2 cross-chapter check

No model was refitted, and no exported value was recomputed. Nothing was pushed.

## Missingness-model trial count

M5 was fitted on `output/diagnostics/ch2_phase2b/ch2fit_20261024_193248/analysis_data_m5.csv`. That table was built from `data/analysis_ready/ch2_r2/ch2r2_20261024_140108/ch2_pupil_trials_r2.csv` by keeping rows with `matched == TRUE`. The pupil-file checksum in the Phase 2b build log is `7d3f64607eb5ac947155d5cffc4e8cff`, which is the checksum of that file now. The analysis-table checksum `ca72ef3b424e539d89658020062c23be` matches `data_md5` in `M5_meta.json`. That metadata records 12,630 trials (6,319 auditory, 6,311 visual). The saved fit used `in_tepr_final ~ effort_c * task_c + catch + x_c + (1 | subject)`.

`matched == TRUE` is the same set as “a pupil file was present” in this export (12,630 rows, and the two sets do not differ). The 250 Hz acceptance rule is a later filter: `time_rule == "fixed_250hz_from_m3"`. That rule keeps 12,626 rows in `ch2_pupil_trials_r2.csv` and the same 12,626 keys in `ch3_compatible_pupil_r2.csv`. The Chapter 3 export also contains the other four rows, with `time_rule == "refused"`, so they are in the file and outside the accepted count.

Those four trials are visual trials from the December 21 files. Each one matched a recording, and each median time step is within \(10^{-6}\) of 0.004 s. The axis check refused them because the sample count is not 3,425 or 3,426 (`refuse_class = n_not_3425_or_3426`). They are in the 12,630 and not in the 12,626. All four have `g_tepr_final = FALSE`, so in M5 they are trials that did not pass the task-evoked gate.

| Subject | Task | Session | Run | Trial | Samples |
| --- | --- | --- | --- | --- | --- |
| BAP191 | VDT | 3 | 1 | 24 | 2,124 |
| BAP194 | VDT | 2 | 1 | 22 | 1,932 |
| BAP194 | VDT | 2 | 1 | 23 | 1,373 |
| BAP195 | VDT | 3 | 4 | 20 | 1,736 |

The 12,630 count is the right count for M5’s definition: trials matched to a recording, before the time-axis check. The Methods text was not edited. One clause that says so:

> The missingness model uses the 12,630 trials that could be matched to a pupil recording before the time-axis check (61 participants), of which 6,299 passed the task-evoked gate.

## Render checks

`chapter2_dissertation.pdf` and `chapter2_dissertation.docx` were rendered with Quarto. `pubstate = {inpress}` renders the date as “(in press)”, so the year field was left unchanged.

Both renders contain these sentences once each:

- Harder trials were less accurate in every task and slower in both discrimination tasks, and high physical effort slowed responses in the visual discrimination task, but physical effort and task difficulty did not interact on any measure.
- saw or heard a standard and then a comparison stimulus, referred to here as the probe (the target in Chapter 1), judged whether the two were the same or different,
- Chapter 1 (Dastgheib et al., in press) tested this account in younger adults
- Chapter 1 (Dastgheib et al., in press) found no such interaction in younger adults,

The reference entry is:

> Dastgheib, M., Sun, A. Y., Yaghoubi, K. C., Zhang, W., Peters, M. A. K., Bennett, I. J., & Seitz, A. R. (in press). Effects of concurrent task difficulty and physical effort on memory and perceptual performance and pupil response in younger adults. *Cognitive Research: Principles and Implications*.

The journal title is italic in the PDF (STIX Two Text Italic) and in the docx. “(Dastgheib et al., in press)” appears twice, and “(in press)” appears once more, in the reference list. Later mentions of Chapter 1 are uncited. A fourth occurrence of the surname is the existing Sun et al. reference, where Dastgheib is a coauthor. Neither render contains “??” or an unresolved “@”.

Numeric tokens were taken from the previous PDF (copied before this render) and from the new PDF, after dropping each page-number footer and ignoring line breaks. The new PDF has 1,212 tokens and the previous PDF has 1,211. The only difference is one extra “1”, the digit in the required phrase “the target in Chapter 1”, which falls between “40%” and “1.5–4.5 s”. Every other token is unchanged, including 12,630.
