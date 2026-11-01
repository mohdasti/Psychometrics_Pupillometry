# Chapter 2 cross-chapter check

No model was refitted, and no exported value was recomputed. Nothing was pushed.

Steps 1–3 were already in the chapter. Each original find string occurs 0 times, and each replacement occurs once. The bibliography key `dastgheib_inpress_effects` occurs once. Those sentences were left as they were.

## Missingness-model trial count

M5 was fitted on `output/diagnostics/ch2_phase2b/ch2fit_20261024_193248/analysis_data_m5.csv`, built from `data/analysis_ready/ch2_r2/ch2r2_20261024_140108/ch2_pupil_trials_r2.csv` with `matched == TRUE`. The pupil-file checksum `7d3f64607eb5ac947155d5cffc4e8cff` matches the Phase 2b build log, and the analysis-table checksum `ca72ef3b424e539d89658020062c23be` matches `M5_meta.json` (`nobs` 12,630: 6,319 auditory, 6,311 visual).

The 250 Hz export `ch3_compatible_pupil_r2.csv` has the same 12,626 keys with `time_rule == "fixed_250hz_from_m3"`. The four extra M5 trials matched a recording and failed the sample-count check (`n` not 3,425 or 3,426). Their median step is within \(10^{-6}\) of 0.004 s. They are in the Chapter 3 file with `time_rule == "refused"`, and in M5 with `g_tepr_final = FALSE`.

| Subject | Task | Session | Run | Trial | Samples |
| --- | --- | --- | --- | --- | --- |
| BAP191 | VDT | 3 | 1 | 24 | 2,124 |
| BAP194 | VDT | 2 | 1 | 22 | 1,932 |
| BAP194 | VDT | 2 | 1 | 23 | 1,373 |
| BAP195 | VDT | 3 | 4 | 20 | 1,736 |

12,630 is the right count for M5: trials matched to a recording, before the time-axis check. The Methods sentence was not edited. A clause that says so:

> The missingness model uses the 12,630 trials that could be matched to a pupil recording before the time-axis check (61 participants), of which 6,299 passed the task-evoked gate.

## Terminology

Before editing, the bare terms and the phrases in the concept table were on these lines: 4, 211, 213, 215, 217, 219, 221, 265, 273, 292, 302, 306, 308, 310, 312, 314, 320, 322, 326, 335, 339, 343, 352, 354, 384, 386, 388, 392, 394, 546, and 690. Code, inline keys, and `effort_c` were not in that list.

Thirty-four explicit substitutions were applied, including five `grip-window` and one `Grip-window`. A following pass changed bare `effort` on 24 prose or caption lines, skipping code fences and backtick spans. Five `offset-by-physical-effort` phrases and two `catch-by-physical-effort` phrases were then written as `offset × physical effort` and `catch × physical effort`, so the word `effort` is preceded by `physical`. Three random-effects display labels were changed the same way. The formula strings on the left of those labels were not changed. Table cells that read `Effort` or `Grip-window pupil` are relabeled when the table is drawn.

### Abstract

- “while holding a light or a strong handgrip” → “while holding a handgrip at low (5%) or high (40%) physical effort”
- “Under the strong grip,” → “Under high physical effort,”
- “but discrimination did not become less precise overall” → “but sensitivity did not decline overall”
- “differed little between grip levels” → “differed little between physical effort levels”
- “The pupil response after the second stimulus was smaller under the strong grip,” → “The pupil response after the probe, the second stimulus, was smaller under high physical effort,”
- “A larger pupil just before the second stimulus was weakly associated with less precise discrimination,” → “A larger pupil just before the probe was weakly associated with lower sensitivity,”

### Introduction

- Deleted “In this chapter, effort refers to physical effort throughout.”
- “the difficulty of the perceptual judgment” → “the task difficulty of the perceptual judgment”
- “depends on the difficulty of the judgment” → “depends on task difficulty”
- “Effort could change” → “Physical effort could change”; “that effort degrades” → “that physical effort degrades”
- “a strong grip draw on the same pool, the cost of the grip” → “high physical effort draw on the same pool, its cost”
- “a strong grip lowers” → “high physical effort lowers”
- “The task-evoked pupillary response (TEPR) follows” → “The task-evoked pupil response (TEPR) follows”
- “a strong grip raised thresholds” → “high physical effort raised thresholds”
- “With 75 trials per grip level, however, an individual slope is estimated imprecisely.” → “With 75 trials per physical effort level, however, a single participant's slope is uncertain.”
- “A strong grip shifted” → “High physical effort shifted”
- “the effect of effort changes” → “the effect of physical effort changes”
- “how a strong grip changes” → “how high physical effort changes”
- “an effect of effort” → “an effect of physical effort”; “offset-by-effort” → “offset × physical effort”
- “differs between grip levels” → “differs between physical effort levels”; “differs with effort” → “differs with physical effort”

### Methods

- “The grip-window model uses” → “The grip-period model uses”
- Figure caption: “Low effort is 5% and high effort is 40%” → “Low physical effort is 5% and high physical effort is 40%”
- “The grip-window mean” → “The grip-period mean”
- “The task-evoked pupillary response (TEPR) is the mean dilation” → “The task-evoked pupil response (TEPR) is the mean dilation”
- “Effort is sum-coded” → “Physical effort is sum-coded”; “A coefficient on effort” → “A coefficient on physical effort”
- Model descriptions now say “physical effort” where they said “effort”, and “Grip-period pupil (M2).”
- “offset-by-effort interaction” → “offset × physical effort interaction”
- Pupil-flow caption: “The grip-window model applies” → “The grip-period model applies”

### Results

- Heading “Effort and ‘different’ responses” → “Physical effort and ‘different’ responses”
- “under high effort” → “under high physical effort”
- Figure caption “by offset and effort” → “by offset and physical effort”
- “high effort lowered” → “high physical effort lowered”; “catch-by-effort” → “catch × physical effort”; “offset-by-effort” → “offset × physical effort”
- “The grip-window pupil mean did not differ significantly between effort levels” → “The grip-period pupil mean did not differ significantly between physical effort levels”
- “the effort effect” → “the physical effort effect”; “by task and effort” → “by task and physical effort”
- Figure caption “by task and effort” → “by task and physical effort”; “grip window” → “grip period”
- “under high effort” for the TEPR → “under high physical effort”
- “High-effort trials were more likely than low-effort trials” → “Trials at high physical effort were more likely than trials at low physical effort”

### Discussion

- “Older adults holding a strong grip reported” → “Under high physical effort, older adults reported”
- “the effect of effort” → “the effect of physical effort”; “offset-by-effort” → “offset × physical effort”
- “Why a strong grip would move the criterion” → “Why high physical effort would move the criterion”
- “high grip also improved” → “high physical effort also improved”
- “the two grip effects” → “the two physical effort effects”
- “carry the effort effect” → “carry the physical effort effect”; “placed the effort effect” → “placed the physical effort effect”
- “between grip levels” → “between physical effort levels”
- “high effort increased” → “high physical effort increased”
- “the two effort levels” → “the two physical effort levels”; “The effect of effort” → “The effect of physical effort”
- “under high effort” → “under high physical effort”
- “releasing a strong grip differs from releasing a light one.” → “releasing a grip held at high physical effort differs from releasing one held at low physical effort.”
- “the pupil under high effort” → “the pupil under high physical effort”; “The effort difference” → “The physical effort difference”
- “the size of the stimulus difference” → “the size of the offset”
- “two effort levels” and “between effort and performance” → “two physical effort levels” and “between physical effort and performance”
- “high-effort trials” → “trials at high physical effort”; “included effort” → “included physical effort”
- “the difficulty of the judgment” → “task difficulty”
- Supplement: “for effort”, “catch-by-effort”, and “offset-by-effort” → “for physical effort”, “catch × physical effort”, and “offset × physical effort”

## Render checks

Both renders contain the step 1–3 sentences and the step 5 sentences above. `(Dastgheib et al., in press)` appears twice, and the reference is:

> Dastgheib, M., Sun, A. Y., Yaghoubi, K. C., Zhang, W., Peters, M. A. K., Bennett, I. J., & Seitz, A. R. (in press). Effects of concurrent task difficulty and physical effort on memory and perceptual performance and pupil response in younger adults. *Cognitive Research: Principles and Implications*.

There is no `??` and no unresolved `@`.

After collapsing line breaks, `(?<!physical )effort` and `(?<!task )difficulty` have no hits in the chapter body. `effortful` remains once, in the introduction. Remaining hits are reference titles:

- Kahneman (1973), *Attention and effort*
- Piquado, Isaacowitz, and Wingfield (2010), “cognitive effort”
- Sun et al. (2026), “handgrip effort”
- van der Wel and van Steenbergen (2018), “index of effort”
- Beatty (1982), “Task-evoked pupillary responses”

`strong grip`, `light grip`, `grip level`, `grip-window`, `stimulus difference`, and `precise` do not occur. `second stimulus` occurs once, in “the probe, the second stimulus”.

Against the previous PDF, numeric tokens differ in two ways. The abstract now contains the required `(5%)` and `(40%)`, which adds one `5` and one `40`. The pupil-flow caption now begins on the next page as “Table S3”, which adds one separately tokenized `3`. No estimate, trial count, or other result number changed. 12,630 is unchanged.
