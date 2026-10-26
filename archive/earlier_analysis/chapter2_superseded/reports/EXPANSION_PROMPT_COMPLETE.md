# Complete Prompt for Expanding Introduction Section

**Instructions:** Copy everything below the line "---START PROMPT---" and paste it into ChatGPT, Claude, or another LLM.

---

# START PROMPT

You are an expert scientific writer specializing in cognitive psychology, psychophysics, and neuroscience. I need you to expand and polish a detailed outline for the Introduction section of a dissertation chapter on "Pupil-Indexed Arousal and Psychometric Sensitivity in Older Adults."

## Context

This chapter uses a dual-task paradigm (handgrip effort + perceptual discrimination) to examine how physical effort-induced arousal relates to psychometric sensitivity in older adults (65+ years). The study combines:
- Psychometric function analysis (thresholds, slopes)
- Pupillometry (tonic and phasic pupil metrics)
- Hierarchical GLMMs with continuous stimulus intensity
- Within-subject centering to separate state vs. trait arousal effects

The detailed outline below contains the structure and key content, but needs to be expanded into polished, publication-ready prose. The outline includes:
- Multiple subsections with 2-5 paragraphs each
- Mathematical formulations (already included)
- Citations (in @authorYear format)
- Clear theoretical framework development
- Research questions and testable hypotheses at the end

## Your Task

Expand the detailed outline below into a polished, scientifically accurate Introduction section (approximately 3000-4000 words total). For each subsection:

1. **Expand outline points into full paragraphs** with smooth transitions
2. **Maintain scientific accuracy** and use precise technical terminology
3. **Preserve all mathematical equations** and citations (keep @authorYear format)
4. **Ensure logical flow** between paragraphs and subsections
5. **Write in formal academic style** appropriate for a PhD dissertation
6. **Keep the existing structure** - maintain all headings and subsection organization
7. **Preserve the research questions and hypotheses section** - these are already well-formulated
8. **Integrate citations naturally** into the prose (don't over-cite; use citations where appropriate)

**Output Format:** Deliver the complete expanded Introduction in a single response. The expanded text should be ready to copy directly back into the Quarto markdown (.qmd) file, maintaining all heading levels (##, ###) and formatting.

## Style Guidelines

- Use active voice where appropriate, passive voice for standard scientific descriptions
- Write for an audience familiar with cognitive psychology and neuroscience
- Use precise, technical language but remain clear
- Maintain a logical progression: from general concepts → specific mechanisms → methodological approach → research questions
- Each paragraph should have a clear topic sentence and logical development
- Transitions between paragraphs should be smooth and explicit

## Important Notes

- **Preserve all mathematical equations exactly** as written (they are correct)
- **Keep all citations in @authorYear format** - these will be processed by the bibliography system
- **Don't add new citations** unless absolutely necessary for clarity
- **Don't change the structure** - the outline organization is intentional
- **Maintain the level of detail** in theoretical explanations - this is for a dissertation, not a brief report
- **The Research Questions and Hypotheses section is final** - don't modify it, just ensure it flows well from the preceding content

## What NOT to Change

- The overall structure and subsection organization
- Mathematical formulations
- Citation format (@authorYear)
- Research questions and hypotheses (these are final)
- The logical progression of ideas

## What TO Improve

- Expand bullet points and outline-style content into full sentences and paragraphs
- Add smooth transitions between ideas
- Clarify complex theoretical concepts where needed
- Ensure each paragraph has a clear purpose and flows logically
- Polish the prose to publication quality

---

# Introduction

## Physical–Cognitive Dual-Task Interactions in Everyday Life and Aging

Everyday life frequently requires people to perform cognitively demanding tasks while simultaneously exerting physical effort. Surgeons must maintain continuous muscle engagement while making fine perceptual discriminations, athletes coordinate complex motor sequences while monitoring environmental cues, and older adults navigate dual-task situations—such as walking while monitoring traffic or carrying groceries while having a conversation—in which physical and cognitive demands interact dynamically. These real-world scenarios highlight a fundamental challenge: when multiple task domains compete for limited cognitive resources, performance in one or both domains may suffer [@wickens2008; @pashler1994]. This challenge takes on particular significance in aging, as older adults face both declining cognitive capacity and increased physiological costs of effortful engagement [@salthouse1996; @verhaeghen2003].

A growing literature suggests that physical–cognitive interactions are mediated by shared arousal systems, particularly the locus coeruleus–noradrenergic (LC–NE) system, which modulates sensory gain, evidence accumulation, and response caution as a function of task demands and internal state [@astonjones2005; @mather2016; @jepma2012]. Understanding how physical effort modulates cognitive performance—and how this relationship changes with age—requires moving beyond simple outcome measures (overall accuracy or mean reaction time) to characterize how effort-induced arousal alters the fundamental processes underlying perceptual decision-making. The present study addresses this question by combining a physical–cognitive dual-task paradigm with pupillometry and psychometric function analysis to examine how physical effort–induced arousal relates to perceptual sensitivity in older adults.

## Psychometric Functions: Quantifying Perceptual Sensitivity

### Theoretical Framework

Psychometric functions (PFs) provide a principled framework for characterizing how stimulus intensity relates to perceptual performance. Unlike summary statistics that collapse across stimulus levels, PFs model the complete intensity–response relationship, revealing how performance changes as stimulus differences become more or less discriminable. In a same–different discrimination task, the psychometric function maps continuous stimulus intensity (e.g., frequency offset or contrast difference) onto the probability of correctly detecting a difference or choosing "different" [@wichmann2001psychometric; @green1993signal].

### Mathematical Formulation

The psychometric function typically follows a sigmoidal (S-shaped) curve, most commonly modeled using the cumulative normal distribution (probit link) or logistic function (logit link). For binary choice tasks, the probit formulation is:

$$P(\text{"different"} | X) = \Phi\left(\frac{X - \alpha}{\beta}\right)$$

where:
- $P(\text{"different"} | X)$ is the probability of choosing "different" given stimulus intensity $X$
- $\Phi(\cdot)$ is the cumulative standard normal distribution function (probit link)
- $\alpha$ is the **threshold** (point of subjective equality, PSE, or just noticeable difference, JND), representing the stimulus intensity at which performance reaches 50% (or other criterion level)
- $\beta$ is the **slope** parameter, representing the steepness of the function and inversely related to perceptual sensitivity: steeper slopes indicate higher sensitivity (smaller intensity differences are reliably detected)

Alternatively, using the logit link:

$$P(\text{"different"} | X) = \frac{1}{1 + \exp\left(-\frac{X - \alpha}{\beta}\right)}$$

### Key Parameters

**Threshold ($\alpha$ or PSE/JND):** The threshold parameter indicates the stimulus intensity at which participants perform at a criterion level (typically 50% "different" responses). A lower threshold indicates better sensitivity—participants can detect smaller stimulus differences. In aging research, threshold increases often reflect reduced perceptual sensitivity, requiring larger stimulus differences for reliable detection.

**Slope ($\beta$):** The slope parameter quantifies how rapidly performance improves as stimulus intensity increases. Steeper slopes indicate higher sensitivity and lower variability in perceptual judgments. Slope is inversely related to the standard deviation of the underlying sensory noise distribution: $\sigma = 1/\beta$ (for probit) or $\sigma = \beta \cdot \pi / \sqrt{3}$ (for logit). Age-related slope reductions suggest increased sensory noise or decreased discriminability [@green1993signal; @wichmann2001psychometric].

**Relationship to Signal Detection Theory:** The psychometric function is closely related to signal detection theory (SDT), where threshold corresponds to the decision criterion and slope relates to $d'$ (sensitivity). Changes in threshold reflect criterion shifts (response bias), while changes in slope reflect true sensitivity changes [@macmillan2005detection].

### Advantages Over Summary Statistics

Modeling continuous intensity–response relationships via PFs offers several advantages over collapsing trials into discrete "Easy/Hard" bins:

1. **Preserves information:** Continuous intensity is maintained rather than lost through binning, allowing more precise estimates of sensitivity changes.
2. **Parameter interpretability:** Threshold and slope have clear psychological and mathematical meanings, facilitating mechanistic interpretation.
3. **Statistical power:** Using all stimulus levels simultaneously provides more efficient parameter estimation than comparing discrete bins.
4. **Quantifies sensitivity changes:** Slope changes directly reflect how arousal alters the intensity–performance relationship, distinguishing true sensitivity effects from criterion shifts.

## Arousal, Effort, and Perceptual Performance

### Theoretical Perspectives on Arousal–Performance Relationships

The relationship between arousal and cognitive performance has been formalized in several influential frameworks. The **Yerkes–Dodson law** [@yerkes1908] posits an inverted-U function: performance improves with increasing arousal up to an optimal point, beyond which further arousal (especially stress or anxiety) impairs performance. **Adaptive Gain Theory (AGT)** [@astonjones2005] provides a neural mechanism linking phasic and tonic LC–NE activity to optimal task performance, with moderate tonic baseline and robust phasic bursts facilitating focused attention and rapid behavioral responses.

In the context of aging, this relationship becomes more complex. Research suggests that the arousal–performance curve may be altered in older adults, often manifesting as a **leftward shift or compression** of the inverted-U function [@mather2016; @mikneviciute2022]. This implies that older adults may reach their optimal arousal peak at lower levels of objective demand than younger adults. Consequently, levels of effort or stress that might be engaging or beneficial for younger adults (placing them at the peak of the curve) can push older adults onto the "descending limb," leading to **supra-optimal arousal** and performance decrements [@huang2024; @mather2016].

### Physical Effort as an Arousal Manipulation

Physical effort provides a powerful, controlled method for manipulating arousal state. Sustained isometric handgrip at moderate-to-high intensities (e.g., 30–40% of maximum voluntary contraction, MVC) reliably engages the sympathetic nervous system, increasing heart rate, blood pressure, and cortical arousal [@silvestrini2008; @silvestrini2011]. Critically, this manipulation allows researchers to examine how physiological arousal—independent of task difficulty or cognitive load—modulates perceptual and decision processes.

The **limited-capacity framework** [@wickens2008] predicts that concurrent physical effort should compete with cognitive tasks for shared processing resources, potentially degrading performance when demands exceed available capacity. Supporting this view, Azer et al. [-@azer2023] found that older adults showed significantly reduced accuracy in a visual working memory task while maintaining moderate handgrip (30% MVC), whereas younger adults remained unaffected. This suggests that the combined demand of physical and cognitive tasks may more easily deplete resources in older adults or drive arousal into a dysregulated state [@verhaeghen2003].

### Expected Effects on Psychometric Sensitivity

How should physical effort–induced arousal affect psychometric function parameters? Theoretical predictions vary:

**Threshold ($\alpha$) predictions:**
- If arousal degrades sensory signal quality or increases internal noise, thresholds should increase (larger stimulus differences required for reliable detection).
- If arousal optimizes neural gain at moderate levels, thresholds might decrease (better sensitivity).
- Supra-optimal arousal in older adults might shift thresholds upward, reflecting degraded signal-to-noise ratios.

**Slope ($\beta$) predictions:**
- Steeper slopes (higher sensitivity) would suggest that arousal enhances discriminability, perhaps by improving signal-to-noise ratios or sharpening neural tuning.
- Shallower slopes (lower sensitivity) would suggest that arousal introduces variability or noise, flattening the intensity–response relationship.
- The relationship may be non-linear: moderate arousal might enhance sensitivity (steeper slopes), while excessive arousal might flatten slopes (reduced discriminability).

## Pupillometry: A Window into Arousal Dynamics

### Locus Coeruleus–Norepinephrine System and Pupil Diameter

The LC–NE system serves as a central regulator of cortical arousal and cognitive state. This small brainstem nucleus is the primary source of norepinephrine to the forebrain and modulates the "neural gain" of cortical circuits—essentially the signal-to-noise ratio of information processing [@astonjones2005]. According to Adaptive Gain Theory, optimal performance relies on a balance between moderate tonic (baseline) LC firing that promotes focused attention and robust phasic (event-related) bursts that facilitate rapid behavioral responses to task-relevant stimuli [@gilzenrat2010].

**Pupillometry** provides a powerful, non-invasive window into these LC–NE dynamics. Pupil diameter tracks LC firing activity with high temporal precision [@joshi2016; @murphy2014], making it an ideal proxy for both tonic and phasic arousal states. The neural pathway linking LC to pupil diameter involves direct projections from LC to the Edinger–Westphal nucleus, which controls pupil constriction/dilation via parasympathetic and sympathetic pathways [@mcDougal2010].

### Pupil Metrics in Cognitive Tasks

Two primary pupil metrics capture distinct aspects of arousal:

**Baseline/Tonic Pupil Diameter:** Reflects tonic LC activity and general alertness levels [@gilzenrat2010; @alnaes2014]. Larger baseline pupil diameter indicates higher tonic arousal, which may support sustained attention but can also reflect hyperarousal if excessive.

**Task-Evoked Pupil Response (TEPR):** Reflects phasic LC activation and the mobilization of mental effort during task execution [@beatty1982; @kahneman1966]. The TEPR typically peaks 1–2 seconds after stimulus onset and is quantified as:
- **Peak amplitude:** Maximum dilation relative to baseline
- **Area Under the Curve (AUC):** Integral of the baseline-corrected pupil signal over a time window, capturing both amplitude and duration of the response
- **Mean dilation:** Average pupil diameter over a fixed post-stimulus window

TEPR magnitude has been linked to task difficulty, cognitive load, surprise, and decision confidence [@preuschoff2011; @vanbergen2021]. In discrimination tasks, larger TEPRs are associated with more difficult decisions, suggesting that phasic arousal signals the recruitment of cognitive control resources [@kiefer2018].

### Pupil–Cognition Links in Aging

In aging, the LC–NE system undergoes significant changes. Structural degradation of the LC is common in older adults [@mather2016], and functional compensatory mechanisms often emerge, such as chronically elevated tonic arousal or hyper-responsivity to challenge [@lee2018; @mather2016neural]. However, this compensation has limits: if older adults reach supra-optimal arousal states more readily, excessive norepinephrine release may lead to distractibility, indiscriminate processing, and performance decrements [@astonjones2005; @eldar2013].

Pupillometry studies in aging reveal complex patterns. Older adults often show larger baseline pupil diameters but reduced task-evoked responses, suggesting altered phasic–tonic balance [@granholm2007; @vanbergen2021]. Physical effort manipulations provide an opportunity to examine how externally induced arousal modulates these dynamics and whether effort-induced pupil changes predict cognitive performance changes.

## Dual-Task Paradigms and Resource Competition

### Theoretical Frameworks

Dual-task paradigms, in which participants perform two tasks simultaneously, have been central to understanding resource limitations and cognitive architecture. Several theoretical frameworks explain dual-task interference:

**Limited Capacity Model** [@kahneman1973; @wickens2008]: Posits that cognitive resources are finite and shared across tasks. When total demand exceeds capacity, performance suffers in one or both tasks.

**Resource Competition** [@navon1984; @navon1985]: Emphasizes that interference occurs when tasks compete for the same resource pool (e.g., both require attention or working memory).

**Multiple Resource Theory** [@wickens2008]: Proposes that interference depends on the similarity of resource requirements. Tasks using different resource dimensions (e.g., spatial vs. verbal) show less interference than tasks using similar dimensions.

### Physical–Cognitive Dual Tasks

Physical–cognitive dual tasks are particularly relevant for understanding real-world performance. When physical effort (e.g., maintaining handgrip force) is combined with cognitive tasks (e.g., perceptual discrimination), interference can arise from multiple sources:

1. **Shared attentional resources:** Both tasks require sustained attention and monitoring [@proctor2012].
2. **Physiological arousal:** Physical effort increases sympathetic activation, which may enhance or impair cognitive performance depending on arousal level [@silvestrini2008].
3. **Motor–cognitive interference:** Maintaining grip force may compete with response selection and execution processes [@woollacott2002].

Age-related changes in dual-task performance are well-documented, with older adults showing larger performance decrements under dual-task conditions [@verhaeghen2003; @beurskens2014]. This may reflect reduced cognitive reserve, impaired executive control, or altered arousal regulation.

## Linking Arousal to Psychometric Sensitivity: A Novel Approach

### Beyond Binned Analyses

Traditional approaches to examining arousal–performance relationships often collapse trials into discrete difficulty bins (e.g., "Easy" vs. "Hard"), losing information about continuous stimulus intensity. This binning approach assumes that arousal effects are uniform across intensity levels and cannot reveal how arousal modulates the intensity–response relationship itself.

### Within-Subject Centering: Separating State from Trait

A key methodological innovation is the decomposition of pupil metrics into **state** and **trait** components using within-subject centering:

$$P^{(\text{trait})}_{j} = \overline{P}_{j}, \qquad P^{(\text{state})}_{ij} = P_{ij} - \overline{P}_{j}$$

where $P_{ij}$ is the pupil metric on trial $i$ for subject $j$, $\overline{P}_{j}$ is the subject's mean pupil metric (trait), and $P^{(\text{state})}_{ij}$ is the trial-level deviation from that mean (state). This decomposition allows testing whether **within-person fluctuations** in arousal (state) predict sensitivity changes, independent of stable individual differences in baseline arousal (trait) [@enders2013; @hoffman2015].

### Hierarchical GLMM Framework

The primary analysis uses hierarchical generalized linear mixed models (GLMMs) with a probit link to model continuous intensity–response relationships while incorporating pupil-indexed arousal:

$$\text{probit}(P(Y_{ij}=1)) = \beta_0 + \beta_1 X_{ij} + \beta_2 \text{Effort}_{ij} + \beta_3 \text{Modality}_{ij} + \beta_4 P^{(\text{state})}_{ij} + \beta_5 (X_{ij} \times P^{(\text{state})}_{ij}) + \beta_6 P^{(\text{trait})}_{j} + u_{0j} + u_{1j}X_{ij}$$

The **key interaction term** $X_{ij} \times P^{(\text{state})}_{ij}$ tests whether within-person arousal fluctuations modulate psychometric sensitivity—that is, whether the slope of the intensity–response relationship changes as a function of trial-level arousal state. This approach preserves continuous stimulus intensity, separates state from trait arousal effects, and allows for individual differences in both baseline sensitivity and arousal reactivity.

## Research Questions and Hypotheses

Based on the theoretical frameworks and methodological considerations outlined above, the present study addresses the following research questions:

### Research Question 1: Behavioral Backbone

**RQ1:** How do High (40% MVC) versus Low (5% MVC) handgrip effort conditions alter psychometric function parameters (thresholds and slopes) in older adults performing auditory and visual same–different discrimination?

**Hypothesis 1a:** Thresholds will be higher (worse sensitivity) under High effort relative to Low effort, reflecting degraded signal-to-noise ratios from resource competition or supra-optimal arousal.

**Hypothesis 1b:** Slopes will be shallower (lower sensitivity) under High effort, indicating increased variability in perceptual judgments when arousal is elevated.

### Research Question 2: Effort–Pupil Manipulation Check

**RQ2:** Does High effort increase tonic and task-evoked pupil dynamics relative to Low effort in older adults?

**Hypothesis 2a:** Baseline (tonic) pupil diameter will be larger under High effort, indicating elevated tonic arousal.

**Hypothesis 2b:** Task-evoked pupil responses (TEPR, quantified as AUC) will be larger under High effort, indicating increased phasic arousal during task execution.

### Research Question 3: Pupil–Psychometric Coupling (Primary)

**RQ3:** Does trial-wise phasic arousal (pupil state) predict psychometric sensitivity when stimulus intensity is modeled continuously?

**Hypothesis 3a:** The interaction between stimulus intensity and pupil state ($X_{ij} \times P^{(\text{state})}_{ij}$) will be negative, indicating that higher trial-level arousal is associated with shallower psychometric slopes (reduced sensitivity). This would be consistent with supra-optimal arousal degrading signal quality or introducing noise.

**Hypothesis 3b (Alternative):** If moderate arousal enhances performance, the interaction might be positive, indicating steeper slopes (higher sensitivity) when arousal is elevated.

**Hypothesis 3c:** Pupil trait (between-subject baseline arousal) will show weaker or null associations with sensitivity, as trait effects may be confounded with other individual differences (e.g., LC integrity, cognitive reserve).

### Research Question 4: Subject-Level Individual Differences

**RQ4:** Do individuals with larger effort-evoked arousal changes (High–Low effort) also show larger effort-related changes in PF parameters?

**Hypothesis 4:** Subject-level changes in pupil metrics (Δpupil = High effort − Low effort) will be positively correlated with subject-level changes in thresholds (Δthreshold) and negatively correlated with changes in slopes (Δslope), indicating that individuals who show stronger physiological responses to effort also show larger behavioral sensitivity decrements.

---

# END PROMPT

