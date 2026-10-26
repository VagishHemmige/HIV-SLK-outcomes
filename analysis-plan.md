# Statistical Analysis Plan: Outcomes of Simultaneous Liver–Kidney Transplantation in People With HIV

**Version:** 0.1 (draft) · **Date:** 2026-10-25 · **Authors:** Hannah Rosenthal, Christina Tsai, Jonathan Mamber Czeresnia, Vagish Hemmige, Clara Tow

This plan states the intended design and analysis. It was reconstructed from the 2026 NY-NJ CFAR poster and the draft manuscript methods. Where those sources disagree, or where the method behind the poster results is not documented, the choice is listed as **Decision D*n*** in [Open decisions](#13-open-decisions) rather than resolved here.

Reporting will follow STROBE. The [reporting map](#12-reporting-map) links each checklist item to a section of this plan.

---

## 1. Objective

To compare all-cause mortality after simultaneous liver–kidney (SLK) transplantation between recipients with HIV and propensity-score-matched recipients without HIV, in the United States, 2005–2023.

**Secondary objectives**

- Compare death-censored kidney graft failure and death-censored liver graft failure.
- Compare post-transplant hospital length of stay among recipients discharged alive.
- Assess whether the association between HIV and each outcome differs between the eras before and after direct-acting antivirals (DAAs) for hepatitis C.

**Planned future aim (not part of this analysis):** compare SLK recipients with HIV to liver-only recipients with HIV (**Decision D9**).

## 2. Design

Retrospective matched cohort study in national transplant registry data. Each HIV-positive SLK recipient is matched to up to five HIV-negative SLK recipients on a propensity score, with exact matching on DAA era and a caliper on transplant date ([Section 6](#6-matching)). All outcome analyses are conducted in the matched cohort.

The draft manuscript calls the design a "matched case-control study". Because exposure (HIV) defines the groups and outcomes are followed forward from transplant, this plan describes it as a matched cohort.

## 3. Data source

- **Scientific Registry of Transplant Recipients (SRTR)** standard analysis files, Spring 2023 release, accessed under a data use agreement.
  - Transplant recipient files: liver (`tx_li`) and kidney (`tx_ki`).
  - Candidate files: kidney–pancreas (`cand_kipa`) and liver/intestine (`cand_liin`).
  - Deceased donor file (`donor_deceased`).
  - Follow-up files (`txf_li`, `txf_ki`) where a variable requires them.
- **Linkage:** liver and kidney transplant records are linked within SRTR by the person identifier `PERS_ID`. Candidate records are linked by `PX_ID`, and donor records by `DONOR_ID`. No external linkage is performed.
- **Period:** transplants in the release run from 1987-01-01 to 2023-03-31. The study period is 2005-01-01 to 2023-03-31, because the first SLK transplant in a recipient with HIV in the data occurred in 2005.
- **Ethics:** approved by the Albert Einstein College of Medicine Institutional Review Board (IRB #2021-13273); de-identified data.

## 4. Study population

### 4.1 SLK definition

An SLK transplant is a liver transplant and a kidney transplant in the same person (`PERS_ID`) on the same day, plus or minus one day.

### 4.2 Inclusion

1. SLK transplant during the study period ([Section 3](#3-data-source)).
2. Recipient age at transplant of at least 16 years. **Decision D1:** the age threshold (the poster cohort used ≥16 years; the manuscript says "greater than 16" and "adult").
3. Non-missing identifiers needed for linkage and follow-up, including the donor identifier in both files.

### 4.3 Exclusion

1. Any prior transplant of any organ, identified both from the prior-transplant fields (`CAN_PREV_TX`) and from an earlier transplant date in either file.
2. Additional organs at the SLK transplant (kidney–liver–heart, kidney–liver–intestine).
3. Liver and kidney allografts from different donors.
4. HIV status recorded as positive in one file and negative in the other ([Section 5.1](#51-exposure-definition)).

If a recipient has more than one SLK transplant, only the first is used. Same-day duplicate records are resolved to one record per recipient by a documented, reproducible rule.

## 5. Exposure, era, and time zero

### 5.1 Exposure definition

Recipient HIV serostatus at transplant, from `REC_HIV_STAT` in the liver and kidney files.

- **HIV-positive:** positive in at least one file, and positive or missing/unknown in the other.
- **HIV-negative:** every other non-discordant recipient, including those whose status is unknown or not done in both files.
- **Discordant** (positive in one file, negative in the other): excluded ([Section 4.3](#43-exclusion)).

**Decision D2:** the manuscript says a positive status must be "not contradicted by an 'unknown' designation" in the other file, while the poster cohort classified positive-plus-unknown as positive. Because HIV-negative includes unknown status, tables label that group "Negative/Unknown".

### 5.2 DAA era

- **Pre-DAA:** transplant year 2005–2013.
- **Post-DAA:** transplant year 2014–2023.

2014 is the first year in which DAA therapy was widely available to transplant recipients.

### 5.3 Time zero

The composite transplant date: the earlier of the liver and kidney transplant dates. For organ-specific graft outcomes, time zero is that organ's transplant date. Covariates are measured at or before transplant.

## 6. Matching

### 6.1 Method

- **Propensity score:** probability of HIV-positive status, estimated by logistic regression.
- **Exact matching** on DAA era.
- **Caliper** of 365 days on the transplant date.
- **Nearest-neighbour matching without replacement**, ratio 1:5 (up to five HIV-negative recipients per HIV-positive recipient).
- **No caliper on the propensity score** itself.
- Matching is reproducible, and the matched set is carried through to every outcome model.

### 6.2 Propensity score covariates

| Domain | Covariates |
|---|---|
| Donor | Age, sex |
| Recipient | Age, sex, race (White, Black, Hispanic, Other) |
| Liver disease | Last laboratory MELD score before transplant; primary liver diagnosis (OPTN categories, collapsed) |
| Kidney disease | Primary kidney diagnosis (OPTN categories, collapsed) |
| Viral serology | HCV serostatus, HBV surface antigen, CMV serostatus |

Serologies are harmonized across the liver and kidney files: positive if positive in one file and positive or missing in the other; negative if negative in one and negative or missing in the other; directly conflicting values (positive versus negative) are treated as missing.

**Decision D3:** the covariate set, including whether recipient race is included. The manuscript list omits race; the poster lists transplant year and era (era is handled by exact matching, transplant year by the caliper). The covariate set behind the poster results is not fully documented.

**Decision D4:** handling of missing covariate values. Categorical covariates use an explicit "Missing" level. For continuous covariates (MELD, donor age, recipient age), the method used for the poster results is not documented. The options are complete-case exclusion before matching, missing indicators, or imputation.

### 6.3 Matching outcomes reported

- The number of HIV-positive recipients with 5, 4, 3, 2, 1 and 0 matched controls.
- HIV-positive recipients excluded or unmatched, and how they compare with those matched.
- The proportion of matched sets in which members differ by more than the caliper, as a check.

## 7. Outcomes

### 7.1 Primary outcome: all-cause mortality

- **Death date:** a composite of registry sources, in order of priority: OPTN-reported death date (`PERS_OPTN_DEATH_DT`), Social Security Administration death date (`PERS_SSA_DEATH_DT`), then transplant follow-up death date (`TFL_DEATH_DT`), taking whichever file (liver or kidney) records it.
- **Censoring:** recipients without a death date are censored at the latest last-follow-up date (`TFL_LAFUDATE`) across the liver and kidney files.
- **Time:** from the composite transplant date to death or censoring, in years.

### 7.2 Secondary outcomes: death-censored graft failure

Evaluated separately for the kidney and the liver graft.

- **Event:** registry-reported graft failure date (`TFL_GRAFT_DT`) for that organ.
- **Follow-up** ends at the earliest of graft failure, death, or that organ's last follow-up date. Death before graft failure is censored.
- **Time:** from that organ's transplant date, in years.

**Decision D5:** whether retransplantation of the organ (`PERS_NEXTTX`) or return to chronic dialysis without a recorded graft failure date also counts as graft failure.

### 7.3 Secondary outcome: hospital length of stay

- Days from transplant date to transplant discharge date (`REC_DISCHRG_DT`), counted inclusively so that the transplant day is day 1.
- Recipients who died on or before the recorded discharge date are excluded from this analysis.

## 8. Statistical analysis

### 8.1 Descriptive analysis

- A cohort flow diagram from all SLK transplants in the release through each exclusion to the matched cohort.
- Baseline donor and recipient characteristics by HIV status, before and after matching: median and interquartile range for continuous variables, counts and percentages for categorical variables.
- Unadjusted comparisons: Wilcoxon rank-sum test for continuous variables, and chi-square or Fisher's exact test for categorical variables, as appropriate.

### 8.2 Balance

- Standardized mean differences for every propensity score covariate, before and after matching.
- An absolute standardized mean difference below 0.10 is considered adequate balance.
- Balance is assessed before any outcome analysis is run.

### 8.3 Survival analysis

- Kaplan–Meier estimates of patient survival by HIV status, compared with the log-rank test; overall and within each DAA era.
- Cox proportional hazards models for HIV status and each time-to-event outcome (mortality, kidney graft failure, liver graft failure), with robust standard errors clustered on matched set.
- The proportional hazards assumption is checked with scaled Schoenfeld residuals.

### 8.4 Effect modification by DAA era

- Cox models fitted separately within the pre-DAA and post-DAA matched cohorts. Matched sets stay within one era because era is matched exactly.
- A model with an HIV × era interaction in the full matched cohort, to test for effect modification. The interaction hazard ratio is the ratio of the post-DAA to the pre-DAA hazard ratio.

**Decision D7:** whether era-specific models are reported unadjusted only (as on the poster), or also with adjustment for covariates with residual imbalance after matching (for example, HBV surface antigen and CMV).

**Decision D10:** handling of sparse or zero events (for example, liver graft failure in the post-DAA era): report as not estimable, or use Firth's penalized Cox regression.

### 8.5 Length of stay

Compared between groups with the Wilcoxon rank-sum test, because of its right-skewed distribution.

**Decision D6:** whether length of stay is also analysed by regression (negative binomial or log-linear, with standard errors clustered on matched set) as a secondary analysis.

### 8.6 Missing data

- Missingness of every covariate and outcome field is described by HIV status.
- Missing categorical covariates are an explicit category ([Section 6.2](#62-propensity-score-covariates)); continuous covariates follow D4.

### 8.7 Inference and software

- All tests are two-sided, with p < 0.05 considered statistically significant.
- Analyses are run in R (version 4.5.1 for the poster results), with propensity score matching in `MatchIt`.
- Code is public at https://github.com/VagishHemmige/HIV-SLK-outcomes, and a website at https://vagishhemmige.github.io/HIV-SLK-outcomes documents each step. Data are not shared, as the data use agreement requires.

## 9. Sensitivity analyses

1. **HCV-positive recipients:** the matched cohort restricted to recipients with HCV-positive serostatus, with the primary Cox model and era-specific models, to assess whether the association differs among those directly affected by DAAs.
2. **Missing-data handling:** the alternative under D4 to the one chosen as primary.
3. **Exposure classification:** the alternative rule under D2 for positive-plus-unknown recipients.

## 10. Disclosure control

- All outputs are reviewed against the SRTR data use agreement before they leave the secure environment.
- **Decision D8:** the small-cell rule for tables, figures, flow diagrams, risk tables and website text, under the exact wording of the SRTR data use agreement (for example, suppressing counts below 11).
- Figures that show individual people are not published.

## 11. Limitations to address in reporting

- Small number of HIV-positive SLK recipients, limiting power, especially for graft failure and era-specific analyses.
- HIV status from registry fields, with unknown status grouped with HIV-negative; some misclassification is possible.
- No HIV-specific clinical data (CD4 count, viral load, antiretroviral regimen) in the standard analysis files.
- Residual confounding by unmeasured factors, including center practices.
- Registry follow-up and graft failure ascertainment vary by center and over time.

## 12. Reporting map

| Checklist item | Topic | Plan section |
|---|---|---|
| STROBE 1 | Title and abstract | 1, 2 |
| STROBE 2–3 | Background, objectives | 1 |
| STROBE 4 | Study design | 2 |
| STROBE 5 | Setting, dates | 3 |
| STROBE 6 | Eligibility; matching criteria and number of controls | 4, 6 |
| STROBE 7 | Variables | 5, 6.2, 7 |
| STROBE 8 | Data sources and measurement | 3, 5, 7 |
| STROBE 9 | Bias | 6, 9, 11 |
| STROBE 10 | Study size | 6.3, 8.1 |
| STROBE 11 | Quantitative variables | 6.2, 7 |
| STROBE 12 | Statistical methods, including matching, missing data and sensitivity analyses | 8, 9 |
| STROBE 13 | Participants, flow diagram | 8.1 |
| STROBE 14–17 | Descriptive data, outcomes, main and other results | 8 |
| STROBE 18–21 | Discussion, limitations, interpretation, generalizability | 11 |
| STROBE 22 | Funding; access to code | 8.7 |

## 13. Open decisions

| ID | Decision | Section |
|---|---|---|
| D1 | Recipient age threshold: ≥16 years (poster), >16 years, or adults ≥18 years | 4.2 |
| D2 | HIV classification when one file is positive and the other unknown or not done | 5.1 |
| D3 | Propensity score covariate set, including recipient race | 6.2 |
| D4 | Missing continuous covariates in the propensity score | 6.2 |
| D5 | Whether retransplantation or return to dialysis counts as graft failure | 7.2 |
| D6 | Whether regression models of length of stay are reported alongside the Wilcoxon test | 8.5 |
| D7 | Era-specific Cox models: unadjusted only, or also adjusted | 8.4 |
| D8 | Small-cell disclosure rule under the SRTR data use agreement | 10 |
| D9 | Whether the liver-only comparison is part of this paper or a separate analysis | 1 |
| D10 | Sparse or zero events: report as not estimable, or Firth's penalized Cox regression | 8.4 |

## References

- von Elm E, et al. The Strengthening the Reporting of Observational Studies in Epidemiology (STROBE) statement. *Lancet* 2007;370:1453–1457.
- Ho DE, Imai K, King G, Stuart EA. MatchIt: nonparametric preprocessing for parametric causal inference. *J Stat Softw* 2011;42(8):1–28.
- Austin PC. Balance diagnostics for comparing the distribution of baseline covariates between treatment groups in propensity-score matched samples. *Stat Med* 2009;28:3083–3107.
- Blumberg EA, Rogers CC; AST Infectious Diseases Community of Practice. Human immunodeficiency virus in solid organ transplantation. *Am J Transplant* 2013;13 Suppl 4:169–178.
- Scientific Registry of Transplant Recipients. Standard analysis files, Spring 2023 release, and data dictionary.
