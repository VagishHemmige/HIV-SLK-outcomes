# AGENTS.md

Guidance for everyone working on this repository: the maintainer, collaborators, and any AI coding agents working for them. It covers how to work here: conventions, data handling, and checks.

Four places divide the documentation:

| Where | Covers |
|---|---|
| `analysis-plan.md` | Study design and statistics: what the analysis is meant to do |
| `ARCHITECTURE.md` | Code structure: scripts, data flow, and the objects passed between them (maintained through Posit) |
| `AGENTS.md` (this file) | How to work in the repository: rules, conventions, and checks |
| [GitHub Issues](https://github.com/VagishHemmige/HIV-SLK-outcomes/issues) | Implementation status: where the code does not yet match the plan, and known defects |

Read the three files before making changes. Each covers its own topic and does not repeat the others. `REPOSITORY-BLUEPRINT.md` describes the intended file layout.

## The project

This repository compares mortality and graft outcomes after simultaneous liver–kidney (SLK) transplantation between recipients with and without HIV. It uses Scientific Registry of Transplant Recipients (SRTR) standard analysis files, and it publishes its code and methods as a Quarto website.

- **`analysis-plan.md` is the source of truth for design.** It covers the cohort, exposure, era, matching, outcomes, models, and sensitivity analyses. Code implements the plan; it does not redefine it.
  - The plan contains no code: no scripts, functions, objects, packages beyond those named as methods, or descriptions of what the code currently does. Rules for the code go in this file, a description of the code in `ARCHITECTURE.md`, and gaps between the code and the plan in Issues. SRTR variable names are data definitions and may appear in the plan.
  - If the code needs to depart from the plan, change the plan in the same pull request.
  - Plan changes are tracked in git, not in a change log inside the plan. Commit each design change on its own, with a message that gives the reason and says whether any results that depend on it had already been seen.
  - The plan lists open design choices as **Decision D*n*** in its "Open decisions" table. Do not settle one in code. Raise it with the maintainer. Until it is settled, the code keeps the behaviour that produced the poster results and says so in a comment that names the decision.
- **This is an analysis project, not an R package**, even though the scripts live in `R/` and there is an `.Rbuildignore`. Do not add DESCRIPTION, NAMESPACE, roxygen, or pkgdown infrastructure. Do not apply CRAN conventions.
- **Implementation status lives in GitHub Issues**, not in these files. When you find a gap between the code and the plan, or a defect, open or update an issue. Reference the issue in the pull request that closes it.

## R scripts

- **Running the analysis.** Open the `.Rproj` and run `R/main.R` from the project root. `main.R` sources the other scripts in dependency order.
- **One job per script.** Scripts are grouped by stage: `R/primary_data_preparation/` (import and cohort construction), `R/add_data/` (merging candidate and donor files), `R/matching/`, `R/modeling/`, then `R/tables.R` and `R/figures.R`. When you add a script:
  - source it from `main.R` (or from `tables.R` or `figures.R`) at the right point
  - add its Quarto page in the same commit
- **Header comment.** Each script begins with a comment saying what it does and which objects it needs from earlier scripts.
- **Constants and libraries** live in `R/setup.R`. Other scripts do not call `library()`, re-source `setup.R`, or hard-code values. Any number a reader might want to change, such as the study start year, the era cut-off, the age threshold, the matching ratio, the caliper, or the seed, is a named constant in `setup.R`.
- **Helper functions** go in `R/functions.R`, sourced after `setup.R`. `functions.R` defines functions only; it never runs analysis code. A function takes its inputs as arguments and does not read global objects, including through default arguments.
- **No side effects outside the project.** Do not use `setwd()`, absolute paths, `~/` paths, or writes outside the project. Locate data only through `sRtr::set_srtr_wd()`, called by the person running the analysis, not in committed code.
- **Exploration code** does not stay in pipeline scripts. Delete it, or move it to `R/exploratory/`, which `main.R` never sources. The website shows the pipeline scripts as they are.
- **Legacy code.** `rmd/SLK_1.8.rmd` and `rmd/SLK_2.8.rmd` are the R Markdown notebooks the pipeline was split from. They are not run and are kept for reference only. Do not build on them.

## Chunk labels

Scripts are divided into labelled sections, so that Quarto pages can show the code without copying it:

```r
## ---- exclude-prior-transplants
```

- Every logical step has a label. A label covers the code from its line to the next label.
- Labels are kebab-case, describe the step, and are **unique across all scripts**. Quarto pulls chunks by label, so a duplicate label silently picks up the wrong code.
- Renaming or deleting a label breaks the Quarto page that uses it. Search the `.qmd` files before you change one.
- Scripts must still run end to end through `main.R`. Labels are comments and must never change what the code does.

## Variable labels

- Label analysis variables with human-readable descriptions, for example with `labelled::set_variable_labels()`. Tables then show readable names without renaming code by hand.
- For SRTR variables, load files with `load_srtr_file(..., factor_labels = TRUE, var_labels = TRUE)` and use `apply_srtr_varlabels()` and `apply_srtr_factors()` rather than writing labels by hand.
- Label each derived variable where it is created. Include units and the source fields where they apply (for example, "Time to death or censoring from composite transplant date, years").

## Quarto website

The Quarto site documents the code. It does not run the analysis.

- **Setup files:** `_quarto.yml` and `index.qmd` sit at the root. There is one page per pipeline script in `quarto/`, named after the script (for example, `quarto/process_slk_data.qmd` for `R/primary_data_preparation/process_slk_data.R`). The `custom-callout` extension in `_extensions/` provides the `Rcode` callout.
- **Showing code:** each page loads its script with `knitr::read_chunk()` in a hidden chunk. It then shows each labelled section in a chunk with `#| eval: false`, with prose explaining what the section does and why. Pages link to the script on GitHub and to the scripts it depends on.
- **Never evaluate analysis code in the site.** Pages must render without SRTR data. The rendered site therefore contains code and explanation only, never results. The one exception is the poster on the home page, which was already approved for public release.
- **Rendering:** the site renders to `docs/`, which is not committed. `.github/workflows/publish.yml` renders it on every pull request as a check and, on each push to `master`, publishes it to the `gh-pages` branch, which GitHub Pages serves. Render locally to preview a change to a script's labels or a page's prose, but do not commit the output.
- **New scripts:** when you add a script, add its page and its sidebar entry in `_quarto.yml` in the same commit.
- **Prose must match the code.** If a change alters what a section does, update that section's explanation in the same pull request.

## Outputs

- **Exported files:** tables and figures are exported by `R/tables.R` and `R/figures.R` to `tables/` and `figures/`. Both folders are gitignored, because their contents are results under the disclosure rules below.
- **Intermediate files** that must be written (for example, records exported for manual review) go in `data-private/`, which is gitignored. The pipeline must not depend on a file someone edited by hand; encode the decision made from a manual review as a reproducible rule in code, with a comment explaining it.
- **Figures** need alt text when they are published.
- `README.Rmd` is the source of `README.md`. Edit the `.Rmd` and re-knit; do not edit `README.md` directly.

## Restricted data (applies to everyone)

SRTR data are covered by a data use agreement (DUA). The rules below apply to people and agents alike.

- **Never commit data.** That includes SRTR files, extracts, and any saved R object or output derived from patient-level data (`.rds`, `.RData`, `.sas7bdat`, `.parquet`, `.csv`, `.xlsx`, and the like). Keep data outside the repository, and check `.gitignore` before adding a new output type.
- **No patient-level values in code or comments.** Do not write identifiers (`PERS_ID`, `PX_ID`, `TRR_ID`, `DONOR_ID`), dates, or counts from the data into scripts or comments. Code must not print identifiers or rows of patient-level data; checks use `stopifnot()` or `stop()` rather than printing.
- **AI agents must not read restricted data.** Anything an agent opens, prints, or summarizes is sent to the model provider. Agents therefore must not:
  - open data files or patient-level objects
  - print rows or `head()` of them
  - display identifiers, person-level dates, or individual records

  This applies unless your own DUA explicitly permits that use. Agents may write code, review it, and run it on **synthetic data** that mimics the column names and types. Results on real data are produced by an authorized person.
- **Use synthetic data for development and tests.** Build small synthetic data frames in code, label them clearly as synthetic, and never derive them from real records.
- **Disclosure.** Check every table, figure, flowchart box, risk table, and piece of text before it leaves the secure environment, against the exact wording of the SRTR DUA. The small-cell rule is open decision D8 in the plan; until it is settled, ask the maintainer before releasing any count below 11. Figures that plot individual people are not published.

## Look in sRtr first

The [sRtr](https://github.com/VagishHemmige/sRtr) package is the **first place to look** for anything that touches SRTR files: loading files (`load_srtr_file()`), labels and factors (`apply_srtr_varlabels()`, `apply_srtr_factors()`), missing-value codes (`srtr_normalize_missing()`), composite death dates (`srtr_composite_death()`), time-to-event variables (`srtr_time_to_event()`), and follow-up outcomes (`add_txf_outcomes()`).

- Check the package's function reference before writing project-specific code. Use an existing function, or extend it.
- **Package problems belong in the package.** If an sRtr function is wrong, or does almost but not exactly what the plan needs, open an issue in the [sRtr repository](https://github.com/VagishHemmige/sRtr/issues) rather than documenting it here. Then fix it there, or wrap it in this project with a code comment that links the issue. Remove the wrapper when the issue is fixed.
- Before replacing project code with an sRtr function, confirm it gives identical results on synthetic data. For example, `srtr_time_to_event()` adds one day to times by default, and the project's mortality times do not.
- Record the sRtr version (or commit) used for any result.

## SRTR data facts

Source: the SRTR standard analysis file data dictionary, as bundled in sRtr (`sRtr::dictionary`, built from `data-raw/dataDictionary.html`). Check these against the release you are using.

**One SLK transplant appears twice.** It has a record in the liver file (`tx_li`) and one in the kidney file (`tx_ki`), each with its own `TRR_ID` and `PX_ID`, linked by `PERS_ID`. After the join, the same field carries an `_LI` or `_KI` suffix. The two copies of a field can disagree; harmonize them with a documented rule (`combine_li_ki_consistent()` for serologies), never by silently preferring one file.

**Identifiers.**

| Field | Identifies | Used to join |
|---|---|---|
| `PERS_ID` | Person | Liver to kidney transplant records |
| `PX_ID` | Candidate registration | Transplant to candidate files (`cand_kipa`, `cand_liin`) |
| `TRR_ID` | Transplant recipient registration | Transplant to follow-up files (`txf_li`, `txf_ki`) |
| `DONOR_ID` | Donor | Transplant to `donor_deceased` |

**Dates and status.**

- `REC_TX_DT`: transplant date. `REC_DISCHRG_DT`: transplant discharge date.
- `TFL_GRAFT_DT`: graft failure date. `TFL_LAFUDATE`: last graft follow-up date. `TFL_DEATH_DT`: date patient died, from follow-up.
- `PERS_OPTN_DEATH_DT`: OPTN death date by person. `PERS_SSA_DEATH_DT`: death date from the Social Security Administration.
- `PERS_NEXTTX`: date of the person's next transplant.
- `REC_TX_ORG_TY`: the organ types involved in the transplant, as text.
- `CAN_PREV_TX`: previous transplants. `REC_HIV_STAT`: HIV serology status.
- `CAN_LAST_SRTR_LAB_MELD` and `CAN_LAST_SRTR_LAB_MELD_TY`: last SRTR MELD/PELD score and its type (MELD or PELD).

**Coded values.** With `factor_labels = TRUE`, coded fields become text of the form `"<code>: <label>"` (for example, `"16: Black"`). Code that matches on these strings breaks if the labels change between releases, so match on the numeric code where you can.

**Unclear variables.** If a variable's meaning is unclear, check the data dictionary and the SRTR documentation for the release. Do not guess from the name.

## Coding conventions

- **Flowchart labels and text** are built from the constants in `setup.R`, not hard-coded. For example, a label that says "2005 or later" or "age 16 or older" goes stale when the constants change.
- **Cohort flow** uses the `flowchart` package (`as_fc()`, `fc_filter()`, `fc_draw()`). Every exclusion goes through `fc_filter()`. Never drop rows from `$data` outside the flowchart; the diagram must reproduce the cohort exactly. The cohort used downstream is the data from the final flowchart step.
- **Matching**
  - Matching runs inside the pipeline and is reproducible from the seed in `setup.R`. Control selection must not depend on row order.
  - The matched-set identifier (`subclass`) is kept as an ordinary column through to every model, so models can cluster on it.
  - Report the number of HIV-positive recipients with each number of matched controls, including zero, and balance (standardized mean differences) before and after matching.
- **Joins**
  - Use `join_by()` with an explicit `relationship` where you can.
  - After every join of a supplementary file (candidate, donor, follow-up) to the cohort, assert that it did not multiply rows.
  - Decide deliberately what happens to people with no match: keep them, exclude them through a flowchart step, or stop with an error.
  - Avoid `select(-ends_with(".y"))` after a join; it silently keeps one copy of a duplicated column. Select the columns you need from the supplementary file before joining.
- **Dates and times**
  - Compute times from `Date` columns with `difftime(..., units = "days")`, and state the conversion to years (365.25 days).
  - Death-censored outcomes end at the earliest of event, death, and last follow-up; test the edges on synthetic data.
- **Models.** Keep code that formats model results (hazard ratios, intervals, p-values) in named functions in `R/functions.R`, and test them on synthetic data with a known answer. Table labels such as event counts are computed from the data, never typed in.
- **Style:** the tidyverse style guide. Style changes go in their own commits, separate from changes that alter analytical behaviour.

## Checks for any change

Before opening a pull request, check the items below that apply to your change.

**Cohort construction**

- Do the order, the denominator, and the labels of each flowchart step match the condition it applies?
- Can each count be reproduced from the code?
- Is the downstream cohort the data from the final flowchart step?
- Is there exactly one row per `PERS_ID` after the duplicate steps?

**Exposure, era and covariates**

- Is HIV status classified as the plan states (Section 5.1, D2)?
- Are liver and kidney copies of each covariate harmonized by a documented rule?
- Is every covariate measured at or before transplant?

**Matching**

- Does the propensity score model contain exactly the covariates in the plan (Section 6.2, D3)?
- Are era exact matching, the transplant-date caliper, and the ratio taken from `setup.R`?
- Is matching reproducible from the seed, and independent of row order?
- Are unmatched and partially matched HIV-positive recipients counted and reported?

**Outcomes**

- Does follow-up for mortality start at the composite transplant date and end at death or the latest last follow-up date?
- Are graft failure outcomes death-censored, starting at that organ's transplant date?
- Are recipients who died before discharge excluded from length of stay?

**Models and reported numbers**

- Does every Cox model use robust standard errors clustered on `subclass`?
- Do the tables, figures, and website prose say what the model actually estimates?
- Are event counts and sample sizes in labels computed, not typed?

**Structure and documentation**

- Are the chunk labels unique, and does each script's Quarto page still find every label it uses?
- Does every pipeline script have a Quarto page and a sidebar entry, and does the pull request's render check pass?
- Are all site chunks `eval: false`?
- Are new derived variables labelled?
- Has the maintainer been told that `ARCHITECTURE.md` needs regenerating, if scripts or data flow changed?

**Execution**

- Does `main.R` run from a clean session (`Ctrl+Shift+F10` in RStudio, then source `main.R`), with no objects left over from earlier work?
- Are paths relative, with no hard-coded data directories?
- Are package versions recorded (`sessionInfo()`, and the sRtr commit)?

**Validation on real data** (run by an authorized person; report only aggregate, disclosure-checked results)

- counts at each flowchart step, compared with the poster and manuscript
- row counts before and after each join of a supplementary file
- the number of HIV-positive recipients with 5, 4, 3, 2, 1 and 0 matched controls
- standardized mean differences before and after matching
- events by group for each outcome, overall and by era

## Working with agents

- **Autonomy.** The person directing an agent decides how much it may do. That person may also give the agent stricter rules of their own, and those take precedence over this section. By default, an agent may:
  - edit code and documentation
  - add scripts and tests
  - run code and render the site on synthetic data
  - commit to a branch and open a pull request
- **Collaborators:** `master` is protected. Changes reach it only through a pull request that the maintainer reviews.
- **Every agent, under any level of autonomy:**
  - follows the restricted-data rules above
  - asks before any step that would read real data, run the full pipeline, or make expensive calls
  - does not settle an open decision in the plan
- Keep pull requests focused. In each one, state which section of the plan or which issue it addresses, and what was checked. Say whether it was checked on synthetic data or by reading only. Never describe synthetic results as results.
- For references, use the SRTR data dictionary and documentation, and PubMed. Cite what you used, and verify every citation (authors, journal, year, DOI) against PubMed before adding it to `references.bib`.
