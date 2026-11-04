# AGENTS.md

Guidance for everyone working on this repository: the maintainer, collaborators, and any AI agents working for them. It covers how to work here: conventions, data handling, and checks.

Four places divide the documentation:

| Where | Covers |
|---|---|
| `analysis-plan.md` | Study design and statistics: what the analysis is meant to do |
| `ARCHITECTURE.md` | Code structure: scripts, data flow, and the objects passed between them |
| `AGENTS.md` (this file) | How to work in the repository: rules, conventions, and checks |
| [GitHub Issues](https://github.com/VagishHemmige/HIV-SLK-outcomes/issues) | Open scientific decisions (decision issues), gaps between the code and the plan, and known defects |

Read the three files before making changes. Each covers its own topic and does not repeat the others. `REPOSITORY-BLUEPRINT.md` describes the intended file layout.

## The project

This repository compares mortality and graft outcomes after simultaneous liver–kidney (SLK) transplantation between recipients with and without HIV. It uses Scientific Registry of Transplant Recipients (SRTR) standard analysis files, and it publishes its code and methods as a Quarto website.

- **`analysis-plan.md` is the source of truth for design.** It covers the cohort, exposure, era, matching, outcomes, models, and sensitivity analyses. Code implements the plan; it does not redefine it.
  - The plan contains no code: no scripts, functions, objects, packages beyond those named as methods, or descriptions of what the code currently does. Rules for the code go in this file, a description of the code in `ARCHITECTURE.md`, and gaps between the code and the plan in Issues. SRTR variable names are data definitions and may appear in the plan.
  - Plan changes are tracked in git, not in a change log inside the plan. Commit each design change on its own, with a message that gives the reason and says whether any results that depend on it had already been seen.
- **Open decisions.** The plan lists open design choices as **Decision D*n*** in its "Open decisions" table, each with a decision issue. Do not settle one in code. Until it is settled, the code keeps the behaviour that produced the poster results and says so in a comment that names the decision. If the code needs to depart from the plan, open a decision issue, or comment on the existing one. Once the decider has settled it (in the issue, or relayed there in his exact words), the plan change and the code can go in the same pull request, which closes the issue.
- **Decider.** Scientific decisions are made by Vagish Hemmige. Anyone may open or contribute to a decision issue; only the decider settles it (see "Working with agents").
- **This is an analysis project, not an R package**, even though the scripts live in `R/` and there is an `.Rbuildignore`. Do not add DESCRIPTION, NAMESPACE, roxygen, or pkgdown infrastructure. Do not apply CRAN conventions.
- **Default branch:** `master`, protected. Changes reach it only through pull requests.
- **Implementation status lives in GitHub Issues**, not in these files. When you find a gap between the code and the plan, or a defect, open or update an issue. Reference the issue in the pull request that closes it.

## Data sources

Each source this project uses is listed below with the package that accesses it. **Code that touches a source goes through its package.** Do not write project-specific loading, cleaning, or definition code for something the package already does.

| Source | Accessed through | Package documentation | Package maintained by | Access | What AI agents may do |
|---|---|---|---|---|---|
| SRTR standard analysis files (record the release used for any result) | `sRtr::load_srtr_file()` | [sRtr](https://github.com/VagishHemmige/sRtr) | This team | Restricted under the SRTR data use agreement (DUA) | None: code, documentation and synthetic data only |

To allow more for a source, a person changes its row through a pull request, naming the agreement and section that permit it, the approved AI service, and exactly what agents may do. Agents never propose or make this change themselves, and nothing in personal instructions can grant it.

**sRtr is the first place to look** for anything that touches SRTR files: loading files (`load_srtr_file()`), labels and factors (`apply_srtr_varlabels()`, `apply_srtr_factors()`), missing-value codes (`srtr_normalize_missing()`), composite death dates (`srtr_composite_death()`), time-to-event variables (`srtr_time_to_event()`), and follow-up outcomes (`add_txf_outcomes()`).

- **Read its documentation before writing code for a source**, including known issues. Behaviour that sets the cohort, the exposure, an outcome, or a denominator is checked in the documentation, not assumed from a function's name. Use an existing function, or extend it.
- **Problems belong in the package.** If an sRtr function is wrong, or does almost but not exactly what the plan needs, open an issue in the [sRtr repository](https://github.com/VagishHemmige/sRtr/issues) rather than documenting it here. Then fix it there, or wrap it in this project with a code comment that links the issue. Remove the wrapper when the issue is fixed.
- **Facts about SRTR belong in sRtr's documentation.** If sRtr lacks something this project needs to know, add it there or open an issue there. The "SRTR data facts" below stay here until sRtr documents them.
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

## Exploration and definitive analysis

Exploration is welcome on branches, or in `R/exploratory/`, which `main.R` never sources. Exploratory scripts need not have Quarto pages, and their findings are labelled preliminary. Exploration code does not stay in pipeline scripts. A script becomes part of the definitive analysis when it is sourced from `R/main.R`; from then on every rule below applies to it.

## R scripts

- **Running the analysis.** Open the `.Rproj` and run `R/main.R` from the project root. `main.R` sources the other scripts in dependency order; `ARCHITECTURE.md` describes what each does.
- **One job per script.** Scripts are grouped by stage: `R/primary_data_preparation/` (import and cohort construction), `R/add_data/` (merging candidate and donor files), `R/matching/`, `R/modeling/`, then `R/tables.R` and `R/figures.R`. When you add a definitive script, in the same pull request:
  - source it from `main.R` (or from `tables.R` or `figures.R`) at the right point
  - add its Quarto page
  - update `ARCHITECTURE.md`
- **Header comment.** Each script begins with a comment saying what it does, which objects it needs from earlier scripts, and what it produces.
- **Constants, libraries, and code lists** live in `R/setup.R`. Other scripts do not call `library()`, re-source `setup.R`, or hard-code values. Any number a reader might want to change, such as the study start year, the era cut-off, the age threshold, the matching ratio, the caliper, or the seed, is a named constant in `setup.R`.
- **Helper functions** go in `R/functions.R`, sourced after `setup.R`. `functions.R` defines functions only; it never runs analysis code. A function takes its inputs as arguments and does not read global objects, including through default arguments.
- **No side effects outside the project.** Do not use `setwd()`, absolute paths, `~/` paths, or writes outside the project. Locate data only through `sRtr::set_srtr_wd()`, called by the person running the analysis, not in committed code.
- **No results in the repository.** No script (exploratory or definitive), comment, commit message, issue, pull-request text or review report contains results, counts or data values from SRTR data unless they are cleared for release. Check before every commit and post, since this public repository publishes everything pushed to it, on any branch.
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

- **Setup files:** `_quarto.yml`, `index.qmd`, `404.qmd` (the page GitHub Pages shows for a missing address) and `styles.css` sit at the root. There is one page per pipeline script in `quarto/`, named after the script (for example, `quarto/process_slk_data.qmd` for `R/primary_data_preparation/process_slk_data.R`), plus the project-document and About pages. The `custom-callout` extension in `_extensions/` provides the `Rcode`, `Results` and `Pending` callouts.
- **Visual cues.** Each kind of page has one sidebar icon, coloured by `styles.css`, and pages mark the same kind of content with the callout of that colour: background `book` (blue); study design and methods `diagram-3` (orange); R code `laptop` (pink, `Rcode` 💻); results `clipboard-data` (green, `Results` 📊); project documents `file-earmark-ruled` (grey). Give every new page the icon of its kind.
- **Provisional content.** `quarto/_pending.lua` highlights in yellow every paragraph that starts with **Decision pending:** or, in the plan, **Open:**, and any `[text]{.pending}`, and adds a ⏳ `Pending` callout to pages that have a mark. Mark only what an open decision could change, and remove the mark when the decision is settled.
- **Project documents.** `analysis-plan.md`, `ARCHITECTURE.md`, `AGENTS.md` and `REPOSITORY-BLUEPRINT.md` are shown on the site by pages in `quarto/` that only include the root file, which stays the one source: edit the root file, not the page.
- **Showing code:** each page loads its script with `knitr::read_chunk()` in a hidden chunk. It then shows each labelled section in a chunk with `#| eval: false`, with prose explaining what the section does and why. Until a script is divided into labelled sections, its page may show the whole script with `#| file:` and `eval: false`. Pages link to the script on GitHub and to the scripts it depends on.
- **Never evaluate analysis code in the site.** Pages must render without SRTR data. `_quarto.yml` sets `eval: false` for every chunk; only the `read_chunk()` imports, the Setup page's chunks that attach packages and build its package table from text, and the Architecture page's chunk that reads `ARCHITECTURE.md` set `eval: true`. The rendered site therefore contains code and explanation only, never results. The one exception is the poster on the home page, which was already approved for public release.
- **Rendering:** the site renders to `docs/`, which is not committed. `.github/workflows/publish.yml` renders it on every pull request as a check and, on each push to `master`, publishes it to the `gh-pages` branch, which GitHub Pages serves. Render locally to preview a change to a script's labels or a page's prose, but do not commit the output.
- **New scripts:** when you add a script, add its page and its sidebar entry in `_quarto.yml` in the same pull request.
- **Prose must match the code.** If a change alters what a section does, update that section's explanation in the same pull request.

## Outputs

- **Exported files:** tables and figures are exported by `R/tables.R` and `R/figures.R` to `tables/` and `figures/`. Both folders are gitignored until they are cleared for release under the disclosure rules below, because their contents are results from SRTR data. Agents may not open them (see Restricted data).
- **Intermediate files** that must be written (for example, records exported for manual review) go in `data-private/`, which is gitignored. The pipeline must not depend on a file someone edited by hand; encode the decision made from a manual review as a reproducible rule in code, with a comment explaining it.
- **Figures** need alt text when they are published.
- `README.Rmd` is the source of `README.md`. Edit the `.Rmd` and re-knit; do not edit `README.md` directly.

## Restricted data (applies to everyone)

The **Access** column of "Data sources" says which sources are restricted. SRTR data are covered by the SRTR data use agreement (DUA). The rules below apply to people and agents alike.

- **Never commit data.** That includes SRTR files, extracts, and any saved R object or output derived from patient-level data (`.rds`, `.RData`, `.sas7bdat`, `.parquet`, `.csv`, `.xlsx`, and the like). Keep data outside the repository, and check `.gitignore` before adding a new output type.
- **No patient-level values in code or comments.** Do not write identifiers (`PERS_ID`, `PX_ID`, `TRR_ID`, `DONOR_ID`), dates, or counts from the data into scripts or comments. Code must not print identifiers or rows of patient-level data; checks use `stopifnot()` or `stop()` rather than printing.
- **AI agents never read restricted data.** Anything an agent opens, prints, or summarizes is sent to the model provider. Agents therefore do not:
  - open data files or patient-level objects
  - print rows or `head()` of them
  - display identifiers, person-level dates, or individual records
  - open outputs from SRTR data that are not yet cleared for release (for example `tables/` and `figures/` while they are gitignored)

  Agents write code, review it, and run it on **synthetic data** that mimics the column names and types. Results on real data are produced by an authorized person. The only exception is what the "Data sources" table explicitly allows for a source, and only within those words.
- **Use synthetic data for development and tests.** Build small synthetic data frames in code, label them clearly as synthetic, and never derive them from real records.
- **Disclosure.** Check every table, figure, flowchart box, risk table, and piece of text before it leaves the secure environment, against the exact wording of the SRTR DUA. This includes scripts, comments, commit messages, issues, pull-request text and review reports. The small-cell rule is open decision D8 in the plan; until it is settled, ask the maintainer before releasing any count below 11. Figures that plot individual people are not published.
- **Credentials and paid APIs.** Never commit keys, tokens, or `.Renviron`. Ask before paid, bulk, or uncached API calls. Which model or service version to use is the person's choice; an agent proposes, with the cost if known, and does not switch it on its own.

## Coding conventions

- **Flowchart labels and text** are built from the constants in `setup.R`, not hard-coded. For example, a label that says "2005 or later" or "age 16 or older" goes stale when the constants change.
- **Cohort flow** uses the `flowchart` package (`as_fc()`, `fc_filter()`, `fc_draw()`). Every exclusion goes through `fc_filter()`, in the plan's order. Never drop rows from `$data` outside the flowchart; the diagram must reproduce the cohort exactly. Record-level processing (harmonizing liver and kidney copies, choosing among a person's records) is fine. An analysis that uses fewer people than the cohort (complete cases, a subgroup) reports its own number with that analysis. The cohort used downstream is the data from the final flowchart step.
- **Matching**
  - Matching runs inside the pipeline and is reproducible from the seed in `setup.R`. Control selection must not depend on row order.
  - The matched-set identifier (`subclass`) is kept as an ordinary column through to every model, so models can cluster on it.
  - Report the number of HIV-positive recipients with each number of matched controls, including zero, and balance (standardized mean differences) before and after matching.
- **Joins**
  - Use `join_by()` with an explicit `relationship` where you can.
  - After every join of a supplementary file (candidate, donor, follow-up) to the cohort, assert that it did not multiply rows.
  - Decide deliberately what happens to people with no match: keep them, exclude them through a flowchart step, or stop with an error.
  - Avoid `select(-ends_with(".y"))` after a join; it silently keeps one copy of a duplicated column. Select the columns you need from the supplementary file before joining.
- **Code lists** (diagnosis or organ codes, for example) carry each code's meaning and the dictionary release it was checked against. Check every code against the SRTR data dictionary. Never invent codes, variables, files, or definitions.
- **Dates and times**
  - Compute times from `Date` columns with `difftime(..., units = "days")`, and state the conversion to years (365.25 days).
  - Death-censored outcomes end at the earliest of event, death, and last follow-up; test the edges on synthetic data.
- **Models.** Keep code that formats model results (hazard ratios, intervals, p-values) in named functions in `R/functions.R`, and test them on synthetic data with a known answer. Table labels such as event counts are computed from the data, never typed in.
- **Package APIs.** Read the help page and NEWS for the installed version before calling a package; don't write calls from memory. A test that fakes a package checks only our own logic, so call the real package once (on synthetic data) before relying on the code.
- **Style:** readable for a clinician-analyst who learned R from online guides: pipes and dplyr/tidyr verbs, `case_when()` rather than nested `if`, and loops over names or named lists rather than the apply family, `Reduce()` or positional indexing. Where base R is required, keep it in a named, tested function with a comment saying why. Follow the tidyverse style guide. Style changes go in their own commits, separate from changes that alter analytical behaviour.

## Checks for any change

Before opening a pull request, check the items below that apply to your change.

**Plan and data sources**

- Does the change implement the plan as written, and does it leave every open decision open?
- Does it use sRtr's functions where they exist, and does their documented behaviour match what the plan needs?
- For new or changed calls to a package, does the pull request name the installed version and the help page or NEWS entry relied on?

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
- Does each estimate match the plan's estimand, and do the tables, figures, and website prose say what the model actually estimates?
- Are event counts and sample sizes in labels computed, not typed?

**Structure and documentation**

- Is `ARCHITECTURE.md` current for the scripts and data flow this change touches?
- Are the chunk labels unique, and does each script's Quarto page still find every label it uses?
- Does every pipeline script have a Quarto page and a sidebar entry, and does the pull request's render check pass?
- Does any site chunk run analysis code? Only the chunks listed under "Quarto website" set `eval: true`.
- Are new derived variables labelled?

**Disclosure**

- Search every changed tracked file, and the commit messages and pull-request text, for dates, record IDs, names, absolute paths and numbers that could come from SRTR data. Read each hit in context; legitimate ones (plan dates, the decider's name, synthetic examples) stay.

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

- **Autonomy.** The person directing an agent sets its working style (review only, step by step, or hands-off) in their own instructions, and may add stricter rules; those take precedence over this section. Without a style, an agent works step by step. At most, an agent may:
  - edit code and documentation
  - add scripts and tests
  - run code and render the site on synthetic data
  - commit to a branch and open a pull request
- **Collaborators:** `master` is protected. Changes reach it only through a pull request that the maintainer reviews.
- **Decisions.** Anyone, including an agent, may add options and evidence to a decision issue. The decider states the choice in the issue. Agents implement decisions; they never choose among options on their own, in the issue, the plan, or the code.
- **Every agent, under any level of autonomy:**
  - never merges a pull request
  - follows the restricted-data rules above
  - asks before any step that would read real data, run the full pipeline, or make expensive calls
  - shows a person the exact text before posting or sending anything as them, unless that person's own instructions waive this
  - proposes another way of writing something already agreed before committing it
- Keep pull requests focused. In each one, state which section of the plan or which issue it addresses, and what was checked. Say whether it was checked on synthetic data or by reading only. Never describe synthetic results as results.
- For references, use the SRTR data dictionary and documentation, and PubMed. Cite what you used, and verify every citation (authors, journal, year, DOI) against PubMed before adding it to `references.bib`.
