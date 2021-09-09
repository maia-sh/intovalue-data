
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IntoValue Dataset

## Overview

This dataset is modified from the [main IntoValue
dataset](https://doi.org/10.5281/zenodo.5141342), and includes updated
registry data from ClinicalTrials.gov and DRKS. It also includes
additional data on associated results publications, including links in
the registries and trial registration number reporting in the
publications.

Detailed documentation for the parent IntoValue dataset is provided in a
data dictionary and readme alongside the [dataset in
Zenodo](https://doi.org/10.5281/zenodo.5141342). This readme serves to
highlight/document changes.

Note that the parent IntoValue dataset includes `summary_results_date`
for the few DRKS summary results, which have been removed in this
version. We remove them as they had been manually extracted (from PDFs)
for only the subset of summary results manually found in IntoValue,
whereas this dataset includes additional summary results found via
automated search and hence without dates. For parity, we removed DRKS
summary result dates. We could decide to add these from either (1) the
PDF (which indicates when the summary result was created), or (2) the
DRKS change history (which indicates when the summary result was
uploaded and made available).

## Data sources

This dataset builds on several sources, detailed below. The latest query
date is provided when applicable. Raw data, when permissible (i.e., not
for full-text), is shared in either this repository or in Zenodo,
depending on its size.

| Source                            | Type                          | Date       | Raw Data                                                                                                                  | Script                                                                                                            |
|-----------------------------------|-------------------------------|------------|---------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------|
| IntoValue                         | Trials                        | NA         | <https://doi.org/10.5281/zenodo.5141342>                                                                                  | [get-intovalue.R](https://github.com/maia-sh/intovalue-data/blob/main/scripts/01_get-intovalue.R)                 |
| PubMed                            | Bibliometric                  | 2021-08-15 | Zenodo                                                                                                                    | [get-pubmed.R](https://github.com/maia-sh/intovalue-data/blob/main/scripts/02_get-pubmed.R)                       |
| ClinicalTrials.gov/ AACT          | Registry                      | 2021-08-15 | Zenodo                                                                                                                    | [get-aact.R](https://github.com/maia-sh/intovalue-data/blob/main/scripts/06_get-aact.R)                           |
| DRKS                              | Registry                      | 2021-08-15 | Zenodo?                                                                                                                   | [get-drks.R](https://github.com/maia-sh/intovalue-data/blob/main/scripts/08_get-drks.R)                           |
| Unpaywall/ institutional licenses | Full-text PDF                 | NA         | NA                                                                                                                        | [get-ft-pdf.R](https://github.com/maia-sh/intovalue-data/blob/main/scripts/03_get-ft-pdf.R)                       |
| GROBID                            | Full-text XML                 | NA         | NA                                                                                                                        | PDF-to-XML conversion done in Python                                                                              |
| Unpaywall                         | Open access status            | 2021-08-15 | [oa-unpaywall.csv](https://github.com/maia-sh/intovalue-data/blob/main/data/raw/open-access/oa-unpaywall.csv)             | [get-oa-unpaywall-data.R](https://github.com/maia-sh/intovalue-data/blob/main/scripts/13_get-oa-unpaywall-data.R) |
| ShareYourPaper                    | Green open access permissions | 2021-07-23 | [oa-syp-permissions.csv](https://github.com/maia-sh/intovalue-data/blob/main/data/raw/open-access/oa-syp-permissions.csv) | [get-oa-permissions.py](https://github.com/maia-sh/intovalue-data/blob/main/scripts/14_get-oa-permissions.py)     |

## Analysis dataset

We are interested in interventional trials with a German UMC lead
completed between 2009 and 2017. Due to changes in the registry as well
as discrepancies between IntoValue 1 and 2, we re-apply the IntoValue
exclusion criteria and deduplicate to get the analysis dataset.

``` r
trials <-
  trials_all %>% 

  filter(

    # Re-apply the IntoValue exclusion criteria
    iv_completion,
    iv_status,
    iv_interventional,
    has_german_umc_lead,

    # In case of dupes, exclude IV1 version
    !(is_dupe & iv_version == 1)
  )
    
n_iv_trials <- nrow(trials)
```

**Number of included trials**: 2915

For analyses by UMC, split trials by UMC lead city:

``` r
trials_by_umc <-
  trials %>% 
  mutate(lead_cities = strsplit(as.character(lead_cities), " ")) %>%
  tidyr::unnest(lead_cities)
```

Some analyses apply only to trials with a results publication
(optionally limited to journal articles to exclude dissertations and
abstracts) with a PMID that resolves to a PubMed record and for which we
could acquire the full-text as a PDF.

``` r
trials_pubs <-
  trials %>% 
  filter(
    # publication_type == "journal publication", #optional
    has_pubmed,
    has_ft,
  )

n_iv_trials_pubs <- nrow(trials_pubs)
trials_same_pmid <- janitor::get_dupes(trials_pubs, pmid)
n_trials_same_pmid <- n_distinct(trials_same_pmid$id)
n_pmids_same_trial <- n_distinct(trials_same_pmid$pmid)
n_pmids_dupes <- unique(range(trials_same_pmid$dupe_count))
```

**Number of trials with results publications**: 1897

In general, there is max 1 publication per trial and max 1 trial per
publication. However, there are 70 trials associated with the same 35
publications (i.e., 2 publications per trial). Since the unit of
analysis is trials, we disregard this double-counting of publications.

## TRN reporting in abstract

``` r
n_trn_abs <- nrow(filter(trials_pubs, has_iv_trn_abstract))

prop_trn_abs <- n_trn_abs/n_iv_trials_pubs
```

<!-- $$ \text{TRN in abstract (%)} = \frac{\text{Number of trials with PubMed publications with TRN in abstract}}{\text{Number of trials with PubMed publications available as PDF full-text}}$$ -->

**Numerator**: Number of trials with PubMed publications with IntoValue
TRN in abstract

**Denominator**: Number of trials with PubMed publications available as
PDF full-text

38% (715/1897) of trials report a TRN in the abstract of their results
publication.

## TRN reporting in full-text

``` r
n_trn_ft <- nrow(filter(trials_pubs, has_iv_trn_ft))

prop_trn_ft <- n_trn_ft/n_iv_trials_pubs
```

**Numerator**: Number of trials with PubMed publications with IntoValue
TRN in PDF full-text

**Denominator**: Number of trials with PubMed publications available as
PDF full-text

60% (1137/1897) of trials report a TRN in the full-text (PDF) of their
results publication.

## Linked publication in registry

``` r
# ClinicalTrials.gov
trials_ctgov <- filter(trials_pubs, registry == "ClinicalTrials.gov")

n_iv_trials_pubs_ctgov <- nrow(trials_ctgov)

n_reg_pub_link_ctgov <- nrow(filter(trials_ctgov, has_reg_pub_link))

prop_reg_pub_link_ctgov <- n_reg_pub_link_ctgov/ n_iv_trials_pubs_ctgov

n_auto <- nrow(filter(trials_ctgov, reference_derived))
n_manual <- nrow(filter(trials_ctgov, reference_derived))

# DRKS
trials_drks <- filter(trials_pubs, registry == "DRKS")

n_iv_trials_pubs_drks <- nrow(trials_drks)

n_reg_pub_link_drks <- nrow(filter(trials_drks, has_reg_pub_link))

prop_reg_pub_link_drks <- n_reg_pub_link_drks/ n_iv_trials_pubs_drks
```

*Registry Limitations*: ClinicalTrials.gov includes a often-used PMID
field for references. In addition, ClinicalTrials.gov automatically
indexes publications from PubMed using TRN in the secondary identifier
field. In contrast, DRKS includes references as a free-text field,
leaving trialists to decide whether to enter any publication
identifiers.

We consider a publication “linked” if the PMID or DOI is included in the
trial registrations. Note that some publications are included in the
registrations without a PMID or DOI (i.e., publication title and/or URL
only).

**Numerator**: Number of trials with PubMed publications PMIDs and/or
DOIs linked in trial registration

**Denominator**: Number of trials with PubMed publications available as
PDF full-text

59% (849/1449) of trials on clinicaltrials.gov include a link (i.e.,
PMID, DOI) to their PubMed publication (as available in the IntoValue
dataset). This includes 662 (78%) trials with automatically indexed
publications (i.e., using TRN in PubMed’s secondary identifier field)
and 662 (78%) trials with manually added publications.

22% (97/448) of trials on DRKS include a link (i.e., PMID, DOI) to their
PubMed publication (as available in the IntoValue dataset).

## Registry summary results

``` r
trials_pubs %>% 
  count(registry, has_summary_results) %>% 
  knitr::kable()
```

| registry           | has_summary_results |    n |
|:-------------------|:--------------------|-----:|
| ClinicalTrials.gov | FALSE               | 1297 |
| ClinicalTrials.gov | TRUE                |  152 |
| DRKS               | FALSE               |  443 |
| DRKS               | TRUE                |    5 |

*Registry Limitations*: ClinicalTrials.gov includes a structured summary
results field. In contrast, DRKS includes summary results with other
references, and summary results were inferred based on keywords, such as
Ergebnisbericht or Abschlussbericht, in the reference title.

## EUCTR Cross-registrations

``` r
tbl_euctr <-
  trials %>% 
  tbl_cross(
    row = has_crossreg_eudract,
    col = registry,
    margin = "column",
    percent = "column",
    label = list(
      has_crossreg_eudract ~ "EUCTR TRN in Registration",
      registry ~ "Registry"
    )
  )

as_kable(tbl_euctr)
```

| **Characteristic**            | ClinicalTrials.gov | DRKS      | **Total**   |
|:------------------------------|:-------------------|:----------|:------------|
| **EUCTR TRN in Registration** |                    |           |             |
| FALSE                         | 1,925 (85%)        | 558 (87%) | 2,483 (85%) |
| TRUE                          | 345 (15%)          | 87 (13%)  | 432 (15%)   |

Of the 2915 unique trials completed between 2009 and 2017 and meeting
the IntoValue inclusion criteria, we found that 432 (15%) include an
EUCTR id in their registration, and are presumably cross-registered in
EUCTR. This includes 345 (15%) from ClinicalTrials.gov and 87 (13%) from
DRKS.
