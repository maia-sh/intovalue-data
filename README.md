
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IntoValue Dataset

## Overview

This dataset is modified from the [main IntoValue
dataset](https://github.com/quest-bih/IntoValue2/blob/master/data/iv_main_dataset.csv),
and includes updated registry data from ClinicalTrials.gov and DRKS
queried in May 2021. It also includes additional data on associated
results publications, including links in the registries and trial
registration number reporting in the publications.

The documentation for the parent IntoValue dataset is provided in a
[data
dictionary](https://github.com/quest-bih/IntoValue2/blob/master/data/iv_data_dictionary.csv)
and
[readme](https://github.com/quest-bih/IntoValue2/blob/master/data/iv_data_readme.txt).

This readme serves to highlight/document changes.

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

## Analysis dataset

We are interested in interventional trials with a German UMC lead
completed between 2009 and 2017. Due to changes in the registry as well
as discrepancies between IntoValue 1 and 2, we re-apply the IntoValue
exclusion criteria and deduplicate to get the analysis dataset.

``` r
intovalue <-
  intovalue_all %>% 
  
  # In case of dupes, exclude IV1 version
  mutate(is_not_iv1_dupe = if_else(!(is_dupe & iv_version == 1), TRUE, FALSE)) %>% 
  
  filter(
    iv_completion,
    iv_status,
    iv_interventional,
    has_german_umc_lead,
    is_not_iv1_dupe
  )

n_iv_trials <- nrow(intovalue)
```

**Number of included trials**: 2917

Some analyses apply only to trials with a results publication with a
PMID that resolves to a PubMed record and for which we could acquire the
full-text as a PDF.

``` r
intovalue_pubs <-
  intovalue %>% 
  filter(
    has_publication,
    has_pmid,
    has_pubmed,
    has_ft_pdf
  )

n_iv_trials_pubs <- nrow(intovalue_pubs)
trials_same_pmid <- janitor::get_dupes(intovalue_pubs, pmid)
n_trials_same_pmid <- n_distinct(trials_same_pmid$id)
n_pmids_same_trial <- n_distinct(trials_same_pmid$pmid)
n_pmids_dupes <- unique(range(trials_same_pmid$dupe_count))
```

**Number of trials with results publications**: 1896

In general, there is max 1 publication per trial and max 1 trial per
publication. However, there are 70 trials associated with the same 35
publications (i.e., 2 publications per trial). Since the unit of
analysis is trials, we disregard this double-counting of publications.

## TRN reporting in abstract

``` r
n_trn_abs <- nrow(filter(intovalue_pubs, has_iv_trn_abstract))

prop_trn_abs <- n_trn_abs/n_iv_trials_pubs
```

<!-- $$ \text{TRN in abstract (%)} = \frac{\text{Number of trials with PubMed publications with TRN in abstract}}{\text{Number of trials with PubMed publications available as PDF full-text}}$$ -->

**Numerator**: Number of trials with PubMed publications with IntoValue
TRN in abstract

**Denominator**: Number of trials with PubMed publications available as
PDF full-text

38% (714/1896) of trials report a TRN in the abstract of their results
publication.

## TRN reporting in full-text

``` r
n_trn_ft <- nrow(filter(intovalue_pubs, has_iv_trn_ft_pdf))

prop_trn_ft <- n_trn_ft/n_iv_trials_pubs
```

**Numerator**: Number of trials with PubMed publications with IntoValue
TRN in PDF full-text

**Denominator**: Number of trials with PubMed publications available as
PDF full-text

60% (1131/1896) of trials report a TRN in the full-text (PDF) of their
results publication.

## Linked publication in registry

``` r
# ClinicalTrials.gov
intovalue_ctgov <- filter(intovalue_pubs, registry == "ClinicalTrials.gov")

n_iv_trials_pubs_ctgov <- nrow(intovalue_ctgov)

n_reg_pub_link_ctgov <- nrow(filter(intovalue_ctgov, has_reg_pub_link))

prop_reg_pub_link_ctgov <- n_reg_pub_link_ctgov/ n_iv_trials_pubs_ctgov

n_auto <- nrow(filter(intovalue_ctgov, reference_derived))
n_manual <- nrow(filter(intovalue_ctgov, reference_derived))

# DRKS
intovalue_drks <- filter(intovalue_pubs, registry == "DRKS")

n_iv_trials_pubs_drks <- nrow(intovalue_drks)

n_reg_pub_link_drks <- nrow(filter(intovalue_drks, has_reg_pub_link))

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

59% (852/1449) of trials on clinicaltrials.gov include a link (i.e.,
PMID, DOI) to their PubMed publication (as available in the IntoValue
dataset). This includes 666 (78%) trials with automatically indexed
publications (i.e., using TRN in PubMed’s secondary identifier field)
and 666 (78%) trials with manually added publications.

21% (96/447) of trials on DRKS include a link (i.e., PMID, DOI) to their
PubMed publication (as available in the IntoValue dataset).

## Registry summary results

``` r
intovalue_pubs %>% 
  count(registry, has_summary_results) %>% 
  knitr::kable()
```

| registry           | has\_summary\_results |    n |
|:-------------------|:----------------------|-----:|
| ClinicalTrials.gov | FALSE                 | 1298 |
| ClinicalTrials.gov | TRUE                  |  151 |
| DRKS               | FALSE                 |  442 |
| DRKS               | TRUE                  |    5 |

*Registry Limitations*: ClinicalTrials.gov includes a structured summary
results field. In contrast, DRKS includes summary results with other
references, and summary results were inferred based on keywords, such as
Ergebnisbericht or Abschlussbericht, in the reference title.
