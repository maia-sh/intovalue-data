
<!-- README.md is generated from README.Rmd. Please edit that file -->

# IntoValue Dataset

## Overview

``` r
intovalue <-
  intovalue_all %>% 
  filter(
    has_german_umc_lead,
    !is_dupe,
    has_publication,
    has_pmid,
    has_pubmed,
    has_ft_pdf
  )
n_iv_trials_w_pubs <- nrow(intovalue)
```

We are interested in IntoValue trials with a German UMC lead and a
publication with a PMID that resolves to a PubMed record and for which
we could acquire the full-text as a PDF (n = 1902).

``` r
trials_same_pmid <- janitor::get_dupes(intovalue, pmid)

n_trials_same_pmid <- n_distinct(trials_same_pmid$id)
n_pmids_same_trial <- n_distinct(trials_same_pmid$pmid)
n_pmids_dupes <- unique(range(trials_same_pmid$dupe_count))
```

In general, there is max 1 publication per trial and max 1 trial per
publication. However, there are 72 trials associated with the same 36
publications (i.e., 2 publications per trial). Since the unit of
analysis is trials, we disregard this double-counting of publications.

## TRN reporting in abstract

``` r
n_trn_abs <- nrow(filter(intovalue, has_trn_abstract))

prop_trn_abs <- n_trn_abs/n_iv_trials_w_pubs
```

<!-- $$ \text{TRN in abstract (%)} = \frac{\text{Number of trials with PubMed publications with TRN in abstract}}{\text{Number of trials with PubMed publications available as PDF full-text}}$$ -->

**Numerator**: Number of trials with PubMed publications with TRN in
abstract

**Denominator**: Number of trials with PubMed publications available as
PDF full-text

39% (748/1902) of trials report a TRN in the abstract of their results
publication.

## TRN reporting in full-text

``` r
n_trn_ft <- nrow(filter(intovalue, has_trn_ft_pdf))

prop_trn_ft <- n_trn_ft/n_iv_trials_w_pubs
```

**Numerator**: Number of trials with PubMed publications with TRN in PDF
full-text

**Denominator**: Number of trials with PubMed publications available as
PDF full-text

65% (1239/1902) of trials report a TRN in the full-text (PDF) of their
results publication.

## Linked publication in registry

``` r
n_iv_trials_w_pubs_ctgov <- nrow(filter(intovalue, is_c_tgov))

n_reg_pub_link_ctgov <- nrow(filter(intovalue, is_c_tgov, has_reg_pub_link))

prop_reg_pub_link_ctgov <- n_reg_pub_link_ctgov/ n_iv_trials_w_pubs_ctgov
```

*Limitations*: We currently checked only clinicaltrials.gov for linked
publications. DRKS is forthcoming if feasible. We consider a publication
“linked” if the PMID is included in the trial registrations. Note that
some publications are included in the registrations without a PMID.

**Numerator**: Number of trials with PubMed publications PMIDs linked in
trial registration

**Denominator**: Number of trials (clinicaltrials.gov only) with PubMed
publications available as PDF full-text

59% (854/1457) of trials on clinicaltrials.gov include a link (i.e.,
PMID) to their PubMed publication (as available in the IntoValue
dataset).
