
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
intovalue_ctgov <- filter(intovalue, is_c_tgov)

n_iv_trials_w_pubs_ctgov <- nrow(intovalue_ctgov)

n_reg_pub_link_ctgov <- nrow(filter(intovalue_ctgov, has_reg_pub_link))

prop_reg_pub_link_ctgov <- n_reg_pub_link_ctgov/ n_iv_trials_w_pubs_ctgov

filter(intovalue, is_c_tgov, 
       # has_reg_pub_link
       ) %>% filter(reference_type != "derived")
#> # A tibble: 188 x 43
#>    id        pmid doi   url    has_summary_res… days_to_publica… days_to_summary
#>    <chr>    <dbl> <chr> <chr>  <lgl>                       <dbl>           <dbl>
#>  1 NCT00…  2.26e7 10.1… https… TRUE                          518            2119
#>  2 NCT00…  2.13e7 10.1… http:… FALSE                         176              NA
#>  3 NCT00…  2.16e7 10.1… https… FALSE                         822              NA
#>  4 NCT00…  1.70e7 10.1… http:… FALSE                        -930              NA
#>  5 NCT00…  2.34e7 10.1… https… FALSE                         258              NA
#>  6 NCT00…  2.39e7 10.1… https… FALSE                         575              NA
#>  7 NCT00…  2.19e7 10.1… https… TRUE                          711            1168
#>  8 NCT00…  2.11e7 10.1… http:… FALSE                         682              NA
#>  9 NCT00…  2.28e7 10.1… http:… FALSE                        1249              NA
#> 10 NCT00…  1.99e7 10.1… http:… TRUE                          245             339
#> # … with 178 more rows, and 36 more variables: days_reg_to_start <dbl>,
#> #   days_reg_to_compl <dbl>, days_reg_to_publ <dbl>, is_c_tgov <lgl>,
#> #   recruitment_status <chr>, is_multicentric <lgl>, phase <chr>,
#> #   main_sponsor <chr>, intervention_type <chr>, completion_date <date>,
#> #   publication_date <date>, enrollment <dbl>, masking <chr>, allocation <chr>,
#> #   start_date <date>, lead_cities <chr>, has_publication <lgl>,
#> #   has_german_umc_lead <lgl>, intovalue <dbl>, is_dupe <lgl>, has_pmid <lgl>,
#> #   has_trn_abstract <lgl>, has_trn_ft_pmc <lgl>, has_trn_ft_pdf <lgl>,
#> #   has_trn_secondary_id <lgl>, has_trn_intovalue <lgl>, has_trn_any <lgl>,
#> #   pmcid <chr>, has_pubmed <lgl>, has_queried_pmcid <lgl>,
#> #   has_queried_doi <lgl>, has_ft_pmc <lgl>, has_ft_pdf <lgl>,
#> #   is_resolved <lgl>, has_reg_pub_link <lgl>, reference_type <chr>

n_auto <- nrow(filter(intovalue_ctgov, reference_type == "derived"))
n_manual <- nrow(filter(intovalue_ctgov, reference_type != "derived"))
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
dataset). This include 666 (78%) trials with automatically indexed
publications (i.e., using TRN in PubMed’s secondary identifier field)
and 188 (22%) trials with manually added publications.
