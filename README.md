# CHASe-outside-safe-haven

This is a repository of scripts used for the ICD-9-CM to ICD-9 and ICD-10 cross-walk (cross-mapping) and various other conversions, as well as the resulting lists of codes for the CHASe (The impact of childhood adversity and mental health on young person suicide) project. Details and results of the cross-walk were published in the protocol paper by Dougall et al., 2020 [[1]](#1)

We compiled three lists of codes: 

1. a list of codes suggestive of neglect were cross-mapped from ICD-9-CM to ICD-9 and ICD-10 from the codes published by Schnitzer, et al. (2011) [[4]](#4); 
2. a list of codes that were Maltreatment- or Violence-Related (MVR) - these were compiled from codes used in González-Izquierdo, et al. (2010) [[2]](#2), Gilbert, et al. (2012, supplementary files) [[3]](#3), and Schnitzer, et al. (2004) [[5]](#5).
3. a cross-mapping of mental-health related ICD codes from ICD-9-CM and ICD-10-CM to ICD-9 and ICD-10, from the Clinical Classifications Software (CCS) published by AHRQ [[6]](#6)


# Using the codes

All of the codes produced for this project can be found in the [`/processed_ICD_codes/`](/processed_ICD_codes) sub-directory of this repository. There are several files, the most useful of which are probably the cross-mapped codes:


## "Schnitzer" codes (indicative of maltreatment)

The files are organised according to the inclusion and exclusion policy used in the study [[4]](#4). An inclusion code had a corresponding upper age limit, with some inclusion codes only applying when the same hospital episode did not include any of the corresponding exclusion codes. There was one inclusion that needed at least one co-ocurring additional inclusion code from a list.

Inclusion and exclusion codes are listed separately for ICD-9 and ICD-10, and each code has a corresponding **inclusion_index** or **exclusion_index** - this is an identifier that links the ICD-9 and ICD-10 versions of the same code:


* [`schnitzer_et_al_2011_empirical_inclusions_index_icd9.csv`](processed_ICD_codes/schnitzer_et_al_2011_empirical_inclusions_index_icd9.csv)
* [`schnitzer_et_al_2011_empirical_inclusions_index_icd10.csv`](processed_ICD_codes/schnitzer_et_al_2011_empirical_inclusions_index_icd10.csv)

* [`schnitzer_et_al_2011_empirical_exclusions_index_icd9.csv`](processed_ICD_codes/schnitzer_et_al_2011_empirical_exclusions_index_icd9.csv)
* [`schnitzer_et_al_2011_empirical_exclusions_index_icd10.csv`](processed_ICD_codes/schnitzer_et_al_2011_empirical_exclusions_index_icd10.csv)

* [`schnitzer_et_al_2011_empirical_index_map_inclusions_exclusions.csv`](processed_ICD_codes/schnitzer_et_al_2011_empirical_index_map_inclusions_exclusions.csv) - in this fle the two identifiers are linked showing which exclusions apply to which inclusions. as well as identifying which codes require the additional inclusion codes to co-occur.

* [`schnitzer_et_al_2011_empirical_inclusions_age_index.csv`](processed_ICD_codes/schnitzer_et_al_2011_empirical_inclusions_age_index.csv) - this file lists the upper age limit for inclusion codes
* [`schnitzer_et_al_2011_empirical_inclusions_extra_requirements_index_icd10.csv`](processed_ICD_codes/schnitzer_et_al_2011_empirical_inclusions_extra_requirements_index_icd10.csv) - this file lists the additional requirements for a particular inclusion code (only specified in ICD-10 because the relevant inclusion code originally specified in ICD-9-CM does not exist in ICD-9)


A note on file naming: we've called the codes "empirical" inclusions or exclusions because they are based on the codes reported by Schnitzer, et al. [[4]](#4) after they reviewed the cases and confirmed which codes were found using their criteria. There is a longer, "a-priori" list of codes indicative of maltreatment published in the same study, but not all of those codes were found in the case notes they reviewed.


## MVR codes

* [`MVR_codes_long.csv`](processed_ICD_codes/MVR_codes_long.csv) contains all MVR codes in ICD-9 and ICD-10 (merged between [[2]](#2) and [[3]](#3)) in long format, for use in the analysis.


## CCS mental-health-related codes

* [`ccs_categories_icd9.csv`](processed_ICD_codes/ccs_categories_icd9.csv)
* [`ccs_categories_icd10.csv`](processed_ICD_codes/ccs_categories_icd10.csv)
* [`ccs_category_labels.csv`](processed_ICD_codes/ccs_category_labels.csv)

The ICD-9 and ICD-10 files list one code per row, with its corresponding numeric category, which are listed in [`ccs_category_labels.csv`](processed_ICD_codes/ccs_category_labels.csv).


## Other files

Other files were produced for reporting purposes, some of which are:

* [`equivalent_chapters_icd_9_and_10.csv`](processed_ICD_codes/equivalent_chapters_icd_9_and_10.csv) lists equivalent ICD chapters between versions 9 and 10 (including both decimal and roman numeral chapter numbers)
* [`AGI_et_al_2010_ICD_codes.csv`](processed_ICD_codes/AGI_et_al_2010_ICD_codes.csv) contains the extracted ICD-9 and ICD-10 codes used in the González-Izquierdo, et al. (2010) [[2]](#2) paper, provided in spreadsheet format by González-Izquierdo.
* [`MVR_codes_expanded_for_age_check.csv` ](processed_ICD_codes/MVR_codes_expanded_for_age_check.csv) contains all MVR codes of the category *Adverse social circumstances* in ICD-9 and ICD-10 (merged between [[2]](#2) and [[3]](#3)), listed one code per line, with their corresponding meaning
* [`MVR_codes_merged_condensed_for_appendix.csv`](processed_ICD_codes/MVR_codes_merged_condensed_for_appendix.csv) contains all MVR codes in ICD-9 and ICD-10 (merged between [[2]](#2) and [[3]](#3)), condensed in a compact way for printing in an appendix to the paper.



# References

<a id="1">[1]</a>  Dougall, N., Savinc, J., Maxwell, M., Karatzias, T., O’Connor, R. C., Williams, B., ... & Bisson, J. I. (2020). Childhood adversity, mental health and suicide (CHASE): a methods protocol for a longitudinal case-control linked data study. International Journal of Population Data Science, 5(1).

<a id="2">[2]</a>  González-Izquierdo, A., Woodman, J., Copley, L., van der Meulen, J., Brandon, M., Hodes, D., ... & Gilbert, R. (2010). Variation in recording of child maltreatment in administrative records of hospital admissions for injury in England, 1997–2009. Archives of disease in childhood, 95(11), 918-925.

<a id="3">[3]</a> Gilbert, R., Fluke, J., O'Donnell, M., Gonzalez-Izquierdo, A., Brownell, M., Gulliver, P., ... & Sidebotham, P. (2012). Child maltreatment: variation in trends and policies in six developed countries. The Lancet, 379(9817), 758-772.

<a id="4">[4]</a> Schnitzer, P. G., Slusher, P. L., Kruse, R. L., & Tarleton, M. M. (2011). Identification of ICD codes suggestive of child maltreatment. Child abuse & neglect, 35(1), 3-17.

<a id="5">[5]</a> Schnitzer, P. G., Slusher, P., & Van Tuinen, M. (2004). Child maltreatment in Missouri: combining data for public health surveillance. American journal of preventive medicine, 27(5), 379-384.

<a id="6">[6]</a> Elixhauser, A., Steiner, C., & Palmer, L. (2012). Clinical classifications software (CCS). US Agency for Healthcare Research and Quality. https://www.hcup-us.ahrq.gov/toolssoftware/ccsr/ccs_refined.jsp