**Comment 31. The manuscript requires complete reporting of software and analytic code.**

**Response:** We appreciate the reviewer for identifying this reporting gap. We have expanded the Statistical software section to report the software environment and all R packages used in data preparation, analysis, and visualization. We have also organised the complete analysis code into an English-language, version-controlled repository. The repository includes the full workflow for data preparation, Table 1, Table S1, Figures 1–5, Supplementary Figures S2–S4, documented source-data requirements, the order in which scripts should be run, and a script that records the exact R session and package versions. No unreported inferential analyses were performed beyond the EAPC confidence intervals described in the Methods. The repository will be made available to the editors and reviewers during peer review and will be publicly archived upon publication at [repository URL/DOI].

Suggested manuscript text:

> All data preparation, analysis, and visualization were conducted in R version [insert exact version] using the dplyr, ggplot2, ggrepel, maps, patchwork, readxl, scales, tibble, and tidyr packages. The complete analysis code, source-data requirements, execution order, and computational session information are available at [repository URL/DOI].

Suggested Code availability statement:

> The complete R code used for data preparation and generation of all reported tables and figures is maintained in a version-controlled repository at [repository URL/DOI]. The repository includes documented input-data requirements and exact computational session information. During peer review, controlled access will be provided to the editors and reviewers through the journal's preferred mechanism; a permanent public version will be archived upon publication.
