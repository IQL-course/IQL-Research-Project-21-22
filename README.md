# The optimality of word lengths

This is the supporting repository of two articles, which are the product of the master level course _Introduction to Quantitative Linguistics_ (_IQL_) at Universitat Politècnica de Catalunya (spring semester, 2022). Specifically:
*  _Direct and indirect evidence of compression of word lengths. Zipf's law of abbreviation revisited_ ([arXiv:2303.10128](https://arxiv.org/abs/2303.10128))
*  _The optimality of word lengths. Theoretical foundations and an empirical study_ ([arXiv:2208.10384](https://arxiv.org/abs/2208.10384))

### Authors
 * Sonia Petrini
 * Antoni Casas-i-Muñoz
 * Jordi Cluet-i-Martinell
 * Mengxue Wang
 * Christian Bentz
 * Ramon Ferrer-i-Cancho

### Repository organization
The repository contains the following folders:
 * _code_: all the R and Python code developed to preprocess and analyze the data (running R code requires being located in the parent directory)
 * _data_: Common Voice Forced Alignments and Parallel Universal Dependencies datasets, both filtered (_filtered_ subfolder) and not filtered (_non\_filtered_ subfolder)  as described in the paper. The _other_ subfolder contains other material used throughout the project
 * _figures_: figures produced for the paper, both using the filtered data (_filtered_ subfolder) and the non-filtered data (_non\_filtered_ subfolder)
 * _latex\_tables_: latex tables produced for the paper, both using the filtered data (_filtered_ subfolder) and the non-filtered data (_non\_filtered_ subfolder)
 * _results_: csv files obtained from the analysis, both using the filtered data (_filtered_ subfolder) and the non-filtered data (_non\_filtered_ subfolder)

### Branches
The two branches are related to the first and the second article respectively. The data for _pud_ differs slightly between branches, as we improved its preprocessing after the publication of the first article. However, the changes are minimal, only concern few languages, and do not impact the qualitative results.

### Notes
Throughout the whole repository _pud_ stands for the Parallel Universal Dependencies collection and _cv_ stands for the Common Voice Forced Alignments collection.
