# Code and data for: Biomass accrual benefits of community-based marine protected areas outweigh their operational costs

## Reproducibility notes

All data analyses were conducted in R within RStudio. All packages are listed in
the `renv.loc` file. In your R console. You can install the exact same packages
and versions used in the development of this project by running the following
lines of code:

```
if(!require(renv)){install.packages("renv")}
renv::restore()
```

The project also contains a `Makefile` listing the dependency and relationship
between code and output files. You can execute the entire project by running
`make -B` in your terminal.

Additionally, a DAG is provided:

![](makefile-dag.png)


## Repository structure

- All R code can be found under `scripts/` folder.
- All data are available in the `data/*` sub-folders (`raw`, `processed`, and `output`)
- All figures and tables are exported in the `results` folder