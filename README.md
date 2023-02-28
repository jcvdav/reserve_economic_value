# Code and data for: Operational costs and biomass accrual benefits of community-based marine reserves

## Reproducibility notes

This project contains a `Makefile` listing the dependency and relationship
between files. You can execute the entire project by running `make` in your terminal.

Additionally, a DAG is provided:

~()[makefile-dag.png]

All packages are listed in the `renv.loc` file. In your R console. You can install
the exact same packages and versions used in the development of this project by running
the following lines of code:

```
# install.packages("renv")
renv::restore()
```


## Repository structure

- All R code can be found under `scripts/` folder.
- All data are available in the `data/*` sub-folders (`raw`, `processed`, and `output`)
- All figures and tables are exported in the `results` folder