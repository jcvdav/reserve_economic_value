all: fig tab makefile-dag.png
fig: results/img/*.png
tab: results/tab/*.tex

makefile-dag.png: Makefile
		make -Bnd | make2graph -b | dot -Tpng -Gdpi=300 -o makefile-dag.png

# Make all figures
results/img/%.png: scripts/03_figures_and_tables/%.R
		cd $(<D);Rscript $(<F)

# Make all tables
results/tab/%.tex: scripts/03_figures_and_tables/%.R
		cd $(<D);Rscript $(<F)
