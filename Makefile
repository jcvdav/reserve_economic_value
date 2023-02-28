all: fig tab makefile-dag.png
fig: results/img/*.png
tab: results/tab/*.tex

makefile-dag.png: Makefile
				make -Bnd | make2graph -b | dot -Tpng -Gdpi=300 -o makefile-dag.png

results/img/%.png: scripts/03_figures_and_tables/%.R
		cd $(<D);Rscript $(<F)

results/tab/%.tex: scripts/03_figures_and_tables/%.R
		cd $(<D);Rscript $(<F)
