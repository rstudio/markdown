roxygen:
	Rscript -e "Rd2roxygen::rab('.', install=TRUE)"

tests: tests/tests.Rout.save

tests/tests.Rout.save: tests/tests.R inst/examples/render-options.R
	cd tests && R CMD BATCH --no-save --no-restore --no-timing tests.R tests.Rout.save
