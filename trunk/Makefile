output = .output
package = rworldmap
backup = .backup

all:
	R CMD build --resave-data --compact-vignettes $(package)

test:
	R CMD build $(package)
	
check:
	R CMD check --as-cran $(package)*.tar.gz

bnc:
	R CMD build --resave-data --compact-vignettes $(package)
	R CMD check --as-cran $(package)*.tar.gz
	
novin:
	R CMD build  --no-vignettes $(package)

install:
	R CMD INSTALL --build $(package)
tex:
	cd $(output); texify --run-viewer --pdf $(package).tex

clean:
	-rm $(output)/*

backup:
	-mkdir $(backup)
	cp $(output)/* $(backup)/*