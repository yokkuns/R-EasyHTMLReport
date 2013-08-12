pkg_name = EasyHTMLReport
version=`cat ${pkg_name}/DESCRIPTION | grep "Version:" | awk '{print $$2}' | sed -e 's/ //g'`

build:
	R CMD build ${pkg_name}

check: build
	R CMD check --as-cran --timings ${pkg_name}_${version}.tar.gz
