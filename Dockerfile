FROM bioconductor/bioconductor_docker

RUN R -e "BiocManager::install(c('tidyverse', 'DBI', 'furrr', 'Spectra', 'rcdk', 'progress', 'RSQLite', 'logger'))"
RUN R -e "BiocManager::install(c('meowcat/SpectraMapping', 'CDK-R/rinchi'))"
RUN R -e 'remotes::install_github("CDK-R/rcdklibs@c1f2d125cb00e09c8aac815107b54fe1ef8ebe8f")' && \ 
    R -e 'remotes::install_github("CDK-R/cdkr/rcdk@cef1eed1555947ed82e8303cb6d79c77fd89b3c1")'
