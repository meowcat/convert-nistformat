#!/bin/bash

DOCKER_NAME=rstudio_stravsmi
RSTUDIO_PASS=mypass
RSTUDIO_PORT=8666

case $1 in
    build)
        DOCKER_BUILDKIT=1 docker build . --progress=plain -t spectramapping
        ;;
    kill)
         docker kill $DOCKER_NAME
        ;;
    *)
       docker run \
        --rm -d \
        -e PASSWORD=$RSTUDIO_PASS \
        -e USERID=$UID \
        -v $PWD:/data \
        -v ./rsession.conf:/etc/rstudio/rsession.conf \
        --name $DOCKER_NAME \
        -p $RSTUDIO_PORT:8787 \
        --mount type=tmpfs,destination=/work \
        spectramapping
        ;;
esac



