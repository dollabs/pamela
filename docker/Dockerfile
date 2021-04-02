## Copyright © 2017 Dynamic Object Language Labs Inc.

## This software is licensed under the terms of the
## Apache License, Version 2.0 which can be found in
## the file LICENSE at the root of this distribution.


FROM       ubuntu:20.04
MAINTAINER Prakash <prakash@dollabs.com>

## Notes
# When building new docker images
### Build as `docker build -t pamela .`
### Run image as `docker run --rm -it pamela`
### Tag image as follows for our registry
###    `docker tag pamela registry.gitlab.com/dollabsp/docker/pamela`
### Push image as `docker push registry.gitlab.com/dollabsp/docker/pamela`

# When using docker image from registry
### Pull image as `docker pull registry.gitlab.com/dollabsp/docker/pamela`
### Create a short alias for the image
### docker tag registry.gitlab.com/dollabsp/docker/pamela pamela
###    Pull and alias are only needed __again__ when we need to retrieve a newer version of the image.


### Basic environment setup
ENV APPDIR /Applications
RUN mkdir $APPDIR

######################## Core tools #################################
RUN apt-get update \
    && apt-get install openjdk-8-jdk-headless curl git-core netcat -y

######################## PAMELA ################################
env HOME /root
ENV PATH $HOME/bin:$PATH
ENV PATH /root/src/github/dollabs/pamela/bin:/root/src/github/dollabs/planviz/bin:$PATH

# Since the default user is root
ENV BOOT_AS_ROOT=yes
RUN curl -fsSLo pamela-setup https://raw.githubusercontent.com/dollabs/pamela/master/bin/pamela-setup && chmod +x pamela-setup && ./pamela-setup

# Make pamela.jar available locally
WORKDIR /root/src/github/dollabs/pamela
RUN boot local

## Useful for debugging but otherwise not needed.
#RUN ls -l
#RUN java -version
#RUN pamela --version
#RUN planviz --version

# Default command bash for now. (At some point, pamela command)
CMD bash
