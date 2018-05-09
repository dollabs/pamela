## Copyright © 2017 Dynamic Object Language Labs Inc.

## This software is licensed under the terms of the
## Apache License, Version 2.0 which can be found in
## the file LICENSE at the root of this distribution.


FROM       ubuntu:16.04
MAINTAINER Prakash <prakash@dollabs.com>

## Notes
### Build as `docker build -t pamela .`

######################## Core tools #################################
RUN apt-get update \
    && apt-get install openjdk-8-jdk-headless curl git-core -y

RUN java -version

######################## PAMELA ################################
env HOME /root
ENV PATH $HOME/bin:$PATH

# Since the default user is root
ENV BOOT_AS_ROOT=yes
RUN curl -fsSLo pamela-setup https://raw.githubusercontent.com/dollabs/pamela/master/bin/pamela-setup && chmod +x pamela-setup && ./pamela-setup

ENV PATH /root/src/github/dollabs/pamela/bin:/root/src/github/dollabs/planviz/bin:$PATH

RUN pamela --version
RUN planviz --version
