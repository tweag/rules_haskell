FROM ubuntu:xenial-20200114

WORKDIR /home/

RUN apt-get --quiet update
RUN apt-get -y upgrade
RUN apt-get --quiet install -y sudo build-essential libtinfo5 libtinfo-dev python python3 libgmp-dev libffi-dev curl
