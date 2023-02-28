FROM ubuntu:22.04
#Install all needed libraries
RUN apt-get update -y && apt-get upgrade -y && apt-get install librocksdb-dev git liblzma-dev libnuma-dev curl automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf libncurses-dev clang llvm-13 llvm-13-dev -y

#Install previsous versions of libffi libs
RUN curl -LO http://archive.ubuntu.com/ubuntu/pool/main/libf/libffi/libffi6_3.2.1-8_amd64.deb \
    && dpkg -i libffi6_3.2.1-8_amd64.deb;
RUN apt install libffi6 libffi7 -y 

# Preparing binary to run in container. 
WORKDIR /cardano-markets-tracker
COPY temp-build/tracker-app /cardano-markets-tracker/
COPY tracker/resources/config.dhall /etc/cardano-markets-tracker/
RUN mkdir ./logs
RUN touch ./logs/tracker.log

ENV ENV_CONFIG "/etc/cardano-markets-tracker/config.dhall"

ENTRYPOINT /cardano-markets-tracker/tracker-app ${ENV_CONFIG}