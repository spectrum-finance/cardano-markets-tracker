FROM ubuntu:22.04
#Install all needed libraries
RUN apt-get update -y && apt-get upgrade -y && apt-get install librocksdb-dev git libsodium-dev locales librdkafka-dev liblzma-dev libnuma-dev curl automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf libncurses-dev clang llvm-13 llvm-13-dev -y

#Install previsous versions of libffi libs
RUN curl -LO http://archive.ubuntu.com/ubuntu/pool/main/libf/libffi/libffi6_3.2.1-8_amd64.deb \
    && dpkg -i libffi6_3.2.1-8_amd64.deb;
RUN apt install libffi6 libffi7 -y 

RUN git clone https://github.com/input-output-hk/libsodium
RUN cd libsodium && git checkout 66f017f1 && ./autogen.sh && ./configure && make && make install
ENV LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
ENV PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"

RUN git clone https://github.com/bitcoin-core/secp256k1
RUN cd secp256k1 && git checkout ac83be33 && ./autogen.sh && ./configure --enable-module-schnorrsig --enable-experimental && make && make check && make install

RUN sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8

# Preparing binary to run in container. 
WORKDIR /cardano-markets-tracker
COPY temp-build/tracker-app /cardano-markets-tracker/
COPY tracker/resources/config.dhall /etc/cardano-markets-tracker/
RUN mkdir ./logs

ENV ENV_CONFIG "/etc/cardano-markets-tracker/config.dhall"

ENTRYPOINT /cardano-markets-tracker/tracker-app ${ENV_CONFIG}