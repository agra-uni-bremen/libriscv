FROM alpine:edge

RUN apk update && apk add --no-cache build-base cmake gcc-riscv-none-elf \
	newlib-riscv-none-elf emacs nano vim ghc cabal wget libffi-dev clang lld

RUN adduser -G users -g 'libriscv user' -D libriscv
ADD --chown=libriscv:users . /home/libriscv/libriscv

USER libriscv
RUN echo "export RISCV_PREFIX=riscv-none-elf-" >> /home/libriscv/.profile
RUN cd /home/libriscv/libriscv && cabal update && cabal install

ENTRYPOINT ["/bin/sh", "-l"]
