FROM alpine:3.16

RUN apk update && apk add --no-cache build-base cmake gcc-riscv-none-elf \
	newlib-riscv-none-elf emacs nano vim ghc cabal wget libffi-dev clang lld
RUN adduser -G users -g 'libriscv user' -D libriscv
ADD --chown=libriscv:users . /home/libriscv/libriscv
RUN su - libriscv -c 'echo "export RISCV_PREFIX=riscv-none-elf-" >> .profile'
RUN su - libriscv -c 'cd /home/libriscv/libriscv && cabal update && cabal install'
CMD su - libriscv
