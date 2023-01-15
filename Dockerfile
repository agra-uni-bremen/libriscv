FROM alpine:3.17

RUN apk update && apk add --no-cache build-base cmake gcc-riscv-none-elf \
	newlib-riscv-none-elf emacs nano vim ghc cabal wget libffi-dev clang lld

# This is a workaround for a bug in GCC which prevents it from compiling
# simdutf (as vendored by the Text Haskell package) [1]. Unfortunately,
# there is also a bug in Cabal which prevents it from passing custom
# compiler options through to GHC [2].
#
# [1]: https://gitlab.alpinelinux.org/alpine/aports/-/issues/14105
# [2]: https://github.com/haskell/cabal/issues/8637
RUN printf '#!/bin/sh\nexec ghc "$@" -optcxx -D_FORTIFY_SOURCE=0\n' \
	> /usr/local/bin/ghc-no-fortify && chmod +x /usr/local/bin/ghc-no-fortify

RUN adduser -G users -g 'libriscv user' -D libriscv
ADD --chown=libriscv:users . /home/libriscv/libriscv

RUN su - libriscv -c 'echo "export RISCV_PREFIX=riscv-none-elf-" >> .profile'
RUN su - libriscv -c 'cd /home/libriscv/libriscv && cabal update && cabal install --with-ghc=/usr/local/bin/ghc-no-fortify'
CMD su - libriscv
