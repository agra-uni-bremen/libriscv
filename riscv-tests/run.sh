#!/bin/sh
set -e

banner() {
	printf "##\n# %s\n##\n\n" "${1}"
}

cd "$(dirname "$0")"

# Compiler configuration for riscv-32
export XLEN=32
export RISCV_GCC_OPTS="-I$(pwd)/include -I$(pwd)/src/env -static -mcmodel=medany -fvisibility=hidden -nostdlib -nostartfiles"

# riscv-test configuration
mem_start=2147483648 # 0x80000000
mem_size=$((1024 * 1024 * 1024))

##
# Compilation
##

banner "Build tests"
make -C src/isa XLEN=${XLEN} rv32ui rv32um
echo

banner "Build interpreter"
mkdir -p bin
cabal install --overwrite-policy=always --installdir ./bin
echo

##
# Run the tests
##

banner "Run tests"

exit=0
for file in src/isa/rv32ui-p-* src/isa/rv32um-p-*; do
	[ -f "${file}" -a -x "${file}" ] || continue

	name=${file##*/}
	printf "Running test case '%s': " "$name"

	if [ "${name}" = rv32ui-p-fence_i ]; then
		printf "SKIP\n"
		continue
	fi

	ret=0; ./bin/riscv-test -m "${mem_start}" -s "${mem_size}" "${file}" || ret=$?
	if [ "${ret}" -ne 0 ]; then
		exit=1
		printf "FAIL\n"
		continue
	fi

	printf "OK\n"
done

exit "${exit}"
