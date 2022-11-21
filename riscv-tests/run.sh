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
# Compile the tests
##

banner "Build tests"
make -C src/isa XLEN=${XLEN} rv32ui
echo

##
# Run the tests
##

banner "Run tests"

exit=0
for file in src/isa/rv32ui-p-*; do
	[ -f "${file}" -a -x "${file}" ] || continue

	name=${file##*/}
	printf "Running test case '%s': " "$name"

	if [ "${name}" = rv32ui-p-fence_i ]; then
		printf "SKIP\n"
		continue
	fi

	ret=0; cabal run riscv-test -- -m "${mem_start}" -s "${mem_size}" "${file}" >/dev/null || ret=$?
	if [ "${ret}" -ne 42 ]; then
		exit=1
		printf "FAIL\n"
		continue
	fi

	printf "OK\n"
done

exit "${exit}"
