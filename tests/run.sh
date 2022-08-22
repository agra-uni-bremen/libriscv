#!/bin/sh
set -e

cd "$(dirname "$0")"

TRIPLET="${TRIPLET:-riscv32-unknown-elf}"
TESTDIR="${TMPDIR:-/tmp}/riscv-tiny-tests"

mkdir -p "${TESTDIR}"
trap "rm -rf '${TESTDIR}'" INT EXIT

abort() {
	printf "ERROR: ${1}\n" 1>&2
	exit 1
}

compile() {
	[ $# -eq 1 ] || return 1

	# TODO: use clang
	${TRIPLET}-gcc "${1}" -o ${1%%.*} -march=rv32i -mabi=ilp32 -nostdlib
}

runtest() {
	[ $# -eq 2 ] || return 1

	riscv-tiny -r "${1}" 1>"${TESTDIR}/out" 2>&1
	diff -ur "${2}" "${TESTDIR}/out"
}

command -v "${TRIPLET}"-gcc 1>/dev/null 2>&1 || \
	abort "Cross compiler toolchain for '${TRIPLET}' not found"
command -v riscv-tiny 1>/dev/null 2>&1 || \
	abort "riscv-tiny not in \$PATH"


for file in *; do
	[ -d "${file}" ] || continue

	(
		cd "${file}"
		name="${file##*/}"

		printf "Running test case '%s': " "${name}"

		compile test.S
		diff=$(runtest test output)

		if [ $? -ne 0 ]; then
			printf "FAIL: Standard output didn't match.\n\n"
			printf "%s\n" "${diff}"
			exit 1
		else
			printf "OK.\n"
		fi
	)
done
