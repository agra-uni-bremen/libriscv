#!/bin/sh

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

	# Cross-compilation flags for rv32i.
	flags="--target=${TRIPLET} -march=rv32i -mabi=ilp32 -nostdlib"

	# Compile test cases with clang (easier cross-compilation).
	# Also: Use implicit make(1) rules to avoid re-compilations.
	make --quiet CC=clang LDFLAGS="$flags" CFLAGS="$flags" ASFLAGS="$flags" "${1}"
}

runtest() {
	[ $# -eq 2 ] || return 1

	riscv-tiny -r "${1}" 1>"${TESTDIR}/out.in"

	# Post process $outfile to remove toolchain-specific output.
	if [ -x post-process.sh ]; then
		./post-process.sh < "${TESTDIR}/out.in" > "${TESTDIR}/out"
	else
		mv "${TESTDIR}/out.in" "${TESTDIR}/out"
	fi

	diff -ur "${2}" "${TESTDIR}/out"
}

command -v make 1>/dev/null 2>&1 || \
	abort "GNU make is required to compile test cases"
command -v clang 1>/dev/null 2>&1 || \
	abort "Tests require Clang with rv32i support"
command -v riscv-tiny 1>/dev/null 2>&1 || \
	abort "riscv-tiny not in \$PATH"

for file in *; do
	[ -d "${file}" ] || continue

	(
		cd "${file}"
		name="${file##*/}"

		printf "Running test case '%s': " "${name}"

		compile test
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
