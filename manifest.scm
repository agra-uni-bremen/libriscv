(use-modules (guix ui))

;; This file can be used to create an interactive development environment for
;; working on LibRISCV. It re-uses the package definitions from guix.scm but
;; adds additional dependencies to the manifest that are useful for development
;; purposes.
;;
;; Unfournutately, Guix does currently not provide a RV32 cross compiler, hence
;; this is not yet included in the image but may change in the future [1]. For
;; now, a clang toolchain with RISC-V support is included though.
;;
;; [1]: https://issues.guix.gnu.org/63981

(let ((libriscv (load* "guix.scm" (make-user-module '()))))
  (concatenate-manifests
    (list (packages->manifest (list libriscv))
          (specifications->manifest
            ;; Additional packages useful for working on LibRISCV.
            ;;
            ;; TODO: Add fourmolu
            ;; TODO: Add RV32 cross compiler
            (list "cabal-install"
                  "git"
                  "ghc"
                  "gcc-toolchain"

                  ;; For RISC-V cross compilation (see comment above).
                  "clang-toolchain"
                  "lld")))))
