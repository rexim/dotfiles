;; magit requres this lib, but it is not installed automatically on
;; Windows.
(rc/ensure-package-installed 'cl-lib)
(rc/ensure-package-installed 'magit)
