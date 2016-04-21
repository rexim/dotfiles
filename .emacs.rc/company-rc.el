(rc/require 'company)
(rc/require 'company-lua)

(require 'company)
(require 'company-lua)

(custom-set-variables
 '(company-lua-interpreter 'love))

(add-to-list 'company-backends 'company-lua)

(global-company-mode)
