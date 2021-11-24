;;; packages.el --- org-config layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author:  <machuan@LAPTOP-LRSTIUM0>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `org-config-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-config/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-config/pre-init-PACKAGE' and/or
;;   `org-config/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org-config-packages
  '(
    ;; Owned
    org-ref
    calfw
    calfw-org
    calfw-cal
    calfw-ical
    helm-bibtex
    citeproc-org
    auctex
    )
  "The list of Lisp packages required by the org-config layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun org-config/post-init-org-ref()

  (use-package org-ref
    :ensure t
    ;;:defer 1
    :after (org)
    :config
    ;;(setq reftex-default-bibliography '("~/OneDrive/2020.03.28_PunchingShearReferences/Literature.bib"))
    ;; see org-ref for use of these variables
    (setq bibtex-completion-pdf-field "file")
    (setq org-ref-bibliography-notes "~/literature/refnotes.org"
          org-ref-default-bibliography '("~/literature/bibliography.bib")
	        org-ref-pdf-directory "~/literature/pdfs")
    ;;(setq bibtex-completion-bibliography "~/OneDrive/2020.03.28_PunchingShearReferences/Literature.bib"
    ;;    bibtex-completion-library-path "~/OneDrive/2020.03.28_PunchingShearReferences/PDFs"
    ;;    bibtex-completion-notes-path "~/OneDrive/2020.03.28_PunchingShearReferences/Literature-manuscript.org")
    (setq org-ref-show-broken-links nil)
    (setq bibtex-completion-pdf-open-function 'org-open-file)
    (setq org-ref-note-title-format
          "** TODO %k - %t
 :PROPERTIES:
  :CUSTOM_ID: %k
  :AUTHOR: %9a
  :JOURNAL: %j
  :DOI: %D
  :URL: %U
 :END:
")

    (setq bibtex-completion-display-formats
	        '((t . "${author:20} ${year:4} ${=has-pdf=:3} ${=has-note=:1} ${=type=:7} ${title:90}")))
    (defun my/org-ref-notes-function (candidates)
      (let ((key (helm-marked-candidates)))
        (funcall org-ref-notes-function (car key))))

    (helm-delete-action-from-source "Edit notes" helm-source-bibtex)
    ;; Note that 7 is a magic number of the index where you want to insert the command. You may need to change yours.
    (helm-add-action-to-source "Edit notes" 'my/org-ref-notes-function helm-source-bibtex 7)
    )
  )

(defun org-config/init-calfw()
  (use-package calfw)
  )

(defun org-config/init-calfw-org()
  (use-package calfw-org)
  )

(defun org-config/init-calfw-cal()
  (use-package calfw-cal)
  )

(defun org-config/init-calfw-ical()
  (use-package calfw-ical)
  )

(defun org-config/post-init-helm-bibtex()
  ;; init helm-bibtex
  (use-package helm-bibtex
    :ensure t
    :after (org)
    :bind ("<f11>" . helm-bibtex)
    :commands (helm-bibtex)
    :init
    (add-hook 'bibtex-completion-edit-notes 'org-ref-open-bibtex-notes)
    (setq bibtex-completion-open-any 'org-ref-open-bibtex-pdf)
    :config
    (setq bibtex-completion-bibliography "~/literature/bibliography.bib"
          bibtex-completion-library-path "~/literature/pdfs"
          bibtex-completion-notes-path "~/literature/refnotes.org")
                                        ;(setq bibtex-completion-display-formats
                                        ;  '((t . "${=type=:7} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${author:30} ${title:72} ")))
    (setq bibtex-completion-additional-search-fields '(keywords))
    (setq bibtex-completion-notes-template-one-file
	        (format "\n** TODO ${=key=} - ${title}\n  :PROPERTIES:\n    :Author: ${author-or-editor}\n    :Journal: ${journal}\n  :END:\n\n"))
    (setq bibtex-completion-display-formats
	        '((t . "${author:20} ${year:4} ${=has-pdf=:3} ${=has-note=:1} ${=type=:7} ${title:90}")))
    (setq bibtex-completion-pdf-field "file")
    (setq bibtex-completion-pdf-symbol "PDF")
    (setq bibtex-completion-notes-symbol "N")
    )
  )

(defun org-config/init-citeproc-org()
  (use-package citeproc-org))

(defun org-config/init-auctex()
  ;; we don't need to use auctex
  )
