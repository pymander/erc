;;; erc-spelling.el --- use flyspell in ERC

;; Copyright (C) 2005  Free Software Foundation, Inc.

;; Author: Jorgen Schaefer <forcer@forcix.cx>
;; Keywords: irc
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?ErcSpelling

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; This is an ERC module to enable flyspell mode in ERC buffers. This
;; ensures correct behavior of flyspell, and even sets up a
;; channel-local dictionary if so required.

;;; Code:

(require 'erc)
(require 'flyspell)

(defconst erc-spelling-version "$Revision: 1.1 $"
  "ERC spelling revision.")

(defcustom erc-spelling-dictionaries nil
  "An alist mapping buffer names to dictionaries.
The `car' of every cell is a buffer name, the `cadr' is the
string name of an associated dictionary.
The dictionary is inherited from server buffers, so if you want a
default dictionary for some server, you can use a server buffer
name here.")

(define-erc-module spelling nil
  "Enable flyspell mode in ERC buffers."
  ;; Use erc-connect-pre-hook instead of erc-mode-hook as pre-hook is
  ;; called AFTER the server buffer is initialized.
  ((add-hook 'erc-connect-pre-hook 'erc-spelling-init))
  ((remove-hook 'erc-connect-pre-hook 'erc-spelling-init)))

(defun erc-spelling-init ()
  "Enable flyspell mode in an ERC buffer."
  (let ((name (downcase (buffer-name)))
        (dicts erc-spelling-dictionaries))
    (while (and dicts
                (not (string= name (downcase (caar dicts)))))
      (setq dicts (cdr dicts)))
    (setq ispell-local-dictionary
          (if dicts
              (cadr (car dicts))
            (let ((server (erc-server-buffer)))
              (if server
                  (with-current-buffer server
                    ispell-local-dictionary)
                nil)))))
  (flyspell-mode 1))

(put 'erc-mode
     'flyspell-mode-predicate
     'erc-spelling-flyspell-verify)

(defun erc-spelling-flyspell-verify ()
  "Flyspell only the input line, nothing else."
  (> (point)
     (save-excursion
       (goto-char (erc-beg-of-input-line))
       (erc-bol))))

(provide 'erc-spelling)
;;; erc-spelling.el ends here
