;;; erc-viper.el --- Viper compatibility hacks for ERC

;; Copyright (C) 2005 Free Software Foundation, Inc.

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: emulation

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; Viper is a VI emulation mode for Emacs. ERC and Viper don't quite get
;; along by default; the code in this file fixes that. A simple
;;   (require 'erc-viper)
;; in your ~/.ercrc.el should be all it takes for you to use ERC and
;; Viper together happily.

;;; Code:

(require 'viper)

;; We need this for `erc-mode-hook' and `erc-buffer-list'. Perhaps it
;; would be better to use an `eval-after-load', so that there could be
;; some autodetection / loading of this file from within erc.el?
(require 'erc)

;; Fix RET in ERC buffers, by telling Viper to pass RET through to the
;; normal keymap.

(add-to-list 'viper-major-mode-modifier-list
             '(erc-mode insert-state viper-comint-mode-modifier-map))
(add-to-list 'viper-major-mode-modifier-list
             '(erc-mode vi-state viper-comint-mode-modifier-map))

(viper-apply-major-mode-modifiers)

;; Ensure that ERC buffers come up in insert state.
(add-to-list 'viper-insert-state-mode-list 'erc-mode)

;; Fix various local variables in Viper.
(add-hook 'erc-mode-hook 'viper-comint-mode-hook)

;; Fix local variables in ERC buffers that already exist (buffers in
;; which `erc-mode-hook' has already been run).
(mapc (lambda (buf) (viper-comint-mode-hook))
      (erc-buffer-list))

(provide 'erc-viper)
;;; erc-viper.el ends here
