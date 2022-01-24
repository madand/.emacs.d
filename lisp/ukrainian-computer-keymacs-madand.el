;;; ukrainian-computer-keymacs-madand.el --- ЙЦУКЕН Ukrainian input method for Keymacs Madand layout  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Andriy B. Kmit'

;; Author: Andriy B. Kmit' <dev@madand.net>
;; Keywords: languages, i18n
;; URL: https://github.com/madand/ukrainian-computer-keymacs-madand.el
;; Package-Requires: ((emacs "23.1"))

;; This file is NOT part of GNU Emacs.

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

;; This package defines new Emacs input method
;; `ukrainian-computer-keymacs-madand'. This is an adaptation of
;; `ukrainian-computer' for the Programmer Dvorak keyboard layout.

;; ```text
;; 'ʼ 1! 2" 3№ 4; 5% 6: 7? 8* 9( 0) -_ =+
;;   Й  Ц  У  К  Е  Н  Г  Ш  Щ  З  Х  Ї
;;    Ф  І  В  А  П  Р  О  Л  Д  Ж  Є  Ґ
;;      Я  Ч  С  М  И  Т  Ь  Б  Ю  .,
;; ```

;;; TL;DR usage

;; Somehow install the package into your `load-path', and then:

;; ```elisp
;; (require 'ukrainian-computer-keymacs-madand)
;; (setq default-input-method 'ukrainian-computer-keymacs-madand)
;; ```

;;; Installation

;; #### Manual Installation

;; Download the file `ukrainian-computer-keymacs-madand.el` from this repo and
;; put it somewhere in your `load-path'.

;; ```shell
;; # Download the latest version of the package
;; mkdir -p ~/path/to/ukrainian-computer-keymacs-madand
;; cd ~/path/to/ukrainian-computer-keymacs-madand
;; wget -q 'https://github.com/madand/ukrainian-computer-keymacs-madand.el/raw/master/ukrainian-computer-keymacs-madand.el'

;; # Alternatively, clone the whole repo.
;; git clone 'https://github.com/madand/ukrainian-computer-keymacs-madand.elgit' ~/path/to/ukrainian-computer-keymacs-madand
;; ```

;; Now update the load path in your `init.el':

;; ```elisp
;; (add-to-load-path (expand-file-name "~/path/to/ukrainian-computer-keymacs-madand"))
;; ```

;; #### Installation with `straight.el'

;; ```elisp
;; (straight-use-package '(ukrainian-computer-keymacs-madand
;;                         :host github
;;                         :repo "madand/ukrainian-computer-keymacs-madand.el"))
;; ```

;; * https://github.com/raxod502/straight.el

;;; Loading with a helper

;; #### `use-package'

;; This assumes the package is installed somewhere in the `load-path':

;; ```elisp
;; (use-package ukrainian-computer-keymacs-madand
;;   :custom (default-input-method 'ukrainian-computer-keymacs-madand)
;; ```

;; * https://github.com/jwiegley/use-package

;; #### `leaf.el'

;; This assumes the package is installed somewhere in the `load-path':

;; ```elisp
;; (leaf ukrainian-computer-keymacs-madand
;;   :require t
;;   :custom (default-input-method . 'ukrainian-computer-keymacs-madand)
;; ```

;; * https://github.com/conao3/leaf.el

;; #### `use-package'+`straight.el'

;; Single declaration that installs, loads and activates the package:

;; ```elisp
;; (use-package ukrainian-computer-keymacs-madand
;;   :straight (:host github :repo "madand/ukrainian-computer-keymacs-madand.el")
;;   :custom (default-input-method 'ukrainian-computer-keymacs-madand)
;; ```

;; * https://github.com/jwiegley/use-package
;; * https://github.com/raxod502/straight.el

;; #### `leaf.el'+`leaf-keywords.el'+`straight.el'

;; Single declaration that installs, loads and activates the package:

;; ```elisp
;; (leaf ukrainian-computer-keymacs-madand
;;   :straight (ukrainian-computer-keymacs-madand :host github
;;                                           :repo "madand/ukrainian-computer-keymacs-madand.el")
;;   :require t
;;   :custom (default-input-method . 'ukrainian-computer-keymacs-madand)
;; ```

;; * https://github.com/conao3/leaf.el
;; * https://github.com/conao3/leaf-keywords.el
;; * https://github.com/raxod502/straight.el

;;; Code:

(require 'quail)

(quail-define-package
 "ukrainian-computer-keymacs-madand" "Ukrainian" "UK" nil
 "ЙЦУКЕН Ukrainian for Programmer Dvorak layout."
 nil t t t nil nil nil nil nil nil t)

;; 'ʼ 1! 2" 3№ 4; 5% 6: 7? 8* 9( 0) -_ =+
;;   Й  Ц  У  К  Е  Н  Г  Ш  Щ  З  Х  Ї
;;    Ф  І  В  А  П  Р  О  Л  Д  Ж  Є  Ґ
;;      Я  Ч  С  М  И  Т  Ь  Б  Ю  .,

(quail-define-rules

 ("~" ?') ("`" ?ʼ)
 ("1" ?1) ("!" ?!)
 ("2" ?2) ("@" ?\")
 ("3" ?3) ("#" ?№)
 ("4" ?4) ("$" ?\;)
 ("5" ?5) ("%" ?%)
 ("6" ?6) ("^" ?:)
 ("7" ?7) ("&" ??)
 ("8" ?8) ("*" ?*)
 ("9" ?9) ("{" ?\()
 ("0" ?0) ("}" ?\))
 ("(" ?-) ("?" ?_)
 (")" ?=) ("+" ?+)

 ("q" ?й) ("Q" ?Й)
 ("b" ?ц) ("B" ?Ц)
 ("p" ?у) ("P" ?У)
 ("f" ?к) ("F" ?К)
 ("g" ?е) ("G" ?Е)
 ("v" ?н) ("V" ?Н)
 ("c" ?г) ("C" ?Г)
 ("l" ?ш) ("L" ?Ш)
 ("y" ?щ) ("Y" ?Щ)
 ("'" ?з) ("\"" ?З)
 ("/" ?х) ("[" ?Х)
 (":" ?ї) ("]" ?Ї)

 ("r" ?ф) ("R" ?Ф)
 ("a" ?і) ("A" ?І)
 ("e" ?в) ("E" ?В)
 ("n" ?а) ("N" ?А)
 ("s" ?п) ("S" ?П)
 ("d" ?р) ("D" ?Р)
 ("o" ?о) ("O" ?О)
 ("t" ?л) ("T" ?Л)
 ("i" ?д) ("I" ?Д)
 ("h" ?ж) ("H" ?Ж)
 ("-" ?є) ("_" ?Є)
 ("\\" ?ґ) ("|" ?Ґ)

 ("z" ?я) ("Z" ?Я)
 ("x" ?ч) ("X" ?Ч)
 ("u" ?с) ("U" ?С)
 ("k" ?м) ("K" ?М)
 ("j" ?и) ("J" ?И)
 ("w" ?т) ("W" ?Т)
 ("m" ?ь) ("M" ?Ь)
 ("," ?б) ("<" ?Б)
 ("." ?ю) (">" ?Ю)
 (";" ?.) ("=" ?,))

(provide 'ukrainian-computer-keymacs-madand)

;;; ukrainian-computer-keymacs-madand.el ends here
