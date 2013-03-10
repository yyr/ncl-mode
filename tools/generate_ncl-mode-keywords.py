#!/usr/bin/env python
'''
'''

DATE = "Thursday, March  7 2013"
AUTHOR = "Yagnesh Raghava Yakkala"
WEBSITE = "http://yagnesh.org"
LICENSE ="GPL v3 or later"

import os
import fetch_ncl_keywords
import datetime as dt
import inspect

file_path = os.path.abspath(os.path.split(inspect.getfile(inspect.currentframe()))[0])

class KeywordWriter(object):
    """update to ncl-keyword
    """
    def __init__(self,elisp_file):
        self.elisp_file = elisp_file
        # self.elisp_file_lines = open(elisp_file).read()
        # self.ncl_keywords = self.fetch_keywords()

    def fetch_keywords(self):
        return NclKeywordFetcher()

    def write_el_file(self,defvars=None):
        """
        """
        header = """
;;; ncl-mode-keywords.el

;; Copyright (C) 2012-%(year)s Yagnesh Raghava Yakkala <http://yagnesh.org>

;; Author: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; URL: https://github.com/yyr/ncl-mode
;; Maintainer: Yagnesh Raghava Yakkala <hi@yagnesh.org>
;; Created: Tuesday, July 24 2012
;; Keywords: ncl, Major Mode, ncl-mode, atmospheric science.

;; This file is NOT part of GNU Emacs.

;; ncl-mode.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ncl-mode.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Auto generated keywords on %(datestring)s

;;; Code:

""" % {'year':dt.datetime.now().year, 'datestring': dt.datetime.now().strftime('%Y-%m-%d')}

        footer = """
(provide 'ncl-mode-keywords.el)
;;; ncl-mode-keywords.el ends here
        """

        fetcher = fetch_ncl_keywords.NclKeywordFetcher()
        defvars = fetcher.keys2defvar()
        fh = open(self.elisp_file,"w")
        fh.write(header + defvars +footer)

def main():
    elisp_file = os.path.join(file_path, "../lisp/ncl-mode-keywords.el")
    if os.path.exists(elisp_file):
        writer = KeywordWriter(elisp_file)
        writer.write_el_file()
    else:
        print(elisp_file + " is not available.")
        sys.exit(2)

if __name__ == '__main__':
    main()
