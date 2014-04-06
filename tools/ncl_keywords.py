#!/usr/bin/env python
'''
Download and collect Ncl keywords from Ncl website
'''

DATE     = "Friday, February 15 2013"
AUTHOR   = "Yagnesh Raghava Yakkala"
WEBSITE  = "http://yagnesh.org/yyr/ncl-mode"
LICENSE  = "GPL v3 or later"

import sys
import os
import pickle
import inspect
import string
import urllib2
import datetime as dt
from bs4 import BeautifulSoup

file_path = os.path.abspath(os.path.split(inspect.getfile(inspect.currentframe()))[0])
DATA_DIR = os.path.join(file_path,'../data')
base_url = "http://www.ncl.ucar.edu/Document/"
force_download=False

def get_save_page(url,local_file = None):
    """fetch given url and save it to data directory.
    """
    if not os.path.exists(DATA_DIR):
        os.makedirs(DATA_DIR)

    if local_file is None:
        local_file = url.split('/')[-1]

    local_file = os.path.join(DATA_DIR ,  local_file)

    if os.path.exists(local_file) and not force_download:
        fh = open(local_file, "rb")
        print(local_file + " is already exists, skipping ..")
        page = fh.read()
        return page

    else:
        print("Fetching.. '" + url + "' saving as " + local_file)
        fh = open(local_file, "wb")
        user_agent = 'Mozilla/4.0 (compatible; MSIE 5.5; Windows NT)'
        r = urllib2.Request(url=url,headers={'User-Agent' : user_agent})
        try:
            oh = urllib2.urlopen(r)
            page = oh.read()
            fh.write(page)
            return page
        except urllib2.URLError, e:
            print("URLError: %s" % e)
            sys.exit()
        except Exception:
            import traceback
            print('Generic exception: ' + traceback.format_exc())
            sys.exit()

class NclKeywords(object):
    """Fetches and stores ncl keywords.
    """
    def __init__(self, down_from_web = False,
                 el_fname='ncl-mode-keywords.el',
                 dict_file_name = None):
        self.down_from_web = down_from_web
        self.el_fname = el_fname
        self.dict_file_name = dict_file_name
        self.ncl_keywords_p = os.path.join(DATA_DIR,"ncl_keywords.p")
        self.parse_keywords()

    def parse_keywords(self):
        """Parse and Ncl keywords.
        ncl_keys = { 'key_type' : ['name' , 'doc' 'base_url', [list,of,keywords]]
                     'key_type2' : ['name' , 'doc' 'base_url', [list,of,keywords]]
                     ...
                     }
        """
        import pickle
        if not os.path.exists(self.ncl_keywords_p):
            self.ncl_keys = {}
            self.ncl_keys['resources'] = self.parse_ncl_resources()
            self.ncl_keys['keywords']  = self.parse_ncl_keywords()
            self.ncl_keys['operators'] = self.parse_ncl_operators()
            self.parse_ncl_functions()
            pickle.dump(self.ncl_keys,open(self.ncl_keywords_p,'wb'))
            return

        self.ncl_keys = pickle.load(open(self.ncl_keywords_p,'rb'))
        return


    def list_keywords(self):
        self.all_keys = []
        for key in self.ncl_keys:
            if key == 'operators':
                self.all_keys = self.all_keys + self.ncl_keys[key][3][3:]
                continue        # dont print "(/" "/)" "\\"
            self.all_keys = self.all_keys + self.ncl_keys[key][3]

        return '\n'.join(sorted(self.all_keys))


    def update_ncl_dict(self):
        fh = open(self.dict_file_name,"wb")
        return fh.write(self.list_keywords())


    def parse_ncl_functions(self):
        """ Fetch and save ncl procedures/function names.
        """
        # url = "http://www.ncl.ucar.edu/Document/Functions/list_alpha_browse.shtml"
        url_base = "http://www.ncl.ucar.edu/"
        cats = [["builtin"    , "built-in functions."                           , "/Document/Functions/Built-in/"]         ,
                ["contrib"    , "contributed functions."                            , "/Document/Functions/Contributed/"]      ,
                ["diag"       , "diagnostics functions."                            , "/Document/Functions/Diagnostics/" ]     ,
                ["pop"        , "pop_remap functions."                              , "/Document/Functions/Pop_remap/"]        ,
                ["shea"       , "shea_util functions."                              , "/Document/Functions/Shea_util/"]        ,
                ["skewt"      , "skewt functions."                                  , "/Document/Functions/Skewt_func/"]       ,
                ["user"       , "user_contributed functions."                       , "/Document/Functions/User_contributed/"] ,
                ["wrfarw"     , "wrf_arw functions."                                , "/Document/Functions/WRF_arw/"]          ,
                ["wrfcontrib" , "wrf_contributed functions."                        , "/Document/Functions/WRF_contributed/"]  ,
                ["windrose"   , "wind_rose functions."                              , "/Document/Functions/Wind_rose/"]        ,
                ["gsn"        , "gsn csm plot templates and special gsn functions." , "/Document/Graphics/Interfaces/"]]

        # process and get keywords
        for cat in cats:
            var_name = 'ncl_key_' + cat[0]
            var_name = []

            url = url_base + cat[2]
            page = get_save_page(url, cat[0] + ".shtml")
            soup = BeautifulSoup(page)
            if cat[0] == "gsn":
                page_chunk = soup.find('div', attrs = {'id':'general_main'})
                reses = page_chunk.findAll('strong')
                for res in reses:
                    try:
                        var_name.append(res.get_text())
                        # resources.append(string.strip(td.get_text(),'\n'))
                    except AttributeError:
                        continue

                self.ncl_keys[cat[0]] = ['ncl_key_' + cat[0] , 'Ncl ' + cat[1] , url ,var_name]
                continue

            page_chunk = soup.find('div', attrs = {'id':'general_main'})
            tds = soup.findAll('td', attrs = {'valign':'top'})
            for td in tds:
                var_name.append(string.strip(td.get_text(),'\n'))


            self.ncl_keys[cat[0]] = ['ncl_key_' + cat[0] , 'Ncl ' + cat[1] , url , var_name]
        return

    def parse_ncl_resources(self):
        """ Fetch and return ncl resources.
        """
        resources = []
        doc = "Ncl resources."
        url = "http://www.ncl.ucar.edu/Document/Graphics/Resources/list_alpha_res.shtml"
        page = get_save_page(url)
        soup = BeautifulSoup(page)
        page_chunk = soup.find('div', attrs = {'id':'general_main'})
        reses = page_chunk.findAll('dt')
        for res in reses:
            try:
                resources.append(res.strong.get_text())
            except AttributeError:
                continue

        return ['ncl_key_resources', doc, url, resources]

    def parse_ncl_keywords(self):
        """ Fetch and return ncl keywords
        """
        keywords = []
        url  = "http://www.ncl.ucar.edu/Document/Manuals/Ref_Manual/NclKeywords.shtml"
        doc  = 'Reserved Keywords in ncl.'
        page = get_save_page(url)
        soup = BeautifulSoup(page)

        page_chunk = soup.find('pre')
        aas = page_chunk.findAll('a')
        for a in aas:
            keywords.append(a.get_text())

        return ['ncl_key_keywords',doc, url, keywords]

    def parse_ncl_operators(self):
        """Return ncl operators list. Ncl documentation doesn't have a special
        page. so manually typed.
        """
        doc = 'Operators in NCL.'
        url = 'No specific url.'
        operators = ["(/","/)","\\\\",".eq.",".ne.",".lt.",".le.",".gt.",
                     ".ge.",".and.",".or.",".not.",".xor."]
        return ['ncl_key_operators',doc, url, operators]


    def fetch_doc_pages(self):
        """Fetches ncl associated documentation pages."""
        for key in self.ncl_keys:
            print('group:# ' + key )
            if key == 'resources':
                get_save_page(self.ncl_keys[key][2])
                continue

            if key == 'keywords' or key == 'operators':
                continue

            for word in self.ncl_keys[key][3]:
                get_save_page(self.ncl_keys[key][2] + word + ".shtml" )


    def keys2defvar(self):
        """Generate all elisp defvar definitions from keys.
        """
        import re
        el_str = ""
        for key in self.ncl_keys:
            dv = string.replace("(defvar %s '(" % self.ncl_keys[key][0],"_","-")
            k = self.ncl_keys[key][3]
            dv = dv + '"' + '" "'.join(map(str,k)) +'"' + ') "' + self.ncl_keys[key][1] + '")'
            el_str = el_str + dv + "\n"

        return el_str

    def write_el_file(self,defvars=None):
        """
        """
        header = """;;; ncl-mode-keywords.el

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
(provide 'ncl-mode-keywords)
;;; ncl-mode-keywords.el ends here"""

        defvars = self.keys2defvar()
        fh = open(self.el_fname,"wb")
        return fh.write(header + defvars +footer)

def arg_parse(el_fname,
              dict_file_name,
              update_lisp_file=None,
              update_ncl_dict=False,
              list_keywords=False):
    writer = NclKeywords(el_fname=el_fname,dict_file_name=dict_file_name)
    if update_lisp_file:
        writer.write_el_file()
    elif list_keywords:
        print(writer.list_keywords())
    elif update_ncl_dict:
        writer.update_ncl_dict()

def main(args=None):
    import argparse
    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawTextHelpFormatter,description=__doc__)
    parser.add_argument('-l', '--list-keywords',
                         help='Print all ncl keywords',
                         action="store_true",default=False)
    parser.add_argument('-u', '--update-lisp-file',
                        help='Update elisp file with parsed keywords',
                        action="store_true", default=False)
    parser.add_argument('--update-ncl-dict', help='Update ncl-mode dictionary',
                        action="store_true", default=False)
    parser.add_argument('--elisp-file-name', dest='el_fname',
                        action='store',default='ncl-mode-keywords.el')
    parser.add_argument('--dict-file-name',
                        action="store", default='ncl-mode')
    if len(sys.argv) == 1:
        parser.print_help()
    else:
        arg_parse(**vars(parser.parse_args(args)))


if __name__ == '__main__':
    main()
