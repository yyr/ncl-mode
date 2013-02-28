#!/usr/bin/env python
'''
Download and collect Ncl keywords from Ncl website
'''

DATE     = "Friday, February 15 2013"
AUTHOR   = "Yagnesh Raghava Yakkala"
WEBSITE  = "http://yagnesh.org/yyr/ncl-mode"
LICENSE  = "GPL v3 or later"

base_url = "http://www.ncl.ucar.edu/Document/"

class KeywordFetcher(object):
    """Fetches and stores ncl keywords.
    """
    def __init__(self, ):
        pass


    def get_ncl_procs_and_funcs():
        cats = [["builtin"    , "ncl built-in functions"                           , "/Document/Functions/Built-in/"]         ,
                ["contrib"    , "contributed functions"                            , "/Document/Functions/Contributed/"]      ,
                ["diag"       , "diagnostics functions"                            , "/Document/Functions/Diagnostics/" ]     ,
                ["pop"        , "pop_remap functions"                              , "/Document/Functions/Pop_remap/"]        ,
                ["shea"       , "shea_util functions"                              , "/Document/Functions/Shea_util/"]        ,
                ["skewt"      , "skewt functions"                                  , "/Document/Functions/Skewt_func/"]       ,
                ["user"       , "user_contributed functions"                       , "/Document/Functions/User_contributed/"] ,
                ["wrfarw"     , "wrf_arw functions"                                , "/Document/Functions/WRF_arw/"]          ,
                ["wrfcontrib" , "wrf_contributed functions"                        , "/Document/Functions/WRF_contributed/"]  ,
                ["windrose"   , "wind_rose functions"                              , "/Document/Functions/Wind_rose/"]        ,
                ["gsn"        , "gsn csm plot templates and special gsn functions" , "/Document/Graphics/Interfaces/"]]
        pass


    def get_ncl_resources():
        pass

    def get_ncl_keywords():
        pass

    def get_ncl_operators():
        operators = ["(/","/)","\ ",".eq.",".ne.",".lt.",".le.",".gt.",
                     ".ge.",".and.",".or.",".not.",".xor."]
        pass


class KeywordWriter(object):
    """update to ncl-keyword
    """
    def __init__(self):
       pass

    def elisp_lines():
        pass

    def update_elisp_file():
        pass


def main():
    elisp_file = "../lisp/ncl-mode-keywords.el"
    pass

if __name__ == '__main__':
    main()
