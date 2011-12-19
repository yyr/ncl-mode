#!/bin/bash
#
# Copyright (C) Yagnesh Raghava Yakkala. http://yagnesh.org
#    File: ncl-ctags-gen.sh
# Created: Monday, October 24 2011
# License: GPL v3 or later.  <http://www.gnu.org/licenses/gpl.html>
#

# Description:
# to generate TAGS for ncl source files; -e option to produce emacs format
# NOTE: may fail if the filename has spaces
# usage: ncl-ctags-gen.sh /path/to/ncl/files

ctags-exuberant -e -a --verbose=yes  --langdef=ncl \
    --langmap=ncl:.ncl --regex-ncl='/^[[:space:]]*function[[:space:]]+([a-zA-Z0-9_]+)[:blank:]*.*/\1/f,function/' \
    --regex-ncl='/^[[:space:]]*procedure[[:space:]]+([a-zA-Z0-9_]+)[:blank:].*/\1/p,procedure/' `find ${1:-"."} -type f -name "*.ncl"`

# ncl-ctags-gen.sh ends here
