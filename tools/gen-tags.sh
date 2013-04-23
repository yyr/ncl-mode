#!/bin/bash
#
# Copyright (C) 2012, 2013 Yagnesh Raghava Yakkala. http://yagnesh.org
#    File: gen-tags.sh
# Created: Monday, October 24 2011
# License: GPL v3 or later.  <http://www.gnu.org/licenses/gpl.html>
#

# Description:
# Generates TAGS file for ncl source files;
# -e option is to produce emacs format
# NOTE: may fail if the filename has spaces

# USAGE: ctags-gen.sh /path/to/ncl/files

function tag_gen()
{
find ${1:-"."} -type f -name "*.ncl" -print0 | \
xargs -0 -I {} -t ctags -e -a --verbose=yes  --langdef=ncl \
    --langmap=ncl:.ncl --regex-ncl='/^[[:space:]]*function[[:space:]]+([a-zA-Z0-9_]+)[:blank:]*.*/\1/f,function/' \
    --regex-ncl='/^[[:space:]]*procedure[[:space:]]+([a-zA-Z0-9_]+)[:blank:].*/\1/p,procedure/' {}
}

tag_gen ${1:-"."}

# gen-tags.sh ends here
