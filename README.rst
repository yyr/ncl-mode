.. |travis| image:: https://travis-ci.org/yyr/ncl-mode.svg?branch=master
            :target: https://travis-ci.org/yyr/ncl-mode
            :alt: Test status

.. |license| image:: https://img.shields.io/badge/license-GPL_3-green.svg?dummy
             :target: https://github.com/yyr/ncl-mode/blob/master/COPYING
             :alt: License GPL 3

===========================
Ncl-mode |travis|
===========================

Emacs editing mode for Ncar Command Language(NCL).

Introduction.
-------------

ncl-mode package contains emacs major mode and other utilities to help
write Ncl scripts

Please do to confuse this mode with the
`other <http://www.ncl.ucar.edu/Applications/Files/ncl.el>`__ written by
NCAR people. You may think this package newer and hopefully better Emacs
support for Ncl.

Features
--------

-  Easy code navigation (consistent with other Emacs modes)
-  Better indentation
-  Proper comment handling
-  Imenu support (my favorite)
-  Menu support
-  Snippets for yasnippet
-  auto-complete support
-  ctags support (ctags generation script included)
-  ncl-doc-mode minor mode for browsing/searching NCL documentation from
   NCAR website
-  inf-ncl.el mode for running NCL within Emacs.

Compatibility
-------------
Emacs-24.1 or above.

Installation
------------

MELPA
~~~~~
``ncl-mode`` can be installed from MELPA. If you have already set melpa in your
sources. Just do

     .. code::

        M-x package-install RET ncl-mode RET

El-get (recommended way)
~~~~~~~~~~~~~~~~~~~~~~~~

If you ``el-get`` just do

     .. code::

        M-x el-get-install ncl-mode


Manual Installation
~~~~~~~~~~~~~~~~~~~

-  Download the package from the latest release from
   `here <https://github.com/yyr/ncl-mode/downloads>`__
-  unzip/untar put some where in your path (optionally rename it to
   ncl-mode)
-  put the following your ``.emacs``

   .. code:: example

       (load "/path/to/downloaded/ncl-mode/ncl-mode-load.el")

-  That' it. It will take care of all the needed set up for ncl-mode,
   ncl-doc-mode, auto-complete, yasnippet and inf-ncl. If you care to
   know what they are, then read on..

Bug & Feature request
---------------------

Please open a issue on
`github <https://github.com/yyr/ncl-mode/issues>`__, if you have a patch
want to contribute please drop `me <mailto:hi%E2%97%8Eyagnesh.org>`__ a
mail (replace unicode character) or preferably make a pull request
through github interface.

ncl-doc.el
----------

Helps you read NCL documentation for NCAR website.

.. image:: https://raw.github.com/yyr/ncl-mode/master/img/ncl-doc-usage.png
   :align: center


Usage:
------

M-x ncl-doc-query-at-point ( C-c C-s )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This function does few things.

#. It prompts for a string/keyword and collects a given string
#. It tries to find a URL for the given string

   -  if it finds URL for the string call the browser to open that URL
   -  If no URL is found, then goes on to search for the given string.
   -  Displays all search matches in separate buffer category wise. In
      that buffer RET in any search match will call browser for take you
      to that page.
   -  If no matches found for a given string it leaves you there


M-x ncl-doc-query-open ( C-c C-o )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Use this function you lets you choose from the all keywords ncl-doc has
in its database and takes you to that page.

To get an idea what ncl-doc does: here I loaded the ncl-doc.el then
called the function "ncl-doc-query-at-point" and given "add" string.


inf-ncl.el
----------

Which lets you run NCL within Emacs and interact NCL process from
ncl-mode buffer. ac-completion support is also available for this mode.


Utilities
---------

gen-tags.sh
~~~~~~~~~~~

One linper to generate ctags from NCL Source files (look into tools
directory); uses USAGE:

.. code:: example

    gen-tags.sh /path/to/ncl/files

if no arguments supplied it searches for ncl files in current directory
including sub directories


Customization:
--------------

M-x customize-group RET ncl-doc RET check out the following variables

-  `ncl-doc-url-base' = defaults to "http://www.ncl.ucar.edu\ " you can
   change this to any folder if you have downloaded the documentation
   from the NCL website and want read offline.

.. code:: scheme

    (setq ncl-doc-url-base "file:///home/yagnesh/docs/ncl/")


Tip from NCL website FAQ:


.. code:: example

     Is there a way I can download the NCL website documentation so I
     can access it locally on my machine?

     TJ Onley pointed out this nice solution. For example, if you just want
     to download the files under http://www.ncl.ucar.edu/Document, use:


.. code:: sh

     wget -r -l0  -p -np -nH -k http://www.ncl.ucar.edu/Document/



    This will create a directory called "Document", and in that directory,
    you can open the "index.shtml" file with your web browser and have
    access to all the files locally.

-  `ncl-doc-minor-mode-hook'

Todos
-----
See todo.org file.

License
-------
GPL v3 (or later). |license|
