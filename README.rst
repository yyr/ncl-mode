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

What's is it?.
--------------

ncl-mode is a package for emacs which contains an emacs major mode for NCL and
number of other utilities to help write NCL scripts easily.

Features
--------
- Syntax highlighting (including all function in the latest NCL release).
- Proper indentation.
- auto-complete and company-mode support for auto completion.
- inferior ncl mode for running NCL within the Emacs (for testing scripts
  while writing).
- Proper comment handling.
- Imenu support code navigation with in the file.
- TAGS file support for code navigation across files (consistent with other
  Emacs modes).
- Snippets for each NCL function (more than 1300 snippets). You will never
  need to remember the arguments for any certain function.
- ctags support (ctags generation script included).
- ncl-doc-mode minor mode for browsing/searching NCL documentation from NCAR
  website (my favorite).
- Menu support.

Compatibility
-------------
Emacs-24.1 or above.

Installation
------------

MELPA (recommended way)
~~~~~~~~~~~~~~~~~~~~~~~
``ncl-mode`` can be installed from MELPA. If you have already set melpa in your
sources. Just do

     .. code::

        M-x package-install RET ncl-mode RET

El-get
~~~~~~

If you use ``el-get``, then just do

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

       (load "/path/to/downloaded/ncl-mode/ncl-mode.el")
       (load "/path/to/downloaded/ncl-mode/ncl-doc.el")
       (load "/path/to/downloaded/ncl-mode/inf-ncl.el")

Bug & Feature requests
----------------------

Please open a issue on
`github <https://github.com/yyr/ncl-mode/issues>`__, if you have a patch
want to contribute please drop `me <mailto:hi%E2%97%8Eyagnesh.org>`__ a
mail (replace unicode character) or preferably make a pull request
through github interface.

ncl-doc.el
----------

Helps you read NCL documentation for NCAR website.

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

Use this function you lets you choose from the all keywords ncl-doc has in its
database and takes you to documentation page of that keyword.

To get an idea what ncl-doc does: here I loaded the ncl-doc.el then
called the function "ncl-doc-query-at-point" and given "add" string.

.. image:: https://raw.github.com/yyr/ncl-mode/master/img/ncl-doc-usage.png
   :align: center


inf-ncl.el
----------

Which lets you run NCL within Emacs and interact NCL process from ncl-mode
buffer. auto-complete support is also available for this mode.

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

Todos
-----
See todo.org file.

License
-------
GPL v3 (or later). |license|

NOTE: Please don't confuse this package with the `old less featured another mode
<http://www.ncl.ucar.edu/Applications/Files/ncl.el>`__ written by NCAR people.
