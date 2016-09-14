--------------

title: Using Emacs as an IDE
----------------------------

This page is meant to provide an introduction to using Emacs as a Lisp
IDE. The key bindings used in the example code snippets assume an Emacs
configuration similar to that provided by the
`.emacs <https://github.com/LispCookbook/cl-cookbook/blob/master/.emacs>`__
file that is included as part of the `Setting up an IDE with Emacs on
Windows or Mac OS X <windows.html>`__ page. If you use ILISP, the key
bindings reflect the bindings that are present in the current CVS
version of ILISP.

Why Use Emacs?
--------------

-  Emacs has fantastic support for working with Lisp code
-  Not tying yourself into a single CL vendor's editor
-  Runs on virtually every OS and CL implementation
-  Extensible
-  Can be customized to do many common tasks
-  Built-in support for different source code version control systems
-  Vast number of add-on packages
-  Emacs will probably always be around
-  Emacs works well either with a mouse or without a mouse
-  Emacs has a large user base with multiple newsgroups
-  Benefits of using Emacs far outweigh the effort spent in learning it

Emacs Lisp vs Common Lisp
-------------------------

-  Learning Emacs Lisp is useful and similar (but different from CL):

   -  Dynamic scope is everywhere
   -  No package system
   -  There are no reader (or reader-related) functions
   -  Does not support all the types that are supported in CL
   -  Incomplete implementation of CLOS (with the add-on EIEIO package)
   -  Not all of CL is supported
   -  No numerical tower support

-  Some good Emacs Lisp learning resources:

   -  `An Introduction to Programming in Emacs
      Lisp <http://www.gnu.org/manual/emacs-lisp-intro/emacs-lisp-intro.html>`__
   -  `Writing GNU Emacs
      Extensions <http://www.oreilly.com/catalog/gnuext/>`__

Lisp Modes in Emacs
-------------------

-  There are 4 different alternative major modes to use for CL
   programming:

   -  Inferior Lisp Mode
   -  `ILISP <http://sourceforge.net/projects/ilisp/>`__
   -  `ELI <http://www.franz.com/>`__
   -  `SLIME <http://common-lisp.net/project/slime/>`__

Inferior Lisp Mode
------------------

-  Pros:

   -  Comes with Emacs
   -  Fast start-up, easy setup
   -  Supports many Lisp implementations

-  Cons:

   -  Limited functionality (compared to ILISP and ELI)
   -  No multiprocessing support
   -  Some conflicts with comint mode

-  Setup:

   -  Included with Emacs, so no separate installation required

ILISP
-----

-  Pros:

   -  Vastly superior to Inferior Lisp Mode in functionality
   -  Supports many Lisp implementations

-  Cons:

   -  No multiprocessing support
   -  Some conflicts with comint mode

-  Setup:

   -  A basic installation involves downloading the ILISP package from
      the web building it and configuring it
   -  Customization can be complex, useful to use instructions on `CL
      Cookbook <windows.html>`__ to get started

ELI: Emacs-Lisp Interface
-------------------------

-  Pros:

   -  Supports multiprocessing (this is a big pro)
   -  Has commands that allow you to work with changed definitions
   -  Standard, consistent set of options for managing output
   -  Support for ACL and support from Franz is very good

-  Cons:

   -  For CMUCL and SBCL, only limited functionality is available
   -  No built-in support for accessing either Franz or CL documentation

-  Setup:

   -  Basic ELI setup is very straight-forward for ACL

SLIME: Superior Lisp Interaction Mode for Emacs
-----------------------------------------------

-  Pros:

   -  Provides REPL which is hooked to implementation directly in Emacs
   -  Has integrated Common Lisp debugger with Emacs interface
   -  Interactive object-inspector in Emacs buffer
   -  Has own minor mode which enhances lisp-mode in many ways
   -  Supports every common Common Lisp implementation
   -  Readily available from MELPA
   -  Actively maintained
   -  Symbol completion
   -  Cross-referencing
   -  Can perform macroexpansions

-  Cons:

   -  Installing SLIME without MELPA can be tricky

-  Setup:

   -  Installing it from MELPA is straightforward. Search
      package-list-packages for 'slime' and click to install. If MELPA
      is configured correctly, it will install itself and all
      dependencies.
   -  Run slime with M-x slime

Lisp Modes in Emacs - Which One to Choose?
------------------------------------------

-  My Recommendation:

   -  Inferior Lisp Mode for casual CL programming only
   -  ELI if you use ACL
   -  SLIME otherwise

Working with Lisp Code
----------------------

-  Lisp Editing
-  Evaluating and Compiling Lisp
-  Searching Lisp Code
-  Note: Example code assumes you are using a setup similar to what is
   defined in the `.emacs
   file <https://github.com/LispCookbook/cl-cookbook/blob/master/.emacs>`__
   from the `CL Cookbook <windows.html>`__ site

Working with Lisp Code - Editing
--------------------------------

Forward/Backward/Up/Down movement and selection by s-expressions (`s1.lisp <s1.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s1.lisp %}

Deleting s-expressions ( `s2.lisp <s2.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s2.lisp %}

Indenting s-expressions ( `s3.lisp <s3.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s3.lisp %}

Support for parenthesis ( `s4.lisp <s4.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s4.lisp %}

Automatic code indentation (CL vs Elisp) ( `s5.lisp <s5.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s5.lisp %}

Close all parenthesis ( `s6.lisp <s6.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s6.lisp %}

Code completion ( `s7.lisp <s7.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s7.lisp %}

Hiding/showing code ( `s8.lisp <s8.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s8.lisp %}

Comments ( `s9.lisp <s9.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s9.lisp %}

Working with Lisp Code - Evaluating and Compiling Lisp
------------------------------------------------------

-  buffer
-  region
-  defun
-  sexp (previous/next)
-  DWIM
-  Example code ( `s11.lisp <s11.lisp>`__ )

.. code:: lisp

    {% include code/s11.lisp %}

Working with Lisp Code - Searching Lisp Code
--------------------------------------------

Standard Emacs text search (isearch forward/backward, regexp searches, search/replace) ( `s12.lisp <s12.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s12.lisp %}

Finding occurances (occur, grep) ( `s13.lisp <s13.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s13.lisp %}

Lisp symbols in current source (imenu) ( `s14.lisp <s14.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s14.lisp %}

Lisp symbols using Lisp ( `s15.lisp <s15.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s15.lisp %}

Lisp symbols in multiple source files (etags) ( `s16.lisp <s16.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s16.lisp %}

Lisp symbols using `ECB <http://ecb.sourceforge.net/>`__ ( `s17.lisp <s17.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s17.lisp %}

Lisp Documentation in Emacs - Learning About Lisp Symbols
---------------------------------------------------------

Argument lists ( `s18.lisp <s18.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s18.lisp %}

Documentation ( `s19.lisp <s19.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s19.lisp %}

Describe ( `s20.lisp <s20.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s20.lisp %}

Inspect ( `s21.lisp <s21.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s21.lisp %}

Macroexpand ( `s22.lisp <s22.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s22.lisp %}

Lisp Documentation in Emacs - Lisp Documentation
------------------------------------------------

-  `CL
   HyperSpec <ftp://ftp.lispworks.com/pub/software_tools/documentation/HyperSpec-7-0.tar.gz>`__
-  `CLtL2 <http://www-2.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/lang/lisp/doc/cltl/cltl_ht.tgz>`__
-  `ACL Documenation <http://www.franz.com/support/documentation/>`__
-  Example code ( `s23.lisp <s23.lisp>`__ )

.. code:: lisp

    {% include code/s23.lisp %}

Miscellaneous
-------------

Lisp Listener ( `s24.lisp <s24.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s24.lisp %}

Project Management
~~~~~~~~~~~~~~~~~~

-  `asdf <http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/cclan/asdf/>`__
-  `mk-defsystem <http://sourceforge.net/projects/clocc>`__

Debugging
~~~~~~~~~

-  ILISP has a standarized set of key bindings for debugging across
   implementations ("C-c C-b C-h" for a list of them).

Comparing versions of code ( `s10.lisp <s10.lisp>`__ , `s10a.lisp <s10a.lisp>`__ , `s10b.lisp <s10b.lisp>`__ )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: lisp

    {% include code/s10.lisp %}

Questions/Answers
-----------------

Controlling evaluation output
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*I get irritated by ELI's switching to an output buffer when I
evaluate a sexp in a Lisp source buffer.*

| You can control where ELI output goes to by setting the
| ``fi:pop-up-temp-window-behavior`` variable. Alternatively, you can
  use
| my copy-eval-dwim-lisp function (bound to "C-c x"). It copies Lisp
| code from the source buffer to the listener buffer and evaluates it
| there. Both buffers stay visible and focus remains in the source
| buffer. The code works for ILISP, ELI and Emacs Lisp.

Viewing HyperSpec from within Emacs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*I like having access to the HyperSpec when I'm in Emacs, but why does
it have to use an external browser? Why can't I just see the HyperSpec
in Emacs?*

| If you use the Emacs add-on package W3 (or W3M which provides
| similar functionality), you can display HTML pages inside of
| Emacs. Once you have W3 and the HyperSpec both installed, use code
| similar to the following to access the HyperSpec from the Shift-F1
| key:

.. code:: lisp

    (global-set-key [(shift f1)]
                    '(lambda ()
                      (interactive)
                      (let ((browse-url-browser-function
                             'browse-url-w3)
                            (common-lisp-hyperspec-root
                             "file://c:/home/docs/Hyperspec/")
                            (common-lisp-hyperspec-symbol-table
                             (concat common-lisp-hyperspec-root
                                     "Data/Map_Sym.txt"))
                            (hyperspec-prog
                             "c:/home/site/ilisp/extra/hyperspec"))
                        (load-library hyperspec-prog)
                        (common-lisp-hyperspec
                         (thing-at-point 'symbol)))))

| Note that the "let" in the above code sets the
| browse-url-browser-function to W3 for just the HyperSpec. You can
| either set the variable globally (if you want to always use W3 or some
| other specific browser) or locally (if you want to use a specific
| browser and not the default one).

Standard shell
~~~~~~~~~~~~~~

*I switch between UNIX® and Windows environments and, although
Emacs makes this switch a lot easier, I find it inconvenient having to
use different Shell environments on different operating systems.*

| On Windows, the `Cygwin tools <http://www.cygwin.com/>`__ provide a
| lot of the same tools that are available under UNIX® as well as a BASH
| shell. Alternatively, you might want to consider using eshell, a shell
| written in Emacs Lisp that comes as a standard feature in later
| releases of Emacs. You can access eshell by pressing "F12".

Using ACL tools with Emacs
~~~~~~~~~~~~~~~~~~~~~~~~~~

*I would like to use Emacs with Franz's ACL but find that I use the
Franz tools so much that I can't afford to not load their IDE.*

| It doesn't have to be an either/or decision. On Windows, Franz
| allows you to specify (under Options) that Emacs is to be the default
| editor in place of their built-in editor. On UNIX®, Emacs also works
| very well together with the Franz tools.\*

Windows-style cut/copy/paste
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*I want to use Emacs on a Windows machine. Unfortunately, I have
the Windows cut/copy/paste key bindings burned into my fingertips and
would find it very difficult to switch back and forth between the
Windows standard for these shortcut keys and the Emacs standard.*

| Luckily, you don't have to! Download
  `cua.el <http://www.emacswiki.org/cgi-bin/wiki.pl?CuaMode>`__ and you
  can continue to use the Windows
| defaults. In fact, you may find that the following commands in your
  .emacs file will make Emacs more
| Windows-like:

.. code:: lisp

    ;; Windows-like mouse/arrow movement & selection (pc-selection-mode)
    (delete-selection-mode t)
    ;; C-z=Undo, C-c=Copy, C-x=Cut, C-v=Paste (needs cua.el)
    (require 'cua) (CUA-mode t)

Simplified Emacs setup
~~~~~~~~~~~~~~~~~~~~~~

*There was a lot of Emacs Lisp code presented in this paper. Do I
really have to type in all this stuff to get started with Emacs and
Lisp?*

| No, there is a
| `sample .emacs
  file <https://github.com/LispCookbook/cl-cookbook/blob/master/.emacs>`__
| that can be used to get started. It contains all of the configurations
| that have been described in this page and (hopefully) should work with
| some minor tweaking. See the
| `CL-Cookbook <http://lispcookbook.github.io/cl-cookbook/>`__ page on
| "`Setting up an IDE with Emacs on Windows or Mac OS
  X <windows.html>`__".

Alternatives to Emacs for CL programming
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*I've tried out Emacs and I just can't get used to it. What other
Lisp-friendly alternative are there?*

-  The `Franz <http://www.franz.com/>`__,
   `LispWorks <http://www.lispworks.com/>`__,
   `Corman <http://www.cormanlisp.com/>`__, and
   `Digitool <http://www.digitool.com/>`__ commercial Lisp
   offerings all have Lisp-aware editors.
-  CMUCL has `Hemlock <http://www.cons.org/cmucl/hemlock/index.html>`__,
   which is also `being adapted for other
   Lisps <http://www.stud.uni-karlsruhe.de/~unk6/hemlock/>`__.
-  `XEmacs <http://www.xemacs.org/>`__ is an alternative to GNU Emacs
   that works with many of the same Elisp libraries. Some people prefer
   it to GNU Emacs.
-  Vim can be used to edit Lisp code. An
   `article <http://lisp-p.org/15-vim/>`__ by Larry Clapp gives some
   pointers on how to use Vim with Lisp.
-  `Jabberwocky <http://jabberwocky.sourceforge.net/>`__ is a Lisp
   editor/debugger written in Java.
-  Lastly, for true masochists, notepad on Windows or ed on UNIX® can
   also be used. ;-)

Disclaimer
----------

The original material on this page was originally presented at the `ILC
2003
conference <http://www.international-lisp-conference.org/index.html>`__.
A paper with more in-depth coverage of some of the material on this page
can be found on `Bill Clementson's
ILC2003 <https://web.archive.org/web/20040213103100/http://home.comcast.net/~b.clementson/ilc_2003.htm>`__
page, which is now archived.
