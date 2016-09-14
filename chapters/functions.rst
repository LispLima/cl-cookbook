--------------

title: Functions
----------------

Functions that return functions
-------------------------------

"How do I write a function that returns a function?" is a typical
question asked by people who have learned Scheme before they started
with Common Lisp. In Scheme, they were accustomed to be able to do
things like this:

.. code:: lisp

    ==> (define (adder n) (lambda (x) (+ x n)))
    adder

    ==> ((adder 3) 5)
    8

    ==> (define (doubler f) (lambda (x) (f x x)))
    doubler

    ==> ((doubler +) 4)
    8

This can of course be done in Common Lisp, but the syntax and the
semantics are different. The first step, creating a function that
returns a function, looks very similar apart from minor syntactical
conventions, but what happens behind the scenes is different:

.. code:: lisp

    CL-USER> (defun adder (n) (lambda (x) (+ x n)))
    ADDER

Here we have defined the function ``adder`` which returns an *object* of
*type*
```function`` <http://www.lispworks.com/documentation/HyperSpec/Body/t_fn.htm>`__.
To create such an object you'll have to use the special operator
```function`` <%3Ca%20href=%22http://www.lispworks.com/documentation/HyperSpec/Body/s_fn.htm>`__
and apply it to a *lambda expression*. ``(function`` *form*\ ``)`` may
be abbreviated as ``#'``\ *form*. In our example above we used a
shorthand notation provided by the macro
```lambda`` <http://www.lispworks.com/documentation/HyperSpec/Body/m_lambda.htm>`__.
Without this little bit of syntactical sugar we would have to write it
as

.. code:: lisp

    CL-USER> (defun adder (n) #'(lambda (x) (+ x n)))
    ADDER

or

.. code:: lisp

    CL-USER> (defun adder (n) (function (lambda (x) (+ x n))))
    ADDER

No matter how we write it, ``adder`` will now return a function whenever
we call it. But we *can't* use it in the same way we would use it in
Scheme:

.. code:: lisp

    CL-USER> (adder 3)
    #<Interpreted Function "LAMBDA (N)" {485FFE81}>

    CL-USER> ((adder 3) 5)
    In: (ADDER 3) 5
        ((ADDER 3) 5)
    Error: Illegal function call.

Here is why: CL has different *namespaces* for functions and variables,
i.e. the same *name* can refer to different things depending on it's
position in a form that's evaluated:

.. code:: lisp

    CL-USER> (boundp 'foo)
    NIL
    CL-USER> (fboundp 'foo)
    NIL
    CL-USER> (defparameter foo 42)
    FOO
    * foo
    42
    CL-USER> (boundp 'foo)
    T
    CL-USER> (fboundp 'foo)
    NIL
    CL-USER> (defun foo (x) (* x x))
    FOO
    CL-USER> (fboundp 'foo)
    T
    * foo            ;;; ***
    42
    CL-USER> (foo 3)        ;;; +++
    9
    CL-USER> (foo foo)
    1764
    CL-USER> (function foo)
    #<Interpreted Function FOO {48523CC1}>
    * #'foo
    #<Interpreted Function FOO {48523CC1}>
    CL-USER> (let ((+ 3)) (+ + +))
    6

To simplify a bit, you can think of each symbol in CL having (at least)
two "cells" in which information is stored. One cell - sometimes
referred to as its *value cell* - can hold a value that is *bound* to
this symbol, and you can use
```boundp`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_boundp.htm>`__
to test whether the symbol is bound to a value (in the global
environment). You can access the value cell of a symbol with
```symbol-value`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_5.htm>`__.

The other cell - sometimes referred to as its *function cell* - can hold
the definition of the symbol's (global) function binding. In this case,
the symbol is said to be *fbound* to this definition. You can use
```fboundp`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_fbound.htm>`__
to test whether a symbol is fbound. You can access the function cell of
a symbol (in the global environment) with
```symbol-function`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_1.htm>`__.

Now, if a *symbol* is evaluated, it is treated as a *variable* in that
it's value cell is returned - see the line marked with \_\*\*\*\_ above.
If a *compound form*, i.e. a *cons*, is evaluated and its *car* is a
symbol, then the function cell of this symbol is used - see the line
marked *+++* above.

In Common Lisp, as opposed to Scheme, it is *not* possible that the car
of the compound form to be evaluated is an arbitrary form. If it is not
a symbol, it *must* be a *lambda expression*, which looks like

``(lambda``\ *lambda-list* \_form\*\_\ ``)``

This explains the error message we got above - ``(adder 3)`` is neither
a symbol nor a lambda expression. But, you might ask, how *do* we use
the function object that is returned by ``adder``? The answer is: Use
```funcall`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_funcal.htm>`__
or
```apply`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_apply.htm>`__:

.. code:: lisp

    ;;; continued from above
    CL-USER> (funcall (adder 3) 5)
    8
    CL-USER> (apply (adder 3) '(5))
    8
    CL-USER> (defparameter *my-fun* (adder 3))
    *MY-FUN*
    * *my-fun*
    #<Interpreted Function "LAMBDA (N)" {486468C9}>
    CL-USER> (funcall *my-fun* 5)
    8
    CL-USER> (*my-fun* 5)
    Warning: This function is undefined:
      *MY-FUN*

Note that in the last example the function object returned by
``(adder 3)`` is stored in the *value cell* of ``*my-fun*`` - thus the
error message. If we want to be able to use the symbol ``*my-fun*`` in
the car of a compound form, we have to explicitely store something in
its *function cell* (which is normally done for us by the macro
```defun`` <http://www.lispworks.com/documentation/HyperSpec/Body/m_defun.htm>`__):

.. code:: lisp

    ;;; continued from above
    CL-USER> (fboundp '*my-fun*)
    NIL
    CL-USER> (setf (symbol-function '*my-fun*) (adder 3))
    #<Interpreted Function "LAMBDA (N)" {4869FA19}>
    CL-USER> (fboundp '*my-fun*)
    T
    CL-USER> (*my-fun* 5)
    8

Now we are ready do define ``doubler`` as well:

.. code:: lisp

    CL-USER> (defun doubler (f)
        (lambda (x) (funcall f x x)))
    DOUBLER
    CL-USER> (doubler #'+)
    #<Interpreted Function "LAMBDA (F)" {48675791}>
    CL-USER> (doubler '+)
    #<Interpreted Function "LAMBDA (F)" {486761B1}>
    CL-USER> (funcall (doubler #'+) 4)
    8
    CL-USER> (funcall (doubler '+) 4)
    8
    CL-USER> (defparameter *my-plus* '+)
    *MY-PLUS*
    CL-USER> (funcall (doubler *my-plus*) 4)
    8
    CL-USER> (defparameter *my-fun* (doubler '+))
    *MY-FUN*
    CL-USER> (funcall *my-fun* 4)
    8

Note that the argument to ``funcall`` (and ``apply``) can either be the
function itself, i.e. ``#'+``, or a symbol which has the function in its
function cell (is fbound to the function), i.e. ``'+``.

All of the above is *extremely simplified* - we haven't even mentioned
macros, special forms, symbol macros, self-evaluating objects, and
lexical environments. Read the CLHS section about `form
evaluation <http://www.lispworks.com/documentation/HyperSpec/Body/03_aba.htm>`__
for the real deal.

Currying functions
------------------

A related concept is that of
*`currying <%3Ca%20href=%22http://www.cs.jhu.edu/~scott/pl/lectures/caml-intro.html#higherorder>`__*
which you might be familiar with if you're coming from a functional
language. After we've read the last section that's rather easy to
implement:

.. code:: lisp

    CL-USER> (declaim (ftype (function (function &rest t) function) curry) (inline curry))
    NIL
    CL-USER> (defun curry (function &rest args)
               (lambda (&rest more-args)
                   (apply function (append args more-args))))
    CURRY
    CL-USER> (funcall (curry #'+ 3) 5)
    8
    CL-USER> (funcall (curry #'+ 3) 6)
    9
    CL-USER> (setf (symbol-function 'power-of-ten) (curry #'expt 10))
    #<Interpreted Function "LAMBDA (FUNCTION &REST ARGS)" {482DB969}>
    CL-USER> (power-of-ten 3)
    1000

Note that the
```declaim`` <http://www.lispworks.com/documentation/HyperSpec/Body/m_declai.htm>`__
statement above is just a hint for the compiler so it can produce more
efficient code if it so wishes. Leaving it out won't change the
semantics of the function.
