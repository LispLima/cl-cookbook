=====================
Files and Directories
=====================

| Note: In this chapter, we use mainly
| `namestrings <http://www.lispworks.com/documentation/HyperSpec/Body/19_aa.htm>`__
| to
| `specify
  filenames <http://www.lispworks.com/documentation/HyperSpec/Body/19_.htm>`__.
  The
| issue of
| `pathnames <http://www.lispworks.com/documentation/HyperSpec/Body/19_ab.htm>`__
| will be the topic of separate chapter
| `REAL SOON
  NOW <http://www.tuxedo.org/~esr/jargon/html/entry/Real-Soon-Now.html>`__.

Testing whether a File Exists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| Use the function
| ```PROBE-FILE`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_probe_.htm>`__
| which will return a
| `generalized
  boolean <http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_g.htm#generalized_boolean>`__
  -
| either ``NIL`` if the file doesn't exists, or its
| `truename <http://www.lispworks.com/documentation/HyperSpec/Body/20_ac.htm>`__
| (which might be different from the argument you supplied).

.. code:: lisp

    edi@bird:/tmp> ln -s /etc/passwd foo
    edi@bird:/tmp> cmucl
    ; Loading #p"/home/edi/.cmucl-init".
    CMU Common Lisp 18d-pre, level-1 built 2002-01-15 on maftia1, running on bird
    Send questions to cmucl-help@cons.org. and bug reports to cmucl-imp@cons.org.
    Loaded subsystems:
    Python native code compiler, target Intel x86
    CLOS based on PCL version:  September 16 92 PCL (f)
    Gray Streams Protocol Support
    CLX X Library MIT R5.02
    * (probe-file "/etc/passwd")

    #p"/etc/passwd"
    * (probe-file "foo")

    #p"/etc/passwd"
    * (probe-file "bar")

    NIL

Opening a File
~~~~~~~~~~~~~~

| Common Lisp has
| ```OPEN`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm>`__
  and
| ```CLOSE`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_close.htm>`__
| functions which resemble the functions of the same denominator from
  other
| programming languages you're probably familiar with. However, it is
  almost
| always recommendable to use the macro
| ```WITH-OPEN-FILE`` <http://www.lispworks.com/documentation/HyperSpec/Body/m_w_open.htm>`__
| instead. Not only will this macro open the file for you and close it
  when you're
| done, it'll also take care of it if your code leaves the body
  abnormally (such
| as by a use of
| `THROW <http://www.lispworks.com/documentation/HyperSpec/Body/s_throw.htm>`__).
  A
| typical use of WITH-OPEN-FILE looks like this:

.. code:: lisp

    (with-open-file (str <_file-spec_>
    :direction <_direction_>
    :if-exists <_if-exists_>
    :if-does-not-exist <_if-does-not-exist_>)
    <_your code here_>)

-  ``STR`` is a variable which'll be bound to the stream which is
   created by
   opening the file.
-  <*file-spec*> will be a truename or a pathname.
-  <*direction*> is usually ``:INPUT`` (meaning you want to read from
   the file),
   ``:OUTPUT`` (meaning you want to write to the file) or ``:IO`` (which
   is for
   reading *and* writing at the same time) - the default is ``:INPUT``.
-  <*if-exists*> specifies what to do if you want to open a file for
   writing
   and a file with that name already exists - this option is ignored if
   you
   just want to read from the file. The default is ``:ERROR`` which
   means that an
   error is signalled. Other useful options are ``:SUPERSEDE`` (meaning
   that the
   new file will replace the old one), ``NIL`` (the stream variable will
   be bound
   to ``NIL``), and ``:RENAME`` (i.e. the old file is renamed).
-  <*if-does-not-exist*> specifies what to do if the file you want to
   open does
   not exist. It is one of ``:ERROR`` for signalling an error,
   ``:CREATE`` for
   creating an empty file, or ``NIL`` for binding the stream variable to
   ``NIL``. The default is, to be brief, to do the right thing depending
   on the
   other options you provided. See the CLHS for details.

| Note that there are a lot more options to WITH-OPEN-FILE. See
| `the CLHS entry for
  ``OPEN`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_open.htm>`__
| for all the details. You'll find some examples on how to use
  WITH-OPEN-FILE
| below. Also note that you usually don't need to provide any keyword
  arguments if
| you just want to open an existing file for reading.

Using Strings instead of Files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Reading a File one Line at a Time
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| ```READ-LINE`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_lin.htm>`__
| will read one line from a stream (which defaults to
| `*standard
  input* <http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_s.htm#standard_input>`__)
| the end of which is determined by either a newline character or the
  end of the
| file. It will return this line as a string *without* the trailing
  newline
| character. (Note that READ-LINE has a second return value which is
  true if there
| was no trailing newline, i.e. if the line was terminated by the end of
  the
| file.) READ-LINE will by default signal an error if the end of the
  file is
| reached. You can inhibit this by supplying NIL as the second argument.
  If you do
| this, READ-LINE will return NIL if it reaches the end of the file.

.. code:: lisp

    (with-open-file (stream "/etc/passwd")
      (do ((line (read-line stream nil)
           (read-line stream nil)))
           ((null line))
           (print line)))

| You can also supply a third argument which will be used instead of NIL
  to signal
| the end of the file:

.. code:: lisp

    (with-open-file (stream "/etc/passwd")
      (loop for line = (read-line stream nil 'foo)
       until (eq line 'foo)
       do (print line)))

Reading a File one Character at a Time
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

| ```READ-CHAR`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_rd_cha.htm>`__
| is similar to READ-LINE, but it only reads one character as opposed to
  one
| line. Of course, newline characters aren't treated differently from
  other
| characters by this function.

.. code:: lisp

    (with-open-file (stream "/etc/passwd")
      (do ((char (read-char stream nil)
           (read-char stream nil)))
           ((null char))
           (print char)))

Reading a File into String
~~~~~~~~~~~~~~~~~~~~~~~~~~

| It's quite common to need to access the contents of a file in string
| form. While this can be achieved by using ``READ-LINE`` or
  ``READ-CHAR`` functions,
| that probably won't be the best solution. File might not be divided
  into
| multiple lines or reading one character at a time might bring
  significant
| performance problems. To solve this problems, you can read files using
  buckets
| of specific sizes.

.. code:: lisp

    (with-output-to-string (out)
      (with-open-file (in "/path/to/big/file")
        (loop with buffer = (make-array 8192 :element-type 'character)
              for n-characters = (read-sequence buffer in)
              while (< 0 n-characters)
              do (write-sequence buffer out :start 0 :end n-characters)))))

| Furthermore, you're free to change the format of the read/written
  data, instead
| of using elements of type character everytime. For instance, you can
  set
| ``:ELEMENT-TYPE`` type argument of ``WITH-OUTPUT-TO-STRING``,
  ``WITH-OPEN-FILE`` and
| ``MAKE-ARRAY`` functions to ``'(UNSIGNED-BYTE 8)`` to read data in
  octets.

Looking one Character ahead
~~~~~~~~~~~~~~~~~~~~~~~~~~~

| You can 'look at' the next character of a stream without actually
  removing it
| from there - this is what the function
| ```PEEK-CHAR`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_peek_c.htm>`__
| is for. It can be used for three different purposes depending on its
  first
| (optional) argument (the second one being the stream it reads from):
  If the
| first argument is ``NIL``, PEEK-CHAR will just return the next
  character that's
| waiting on the stream:

.. code:: lisp

    CL-USER> (with-input-from-string (stream "I'm not amused")
               (print (read-char stream))
               (print (peek-char nil stream))
               (print (read-char stream))
               (values))

    #\I
    #\'
    #\'

| If the first argument is ``T``, PEEK-CHAR will skip
| `whitespace <http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_w.htm#whitespace>`__
| characters, i.e. it will return the next non-whitespace character
  that's waiting
| on the stream. The whitespace characters will vanish from the stream
  as if they
| had been read by READ-CHAR:

.. code:: lisp

    CL-USER> (with-input-from-string (stream "I'm not amused")
               (print (read-char stream))
               (print (read-char stream))
               (print (read-char stream))
               (print (peek-char t stream))
               (print (read-char stream))
               (print (read-char stream))
               (values))

    #\I
    #\'
    #\m
    #\n
    #\n
    #\o

| If the first argument to PEEK-CHAR is a character, the function will
  skip all
| characters until that particular character is found:

.. code:: lisp

    CL-USER> (with-input-from-string (stream "I'm not amused")
               (print (read-char stream))
               (print (peek-char #\a stream))
               (print (read-char stream))
               (print (read-char stream))
               (values))
      
    #\I
    #\a
    #\a
    #\m

| Note that PEEK-CHAR has further optional arguments to control its
  behaviour on
| end-of-file similar to those for READ-LINE and READ-CHAR (and it will
  signal an
| error by default):

.. code:: lisp

    CL-USER> (with-input-from-string (stream "I'm not amused")
               (print (read-char stream))
               (print (peek-char #\d stream))
               (print (read-char stream))
               (print (peek-char nil stream nil 'the-end))
               (values))
      
    #\I
    #\d
    #\d
    THE-END

| You can also put one character back onto the stream with the function
| ```UNREAD-CHAR`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_unrd_c.htm>`__.
  You
| can use it as if, *after* you have read a character, you decide that
  you'd
| better used PEEK-CHAR instead of READ-CHAR:

.. code:: lisp

    CL-USER> (with-input-from-string (stream "I'm not amused")
               (let ((c (read-char stream)))
                 (print c)
                 (unread-char c stream)
                 (print (read-char stream))
                 (values)))
        
    #\I
    #\I

| Note that the front of a stream doesn't behave like a stack: You can
  only put
| back exactly *one* character onto the stream. Also, you *must* put
  back the same
| character that has been read previously, and you can't unread a
  character if
| none has been read before.

Random Access to a File
~~~~~~~~~~~~~~~~~~~~~~~

| Use the function
| ```FILE-POSITION`` <http://www.lispworks.com/documentation/HyperSpec/Body/f_file_p.htm>`__
| for random access to a file. If this function is used with one
  argument (a
| stream), it will return the current position within the stream. If
  it's used
| with two arguments (see below), it will actually change the
| `file
  position <http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_f.htm#file_position>`__
| in the stream.

.. code:: lisp

    CL-USER> (with-input-from-string (stream "I'm not amused")
               (print (file-position stream))
               (print (read-char stream))
               (print (file-position stream))
               (file-position stream 4)
               (print (file-position stream))
               (print (read-char stream))
               (print (file-position stream))
               (values))
      
    0
    #\I
    1
    4
    #\n
    5
