# CL-HAML

## What is CL-HAML?

CL-HAML is HTML generator for Common Lisp.
Syntax similar to Ruby's library, [Haml](http://haml-lang.com/ "Haml").

## Install

CL-HAML is available on [QuickLisp](http://www.quicklisp.org/beta/).

    (ql:quickload :cl-haml)

or git clone from Github.

    git clone https://github.com/Unspeakable/cl-haml.git

## Execute

    CL-USER> (ASDF:LOAD-SYSTEM :CL-HAML)
    CL-USER> (CL-HAML:EXECUTE-HAML (MERGE-PATHNAMES
                                      "examples/example2.haml"
                                      (ASDF:SYSTEM-SOURCE-DIRECTORY :CL-HAML))
                                   :ENV '(:ARGUMENT "Hello!"))
