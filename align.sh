#!/bin/sh
if [ -z ${LISP} ]; then LISP=/usr/bin/sbcl; fi
if [ -z ${LISPCORE} ]; then LISPCORE=/usr/lib/sbcl/sbcl.core; fi
if [ -z ${ASDFSYSTEMS} ]; then ASDFSYSTEMS=/usr/share/common-lisp/systems/; fi

${LISP} --core ${LISPCORE} --noinform --noprint --disable-debugger \
    --eval "(require :asdf)" \
    --eval "(setq asdf:*central-registry* '(#p\"${ASDFSYSTEMS}\"))" \
    --eval "(asdf:operate 'asdf:load-op 'lfgalign)" \
    --eval "(in-package :lfgalign)" \
    --script align-sh.lisp "$@" \
    | grep -v '^;'
