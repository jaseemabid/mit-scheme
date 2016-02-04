#!/bin/sh
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014,
#     2015, 2016 Massachusetts Institute of Technology
#
# This file is part of MIT/GNU Scheme.
#
# MIT/GNU Scheme is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# MIT/GNU Scheme is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with MIT/GNU Scheme; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
# 02110-1301, USA.

# Build a cross-compiler targeting a new Scheme Virtual Machine.  Use
# it to cross-compile everything.  Use the new machine to finish the
# cross-compile, leaving the build tree ready for build-bands.sh.

set -e

. etc/functions.sh

HOST_ARCH=`echo "(display microcode-id/compiled-code-type)" \
		 | ${@} --batch-mode`

# To avoid miscuing GNU Emacs's compilation-mode.
mydate ()
{
  date "+%Y-%m-%d %H.%M.%S"
}

echo "# `mydate`: Remove the cross-compiler's bands and stash its products."
run_cmd rm -f lib/x-runtime.com
run_cmd rm -f lib/x-compiler.com
run_cmd ./Stage.sh remove 0
run_cmd ./Stage.sh make-cross 0

echo "# `mydate`: Restore the cross-compiler's host-compiled .coms."
run_cmd ./Stage.sh unmake X

echo "# `mydate`: Re-compile the cross-compiler."

echo "# `mydate`:    Re-syntax prerequisites."
for DIR in runtime sf cref; do
    run_cmd_in_dir ${DIR} "${@}" --batch-mode --load ${DIR}.sf </dev/null
done

echo "# `mydate`:    Re-compile prerequisites."
for DIR in runtime sf cref; do
    run_cmd_in_dir ${DIR} "${@}" --batch-mode --load ${DIR}.cbf </dev/null
done
run_cmd_in_dir star-parser "${@}" --batch-mode --load compile.scm </dev/null

echo "# `mydate`:    Dump new runtime into x-runtime.com."
get_fasl_file
run_cmd_in_dir runtime \
    "${@}" --batch-mode --library ../lib --fasl "${FASL}" <<EOF
(disk-save "../lib/x-runtime.com")
EOF
echo ""

echo "# `mydate`:    Re-syntax cross-compiler using x-runtime.com."
run_cmd_in_dir compiler \
    "${@}" --batch-mode --library ../lib --band x-runtime.com <<EOF
(begin
  (load "compiler.sf")
  (sf "base/crsend"))
EOF

if [ -s compiler/compiler-unx.crf ]; then
    echo "compiler/compiler-unx.crf:0: error: not empty!"
    exit 1
fi

echo "# `mydate`:     Re-compile cross-compiler."
run_cmd_in_dir compiler "${@}" --batch-mode --load compiler.cbf </dev/null

echo "# `mydate`: Dump cross-compiler into x-compiler.com."
run_cmd "${@}" --batch-mode --library lib --band x-runtime.com <<EOF
(begin
  (load-option 'SF)
  (load-option 'CREF)
  (load-option '*PARSER)
  (load-option 'COMPILER)
  (load "compiler/base/crsend.com" user-initial-environment)
  (disk-save "lib/x-compiler.com"))
EOF

echo "# `mydate`: Remove host code to STAGEX/ subdirs."
run_cmd ./Stage.sh make X

echo "# `mydate`: Restore previously cross-compiled code."
# (Replace "unmake" with "remove" to start from scratch with each
# rebuilt cross-compiler.)
run_cmd ./Stage.sh unmake 0

if [ "${FAST}" ]; then

    # Use the host-compiled cross-compiler to compile everything.  If
    # the host system is native, this will be much faster than using the
    # svm but will not compile by-procedures.

    if [ "$HOST_ARCH" = "i386" ]; then
	HEAP="--heap 12000"
    else
	HEAP=""
    fi

    echo "# `mydate`: Re-cross-compile everything."
    run_cmd "${@}" --batch-mode --library lib \
		   --band x-compiler.com ${HEAP} <<EOF
(begin
  (load "etc/compile")
  (fluid-let ((compiler:generate-lap-files? #t)
	      (compiler:intersperse-rtl-in-lap? #t)
	      (compiler:cross-compiling? #t))
    (compile-everything)))
EOF

    echo "# `mydate`:     Finish-cross-compilation of fni files."
    run_cmd "${@}" --batch-mode --library lib --band x-compiler.com <<EOF
(finish-cross-compilation:info-files ".")
EOF

    echo "# `mydate`:     Finish-cross-compilation of everything."
    # Use a large heap for all that s-code!
    run_cmd_in_dir runtime \
	../microcode/scheme --batch-mode --library ../lib \
			    --fasl make.bin ${HEAP} <<EOF
(begin
  (load "../compiler/base/crsend")
  (finish-cross-compilation:directory ".."))
EOF

    echo "# `mydate`: Ready to build-bands.sh with the new machine."
    exit 0

fi

# Use the host-compiled LIAR/svm to compile boot-dirs, then use the
# new svm and boot-compiler to compile everything.  This exercises the
# code produced by the cross-compiler, and compiles by-procedure.

# Compilation of machines/svm/assembler-db.bin aborted "out of
# memory" with --heap 8000 on i386 and x86-64.
if [ "$HOST_ARCH" = "i386" ]; then
    HEAP="--heap 9000"
else
    HEAP=""
fi

echo "# `mydate`: Re-cross-compile boot-dirs."
run_cmd "${@}" --batch-mode --library lib \
    --band x-compiler.com ${HEAP} <<EOF
(begin
  (load "etc/compile")
  (fluid-let ((compiler:generate-lap-files? #f)
	      (compiler:intersperse-rtl-in-lap? #f)
	      (compiler:cross-compiling? #t))
    (compile-boot-dirs compile-dir)))
EOF

echo "# `mydate`:     Finish-cross-compilation of boot-dirs fni files."
run_cmd "${@}" --batch-mode --library lib --band x-compiler.com <<EOF
(finish-cross-compilation:info-files ".")
EOF

echo "# `mydate`:     Finish-cross-compilation of boot-dirs."
run_cmd_in_dir runtime \
    ../microcode/scheme --batch-mode --library ../lib --fasl make.bin <<EOF
(begin
  (load "../compiler/base/crsend")
  (finish-cross-compilation:directory ".."))
EOF

echo "# `mydate`: Dump new compiler into boot-compiler.com."
run_cmd_in_dir runtime \
    ../microcode/scheme --batch-mode --library ../lib --fasl "${FASL}" <<EOF
(begin
  (disk-save "../lib/boot-runtime.com")
  (load-option 'SF)
  (load-option 'CREF)
  (load-option '*PARSER)
  (load-option 'COMPILER)
  (disk-save "../lib/boot-compiler.com"))
EOF

run_cmd ./Stage.sh make-clean 0

echo "# `mydate`: Use the new machine and compiler to re-compile everything."
run_cmd ./microcode/scheme --batch-mode --library lib \
    --band boot-compiler.com ${HEAP} <<EOF
(begin
  (load "etc/compile")
  (fluid-let ((compiler:generate-lap-files? #f)
	      (compiler:intersperse-rtl-in-lap? #f))
    ;;This can take 3-4 hours on a Core i3-550 with 4GB.
    (compile-everything)))
EOF

echo "# `mydate`: Ready to build-bands.sh with the new machine."
