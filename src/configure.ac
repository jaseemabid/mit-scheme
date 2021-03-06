dnl Process this file with autoconf to produce a configure script.

AC_INIT([MIT/GNU Scheme], [9.2], [bug-mit-scheme@gnu.org], [mit-scheme])
AC_CONFIG_SRCDIR([microcode/boot.c])
AC_CONFIG_AUX_DIR([microcode])
AC_PROG_MAKE_SET

AC_COPYRIGHT(
[Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.
])

if test -f .native-release-marker; then
    IS_NATIVE_RELEASE=yes
else
    IS_NATIVE_RELEASE=no
fi

AC_ARG_ENABLE([debugging],
    AS_HELP_STRING([--enable-debugging],
	[Compile with debugging support [[no]]]))
: ${enable_debugging='no'}

AC_ARG_ENABLE([native-code],
    AS_HELP_STRING([--enable-native-code],
	[Support native compiled code if available [[yes]]]))
: ${enable_native_code=yes}

AC_ARG_ENABLE([host-scheme-test],
    AS_HELP_STRING([--enable-host-scheme-test],
	[Test for working scheme on build host [[if necessary]]]))
if test x"${IS_NATIVE_RELEASE}" = xyes || test -f lib/all.com; then
    : ${enable_host_scheme_test=no}
else
    : ${enable_host_scheme_test=yes}
fi

AC_ARG_ENABLE([cross-compiling],
    AS_HELP_STRING([--enable-cross-compiling],
	[Cross-compile scheme, even if to the same target]))
: ${enable_cross_compiling=no}
if test "x$enable_cross_compiling" = xyes; then
    IF_CROSS=
else
    IF_CROSS=\#
fi

AC_ARG_ENABLE([debug-toolchain],
    AS_HELP_STRING([--enable-debug-toolchain],
	[Use slow interpreted cross-compiler toolchain]))
: ${enable_debug_toolchain=no}
if test "x$enable_debug_toolchain" = xyes; then
    IF_DEBUG_TOOLCHAIN=
    IF_NOT_DEBUG_TOOLCHAIN=\#
else
    IF_DEBUG_TOOLCHAIN=\#
    IF_NOT_DEBUG_TOOLCHAIN=
fi

AC_ARG_WITH([default-target],
    AS_HELP_STRING([--with-default-target],
	[Set the default make target [[all]]]))
if test x"${IS_NATIVE_RELEASE}" = xyes; then
    : ${with_default_target=compile-microcode}
else
    : ${with_default_target=all}
fi
DEFAULT_TARGET=${with_default_target}

AC_CANONICAL_HOST

MIT_SCHEME_NATIVE_CODE([${enable_native_code}],[${host_cpu}])
IF_LIARC=\#
IF_SVM=\#
case ${mit_scheme_native_code} in
c)
    ALL_TARGET=all-liarc
    INSTALL_COM=:
    INSTALL_LIARC_BUNDLES=install-liarc-bundles
    IF_LIARC=
    ;;
svm1)
    ALL_TARGET=all-native
    INSTALL_COM='$(INSTALL_DATA)'
    INSTALL_LIARC_BUNDLES=
    IF_SVM=
    ;;
*)
    ALL_TARGET=all-native
    INSTALL_COM='$(INSTALL_DATA)'
    INSTALL_LIARC_BUNDLES=
    ;;
esac
AUXDIR_NAME=mit-scheme-${mit_scheme_native_code}
AUXDIR='$(libdir)'/${AUXDIR_NAME}

# Please keep the following messages synchronized with the messages in
# these files:
#
#   src/Setup.sh
#   src/configure.ac
#   src/etc/make-native.sh

native_exe=mit-scheme-${mit_scheme_native_code}
if test x"${enable_host_scheme_test}" = xno; then
    : ${MIT_SCHEME_EXE=${native_exe}}
else
    AC_MSG_CHECKING([for an existing MIT/GNU Scheme installation])
    # Kinda hairy logic to detect this: (1) Use MIT_SCHEME_EXE, and
    # don't try to detect anything automagically, if it's set -- this
    # will make broken scripts that set MIT_SCHEME_EXE brokenly fail
    # early.  (2) Otherwise, use mit-scheme-ARCH if we can, because
    # it's guaranteed to have the right compiler.  (3) Try mit-scheme
    # if that fails.  (Will mit-scheme and not mit-scheme-ARCH ever be
    # the right thing?)
    found_p=no
    env_p=no
    if test x"${MIT_SCHEME_EXE}" != x; then
	env_p=yes
	if "${MIT_SCHEME_EXE}" --batch-mode --eval '(%exit)' >/dev/null 2>&1;
	then
	    found_p=yes
	fi
    elif "${native_exe}" --batch-mode --eval '(%exit)' >/dev/null 2>&1; then
	MIT_SCHEME_EXE="${native_exe}"
	found_p=yes
    elif mit-scheme --batch-mode --eval '(%exit)' >/dev/null 2>&1; then
	MIT_SCHEME_EXE=mit-scheme
	found_p=yes
    fi
    if test x"${found_p}" = xyes; then
	AC_MSG_RESULT([yes, using ${MIT_SCHEME_EXE}])
	# To get some version numbers into the build log:
	${MIT_SCHEME_EXE} --version
    else
	message=
	if test x"${env_p}" = xyes; then
	    message="the MIT_SCHEME_EXE environment variable does not run it:

   MIT_SCHEME_EXE=${MIT_SCHEME_EXE}"
	else
	    message="the following programs do not run it:

   ${native_exe}
   mit-scheme"
	fi
	AC_MSG_ERROR([
This script needs an existing MIT/GNU Scheme installation to function,
but ${message}

If you have installed MIT/GNU Scheme in an unusual location, set the
environment variable MIT_SCHEME_EXE to the name or pathname of the
MIT/GNU Scheme executable, which is usually \`mit-scheme' or
\`/usr/local/bin/mit-scheme', and set the environment variable
MITSCHEME_LIBRARY_PATH to the pathname of the MIT/GNU Scheme library
directory, which is usually \`/usr/local/lib/mit-scheme-${mit_scheme_native_code}'.
])
    fi
fi

# A 32bit host running LIAR/svm needs a large heap.
small_words='(= 4 (vector-ref (gc-space-status) 0))'
if test x"${mit_scheme_native_code}" = xsvm1 \
   && ${MIT_SCHEME_EXE} --eval "(%exit (if ${small_words} 0 1))"; then \
    TOOL_TOOLCHAIN_HEAP="--heap 10000"
fi

AC_SUBST([DEFAULT_TARGET])
AC_SUBST([ALL_TARGET])
AC_SUBST([INSTALL_COM])
AC_SUBST([INSTALL_LIARC_BUNDLES])
AC_SUBST([MIT_SCHEME_EXE])
AC_SUBST([AUXDIR_NAME])
AC_SUBST([AUXDIR])
AC_SUBST([IF_CROSS])
AC_SUBST([IF_DEBUG_TOOLCHAIN])
AC_SUBST([IF_LIARC])
AC_SUBST([IF_NOT_DEBUG_TOOLCHAIN])
AC_SUBST([IF_SVM])
AC_SUBST([TOOL_TOOLCHAIN_HEAP])

AC_PROG_INSTALL
AC_PROG_LN_S

echo etc/create-makefiles.sh "${MIT_SCHEME_EXE}" "${mit_scheme_native_code}"
etc/create-makefiles.sh "${MIT_SCHEME_EXE}" "${mit_scheme_native_code}" \
  || exit $?
compiler/configure "${mit_scheme_native_code}" || exit $?

AC_CONFIG_SUBDIRS([microcode])

AC_CONFIG_FILES([
Makefile
Makefile.tools
6001/Makefile
compiler/Makefile
cref/Makefile
edwin/Makefile
ffi/Makefile
imail/Makefile
runtime/Makefile
sf/Makefile
sos/Makefile
ssp/Makefile
star-parser/Makefile
win32/Makefile
xdoc/Makefile
xml/Makefile
])
AC_OUTPUT

if test x"${mit_scheme_native_code}" = xc; then
    BN=liarc-cc
    (cd lib; rm -f ${BN}; ${LN_S} ../microcode/${BN} .)
    BN=liarc-ld
    (cd lib; rm -f ${BN}; ${LN_S} ../microcode/${BN} .)
    for BN in star-parser; do
        (cd lib; rm -f ${BN}; ${LN_S} ../${BN} .)
    done
    for BUNDLE in 6001 compiler cref edwin ffi imail sf sos ssp star-parser \
		  xdoc xml; do
    	SO=${BUNDLE}.so
	(cd lib/lib; rm -f ${SO}; ${LN_S} ../../${BUNDLE}/${SO} .)
    done
fi
