#!/bin/sh
#
# $Id: Clean.sh,v 1.13 2007/04/04 05:08:19 riastradh Exp $
#
# Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
#     1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
#     2005, 2006, 2007 Massachusetts Institute of Technology
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

# Utility for cleaning up the MIT/GNU Scheme edwin directory.
# The working directory must be the edwin directory.

if [ $# -ne 1 ]; then
    echo "usage: $0 <command>"
    exit 1
fi

../etc/Clean.sh "${1}"

echo "rm -rf edwin.bld"
rm -f edwin.bld

echo "rm -f edwin-unx.crf edwin-w32.crf edwin-os2.crf"
rm -f edwin-unx.crf edwin-w32.crf edwin-os2.crf

echo "rm -f edwin-unx.pkd edwin-w32.pkd edwin-os2.pkd"
rm -f edwin-unx.pkd edwin-w32.pkd edwin-os2.pkd

echo "rm -f edwin-unx.fre edwin-w32.fre edwin-os2.fre"
rm -f edwin-unx.fre edwin-w32.fre edwin-os2.fre

exit 0