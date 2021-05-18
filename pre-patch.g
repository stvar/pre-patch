# Copyright (C) 2018, 2021  Stefan Vargyas
# 
# This file is part of Pre-Patch.
# 
# Pre-Patch is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Pre-Patch is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Pre-Patch.  If not, see <http://www.gnu.org/licenses/>.

#
# Grammar for unified diff text format
#

# The grammar rules below prescribe with precision where it is
# allowing whitespaces in input text. That is that whitespaces
# in any unified diff text are processed the way they've been
# given and are not removed during text tokenization.
#
# As a result of this rule, the terminal symbols '---', '+++',
# '@@', '-', '+' and ' ' are always placed at the beginning of
# input text lines.
#

# Special notations:
# ~ stands for any non-empty sequence of whitespace characters
#     excepting NL ('\n'); that is any sequence of SPACE (' '),
#     FF ('\f'), HT ('\t'), VT ('\v') and CR ('\r') characters;
# . stands for any character except NL. (This notation overrides
#     the well-established regex notation.)

diff    : entry*
        ;
entry   : text
        | unified
        ;
text    : (?<![-+@ ]) any "\n"
        ;
unified : source target hunk+
        ;
source  : "---" ~ file [stamp] "\n"
        ;
target  : "+++" ~ file [stamp] "\n"
        ;
file    : [^ \f\t\v\r\n]+
        ;
stamp   : ~ any
        ;
hunk    : address line+
        ;
address : "@@" ~ "-" num ["," num] ~ "+" num ["," num] ~ "@@" [annot] "\n"
        ;
num     : [0-9]+
        ;
annot   : ~ any
        ;
line    : [-+ ] any "\n"
        ;
any     : .*
        ;


