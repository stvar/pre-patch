#!/usr/bin/python

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

import sys, os, re, ctypes
from itertools import imap

program = 'pre-patch'
verdate = '0.1 2018-01-29 17:00' # $ date +'%F %R'

license = """\
Copyright (C) 2018  Stefan Vargyas.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law."""

def joinln(args):
    return "\n".join(imap(str, args))

def write(where, msg, args):
    s = isinstance(msg, str)
    l = isinstance(msg, list) or \
        isinstance(msg, tuple)
    n = len(args)
    if l:
        msg = joinln(msg)
    elif not s:
        msg = str(msg)
    if s and n:
        msg = msg % args
    elif not s and n:
        msg += "\n" + joinln(args)
    where.write(msg)

def cout(msg = "", *args):
    write(sys.stdout, msg, args)

def cerr(msg = "", *args):
    write(sys.stderr, msg, args)

def error(msg, *args):
    if len(args):
        msg = msg % args
    cerr("%s: error: %s\n", program, msg)
    sys.exit(1)

def warn(msg, *args):
    if len(args):
        msg = msg % args
    cerr("%s: warning: %s\n", program, msg)

def is_done(iter):
    if iter is None:
        return True
    try:
        iter.next()
    except StopIteration:
        return True
    else:
        return False

class Libc:

    class Struct_tm(ctypes.Structure):

        _fields_ = [
            ('tm_sec',    ctypes.c_int),
            ('tm_min',    ctypes.c_int),
            ('tm_hour',   ctypes.c_int),
            ('tm_mday',   ctypes.c_int),
            ('tm_mon',    ctypes.c_int),
            ('tm_year',   ctypes.c_int),
            ('tm_wday',   ctypes.c_int),
            ('tm_yday',   ctypes.c_int),
            ('tm_isdst',  ctypes.c_int),
            ('tm_gmtoff', ctypes.c_long),
            ('tm_zone',   ctypes.POINTER(
                          ctypes.c_char)),
        ]

    def __init__(self):
        self.libc = ctypes.CDLL('libc.so.6')
        self.libc.strptime.argtypes = [
            ctypes.c_char_p,
            ctypes.c_char_p,
            ctypes.POINTER(self.Struct_tm)]
        self.libc.strptime.restype = \
            ctypes.POINTER(ctypes.c_char)
        self.libc.mktime.argtypes = [
            ctypes.POINTER(self.Struct_tm)]
        self.libc.mktime.restype = \
            ctypes.c_long
        self.libc.memset.argtypes = [
            ctypes.c_void_p,
            ctypes.c_int,
            ctypes.c_size_t]
        self.libc.memset.restype = \
            ctypes.c_void_p
        self.libc.setlinebuf.argtypes = [
            ctypes.c_void_p]
        self.libc.fnmatch.argtypes = [
            ctypes.c_char_p,
            ctypes.c_char_p,
            ctypes.c_int]
        self.libc.fnmatch.restype = \
            ctypes.c_int
        self.stdout = ctypes.c_void_p.in_dll(
            self.libc, 'stdout')
        assert self.stdout != 0

    def setlinebuf(self):
        self.libc.setlinebuf(self.stdout)

    def fnmatch(self, glob, str):
        g = ctypes.c_char_p(glob)
        s = ctypes.c_char_p(str)
        return self.libc.fnmatch(g, s, 0) == 0

    @staticmethod
    def diff_ptr(a, b):
        return \
            ctypes.cast(a, ctypes.c_void_p).value - \
            ctypes.cast(b, ctypes.c_void_p).value

    def strptime(self, str, fmt):
        s = ctypes.c_char_p(str)
        f = ctypes.c_char_p(fmt)
        t = self.Struct_tm()
        u = ctypes.byref(t)
        n = ctypes.sizeof(t)
        ctypes.memset(u, 0, n)
        r = self.libc.strptime(s, f, u)
        if r:
            return \
                self.diff_ptr(r, s), \
                self.libc.mktime(u)
        else:
            return None

libc = Libc()

class Rex:

    def __init__(self, expr):
        self.expr = expr
        self.regex = None
        self.result = None

    def compile(self):
        if self.regex is None:
            self.regex = re.compile(self.expr)
        return bool(self.regex)

    def match(self, text):
        assert self.compile()
        self.result = self.regex.match(text)
        return bool(self.result)

    def search(self, text):
        assert self.compile()
        self.result = self.regex.search(text)
        return bool(self.result)

    def remove(self, text):
        assert self.compile()
        return self.regex.sub("", text, 1)

    def clear(self, text):
        assert self.compile()
        self.result, c = self.regex.subn(
            "", text, 1)
        return c > 0

    def __getitem__(self, index):
        assert self.result is not None
        return self.result.group(index)

    def start(self, index):
        assert self.result is not None
        return self.result.start(index)

    def end(self, index):
        assert self.result is not None
        return self.result.end(index)

class Range:

    def __init__(self, first, last):
        self.first = first
        self.last = last

    def includes(self, val):
        return \
            (self.first == 0 or \
             self.first <= val) and \
            (self.last == 0 or \
             self.last >= val)

class Lexeme:

    def __init__(self, start, end):
        self.start = start
        self.end = end

class Lineno:

    def __init__(self, lineno):
        self.lno = lineno

    def lineno(self, where, opt, off = 0):
        opt.line_numbers and \
        where('%d:' % (self.lno + off))

    @staticmethod
    def make(lineno):
        return Lineno(lineno).lineno

class Text:

    def __init__(self, lineno, text):
        self.lineno = Lineno.make(lineno)
        self.text = text

    def write(self, where, opt):
        self.lineno(where, opt)
        opt.clear_text or \
        where(self.text)

    def filter(self, opt, state):
        return bool(opt.clear_text)

    def stats(self, where, opt):
        pass

    def fixup_next(self, fiter):
        return fiter

class Address:

    def __init__(self, text, source, target, annot):
        self.text = text
        self.source = source
        self.target = target
        self.annot = annot
        self.hunks = None

    def __getitem__(self, index):
        if index == 0:
            return self.source
        if index == 1:
            return self.target
        else:
            assert False

    def set_hunks(self, h0, h1):
        self.hunks = h0, h1

    EMPTY_FILE = Rex(r"^0+(?:,0+)?$")

    def is_empty_file(self, index):
        return self.EMPTY_FILE.match(
            self.text[
            self[index].start:
            self[index].end])

    def write(self, where, opt):
        if opt.hunk_numbers:
            where(self.text[:-1])
            assert self.hunks is not None
            where(" #%d" % self.hunks[0])
            self.hunks[0] != self.hunks[1] and \
            where(",%d" % self.hunks[1])
            where("\n")
        else:
            where(self.text)

    REX = Rex(r"^([0-9]+)((?:,[0-9]+)?)$")

    def fixup_addr(self, lexeme, offset):
        assert self.REX.match(self.text[
            lexeme.start:
            lexeme.end])
        return \
            str(int(self.REX[1]) + offset) + \
            self.REX[2]

    def fixup(self, offset):
        self.text = \
        self.text[:self.source.start] + \
        self.fixup_addr(self.source, offset) + \
        self.text[self.source.end:self.target.start] + \
        self.fixup_addr(self.target, offset) + \
        self.text[self.target.end:]

class Hunk:

    def __init__(self, lineno, addr, lines):
        self.lineno = Lineno.make(lineno)
        self.addr = addr
        self.lines = lines

    def set_hunk_nums(self, n0, n1):
        self.addr.set_hunks(n0, n1)

    def write(self, where, opt):
        self.lineno(where, opt)
        self.addr.write(where, opt)
        k = 0
        for l in self.lines:
            k += 1
            self.lineno(where, opt, k)
            where(l)

    def fixup(self, offset):
        self.addr.fixup(offset)

class Header:

    def __init__(self, text, file, stamp, time):
        self.text = text
        self.file = file
        self.stamp = stamp
        self.time = time

    def file_name(self, opt = None):
        n = self.text[
            self.file.start:
            self.file.end]
        if not opt: return n
        # stev: opt.clear_suffix =>
        #       opt.clear_prefix
        assert \
            opt.clear_suffix is None or \
            opt.clear_prefix is not None
        if opt.clear_prefix is not None and \
            opt.clear_prefix.clear(n):
            n = opt.clear_prefix.result
            if opt.clear_suffix is not None:
                n = opt.clear_suffix.remove(n)
            return True, n
        else:
            return False, n

    def write(self, where, opt):
        if opt.clear_tstamps:
            e = self.stamp.start
        else:
            e = len(self.text)
        p, f = self.file_name(opt)
        if p:
            where(self.text[:self.file.start])
            where(f)
            where(self.text[self.file.end:e])
        elif opt.clear_tstamps:
            where(self.text[:e])
        else:
            where(self.text)
        if opt.clear_tstamps:
            where('\n')

class Unified:

    def __init__(self, lineno, source, target, hunks):
        self.lineno = Lineno.make(lineno)
        self.source = source
        self.target = target
        self.hunks = hunks

    def file_mode(self):
        h = self.hunks[0]
        s = h.addr.is_empty_file(0)
        t = h.addr.is_empty_file(1)
        assert not s or not t
        if s:
            return '+'
        if t:
            return '-'
        else:
            return '!'

    NORM = Rex(r"^.*?/+")

    NAME_SOURCE = 0
    NAME_TARGET = 1
    NAME_SUFFIX = 2

    def file_name(self, opt):
        if opt.name_globbing != self.NAME_TARGET:
            s = self.source.file_name(opt)[1]
        if opt.name_globbing == self.NAME_SOURCE:
            return s
        if opt.name_globbing != self.NAME_SOURCE:
            t = self.target.file_name(opt)[1]
        if opt.name_globbing == self.NAME_TARGET:
            return t
        assert opt.name_globbing == self.NAME_SUFFIX

        if s == '/dev/null':
            return t
        if t == '/dev/null':
            return s

        i, j = len(s), len(t)
        assert i > 0
        assert j > 0

        # stev: compute longest common suffix
        while i > 0 and j > 0:
            i -= 1
            j -= 1
            if s[i] != t[j]:
                if i + 1 < len(s):
                    i += 1
                    j += 1
                else:
                    # i == len(s) - 1
                    # j == len(t) - 1
                    i = 0
                    j = 0
                break

        if i == 0 and j == 0:
            return s

        if i == 0:
            u = s
        elif j == 0:
            u = t
        else:
            assert i < len(s)
            # stev: => len(u) > 0
            u = s[i:]

        u = self.NORM.remove(u)
        return u or s

    def write(self, where, opt):
        self.lineno(where, opt)
        self.source.write(where, opt)
        self.lineno(where, opt, 1)
        self.target.write(where, opt)
        for h in self.hunks:
            h.write(where, opt)

    def filter(self, opt, state):
        return \
            Filter.exclude_unif(self, \
                opt, state) or \
            Filter.remove_hunks(self, \
                opt, state)

    def stats(self, where, opt):
        where(self.file_mode())
        where(' ')
        where(self.file_name(opt))
        where("\n")

    def fixup(self, entry):
        s = self.source.file_name()
        assert entry.file == s
        t = self.target.file_name()
        assert entry.file == t
        entry.fixup(self.hunks)

    def fixup_next(self, fiter):
        try:
            self.fixup(fiter.next())
        except StopIteration:
            return None
        else:
            return fiter

class Diff:

    def __init__(self, entries):
        self.entries = entries

class Filter:

    @staticmethod
    def regex_hunk(hunk, opt):
        if opt.hunk_regex is None:
            return True
        for l in hunk.lines:
            if opt.apply_regex and \
                l[0] != opt.apply_regex:
                continue
            if opt.hunk_regex.search(l):
                return not opt.invert_regex
        return opt.invert_regex

    @staticmethod
    def range_hunk(hunk, opt):
        if opt.hunk_range is None:
            return True
        for r in opt.hunk_range:
            if r.includes(hunk):
                return not opt.invert_range
        return opt.invert_range

    @staticmethod
    def is_new_file(hunk, opt):
        if not opt.exclude_new:
            return False
        # stev: exclusive or
        return \
            hunk.addr.is_empty_file(0) != \
            opt.invert_new

    @staticmethod
    def exclude_hunk(hunk, num, opt):
        return \
            not Filter.regex_hunk(hunk, opt) or \
            not Filter.range_hunk(num, opt) or \
                Filter.is_new_file(hunk, opt)

    @staticmethod
    def remove_hunks(unif, opt, state):
        k = 0
        d = []
        for h in unif.hunks:
            k += 1
            if opt.hunk_numbers:
                h.set_hunk_nums(*state.hunk_nums)
            if Filter.exclude_hunk(
                    h, state.hunk_nums[0], opt):
                d.append(k - 1)
            else:
                state.hunk_nums[1] += 1
            state.hunk_nums[0] += 1
            if state.dry_run:
                continue
            if opt.max_lines:
                h.lines[opt.max_lines:] = ' ...\n'
            elif opt.max_lines is not None:
                del h.lines[:]
        if not state.dry_run:
            for k in reversed(d):
                del unif.hunks[k]
            return len(unif.hunks) == 0
        else:
            return len(unif.hunks) == len(d)

    @staticmethod
    def match_file(name, globs, empty):
        if not len(globs):
            return empty
        for g in globs:
            if libc.fnmatch(g, name):
                return True
        return False

    @staticmethod
    def include_unif(unif, opt):
        f = unif.file_name(opt)
        return \
            not Filter.match_file(
                    f, opt.exclude_files, False) and \
                Filter.match_file(
                    f, opt.include_files, True)

    @staticmethod
    def exclude_unif(unif, opt, state):
        if not Filter.include_unif(unif, opt):
            state.hunk_nums[0] += len(unif.hunks)
            return True
        return False

class Processor:

    class State:

        def __init__(self, dry_run):
            self.dry_run = dry_run
            self.hunk_nums = [1, 1]

    def __init__(self, diff, dry_run):
        self.entries = diff.entries
        self.state = self.State(
            dry_run)

    def write(self, where, opt):
        for e in self.entries:
            if not e.filter(opt, self.state):
                self.write_entry(e, where, opt)

class Printer(Processor):

    def __init__(self, diff):
        Processor.__init__(self, diff, False)

    @staticmethod
    def write_entry(entry, where, opt):
        entry.write(where, opt)

class Statser(Processor):

    def __init__(self, diff):
        Processor.__init__(self, diff, True)

    @staticmethod
    def write_entry(entry, where, opt):
        entry.stats(where, opt)

class Fixuper(Printer):

    def __init__(self, diff):
        Printer.__init__(self, diff)

    def write(self, where, opt):
        f = iter(opt.fixup.entries)
        for e in self.entries:
            if f is not None:
                f = e.fixup_next(f)
            if not e.filter(opt, self.state):
                e.write(where, opt)
        assert is_done(f)

class BaseParser:

    def __init__(self, input):
        self.input = input
        self.open_input()

    def open_input(self):
        if self.input is None:
            self.file = sys.stdin
        else:
            try:
                self.file = open(self.input)
            except:
                error("opening '%s'", self.input)

    def name(self):
        return self.input or '<stdin>'

    def debug(self, msg, *args):
        cerr("!!! %s:%d: %s\n", self.name(), self.lno, msg % args)

    def warn(self, msg, *args):
        warn("%s:%d: %s", self.name(), self.lno, msg % args)

    def error(self, pos, msg, *args):
        error("%s:%d: %s", self.name(), pos, msg % args)

    def error_context(self):
        from traceback import extract_stack
        for t in reversed(extract_stack()):
            if t[2].startswith('parse_'):
                return t[2][6:].replace('_', ' ')
            if t[2] == 'parse':
                return 'input'
        return None

    def invalid_line(self, line, part = False):
        c = self.error_context()
        assert c is not None
        if not part:
            f = "invalid %s line\n  %s"
            l = line[:-1]
        else:
            f = "invalid %s: %s"
            l = line
        self.error(self.lno - 1, f, c, l)

    TOK_BOF = 1 << 0
    TOK_EOF = 1 << 1

    def missed_tok(self):
        if self.tok == self.TOK_EOF:
            self.error(self.lno - 1,
                "unexpected end of file")
        else:
            self.error(self.lno - 1,
                "unexpected token")

    def need_tok(self, tok):
        if self.tok != tok:
            self.missed_tok()
        elif tok != self.TOK_EOF:
            self.eat_tok()

    def peek_tok(self, tok):
        return bool(self.tok & tok)

    def try_tok(self, tok):
        if self.peek_tok(tok):
            self.eat_tok()
            return True
        else:
            return False

    def parse_closure(
            self, parse, tok, empty = False):
        r = []
        if not empty:
            r.append(parse())
        while self.peek_tok(tok):
            r.append(parse())
        return r

class DiffParser(BaseParser):

    def __init__(self, opt):
        BaseParser.__init__(self, opt.input_file)
        self.yielding = \
            opt.yielding_parse and \
            opt.action != Act.parse
        # stev: '... or None' below ensures
        # that all 'Header.time' be 'None'
        # if 'opt.check_tstamps' is 'False'
        self.check_timestamps = \
            opt.check_tstamps or None

    TOK_SOURCE = 1 << 2 # '--- '
    TOK_TARGET = 1 << 3 # '+++ '
    TOK_ADDR   = 1 << 4 # '@'
    TOK_LINE   = 1 << 5 # '-', '+', ' ', '\\'
    TOK_TEXT   = 1 << 6 # anything else

    def next_tok(self):
        self.line = self.file.readline()
        assert isinstance(self.line, str)
        self.lno += 1

        if len(self.line) == 0:
            self.tok = self.TOK_EOF
        elif self.line.startswith('--- '):
            self.tok = self.TOK_SOURCE
        elif self.line.startswith('+++ '):
            self.tok = self.TOK_TARGET
        elif self.line.startswith('@'):
            self.tok = self.TOK_ADDR
        elif self.line.startswith(
                ('-', '+', ' ', '\\')):
            self.tok = self.TOK_LINE
        else:
            self.tok = self.TOK_TEXT

    def eat_tok(self):
        self.prev = self.line
        self.next_tok()

    def parse_tok(self, tok, constr = None):
        self.need_tok(tok)
        if constr is not None:
            return constr(
                text = self.prev,
                lineno = self.lno - 1
            )
        else:
            return self.prev

    @staticmethod
    def zoffset(zone):
        o = int(zone)
        if o < 0:
            s = -1
            o = -o
        else:
            s = 1
        assert o < 10000
        return \
            ((o / 100)  * 60 +
             (o % 100)) * 60 * s

    ZONE = Rex(r"^\s+([-+]\d{4})$")
    TAIL = Rex(r"^\.\d{9}" + ZONE.expr[1:])

    FMTS = ((
        "%Y-%m-%d %H:%M:%S", TAIL
    ),(
        "%a %b %e %T %Y", ZONE
    ),(
        "%b %Y %H:%M:%S", ZONE
    ))

    def parse_timestamp(self, stamp):
        if not len(stamp):
            return None
        for f, r in self.FMTS:
            t = libc.strptime(stamp, f)
            if t is None:
                continue
            if r.match(stamp[t[0]:]):
                return t[1] + self.zoffset(r[1])
            else:
                break
        self.invalid_line(stamp, True)

    ADDRESS = Rex(r"^@@\s+\-([0-9]+(?:,[0-9]+)?)" \
                     r"\s+\+([0-9]+(?:,[0-9]+)?)\s+@@((?:\s+.*)?)$")

    # any     : .*
    #         ;
    # num     : [0-9] +
    #         ;
    # annot   : ~ any
    #         ;
    # address : "@@" ~ "-" num ["," num] ~ "+" num ["," num] ~ "@@" [annot] "\n"
    #         ;
    def parse_address(self):
        r = self.parse_tok(self.TOK_ADDR)
        if not self.ADDRESS.match(r):
            self.invalid_line(r)
        return Address(
            text = r,
            source = Lexeme(
                start = self.ADDRESS.start(1),
                end = self.ADDRESS.end(1),
            ),
            target = Lexeme(
                start = self.ADDRESS.start(2),
                end = self.ADDRESS.end(2),
            ),
            annot = Lexeme(
                start = self.ADDRESS.start(3),
                end = self.ADDRESS.end(3),
            )
        )

    # line    : [-+ \] any "\n"
    #         ;
    def parse_line(self):
        return self.parse_tok(self.TOK_LINE)

    # hunk    : address line+
    #         ;
    def parse_hunk(self):
        return Hunk(
            lineno = self.lno,
            addr = self.parse_address(),
            lines = self.parse_closure(
                self.parse_line,
                self.TOK_LINE
            )
        )

    HEADER = Rex(r"^[-+]{3}\s+([^\s]+)((?:\s+(.*?)\s*)?)$")

    # file    : [^\s]+
    #         ;
    # stamp   : ~ any
    #         ;
    # header  : ( "---" | "+++" ) ~ file [stamp] "\n"
    #         ;
    def parse_header(self, which):
        r = self.parse_tok(which)
        if not self.HEADER.match(r):
            self.invalid_line(r)
        return Header(
            text = r,
            file = Lexeme(
                start = self.HEADER.start(1),
                end = self.HEADER.end(1)
            ),
            stamp = Lexeme(
                start = self.HEADER.start(2),
                end = self.HEADER.end(2),
            ),
            time = self.check_timestamps and \
                   self.parse_timestamp(
                 r[self.HEADER.start(3):
                   self.HEADER.end(3)]
            )
        )

    # source  : "---" ~ file [stamp] "\n"
    #         ;
    # target  : "+++" ~ file [stamp] "\n"
    #         ;
    # unified : source target hunk+
    #         ;
    def parse_unified(self):
        return Unified(
            lineno = self.lno,
            source = self.parse_header(
                self.TOK_SOURCE),
            target = self.parse_header(
                self.TOK_TARGET),
            hunks = self.parse_closure(
                self.parse_hunk,
                self.TOK_ADDR
            )
        )

    # text    : (?<! [-+@ \]) any "\n"
    #         ;
    # entry   : text
    #         | unified
    #         ;
    def parse_entry(self):
        if self.peek_tok(self.TOK_TEXT):
            return self.parse_tok(self.TOK_TEXT, Text)
        elif self.peek_tok(self.TOK_SOURCE):
            return self.parse_unified()
        else:
            self.missed_tok()

    # diff    : entry*
    #         ;
    def parse_diff(self):
        e = self.parse_closure(
            self.parse_entry,
            self.TOK_SOURCE |
            self.TOK_TEXT,
            empty = True
        )
        self.need_tok(self.TOK_EOF)
        return e

    # diff    : entry*
    #         ;
    def parse_diff_yielding(self):
        while self.peek_tok(
            self.TOK_SOURCE |
            self.TOK_TEXT):
            yield self.parse_entry()
        self.need_tok(self.TOK_EOF)

    def parse(self):
        self.lno = 0
        self.next_tok()
        return Diff(
            entries = [
                self.parse_diff,
                self.parse_diff_yielding][
                self.yielding]()
        )

class Fixup:

    class Hunk:

        def __init__(self, index, lineno, offset):
            self.index = index
            self.lineno = lineno
            self.offset = offset

        def fixup(self, hunks):
            assert self.index > 0
            assert self.index <= len(hunks)
            hunks[self.index - 1].fixup(self.offset)

    class Fail:

        def __init__(self, index, lineno):
            self.index = index
            self.lineno = lineno

        def fixup(self, hunks):
            pass

    class Entry:

        def __init__(self, file, hunks, errs):
            self.file = file
            self.hunks = hunks
            self.errs = errs

        def fixup(self, hunks):
            for e in self.hunks:
                e.fixup(hunks)

    def __init__(self, entries):
        self.entries = entries

class FixupParser(BaseParser):

    def __init__(self, input):
        self.input = input
        self.open_input()

    TOK_FILE = 1 << 2
    TOK_HUNK = 1 << 3
    TOK_FAIL = 1 << 4
    TOK_ERRS = 1 << 5

    FILE = Rex(r"^checking file (.+)$")
    HUNK = Rex(r"^Hunk #([0-9]+) succeeded at ([0-9]+) "
               r"\(offset ([0-9]+) lines\)\.$")
    FAIL = Rex(r"^Hunk #([0-9]+) FAILED at ([0-9]+)\.$")
    ERRS = Rex(r"^([0-9]+) out of [0-9]+ hunk FAILED$")

    def next_tok(self):
        self.line = self.file.readline()
        assert isinstance(self.line, str)
        self.lno += 1

        if len(self.line) == 0:
            self.tok = self.TOK_EOF
            self.lex = None
        elif self.FILE.match(self.line):
            self.tok = self.TOK_FILE
            self.lex = self.FILE[1]
        elif self.HUNK.match(self.line):
            self.tok = self.TOK_HUNK
            self.lex = int(self.HUNK[1]), \
                       int(self.HUNK[2]), \
                       int(self.HUNK[3])
        elif self.FAIL.match(self.line):
            self.tok = self.TOK_FAIL
            self.lex = int(self.FAIL[1]), \
                       int(self.FAIL[2])
        elif self.ERRS.match(self.line):
            self.tok = self.TOK_ERRS
            self.lex = int(self.ERRS[1])
        else:
            self.invalid_line(self.line)

    def eat_tok(self):
        self.prev = self.lex
        self.next_tok()

    def parse_tok(self, tok):
        self.need_tok(tok)
        return self.prev

    # hunk   : HUNK | FAIL
    #        ;
    def parse_hunk(self):
        if self.try_tok(self.TOK_HUNK):
            return Fixup.Hunk(
                index = self.prev[0],
                lineno = self.prev[1],
                offset = self.prev[2]
            )
        if self.try_tok(self.TOK_FAIL):
            return Fixup.Fail(
                index = self.prev[0],
                lineno = self.prev[1]
            )
        else:
            return self.missed_tok()

    # hunks  : hunk *
    #        ;
    def parse_hunks(self):
        return self.parse_closure(
            self.parse_hunk,
            self.TOK_HUNK |
            self.TOK_FAIL,
            True
        )

    # errs   : ERRS ?
    #        ;
    def parse_errs(self):
        if self.try_tok(self.TOK_ERRS):
            return self.prev
        return None

    # fixup  : FILE hunks errs
    #        ;
    def parse_fixup(self):
        return Fixup.Entry(
            file = self.parse_tok(self.TOK_FILE),
            hunks = self.parse_hunks(),
            errs = self.parse_errs()
        )

    # fixups : fixup +
    #        ;
    def parse(self):
        self.lno = 1
        self.next_tok()
        e = self.parse_closure(
            self.parse_fixup,
            self.TOK_FILE
        )
        self.need_tok(self.TOK_EOF)
        return Fixup(entries = e)

class Act:

    @staticmethod
    def parse(opt):
        p = DiffParser(opt)
        return p.parse()

    @staticmethod
    def print_diff(opt):
        p = Printer(Act.parse(opt))
        p.write(cout, opt)

    @staticmethod
    def print_stats(opt):
        p = Statser(Act.parse(opt))
        p.write(cout, opt)

    @staticmethod
    def fixup_diff(opt):
        p = Fixuper(Act.parse(opt))
        p.write(cout, opt)

class Options:

    def __init__(self):
        self.action = Act.print_diff
        self.apply_regex = 0
        self.input_file = None
        self.include_files = []
        self.exclude_files = []
        self.exclude_new = False
        self.hunk_numbers = False
        self.hunk_regex = None
        self.hunk_range = None
        self.clear_prefix = None
        self.clear_suffix = None
        self.clear_text = False
        self.clear_tstamps = False
        self.check_tstamps = True
        self.invert_regex = False
        self.invert_range = False
        self.invert_new = False
        self.max_lines = None
        self.line_buffered = False
        self.line_numbers = False
        self.name_globbing = \
                Unified.NAME_SUFFIX
        self.yielding_parse = False
        self.fixup = None
        self.parse()

    def parse(self):
        args = sys.argv[1:]

        help = \
"""\
usage: %s [ACTION|OPTION]... [FILE]
where the actions are:
  -O|--parse-only              only parse input -- no output generated
  -P|--print-diff              print out a processed input (default)
  -S|--print-stats             print out statistics info
  -F|--fixup-diff=FILE         fixup hunk addresses
and the options are:
  -a|--apply-regex=WHERE       restrict applicating the hunk regex to diff text
                                 lines specified; WHERE is either 'all' (default),
                                 'del', 'add' or 'equ' for all, deleted, added or
                                 common lines respectively; the short option `-a'
                                 accepts shortcut arguments: '@', '-', '+' or '='
  -d|--[no-]line-buffered      use line buffering on output; this improves the
                                 script's responsiveness when run interactively;
                                 the default buffering is system's default; note
                                 that `--no-line-buffering' means the script is
                                 using the type of buffering given by the system
  -e|--hunk-regex=REGEX        filter hunks from output according to given regex
     --no-hunk-regex
  -f|--input-file=FILE         input plain diff file (default: '-', i.e. stdin)
  -g|--name-globbing=WHAT      select the diff name to use when globbing with
                                 `-i|--include-files' or `-x|--exclude-files';
                                 WHAT is either 'source' or 'target' for the
                                 corresponding diff file name, or, otherwise,
                                 'suffix' for the longest common path name
                                 suffix between diff's source and target file
                                 names; there's an exception when one of the
                                 file name is '/dev/null': then use the other
                                 file name; if no such suffix exists, then use
                                 the source file name; 'suffix' is the default
  -h|--[no-]hunk-numbers       annotate each hunk address with hunk numbers
  -i|--include-files=GLOB      include in output only files matching given globbing
     --no-include-files          pattern
  -l|--max-lines=NUM           print out at most NUM lines for each hunk
     --no-max-lines
  -m|--[no-]check-timestamps   do not check timestamps or otherwise do (default do)
  -n|--[no-]line-numbers       prefix each line of output with line number within
                                 the input file
  -p|--clear-prefix=STR        remove shortest prefix preceding STR from diff file
     --no-clear-prefix           names
  -r|--hunks=RANGE             include only hunks in the given range; hunks are
     --hunk-range=RANGE          numbered from 1; RANGE is a comma-separated list
     --no-hunk-range             of FIRST-LAST; either FIRST or LAST may be ommitted
  -s|--[no-]clear-timestamps   remove file timestamps from output
  -t|--[no-]clear-text         remove non-diff lines from output
  -u|--clear-suffix=REGEX      when given `-p|--clear-prefix=STR', remove from
     --no-clear-suffix           the diff file names that contain the substring
                                 STR the substring that matches the given regex
                                 immediately to the right of the leftmost STR 
  -ve|--[no-]invert=regex      select hunks either non-matching or matching the
                                 regex of `-e|--hunk-regex' (default: matching)
  -vr|--[no-]invert=range      select hunks either outside or inside the range
                                 of `-r|--hunk-range' (default: inside)
  -vw|--[no-]invert=new        include only or exclude new file hunks when given
                                 `-n|--exclude-new-files' (default: exclude)
  -w|--[no-]exclude-new-files  exclude new files from output (those files having
                                 the first hunk address of form '@@ -0,0 ...')
  -x|--exclude-files=GLOB      exclude from output files matching given globbing
     --no-exclude-files          pattern
  -y|--[no-]yielding-parse     parse input by yielding forward each diff entry to
                                 be processed as early as possible; otherwise,
                                 input is processed only after parsing completion
                                 (default); note that `-y|--yielding-parse' makes
                                 this script be much more responsive when is used
                                 interactively
     --dump-opts               print options and exit
     --version                 print version numbers and exit
  -?|--help                    display this help info and exit
"""

        GLOBBING = (
            'source', # Unified.NAME_SOURCE
            'target', # Unified.NAME_TARGET
            'suffix', # Unified.NAME_SUFFIX
        )

        REGEX_APPLY = (
            'all',
            'del',
            'add',
            'equ',
        )

        class bits:

            help = False
            version = False
            dump = False

        def usage():
            cout(help, program)

        def version():
            cout("%s: version %s\n\n%s\n",
                    program, verdate, license)

        def dump():
            cout("""\
action:         %s
apply-regex:    %s
input-file:     %s
include-files:  %s
exclude-files:  %s
exclude-new:    %s
hunk-numbers:   %s
hunk-regex:     %s
hunk-range:     %s
clear-prefix:   %s
clear-suffix:   %s
clear-text:     %s
clear-tstamps:  %s
check-tstamps:  %s
invert-regex:   %s
invert-range:   %s
invert-new:     %s
max-lines:      %s
line-buffered:  %s
line-numbers:   %s
name-globbing:  %s
yielding-parse: %s
fixup:          %s
""",
            self.action.__name__.replace('_', '-'),
            REGEX_APPLY[
            self.apply_regex],
            self.input_file or '<stdin>',
            self.include_files,
            self.exclude_files,
            self.exclude_new,
            self.hunk_numbers,
            self.hunk_regex,
            self.hunk_range,
            self.clear_prefix,
            self.clear_suffix,
            self.clear_text,
            self.clear_tstamps,
            self.check_tstamps,
            self.invert_regex,
            self.invert_range,
            self.invert_new,
            self.max_lines,
            self.line_buffered,
            self.line_numbers,
            GLOBBING[
            self.name_globbing],
            self.yielding_parse,
            self.fixup or '-')

        def invalid_arg(opt, arg, *extra):
            assert isinstance(arg, str)
            if len(extra):
                arg = arg % extra
            else:
                arg = "'%s'" % arg
            error("invalid argument for '%s' option: %s", \
                opt, arg)

        def parse_int(opt, arg):
            try:
                r = int(arg)
            except:
                invalid_arg(opt, arg)
            if r < 0:
                invalid_arg(opt, arg)
            return r

        def parse_file_name(opt, arg):
            if arg != '-' and not os.access(arg, os.F_OK):
                if opt is not None:
                    invalid_arg(opt, "file '%s' not found", arg)
                else:
                    error("input file '%s' not found", arg)
            if arg == '-':
                return None
            else:
                return arg

        REGEX_APPLY2 = '@-+='

        def parse_apply(opt, arg):
            if opt == '-a' and len(arg) == 1:
                v = REGEX_APPLY2
            else:
                v = REGEX_APPLY
            try:
                return v.index(arg)
            except ValueError:
                invalid_arg(opt, arg)

        def parse_regex(opt, arg):
            if not len(arg):
                invalid_arg(opt, "%s", "empty expression")
            try:
                return re.compile(arg)
            except re.error, m:
                invalid_arg(opt, "'%s': %s", arg, m)

        RANGE = Rex(r'^([0-9]*)-?([0-9]*)$')

        def parse_range(opt, arg):
            r = []
            for a in arg.split(','):
                if not RANGE.match(a):
                    invalid_arg(opt, arg)
                f, l = RANGE[1] or '', \
                       RANGE[2] or ''
                m, n = len(f), len(l)
                if not m and not n:
                    invalid_arg(opt, arg)
                if m:
                    f = int(f)
                else:
                    f = 0
                if n:
                    l = int(l)
                else:
                    l = 0
                if a.find('-') < 0:
                    l = f
                if m and not f or \
                    n and not l or \
                    m and n and f > l:
                    invalid_arg(opt, arg)
                r.append(Range(f, l))
            return r

        INVERT0 = ('regex', 'range', 'new')
        INVERT1 = 'erw'

        def parse_invert(opt, arg):
            if arg in INVERT0:
                return arg
            if opt == '-v' and len(arg) == 1:
                i = INVERT1.find(arg)
                if i >= 0:
                    return INVERT0[i]
            invalid_arg(opt, arg)

        def set_invert(which, val):
            setattr(self, 'invert_%s' % which, val)

        def parse_name_globbing(opt, arg):
            try:
                return GLOBBING.index(arg)
            except ValueError:
                invalid_arg(opt, arg)

        def parse_fixup_file(arg):
            p = FixupParser(arg)
            return p.parse()

        from getopt import gnu_getopt, GetoptError
        try:
            opts, args = gnu_getopt(args,
                '?F:OPS' 'a:de:f:g:hi:l:mnp:r:stu:v:wx:y', (

                'apply-regex=',
                'clear-prefix=',
                'no-clear-prefix',
                'clear-suffix=',
                'no-clear-suffix',
                'clear-text',
                'no-clear-text',
                'check-timestamps',
                'no-check-timestamps',
                'clear-timestamps',
                'no-clear-timestamps',
                'exclude-files=',
                'no-exclude-files',
                'exclude-new-files',
                'no-exclude-new-files',
                'fixup-diff=',
                'hunk-range=',
                'no-hunk-range',
                'hunk-regex=',
                'no-hunk-regex',
                'hunk-numbers',
                'no-hunk-numbers',
                'hunks=',
                'include-files=',
                'no-include-files',
                'input-file=',
                'invert=',
                'no-invert=',
                'line-buffered',
                'no-line-buffered',
                'line-numbers',
                'no-line-numbers',
                'max-lines=',
                'no-max-lines',
                'name-globbing=',
                'parse-only',
                'print-diff',
                'print-stats',
                'yielding-parse',
                'no-yielding-parse',

                'dump-opts',
                'version',
                'help'))
        except GetoptError, msg:
            error(msg)

        for opt, arg in opts:
            if opt in ('-O', '--parse-only'):
                self.action = Act.parse
            elif opt in ('-P', '--print-diff'):
                self.action = Act.print_diff
            elif opt in ('-S', '--print-stats'):
                self.action = Act.print_stats
            elif opt in ('-F', '--fixup-diff'):
                self.action = Act.fixup_diff
                self.fixup = arg
            elif opt in ('-a', '--apply-regex'):
                self.apply_regex = parse_apply(opt, arg)
            elif opt in ('-d', '--line-buffered'):
                self.line_buffered = True
            elif opt == '--no-line-buffered':
                self.line_buffered = False
            elif opt in ('-e', '--hunk-regex'):
                self.hunk_regex = arg
            elif opt == '--no-hunk-regex':
                self.hunk_regex = None
            elif opt in ('-f', '--input-file'):
                self.input_file = \
                    parse_file_name(opt, arg)
            elif opt in ('-g', '--name-globbing'):
                self.name_globbing = \
                    parse_name_globbing(opt, arg)
            elif opt in ('-h', '--hunk-numbers'):
                self.hunk_numbers = True
            elif opt == '--no-hunk-numbers':
                self.hunk_numbers = False
            elif opt in ('-i', '--include-files'):
                self.include_files.append(arg)
            elif opt == '--no-include-files':
                self.include_files = []
            elif opt in ('-l', '--max-lines'):
                self.max_lines = parse_int(opt, arg)
            elif opt == '--no-max-lines':
                self.max_lines = None
            elif opt in ('-m', '--no-check-timestamps'):
                self.check_tstamps = False
            elif opt == '--check-timestamps':
                self.check_tstamps = True
            elif opt in ('-n', '--line-numbers'):
                self.line_numbers = True
            elif opt == '--no-line-numbers':
                self.line_numbers = False
            elif opt in ('-p', '--clear-prefix'):
                self.clear_prefix = arg
            elif opt == '--no-clear-prefix':
                self.clear_prefix = None
            elif opt in ('-r', '--hunks', '--hunk-range'):
                self.hunk_range = arg
            elif opt == '--no-hunk-range':
                self.hunk_range = None
            elif opt in ('-s', '--clear-timestamps'):
                self.clear_tstamps = True
            elif opt == '--no-clear-timestamps':
                self.clear_tstamps = False
            elif opt in ('-t', '--clear-text'):
                self.clear_text = True
            elif opt == '--no-clear-text':
                self.clear_text = False
            elif opt in ('-u', '--clear-suffix'):
                self.clear_suffix = arg
            elif opt == '--no-clear-suffix':
                self.clear_suffix = None
            elif opt in ('-v', '--invert'):
                set_invert(parse_invert(opt, arg), True)
            elif opt == '--no-invert':
                set_invert(parse_invert(opt, arg), False)
            elif opt in ('-w', '--exclude-new-files'):
                self.exclude_new = True
            elif opt == '--no-exclude-new-files':
                self.exclude_new = False
            elif opt in ('-x', '--exclude-files'):
                self.exclude_files.append(arg)
            elif opt == '--no-exclude-files':
                self.exclude_files = []
            elif opt in ('-y', '--yielding-parse'):
                self.yielding_parse = True
            elif opt == '--no-yielding-parse':
                self.yielding_parse = False
            elif opt == '--dump-opts':
                bits.dump = True
            elif opt == '--version':
                bits.version = True
            elif opt in ('-?', '--help'):
                bits.help = True
            else:
                assert False

        if len(args):
            self.input_file = parse_file_name(None, args[0])

        if self.action != Act.fixup_diff:
            self.fixup = None

        if bits.version:
            version()
        if bits.dump:
            dump()
        if bits.help:
            usage()
        if bits.version or \
           bits.dump or \
           bits.help:
            sys.exit(0)

        self.apply_regex = \
            (None, '-', '+', ' ') \
            [self.apply_regex]

        if self.line_buffered:
            libc.setlinebuf()

        if self.hunk_regex is not None:
            self.hunk_regex = parse_regex(
                "--hunk-regex", self.hunk_regex)

        if self.hunk_range is not None:
            self.hunk_range = parse_range(
                "--hunk-range", self.hunk_range)

        if self.clear_suffix is not None:
            parse_regex("--clear-suffix",
                self.clear_suffix)

        if self.clear_prefix is not None and \
            self.clear_suffix is not None:
            self.clear_suffix = Rex(
                r"(?<=" +
                re.escape(self.clear_prefix) +
                ")" +
                self.clear_suffix)

        if self.clear_prefix is not None:
            self.clear_prefix = Rex(
                r"^.*?(?=" +
                re.escape(self.clear_prefix) +
                ")")
        else:
            self.clear_suffix = None

        if self.action == Act.fixup_diff:
            self.fixup = parse_fixup_file(self.fixup)

        from signal import signal, SIGPIPE, SIG_DFL
        signal(SIGPIPE, SIG_DFL)

def main():
    opt = Options()
    opt.action(opt)

def call(func, catch = Exception):
    try:
        func()
    except catch:
        pass

if __name__ == '__main__':
    try:
        call(main, KeyboardInterrupt)
    finally:
        call(sys.stdin.close)
        call(sys.stdout.close)


