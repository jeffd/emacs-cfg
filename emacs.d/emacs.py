"""Definitions used by commands sent to inferior Python in python.el."""

# Copyright (C) 2004, 2005, 2006, 2007  Free Software Foundation, Inc.
# Author: Dave Love <fx@gnu.org>

# This file is part of GNU Emacs.

# GNU Emacs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

# $Revision: 1.12 $

import os, sys, traceback, inspect
from sets import Set

__all__ = ["eexecfile", "eargs", "complete", "ehelp", "eimport", "modpath"]

def eexecfile (file):
    """Execute FILE and then remove it.
    If we get an exception, print a traceback with the top frame
    (ourselves) excluded."""
    import __main__

    try:
	try: execfile (file, __main__.__dict__)
	except:
	    (etype, value, tb) = sys.exc_info ()
	    # Lose the stack frame for this location.
	    tb = tb.tb_next
	    if tb is None:	# print_exception won't do it
		print "Traceback (most recent call last):"
	    traceback.print_exception (etype, value, tb)
    finally:
	os.remove (file)

def eargs (name, imports):
    """Get arglist of NAME for Eldoc &c.
    Exec IMPORTS first."""
    try:
	try:			# don't give up if the imports fail
	    if imports: exec imports
	    parts = name.split ('.')
	    if len (parts) > 1:
		exec 'import ' + parts[0]
	except: pass
	func = eval (name)
	if inspect.isbuiltin (func) or inspect.isclass (func):
	    doc = func.__doc__
	    if doc.find (' ->') != -1:
		print '_emacs_out', doc.split (' ->')[0]
	    elif doc.find ('\n') != -1:
		print '_emacs_out', doc.split ('\n')[0]
	    return
	if inspect.ismethod (func):
	    func = func.im_func
	if not inspect.isfunction (func): return
	(args, varargs, varkw, defaults) = inspect.getargspec (func)
	# No space between name and arglist for consistency with builtins.
	print '_emacs_out', \
	    func.__name__ + inspect.formatargspec (args, varargs, varkw,
						   defaults)
    except: pass

def all_names (object):
    """Return (an approximation to) a list of all possible attribute
    names reachable via the attributes of OBJECT, i.e. roughly the
    leaves of the dictionary tree under it."""

    def do_object (object, names):
	if inspect.ismodule (object):
	    do_module (object, names)
	elif inspect.isclass (object):
	    do_class (object, names)
	# Might have an object without its class in scope.
	elif hasattr (object, '__class__'):
	    names.add ('__class__')
	    do_class (object.__class__, names)
	# Probably not a good idea to try to enumerate arbitrary
	# dictionaries...
	return names

    def do_module (module, names):
	if hasattr (module, '__all__'):	# limited export list
	    names.union_update (module.__all__)
	    for i in module.__all__:
		do_object (getattr (module, i), names)
	else:			# use all names
	    names.union_update (dir (module))
	    for i in dir (module):
		do_object (getattr (module, i), names)
	return names

    def do_class (object, names):
	names.union_update (dir (object))
	if hasattr (object, '__bases__'): # superclasses
	    for i in object.__bases__: do_object (i, names)
	return names

    return do_object (object, Set ([]))

def complete (name, imports):
    """Complete TEXT in NAMESPACE and print a Lisp list of completions.
    Exec IMPORTS first."""
    import __main__, keyword

    def class_members (object):
	names = dir (object)
	if hasattr (object, '__bases__'):
	    for superc in object.__bases__:
		names = class_members (superc)
	return names	

    names = Set ([])
    base = None
    try:
	dic = __main__.__dict__.copy()
	if imports:
	    try: exec imports in dic
	    except: pass
	l = len (name)
	dot = name.rfind ('.')
	if dot == -1:
	    for elts in [dir (__builtins__), keyword.kwlist, dic.keys()]:
		for elt in elts:
		    if elt[:l] == name: names.add (elt)
	else:
	    base = name[:dot]
	    name = name[dot+1:]
	    try:
		obj = eval (base, dic)
		names = Set (dir (obj))
		if hasattr (obj, '__class__'):
		    names.add ('__class__')
		    names.union_update (class_members (obj))
	    except: names = all_names (dic)
    except: return []
    l = len (name)
    print '_emacs_out (',
    for n in names:
	if name == n[:l]:
	    if base: print '"%s.%s"' % (base, n),
	    else: print '"%s"' % n,
    print ')'

# Fixme:  This could try to look up methods/attributes applied to
# variables by generating possibilities like the completion code.  The
# trouble is that could be misleading if it gets the wrong one.
def ehelp (name, imports):
    """Get help on string NAME.
    First try to eval name for, e.g. user definitions where we need
    the object.  Otherwise try the string form.
    Exec IMPORTS first."""
    locls = {}
    if imports:
	try: exec imports in locls
	except: pass
    try: help (eval (name, globals (), locls))
    except: help (name)

def eimport (mod, dir):
    """Import module MOD with directory DIR at the head of the search path.
    NB doesn't load from DIR if MOD shadows a system module."""
    from __main__ import __dict__

    path0 = sys.path[0]
    sys.path[0] = dir
    try:
	try:
	    if __dict__.has_key (mod) and inspect.ismodule (__dict__[mod]):
		reload (__dict__[mod])
	    else:
		__dict__[mod] = __import__ (mod)
	except:
	    (etype, value, tb) = sys.exc_info ()
	    print "Traceback (most recent call last):"
	    traceback.print_exception (etype, value, tb.tb_next)
    finally:
	sys.path[0] = path0

def modpath (module):
    """Get the source file for the given MODULE (or nil)."""
    locls = {}
    try:
	exec "import " + module
	print "_emacs_out", \
	    inspect.getsourcefile (eval (module, globals (), locls))
    except:
	print "_emacs_out ()"

def location_of (name, imports):
    """Get location at which NAME is defined (or nil).
    Returns a pair (PATH, LINE), where LINE is the start of the definition
    in path name PATH.
    Exec IMPORTS first."""
    locls = {}
    if imports:
	try: exec imports in locls
	except: pass
    try:
	obj = eval (name, globals (), locls)
	# Bug: (in Python 2.5) `getsourcefile' only works with modules,
	# hence the `getmodule' here.
	srcfile = inspect.getsourcefile (inspect.getmodule (obj))
	_, line = inspect.getsourcelines (obj)
	print '_emacs_out ("%s" . %d)' % (srcfile, line)
    except:
	print "_emacs_out ()"

# print '_emacs_ok'		# ready for input and can call continuation

# arch-tag: d90408f3-90e2-4de4-99c2-6eb9c7b9ca46
