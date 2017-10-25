from subprocess import call
from os import walk, chdir
dir = 'c:/emacs/lisp/addons/yasnippet/'

for p,d,f in walk(dir):
    chdir(p)
    call(['hg', 'add', '*.*'])

print 'done'
