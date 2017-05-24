import site
import sys
from os import environ
from os.path import join

import IPython
from IPython.terminal.prompts import ClassicPrompts


c = get_config()


c.TerminalInteractiveShell.prompts_class = ClassicPrompts
c.TerminalInteractiveShell.deep_reload = True
c.TerminalInteractiveShell.autocall = 1
c.TerminalInteractiveShell.confirm_exit = False
c.TerminalInteractiveShell.automagic = False

c.InteractiveShellApp.extensions = ['autoreload']
c.InteractiveShellApp.exec_lines = ['%autoreload 1']

c.TerminalInteractiveShell.banner1 = "Py %s IPy %s\n" % (
    sys.version.split('\n', 1)[0], IPython.__version__)


# virtualenv support
if 'VIRTUAL_ENV' in environ:
    virtual_env = join(environ.get('VIRTUAL_ENV'),
                       'lib',
                       'python%d.%d' % sys.version_info[:2],
                       'site-packages')

    # Remember original sys.path.
    prev_sys_path = list(sys.path)
    site.addsitedir(virtual_env)

    # Reorder sys.path so new directories at the front.
    new_sys_path = []
    for item in list(sys.path):
        if item not in prev_sys_path:
            new_sys_path.append(item)
            sys.path.remove(item)
    sys.path[1:1] = new_sys_path

    c.TerminalInteractiveShell.banner1 += 'virtualenv -> %s\n' % virtual_env
