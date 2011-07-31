c = get_config()

# c.TerminalIPythonApp.display_banner = False
c.TerminalIPythonApp.nosep = True
c.TerminalIPythonApp.exec_files = ['ipyvenv.py']
c.TerminalInteractiveShell.prompt_in1 = '\\C_Green>>\\C_LightGreen> '
c.TerminalInteractiveShell.prompt_in2 = '\\C_Green..\\C_LightGreen. '
c.TerminalInteractiveShell.prompt_out = ''
c.TerminalInteractiveShell.separate_in = ''
c.TerminalInteractiveShell.separate_out1 = ''
c.TerminalInteractiveShell.separate_out2 = ''

c.TerminalInteractiveShell.deep_reload = True
c.TerminalInteractiveShell.autocall = 1
c.TerminalInteractiveShell.confirm_exit = False
c.TerminalInteractiveShell.automagic = False

import sys, IPython
c.TerminalInteractiveShell.banner1 = "Py %s IPy %s\n" % (
    sys.version.split('\n', 1)[0], IPython.__version__)
