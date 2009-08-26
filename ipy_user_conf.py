""" User configuration file for IPython
(c) Alexander Solovyov 2007-2008

This config has input prompts like in usual python shell to avoid
confusion and simplification of doctest creation.

See http://ipython.scipy.org/moin/IpythonExtensionApi for detailed
description on what you could do here.
"""

import IPython.ipapi
ip = IPython.ipapi.get()

def main():
    import ipy_stock_completers

    o = ip.options
    o.autocall = 0

    # My prompt, imitating usual python
    o.prompt_in1 = '\\C_Green>>\\C_LightGreen> '
    o.prompt_in2 = '\\C_Green..\\C_LightGreen. '
    o.prompt_out = ''

    # Remove all blank lines in between prompts
    o.separate_in="0"
    o.separate_out="0"
    o.separate_out2="0"

    # I like to keep my banner minimal.
    from IPython import Release
    import sys
    o.banner = "Py %s IPy %s\n" % (sys.version.split('\n', 1)[0], Release.version)

    o.confirm_exit = 0
    try:
        ip.ex("from ipy_addons import *")
    except ImportError:
        pass
    try:
        ip.ex("from ipipe import *")
    except ImportError:
        pass

main()
