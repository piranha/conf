""" User configuration file for IPython

See http://ipython.scipy.org/moin/IpythonExtensionApi for detailed
description on what you could do here.
"""

import IPython.ipapi
ip = IPython.ipapi.get()

def main():
    import ipy_stock_completers

    o = ip.options
    o.autocall = 0

    # my prompt
    o.prompt_in1 = '\\C_Green>>\\C_LightGreen> '
    o.prompt_in2 = '\\C_Green..\\C_LightGreen. '
    o.prompt_out = ''

    # Remove all blank lines in between prompts, like a normal shell.
    o.separate_in="0"
    o.separate_out="0"
    o.separate_out2="0"

    # I like my banner minimal.
    from IPython import Release
    import sys
    o.banner = "Py %s IPy %s\n" % (sys.version.split('\n', 1)[0], Release.version)

    o.confirm_exit = 0
    try:
        ip.ex("from ipy_addons import *")
    except ImportError:
        pass

main()