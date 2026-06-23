import gdb
import gdb.printing

def register_bali_phy_printers(obj):
    if obj is None:
        obj = gdb

# While I got the infrastructure working, there doesn't seem to be
# an easy way to call the E.px->print() from python.
#
# One suggestion would have bee something like:
#     gdb.parse_and_eval('Object::print(%s)' % x) to call x->print().
#
# From inside gdb, one can run:
#    python print gdb.parse_and_eval('E.px->print()')
# and get
#    "49".
# Apparently this uses the pretty-printer for strings.
