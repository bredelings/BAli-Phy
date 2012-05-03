import gdb
import gdb.printing

class expression_ref_printer:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        x = self.val['px']
# Apparently we (Object *) <number> doesn't work, but (reg_var* ) <number> does??
#        return gdb.parse_and_eval('((Object*) %s)->print()' % x)
        return "expression_ref: " + ('%s' % x) # + " " +x.type 

def expression_ref_lookup_function(val):
    if val.type.tag == 'expression_ref':
        return expression_ref_printer(val)
    return None

def register_bali_phy_printers(obj):
    if obj is None:
        obj = gdb
#    obj.pretty_printers.append(expression_ref_lookup_function)

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
