import gdb
import gdb.printing

class expression_ref_printer:
    def __init__(self, val):
        self.val = val

    def to_string(self):
        x = self.val['px']
#        y = gdb.parse_and_eval('Object::print(%s)' % x)
        return "expression_ref: " + ('%s' % x) # + " " +x.type 

def expression_ref_lookup_function(val):
    if val.type.tag == 'expression_ref':
        return expression_ref_printer(val)
    return None

def register_bali_phy_printers(obj):
    if obj is None:
        obj = gdb
    obj.pretty_printers.append(expression_ref_lookup_function)
