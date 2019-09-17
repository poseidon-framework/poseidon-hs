import argparse
from poseidon.data_module import findPoseidonModulesFiles, loadModules, PoseidonModule

def load_all_modules(args) -> [PoseidonModule]:
    modules = []
    for d in args.dir:
        f = findPoseidonModulesFiles(d)
        m = loadModules(f)
        modules.extend(m)
    for f in args.file:
        m = PoseidonModule(f)
        modules.append(m)
    return modules

def cmd_list(args):

    print("you have called summary")
    print(args)

def cmd_export(args):
    print("you have called validate")
    print(args)


parser = argparse.ArgumentParser(description="poseidon command line tool")
parser.set_defaults(cmd_func=None)
parser.add_argument('-d', '--dir', action='append', required=True)
parser.add_argument('-f', '--file', action='append', required=True)
subparsers = parser.add_subparsers()

parser_list = subparsers.add_parser('list')
parser_list.set_defaults(cmd_func=cmd_list)
parser_list.add_argument()

parser_export = subparsers.add_parser('export')
parser_export.set_defaults(cmd_func=cmd_export)

def runner():
    parsed = parser.parse_args()
    if parsed.cmd_func is None:
        parser.print_help()
        return
    parsed.cmd_func(parsed)
