import argparse

def cmd_summary(args):
    print("you have called summary")
    print(args)

def cmd_validate(args):
    print("you have called validate")
    print(args)


parser = argparse.ArgumentParser(description="poseidon command line tool")
parser.set_defaults(cmd_func=None)
parser.add_argument('-d', '--dir', action='append', required=True)
subparsers = parser.add_subparsers()

parser_summary = subparsers.add_parser('summary')
parser_summary.set_defaults(cmd_func=cmd_summary)

parser_validate = subparsers.add_parser('validate')
parser_validate.set_defaults(cmd_func=cmd_validate)

def runner():
    parsed = parser.parse_args()
    if parsed.cmd_func is None:
        parser.print_help()
        return
    parsed.cmd_func(parsed)
