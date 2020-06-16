import os
import re
import shutil
import sys


rep = re.compile("\((Grounded[a-zA-Z]+)\s*?\"\s*?(scm):\s*(.*?)\"\s*?\)")

def replace(path, target_path):
    os.makedirs(os.path.dirname(target_path), exist_ok=True)
    s = open(path, 'rt').read()
    new_s = ''
    prev_end = 0
    python_module = 'pln'

    pln_found = False
    for item in path.split('pln/opencog')[1].split('/')[1:-1]:
        if (not pln_found) and (item == 'pln'):
            pln_found = True
            continue
        if pln_found:
            python_module += '.' + item
    python_module += '.'
    for match in re.finditer(rep, s):
        formula = match.group(3)
        if 'GroundedPredicate' in match.group(1):
            res = '(GroundedPredicate "py:{0}")'
        else:
            res = '(GroundedSchema "py:{0}")'
        new_s += s[prev_end: match.start()]
        new_s += res.format(python_module + formula.replace('-', '_'))
        prev_end = match.end()
    new_s += s[prev_end:]
    with open(target_path, 'wt') as f:
        f.write(new_s)


def convert_pln_rules(pln_dir):
    if os.path.exists('/tmp/pln'):
        shutil.rmtree('/tmp/pln')
    for root, dirs, files in os.walk(pln_dir):
        for file in files:
            if file.endswith('scm'):
                path = os.path.join(root, file)
                print(path)
                subpath = root.split('opencog/pln')[1]
                replace(path, os.path.join('/tmp/pln/' + subpath, file))


if __name__ == '__main__':
    convert_pln_rules(os.path.abspath(os.path.expanduser(sys.argv[1])))
