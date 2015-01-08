from docker.client import Client

cli = Client()
cs = cli.containers()

print '(',
for c in cs:
    if len(c['Names']) > 1:
        c['Names'] = filter(lambda x: len(x.split('/'))==2, c['Names'])
    print('("{Id}" ["{Names[0]}" "{Status}" "{Image}"])'.format(**c)),

print ')'
