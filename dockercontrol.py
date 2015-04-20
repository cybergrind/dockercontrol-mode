from docker.client import Client

cli = Client()
cs = cli.containers(all=True)

print '(',
for c in cs:
    if len(c['Names']) > 1:
        c['Names'] = filter(lambda x: len(x.split('/')) == 2, c['Names'])
    s = c['Status']
    if s.endswith('Paused)'):
        c['Status'] = 'Paused'
    elif s.startswith('Up'):
        c['Status'] = 'Running'
    elif s.startswith('Exited'):
        c['Status'] = 'Stopped'
    print('("{Id}" ["{Names[0]}" "{Status}" "{Image}"])'.format(**c)),

print ')'
