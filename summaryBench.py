def readResult(fn):
    with open(fn) as f:
        txt = f.readlines()
    txt = [ln.split('|')
           for ln in txt if '|' in ln and ln[0] != 'P']
    txt = [[float(col) if idx > 0 else col
            for idx, col in enumerate(ln) if idx < 3]
           for ln in txt]
    txt = [txt[0:5], txt[5:10], txt[10:15]]  # F, I, E
    return txt

fnames = [f'bench-ts/bench-DOM{n}/results.txt'
          for n in [17, 25, 33]]

results = [readResult(fn) for fn in fnames]
results = [results[a][b]  # 17E, 17I, 17F, 25F, 33F
           for a, b in [(0,2), (0,1), (0,0), (1,0), (2,0)]]

maxStd = max(ln[2]/ln[1] if ln[1] else 0
             for cs in results for ln in cs)
print(f'std/avg <= {maxStd * 100:.2f}%')

orig = results[2][4][1]
print(f'17F (bar3) total = {orig:.2f}ms as 100%')
orig /= 100
for idx, bar in enumerate(results, 1):
    bar = [f'{ln[1] / orig:.2f}%' for ln in bar]
    print('bar =', idx,
          'typer =', bar[0],
          'cc =', bar[1],
          'eff =', bar[2],
          'other =', bar[3],
          'total =', bar[4], sep='\t')

