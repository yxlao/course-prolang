import numpy as np

num_graph = 200

for i in xrange(num_graph):
    depth = np.random.randint(9) + 6
    seeds = np.random.randint(200, size=2)
    print "let g%s () = (%s, %s, %s)" % (i, depth, seeds[0], seeds[1])
    
for i in xrange(num_graph):
    depth = np.random.randint(9) + 6
    seeds = np.random.randint(200, size=2)
    print "let c%s () = (%s, %s, %s)" % (i, depth, seeds[0], seeds[1])

for i in xrange(num_graph):
    print '(fun () -> mkTest (fun () -> doRandomGray (g%s ())) () () "g%s");' % (i, i)
    
for i in xrange(num_graph):
    print '(fun () -> mkTest (fun () -> doRandomColor (c%s ())) () () "c%s");' % (i, i)

