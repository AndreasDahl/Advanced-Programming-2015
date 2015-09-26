:- consult(set).

testData([a,b,c,d,e,f,x,y,z]).

testSet :-
    testData(Z),

    setSubset([], [a,b,c,d]),
    setSubset([c], [a,b,c,d]),
    setSubset([a,b,c,d], [d,c,a,b]),

    setUnion(Z, Z, Z),
    setUnion([a,b,c,x,y,z], [d,e,f,x,y,z], A),
    setSubset(A, [a,b,c,d,e,f,x,y,z]),

    setIntersection(Z, Z, Z),
    setIntersection([a,b,c,x,y,z], [d,e,f,x,y,z], B),
    setSubset(B, [x,y,z]),

    setComplement(Z, Z, []),
    setComplement(Z, [], Z),
    setComplement([], Z, []),
    setComplement([a,b,c,x,y,z], [d,e,f,x,y,z], C),
    setSubset(C, [a,b,c]),

    % symmetric difference
    setComplement([d,e,f,x,y,z], [a,b,c,x,y,z], D),
    setComplement([a,b,c,x,y,z], [d,e,f,x,y,z], E),
    setUnion(D, E, F),

    setUnion([d,e,f,x,y,z], [a,b,c,x,y,z], G),
    setIntersection([d,e,f,x,y,z], [a,b,c,x,y,z], H),
    setComplement(G, H, I),

    setSubset(F, I),
    setSubset(I, F).
