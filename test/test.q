(:P {nom string, age int})
(:E {nom string, pme bool})

(:P) -[:ami]-> (:P)
(:P) -[:emp]-> (:E)

create 
    (a:P) -[:ami]-> (b:P) -[:ami]-> (c:P) -[:ami]-> (a), 
    (a) -[:emp]-> (e:E),
    (b) -[:emp]-> (e),
    (c) -[:emp]-> (e)

set 
    a.nom = "Albert", a.age = 47,
    b.nom = "Benoit", b.age = 24,
    c.nom = "Corentin", c.age = 8,
    e.nom = "Apple", e.pme = false,
    





    