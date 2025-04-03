  $ dune exec Proj_GraphDB f tiny.q
  (Lang.Prog (
   (Graphstruct.DBG (
      [(Graphstruct.DBN ("A", [])); (Graphstruct.DBN ("B", []))], [])),
   (Lang.Query
      [(Lang.Create [(Lang.SimpPattern (Lang.DeclPattern ("a", "A")))]);
        (Lang.Create [(Lang.SimpPattern (Lang.DeclPattern ("b", "B")))])])
   ))