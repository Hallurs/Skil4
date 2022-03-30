// T-501-FMAL, Spring 2022, Assignment 4

module Assignment4Tests

open Assignment4

////////////////////////////////////////////////////////////////////////
// Tests for Problem 3                                                //
////////////////////////////////////////////////////////////////////////

let testMirror (t : int tree) =
  let tref = makeRefTree t
  mirror tref;
  freeze tref

// > testMirror Lf;;
// val it: int tree = Lf
// > testMirror (Br (1, Lf, Lf));;
// val it: int tree = Br (1, Lf, Lf)
// > testMirror (Br (1, Br (2, Lf, Lf), Lf));;
// val it: int tree = Br (1, Lf, Br (2, Lf, Lf))
// > testMirror (Br (1, Br (2, Br (3, Lf, Lf), Lf), Lf));;
// val it: int tree = Br (1, Lf, Br (2, Lf, Br (3, Lf, Lf)))
// > testMirror (Br (1, Br (2, Br (3, Lf, Lf), Br (4, Lf, Lf)), Lf));;
// val it: int tree = Br (1, Lf, Br (2, Br (4, Lf, Lf), Br (3, Lf, Lf)))
// > testMirror (Br (1, Br (2, Br (3, Lf, Lf), Br (4, Lf, Lf)), Br (5, Lf, Lf)));;
// val it: int tree = Br (1, Br (5, Lf, Lf), Br (2, Br (4, Lf, Lf), Br (3, Lf, Lf)))
// > testMirror (Br (1, Br (2, Lf, Lf), Br (5, Lf, Lf)));;
// val it: int tree = Br (1, Br (5, Lf, Lf), Br (2, Lf, Lf))

let testRotate (t : int tree) =
  let tref = makeRefTree t
  rotate tref;
  freeze tref

// > testRotate Lf;;
// val it: int tree = Lf
// > testRotate (Br (1, Lf, Lf));;
// val it: int tree = Br (1, Lf, Lf)
// > testRotate (Br (1, Br (2, Lf, Lf), Lf));;
// val it: int tree = Br (2, Lf, Br (1, Lf, Lf))
// > testRotate (Br (1, Br (2, Br (3, Lf, Lf), Lf), Lf));;
// val it: int tree = Br (2, Br (3, Lf, Lf), Br (1, Lf, Lf))
// > testRotate (Br (1, Br (2, Br (3, Lf, Lf), Br (4, Lf, Lf)), Lf));;
// val it: int tree = Br (2, Br (3, Lf, Lf), Br (1, Br (4, Lf, Lf), Lf))
// > testRotate (Br (1, Br (2, Br (3, Lf, Lf), Br (4, Lf, Lf)), Br (5, Lf, Lf)));;
// val it: int tree = Br (2, Br (3, Lf, Lf), Br (1, Br (4, Lf, Lf), Br (5, Lf, Lf)))
// > testRotate (Br (1, Br (2, Lf, Lf), Br (5, Lf, Lf)));;
// val it: int tree = Br (2, Lf, Br (1, Lf, Br (5, Lf, Lf)))


////////////////////////////////////////////////////////////////////////
// Tests for Problem 4                                                //
////////////////////////////////////////////////////////////////////////

// > run (Block ("x", [Assign ("x", Num 1); Block ("x", [Print (Var "x")])]));;
// 0
// val it: unit = ()

// > run (Block ("x", [Print (Var "x"); Assign ("x", Num 1); Block ("y", [Print (Var "x")])]));;
// 0
// 1
// val it: unit = ()

// > run (Block ("y", [Assign ("x", Num 5); Assign ("y", Num 6); Block ("y", [Print (Var "x"); Print (Var "y")])]));;
// 5
// 0
// val it: unit = ()

// > run (Block ("x", [Assign ("x", Num 1); Assign ("y", Num 2); Print (Var "y"); Block ("y", [Print (Var "y")]); Print (Var "y")]));;
// 2
// 0
// 2
// val it: unit = ()

// > run (Block ("x", [Assign ("x", Num 10); Block ("x", [Assign ("x", Num 20); Block ("x", [Assign ("x", Num 30); Print (Var "x")]); Print (Var "x")]); Print (Var "x")]));;
// 30
// 20
// 10
// val it: unit = ()

// > run (Block ("x", [Assign ("x", Num 10); Block ("x", [Assign ("x", Num 20); Block ("x", [Assign ("x", Num 30); Assign ("y", Plus (Var "y", Var "x"))]); Assign ("y", Plus (Var "y", Var "x"))]); Assign ("y", Plus (Var "y", Var "x")); Print (Var "y")]));;
// 60
// val it: unit = ()

