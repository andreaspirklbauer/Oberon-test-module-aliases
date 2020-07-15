--------------------------------------------------------------
TestAliases.Mod (compiler messages shown for ORB06.Mod)
--------------------------------------------------------------

--------------------------- Test suite #1 (no errors) --------------------------------------

ORP.Compile @/s ~

MODULE M;      (*no error message*)
  TYPE T0* = RECORD i: INTEGER END ;
END M.

MODULE M0;      (*no error message*)
  TYPE T0* = RECORD i: INTEGER END ;
END M0.

MODULE M1;      (*no error message*)
  IMPORT M0;
  TYPE T1* = RECORD (M0.T0) j: INTEGER END ;
  VAR a: M0.T0; b: T1;
BEGIN a := b
END M1.

MODULE M2;      (*no error message*)
  IMPORT M1;
  TYPE T2* = RECORD (M1.T1) k: INTEGER END ;  (*re-imports M0.T0*)
  VAR a: M1.T1; b: T2;
BEGIN a := b
END M2.

--------------------------- Test suite #2 (no errors) --------------------------------------

ORP.Compile @/s ~

MODULE M3;                     (*no error message*)
  IMPORT M := M0, M1;
  VAR a: M.T0; b: M1.T1;
BEGIN a := b
END M3.

MODULE M4;                     (*no error message*)
  IMPORT M := M0, M0 := M1;
  VAR a: M.T0;
    b: M0.T1;
BEGIN
  a := b
END M4.

MODULE M5;                     (*no error message*)
  IMPORT Y1 := M0, M1;
  VAR a: Y1.T0; b: M1.T1;
BEGIN a := b
END M5.

MODULE M6;                     (*no error message*)
  IMPORT D := Display, Oberon; (*Oberon re-imports module Display*)
  VAR m1: D.FrameMsg; m2: Oberon.InputMsg;
  PROCEDURE Handle(V: D.Frame; VAR M: D.FrameMsg);
  BEGIN
    CASE M OF
      Oberon.InputMsg: m2 := M |
      D.FrameMsg: m1 := M
    END
  END Handle;
END M6.

--------------------------- Test suite #3 (with errors) ------------------------------------

ORP.Compile @/s ~

MODULE M7;                     (*mult def*)
  IMPORT M := M0, M0;          (*because M4 imports a module M0 under *two* different names M and M0*)
  VAR a: M.T0; b: M0.T0;
BEGIN a := b
END M7.

MODULE M8;                     (*mult def*)
  IMPORT M := M0, M2, M;       (*because the name M is used twice, once as an alias, once as a module name*)
  TYPE T3* = RECORD (M2.T2) k: INTEGER END ;
  VAR a: M2.T2; b: T3; c: M.T0;
BEGIN a := b; c := b
END M8.

MODULE M9;                     (*invalid import order*)
  IMPORT M1, M0;               (*because M1.T1 re-imports M0.T0 *before* M0 is imported directly*)
  VAR a: M0.T0; b: M1.T1;
BEGIN a := b
END M9.

MODULE M10;                    (*conflict with alias*)
  IMPORT M0 := M, M1;          (*because M is imported under alias name M0, then M1 re-imports the type M0.T0*)
  VAR a: M0.T0; b: M1.T1;
BEGIN a := b
END M10.

--------------------------- Test suite #4 (some errors) ------------------------------------

ORP.Compile @/s ~

MODULE B0; IMPORT M0, X := M1; END B0.  (*no error message*)

MODULE B1; IMPORT M, M; END B1.  (*mult def*)
MODULE B2; IMPORT M, M := M0; END B2.  (*mult def*)
MODULE B3; IMPORT M := M0, M; END B3.  (*mult def*)
MODULE B4; IMPORT M := M0, M := M0; END B4.  (*mult def*)
MODULE B5; IMPORT X := M, X := M0; END B5.  (*mult def*)

MODULE B6; IMPORT M, X := M; END B6.  (*mult def*)
MODULE B7; IMPORT X := M, M; END B7.  (*mult def*)
MODULE B8; IMPORT M, X := M, Y := M; END B8.  (*mult def*)
MODULE B9; IMPORT X := M, Y := M; END B9.  (*mult def*)

MODULE B10; IMPORT X := M, M := M0; END B10.  (*no error message*)
MODULE B11; IMPORT M := M0, M0 := M; END B11.  (*no error message*)
MODULE B12; IMPORT M0 := M, X := M0; END B12.  (*no error message*)

MODULE B13; IMPORT M0, M1; END B13.  (*no error message (M0 explicitly imported and re-imported)*)
MODULE B14; IMPORT M := M0, M1; END B14.  (*no error message (M0 explicitly imported and re-imported)*)

MODULE B15; IMPORT M0 := M, M1; END B15.  (*conflict with alias*)
MODULE B16; IMPORT M1, M0 := M; END B16.  (*invalid import order*)
MODULE B17; IMPORT M1, M0; END B17.  (*invalid import order*)
