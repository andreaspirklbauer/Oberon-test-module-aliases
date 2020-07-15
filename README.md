# Oberon-test-module-aliases
Test various ways to import modules with aliases on the Project Oberon 2013 system (www.projectoberon.com).

Load the source files from the [**Sources**](Sources/) directory of this repository onto your Oberon system and compile the modules contained in the source file [**TestAliases.Mod**](Sources/TestAliases.Mod) under the various versions of ORB shown below.

See also:
* [**Comparison with the "oberonc" compiler**](http://github.com/andreaspirklbauer/Oberon-test-module-aliases/issues/1)
* [**Proof of correctness of ORB.ThisModule**](http://github.com/andreaspirklbauer/Oberon-test-module-aliases/blob/master/CaseAnalysis.md)

-------------------------------------------------------------------------------------

**RECOMMENDATION**

Of the solutions provided below, only the following can be recommended ("lines" means *sloc* = **s**ource **l**ines **o**f **c**ode)

**1)** [**ORB06.Mod**](Sources/ORB06.Mod) (one-pass solution with restrictions, e.g. multiple aliases to the same module are not allowed)

* More restrictive than other solutions (for example, the explicit import of a module M must come before re-imports of M, there can be at most one alias name per imported module, test modules M7 and M9 don't compile)
* But allows reusing module names and aliases in some cases, e.g.,

      MODULE B10; IMPORT X := M, M := M0; END B10.  (*reuse the canonical name M of a module as an alias for another module M0, if M has itself been imported under an alias X*)
      MODULE B11; IMPORT M := M0, M0 := M; END B11.  (*same as B10, but used to effectively "swap" (the names of) two modules M and M0*)
      MODULE B12; IMPORT M0 := M, X := M0; END B12.  (*reuse the alias name M0 of another module as the canonical name of a module which is itself imported under an alias X*)

* Does not detect cyclic module imports at compile time (these will cause the loader to enter an endless recursion)
* For a detailed case analysis, see [**CaseAnalysis**](http://github.com/andreaspirklbauer/Oberon-test-module-aliases/blob/master/CaseAnalysis.md)

**2)** [**ORB07.Mod**](Sources/ORB07.Mod) (one-pass solution with more restrictions, e.g. multiple aliases to the same module are not allowed)

* -5 lines relative to ORB06.Mod
* Even more restrictive than ORB06.Mod (for example, test modules B10, B11, B12 don't compile)
* Does not detect cyclic module imports at compile time (these will cause the loader to enter an endless recursion)

**3)** [**ORB10.Mod**](Sources/ORB10.Mod)  (one-pass solution with no restrictions except restrictions on import order)

* +15 lines relative to ORB06.Mod
* Allows multiple aliases to refer to the same module (now the test modules B6-B13 compile)
* This removes all restrictions *except* the restriction on *import order* (explicit imports must come *before* re-imports)
* Does not detect cyclic module imports at compile time (these will cause the loader to enter an endless recursion)

**4)** [**ORB11.Mod**](Sources/ORB11.Mod)  (one-pass solution with no restrictions on import order)

* +15 lines relative to ORB06.Mod
* Eliminates the *import order* restriction by stitching together implicit imports (re-exported types) and later explicit ones
* Does not detect cyclic module imports at compile time (these will cause the loader to enter an endless recursion)

-------------------------------------------------------------------------------------

**ORB00.Mod** (only tests *obj.name* against *name*)

*This is a one-pass solution with restrictions on import order.*

    obj1 := topScope; obj := obj1.next;  (*search for module*)
    WHILE (obj # NIL) & (obj.name # name) DO obj1 := obj; obj := obj1.next END ;
    IF obj = NIL THEN  (*insert new module*)
      ...
    ELSE (*module already present*)
      IF decl THEN ORS.Mark("invalid import order") END
    END ;

This variant does not handle module aliases correctly in all cases (for example, modules A3 and A3 in the source file [**TestAliases.Mod**](Sources/TestAliases.Mod) don't compile correctly). All other versions of ORB provided in this repository do compile A3 correctly.

------------------------------------------------------------------------

**ORB01.Mod** (tests *obj.name* against *name* and *obj.orgname* against *orgname* separately)

    obj1 := topScope; obj := obj1.next;  (*search for module*)
    WHILE (obj # NIL) & (obj.name # name) DO obj1 := obj; obj := obj1.next END ;
    IF obj = NIL THEN 
      obj1 := topScope; obj := obj1.next;
      WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO
        obj1 := obj; obj := obj1.next
      END
    END ;
    IF obj = NIL THEN  (*insert new module*)
      ...
    ELSE (*module already present*)
      IF decl THEN ORS.Mark("invalid import order") END
    END ;

------------------------------------------------------------------------

**ORB02.Mod** (tests *obj.name* against *name* and *obj.orgname* against *orgname* in a single loop)

    obj1 := topScope; obj := obj1.next;  (*search for module*)
    WHILE (obj # NIL) & (obj.name # name) & (obj(Module).orgname # orgname) DO
      obj1 := obj; obj := obj1.next
    END ;
    IF obj = NIL THEN  (*insert new module*)
      ...
    ELSE (*module already present*)
      IF decl THEN ORS.Mark("invalid import order") END
    END ;

------------------------------------------------------------------------

**ORB03.Mod**  (tests only *obj.orgname* against *orgname*)

    obj1 := topScope; obj := obj1.next;  (*search for module*)
    WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO
      obj1 := obj; obj := obj1.next
    END ;
    IF obj = NIL THEN  (*insert new module*)
      ...
    ELSE (*module already present*)
      IF decl THEN ORS.Mark("invalid import order") END
    END ;

------------------------------------------------------------------------

**ORB04.Mod**  (tests *obj.orgname* against *name*)

    obj1 := topScope; obj := obj1.next;  (*search for module*)
    WHILE (obj # NIL) & (obj(Module).orgname # name) DO
      obj1 := obj; obj := obj1.next
    END ;
    IF obj = NIL THEN  (*insert new module*)
      ...
    ELSE (*module already present*)
      IF decl THEN ORS.Mark("invalid import order") END
    END ;

------------------------------------------------------------------------

**ORB05.Mod**  (tests all possible combinations EXCEPT *obj.orgname* against *name*)

    obj1 := topScope; obj := obj1.next;  (*search for module*)
    WHILE (obj # NIL) & (obj.name # name) & (obj.name # orgname) &
        (obj(Module).orgname # orgname) DO
      obj1 := obj; obj := obj1.next
    END ;
    IF obj = NIL THEN  (*insert new module*)
      ...
    ELSE (*module already present*)
      IF decl THEN ORS.Mark("invalid import order") END
    END ;

------------------------------------------------------------------------

**ORB06.Mod** (first tests *obj.orgname* against *orgname*, then tests *obj.name* against *name* in two separate loops)

    PROCEDURE ThisModule(name, orgname: ORS.Ident; decl: BOOLEAN; key: LONGINT): Object;
      VAR mod: Module; obj, obj1: Object;
    BEGIN obj1 := topScope; obj := obj1.next;  (*search for module*)
      WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END ;
      IF obj = NIL THEN  (*new module, search for alias*)
        obj := topScope.next;
        WHILE (obj # NIL) & (obj.name # name) DO obj := obj.next END ;
        IF obj = NIL THEN (*insert new module*)
          NEW(mod); mod.class := Mod; mod.rdo := FALSE;
          mod.name := name; mod.orgname := orgname; mod.val := key;
          mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
          obj1.next := mod; obj := mod
        ELSIF decl THEN
          IF obj.rdo THEN ORS.Mark("mult def") ELSE ORS.Mark("conflict with re-import") END
        ELSE ORS.Mark("conflict with alias")
        END
      ELSIF decl THEN (*module already present, explicit import by declaration*)
        IF obj.rdo THEN ORS.Mark("mult def") ELSE ORS.Mark("invalid import order") END
      END ;
      RETURN obj
    END ThisModule;

As compared with *ORB07.Mod* - which checks all four possible combinations of *obj.name* and *obj.orgname* against *name* and *orgname* - *ORB06.Mod* does *not* test the two cross-combinations *obj.orgname # name* and *obj.name # orgname*, making it less restrictive by allowing the cases B10, B11 and B12 below (see also TestAliases.Mod).

Not checking the first cross-combination **(obj.orgname # name)** safely allows:

      MODULE B10; IMPORT X := M, M := M0; END B10.  (*reuse the canonical name M of a module, which has itself been imported under an alias X, as an alias name for another module M0*)
      MODULE B11; IMPORT M := M0, M0 := M; END B11.  (*same as B10, but used to effectively "swap" (the names of) two modules M and M0*)

Not checking the second cross-combination **(obj.name # orgname)** safely allows:

      MODULE B12; IMPORT M0 := M, X := M0; END B12.  (*reuse the alias name M0 of another module as the canonical name of a module which is itself imported under an alias X*)

These cases (B10, B11, B12) can still be handled by the Project Oberon 2013 data structure.

For a detailed case analysis, see [**CaseAnalysis**](http://github.com/andreaspirklbauer/Oberon-test-module-aliases/blob/master/CaseAnalysis.md).

Another variant of ORB06.Mod (ORB06PR.Mod):

    PROCEDURE ThisModule(name, orgname: ORS.Ident; decl: BOOLEAN; key: LONGINT): Object;
      VAR mod: Module; obj, obj0, obj1: Object;
    BEGIN obj1 := topScope; obj0 := obj1.next;  (*search for declared name/alias*)
      WHILE (obj0 # NIL) & (~obj.rdo OR (obj0.name # name)) DO obj0 := obj0.next END ;
      obj := obj1.next; (*search for actual module*)
      WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END ;
      IF (obj0 = NIL) & (obj = NIL) THEN (*insert new module*)
        NEW(mod); mod.class := Mod; mod.rdo := FALSE;
        mod.name := name; mod.orgname := orgname; mod.val := key;
        mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
        obj1.next := mod; obj := mod
      ELSIF ~decl & (obj = NIL) THEN ORS.Mark("hidden import conflict"); obj := obj0
      ELSIF decl THEN (*module already present*)
        IF (obj0 = NIL) & ~obj.rdo THEN obj.name := name (*was hidden, name free*)
        ELSE ORS.Mark("mult decl"); IF obj = NIL THEN obj := obj0 END
        END
      END ;
      RETURN obj
    END ThisModule;

------------------------------------------------------------------------

**ORB07.Mod**  (checks all 4 possible combinations of obj.name, obj.orgname, name and orgname)

As this variant checks all possible combinations of obj.name, obj.orgname, name and orgname, it is *more restrictive* than other solutions (for example, test modules B10, B11, B12 don't compile anymore). This may be desired by the compiler implementor.

    WHILE (obj # NIL) & (obj.name # name) & (obj.name # orgname) &
        (obj(Module).orgname # orgname) & (obj(Module).orgname # name) DO
      obj1 := obj; obj := obj1.next
    END ;
    IF obj = NIL THEN  (*insert new module*)
      ...
    ELSE (*module already present*)
      IF decl OR (obj(Module).orgname # orgname) THEN ORS.Mark("invalid import order") END  (*improved error checking for re-imported modules*)
    END ;

The additional guard **(obj(Module).orgname # orgname)** in the last ELSE clause catches false positives in the first WHILE loop and thereby prevents module B12 from being compiled:

     MODULE B15; IMPORT M0 := M, M1; END B15.  (*conflict with alias, as the name of the re-imported module M0 is already used as an alias*)
 
In this example, the first entry created in the module list during compilation of B15 is <M0,M> ("M0 as an alias for M"). The second entry created is <M1, M1>, which in turn will attempt to re-import M0 (note that M1 imports M0). 

At that moment, the first loop in *ORB.ThisModule* will actually "find" the entry <M0,M>. But it neverthess not correct to continue without an error message, as the name M0 is already taken as an alias for M. Thus, ror re-imports (non = FALSE) - and only in that case - we must make sure that the one found has indeed the canoncial name M0 also. In the above example, that is not the case. The canonical name of the entry found (<M0, M) is M and not M0.

------------------------------------------------------------------------

**ORB08.Mod** (treats explicit imports and re-imports separately)

Treats explict imports and re-imports separately in ORB.ThisModule.

However, this version is incorrect. For example, it does not treat all cases correctly (test module M6 in [**TestAliases.Mod**](Sources/TestAliases.Mod) does not compile):

     PROCEDURE ThisModule(name, orgname: ORS.Ident; decl: BOOLEAN; key: LONGINT): Object;
       VAR mod: Module; obj, obj1: Object;
     BEGIN obj1 := topScope; obj := obj1.next;  (*search for module*)
       IF decl THEN (* direct import from module header*)
         WHILE (obj # NIL) & (obj.name # name) DO obj1 := obj; obj := obj1.next END
       ELSE (*re-import while reading symbol file*)
         WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END
       END ;
       IF obj = NIL THEN  (*insert new module*)
         NEW(mod); mod.class := Mod; mod.rdo := FALSE;
         mod.name := name; mod.orgname := orgname; mod.val := key;
         mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
         obj1.next := mod; obj := mod
       ELSE (*module already present*)
         IF decl THEN ORS.Mark("invalid import order") END
       END ;
       RETURN obj
     END ThisModule;

------------------------------------------------------------------------

**ORB09.Mod**  (allows multiple aliases to refer to the same module -> now M7 and B6-B9 also compile)

This variant allows multiple aliases to refer to the same module, making it less restrictive (for example, test modules B6 - B13 now compile). Adding support for multiple aliases is achieved by introducting the notion of "primary instance" and "secondary instance".

    PROCEDURE ThisModule(name, orgname: ORS.Ident; decl: BOOLEAN; key: LONGINT): Object;
      VAR mod: Module; obj, obj1: Object;
    BEGIN obj1 := topScope; obj := obj1.next;
      IF decl THEN  (*implicit import*)
        WHILE (obj # NIL) & (obj.name # name) DO obj1 := obj; obj := obj1.next END ;  (*search for module*)
        IF obj = NIL THEN
          obj := topScope.next;
          WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj := obj.next END ;  (*search for primary instance*)
          IF obj # NIL THEN  (*insert secondary instance*)
            NEW(mod); mod.class := Mod; mod.rdo := FALSE;
            mod.name := name; mod.orgname := orgname; mod.val := obj.val;
            mod.lev := 0; mod.type := noType; mod.dsc := obj; mod.next := obj.next;
            obj.next := mod; obj := mod  (*return secondary instance*)
          END
        ELSE ORS.Mark("mult def")
        END
      ELSE  (*re-import*)
        WHILE (obj # NIL) & (obj.name # name) & (obj(Module).orgname # orgname) DO
          obj1 := obj; obj := obj1.next
        END ;
        IF (obj # NIL) & (obj(Module).orgname # orgname) THEN ORS.Mark("conflict with alias") END
      END ;
      IF obj = NIL THEN  (*insert new primary instance*)
        NEW(mod); mod.class := Mod; mod.rdo := FALSE;
        mod.name := name; mod.orgname := orgname; mod.val := key;
        mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
        obj1.next := mod; obj := mod
      END ;
      RETURN obj
    END ThisModule;

Plus a few other minor changes in *ORB.thisimport*, *ORB.InType* and *ORB.Import*.

------------------------------------------------------------------------

**ORB10.Mod**  (like ORB09.Mod, but with additional check whether the import order is correct and better error reporting)

Like ORB09.Mod, but with better error reporting in some cases.

    PROCEDURE ThisModule(name, orgname: ORS.Ident; decl: BOOLEAN; key: LONGINT): Object;
      VAR mod: Module; obj, obj1: Object;
    BEGIN obj1 := topScope; obj := obj1.next;
      IF decl THEN  (*explicit import*)
        WHILE (obj # NIL) & (obj.name # name) DO obj1 := obj; obj := obj1.next END ;  (*search for module*)
        IF obj = NIL THEN
          obj := topScope.next;
          WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj := obj.next END ;  (*search for primary instance*)
          IF obj # NIL THEN
            IF obj.rdo THEN  (*explicitly imported primary instance found, now insert secondary instance*)
              NEW(mod); mod.class := Mod; mod.rdo := FALSE;
              mod.name := name; mod.orgname := orgname; mod.val := key;
              mod.lev := 0; mod.type := noType; mod.dsc := NIL; mod.next := NIL;
              obj1.next := mod; obj := mod
            ELSE ORS.Mark("invalid import order")
            END
          END
        ELSE
          IF obj.rdo THEN ORS.Mark("mult def") ELSE ORS.Mark("invalid import order") END
        END
      ELSE  (*re-import*)
        WHILE (obj # NIL) & (obj.name # name) & (obj(Module).orgname # orgname) DO
          obj1 := obj; obj := obj1.next
        END ;
        IF (obj # NIL) & (obj(Module).orgname # orgname) THEN ORS.Mark("conflict with alias") END
      END ;
      IF obj = NIL THEN  (*insert primary instance*)
        NEW(mod); mod.class := Mod; mod.rdo := FALSE;
        mod.name := name; mod.orgname := orgname; mod.val := key;
        mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
        obj1.next := mod; obj := mod
      END ;
      RETURN obj
    END ThisModule;

------------------------------------------------------------------------

**ORB11.Mod**  (like ORB10.Mod, but with no more import order restrictions -> now M9 and B15-B17 also compile)

This variant eliminates the import order restriction by stitching together implicit imports (re-exported types) and later explicit ones.

    PROCEDURE ThisModule(name, orgname: ORS.Ident; decl: BOOLEAN; key: LONGINT): Object;
      VAR mod: Module; obj, obj1: Object;
    BEGIN obj1 := topScope; obj := obj1.next;
      IF decl THEN  (*explicit import*)
        WHILE (obj # NIL) & (obj.name # name) DO obj1 := obj; obj := obj1.next END ;  (*search for module*)
        IF obj = NIL THEN
          obj := topScope.next;
          WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj := obj.next END ;  (*search for primary instance*)
          IF (obj # NIL) & (obj.rdo OR (obj.name # name)) THEN  (*insert secondary instance*)
            NEW(mod); mod.class := Mod; mod.rdo := FALSE;
            mod.name := name; mod.orgname := orgname; mod.val := key;
            mod.lev := 0; mod.type := noType; mod.dsc := NIL; mod.next := NIL;
            obj1.next := mod; obj := mod
          END
        ELSIF obj.rdo (*explicit import*) THEN ORS.Mark("mult def")
        END
      ELSE  (*re-import*)
        WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END
      END ;
      IF obj = NIL THEN  (*insert primary instance*)
        NEW(mod); mod.class := Mod; mod.rdo := FALSE;
        mod.name := name; mod.orgname := orgname; mod.val := key;
        mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
        obj1.next := mod; obj := mod
      END ;
      RETURN obj
    END ThisModule;

In procedure *ORB.Import*, the lists of re-import and explicit imports are merged as follows: Instead of writing:

    WHILE class # 0 DO
      NEW(obj); obj.class := class; Files.ReadString(R, obj.name);
      InType(R, thismod, obj.type); obj.lev := -thismod.lev;
      ...
      obj.next := thismod.dsc; thismod.dsc := obj; Read(R, class)
    END ;

we now insert a new object into the *mod.dsc* list only if it doesn't already exist:

    WHILE class # 0 DO
      NEW(obj); obj.class := class; Files.ReadString(R, obj.name);
      InType(R, thismod, obj.type); obj.lev := -thismod.lev;
      ...
      obj1 := thismod.dsc;  (*search object*
      WHILE (obj1 # NIL) & (obj1.name # obj.name) DO obj1 := obj1.next END ;
      IF obj1 = NIL THEN (*object not found*) obj.next := thismod.dsc; thismod.dsc := obj END ;
      Read(R, class)
    END ;

------------------------------------------------------------------------

**ORB12.Mod, ORP12.Mod**  (like ORB11.Mod, but realized as a two-pass solution)

*This is an experiment of a TWO-pass solution with NO restrictions on import order.*

The main difference to ORB11.Mod is that this variant *avoids* the need to stitch together re-imports with later explicit ones. This comes at the cost of doing two passes, as follows.

In the first pass, we parse the IMPORT statement and create "empty" module entries for each *explicit* import. This is how we know *upfront*, which modules are imported *explicitly*.

    PROCEDURE PrepImport*(VAR name, orgname: ORS.Ident);
      VAR mod: Module; obj, obj1: Object;
    BEGIN obj1 := topScope; obj := obj1.next;
      WHILE (obj # NIL) & (obj.name # name) DO obj1 := obj; obj := obj1.next END ;  (*search for module*)
      IF obj = NIL THEN
        obj := topScope.next;
        WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj := obj.next END ;  (*search for primary instance*)
        NEW(mod); mod.class := Mod; mod.rdo := TRUE;
        mod.name := name; mod.orgname := orgname; mod.type := noType;
        IF obj = NIL THEN  (*insert primary instance*)
          mod.val := 0; mod.lev := nofmod; INC(nofmod); mod.dsc := NIL; mod.next := NIL; obj1.next := mod
        ELSE  (*insert secondary instance*)
          mod.val := obj.val; mod.lev := 0; mod.dsc := obj; mod.next := obj.next; obj.next := mod
        END ;
        IF orgname = "SYSTEM" THEN mod.dsc := system; (*see ORG.Close*)
          IF mod.lev > 0 THEN (*primary instance*) DEC(nofmod); mod.lev := 0 END
        END
      ELSE ORS.Mark("mult def")
      END
    END PrepImport;

In the second pass, we traverse that list and call *Import* to perform the actual import:

    PROCEDURE ImportAll*;
      VAR obj: Object;
    BEGIN obj := topScope.next;
      WHILE obj # NIL DO
        IF obj.rdo & (obj.lev > 0) & (obj.val = 0) THEN Import(obj) END ;
        obj := obj.next
      END
    END ImportAll;

Procedure *Reimport* is called in *InType* when a type is re-imported.

    PROCEDURE Reimport(name, orgname: ORS.Ident; key: LONGINT): Object;
      VAR mod: Module; obj, obj1: Object;
    BEGIN obj1 := topScope; obj := obj1.next;
      WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END ;
      IF obj = NIL THEN  (*insert primary instance*)
        NEW(mod); mod.class := Mod; mod.rdo := FALSE;
        mod.name := name; mod.orgname := orgname; mod.val := key; mod.lev := nofmod; INC(nofmod);
        mod.type := noType; mod.dsc := NIL; mod.next := NIL; obj1.next := mod; obj := MOD
      END ;
      RETURN obj
    END Reimport;

*Comments:*

* In an earlier recursive implementation (see ORB12Recursive.Mod), procedure *Reimport*, called in procedure *InType*, checked whether the re-imported type belongs to an *explicitly* imported module (i.e. to a module in the initial list created by procedure *Import*), and if so, *fully* imported it by recursively calling *Import* before continuing.

      ELSIF obj.dsc = NIL THEN Import(obj)


     In order to be able to call procedure *Import* recursively, we only needed to make the "typtab" table (=table of references of already imported modules) local to it.

* It is entirely possible that an entry for an explicit import M0, as initially created by procedure *PrepImport*, is referenced by a re-imported type M1.T1 *before* the actual explicit import of M0 happens:

      MODULE M0;
        TYPE T0* = RECORD i: INTEGER END ;
      END M0.

      MODULE M1;
        IMPORT M0;
        TYPE T1* = RECORD (M0.T0) j: INTEGER END ;
      END M1.

      MODULE M3;
        IMPORT M1, M0;               (*M1.T1 re-imports M0.T0, before M0 is imported directly*)
        VAR a: M0.T0;
          b: M1.T1;
      END M3.

     In this example, procedure *ORB.PrepImport* creates 2 empty modules entries for M1 and M0 in the global module list during the first phase. During the second phase, procedure *ORB.Import*, called by procedure *ORB.ImportAll*, begins by actualy importing the first module in that list (M1). This triggers the re-import of type M0.T0 using procedure *ORB.Reimport*, which however detects that M0 is a module that has *also* been added as an *explicit* import during the first phase. Therefore, it can safely import module M0 *in full* at that moment and not as a partial re-import. After this step, it is no longer necessary to also insert the type M0.T0 into the object list *obj.dsc* of M0 (it is already there).

* After having re-compiled the compiler, re-compile the entire Oberon system and the compiler again, then restart the system.

* The condition **ELSIF obj.dsc = NIL** in procedure *Reimport* could be improved. Its purpose is to check whether a module created by ORP.PrepImport (i.e. an explicit import) has in fact already been imported, i.e. whether its exported objects have already been read in from the symbol file. But for a module with no exported objects at all, the field *obj.dsc* will still be NIL after it has been imported (as there simply are no exported objects). Currently, the (practically empty) symbol file ist re-read again in such a case. But no harm is done. We could use a flag in the module object to indicate that it has already been imported.

------------------------------------------------------------------------

**ORB12Recursive.Mod, ORP12.Mod**  (like ORB12.Mod, but realized as a recursive implementation)

------------------------------------------------------------------------

**ORB12CheckCyclicImports.Mod, ORP12CheckCyclicImports.Mod**  (like ORB12.Mod, but checks for cyclic imports)

This variant checks for *all* cyclic imports during compilation. It checks both the case where imported or re-imported types *are* re-exported and the case where imported or re-imported types are *not* re-exported.

However, this solution is "unnatural", as it essentially retrofits the implementation by adding module anchors as an afterthought. It is provided here for completeness' sake only.

Therefore, if you also want to check for cyclic imports, it is recommened to use the "module table" model (as used in *oberonc* for example).

* Detecting cyclic imports in *all* cases, including the case where an imported or re-imported type is not re-exported, requires storing "module anchors" of *all* imported modules in the symbol file of the module being compiler (even of modules from which no types are re-exported).

* If this generality is desired, a different solution using a global (GModtab) and local (LModtab) module table is preferred (see http://github.com/lboasso/oberonc and the discussion at http://github.com/andreaspirklbauer/Oberon-test-module-aliases/issues/1).

------------------------------------------------------------------------

**ORB12CheckCyclicImportsRec.Mod, ORP12CheckCyclicImports.Mod** (like ORB12CheckCyclicImports.Mod, but realized as a recursive implementation)

------------------------------------------------------------------------

**ORB13.Mod, ORP.Mod**  (derived from ORB11.Mod, but with no restrictions on import order)

*This is a ONE-pass solution with NO restrictions on import order.*

Like ORB11.Mod, this one-pass solution stitches together implicit imports (re-exported types) and later explicit ones, but in a "deeply recursive" way, i.e. by going all the way down to the object and type level of the entire data structure: for every encountered object or type, the solution checks whether it has already been imported. If so, it reads the information from the symbol file but does not update dthe symbol table data structure (i.e. does a "dry-run" effectively).

------------------------------------------------------------------------

**ORB13CheckCyclicImports.Mod, ORP13CheckCyclicImports.Mod**  (like ORB13.Mod, but checks for cyclic imports)

*This is a ONE-pass solution which also checks for cyclic imports*

Like ORB12CheckCyclicImports.Mod, ORP12CheckCyclicImports.Mod, this one-pass solution checks for *all* cyclic imports during compilation. It checks both the case where imported or re-imported types *are* re-exported and the case where imported or re-imported types are *not* re-exported.

However, this solution is "unnatural", as it essentially retrofits the implementation by adding module anchors as an afterthought. It is provided here for completeness' sake only.

Therefore, if you also want to check for cyclic imports, it is recommened to use the "module table" model (as used in *oberonc* for example).

* Detecting cyclic imports in *all* cases, including the case where an imported or re-imported type is not re-exported, requires storing "module anchors" of *all* imported modules in the symbol file of the module being compiler (even of modules from which no types are re-exported).

* If this generality is desired, a different solution using a global (GModtab) and local (LModtab) module table is preferred (see http://github.com/lboasso/oberonc and the discussion at http://github.com/andreaspirklbauer/Oberon-test-module-aliases/issues/1).



