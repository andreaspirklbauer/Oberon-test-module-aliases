**PROOF OF CORRECTNESS OF ORB.ThisModule**

The recommended variant of the compiler module ORB (see [**ORB06.Mod**](Sources/ORB06.Mod)) implements the following semantics of an import statement **IMPORT M := M1**

* It is module M1 that is imported, not M.
* During compilation of the current module, the imported module M1 is given a temporary (alias) name M, such that an exported object x declared within M1 is referenced during compilation of the current module as M.x .
* After compilation of the current module, the alias name M is no longer used and does not appear anywhere (for example, it is not part of the generated symbol or object file). It's as if has never been there.

This semantics is in line with the official [**Oberon-07 language report**](http://inf.ethz.ch/personal/wirth/Oberon/Oberon07.Report.pdf), which states: *If the form "M := M1" is used in the import list, an exported object x declared within M1 is referenced in the importing module as M.x .*

There are other possible semantics for the import statement that the language report - at least in principle - allows. For example, one could define the import statement *IMPORT M := M1* to mean that module M is imported, not M1, and that M1 is simply the name of the (symbol) file where the compiler should look for when importing M (file redirection). See [**ORB08.Mod**](Sources/ORB08.Mod) for example. However, this semantics leads to issues with re-imports (for example, when the symbol file of M1 contains types that were re-imported from another module M0).

---------------------------------------------------

We want to show that the following code excerpt (*ORB.ThisModule*) produces the correct message in *all* possible cases.

    PROCEDURE ThisModule(name, orgname: ORS.Ident; decl: BOOLEAN; key: LONGINT): Object;
      VAR mod: Module; obj, obj1: Object;
    BEGIN obj1 := topScope; obj := obj1.next;  (*search for module*)
      WHILE (obj # NIL) & (obj(Module).orgname # orgname) DO obj1 := obj; obj := obj1.next END ;
      IF obj = NIL THEN  (*new module, search for alias*)
        obj := topScope.next;
        WHILE (obj # NIL) & (obj.name # name) DO obj := obj.next END ;
        IF obj = NIL THEN (*insert new module*)

          (*CASE A: obj.orgname # orgname  &  obj.name # name => the module is not present and the alias name is not used*)

          NEW(mod); mod.class := Mod; mod.rdo := FALSE;
          mod.name := name; mod.orgname := orgname; mod.val := key;
          mod.lev := nofmod; INC(nofmod); mod.type := noType; mod.dsc := NIL; mod.next := NIL;
          obj1.next := mod; obj := mod
        ELSE

          (*CASE C: obj.orgname # orgname  &  obj.name = name => the module is not present, but their aliases/names are the same*)

          IF decl THEN
            IF obj.rdo THEN ORS.Mark("mult def") ELSE ORS.Mark("conflict with re-import") END
          ELSE ORS.Mark("conflict with alias")
          END

        END
      ELSE

        (*CASE B: obj.orgname = orgname => i.e. the module is already present*)

        IF decl THEN (*module already present, explicit import by declaration*)
          IF obj.rdo THEN ORS.Mark("mult def") ELSE ORS.Mark("invalid import order") END
        END

      END ;
      RETURN obj
    END ThisModule;

---------------------------------------------------

In the text below, we use the following notation and conventions:

- **<obj.name, org.orgname>** is an **existing** module object found in the module list (if one is found)
- **<name, orgname>** is a **new** module to be imported (it is either found or inserted into the module list)

Furthermore, the meaning of the object fields *obj.rdo* and *obj.decl* in the symbol table is defined as follows:

- obj.rdo = TRUE means that the found module object *obj* was originaly imported *explicitly*
- obj.rdo = FALSE means that the found module object *obj* was originaly *re-imported*

and

- decl = TRUE means that <name, orgname> is imported *explicitly* (via the IMPORT statement)
- decl = FALSE means that <name, orgname = name> is *re-imported* (via ORB.InType)

--------------------------------------------------------------------------------------------------------

First, note that we don't check the two cross-combinations *obj.orgname - name* and *obj.name - orgname* in the *initial* two WHILE loops of *ORB.ThisModule*, i.e. these two cases are explicitly *allowed* in our implementation. However, this is sensible and safe, because:

* not checking the first cross-combination **(obj.orgname # name)** safely allows:

      MODULE B10; IMPORT X := M0, M0 := M1; END B10. (*<obj.name, obj.orgname> = <X, M0>, <name, orgname> = <M0, M1>*)

      MODULE B11; IMPORT M := M0, M0 := M; END B11.  (*<obj.name, obj.orgname> = <M, M0>, <name, orgname> = <M0, M>*)


* not checking the second cross-combination **(obj.name # orgname)** safely allows:

      MODULE B12; IMPORT M0 := M, X := M0; END B12.  (*<obj.name, obj.orgname> = <M0, M>, <name, orgname> = <X, M0>*)

* duplicates are handled in cases B and C, where it is guaranteed that there is only a *single* alias per imported module => this means that these cases can still be handled by the module list data structure of the Oberon-07 compiler on the Project Oberon 2013 system.

The reader may wonder, why B10, B11 and B12 are allowed, while other cases in [**TestAliases.Mod**](Sources/TestAliases.Mod), such as B1-B9, are not. The answer is that a) they come for free, b) they do not cause any harm and c) they are not as pathological as some of the other cases in the test file. Of course, one could also decide to *disallow* these the cases B10, B11, B12, but then one would need to re-introduce the two cross-combinations in the WHILE loop of *ORB.ThisModule*, i.e. add two string comparisons for *every* (!) module. We opted to not do that. If you really feel bad about B10, B11 and B12, just add back these tests (see [**ORB07.Mod**](Sources/ORB07.Mod) for example). But.. it's fine the way it is.

Second, convince yourself that we are either in case A, case B or case C (with the above comment in mind).

Third, if we are in case A, then *obj.orgname # orgname* (the module is not present) and *obj.name # name* (the alias name is not used). In that case, a new module entry is created and inserted in the symbol table.

Finally, if we are not in case A, then we must be either in case B or in case C, i.e.

* CASE B: **obj.orgname = orgname**  (a module is found in the first loop), or
* CASE C: **obj.orgname # orgname & obj.name = name**  (a module is found in the second loop)

--------------------------------------------------------------------------------------------------------

**CASE B: We found an obj with obj.orgname = orgname (i.e. obj found in the first loop):**

We want to verify that the following code (=the last 3 lines above) covers all possible cases:

        (*CASE B: obj.orgname = orgname => i.e. the module is already present*)
        IF decl THEN (*module already present, explicit import by declaration*)
          IF obj.rdo THEN ORS.Mark("mult def") ELSE ORS.Mark("invalid import order") END
        END

Since we are in CASE B, we assume **obj.orgname = orgname** (= M0) and now systematically write down all the possible ways how we got here.

*CASE B.1.* We found an **unaliased explicit import** <obj.name, obj.orgname> = <M0, M0> in the module list (obj.rdo = TRUE)

    Assume obj.name = M0, obj.orgname = M0.

    The following explicit imports <name := orgname> satisfy the condition obj.orgname = orgname (= M0):

         a. IMPORT M0;       (name = orgname = M0)    => "mult def"  (don't allow importing the same module twice)
         b. IMPORT X := M0;  (name = X, orgname = M0) => "mult def"  (don't allow adding explicit aliases to an unaliased explicit import)

    The following re-imports <name, orgname = name> satisfy the conditition obj.orgname = orgname (= M0):

         a. <M0, M0>         (name = orgname = M0) => finds the already explicitly imported module => no error message

    Convince yourself that these are indeed all possible cases.

*CASE B.2.* We found an **aliased explicit import** <obj.name, obj.orgname> = <M1, M0> in the module list (obj.rdo = TRUE)

    Assume obj.name = M1, obj.orgname = M0.

    The following explicit imports <name := orgname> satisfy the conditition obj.orgname = orgname (= M0):

         a. IMPORT M0;       (name = orgname = M0)     => "mult def"  (don't allow importing the same module twice)
         b. IMPORT X := M0;  (name = X, orgname = M0)  => "mult def"  (don't allow multiple explicit aliases to the same explicit import)
         c. IMPORT M1 := M0; (name = M1, orgname = M0) => "mult def"  (don't allow explicitly importing the same aliased module twice)

    The following re-imports <name, orgname = name> satisfy the conditition obj.orgname = orgname (= M0):

         a. <M0, M0>         (name = orgname = M0) => finds the already explicitly imported module => no error message

    Convince yourself that these are indeed all possible cases.

*CASE B.3.* We found an **re-import** <obj.name, obj.orgname> = <M0, M0> in the module list (obj.rdo = FALSE)

    Assume obj.name = M0, obj.orgname = M0.

    The following explicit imports <name := orgname> satisfy the conditition obj.orgname = orgname (= M0):

         a. IMPORT M0;       (name = orgname = M0)   => "invalid import order"  (don't allow explicit imports of already re-imported modules)
         b. IMPORT X := M0;  name = X, orgname = M0) => "invalid import order"  (don't allow aliased explicit imports of already re-imported modules)

    The following re-imports <name, orgname = name> satisfy the conditition obj.orgname = orgname (= M0):

         a. <M0, M0>         (name = orgname = M0) => finds the already re-imported module => no error message

    Convince yourself that these are indeed all possible cases.

*CASE B.4.* We cannot find aliased re-imports <obj.name, obj.orgname> = <M1, M0> in the module list (obj.rdo = FALSE) as these are not possible. Re-imports don't carry alias names, as alias names are only used during compilation and are never propagated to client modules via symbol or object files.

--------------------------------------------------------------------------------------------------------

**CASE C: We found an obj with obj.orgname # orgname and obj.name = name (i.e. obj found in the second loop):**

We want to verify that the following code (=the code starting at the innermost ELSIF above) covers all possible cases:

          (*CASE C: obj.orgname # orgname  &  obj.name = name => the module is not present, but their aliases/names are the same*)
          IF decl THEN
            IF obj.rdo = TRUE THEN ORS.Mark("mult def") ELSE ORS.Mark("conflict with re-import") END
          ELSE ORS.Mark("conflict with alias")
          END

Since we are in CASE C, we assume **obj.orgname # orgname** and **obj.name = name** (= M0) and now systematically write down all the possible ways how we got here.

*CASE C.1.* We found an **unaliased explicit import** <obj.name, obj.orgname> = <M0, M0> in the module list (obj.rdo = TRUE)

    Assume obj.name = M0, obj.orgname = M0.

    The following explicit imports <name := orgname> satisfy the condition obj.orgname # orgname and obj.name = name (= M0):

         a. IMPORT M0 := M1;  (name = M0, orgname = M1) => "mult def"  (don't allow using an existing module name M1 as an alias for another module)

    No re-imports <name, orgname = name> satisfy the condition obj.orgname # orgname and obj.name = name, because:

         - the first condition obj.orgname # orgname would impose orgname = M1
         - the second condition obj.name = name would impose name = M0
         - but a re-import can only be of the form <name, orgname> = <M0, M0> (i.e. is always unaliased)

    Convince yourself that these are indeed all possible cases.

*CASE C.2.* We found an **aliased explicit import** <obj.name, obj.orgname> = <M1, M0> in the module list (obj.rdo = TRUE)

    Assume obj.name = M1, obj.orgname = M0.

    The following explicit imports <name := orgname> satisfy the condition obj.orgname # orgname and obj.name = name (= M1):

         a. IMPORT M1;        (name = orgname = M1)     => "mult def"  (don't allow re-using an existing alias name for a new unaliased explict import)
         b. IMPORT M1 := M2;  (name = M1, orgname = M2) => "mult def"  (don't allow re-using an existing alias as an alias for a few aliased explict import)

    The following re-imports <name, orgname = name> satisfy the condition obj.orgname # orgname and obj.name = name:

         a. <M1, M1>         (name = orgname = M1) => "conflict with alias"           (M1 conflicts with alias of explicit import)

    Convince yourself that these are indeed all possible cases.

*CASE C.3.* We found a **re-import** <obj.name, obj.orgname> = <M0, M0> in the module list (obj.rdo = FALSE)

    Assume obj.name = M0, obj.orgname = M0.

    The following explicit imports <name := orgname> satisfy the condition obj.orgname # orgname and obj.name = name (= M0):

         a. IMPORT M0 := M1;  (name = M0, orgname = M1) => "conflict with re-import"  (M0 conflicts with re-import)

    No re-imports <name, orgname = name> satisfy the condition obj.orgname # orgname and obj.name = name, because:

         - the first condition obj.orgname # orgname would impose orgname = M1
         - the second condition obj.name = name would impose name = M0
         - but a re-import can only be of the form <name, orgname> = <M0, M0> (i.e. is always unaliased)

    Convince yourself that these are indeed all possible cases.

*CASE C.4.* We cannot find aliased re-imports <obj.name, obj.orgname> = <M1, M0> in the module list (obj.rdo = FALSE) as these are not possible. Re-imports don't carry alias names, as alias names are only used during compilation and are never propagated to client modules via symbol or object files.
