let all = 
    let integ = {
        AST.clid="Integer";
        AST.clname="Integer";
        AST.classScope=[Object.objectInfo];
        AST.clmodifiers=[Public];
        AST.cparent = {tpath=[];tid="Object"} ;
        AST.cattributes = [];
        AST.cinits = [];
        AST.cconsts = [];
        AST.cmethods = [];
        AST.ctypes = [];
        AST.cloc = Location.none;
    } in
    let str = {
        AST.clid="String";
        AST.clname="String";
        AST.classScope=[Object.objectInfo];
        AST.clmodifiers=[Public];
        AST.cparent = {tpath=[];tid="Object"} ;
        AST.cattributes = [];
        AST.cinits = [];
        AST.cconsts = [];
        AST.cmethods = [];
        AST.ctypes = [];
        AST.cloc = Location.none;
    } in
    let flt = {
        AST.clid="Float";
        AST.clname="Float";
        AST.classScope=[Object.objectInfo];
        AST.clmodifiers=[Public];
        AST.cparent = {tpath=[];tid="Object"} ;
        AST.cattributes = [];
        AST.cinits = [];
        AST.cconsts = [];
        AST.cmethods = [];
        AST.ctypes = [];
        AST.cloc = Location.none;
    } in
    let boole = {
        AST.clid="Boolean";
        AST.clname="Boolean";
        AST.classScope=[Object.objectInfo];
        AST.clmodifiers=[Public];
        AST.cparent = {tpath=[];tid="Object"} ;
        AST.cattributes = [];
        AST.cinits = [];
        AST.cconsts = [];
        AST.cmethods = [];
        AST.ctypes = [];
        AST.cloc = Location.none;
    } in

    integ::str::flt::boole::[]

