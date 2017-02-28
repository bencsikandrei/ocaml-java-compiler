let all = 
    let integ = {
        AST.clid="Integer";
        AST.clname="Integer";
        AST.classScope=Object.objectInfo;
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
        AST.classScope=Object.objectInfo;
        AST.clmodifiers=[Public];
        AST.cparent = {tpath=[];tid="Object"} ;
        AST.cattributes = [];
        AST.cinits = [];
        AST.cconsts = [
             {
                cmodifiers = [];
                cname = "String";
                cargstype = [
                    {
                        final = false;
                        vararg = false;
                        ptype = Type.Ref ({Type.tpath=[];Type.tid="String"});
                        pident = "str";
                    }
                ];
                cthrows = [];
                cbody = [];
                mloc = Location.none;
            }

        ];
        AST.cmethods = [];
        AST.ctypes = [];
        AST.cloc = Location.none;
    } in
    let flt = {
        AST.clid="Float";
        AST.clname="Float";
        AST.classScope=Object.objectInfo;
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
        AST.classScope=Object.objectInfo;
        AST.clmodifiers=[Public];
        AST.cparent = {tpath=[];tid="Object"} ;
        AST.cattributes = [];
        AST.cinits = [];
        AST.cconsts = [];
        AST.cmethods = [];
        AST.ctypes = [];
        AST.cloc = Location.none;
    } in

    let ex = {
        AST.clid="Exception";
        AST.clname="Exception";
        AST.classScope=Object.objectInfo;
        AST.clmodifiers=[Public];
        AST.cparent = {tpath=[];tid="Object"} ;
        AST.cattributes = [];
        AST.cinits = [];
        AST.cconsts = [];
        AST.cmethods = [];
        AST.ctypes = [];
        AST.cloc = Location.none;
    } in

    let aex = {
        AST.clid="ArithmeticException";
        AST.clname="ArithmeticException";
        AST.classScope=ex::Object.objectInfo;
        AST.clmodifiers=[Public];
        AST.cparent = {tpath=[];tid="Exception"} ;
        AST.cattributes = [];
        AST.cinits = [];
        AST.cconsts = [];
        AST.cmethods = [];
        AST.ctypes = [];
        AST.cloc = Location.none;
    } in

    integ::str::flt::boole::ex::aex::[]

