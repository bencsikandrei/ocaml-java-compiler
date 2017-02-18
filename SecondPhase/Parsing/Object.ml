let objectInfo = 
    let tmp = {
		AST.clid="Object";
    	AST.clname="Object";
    	AST.classScope=[];
    	AST.clmodifiers=[Public];
    	AST.cparent = {tpath=[];tid="Object"} ;
    	AST.cattributes = [];
    	AST.cinits = [];
    	AST.cconsts = [];
    	AST.cmethods = [
            {
                AST.mmodifiers = [Protected];
                AST.mname = "clone";
                AST.mreturntype = Ref Type.object_type;
                AST.margstype = [];
                AST.mthrows = [];
                AST.mbody = [];
                AST.mloc = Location.none;
            }




        ];
    	AST.ctypes = [];
    	AST.cloc = Location.none;
    } in tmp.classScope<-[tmp];tmp