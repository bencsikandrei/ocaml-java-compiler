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
                AST.mthrows = [{Type.tpath=[];Type.tid="CloneNotSupportedException"}];
                AST.mbody = [];
                AST.mloc = Location.none;
            };
            {
                AST.mmodifiers = [Public];
                AST.mname = "equals";
                AST.mreturntype = Primitive Type.Boolean;
                AST.margstype = [ 
                    {
                        final = false;
                        vararg = false;
                        ptype = Ref Type.object_type;
                        pident = "obj";
                    }
                ];
                AST.mthrows = [];
                AST.mbody = [];
                AST.mloc = Location.none;
            };
            {
                AST.mmodifiers = [Protected];
                AST.mname = "finalize";
                AST.mreturntype = Type.Void;
                AST.margstype = [];
                AST.mthrows = [{Type.tpath=[];Type.tid="Throwable"}];
                AST.mbody = [];
                AST.mloc = Location.none;
            };
            {
                AST.mmodifiers = [Public];
                AST.mname = "hashCode";
                AST.mreturntype = Primitive Type.Int;
                AST.margstype = [];
                AST.mthrows = [];
                AST.mbody = [];
                AST.mloc = Location.none;
            };
            {
                AST.mmodifiers = [Public; Final];
                AST.mname = "notify";
                AST.mreturntype = Type.Void;
                AST.margstype = [];
                AST.mthrows = [];
                AST.mbody = [];
                AST.mloc = Location.none;
            };
            {
                AST.mmodifiers = [Public; Final];
                AST.mname = "notifyAll";
                AST.mreturntype = Type.Void;
                AST.margstype = [];
                AST.mthrows = [];
                AST.mbody = [];
                AST.mloc = Location.none;
            };
            {
                AST.mmodifiers = [Public];
                AST.mname = "toString";
                AST.mreturntype = Ref {Type.tpath=[];Type.tid="String"};
                AST.margstype = [];
                AST.mthrows = [];
                AST.mbody = [];
                AST.mloc = Location.none;
            };
            {
                AST.mmodifiers = [Public; Final];
                AST.mname = "wait";
                AST.mreturntype = Type.Void;
                AST.margstype = [];
                AST.mthrows = [{Type.tpath=[];Type.tid="InterruptedException"}];
                AST.mbody = [];
                AST.mloc = Location.none;
            };
             {
                AST.mmodifiers = [Public; Final];
                AST.mname = "wait";
                AST.mreturntype = Type.Void;
                AST.margstype = [
                    {
                        final = false;
                        vararg = false;
                        ptype = Primitive Type.Long;
                        pident = "timeout";
                    }
                ];
                AST.mthrows = [{Type.tpath=[];Type.tid="InterruptedException"}];
                AST.mbody = [];
                AST.mloc = Location.none;
            };
            {
                AST.mmodifiers = [Public; Final];
                AST.mname = "wait";
                AST.mreturntype = Type.Void;
                AST.margstype = [
                    {
                        final = false;
                        vararg = false;
                        ptype = Primitive Type.Long;
                        pident = "timeout";
                    };
                    {
                        final = false;
                        vararg = false;
                        ptype = Primitive Type.Int;
                        pident = "nanos";
                    };
                ];
                AST.mthrows = [{Type.tpath=[];Type.tid="InterruptedException"}];
                AST.mbody = [];
                AST.mloc = Location.none;
            };
        ];

    	AST.ctypes = [];
    	AST.cloc = Location.none;
    } in tmp.classScope<-[tmp];tmp

