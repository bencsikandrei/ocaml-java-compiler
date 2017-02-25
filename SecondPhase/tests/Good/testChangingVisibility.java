package test;

public class main {

}

class A{}
class B extends A{}

class C {
	public B x(){ return new B();}
	private void x2(){}
	protected B x3(){return new B();}
}

class D extends C {
	public B x(){ return new B();}
	public B x2(){ return new B();}
	public B x3(){ return new B();}
}
