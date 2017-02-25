package test;

public class main {

}

class A{}
class B extends A{}

class C {
	public B x(){ return new B();}
}

class D extends C {
	protected B x(){ return new B();}
}
