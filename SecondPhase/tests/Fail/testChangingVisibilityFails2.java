package test;

public class main {

}

class A{}
class B extends A{}

class C {
	public B x(){ return new B();}
}

class D extends C {
	private B x(){ return new B();}
}
