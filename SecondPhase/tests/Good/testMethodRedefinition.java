package test;

public class main {

}

class A{}
class B extends A{}

class C {
	A x(){ return new B();}
	A x(int a, int b){ return new B();}
}

class D extends C {
	B x(){ return new B();}
	B x(int a, int b){ return new B();}
}
