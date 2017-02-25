package tests;

public abstract class A {

	public A() {
	}

	abstract void mmm(int x);
	void m2(int x){}
	
	void m2(int x[]){}
	
	void m2(String x[]){}
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}

}

public strictfp abstract class C extends A {
	
	public A x;

	public C() {
		// TODO Auto-generated constructor stub
	}

	public static void main(String[] args) {
		// TODO Auto-generated method stub

	}
	
	public void m(int x){
		
	}

	void x(int y){}

	abstract void mmmm(int x);
}

class B extends C {
	void mmmm(int x){}
}
