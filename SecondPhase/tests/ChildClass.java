
public class ChildClass extends ParentClass {
	
	public void childMethod() {
		System.out.println("Child Method");
	}

	public void overwrittenMethod() {
		System.out.println("I've overwritten my parent's method!");
	}

	public static void main(String[] args) {
		ChildClass cc = new ChildClass();
		cc.childMethod();
		cc.parentMethod();
		cc.overwrittenMethod();
	}
}

class ParentClass {
	
	public void parentMethod() {
		System.out.println("Parent Method");
	}

	public void overwrittenMethod() {
		System.out.println("I'm not supposed to be seen");
	}
}