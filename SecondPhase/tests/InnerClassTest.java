public class InnerClassTest {

    public int x = 0;

    // This is a inner class
    class FirstLevel {

        public int x = 1;

        void methodInFirstLevel(int x) {
            System.out.println("x = " + x);
            System.out.println("this.x = " + this.x);
        }
    }

    // This is a nested class
    static class StaticLevel {

        public int x = 42;

        void methodStatic(int x) {
            System.out.println("x = " + x);
            System.out.println("this.x = " + this.x);
        }
    }

    public static void main(String[] args) {
        OutterClass oc = new OutterClass();
        oc.methodOuter();
        InnerClassTest st = new InnerClassTest();
        InnerClassTest.FirstLevel fl = st.new FirstLevel();
        fl.methodInFirstLevel(23);
        InnerClassTest.StaticLevel bl = InnerClassTest.new StaticLevel();
        bl.methodStatic(100);
    }
}

class OutterClass {
    public void methodOuter() {
        //FirstLevel fl = new FirstLevel();
        System.out.println("This is bad when not commented");
    }
}