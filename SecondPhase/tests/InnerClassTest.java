public class InnerClassTest {

    public int x = 0;

    class FirstLevel {

        public int x = 1;

        void methodInFirstLevel(int x) {
            System.out.println("x = " + x);
            System.out.println("this.x = " + this.x);
            System.out.println("InnerClassTest.this.x = " + InnerClassTest.this.x);
        }
    }

    public static void main(String[] args) {
        InnerClassTest st = new InnerClassTest();
        InnerClassTest.FirstLevel fl = st.new FirstLevel();
        fl.methodInFirstLevel(23);
    }
}