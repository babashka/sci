public class PublicFields {
    public int x;
    
    public String instanceFoo = "instance field";
    public static String staticFoo = "static field";

    public PublicFields() {
        this.x = 3;
    }
    
    public String instanceFoo() {
        return "instance method";
    }
    
    public static String staticFoo() {
        return "static method";
    }
}
