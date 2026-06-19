// Marker class for security regression test (issue: string type-hint sandbox bypass).
// Its static initializer sets a system property so a host test can observe whether
// the class was loaded/initialized, without the test itself referencing the class
// symbol (which would trigger the load).
public class StaticInitOracle {
    static {
        System.setProperty("sci.static.init.oracle", "ran");
    }
    public static int marker() {
        return 42;
    }
}
