package borkdude.sci.options;

import java.util.HashMap;

public class Namespace {

    private String _name;
    private HashMap<String,Object> _vars = new HashMap<String, Object>();

    public Namespace(String name) {
        _name = name;
    }

    public String getName() {
        return _name;
    }

    public Namespace addVar(String name, Object value) {
        _vars.put(name, value);
        return this;
    }

    public HashMap<String, Object> val() {
        return _vars;
    }
}
