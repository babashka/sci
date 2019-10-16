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

    /**
     * Implementation detail, don't use.
     * @return: irrelevant.
     */
    public HashMap<String, Object> _val() {
        return _vars;
    }
}
