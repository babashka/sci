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
     * This is used to convert this class to a nested HashMap which will then be
     * used by sci to translate to Clojure. Not part of the `Options`
     * API.
     * @return: mutable HashMap which is not meant for consumption by the end
     * user.
     */
    public HashMap<String, Object> _val() {
        return _vars;
    }
}
