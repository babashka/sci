package borkdude.sci.options;

import java.util.HashMap;
import java.util.ArrayList;
import borkdude.sci.options.Namespace;

public class Options {

    private HashMap<String,Object> _bindings = new HashMap<String,Object>();
    private HashMap<String,Namespace> _namespaces = new HashMap<String, Namespace>();
    private ArrayList<String> _allow = new ArrayList<String>();
    private ArrayList<String> _deny = new ArrayList<String>();
    private String _preset;

    public Options() {
    }

    public Options addBinding(String name, Object value) {
        _bindings.put(name, value);
        return this;
    }

    public Options deny(String name) {
        _deny.add(name);
        return this;
    }

    public Options allow(String name) {
        _allow.add(name);
        return this;
    }

    public Options addNamespace(Namespace n) {
        _namespaces.put(n.getName(), n);
        return this;
    }

    public Options setPreset(String name) {
        _preset = name;
        return this;
    }

    /**
     * This is used to convert this class to a nested HashMap which will then be
     * used by sci to translate to Clojure. Not part of the `Options` API.
     * @return: mutable HashMap which is not meant for consumption by the end
     * user.
     */
    public HashMap<String, Object> _val() {
        HashMap<String, Object> ret = new HashMap<String, Object>();
        ret.put("namespaces", _namespaces);
        ret.put("bindings", _bindings);
        ret.put("allow", _allow);
        ret.put("deny", _deny);
        ret.put("preset", _preset);
        return ret;
    }
}
