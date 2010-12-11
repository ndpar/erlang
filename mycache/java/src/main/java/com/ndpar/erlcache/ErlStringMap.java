package com.ndpar.erlcache;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import com.ericsson.otp.erlang.*;

public class ErlStringMap implements Map<String, String> {

    private final OtpSelf self;
    private final OtpPeer other;
    private final String cacheModule;

    private OtpConnection connection;

    public ErlStringMap(String client, String cookie, String serverNode, String cacheModule) {
        try {
            self = new OtpSelf(client, cookie);
            other = new OtpPeer(serverNode);
            this.cacheModule = cacheModule;
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    public void open() {
        try {
            connection = self.connect(other);
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    public void close() {
        try {
            connection.close();
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    public String put(String key, String value) {
        return remoteCall("put", key, value);
    }

    public String get(Object key) {
        return remoteCall("get", (String) key);
    }

    public String remove(Object key) {
        return remoteCall("remove", (String) key);
    }

    private String remoteCall(String method, String... args) {
        try {
            connection.sendRPC(cacheModule, method, stringsToErlangStrings(args));
            OtpErlangObject received = connection.receiveRPC();
            return parse(received);
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage(), e);
        }
    }

    private OtpErlangObject[] stringsToErlangStrings(String[] strings) {
        OtpErlangObject[] result = new OtpErlangObject[strings.length];
        for (int i = 0; i < strings.length; i++) result[i] = new OtpErlangString(strings[i]);
        return result;
    }

    private String parse(OtpErlangObject otpObj) {
        if (otpObj instanceof OtpErlangAtom) {
            OtpErlangAtom atom = (OtpErlangAtom) otpObj;
            if (atom.atomValue().equals("null")) return null;
            else throw new IllegalArgumentException("Only atom null is supported");

        } else if (otpObj instanceof OtpErlangString) {
            OtpErlangString str = (OtpErlangString) otpObj;
            return str.stringValue();
        }
        throw new IllegalArgumentException("Unexpected type " + otpObj.getClass().getName());
    }


    public void clear() {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    public boolean containsKey(Object key) {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    public boolean containsValue(Object value) {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    public Set entrySet() {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    public boolean isEmpty() {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    public Set keySet() {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    public void putAll(Map m) {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    public int size() {
        throw new UnsupportedOperationException("Not implemented yet");
    }

    public Collection values() {
        throw new UnsupportedOperationException("Not implemented yet");
    }
}
