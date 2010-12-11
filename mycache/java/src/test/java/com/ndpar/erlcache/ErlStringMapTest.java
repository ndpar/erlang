package com.ndpar.erlcache;

import static org.junit.Assert.*;

import java.io.File;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

public class ErlStringMapTest {

    private static String cookie;
    private ErlStringMap map;

    @BeforeClass
    public static void setUpClass() throws Exception {
        cookie = FileUtils.readFileToString(new File("/Users/andrey/.erlang.cookie"));
    }

    @Before
    public void setUp() {
        map = new ErlStringMap("client1", cookie, "slave1@macBook", "mycache");
        map.open();
    }

    @After
    public void tearDown() {
        map.close();
    }

    @Test
    public void getReturnsNullFromEmptyMap() {
        Object result = map.get("testing");
        assertNull(result);
    }

    @Test
    public void getPutRemoveReturnCorrectValues() {
        assertNull(map.put("foo", "bar"));
        assertEquals("bar", map.get("foo"));
        assertEquals("bar", map.put("foo", "baz"));
        assertEquals("baz", map.remove("foo"));
    }

    @Test
    public void removeReturnsNullIfNoSuchEntry() {
        Object result = map.remove("notexsist");
        assertNull(result);
    }

    @Test @Ignore("long running dirty test")
    public void writePerformance() {
        int count = 1000;
        long start = System.currentTimeMillis();
        for (Integer i = 0; i < count; i++) map.put(i.toString(), i.toString());
        long duration = System.currentTimeMillis() - start;
        System.out.println(count + " write operations take: " + duration + "ms. " + (1.0 * duration/count) + "ms per operation");
    }

    @Test @Ignore("long running test")
    public void getPerformance() {
        int count = 1000;
        long start = System.currentTimeMillis();
        for (Integer i = 0; i < count; i++) map.get(i.toString());
        long duration = System.currentTimeMillis() - start;
        System.out.println(count + " get operations take: " + duration + "ms. " + (1.0 * duration/count) + "ms per operation");
    }

    @Test @Ignore("long running dirty test")
    public void removePerformance() {
        int count = 1000;
        long start = System.currentTimeMillis();
        for (Integer i = 0; i < count; i++) map.remove(i.toString());
        long duration = System.currentTimeMillis() - start;
        System.out.println(count + " remove operations take: " + duration + "ms. " + (1.0 * duration/count) + "ms per operation");
    }
}
