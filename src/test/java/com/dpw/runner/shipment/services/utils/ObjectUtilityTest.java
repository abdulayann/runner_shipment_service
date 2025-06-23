package com.dpw.runner.shipment.services.utils;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

@Execution(CONCURRENT)
class ObjectUtilityTest {

    class TestClassA {
        protected String field1;
        protected Integer field2;
        Boolean field3;
    }

    class TestClassB extends TestClassA {
        protected Double field4;
    }
    class TestClassC extends TestClassB {
        Long field5;
        protected Long field6;
    }

    class EmptyClass {}

    @Test
    void testGetAllFields_ClassWithNoSuperClass() {
        Map<String, Class<Object>> fields = ObjectUtility.getAllFields(TestClassA.class, null);
        assertNotNull(fields);
        assertEquals(4, fields.size());
        assertEquals(String.class, fields.get("field1"));
        assertEquals(Integer.class, fields.get("field2"));
        assertEquals(Boolean.class, fields.get("field3"));
    }

    @Test
    void testGetAllFields_ClassWithSuperClass() {
        Map<String, Class<Object>> fields = ObjectUtility.getAllFields(TestClassB.class, null);
        assertNotNull(fields);
        assertEquals(5, fields.size());
        assertEquals(String.class, fields.get("field1"));
        assertEquals(Integer.class, fields.get("field2"));
        assertEquals(Boolean.class, fields.get("field3"));
        assertEquals(Double.class, fields.get("field4"));
    }

    @Test
    void testGetAllFields_ClassWithMultipleSuperClasses() {
        Map<String, Class<Object>> fields = ObjectUtility.getAllFields(TestClassC.class, null);
        assertNotNull(fields);
        assertEquals(7, fields.size());
        assertEquals(String.class, fields.get("field1"));
        assertEquals(Integer.class, fields.get("field2"));
        assertEquals(Boolean.class, fields.get("field3"));
        assertEquals(Double.class, fields.get("field4"));
        assertEquals(Long.class, fields.get("field5"));
        assertEquals(Long.class, fields.get("field6"));
    }

    @Test
    void testGetAllFields_EmptyClass() {
        Map<String, Class<Object>> fields = ObjectUtility.getAllFields(EmptyClass.class, null);
        assertNotNull(fields);
        assertEquals(1, fields.size());
    }
}