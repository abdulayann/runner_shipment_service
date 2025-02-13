package com.dpw.runner.shipment.services.utils;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

@SuppressWarnings("rawtypes")
public class ObjectUtility {
    private ObjectUtility() {
    }

    public static Map<String, Class> getAllFields(final Class<?> type, Map<String, Class> fields) {
        if (fields == null) {
            fields = new HashMap<>();
        }
        if (type.getDeclaredFields().length == 0) {
            return fields;
        }
        for (Field field : type.getDeclaredFields()) {
            fields.put(field.getName(), field.getType());
        }
        if (type.getSuperclass() != null) {
            getAllFields(type.getSuperclass(), fields);
        }
        return fields;
    }

}
