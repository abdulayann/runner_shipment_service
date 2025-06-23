package com.dpw.runner.shipment.services.utils;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

@SuppressWarnings("rawtypes")
public class ObjectUtility {
    private ObjectUtility(){}

    public static <T> Map<String, Class<T>> getAllFields(final Class<?> type, Map<String, Class<T>> fields) {
        if(fields == null) {
            fields = new HashMap<>();
        }
        if(type.getDeclaredFields().length == 0) {
            return fields;
        }
        for (Field field : type.getDeclaredFields()) {
            fields.put(field.getName(), (Class<T>) field.getType());
        }
        if(type.getSuperclass() != null) {
            getAllFields(type.getSuperclass(), fields);
        }
        return fields;
    }

}
