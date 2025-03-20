package com.dpw.runner.shipment.services.utils;

import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

public class FieldUtils {
    private FieldUtils() {
    }

    /**
     * Gets all fields of a class except Hibernate relationship fields.
     *
     * @param clazz The class to inspect.
     * @return A list of fields excluding Hibernate relationship fields.
     */
    public static List<String> getNonRelationshipFields(Class<?> clazz) {
        List<String> fields = new ArrayList<>();

        // Get all declared fields (including private ones)
        for (Field field : clazz.getDeclaredFields()) {
            // Check if the field has any Hibernate relationship annotation
            if (!isRelationshipField(field)) {
                fields.add(field.getName());
            }
        }

        return fields;
    }

    private static boolean isRelationshipField(Field field) {
        return field.isAnnotationPresent(OneToOne.class) ||
                field.isAnnotationPresent(OneToMany.class) ||
                field.isAnnotationPresent(ManyToOne.class) ||
                field.isAnnotationPresent(ManyToMany.class);
    }
}
