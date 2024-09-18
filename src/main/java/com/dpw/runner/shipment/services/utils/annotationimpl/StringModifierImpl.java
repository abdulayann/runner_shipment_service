package com.dpw.runner.shipment.services.utils.annotationimpl;


import com.dpw.runner.shipment.services.utils.annotation.StringModifier;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class StringModifierImpl {

    public static void modifyString(Object object) throws IllegalAccessException {
        List<Field> allField = getAllFields(object.getClass());
        for (Field field : allField) {
            if (!field.isSynthetic() && notTransientFinal(field.getModifiers())) {
                if (field.isAnnotationPresent(StringModifier.class) && field.getType() == String.class) {
                    field.setAccessible(true);
                    String string = (String) field.get(object);
                    if (string != null) {
                        StringModifier annotation = field.getAnnotation(StringModifier.class);
                        int maxLength = annotation.maxLength();
                        if (string.length() > maxLength)
                            string = string.substring(0,  maxLength);

                        String pattern = annotation.pattern().patternString;
                        if (pattern != null)
                            string = string.replaceAll(pattern, " ");
                        field.set(object, string);
                    }
                } else if (List.class.isAssignableFrom(field.getType())) {
                    field.setAccessible(true);
                    List<?> list = (List<?>) field.get(object);
                    if (list != null) {
                        for (Object listItem : list) {
                            modifyString(listItem); // Recursive call for list items
                        }
                    }
                } else if (!field.getType().isPrimitive() && !field.getType().isArray()) {
                    field.setAccessible(true);
                    Object nestedObject = field.get(object);
                    if (nestedObject != null) {
                        modifyString(nestedObject); // Recursive call for nested objects
                    }
                }
            }
        }
    }

    private static List<Field> getAllFields(Class<?> fromClazz) {
        List<Field> fields = new ArrayList<>();
        while (fromClazz != Object.class) {
            fields.addAll(Arrays.asList(fromClazz.getDeclaredFields()));
            fromClazz = fromClazz.getSuperclass();
        }
        return fields;
    }

    private static <T> boolean notTransientFinal(int mod) {
        return !(Modifier.isTransient(mod) || Modifier.isFinal(mod));
    }
}
