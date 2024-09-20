package com.dpw.runner.shipment.services.utils;

import java.security.SecureRandom;

/**
 * this helper is used to implement all common methods in all projects like utils function
 */
public class StringUtility {
    private StringUtility(){}
    public static final SecureRandom random = new SecureRandom();

    public static boolean isEmpty(String value) {
        return value == null || value.isEmpty();
    }

    public static boolean isNotEmpty(String value) {
        return value != null && !value.isEmpty();
    }

    public static String toUpperCase(String value) {
        if(isEmpty(value)){
            return null;
        }
        return value.toUpperCase();
    }

    public static String getRandomString(int length) {
        int lowerLimit = 48;
        int upperLimit = 122;

        return random.ints(lowerLimit, upperLimit + 1)
                .filter(i -> (i <= 57 || i >= 65) && (i <= 90 || i >= 97))
                .limit(length)
                .collect(StringBuilder::new, StringBuilder::appendCodePoint, StringBuilder::append)
                .toString();
    }

    public static String getEmptyString() {
        return "";
    }

    public static String convertToString(Object object) {
        if (object == null)
            return getEmptyString();

        return String.valueOf(object);
    }

    public static String getNullIfEmpty(String value) {
        return StringUtility.isEmpty(value) ? null : value;
    }
}
