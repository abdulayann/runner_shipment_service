package com.dpw.runner.shipment.services.utils;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Execution;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

@Execution(CONCURRENT)
class StringUtilityTest {

    @Test
    void testIsEmpty_NullString() {
        assertTrue(StringUtility.isEmpty(null));
    }

    @Test
    void testIsEmpty_EmptyString() {
        assertTrue(StringUtility.isEmpty(""));
    }

    @Test
    void testIsEmpty_NonEmptyString() {
        assertFalse(StringUtility.isEmpty("test"));
    }

    @Test
    void testIsNotEmpty_NullString() {
        assertFalse(StringUtility.isNotEmpty(null));
    }

    @Test
    void testIsNotEmpty_EmptyString() {
        assertFalse(StringUtility.isNotEmpty(""));
    }

    @Test
    void testIsNotEmpty_NonEmptyString() {
        assertTrue(StringUtility.isNotEmpty("test"));
    }

    @Test
    void testToUpperCase_NullString() {
        assertNull(StringUtility.toUpperCase(null));
    }

    @Test
    void testToUpperCase_EmptyString() {
        assertNull(StringUtility.toUpperCase(""));
    }

    @Test
    void testToUpperCase_LowerCaseString() {
        assertEquals("TEST", StringUtility.toUpperCase("test"));
    }

    @Test
    void testToUpperCase_MixedCaseString() {
        assertEquals("TEST", StringUtility.toUpperCase("TeSt"));
    }

    @Test
    void testGetRandomString_Length() {
        int length = 10;
        String randomString = StringUtility.getRandomString(length);
        assertEquals(length, randomString.length());
    }

    @Test
    void testGetRandomString_CharacterRange() {
        String randomString = StringUtility.getRandomString(100);
        assertTrue(randomString.chars().allMatch(i -> (i >= 48 && i <= 57) || (i >= 65 && i <= 90) || (i >= 97 && i <= 122)));
    }

    @Test
    void testGetEmptyString() {
        assertEquals("", StringUtility.getEmptyString());
    }

    @Test
    void testConvertToString_NullObject() {
        assertEquals("", StringUtility.convertToString(null));
    }

    @Test
    void testConvertToString_NonNullObject() {
        assertEquals("123", StringUtility.convertToString(123));
        assertEquals("test", StringUtility.convertToString("test"));
    }

    @Test
    void testGetNullIfEmpty_NullString() {
        assertNull(StringUtility.getNullIfEmpty(null));
    }

    @Test
    void testGetNullIfEmpty_EmptyString() {
        assertNull(StringUtility.getNullIfEmpty(""));
    }

    @Test
    void testGetNullIfEmpty_NonEmptyString() {
        assertEquals("test", StringUtility.getNullIfEmpty("test"));
    }
}
