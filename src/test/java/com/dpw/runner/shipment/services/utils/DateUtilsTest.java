package com.dpw.runner.shipment.services.utils;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;

import static org.junit.Assert.assertEquals;
import static org.junit.jupiter.api.parallel.ExecutionMode.CONCURRENT;

@ExtendWith(MockitoExtension.class)
@Execution(CONCURRENT)
class DateUtilsTest {

    @Test
    void testGetDateAsString() {
        LocalDateTime dateTime = LocalDateTime.of(2022, 5, 15, 10, 30);
        String expected = "05/15/2022";
        String actual = DateUtils.getDateAsString(dateTime);
        assertEquals(expected, actual);
    }

    @Test
    void testConvertDateFromUserTimeZone_EnableTenantTimeZone() {
        LocalDateTime date = LocalDateTime.of(2022, 5, 15, 10, 30);
        String browserTZ = "America/New_York";
        String tenantTZ = "Europe/London";
        boolean enableTimeZone = true;
        LocalDateTime expected = LocalDateTime.of(2022, 5, 15, 9, 30); // UTC equivalent of 2022-05-15T10:30 in Europe/London
        LocalDateTime actual = DateUtils.convertDateFromUserTimeZone(date, browserTZ, tenantTZ, enableTimeZone);
        assertEquals(expected, actual);
    }

    @Test
    void testConvertDateFromUserTimeZone_EnableBrowserTimeZone() {
        LocalDateTime date = LocalDateTime.of(2022, 5, 15, 10, 30);
        String browserTZ = "America/New_York";
        String tenantTZ = "";
        boolean enableTimeZone = true;
        LocalDateTime expected = LocalDateTime.of(2022, 5, 15, 14, 30); // UTC equivalent of 2022-05-15T10:30 in America/New_York
        LocalDateTime actual = DateUtils.convertDateFromUserTimeZone(date, browserTZ, tenantTZ, enableTimeZone);
        assertEquals(expected, actual);
    }

    @Test
    void testConvertDateFromUserTimeZone_NoTimeZoneEnabled() {
        LocalDateTime date = LocalDateTime.of(2022, 5, 15, 10, 30);
        String browserTZ = "";
        String tenantTZ = "";
        boolean enableTimeZone = false;
        LocalDateTime expected = date; // UTC equivalent of 2022-05-15T10:30 in America/New_York
        LocalDateTime actual = DateUtils.convertDateFromUserTimeZone(date, browserTZ, tenantTZ, enableTimeZone);
        assertEquals(expected, actual);
    }

    @Test
    void testConvertDateToUserTimeZone_EnableTenantTimeZone() {
        LocalDateTime date = LocalDateTime.of(2022, 5, 15, 10, 30);
        String browserTZ = "America/New_York";
        String tenantTZ = "Europe/London";
        boolean enableTimeZone = true;
        LocalDateTime expected = LocalDateTime.of(2022, 5, 15, 11, 30); // Europe/London equivalent of 2022-05-15T10:30 in UTC
        LocalDateTime actual = DateUtils.convertDateToUserTimeZone(date, browserTZ, tenantTZ, enableTimeZone);
        assertEquals(expected, actual);
    }

    @Test
    void testConvertDateToUserTimeZone_EnableBrowserTimeZone() {
        LocalDateTime date = LocalDateTime.of(2022, 5, 15, 10, 30);
        String browserTZ = "America/New_York";
        String tenantTZ = "";
        boolean enableTimeZone = true;
        LocalDateTime expected = LocalDateTime.of(2022, 5, 15, 6, 30); // America/New_York equivalent of 2022-05-15T10:30 in UTC
        LocalDateTime actual = DateUtils.convertDateToUserTimeZone(date, browserTZ, tenantTZ, enableTimeZone);
        assertEquals(expected, actual);
    }

    @Test
    void testConvertDateToUserTimeZone_NoTimeZoneEnabled() {
        LocalDateTime date = LocalDateTime.of(2022, 5, 15, 10, 30);
        String browserTZ = "";
        String tenantTZ = "";
        boolean enableTimeZone = false;
        LocalDateTime expected = date; // No conversion, date remains the same
        LocalDateTime actual = DateUtils.convertDateToUserTimeZone(date, browserTZ, tenantTZ, enableTimeZone);
        assertEquals(expected, actual);
    }
}
