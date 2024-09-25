package com.dpw.runner.shipment.services.commons.constants;

import java.time.LocalDateTime;

public class TimeZoneConstants {
    private TimeZoneConstants(){}
    public static final String DEFAULT_TIME_ZONE_ID = "UTC";
    public static final String BROWSER_TIME_ZONE_NAME = "x-browser-time-zone";
    public static final String TENANT_TIME_ZONE_NAME = "tenant-time-zone";
    public static final String ENABLE_TENANT_TIME_ZONE = "enable-tenant-time-zone";
    public static final LocalDateTime DEFAULT_DATE = LocalDateTime.of(1970, 1, 1, 00, 00, 00);
    public static final String SKIP_TIME_ZONE = "x-skip-time-zone";
}
