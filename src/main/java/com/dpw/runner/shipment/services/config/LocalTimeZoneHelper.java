package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.utils.DateUtils;
import org.slf4j.MDC;

import java.time.LocalDateTime;

public class LocalTimeZoneHelper {
    public static LocalDateTime getDateTime(LocalDateTime value) {
        String timeZone = MDC.get("x-browser-time-zone");
        if(timeZone == null)
            timeZone = "UTC";
        UsersDto userDetails = UserContext.getUser();
        Boolean enableTimeZoneFlag = userDetails.getEnableTimeZone();
        String tenantTimeZone = userDetails.getTimeZoneId();
        String targetTimeZone = timeZone;
        if(enableTimeZoneFlag && tenantTimeZone != null)
            targetTimeZone = tenantTimeZone;
        return DateUtils.convertDateToUserTimeZone(value, timeZone, tenantTimeZone, enableTimeZoneFlag);
    }
}
