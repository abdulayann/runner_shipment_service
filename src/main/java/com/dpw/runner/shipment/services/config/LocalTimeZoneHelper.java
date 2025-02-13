package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.utils.DateUtils;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.dpw.runner.shipment.services.utils.Generated;
import org.slf4j.MDC;

import java.lang.reflect.Field;
import java.time.LocalDateTime;

@Generated
public class LocalTimeZoneHelper {

    private LocalTimeZoneHelper() {
    }

    public static LocalDateTime getDateTime(LocalDateTime value) {
        String timeZone = MDC.get("x-browser-time-zone");
        if (timeZone == null)
            timeZone = "UTC";
        UsersDto userDetails = UserContext.getUser();
        Boolean enableTimeZoneFlag = userDetails.getEnableTimeZone();
        String tenantTimeZone = userDetails.getTimeZoneId();
        return DateUtils.convertDateToUserTimeZone(value, timeZone, tenantTimeZone, enableTimeZoneFlag);
    }

    public static LocalDateTime getDateTimeFromUserTimeZone(LocalDateTime value) {
        String timeZone = MDC.get("x-browser-time-zone");
        if (timeZone == null)
            timeZone = "UTC";
        UsersDto userDetails = UserContext.getUser();
        Boolean enableTimeZoneFlag = userDetails.getEnableTimeZone();
        String tenantTimeZone = userDetails.getTimeZoneId();
        return DateUtils.convertDateFromUserTimeZone(value, timeZone, tenantTimeZone, enableTimeZoneFlag);
    }

    public static void transformTimeZone(Object obj) throws IllegalAccessException {
        if (obj == null)
            return;
        Class<?> clazz = obj.getClass();
        Field[] fields = clazz.getDeclaredFields();

        for (Field field : fields) {
            field.setAccessible(true);
            if (field.isAnnotationPresent(ExcludeTimeZone.class))
                continue;
            if (field.getType().equals(LocalDateTime.class)) {
                try {
                    LocalDateTime value = (LocalDateTime) field.get(obj);
                    if (value != null)
                        field.set(obj, getDateTime(value));
                } catch (IllegalAccessException e) {
                    e.printStackTrace();
                }
            } else if (!(field.get(obj) instanceof Enum<?>) && field.get(obj) != null && field.get(obj).getClass().getName().startsWith("com.dpw")) {
                transformTimeZone(field.get(obj));
            }
        }
    }
}
