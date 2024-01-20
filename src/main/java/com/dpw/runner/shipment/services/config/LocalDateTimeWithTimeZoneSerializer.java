package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.introspect.AnnotatedMember;
import com.fasterxml.jackson.databind.ser.BeanPropertyWriter;
import org.slf4j.MDC;

import java.io.IOException;
import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

public class LocalDateTimeWithTimeZoneSerializer extends JsonSerializer<LocalDateTime> {

    @Override
    public void serialize(LocalDateTime value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
        String timeZone = MDC.get("x-browser-time-zone");
        if(timeZone == null)
            timeZone = "UTC";
        UsersDto userDetails = UserContext.getUser();
        Boolean enableTimeZoneFlag = userDetails.getEnableTimeZone();
        String tenantTimeZone = userDetails.getTimeZoneId();
        String targetTimeZone = timeZone;
        if(enableTimeZoneFlag && tenantTimeZone != null)
            targetTimeZone = tenantTimeZone;
        ZonedDateTime zonedDateTime = value.atZone(ZoneId.of(targetTimeZone));
        gen.writeString(String.valueOf(zonedDateTime));
    }
}
