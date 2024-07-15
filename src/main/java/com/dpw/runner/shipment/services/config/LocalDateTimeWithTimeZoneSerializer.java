package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

@Component
@Generated
public class LocalDateTimeWithTimeZoneSerializer extends JsonSerializer<LocalDateTime> {

    @Override
    public void serialize(LocalDateTime value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
        CommonUtils commonUtils = SpringContext.getBean(CommonUtils.class);
        LocalDateTime localDateTime = LocalTimeZoneHelper.getDateTime(value);
        String dateFormat = commonUtils.getCurrentTenantSettings() != null ? commonUtils.getCurrentTenantSettings().getDPWDateFormat() : "dd/MM/yyyy";
        gen.writeString(localDateTime.format(DateTimeFormatter.ofPattern(dateFormat)));
    }
}
