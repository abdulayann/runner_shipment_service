package com.dpw.runner.booking.services.config;

import com.dpw.runner.booking.services.utils.CommonUtils;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Configurable;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;

@Configurable
@Slf4j
public class CustomWeightValueSerializer extends JsonSerializer<BigDecimal> {

    @Override
    public void serialize(BigDecimal bigDecimal, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
        CommonUtils commonUtils = SpringContext.getBean(CommonUtils.class);
        Integer weightDecimalValue = commonUtils.getCurrentTenantSettings().getWeightDecimalPlace();
        // Set default value
        if(weightDecimalValue == null)
            weightDecimalValue = 2;
        bigDecimal = bigDecimal.setScale(weightDecimalValue, RoundingMode.HALF_UP);
        jsonGenerator.writeNumber(bigDecimal);
    }
}
