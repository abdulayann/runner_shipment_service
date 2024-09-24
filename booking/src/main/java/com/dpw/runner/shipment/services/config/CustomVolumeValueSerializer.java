package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.utils.CommonUtils;
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
public class CustomVolumeValueSerializer extends JsonSerializer<BigDecimal> {

    @Override
    public void serialize(BigDecimal bigDecimal, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
        CommonUtils commonUtils = SpringContext.getBean(CommonUtils.class);
        Integer volumeDecimalValue = commonUtils.getCurrentTenantSettings().getVolumeDecimalPlace();
        // Set default value
        if(volumeDecimalValue == null)
            volumeDecimalValue = 3;
        bigDecimal = bigDecimal.setScale(volumeDecimalValue, RoundingMode.HALF_UP);
        jsonGenerator.writeNumber(bigDecimal);
    }
}
