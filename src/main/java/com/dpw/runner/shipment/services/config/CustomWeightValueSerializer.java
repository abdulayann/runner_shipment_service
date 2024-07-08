package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Configurable;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;

@Configurable
@Slf4j
public class CustomWeightValueSerializer extends JsonSerializer<BigDecimal> {

    private CommonUtils commonUtils;

    @Autowired
    public CustomWeightValueSerializer(CommonUtils commonUtils) {
        this.commonUtils = commonUtils;
    }

    @Override
    public void serialize(BigDecimal bigDecimal, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
        if(commonUtils == null) {
            log.info("commonUtils is null in CustomWeightValueSerializer");
        }
        Integer weightDecimalValue = commonUtils == null ? null : commonUtils.getShipmentSettingFromContext().getWeightDecimalPlace();
        // Set default value
        if(weightDecimalValue == null)
            weightDecimalValue = 2;
        bigDecimal = bigDecimal.setScale(weightDecimalValue, RoundingMode.HALF_UP);
        jsonGenerator.writeNumber(bigDecimal);
    }
}
