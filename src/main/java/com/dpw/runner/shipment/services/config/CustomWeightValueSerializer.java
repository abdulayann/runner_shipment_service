package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.utils.ContextUtility;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Configurable;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;

@Configurable
public class CustomWeightValueSerializer extends JsonSerializer<BigDecimal> {

    @Autowired
    private ContextUtility contextUtility;

    @Override
    public void serialize(BigDecimal bigDecimal, JsonGenerator jsonGenerator, SerializerProvider serializerProvider) throws IOException {
        Integer weightDecimalValue = contextUtility.shipmentSettingsDetailsContext.getCurrentTenantSettings().getWeightDecimalPlace();
        // Set default value
        if(weightDecimalValue == null)
            weightDecimalValue = 2;
        bigDecimal = bigDecimal.setScale(weightDecimalValue, RoundingMode.HALF_UP);
        jsonGenerator.writeNumber(bigDecimal);
    }
}
