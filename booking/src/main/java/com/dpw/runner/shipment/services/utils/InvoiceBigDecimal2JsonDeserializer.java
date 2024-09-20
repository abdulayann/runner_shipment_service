package com.dpw.runner.shipment.services.utils;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.deser.std.NumberDeserializers;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

@Component
public class InvoiceBigDecimal2JsonDeserializer extends NumberDeserializers.BigDecimalDeserializer {

    @Override
    public BigDecimal deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        BigDecimal value = super.deserialize(p, ctxt);

        // set scale
        if (ObjectUtils.isEmpty(value)) {
            return null;
        }
        value = value.setScale(5, RoundingMode.HALF_UP);

        return value;
    }
}
