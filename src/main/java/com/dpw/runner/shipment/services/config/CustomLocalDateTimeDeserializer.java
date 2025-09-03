package com.dpw.runner.shipment.services.config;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import org.springframework.boot.jackson.JsonComponent;
import java.io.IOException;
import java.time.LocalDateTime;

@JsonComponent
public class CustomLocalDateTimeDeserializer extends JsonDeserializer<LocalDateTime> {

    private MultiFormatLocalDateTimeConverter multiFormatLocalDateTimeConverter = new MultiFormatLocalDateTimeConverter();

    @Override
    public LocalDateTime deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
        String dateTimeString = jsonParser.getValueAsString();
        if (dateTimeString == null || dateTimeString.trim().isEmpty()) {
            return null;
        }
        return multiFormatLocalDateTimeConverter.convert(dateTimeString);
    }

}
