package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.utils.Generated;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.TextNode;
import org.springframework.boot.jackson.JsonComponent;

import java.io.IOException;
import java.time.LocalTime;

@JsonComponent
@Generated
public class CustomLocalTimeDeserializer extends JsonDeserializer<LocalTime> {
    @Override
    public LocalTime deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
        JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        if (node == null || (node.isObject() && node.isEmpty())) {
            return null;
        }
        try {
            if ((node.get(Constants.HOUR) == null || node.get(Constants.HOUR).isNull()) &&
                    (node.get(Constants.MINUTE) == null || node.get(Constants.MINUTE).isNull()) &&
                    (node.get(Constants.SECOND) == null || node.get(Constants.SECOND).isNull()))
                return null;
            int hour = node.get(Constants.HOUR) == null ? 0 : node.get(Constants.HOUR).asInt();
            int minute = node.get(Constants.MINUTE) == null ? 0 : node.get(Constants.MINUTE).asInt();
            int second = node.get(Constants.SECOND) == null ? 0 : node.get(Constants.SECOND).asInt();
            return LocalTime.of(hour, minute, second);
        } catch (Exception e) {
            try {
                return LocalTime.parse(node.textValue());
            } catch (Exception e1) {
                throw new IllegalArgumentException("Invalid duration format");
            }
        }
    }
}
