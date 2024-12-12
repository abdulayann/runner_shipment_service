package com.dpw.runner.shipment.services.config;

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
        if(node == null || (node.isObject() && node.isEmpty())) {
            return null;
        }
        try {
            if((node.get("hour") == null || node.get("hour").isNull()) &&
                    (node.get("minute") == null || node.get("minute").isNull()) &&
                    (node.get("second") == null || node.get("second").isNull()))
                return null;
            int hour = node.get("hour") == null ? 0 : node.get("hour").asInt();
            int minute = node.get("minute") == null ? 0 : node.get("minute").asInt();
            int second = node.get("second") == null ? 0 : node.get("second").asInt();
            return LocalTime.of(hour, minute, second);
        } catch (Exception e) {
            try {
                return LocalTime.parse(((TextNode) node).textValue());
            } catch (Exception e1) {
                throw new IllegalArgumentException("Invalid duration format");
            }
        }
    }
}
