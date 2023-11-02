package com.dpw.runner.shipment.services.config;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.TextNode;
import org.springframework.boot.jackson.JsonComponent;

import java.io.IOException;
import java.time.LocalTime;

@JsonComponent
public class CustomLocalTimeDeserializer extends JsonDeserializer<LocalTime> {
    @Override
    public LocalTime deserialize(JsonParser jsonParser, DeserializationContext deserializationContext) throws IOException {
        JsonNode node = jsonParser.getCodec().readTree(jsonParser);
        if(node == null) {
            return null;
        }
        try {
            int hour = node.get("hour").asInt();
            int minute = node.get("minute").asInt();
            int second = node.get("second").asInt();
            int nano = node.get("nano").asInt();
            return LocalTime.of(hour, minute, second, nano);
        } catch (Exception e) {
            try {
                return LocalTime.parse(((TextNode) node).textValue());
            } catch (Exception e1) {
                throw new IllegalArgumentException("Invalid duration format");
            }
        }
    }
}
