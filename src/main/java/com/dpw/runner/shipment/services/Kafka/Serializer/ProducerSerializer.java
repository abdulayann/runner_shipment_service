package com.dpw.runner.shipment.services.Kafka.Serializer;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang.SerializationException;
import org.apache.kafka.common.serialization.Serializer;

import java.io.Serializable;
import java.util.Map;

public class ProducerSerializer <T extends Serializable> implements Serializer<T> {

    private final ObjectMapper objectMapper = new ObjectMapper();
    private boolean isKey;

    public ProducerSerializer() {

    }

    @Override
    public void configure(Map<String, ?> configs, boolean isKey) {
        this.isKey = isKey;
    }

    @Override
    public byte[] serialize(String topic, T data) {
        if (data == null) {
            return null;
        }
        try {

            return data.toString().getBytes();

        } catch (Exception e) {

            throw new SerializationException("Error serializing value", e);
        }
    }

    @Override
    public void close() {
    }

}
