package com.dpw.runner.shipment.services.kafka.serializer;

import com.dpw.runner.shipment.services.utils.Generated;
import org.apache.commons.lang.SerializationException;
import org.apache.kafka.common.serialization.Serializer;

import java.io.Serializable;

@Generated
public class ProducerSerializer<T extends Serializable> implements Serializer<T> {

    @Override
    public byte[] serialize(String topic, T data) {
        if (data == null) {
            return new byte[0];
        }
        try {
            return data.toString().getBytes();
        } catch (Exception e) {
            throw new SerializationException("Error serializing value", e);
        }
    }
}
