package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.context.ApplicationContext;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;

@ExtendWith(MockitoExtension.class)
class CustomWeightValueSerializerTest {
    @Mock
    private CommonUtils commonUtils;

    @Mock
    private JsonGenerator jsonGenerator;

    @Mock
    private SerializerProvider serializerProvider;

    @InjectMocks
    private CustomWeightValueSerializer customWeightValueSerializer;

    @Mock
    private ApplicationContext applicationContext;

    @Test
    void testSerializeWithCommonUtils() throws IOException {
        BigDecimal input = new BigDecimal("200.111");
        SpringContext.setApplicationContext(applicationContext);
        Mockito.when(applicationContext.getBean(CommonUtils.class)).thenReturn(commonUtils);
        Mockito.when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().weightDecimalPlace(2).build());

        customWeightValueSerializer.serialize(input, jsonGenerator, serializerProvider);

        BigDecimal expected = input.setScale(2, RoundingMode.HALF_UP);
        Mockito.verify(jsonGenerator).writeNumber(expected);
    }

    @Test
    void testSerializeWithNullVolumeDecimalPlace() throws IOException {
        BigDecimal input = new BigDecimal("200.111");
        SpringContext.setApplicationContext(applicationContext);
        Mockito.when(applicationContext.getBean(CommonUtils.class)).thenReturn(commonUtils);
        Mockito.when(commonUtils.getShipmentSettingFromContext()).thenReturn(ShipmentSettingsDetails.builder().weightDecimalPlace(null).build());

        customWeightValueSerializer.serialize(input, jsonGenerator, serializerProvider);

        BigDecimal expected = input.setScale(2, RoundingMode.HALF_UP);
        Mockito.verify(jsonGenerator).writeNumber(expected);
    }
}
