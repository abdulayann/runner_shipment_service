package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.utils.ExcludeTimeZone;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.introspect.AnnotatedMember;
import com.fasterxml.jackson.databind.ser.BeanPropertyWriter;

import java.io.IOException;
import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;

public class LocalDateTimeWithTimeZoneSerializer extends JsonSerializer<LocalDateTime> {

    private final ZoneId targetTimeZone;
    private final DateTimeFormatter formatter;

    public LocalDateTimeWithTimeZoneSerializer(ZoneId targetTimeZone, DateTimeFormatter formatter) {
        this.targetTimeZone = targetTimeZone;
        this.formatter = formatter;
    }

    @Override
    public void serialize(LocalDateTime value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
        ZonedDateTime zonedDateTime = value.atZone(targetTimeZone);
        BeanPropertyWriter writer = getBeanPropertyWriter(serializers);

        if (writer != null) {
            // Access annotations on the field
            AnnotatedMember annotatedMember = writer.getMember();

            // Check if a specific annotation is present
            boolean isMyCustomAnnotationPresent = annotatedMember.hasAnnotation(ExcludeTimeZone.class);

            if (!isMyCustomAnnotationPresent) {
                gen.writeString(formatter.format(zonedDateTime));
            }
            else
                gen.writeString(formatter.format(value));
        }
    }

    private BeanPropertyWriter getBeanPropertyWriter(SerializerProvider provider) {
        try {
            Field field = provider.getClass().getDeclaredField("_defaultGenerator");
            field.setAccessible(true);
            Object defaultGenerator = field.get(provider);

            Field field2 = defaultGenerator.getClass().getDeclaredField("_currentBean");
            field2.setAccessible(true);
            Object currentBean = field2.get(defaultGenerator);

            Field field3 = currentBean.getClass().getDeclaredField("_beanProperties");
            field3.setAccessible(true);
            List<BeanPropertyWriter> beanPropertyWriters = (List<BeanPropertyWriter>) field3.get(currentBean);

            // Assuming you want the first property (modify as needed)
            return beanPropertyWriters.get(0);
        } catch (Exception e) {
            throw new RuntimeException("Error getting BeanPropertyWriter", e);
        }
    }
}
