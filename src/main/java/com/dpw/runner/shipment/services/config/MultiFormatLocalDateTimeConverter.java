package com.dpw.runner.shipment.services.config;

import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

@Component
public class MultiFormatLocalDateTimeConverter implements Converter<String, LocalDateTime> {

    private final List<DateTimeFormatter> formatters = new ArrayList<>();

    public MultiFormatLocalDateTimeConverter() {
        // Add multiple date formats that you want to support
        formatters.add(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        formatters.add(DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss"));
        formatters.add(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss"));
        // Add more formats as needed
    }



    @Override

    public LocalDateTime convert(String source) {
        for (DateTimeFormatter formatter : formatters) {
            try {
                LocalDateTime dateTime = LocalDateTime.parse(source, formatter);
                // Assuming you want to convert to a fixed format
                DateTimeFormatter fixedFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
                return LocalDateTime.parse(dateTime.format(fixedFormatter), fixedFormatter);
            } catch (Exception ignored) {
                // Try the next format
            }
        }
        throw new IllegalArgumentException("Invalid date-time format");
    }

}
