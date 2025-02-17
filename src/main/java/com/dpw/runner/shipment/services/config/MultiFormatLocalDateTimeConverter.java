package com.dpw.runner.shipment.services.config;

import com.dpw.runner.shipment.services.utils.Generated;
import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

@Component
@Generated
public class MultiFormatLocalDateTimeConverter implements Converter<String, LocalDateTime> {

    public static final String YYYY_MM_DD_HH_MM_SS = "yyyy-MM-dd HH:mm:ss";
    private final List<DateTimeFormatter> formatters = new ArrayList<>();


    public MultiFormatLocalDateTimeConverter() {
        // Add multiple date formats that you want to support
        formatters.add(DateTimeFormatter.ofPattern(YYYY_MM_DD_HH_MM_SS));
        formatters.add(DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss"));
        formatters.add(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss"));
        formatters.add(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS"));
        formatters.add(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX"));
        formatters.add(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSX"));
        formatters.add(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSXXX"));
        formatters.add(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSSXXX"));
        formatters.add(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSz"));
        formatters.add(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'"));
        formatters.add(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSX"));
        // Add more formats as needed
    }


    @Override

    public LocalDateTime convert(String source) {
        for (DateTimeFormatter formatter : formatters) {
            try {
                LocalDateTime dateTime = LocalDateTime.parse(source, formatter);
                // Assuming you want to convert to a fixed format
                DateTimeFormatter fixedFormatter = DateTimeFormatter.ofPattern(YYYY_MM_DD_HH_MM_SS);
                return LocalDateTime.parse(dateTime.format(fixedFormatter), fixedFormatter);
            } catch (Exception ignored) {
                // Try the next format
            }
        }
        // try with default
        try {
            LocalDateTime dateTime = LocalDateTime.parse(source);
            // Assuming you want to convert to a fixed format
            DateTimeFormatter fixedFormatter = DateTimeFormatter.ofPattern(YYYY_MM_DD_HH_MM_SS);
            return LocalDateTime.parse(dateTime.format(fixedFormatter), fixedFormatter);
        } catch (Exception ignored) {
            // Try the next format
        }

        throw new IllegalArgumentException("Invalid date-time format");
    }

}
