package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.exception.exceptions.DoubleParseException;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Map;
import java.util.Objects;

public class ParseUtils {
    public static Double parseDoubleFromString(String number){
        try {
            return Double.parseDouble(number);
        } catch (Exception exception){
            throw new DoubleParseException(exception.getMessage());
        }
    }

    public static String getValueFromMap(String key, Map<String,String> map) {
        String res = null;
        if (Objects.nonNull(key) && Objects.nonNull(map) && !map.isEmpty() && map.containsKey(key))
            res = map.get(key);
        return res;
    }

    public static String getUTCTimeZone() {
        return LocalDateTime.now(ZoneId.of("UTC")).format(DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss"));
    }
}
