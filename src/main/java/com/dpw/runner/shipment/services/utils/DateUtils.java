package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;


public class DateUtils {
    public static String getDateAsString(LocalDateTime dateTime){
        DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(Constants.DEFAULT_DATE_FORMAT);
        String formattedDateTime = dateTime.format(dateTimeFormatter);
        return formattedDateTime;
    }
}
