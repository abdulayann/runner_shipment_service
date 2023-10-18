package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.timeZone.utility.RunnerTimeZoneConvertor;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;


public class DateUtils {
    public static String getDateAsString(LocalDateTime dateTime){
        DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(Constants.DEFAULT_DATE_FORMAT);
        String formattedDateTime = dateTime.format(dateTimeFormatter);
        return formattedDateTime;
    }

    public static LocalDateTime convertDateFromUserTimeZone(LocalDateTime date, String browserTZ, String tenantTZ, boolean enableTimeZone) {
        if (date == null) return null;

        LocalDateTime convertedUserDate = null;
        if (BooleanUtils.toBoolean(enableTimeZone)
                && StringUtils.isNotBlank(tenantTZ)) {
            convertedUserDate = RunnerTimeZoneConvertor.convertToUTC(date, tenantTZ);
        } else {
            convertedUserDate = RunnerTimeZoneConvertor.convertToUTC(date, browserTZ);
        }
        return convertedUserDate;
    }

    public static LocalDateTime convertDateToUserTimeZone(LocalDateTime date, String browserTZ, String tenantTZ, boolean enableTimeZone) {
        if (date == null) return null;

        LocalDateTime convertedUserDate = null;
        if (BooleanUtils.toBoolean(enableTimeZone)
                && StringUtils.isNotBlank(tenantTZ)) {
            convertedUserDate = RunnerTimeZoneConvertor.convertFromUTC(date, tenantTZ);
        } else {
            convertedUserDate = RunnerTimeZoneConvertor.convertFromUTC(date, browserTZ);
        }
        return convertedUserDate;
    }
}
