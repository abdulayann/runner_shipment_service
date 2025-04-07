package com.dpw.runner.shipment.services.utils;

import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.timeZone.utility.RunnerTimeZoneConvertor;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;


public class DateUtils {
    private DateUtils(){}
    public static String getDateAsString(LocalDateTime dateTime){
        DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(Constants.DEFAULT_DATE_FORMAT);
        return dateTime.format(dateTimeFormatter);
    }

    public static LocalDateTime convertDateFromUserTimeZone(LocalDateTime date, String browserTZ, String tenantTZ, boolean enableTimeZone) {
        if (date == null) return null;

        LocalDateTime convertedUserDate = null;
        if (BooleanUtils.toBoolean(enableTimeZone)
                && StringUtils.isNotBlank(tenantTZ)) {
            convertedUserDate = RunnerTimeZoneConvertor.convertToUTC(date, tenantTZ);
        } else {
            if(StringUtils.isNotBlank(browserTZ))
                convertedUserDate = RunnerTimeZoneConvertor.convertToUTC(date, browserTZ);
            else
                convertedUserDate = date;
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
            if(StringUtils.isNotBlank(browserTZ))
                convertedUserDate = RunnerTimeZoneConvertor.convertFromUTC(date, browserTZ);
            else
                convertedUserDate = date;
        }
        return convertedUserDate;
    }
}
