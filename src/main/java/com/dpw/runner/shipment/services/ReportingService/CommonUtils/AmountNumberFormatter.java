package com.dpw.runner.shipment.services.ReportingService.CommonUtils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.enums.DigitGrouping;
import com.dpw.runner.shipment.services.entity.enums.GroupingNumber;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.Locale;
import java.util.Objects;

import static com.dpw.runner.shipment.services.ReportingService.Reports.IReport.DisplayFormat;
import static com.dpw.runner.shipment.services.ReportingService.Reports.IReport.formatValue;


public class AmountNumberFormatter {
    private AmountNumberFormatter(){}

    public static String Format(BigDecimal amount, String localCurrency, V1TenantSettingsResponse tenantSettings) {
        String formattedAmount = null;

        if (amount != null) {

            var user = UserContext.getUser();
            int numberDecimalDigits = 2;

            if (!Objects.isNull(tenantSettings.getCurrencyDecimalPlace()))
                numberDecimalDigits = tenantSettings.getCurrencyDecimalPlace();

            if (tenantSettings.getRoundoffLocalCurrencyAmount() != null
                    && tenantSettings.getRoundoffLocalCurrencyAmount() && Objects.equals(localCurrency, user.CompanyCurrency)) {
                numberDecimalDigits = 0;
            }

            if (tenantSettings.getIsGroupingOverseas() != null && !tenantSettings.getIsGroupingOverseas()
                    && !Objects.equals(localCurrency, user.CompanyCurrency)) {
                formattedAmount = ReportHelper.addCommasWithPrecision(amount, numberDecimalDigits);
            } else {
                formattedAmount = displayFormat(amount, numberDecimalDigits, tenantSettings);
            }
        }

        return formattedAmount;
    }

    public static String displayFormat(BigDecimal amount, int numberDecimalDigits, V1TenantSettingsResponse tenantSettings) {
        if (amount != null) {
            return DisplayFormat(amount, numberDecimalDigits, tenantSettings, false);
        }
        return null;
    }
    public static String formatWithoutDecimal(Object amount, String localCurrency, V1TenantSettingsResponse tenantSettings)
    {
        if (amount == null)
        {
            return null;
        }

        if (amount instanceof BigDecimal)
        {
            return formatWithoutDecimal((BigDecimal) amount, localCurrency, tenantSettings);
        }
        amount = amount.toString();
        if (amount != null)
        {
            try {
                BigDecimal parsedDecimal = new BigDecimal((String) amount);
                return formatWithoutDecimal(parsedDecimal, localCurrency, tenantSettings);
            } catch (NumberFormatException e) {
                return String.format("%,.0f", amount);
            }
        }

        return String.format("%,.0f", amount);
    }
    private static String formatWithoutDecimal(BigDecimal amount, String localCurrency, V1TenantSettingsResponse tenantSettings)
    {

        var user = UserContext.getUser();
        if (tenantSettings.getIsGroupingOverseas() != null && !tenantSettings.getIsGroupingOverseas() &&
                !Objects.equals(localCurrency, user.CompanyCurrency))
        {
            return ReportHelper.addCommasWithPrecision(amount, 0);
        }
        else
        {
            return displayFormat(amount, 0, tenantSettings);
        }
    }

    public static String FormatExchangeRate(BigDecimal amount, String localCurrency, V1TenantSettingsResponse tenantSettings) {
        if (amount == null)
            return null;
        NumberFormat customFormat = NumberFormat.getNumberInstance(Locale.US);
        DecimalFormat customDecimalFormat = (DecimalFormat) customFormat;
        customDecimalFormat.setDecimalSeparatorAlwaysShown(true);
        customDecimalFormat.setMaximumFractionDigits(29);
        customDecimalFormat.setMinimumFractionDigits(29);
        if (tenantSettings.getIsGroupingOverseas() != null && !tenantSettings.getIsGroupingOverseas()
                && !Objects.equals(localCurrency, UserContext.getUser().CompanyCurrency)) {
            return customFormat.format(amount);
        }
        if(tenantSettings.getCurrencyDigitGrouping() != null && tenantSettings.getCurrencyGroupingNumber() != null) {
            char customThousandsSeparator = ',';
            char customDecimalSeparator = '.';
            if(tenantSettings.getCurrencyGroupingNumber() != null && tenantSettings.getCurrencyGroupingNumber() == GroupingNumber.DotAndComma.getValue()) {
                customThousandsSeparator = '.';
                customDecimalSeparator = ',';
            }
            int dynamicGroupSizes = (tenantSettings.getCurrencyDigitGrouping() == DigitGrouping.THREE.getValue()) ? 3 : 2;
            return formatValue(amount, customDecimalSeparator, customThousandsSeparator, 29, dynamicGroupSizes);
        }
        return customFormat.format(amount);
    }

}
