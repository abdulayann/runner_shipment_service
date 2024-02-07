package com.dpw.runner.shipment.services.ReportingService.CommonUtils;

import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;

import java.math.BigDecimal;
import java.util.Objects;

import static com.dpw.runner.shipment.services.ReportingService.Reports.IReport.DisplayFormat;


public class AmountNumberFormatter {

    public static String Format(BigDecimal amount, String localCurrency, V1TenantSettingsResponse tenantSettings) {
        String formattedAmount = null;

        if (amount != null) {

            var user = UserContext.getUser();
            int numberDecimalDigits = 2;

            if (tenantSettings.getRoundoffLocalCurrencyAmount() != null
                    && tenantSettings.getRoundoffLocalCurrencyAmount() && localCurrency.equals(user.CompanyCurrency)) {
                numberDecimalDigits = 0;
            }

            if (tenantSettings.getIsGroupingOverseas() != null && !tenantSettings.getIsGroupingOverseas()
                    && !localCurrency.equals(user.CompanyCurrency)) {
                formattedAmount = ReportHelper.addCommasWithPrecision(amount, numberDecimalDigits);
            } else {
                formattedAmount = displayFormat(amount, numberDecimalDigits, tenantSettings);
            }
        }

        return formattedAmount;
    }

    public static String displayFormat(BigDecimal amount, int numberDecimalDigits, V1TenantSettingsResponse tenantSettings) {
        if (amount != null) {
            return DisplayFormat(amount, numberDecimalDigits, tenantSettings);
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
}
