package com.dpw.runner.shipment.services.ReportingService.CommonUtils;

import com.dpw.runner.shipment.services.ReportingService.Enums.DigitGrouping;
import com.dpw.runner.shipment.services.ReportingService.Enums.GroupingNumber;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.Locale;


public class AmountNumberFormatter {

    public static String Format(BigDecimal amount, String localCurrency, TenantModel tenantSettings) {
        String formattedAmount = null;

        if (amount != null) {

            var user = UserContext.getUser();
            int numberDecimalDigits = 2;

            if (tenantSettings.RoundoffLocalCurrencyAmount != null
                    && tenantSettings.RoundoffLocalCurrencyAmount && localCurrency.equals(user.CompanyCurrency)) {
                numberDecimalDigits = 0;
            }

            if (tenantSettings.IsGroupingOverseas != null && !tenantSettings.IsGroupingOverseas
                    && !localCurrency.equals(user.CompanyCurrency)) {
                formattedAmount = ReportHelper.addCommasWithPrecision(amount, numberDecimalDigits);
            } else {
                formattedAmount = displayFormat(amount, numberDecimalDigits, tenantSettings);
            }
        }

        return formattedAmount;
    }

    public static String displayFormat(BigDecimal amount, int numberDecimalDigits, TenantModel tenantSettings) {
        if (amount != null) {

            if (tenantSettings.CurrencyDigitGrouping != null && tenantSettings.CurrencyGroupingNumber != null) {
                char customThousandsSeparator = ',';
                char customDecimalSeparator = '.';

                if (tenantSettings.CurrencyGroupingNumber.equals(GroupingNumber.DotAndComma)) {
                    customThousandsSeparator = '.';
                    customDecimalSeparator = ',';
                }

                Locale customLocale = new Locale("en", "US");

                DecimalFormatSymbols customSymbols = new DecimalFormatSymbols(customLocale);
                customSymbols.setDecimalSeparator(customDecimalSeparator);
                customSymbols.setGroupingSeparator(customThousandsSeparator);

                StringBuilder patternBuilder = new StringBuilder("#,##0");
                if (numberDecimalDigits > 0) {
                    patternBuilder.append('.');
                    for (int i = 0; i < numberDecimalDigits; i++) {
                        patternBuilder.append('0');
                    }
                }

                DecimalFormat customFormat = new DecimalFormat(patternBuilder.toString(), customSymbols);

                NumberFormat numberInstance = NumberFormat.getNumberInstance(customLocale);
                if (numberInstance instanceof DecimalFormat) {
                    ((DecimalFormat) numberInstance).applyPattern(patternBuilder.toString());
                }

                return amount != null ? numberInstance.format(amount) : null;
            } else {
                return ReportHelper.addCommasWithPrecision(amount, numberDecimalDigits);
            }
        }

        return null;
    }
}
