package com.dpw.runner.shipment.services.ReportingService.CommonUtils;

import com.dpw.runner.shipment.services.ReportingService.Enums.DigitGrouping;
import com.dpw.runner.shipment.services.ReportingService.Enums.GroupingNumber;
import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.Locale;

import static com.dpw.runner.shipment.services.ReportingService.Reports.IReport.GetDPWWeightVolumeFormat;


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
            return GetDPWWeightVolumeFormat(amount, numberDecimalDigits, tenantSettings);
        }
        return null;
    }
}
