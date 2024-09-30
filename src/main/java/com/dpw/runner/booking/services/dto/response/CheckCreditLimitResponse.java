package com.dpw.runner.booking.services.dto.response;

import com.dpw.runner.booking.services.commons.responses.IRunnerResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CheckCreditLimitResponse implements IRunnerResponse {
    private double totalCreditLimit;
    private String currency;
    private double outstandingAmount;
    private double notDueAmount;
    private double overdueAmount;
    private double totalCreditAvailableBalance;
    private double creditLimitUtilizedPer;
    private double overduePer;
    private String paymentTerms;
    private String siteNumber;
    private String accountNumber;
}
