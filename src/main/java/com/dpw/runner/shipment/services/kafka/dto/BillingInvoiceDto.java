package com.dpw.runner.shipment.services.kafka.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import java.time.LocalDateTime;
import java.util.List;
import lombok.Data;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class BillingInvoiceDto {
    private String tenantCode;
    private InvoiceDto payload;

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class InvoiceDto {
        private AccountReceivableDto accountReceivable;

        @Data
        @JsonIgnoreProperties(ignoreUnknown = true)
        public static class AccountReceivableDto {
            private String id;
            private String invoiceNumber;
            private LocalDateTime invoiceDate;
            private String fusionInvoiceStatus;
            private String userDisplayName;
            private String userEmail;
            private String branchCode;
            private List<BillDto> bills;
            private List<BillChargeDto> billCharges;

            @Data
            @JsonIgnoreProperties(ignoreUnknown = true)
            public static class BillDto {
                private String moduleId;
                private String moduleTypeCode;
            }

            @Data
            @JsonIgnoreProperties(ignoreUnknown = true)
            public static class BillChargeDto {
                private String payableLocation;
            }
        }
    }
}
