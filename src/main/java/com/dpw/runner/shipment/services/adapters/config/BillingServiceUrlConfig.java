package com.dpw.runner.shipment.services.adapters.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "billing")
@Data
public class BillingServiceUrlConfig {

    private String baseUrl;
    private String xApiKey;
    private String getInvoiceData;
    private String externalCreateOrUpdate;
    private String chargeTypeFilter;
    private String billingBulkSummary;
    private String billingBulkSummaryBranchWise;
    private String billingBulkDueSummaryBranchWise;
    private String lastPostedInvoiceDate;
    private Boolean enableBillingIntegration;
    private String getBillByEntity;
    private String billChargesFilter;
}
