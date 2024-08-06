package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IBillingServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.InvoiceSummaryRequest;
import com.dpw.runner.shipment.services.dto.request.billing.LastPostedInvoiceDateRequest;
import com.dpw.runner.shipment.services.dto.response.billing.BillingEntityResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummary;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummaryResponse;
import com.dpw.runner.shipment.services.dto.response.billing.LastPostedInvoiceDateResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import java.time.LocalDateTime;
import java.util.Objects;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
@Slf4j
public class BillingServiceAdapter implements IBillingServiceAdapter {

    @Autowired
    private RestTemplate restTemplate;

    @Autowired
    private ModelMapper modelMapper;

    @Value("${billing.baseUrl}")
    private String billingBaseUrl;

    @Value("${billing.getInvoiceData}")
    private String getInvoiceData;

    @Autowired
    private BillingServiceUrlConfig billingServiceUrlConfig;

    @Override
    public Boolean fetchActiveInvoices(CommonGetRequest request) throws RunnerException {

        InvoiceSummaryRequest invoiceSummaryRequest = new InvoiceSummaryRequest();
        invoiceSummaryRequest.setModuleType(Constants.SHIPMENT);
        invoiceSummaryRequest.setModuleGuid(request.getGuid());

        String url = billingBaseUrl + getInvoiceData;

        HttpEntity<InvoiceSummaryRequest> httpEntity = new HttpEntity<>(invoiceSummaryRequest, V1AuthHelper.getHeaders());
        var response = this.restTemplate.postForEntity(url, httpEntity, BillingSummaryResponse.class).getBody();
        BillingSummary billingSummary = new BillingSummary();
        if (Objects.nonNull(response)) {
            billingSummary = modelMapper.map(response.getData(), BillingSummary.class);
        }

        return checkActiveCharges(billingSummary);
    }

    /**
     * Fetches the last posted invoice date based on the provided invoice summary request.
     *
     * @param request the LastPostedInvoiceDateRequest request object
     * @return the last posted invoice date, or null if not found
     * @throws BillingException if there is an error during the request
     */
    @Override
    public LocalDateTime fetchLastPostedInvoiceDate(LastPostedInvoiceDateRequest request) {
        LocalDateTime lastPostedInvoiceDate = null;

        // Construct the URL for the last posted invoice date endpoint
        String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getLastPostedInvoiceDate();
        log.info("Sending last posted invoice date request to URL: {}", url);

        // Create an HttpEntity object with the request payload and authentication headers
        HttpEntity<LastPostedInvoiceDateRequest> httpEntity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
        log.debug("Request payload: {}", request);

        try {
            // Execute the POST request and get the response entity
            log.info("Executing POST request...");
            ResponseEntity<BillingEntityResponse> responseEntity = restTemplate.postForEntity(url, httpEntity, BillingEntityResponse.class);
            BillingEntityResponse billingEntityResponse = responseEntity.getBody();

            // Log the response status and body
            log.info("Received response with status: {}", responseEntity.getStatusCode());
            log.debug("Response body: {}", billingEntityResponse);

            // Check if the response is not null and contains no errors
            if (Objects.nonNull(billingEntityResponse)) {
                if (ObjectUtils.isNotEmpty(billingEntityResponse.getErrors())) {
                    String errorMsg = "Bill creation response contains errors: " + billingEntityResponse.getErrors().toString();
                    throw new BillingException(errorMsg);
                }

                // Map the response data to LastPostedInvoiceDateResponse and get the last posted invoice date
                LastPostedInvoiceDateResponse lastPostedInvoiceDateResponse = modelMapper.map(billingEntityResponse.getData(), LastPostedInvoiceDateResponse.class);
                lastPostedInvoiceDate = lastPostedInvoiceDateResponse.getLastPostedInvoiceDate();
            } else {
                log.warn("Received null response from billing service");
            }

        } catch (Exception e) {
            throw new BillingException("Error occurred while fetching last posted invoice date", e);
        }

        return lastPostedInvoiceDate;
    }

    private Boolean checkActiveCharges(BillingSummary billingSummary) {
        return (!Objects.equals(null, billingSummary.getTotalCount()) && !Objects.equals(0, billingSummary.getTotalCount())) ||
                (!Objects.equals(null, billingSummary.getTotalRevenue()) && Double.compare(billingSummary.getTotalRevenue(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getTotalCost()) && Double.compare(billingSummary.getTotalCost(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getAccruedRevenue()) && Double.compare(billingSummary.getAccruedRevenue(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getAccruedCost()) && Double.compare(billingSummary.getAccruedCost(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getInvoicedRevenue()) && Double.compare(billingSummary.getInvoicedRevenue(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getInvoicedCost()) && Double.compare(billingSummary.getInvoicedCost(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getDisbursementAccruedRevenue()) && Double.compare(billingSummary.getDisbursementAccruedRevenue(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getDisbursementAccruedCost()) && Double.compare(billingSummary.getDisbursementAccruedCost(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getDisbursementInvoicedRevenue()) && Double.compare(billingSummary.getDisbursementInvoicedRevenue(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getDisbursementInvoicedCost()) && Double.compare(billingSummary.getDisbursementInvoicedCost(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getDisbursementRevenue()) && Double.compare(billingSummary.getDisbursementRevenue(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getDisbursementCost()) && Double.compare(billingSummary.getDisbursementCost(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getCumulativeGP()) && Double.compare(billingSummary.getCumulativeGP(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getCumulativeGPPercentage()) && Double.compare(billingSummary.getCumulativeGPPercentage(), 0.0) > 0);
    }
}
