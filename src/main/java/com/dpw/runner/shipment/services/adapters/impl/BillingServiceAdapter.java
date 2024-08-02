package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IBillingServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.InvoiceBulkSummaryRequest;
import com.dpw.runner.shipment.services.dto.request.InvoiceSummaryRequest;
import com.dpw.runner.shipment.services.dto.response.billing.BillingEntityResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummary;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummaryResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
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
     * Fetches the billing summary for a bulk set of modules.
     * <p>
     * This method takes an InvoiceBulkSummaryRequest object, sends a POST request to the billing service to retrieve the summary, and maps the response to a BillingSummary object
     * if the response is not null.
     *
     * @param request the InvoiceBulkSummaryRequest containing module GUIDs and module type for which the billing summary is requested.
     * @return a BillingSummary object containing the billing summary information.
     */
    @Override
    public List<BillingSummary> fetchBillingBulkSummary(InvoiceBulkSummaryRequest request) {
        // Construct the URL for the billing bulk summary endpoint
        String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getBillingBulkSummary();
        log.info("Sending billing bulk summary request to URL: {}", url);

        // Create an HttpEntity object with the request payload and authentication headers
        HttpEntity<InvoiceBulkSummaryRequest> httpEntity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
        log.debug("Request payload: {}", request);

        try {
            // Send the POST request and get the response body
            log.info("Executing POST request...");
            ResponseEntity<BillingEntityResponse> responseEntity = restTemplate.postForEntity(url, httpEntity, BillingEntityResponse.class);

            // Check the response status and body
            BillingEntityResponse billingEntityResponse = responseEntity.getBody();
            if (responseEntity.getStatusCode().is2xxSuccessful() && billingEntityResponse != null) {
                log.info("Received billingEntityResponse from billing service");
                log.debug("Response data: {}", billingEntityResponse.getData());

                // Convert the billingSummary object to a List<Map<String, Object>>
                List<Map<String, Object>> billingSummaryListMap = (List<Map<String, Object>>) billingEntityResponse.getData().get("billingSummary");

                // Map the list of maps to a list of BillingSummary objects
                return modelMapper.map(billingSummaryListMap, new TypeToken<List<BillingSummary>>() {}.getType());
            } else {
                log.warn("Received non-successful response from billing service: {}", responseEntity.getStatusCode());
                return List.of();
            }
        } catch (Exception e) {
            log.error("Error occurred while fetching billing bulk summary", e);
            throw new BillingException("Error occurred while fetching billing bulk summary", e);
        }
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
