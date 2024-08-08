package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IBillingServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.InvoiceSummaryRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillChargesFilterRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ChargeTypeFilterRequest;
import com.dpw.runner.shipment.services.dto.response.billing.BillBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingEntityResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingListResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummary;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummaryResponse;
import com.dpw.runner.shipment.services.dto.response.billing.ChargeTypeBaseResponse;
import com.dpw.runner.shipment.services.dto.v1.request.ShipmentBillingListRequest;
import com.dpw.runner.shipment.services.dto.v1.response.ShipmentBillingListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.ShipmentBillingListResponse.BillingData;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
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

    private static final String NULL_RESPONSE_ERROR = "Received null response from billing service or response data is null";

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
     * This method takes an BillingBulkSummaryRequest object, sends a POST request to the billing service to retrieve the summary, and maps the response to a BillingSummary object
     * if the response is not null.
     *
     * @param request the BillingBulkSummaryRequest containing module GUIDs and module type for which the billing summary is requested.
     * @return a BillingSummary object containing the billing summary information.
     */
    @Override
    public List<BillingSummary> fetchBillingBulkSummary(BillingBulkSummaryRequest request) {
        // Construct the URL for the billing bulk summary endpoint
        String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getBillingBulkSummary();
        log.info("Sending billing bulk summary request to URL: {}", url);

        // Create an HttpEntity object with the request payload and authentication headers
        HttpEntity<BillingBulkSummaryRequest> httpEntity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
        log.debug("Request payload: {}", request);

        try {
            // Send the POST request and get the response body
            log.info("Executing POST request...");
            ResponseEntity<BillingEntityResponse> responseEntity = restTemplate.postForEntity(url, httpEntity, BillingEntityResponse.class);

            // Check the response status and body
            BillingEntityResponse billingEntityResponse = responseEntity.getBody();
            if (responseEntity.getStatusCode().is2xxSuccessful() && billingEntityResponse != null) {
                log.info("Received billingEntityResponse from billing service");
                Map<String, Object> data = billingEntityResponse.getData();
                log.debug("Response data: {}", data);

                // Convert the billingSummary object to a List<Map<String, Object>>
                List<Map<String, Object>> billingSummaryListMap = (List<Map<String, Object>>) data.get("billingSummary");

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

    private <T, R> R executePostRequest(String url, HttpEntity<T> httpEntity, ParameterizedTypeReference<R> responseType) {
        log.info("Sending request to URL: {}", url);
        log.debug("Request payload: {}", httpEntity.getBody());

        try {
            log.info("Executing POST request...");
            ResponseEntity<R> responseEntity = restTemplate.exchange(url, HttpMethod.POST, httpEntity, responseType);
            R response = responseEntity.getBody();

            log.info("Received response with status: {}", responseEntity.getStatusCode());
            log.debug("Response body: {}", response);

            if (Objects.nonNull(response) && response instanceof BillingBaseResponse billingBaseResponse) {
                if (ObjectUtils.isNotEmpty(billingBaseResponse.getErrors())) {
                    String errorMsg = "Response contains errors: " + billingBaseResponse.getErrors().toString();
                    throw new BillingException(errorMsg);
                }
            } else {
                log.warn("Received null response from billing service");
            }

            return response;
        } catch (Exception e) {
            throw new BillingException("Error occurred while making a request to the billing service", e);
        }
    }

    @Override
    public List<BillChargesBaseResponse> fetchBillCharges(BillChargesFilterRequest request) {
        String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getBillChargesFilter();
        HttpEntity<BillChargesFilterRequest> httpEntity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
        ParameterizedTypeReference<BillingListResponse<BillChargesBaseResponse>> responseType = new ParameterizedTypeReference<>() {
        };
        BillingListResponse<BillChargesBaseResponse> billingListResponse = executePostRequest(url, httpEntity, responseType);
        if (billingListResponse == null || billingListResponse.getData() == null) {
            throw new BillingException(NULL_RESPONSE_ERROR);
        }

        Type listType = new TypeToken<List<BillChargesBaseResponse>>() {
        }.getType();
        return modelMapper.map(billingListResponse.getData(), listType);
    }

    @Override
    public BillBaseResponse fetchBill(BillRetrieveRequest request) {
        String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getGetBillByEntity();
        HttpEntity<BillRetrieveRequest> httpEntity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
        ParameterizedTypeReference<BillingEntityResponse> responseType = new ParameterizedTypeReference<>() {
        };
        BillingEntityResponse billingEntityResponse = executePostRequest(url, httpEntity, responseType);
        if (billingEntityResponse == null || billingEntityResponse.getData() == null) {
            throw new BillingException(NULL_RESPONSE_ERROR);
        }

        return modelMapper.map(billingEntityResponse.getData(), BillBaseResponse.class);
    }

    @Override
    public List<ChargeTypeBaseResponse> fetchChargeTypes(ChargeTypeFilterRequest request) {
        String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getChargeTypeFilter();
        HttpEntity<ChargeTypeFilterRequest> httpEntity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
        ParameterizedTypeReference<BillingListResponse<ChargeTypeBaseResponse>> responseType = new ParameterizedTypeReference<>() {
        };
        BillingListResponse<ChargeTypeBaseResponse> listResponse = executePostRequest(url, httpEntity, responseType);
        if (listResponse == null || listResponse.getData() == null) {
            throw new BillingException(NULL_RESPONSE_ERROR);
        }

        Type listType = new TypeToken<List<ChargeTypeBaseResponse>>() {
        }.getType();
        return modelMapper.map(listResponse.getData(), listType);
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

    @Override
    public ShipmentBillingListResponse fetchShipmentBillingData(ShipmentBillingListRequest request) {
            ShipmentBillingListResponse shipmentBillingListResponse = new ShipmentBillingListResponse();

            List<BillingSummary> billingSummaries = fetchBillingBulkSummary(BillingBulkSummaryRequest.builder()
                    .moduleGuids(request.getGuidsList().stream().map(UUID::toString).toList())
                    .moduleType(Constants.SHIPMENT).build());

            Map<String, BillingSummary> billingSummaryMap = billingSummaries.stream()
                    .collect(Collectors.toMap(BillingSummary::getModuleGuid, summary -> summary));

            billingSummaryMap.forEach((shipmentGuid, v2BillingData) -> {
                BillingData v1BillingData = new BillingData();
                v1BillingData.setTotalEstimatedCost(v2BillingData.getTotalEstimatedCost());
                v1BillingData.setTotalEstimatedRevenue(v2BillingData.getTotalEstimatedRevenue());
                v1BillingData.setTotalEstimatedProfit(v2BillingData.getTotalEstimatedProfit());
                v1BillingData.setTotalEstimatedProfitPercent(v2BillingData.getTotalEstimatedProfitPercent());
                v1BillingData.setTotalCost(BigDecimal.valueOf(v2BillingData.getTotalCost()));
                v1BillingData.setTotalRevenue(BigDecimal.valueOf(v2BillingData.getTotalRevenue()));
                v1BillingData.setTotalProfit(v2BillingData.getTotalProfit());
                v1BillingData.setTotalProfitPercent(v2BillingData.getTotalProfitPercent());
                v1BillingData.setTotalPostedCost(v2BillingData.getTotalPostedCost());
                v1BillingData.setTotalPostedRevenue(v2BillingData.getTotalPostedRevenue());
                v1BillingData.setTotalPostedProfit(v2BillingData.getTotalPostedProfit());
                v1BillingData.setTotalPostedProfitPercent(v2BillingData.getTotalPostedProfitPercent());
                v1BillingData.setId(null); // TODO: SUBHAM fetch id of Shipment

                Map<String, BillingData> data = Optional.ofNullable(shipmentBillingListResponse.getData()).orElseGet(HashMap::new);
                data.put(shipmentGuid, v1BillingData);
                shipmentBillingListResponse.setData(data);
            });
            return shipmentBillingListResponse;


    }
}
