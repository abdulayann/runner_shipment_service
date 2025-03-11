package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.ReportingService.Models.TenantModel;
import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IBillingServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1.BookingEntity;
import com.dpw.runner.shipment.services.dto.request.CreateBookingModuleInV1.BookingEntity.BillCharge;
import com.dpw.runner.shipment.services.dto.request.InvoiceSummaryRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillChargesFilterRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillRetrieveRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryBranchWiseRequest;
import com.dpw.runner.shipment.services.dto.request.billing.BillingBulkSummaryRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ChargeTypeFilterRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ExternalBillPayloadRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ExternalBillPayloadRequest.BillChargeCostDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ExternalBillPayloadRequest.BillChargeRevenueDetailsRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ExternalBillPayloadRequest.BillChargesRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ExternalBillPayloadRequest.BillRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ExternalBillPayloadRequest.ExternalBillChargeRequest;
import com.dpw.runner.shipment.services.dto.request.billing.ExternalBillPayloadRequest.ExternalBillConfiguration;
import com.dpw.runner.shipment.services.dto.request.billing.ExternalBillPayloadRequest.ExternalBillRequest;
import com.dpw.runner.shipment.services.dto.request.billing.LastPostedInvoiceDateRequest;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillChargesBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingBaseResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingDueSummary;
import com.dpw.runner.shipment.services.dto.response.billing.BillingEntityResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingListResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummary;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummaryResponse;
import com.dpw.runner.shipment.services.dto.response.billing.ChargeTypeBaseResponse;
import com.dpw.runner.shipment.services.dto.v1.request.ShipmentBillingListRequest;
import com.dpw.runner.shipment.services.dto.v1.response.ShipmentBillingListResponse;
import com.dpw.runner.shipment.services.dto.v1.response.ShipmentBillingListResponse.BillingData;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.enums.MeasurementBasis;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferAddress;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferOrganizations;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import com.dpw.runner.shipment.services.utils.V2AuthHelper;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
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
    @Autowired
    private V2AuthHelper v2AuthHelper;
    @Autowired
    private V1ServiceUtil v1ServiceUtil;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private CommonUtils commonUtils;

    private static final String NULL_RESPONSE_ERROR = "Received null/empty response from billing service or response data is null/empty";
    private static final String NO_ORG_FOUND_FOR = "No OrganizationsRow found for ";
    private static final String REQUEST_PAYLOAD = "Request payload: {}";
    private static final String EXECUTING_POST_REQUEST = "Executing POST request...";
    private static final String RESPONSE_CONTAINS_ERROR = "Response contains errors: ";
    private static final String BILLING_SUMMARY = "billingSummary";
    public static final String LOG_TIME_CONSUMED = "Request ID: {} | BILLING_API_CALL | API {} | Time taken: {} ms";

    @NotNull
    private static List<String> getClientCodeListForBillCreationRequest(BookingEntity entity) {
        List<String> clientCodeList = new ArrayList<>();
        if (ObjectUtils.isNotEmpty(entity.getClientCode())) {
            clientCodeList.addAll(Stream.of(entity.getClientCode())
                    .filter(ObjectUtils::isNotEmpty).toList());
        }
        if (ObjectUtils.isNotEmpty(entity.getBillCharges())) {
            List<String> creditorAndDebtorCodes = entity.getBillCharges().stream()
                    .flatMap(billCharge -> Stream.of(billCharge.getCreditorCode(), billCharge.getDebtorCode()))
                    .filter(ObjectUtils::isNotEmpty).toList();

            clientCodeList.addAll(creditorAndDebtorCodes);
        }
        return clientCodeList;
    }

    @Override
    public Boolean fetchActiveInvoices(CommonGetRequest request) throws RunnerException {

        InvoiceSummaryRequest invoiceSummaryRequest = new InvoiceSummaryRequest();
        invoiceSummaryRequest.setModuleType(Constants.SHIPMENT);
        invoiceSummaryRequest.setModuleGuid(request.getGuid());

        String url = billingBaseUrl + getInvoiceData;
        double start = System.currentTimeMillis();
        HttpEntity<InvoiceSummaryRequest> httpEntity = new HttpEntity<>(invoiceSummaryRequest, V1AuthHelper.getHeaders());
        var response = this.restTemplate.postForEntity(url, httpEntity, BillingSummaryResponse.class).getBody();
        log.info(LOG_TIME_CONSUMED, LoggerHelper.getRequestIdFromMDC(), url, System.currentTimeMillis() - start);
        BillingSummary billingSummary = new BillingSummary();
        if (Objects.nonNull(response)) {
            billingSummary = modelMapper.map(response.getData(), BillingSummary.class);
        }

        return checkActiveCharges(billingSummary);
    }

    /**
     * Sends a bill creation or update request to the external billing service.
     * <p>
     * This method constructs a URL from the base URL and endpoint configuration, creates an {@link HttpEntity} with the provided payload and authentication headers, and sends a
     * POST request to the specified URL. It then processes the response, logging the status and any errors encountered.
     *
     * @param externalBillPayloadRequest The request payload containing the bill details. This should not be {@code null}.
     * @param headers   V1 headers for bearer token
     * @return {@link ResponseEntity} containing the response from the billing service.
     * @throws BillingException if the response contains errors or if an exception occurs during the request process.
     */
    @Override
    public ResponseEntity<BillingEntityResponse> sendBillCreationRequest(ExternalBillPayloadRequest externalBillPayloadRequest, HttpHeaders headers) {
        // Construct the URL for the bill creation or update endpoint
        String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getExternalCreateOrUpdate();
        log.info("Sending bill creation request to URL: {}", url);

        // Create an HttpEntity object with the payload and authentication headers
        HttpEntity<ExternalBillPayloadRequest> httpEntity = new HttpEntity<>(externalBillPayloadRequest, headers);
        log.debug(REQUEST_PAYLOAD, externalBillPayloadRequest);
        double start = System.currentTimeMillis();
        try {
            // Send a POST request to the specified URL with the HttpEntity and expect a BillingEntityResponse
            log.info(EXECUTING_POST_REQUEST);
            ResponseEntity<BillingEntityResponse> responseEntity = this.restTemplate.postForEntity(url, httpEntity, BillingEntityResponse.class);
            BillingEntityResponse billingEntityResponse = responseEntity.getBody();

            // Log the response status and body
            log.info("Received response with status: {}", responseEntity.getStatusCode());
            log.debug("Response body: {}", billingEntityResponse);
            log.info(LOG_TIME_CONSUMED, LoggerHelper.getRequestIdFromMDC(), url, System.currentTimeMillis() - start);

            // Check if the response is not null and contains errors
            if (billingEntityResponse != null && ObjectUtils.isNotEmpty(billingEntityResponse.getErrors())) {
                log.error("Bill creation response contains errors: {}", billingEntityResponse.getErrors());
                throw new BillingException(billingEntityResponse.getErrors().toString());
            }
            return responseEntity;
        } catch (Exception e) {
            throw new BillingException(e.getMessage());
        }
    }

    @Override
    public List<BillingSummary> fetchBillingBulkSummary(BillingBulkSummaryRequest request) {
        String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getBillingBulkSummary();
        HttpEntity<BillingBulkSummaryRequest> httpEntity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
        return fetchBillingSummary(url, httpEntity);
    }

    @Override
    public List<BillingSummary> fetchBillingBulkSummaryBranchWise(BillingBulkSummaryBranchWiseRequest request) {
        String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getBillingBulkSummaryBranchWise();
        HttpEntity<BillingBulkSummaryBranchWiseRequest> httpEntity = new HttpEntity<>(request, v2AuthHelper.getInvoiceServiceXApiKeyHeader());
        return fetchBillingSummary(url, httpEntity);
    }

    @Override
    public List<BillingDueSummary> fetchBillingDueSummary(BillingBulkSummaryBranchWiseRequest request) {
        String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getBillingBulkDueSummaryBranchWise();
        HttpEntity<BillingBulkSummaryBranchWiseRequest> httpEntity = new HttpEntity<>(request, v2AuthHelper.getInvoiceServiceXApiKeyHeader());
        return fetchBillingDueSummary(url, httpEntity);
    }

    private <T> List<BillingSummary> fetchBillingSummary(String url, HttpEntity<T> httpEntity) {
        log.info("Sending request to URL: {}", url);
        log.debug(REQUEST_PAYLOAD, httpEntity.getBody());
        double start = System.currentTimeMillis();
        try {
            log.info(EXECUTING_POST_REQUEST);
            ResponseEntity<BillingEntityResponse> responseEntity = restTemplate.postForEntity(url, httpEntity, BillingEntityResponse.class);
            log.info(LOG_TIME_CONSUMED, LoggerHelper.getRequestIdFromMDC(), url, System.currentTimeMillis() - start);

            BillingEntityResponse billingEntityResponse = responseEntity.getBody();

            if (billingEntityResponse != null && ObjectUtils.isNotEmpty(billingEntityResponse.getErrors())) {
                String errorMsg = RESPONSE_CONTAINS_ERROR + billingEntityResponse.getErrors().toString();
                log.error(errorMsg);
                throw new BillingException(errorMsg);
            }

            if (responseEntity.getStatusCode().is2xxSuccessful() && billingEntityResponse != null
                    && ObjectUtils.isNotEmpty(billingEntityResponse.getData())
                    && ObjectUtils.isNotEmpty(billingEntityResponse.getData().get(BILLING_SUMMARY))) {
                log.info("Received billingEntityResponse from billing service");
                Map<String, Object> data = billingEntityResponse.getData();
                log.debug("Response data: {}", data);

                List<Map<String, Object>> billingSummaryListMap = (List<Map<String, Object>>) data.get(BILLING_SUMMARY);
                return modelMapper.map(billingSummaryListMap, new TypeToken<List<BillingSummary>>() {
                }.getType());
            } else {
                log.warn("Received non-successful response from billing service: {}", responseEntity.getStatusCode());
                return Collections.emptyList();
            }
        } catch (Exception e) {
            throw new BillingException("Error occurred while fetching billing summary. "+ e.getMessage());
        }
    }

    private <T> List<BillingDueSummary> fetchBillingDueSummary(String url, HttpEntity<T> httpEntity) {
        BillingEntityResponse response = executePostRequest(url, httpEntity, new ParameterizedTypeReference<BillingEntityResponse>() {});

        if (response != null && ObjectUtils.isNotEmpty(response.getData())
                && ObjectUtils.isNotEmpty(response.getData().get(BILLING_SUMMARY))) {
            List<Map<String, Object>> billingDueSummaryListMap = (List<Map<String, Object>>) response.getData().get(BILLING_SUMMARY);
            return modelMapper.map(billingDueSummaryListMap, new TypeToken<List<BillingDueSummary>>() {}.getType());
        } else {
            log.warn("Billing due summary data not found in response.");
            return Collections.emptyList();
        }
    }

    private <T, R> R executePostRequest(String url, HttpEntity<T> httpEntity, ParameterizedTypeReference<R> responseType) {
        log.info("Sending request to URL: {} request: {}", url, jsonHelper.convertToJson(httpEntity));
        log.debug(REQUEST_PAYLOAD, httpEntity.getBody());
        double start = System.currentTimeMillis();
        try {
            log.info(EXECUTING_POST_REQUEST);
            ResponseEntity<R> responseEntity = restTemplate.exchange(url, HttpMethod.POST, httpEntity, responseType);
            R response = responseEntity.getBody();
            log.info(LOG_TIME_CONSUMED, LoggerHelper.getRequestIdFromMDC(), url, System.currentTimeMillis() - start);
            log.info("Received response with status: {}", responseEntity.getStatusCode());
            log.info("Response body: {}", response);

            if (Objects.nonNull(response) && response instanceof BillingBaseResponse billingBaseResponse) {
                if (ObjectUtils.isNotEmpty(billingBaseResponse.getErrors())) {
                    String errorMsg = RESPONSE_CONTAINS_ERROR + billingBaseResponse.getErrors().toString();
                    throw new BillingException(errorMsg);
                }
            } else {
                log.warn("Received null response from billing service");
            }

            return response;
        } catch (Exception e) {
            throw new BillingException("Error occurred while making a request to the billing service: "+ e.getMessage());
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

    /*
    Please don't use this api going it retrieve shipment data internally
     */
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
        if (listResponse == null || ObjectUtils.isEmpty(listResponse.getData())) {
            throw new BillingException(NULL_RESPONSE_ERROR);
        }

        Type listType = new TypeToken<List<ChargeTypeBaseResponse>>() {
        }.getType();
        return modelMapper.map(listResponse.getData(), listType);
    }

    @Override
    public LocalDateTime fetchLastPostedInvoiceDate(LastPostedInvoiceDateRequest request) {
        String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getLastPostedInvoiceDate();
        HttpEntity<LastPostedInvoiceDateRequest> httpEntity = new HttpEntity<>(request, V1AuthHelper.getHeaders());
        ParameterizedTypeReference<BillingEntityResponse> responseType = new ParameterizedTypeReference<>() {
        };
        BillingEntityResponse billingEntityResponse = executePostRequest(url, httpEntity, responseType);
        if (billingEntityResponse == null
                || billingEntityResponse.getData() == null
                || billingEntityResponse.getData().get("lastPostedInvoiceDate") == null) {
            throw new BillingException(NULL_RESPONSE_ERROR);
        }

        return LocalDateTime.parse(
                billingEntityResponse.getData().get("lastPostedInvoiceDate").toString(),
                DateTimeFormatter.ofPattern(Constants.DATE_TIME_FORMAT));
    }

    public Boolean checkActiveCharges(BillingSummary billingSummary) {
        return (!Objects.equals(null, billingSummary.getTotalCount()) && !Objects.equals(0, billingSummary.getTotalCount())) ||
                (!Objects.equals(null, billingSummary.getTotalRevenue()) && Double.compare(billingSummary.getTotalRevenue(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getTotalCost()) && Double.compare(billingSummary.getTotalCost(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getAccruedRevenue()) && Double.compare(billingSummary.getAccruedRevenue(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getAccruedCost()) && Double.compare(billingSummary.getAccruedCost(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getInvoicedRevenue()) && Double.compare(billingSummary.getInvoicedRevenue(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getInvoicedCost()) && Double.compare(billingSummary.getInvoicedCost(), 0.0) > 0) ||
                areDisbursementFieldsPresent(billingSummary) || areCumulativeGPFieldsPresent(billingSummary);
    }

    private boolean areCumulativeGPFieldsPresent(BillingSummary billingSummary) {
        return (!Objects.equals(null, billingSummary.getCumulativeGP()) && Double.compare(billingSummary.getCumulativeGP(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getCumulativeGPPercentage()) && Double.compare(billingSummary.getCumulativeGPPercentage(), 0.0) > 0);
    }

    private boolean areDisbursementFieldsPresent(BillingSummary billingSummary) {
        return (!Objects.equals(null, billingSummary.getDisbursementAccruedRevenue()) && Double.compare(billingSummary.getDisbursementAccruedRevenue(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getDisbursementAccruedCost()) && Double.compare(billingSummary.getDisbursementAccruedCost(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getDisbursementInvoicedRevenue()) && Double.compare(billingSummary.getDisbursementInvoicedRevenue(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getDisbursementInvoicedCost()) && Double.compare(billingSummary.getDisbursementInvoicedCost(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getDisbursementRevenue()) && Double.compare(billingSummary.getDisbursementRevenue(), 0.0) > 0) ||
                (!Objects.equals(null, billingSummary.getDisbursementCost()) && Double.compare(billingSummary.getDisbursementCost(), 0.0) > 0);
    }

    @Override
    public ResponseEntity<BillingEntityResponse> createBillV2(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled,
            ShipmentDetailsResponse shipmentDetailsResponse, HttpHeaders headers) {

        CreateBookingModuleInV1 bookingRequestForV1 = v1ServiceUtil.createBookingRequestForV1(customerBooking, isShipmentEnabled, isBillingEnabled,
                shipmentDetailsResponse.getGuid());

        BookingEntity entity = bookingRequestForV1.getEntity();
        TenantModel tenantModel = getTenantModel(headers);
        ExternalBillPayloadRequest externalBillPayloadRequest = new ExternalBillPayloadRequest();
        List<ExternalBillRequest> externalBillRequests = new ArrayList<>();
        List<ExternalBillChargeRequest> externalBillChargeRequests = new ArrayList<>();

        List<EntityTransferOrganizations> organizationList = getOrganizationsListForBillCreationRequest(entity, headers);

        List<EntityTransferAddress> addressList = getAddressListForBillCreationRequest(organizationList, headers);

        EntityTransferOrganizations clientDetails = organizationList.stream()
                .filter(org -> org.getOrganizationCode().equalsIgnoreCase(entity.getClientCode()))
                .filter(ObjectUtils::isNotEmpty).findFirst().orElse(null);

        Long clientId = Optional.ofNullable(clientDetails)
                .map(EntityTransferOrganizations::getId)
                .orElseThrow(() -> new BillingException(NO_ORG_FOUND_FOR + entity.getClientCode()));

        EntityTransferAddress clientAddressDetails = Optional.ofNullable(entity.getClientAddressShortCode())
                .filter(code -> !code.trim().isEmpty()).flatMap(code -> addressList.stream()
                        .filter(x -> x.getAddressShortCode().equalsIgnoreCase(code)).findFirst())
                .or(() -> addressList.stream()
                        .filter(x -> x.getOrgId().equals(clientId) && Boolean.TRUE.equals(x.getDefaultAddress())).findFirst()).orElse(null);

        processExternalBillChargeRequest(entity, tenantModel, externalBillChargeRequests,
                organizationList, addressList, clientId, clientAddressDetails);

        processExternalBillRequest(shipmentDetailsResponse.getGuid(), entity, externalBillRequests, externalBillChargeRequests, clientId, clientAddressDetails);

        externalBillPayloadRequest.setExternalBillRequestList(externalBillRequests);

        return sendBillCreationRequest(externalBillPayloadRequest, headers);
    }

    private void processExternalBillRequest(UUID shipmentGuid, BookingEntity entity, List<ExternalBillRequest> externalBillRequests,
            List<ExternalBillChargeRequest> externalBillChargeRequests,
            Long clientId, EntityTransferAddress clientAddressDetails) {
        ExternalBillRequest externalBillRequest = ExternalBillRequest.builder()
                .externalBill(BillRequest.builder()
                        .jobStatus(Constants.WRK)
                        .isHiplHrBilling(Boolean.FALSE)
                        .moduleType(Constants.SHIPMENT)
                        .moduleGuid(shipmentGuid.toString())
                        .clientId(clientId.toString())
                        .clientAddressId(Optional.ofNullable(clientAddressDetails).map(EntityTransferAddress::getId)
                                .map(Object::toString).orElse(null))
                        .actualInvoiceDateEnabled(false)
                        .manualInvoiceDateEnabled(false)
                        .isLocked(false)
                        .lastLoadJson(entity.getLastTransactionLoadJson())
                        .build())
                .configuration(ExternalBillConfiguration.builder()
                        .autoCalculate(List.of(
                                Constants.LOCAL_REFERENCE_NUMBER,
                                "Tax",
                                "SequenceNumber",
                                "OverseasExchangeRate",
                                "RevenueVendorSection",
                                "OverseasAmounts",
                                "DueDate"))
                        .ignoreValidations(List.of(
                                "MeasurementBasisQuantity",
                                "Client",
                                "OverseasAgent",
                                "JobStatus"))
                        .build())
                .externalBillCharges(externalBillChargeRequests)
                .build();
        externalBillRequests.add(externalBillRequest);
    }

    private void processExternalBillChargeRequest(BookingEntity entity, TenantModel tenantModel,
            List<ExternalBillChargeRequest> externalBillChargeRequests, List<EntityTransferOrganizations> organizationList,
            List<EntityTransferAddress> addressList, Long clientId, EntityTransferAddress clientAddressDetails) {
        for (BillCharge billCharge : entity.getBillCharges()) {
            populateExternalBillChargeRequest(tenantModel, externalBillChargeRequests, organizationList, addressList, clientId, clientAddressDetails, billCharge);
        }

    }

    @NotNull
    private static MeasurementBasisRecord getMeasurementBasisRecord(BillCharge billCharge, String revenueMeasurementBasisV2, String measurementBasisUnit) {
        try {
            MeasurementBasis revenueMeasurementBasis = MeasurementBasis.valueOf(billCharge.getPerMeasurementBasis());
            revenueMeasurementBasisV2 = switch (revenueMeasurementBasis) {
                case CONTAINERCONT, CONTAINER_COUNT -> MeasurementBasis.CONTAINERCONT.getBillingValue();
                case WEIGHT -> MeasurementBasis.WEIGHT.getBillingValue();
                case VOLUME -> MeasurementBasis.VOLUME.getBillingValue();
                case CHARGEABLE -> MeasurementBasis.CHARGEABLE.getBillingValue();
                case LOWEST_BILL -> MeasurementBasis.LOWEST_BILL.getBillingValue();
                case PACKAGE -> MeasurementBasis.PACKAGE.getBillingValue();
                case SHIPMENT -> MeasurementBasis.SHIPMENT.getBillingValue();
                case TEU -> MeasurementBasis.TEU.getBillingValue();
                case CHARGE_PERCENTAGE -> MeasurementBasis.CHARGE_PERCENTAGE.getBillingValue();
                case CUSTOM -> MeasurementBasis.CUSTOM.getBillingValue();
                case CONTAINER_TYPE -> MeasurementBasis.CONTAINER_TYPE.getBillingValue();
            };

            if (ObjectUtils.isEmpty(measurementBasisUnit)) {
                measurementBasisUnit = switch (revenueMeasurementBasis) {
                    case CONTAINERCONT -> "Containers";
                    case WEIGHT -> "KG";
                    case VOLUME -> "M3";
                    case CHARGEABLE -> "KG";
                    case LOWEST_BILL -> "LB";
                    case PACKAGE -> "Packages";
                    case SHIPMENT -> "SHIPMENT";
                    case TEU -> "TEU";
                    case CHARGE_PERCENTAGE -> "%";
                    case CUSTOM -> "Custom";
                    case CONTAINER_TYPE -> "Containers";
                    default -> "";
                };
            }
        } catch (Exception e) {
            log.error(e.getMessage());
        }
        return new MeasurementBasisRecord(revenueMeasurementBasisV2, measurementBasisUnit);
    }

    private void populateExternalBillChargeRequest(TenantModel tenantModel, List<ExternalBillChargeRequest> externalBillChargeRequests,
            List<EntityTransferOrganizations> organizationList,
            List<EntityTransferAddress> addressList, Long clientId, EntityTransferAddress clientAddressDetails, BillCharge billCharge) {
        try {
            EntityTransferOrganizations creditorDetails = organizationList.stream().filter(org -> org.getOrganizationCode().equalsIgnoreCase(billCharge.getCreditorCode()))
                    .filter(ObjectUtils::isNotEmpty).findFirst().orElse(null);

            Long creditorId = getCreditorId(clientId, billCharge, creditorDetails);

            EntityTransferOrganizations debtorDetails = organizationList.stream()
                    .filter(org -> org.getOrganizationCode().equalsIgnoreCase(billCharge.getDebtorCode()))
                    .filter(ObjectUtils::isNotEmpty).findFirst().orElse(null);

            Long debtorId = Optional.ofNullable(billCharge.getDebtorCode()).filter(code -> !code.trim().isEmpty())
                    .map(code -> Optional.ofNullable(debtorDetails).map(EntityTransferOrganizations::getId)
                            .orElseThrow(() -> new BillingException(NO_ORG_FOUND_FOR + code)))
                    .orElseGet(() -> Optional.ofNullable(debtorDetails).map(EntityTransferOrganizations::getId)
                            .orElse(clientId));

            final Long capturedCreditorId = creditorId;

            EntityTransferAddress creditorAddressDetails = creditorId > 0 ?
                    Optional.ofNullable(billCharge.getCreditorAddressCode()).filter(code -> !code.trim().isEmpty())
                            .flatMap(code -> addressList.stream().filter(x -> x.getAddressShortCode().equalsIgnoreCase(code)).findFirst())
                            .orElseGet(() -> addressList.stream().filter(x -> x.getOrgId().equals(capturedCreditorId) && Boolean.TRUE.equals(x.getDefaultAddress())).findFirst().orElse(null))
                    : null;

            EntityTransferAddress debtorAddressDetails = Optional.ofNullable(billCharge.getDebitorAddressCode()).filter(code -> !code.trim().isEmpty())
                    .flatMap(code -> addressList.stream().filter(x -> x.getAddressShortCode().equalsIgnoreCase(code)).findFirst())
                    .orElseGet(() -> addressList.stream().filter(x -> x.getOrgId().equals(debtorId) && Boolean.TRUE.equals(x.getDefaultAddress()))
                            .findFirst().orElse(null));

            if (creditorAddressDetails == null && clientAddressDetails != null) {
                creditorAddressDetails = clientAddressDetails;
            }

            if (debtorAddressDetails == null && clientAddressDetails != null) {
                debtorAddressDetails = clientAddressDetails;
            }

            String revenueMeasurementBasisV2 = "";
            BigDecimal measurementBasisQuantity = null;
            String measurementBasisUnit = billCharge.getMeasurementsUnit();
            MeasurementBasisRecord measurementBasisRecord = getMeasurementBasisRecord(billCharge, revenueMeasurementBasisV2,
                    measurementBasisUnit);

            if (measurementBasisRecord.revenueMeasurementBasisV2() != null && !measurementBasisRecord.revenueMeasurementBasisV2().isBlank()) {
                measurementBasisQuantity = billCharge.getTotalUnitsCount() != null ? billCharge.getTotalUnitsCount() : BigDecimal.ONE;
            }

            ExternalBillChargeRequest externalBillChargeRequest = ExternalBillChargeRequest.builder()
                    .postAPInvoice(false)
                    .postARInvoice(false)
                    .billChargeRequest(BillChargesRequest.builder()
                            .chargeTypeCode(billCharge.getChargeTypeCode())
                            .isFromConsolidation(false)
                            .containerGuids(billCharge.getContainersGuid().stream().filter(ObjectUtils::isNotEmpty).map(UUID::toString).toList())
                            .autoCalculate(new ArrayList<>())
                            .ignoreValidations(new ArrayList<>())
                            .payableLocation("Origin")
                            .rateSource("PROCURED")
                            .billChargeCostDetails(BillChargeCostDetailsRequest.builder()
                                    .creditorId(creditorId > 0 ? creditorId.toString() : "")
                                    .creditorAddressId(creditorId > 0
                                            ? Optional.ofNullable(creditorAddressDetails)
                                            .map(Object::toString)
                                            .orElse("") : StringUtils.EMPTY)
                                    .measurementBasis(measurementBasisRecord.revenueMeasurementBasisV2())
                                    .measurementBasisUnit(measurementBasisRecord.measurementBasisUnit())
                                    .measurementBasisQuantity(measurementBasisQuantity)
                                    .unitRate(billCharge.getLocalCostAmount())
                                    .unitRateCurrency(Optional.ofNullable(billCharge.getLocalCostCurrency()).filter(ObjectUtils::isNotEmpty)
                                            .orElse(tenantModel.getCurrencyCode()))
                                    .localCostAmount(billCharge.getLocalCostAmount())
                                    .localCostCurrency(Optional.ofNullable(billCharge.getLocalCostCurrency()).filter(ObjectUtils::isNotEmpty)
                                            .orElse(tenantModel.getCurrencyCode()))
                                    .overseasCostAmount(billCharge.getOverseasCostAmount())
                                    .overseasCostCurrency(Optional.ofNullable(billCharge.getOverseasCostCurrency()).filter(ObjectUtils::isNotEmpty)
                                            .orElse(tenantModel.getCurrencyCode()))
                                    .noTax(false)
                                    .invoiceDate(LocalDateTime.of(LocalDate.now(), LocalTime.MIDNIGHT))
                                    .documentRecordDate(LocalDateTime.of(LocalDate.now(), LocalTime.MIDNIGHT))
                                    .isRcm(false).build())
                            .billChargeRevenueDetails(BillChargeRevenueDetailsRequest.builder()
                                    .debtorId(debtorId.toString())
                                    .debtorAddressId(Optional.ofNullable(debtorAddressDetails).map(EntityTransferAddress::getId)
                                            .map(Object::toString).orElse(null))
                                    .measurementBasis(measurementBasisRecord.revenueMeasurementBasisV2())
                                    .measurementBasisUnit(measurementBasisRecord.measurementBasisUnit())
                                    .measurementBasisQuantity(measurementBasisQuantity)
                                    .unitRate(Optional.ofNullable(billCharge.getCurrentSellRate()).filter(ObjectUtils::isNotEmpty)
                                            .orElse(BigDecimal.ZERO))
                                    .unitRateCurrency(Optional.ofNullable(billCharge.getLocalSellCurrency()).filter(ObjectUtils::isNotEmpty)
                                            .orElse(tenantModel.getCurrencyCode()))
                                    .localSellAmount(billCharge.getLocalSellAmount())
                                    .localSellCurrency(Optional.ofNullable(billCharge.getLocalSellCurrency()).filter(ObjectUtils::isNotEmpty)
                                            .orElse(tenantModel.getCurrencyCode()))
                                    .overseasSellAmount(billCharge.getOverseasSellAmount())
                                    .overseasSellCurrency(Optional.ofNullable(billCharge.getOverseasSellCurrency()).filter(ObjectUtils::isNotEmpty)
                                            .orElse(tenantModel.getCurrencyCode()))
                                    .noTax(false)
                                    .isRcm(false).build())
                            .build())
                    .build();
            externalBillChargeRequests.add(externalBillChargeRequest);
        } catch (Exception ex) {
            throw new BillingException(ex.getMessage());
        }
    }

    private Long getCreditorId(Long clientId, BillCharge billCharge, EntityTransferOrganizations creditorDetails) {
        Long creditorId = Optional.ofNullable(billCharge.getCreditorCode())
                .filter(code -> !code.trim().isEmpty())
                .map(code -> Optional.ofNullable(creditorDetails)
                        .map(EntityTransferOrganizations::getId)
                        .orElseThrow(() -> new BillingException(NO_ORG_FOUND_FOR + code)))
                .orElseGet(() -> Optional.ofNullable(creditorDetails)
                        .map(EntityTransferOrganizations::getId)
                        .orElse(clientId));

        if (creditorDetails == null || creditorDetails.getPayables() == null || Boolean.FALSE.equals(creditorDetails.getPayables())) {
            creditorId = -1L;
        }
        return creditorId;
    }

    private record MeasurementBasisRecord(String revenueMeasurementBasisV2, String measurementBasisUnit) {

    }

    private TenantModel getTenantModel(HttpHeaders headers) {
        TenantModel tenantModel = new TenantModel();
        try {
            log.info("Fetching Tenant Model");
            tenantModel = modelMapper.map(v1Service.retrieveTenant(headers).getEntity(), TenantModel.class);
        } catch (Exception e) {
            throw new BillingException("Failed in fetching tenant data from V1 with error : {}", e);
        }
        return tenantModel;
    }

    private List<EntityTransferOrganizations> getOrganizationsListForBillCreationRequest(BookingEntity entity, HttpHeaders headers) {
        List<String> clientCodeList = getClientCodeListForBillCreationRequest(entity);
        List<EntityTransferOrganizations> organizationList = new ArrayList<>();
        if (ObjectUtils.isNotEmpty(clientCodeList)) {
            List<Object> finalCriteria = new ArrayList<>();
            CommonV1ListRequest orgRequest = new CommonV1ListRequest();

            List<Object> orgField = new ArrayList<>(List.of(Constants.ORGANIZATION_CODE));
            List<Object> orgCriteria = new ArrayList<>(List.of(orgField, Constants.IN, List.of(clientCodeList)));
            finalCriteria.add(orgCriteria);

            finalCriteria.add("and");

            List<Object> activeClient = new ArrayList<>(List.of(Constants.ACTIVE_CLIENT));
            List<Object> activeClientCriteria = new ArrayList<>(List.of(activeClient, Constants.EQ, 1));
            finalCriteria.add(activeClientCriteria);

            orgRequest.setCriteriaRequests(finalCriteria);
            orgRequest.setTake(clientCodeList.size());
            V1DataResponse orgResponse = v1Service.fetchOrganization(orgRequest, headers);
            organizationList = jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class);
        }
        return organizationList;
    }

    private List<EntityTransferAddress> getAddressListForBillCreationRequest(List<EntityTransferOrganizations> organizations, HttpHeaders headers) {
        List<EntityTransferAddress> addressList = new ArrayList<>();
        if (ObjectUtils.isNotEmpty(organizations)) {
            List<Object> finalCriteria = new ArrayList<>();
            CommonV1ListRequest addressRequest = new CommonV1ListRequest();

            List<Object> orgIdField = new ArrayList<>(List.of(Constants.ORG_ID));
            List<Long> orgIdList = organizations.stream().filter(ObjectUtils::isNotEmpty).map(EntityTransferOrganizations::getId).toList();
            List<Object> orgIdCriteria = new ArrayList<>(List.of(orgIdField, Constants.IN, List.of(orgIdList)));
            finalCriteria.add(orgIdCriteria);

            finalCriteria.add("and");

            List<Object> activeClient = new ArrayList<>(List.of(Constants.ACTIVE));
            List<Object> activeClientCriteria = new ArrayList<>(List.of(activeClient, Constants.EQ, 1));
            finalCriteria.add(activeClientCriteria);

            addressRequest.setCriteriaRequests(finalCriteria);
            addressRequest.setTake(orgIdList.size());
            V1DataResponse addressResponse = v1Service.addressList(addressRequest, headers);
            addressList = jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class);
        }
        return addressList;
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
