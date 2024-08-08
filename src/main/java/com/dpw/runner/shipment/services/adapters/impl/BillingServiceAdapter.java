package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.interfaces.IBillingServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.requests.CommonGetRequest;
import com.dpw.runner.shipment.services.commons.requests.billing.ExternalBillPayloadRequest;
import com.dpw.runner.shipment.services.dto.request.InvoiceSummaryRequest;
import com.dpw.runner.shipment.services.dto.response.billing.BillingEntityResponse;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummary;
import com.dpw.runner.shipment.services.dto.response.billing.BillingSummaryResponse;
import com.dpw.runner.shipment.services.dto.response.billing.ChargeTypeResponse;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.shipment.services.utils.CommonUtils;
import com.dpw.runner.shipment.services.utils.V1AuthHelper;
import java.util.List;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.jetbrains.annotations.NotNull;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.Objects;

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
    private V1ServiceUtil v1ServiceUtil;
    @Autowired
    private IV1Service v1Service;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private CommonUtils commonUtils;

    @Nullable // TODO: SUBHAM Complete this methods to support chargetype transport mode validation
    private static ChargeTypeResponse getChargeTypeDetails(List<ChargeTypeResponse> chargeTypeList, BillCharge billCharge) {
        ChargeTypeResponse chargeTypeDetails = null;

        List<ChargeTypeResponse> matchedChargeTypeDetails =
                chargeTypeList.stream().filter(x -> billCharge.getChargeTypeCode().equalsIgnoreCase(x.getChargeCode())).filter(ObjectUtils::isNotEmpty).toList();

        if (ObjectUtils.isNotEmpty(matchedChargeTypeDetails)) {
            List<String> chargeTypeIds = matchedChargeTypeDetails.stream().map(ChargeTypeResponse::getId).distinct().toList();
        }
        return chargeTypeDetails;
    }

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
     * Sends a bill creation or update request to the external billing service.
     * <p>
     * This method constructs a URL from the base URL and endpoint configuration, creates an {@link HttpEntity} with the provided payload and authentication headers, and sends a
     * POST request to the specified URL. It then processes the response, logging the status and any errors encountered.
     *
     * @param externalBillPayloadRequest The request payload containing the bill details. This should not be {@code null}.
     * @return {@link ResponseEntity} containing the response from the billing service.
     * @throws BillingException if the response contains errors or if an exception occurs during the request process.
     */
    @Override
    public ResponseEntity<BillingEntityResponse> sendBillCreationRequest(ExternalBillPayloadRequest externalBillPayloadRequest) {
        // Construct the URL for the bill creation or update endpoint
        String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getExternalCreateOrUpdate();
        log.info("Sending bill creation request to URL: {}", url);

        // Create an HttpEntity object with the payload and authentication headers
        HttpEntity<ExternalBillPayloadRequest> httpEntity = new HttpEntity<>(externalBillPayloadRequest, V1AuthHelper.getHeaders());
        log.debug("Request payload: {}", externalBillPayloadRequest);

        try {
            // Send a POST request to the specified URL with the HttpEntity and expect a BillingEntityResponse
            log.info("Executing POST request...");
            ResponseEntity<BillingEntityResponse> responseEntity = this.restTemplate.postForEntity(url, httpEntity, BillingEntityResponse.class);
            BillingEntityResponse billingEntityResponse = responseEntity.getBody();

            // Log the response status and body
            log.info("Received response with status: {}", responseEntity.getStatusCode());
            log.debug("Response body: {}", billingEntityResponse);

            // Check if the response is not null and contains errors
            if (ObjectUtils.isNotEmpty(billingEntityResponse) && ObjectUtils.isNotEmpty(billingEntityResponse.getErrors())) {
                log.error("Bill creation response contains errors: {}", billingEntityResponse.getErrors());
                throw new BillingException(billingEntityResponse.getErrors().toString());
            }
            return responseEntity;
        } catch (Exception e) {
            throw new BillingException(e.getMessage());
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
    public ExternalBillPayloadRequest getBillCreationRequest(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled,
            ShipmentDetailsResponse shipmentDetailsResponse) {

        CreateBookingModuleInV1 bookingRequestForV1 = v1ServiceUtil.createBookingRequestForV1(customerBooking, isShipmentEnabled, isBillingEnabled,
                shipmentDetailsResponse.getGuid());

        BookingEntity entity = bookingRequestForV1.getEntity();
        TenantModel tenantModel = getTenantModel();
        ExternalBillPayloadRequest externalBillPayloadRequest = new ExternalBillPayloadRequest();
        List<ExternalBillRequest> externalBillRequests = new ArrayList<>();
        List<ExternalBillChargeRequest> externalBillChargeRequests = new ArrayList<>();
        boolean isAllBillChargesSuccess = true;

        List<ChargeTypeResponse> chargeTypeList = getChargeTypeListForBillCreationRequest(entity);

        List<EntityTransferOrganizations> organizationList = getOrganizationsListForBillCreationRequest(entity);

        List<EntityTransferAddress> addressList = getAddressListForBillCreationRequest(organizationList);

        EntityTransferOrganizations clientDetails = organizationList.stream()
                .filter(org -> org.getOrganizationCode().equalsIgnoreCase(entity.getClientCode()))
                .filter(ObjectUtils::isNotEmpty).findFirst().orElse(null);

        Long clientId = Optional.ofNullable(clientDetails)
                .map(EntityTransferOrganizations::getId)
                .orElseThrow(() -> new BillingException("No OrganizationsRow found for " + entity.getClientCode()));

        EntityTransferAddress clientAddressDetails = Optional.ofNullable(entity.getClientAddressShortCode())
                .filter(code -> !code.trim().isEmpty()).flatMap(code -> addressList.stream()
                        .filter(x -> x.getAddressShortCode().equalsIgnoreCase(code)).findFirst())
                .or(() -> addressList.stream()
                        .filter(x -> x.getOrgId().equals(clientId) && x.getDefaultAddress()).findFirst()).orElse(null);

        isAllBillChargesSuccess = processExternalBillChargeRequest(entity, tenantModel, externalBillChargeRequests,
                isAllBillChargesSuccess, chargeTypeList, organizationList, addressList, clientId, clientAddressDetails);

        if (isAllBillChargesSuccess) {
            processExternalBillRequest(shipmentDetailsResponse.getGuid(), entity, externalBillRequests, externalBillChargeRequests, clientId, clientAddressDetails);
        }
        externalBillPayloadRequest.setExternalBillRequestList(externalBillRequests);

        return externalBillPayloadRequest;
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
                                Constants.LocalReferenceNumber,
                                "Tax",
                                "SequenceNumber",
                                "OverseasExchangeRate",
                                "RevenueVendorSection",
                                "OverseasAmounts"))
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

    private boolean processExternalBillChargeRequest(BookingEntity entity, TenantModel tenantModel, List<ExternalBillChargeRequest> externalBillChargeRequests,
            boolean isAllBillChargesSuccess, List<ChargeTypeResponse> chargeTypeList, List<EntityTransferOrganizations> organizationList, List<EntityTransferAddress> addressList,
            Long clientId, EntityTransferAddress clientAddressDetails) {
        try {
            for (BillCharge billCharge : entity.getBillCharges()) {
                ChargeTypeResponse chargeTypeDetails = getChargeTypeDetails(chargeTypeList, billCharge); // TODO: SUBHAM complete this

                EntityTransferOrganizations creditorDetails = organizationList.stream().filter(org -> org.getOrganizationCode().equalsIgnoreCase(billCharge.getCreditorCode()))
                        .filter(ObjectUtils::isNotEmpty).findFirst().orElse(null);
                Long creditorId = Optional.ofNullable(billCharge.getCreditorCode())
                        .filter(code -> !code.trim().isEmpty())
                        .map(code -> Optional.ofNullable(creditorDetails)
                                .map(EntityTransferOrganizations::getId)
                                .orElseThrow(() -> new BillingException("No OrganizationsRow found for " + code)))
                        .orElseGet(() -> Optional.ofNullable(creditorDetails)
                                .map(EntityTransferOrganizations::getId)
                                .orElse(clientId));

                if (creditorDetails == null || creditorDetails.getPayables() == null || Boolean.FALSE.equals(creditorDetails.getPayables())) {
                    creditorId = -1L;
                }

                EntityTransferOrganizations debtorDetails = organizationList.stream()
                        .filter(org -> org.getOrganizationCode().equalsIgnoreCase(billCharge.getDebtorCode()))
                        .filter(ObjectUtils::isNotEmpty).findFirst().orElse(null);
                Long debtorId = Optional.ofNullable(billCharge.getDebtorCode()).filter(code -> !code.trim().isEmpty())
                        .map(code -> Optional.ofNullable(debtorDetails).map(EntityTransferOrganizations::getId)
                                .orElseThrow(() -> new BillingException("No OrganizationsRow found for " + code)))
                        .orElseGet(() -> Optional.ofNullable(debtorDetails).map(EntityTransferOrganizations::getId)
                                .orElse(clientId));

                final Long capturedCreditorId = creditorId;

                EntityTransferAddress creditorAddressDetails = creditorId > 0 ?
                        Optional.ofNullable(billCharge.getCreditorAddressCode()).filter(code -> !code.trim().isEmpty())
                                .flatMap(code -> addressList.stream().filter(x -> x.getAddressShortCode().equalsIgnoreCase(code)).findFirst())
                                .orElseGet(() -> addressList.stream().filter(x -> x.getOrgId().equals(capturedCreditorId) && x.getDefaultAddress()).findFirst().orElse(null))
                        : null;

                EntityTransferAddress debtorAddressDetails = Optional.ofNullable(billCharge.getDebitorAddressCode()).filter(code -> !code.trim().isEmpty())
                        .flatMap(code -> addressList.stream().filter(x -> x.getAddressShortCode().equalsIgnoreCase(code)).findFirst())
                        .orElseGet(() -> addressList.stream().filter(x -> x.getOrgId().equals(debtorId) && x.getDefaultAddress())
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
                try {
                    MeasurementBasis revenueMeasurementBasis = MeasurementBasis.valueOf(billCharge.getPerMeasurementBasis());
                    revenueMeasurementBasisV2 = switch (revenueMeasurementBasis) {
                        case ContainerCount -> MeasurementBasis.ContainerCount.getBillingValue();
                        case Weight -> MeasurementBasis.Weight.getBillingValue();
                        case Volume -> MeasurementBasis.Volume.getBillingValue();
                        case Chargeable -> MeasurementBasis.Chargeable.getBillingValue();
                        case LowestBill -> MeasurementBasis.LowestBill.getBillingValue();
                        case Package -> MeasurementBasis.Package.getBillingValue();
                        case Shipment -> MeasurementBasis.Shipment.getBillingValue();
                        case TEU -> MeasurementBasis.TEU.getBillingValue();
                        case ChargePercentage -> MeasurementBasis.ChargePercentage.getBillingValue();
                        case Custom -> MeasurementBasis.Custom.getBillingValue();
                        case ContainerType -> MeasurementBasis.ContainerType.getBillingValue();
                    };

                    if (ObjectUtils.isEmpty(measurementBasisUnit)) {
                        measurementBasisUnit = switch (revenueMeasurementBasis) {
                            case ContainerCount -> "Containers";
                            case Weight -> "KG";
                            case Volume -> "M3";
                            case Chargeable -> "KG";
                            case LowestBill -> "LB";
                            case Package -> "Packages";
                            case Shipment -> "SHIPMENT";
                            case TEU -> "TEU";
                            case ChargePercentage -> "%";
                            case Custom -> "Custom";
                            case ContainerType -> "Containers";
                            default -> "";
                        };
                    }
                } catch (Exception e) {
                    log.error(e.getMessage());
                }

                if (revenueMeasurementBasisV2 != null && !revenueMeasurementBasisV2.isBlank()) {
                    measurementBasisQuantity = billCharge.getTotalUnitsCount() != null ? billCharge.getTotalUnitsCount() : BigDecimal.ONE;
                }

                ExternalBillChargeRequest externalBillChargeRequest = ExternalBillChargeRequest.builder()
                        .postAPInvoice(false)
                        .postARInvoice(false)
                        .billChargeRequest(BillChargesRequest.builder()
                                .chargeTypeGuid(null)
                                .details(null)
                                .chargeTypeCode(billCharge.getChargeTypeCode())
                                .isFromConsolidation(false)
                                .containerGuids(billCharge.getContainersGuid().stream().filter(ObjectUtils::isNotEmpty).map(UUID::toString).toList())
                                .autoCalculate(new ArrayList<>())
                                .ignoreValidations(new ArrayList<>())
                                .payableLocation("Origin")
                                .rateSource("PROCURED")
                                .billChargeCostDetails(BillChargeCostDetailsRequest.builder()
                                        .creditorId(creditorId > 0 ? creditorId.toString() : "")
                                        .creditorAddressId(creditorId > 0 ? creditorAddressDetails.toString() : "")
                                        .measurementBasis(revenueMeasurementBasisV2)
                                        .measurementBasisUnit(measurementBasisUnit)
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
                                        .measurementBasis(revenueMeasurementBasisV2)
                                        .measurementBasisUnit(measurementBasisUnit)
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
                                        .dueDate(null)
                                        .paymentTermsCode(null)
                                        .isRcm(false).build())
                                .build())
                        .build();
                externalBillChargeRequests.add(externalBillChargeRequest);
            }
        } catch (Exception e) {
            isAllBillChargesSuccess = false;
            throw new BillingException(e.getMessage());
        }
        return isAllBillChargesSuccess;
    }

    private TenantModel getTenantModel() {
        TenantModel tenantModel = new TenantModel();
        try {
            log.info("Fetching Tenant Model");
            tenantModel = modelMapper.map(v1Service.retrieveTenant().getEntity(), TenantModel.class);
        } catch (Exception e) {
            throw new BillingException("Failed in fetching tenant data from V1 with error : {}", e);
        }
        return tenantModel;
    }

    private List<ChargeTypeResponse> getChargeTypeListForBillCreationRequest(BookingEntity entity) {
        List<ChargeTypeResponse> chargeTypeList = new ArrayList<>();
        List<String> chargeTypeCodes = entity.getBillCharges().stream().filter(ObjectUtils::isNotEmpty)
                .map(BillCharge::getChargeTypeCode).toList();
        try {
            if (ObjectUtils.isNotEmpty(chargeTypeCodes)) {

                ChargeTypeFilterRequest chargeTypeFilterRequest = new ChargeTypeFilterRequest();
                chargeTypeFilterRequest.setChargeCodes(chargeTypeCodes);
                chargeTypeFilterRequest.setPageSize(chargeTypeCodes.size());

                String url = billingServiceUrlConfig.getBaseUrl() + billingServiceUrlConfig.getChargeTypeFilter();
                log.info("Sending bill creation request to URL: {}", url);

                HttpEntity<ChargeTypeFilterRequest> httpEntity = new HttpEntity<>(chargeTypeFilterRequest, V1AuthHelper.getHeaders());

                log.info("Executing POST request...");
                ResponseEntity<BillingListEntityResponse> responseEntity = this.restTemplate.postForEntity(url, httpEntity, BillingListEntityResponse.class);

                BillingListEntityResponse<ChargeTypeResponse> listEntityResponse = responseEntity.getBody();

                log.info("Received response with status: {}", responseEntity.getStatusCode());
                log.debug("Response body: {}", listEntityResponse);

                if (ObjectUtils.isNotEmpty(listEntityResponse) && ObjectUtils.isNotEmpty(listEntityResponse.getErrors())) {
                    log.error("Bill creation response contains errors: {}", listEntityResponse.getErrors());
                    throw new BillingException(listEntityResponse.getErrors().toString());
                }

                return (List<ChargeTypeResponse>) listEntityResponse.getData();
            }
        } catch (Exception e) {
            throw new BillingException(e.getMessage());
        }

        return chargeTypeList;
    }

    private List<EntityTransferOrganizations> getOrganizationsListForBillCreationRequest(BookingEntity entity) {
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
            V1DataResponse orgResponse = v1Service.fetchOrganization(orgRequest);
            organizationList = jsonHelper.convertValueToList(orgResponse.entities, EntityTransferOrganizations.class);
        }
        return organizationList;
    }

    private List<EntityTransferAddress> getAddressListForBillCreationRequest(List<EntityTransferOrganizations> organizations) {
        List<EntityTransferAddress> addressList = new ArrayList<>();
        if (ObjectUtils.isNotEmpty(organizations)) {
            List<Object> finalCriteria = new ArrayList<>();
            CommonV1ListRequest addressRequest = new CommonV1ListRequest();

            List<Object> orgIdField = new ArrayList<>(List.of(Constants.ORG_ID));
            List<Long> orgIdList = organizations.stream().filter(ObjectUtils::isNotEmpty).map(EntityTransferOrganizations::getId).toList();
            List<Object> orgIdCriteria = new ArrayList<>(List.of(orgIdField, Constants.IN, List.of(orgIdList)));
            finalCriteria.add(orgIdCriteria);

            finalCriteria.add("and");

            List<Object> activeClient = new ArrayList<>(List.of(Constants.ACTIVE_CLIENT));
            List<Object> activeClientCriteria = new ArrayList<>(List.of(activeClient, Constants.EQ, 1));
            finalCriteria.add(activeClientCriteria);

            addressRequest.setCriteriaRequests(finalCriteria);
            addressRequest.setTake(orgIdList.size());
            V1DataResponse addressResponse = v1Service.addressList(addressRequest);
            addressList = jsonHelper.convertValueToList(addressResponse.entities, EntityTransferAddress.class);
        }
        return addressList;
    }


}
