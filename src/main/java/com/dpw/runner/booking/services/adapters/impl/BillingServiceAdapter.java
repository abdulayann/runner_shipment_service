package com.dpw.runner.booking.services.adapters.impl;

import com.dpw.runner.booking.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.booking.services.adapters.interfaces.IBillingServiceAdapter;
import com.dpw.runner.booking.services.commons.constants.Constants;
import com.dpw.runner.booking.services.dto.request.CreateBookingModuleInV1;
import com.dpw.runner.booking.services.dto.request.CreateBookingModuleInV1.BookingEntity;
import com.dpw.runner.booking.services.dto.request.CreateBookingModuleInV1.BookingEntity.BillCharge;
import com.dpw.runner.booking.services.dto.request.billing.ExternalBillPayloadRequest;
import com.dpw.runner.booking.services.dto.request.billing.ExternalBillPayloadRequest.*;
import com.dpw.runner.booking.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.booking.services.dto.response.billing.BillingEntityResponse;
import com.dpw.runner.booking.services.dto.v1.TenantModel;
import com.dpw.runner.booking.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.booking.services.entity.CustomerBooking;
import com.dpw.runner.booking.services.entity.enums.MeasurementBasis;
import com.dpw.runner.booking.services.masterDataObjects.dto.AddressData;
import com.dpw.runner.booking.services.masterDataObjects.dto.OrganizationsMasterData;
import com.dpw.runner.booking.services.exception.exceptions.billing.BillingException;
import com.dpw.runner.booking.services.helpers.JsonHelper;
import com.dpw.runner.booking.services.helpers.LoggerHelper;
import com.dpw.runner.booking.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.booking.services.service.v1.IV1Service;
import com.dpw.runner.booking.services.service.v1.util.V1ServiceUtil;
import com.dpw.runner.booking.services.utils.CommonUtils;
import com.dpw.runner.booking.services.utils.V1AuthHelper;
import com.dpw.runner.booking.services.utils.V2AuthHelper;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

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
    public ResponseEntity<BillingEntityResponse> createBillV2(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled,
            ShipmentDetailsResponse shipmentDetailsResponse) {

        CreateBookingModuleInV1 bookingRequestForV1 = v1ServiceUtil.createBookingRequestForV1(customerBooking, isShipmentEnabled, isBillingEnabled,
                shipmentDetailsResponse.getGuid());

        BookingEntity entity = bookingRequestForV1.getEntity();
        TenantModel tenantModel = getTenantModel();
        ExternalBillPayloadRequest externalBillPayloadRequest = new ExternalBillPayloadRequest();
        List<ExternalBillRequest> externalBillRequests = new ArrayList<>();
        List<ExternalBillChargeRequest> externalBillChargeRequests = new ArrayList<>();

        List<OrganizationsMasterData> organizationList = getOrganizationsListForBillCreationRequest(entity);

        List<AddressData> addressList = getAddressListForBillCreationRequest(organizationList);

        OrganizationsMasterData clientDetails = organizationList.stream()
                .filter(org -> org.getOrganizationCode().equalsIgnoreCase(entity.getClientCode()))
                .filter(ObjectUtils::isNotEmpty).findFirst().orElse(null);

        Long clientId = Optional.ofNullable(clientDetails)
                .map(OrganizationsMasterData::getId)
                .orElseThrow(() -> new BillingException(NO_ORG_FOUND_FOR + entity.getClientCode()));

        AddressData clientAddressDetails = Optional.ofNullable(entity.getClientAddressShortCode())
                .filter(code -> !code.trim().isEmpty()).flatMap(code -> addressList.stream()
                        .filter(x -> x.getAddressShortCode().equalsIgnoreCase(code)).findFirst())
                .or(() -> addressList.stream()
                        .filter(x -> x.getOrgId().equals(clientId) && Boolean.TRUE.equals(x.getDefaultAddress())).findFirst()).orElse(null);

        processExternalBillChargeRequest(entity, tenantModel, externalBillChargeRequests,
                organizationList, addressList, clientId, clientAddressDetails);

        processExternalBillRequest(shipmentDetailsResponse.getGuid(), entity, externalBillRequests, externalBillChargeRequests, clientId, clientAddressDetails);

        externalBillPayloadRequest.setExternalBillRequestList(externalBillRequests);

        return sendBillCreationRequest(externalBillPayloadRequest);
    }

    private void processExternalBillRequest(UUID shipmentGuid, BookingEntity entity, List<ExternalBillRequest> externalBillRequests,
            List<ExternalBillChargeRequest> externalBillChargeRequests,
            Long clientId, AddressData clientAddressDetails) {
        ExternalBillRequest externalBillRequest = ExternalBillRequest.builder()
                .externalBill(BillRequest.builder()
                        .jobStatus(Constants.WRK)
                        .isHiplHrBilling(Boolean.FALSE)
                        .moduleType(Constants.SHIPMENT)
                        .moduleGuid(shipmentGuid.toString())
                        .clientId(clientId.toString())
                        .clientAddressId(Optional.ofNullable(clientAddressDetails).map(AddressData::getId)
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
                                                  List<ExternalBillChargeRequest> externalBillChargeRequests, List<OrganizationsMasterData> organizationList,
                                                  List<AddressData> addressList, Long clientId, AddressData clientAddressDetails) {
        for (BillCharge billCharge : entity.getBillCharges()) {
            populateExternalBillChargeRequest(tenantModel, externalBillChargeRequests, organizationList, addressList, clientId, clientAddressDetails, billCharge);
        }

    }

    @NotNull
    private static MeasurementBasisRecord getMeasurementBasisRecord(BillCharge billCharge, String revenueMeasurementBasisV2, String measurementBasisUnit) {
        try {
            MeasurementBasis revenueMeasurementBasis = MeasurementBasis.valueOf(billCharge.getPerMeasurementBasis());
            revenueMeasurementBasisV2 = switch (revenueMeasurementBasis) {
                case ContainerCount, Container_Count -> MeasurementBasis.ContainerCount.getBillingValue();
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
        return new MeasurementBasisRecord(revenueMeasurementBasisV2, measurementBasisUnit);
    }

    private void populateExternalBillChargeRequest(TenantModel tenantModel, List<ExternalBillChargeRequest> externalBillChargeRequests,
                                                   List<OrganizationsMasterData> organizationList,
                                                   List<AddressData> addressList, Long clientId, AddressData clientAddressDetails, BillCharge billCharge) {
        try {
            OrganizationsMasterData creditorDetails = organizationList.stream().filter(org -> org.getOrganizationCode().equalsIgnoreCase(billCharge.getCreditorCode()))
                    .filter(ObjectUtils::isNotEmpty).findFirst().orElse(null);

            Long creditorId = Optional.ofNullable(billCharge.getCreditorCode())
                    .filter(code -> !code.trim().isEmpty())
                    .map(code -> Optional.ofNullable(creditorDetails)
                            .map(OrganizationsMasterData::getId)
                            .orElseThrow(() -> new BillingException(NO_ORG_FOUND_FOR + code)))
                    .orElseGet(() -> Optional.ofNullable(creditorDetails)
                            .map(OrganizationsMasterData::getId)
                            .orElse(clientId));

            if (creditorDetails == null || creditorDetails.getPayables() == null || Boolean.FALSE.equals(creditorDetails.getPayables())) {
                creditorId = -1L;
            }

            OrganizationsMasterData debtorDetails = organizationList.stream()
                    .filter(org -> org.getOrganizationCode().equalsIgnoreCase(billCharge.getDebtorCode()))
                    .filter(ObjectUtils::isNotEmpty).findFirst().orElse(null);

            Long debtorId = Optional.ofNullable(billCharge.getDebtorCode()).filter(code -> !code.trim().isEmpty())
                    .map(code -> Optional.ofNullable(debtorDetails).map(OrganizationsMasterData::getId)
                            .orElseThrow(() -> new BillingException(NO_ORG_FOUND_FOR + code)))
                    .orElseGet(() -> Optional.ofNullable(debtorDetails).map(OrganizationsMasterData::getId)
                            .orElse(clientId));

            final Long capturedCreditorId = creditorId;

            AddressData creditorAddressDetails = creditorId > 0 ?
                    Optional.ofNullable(billCharge.getCreditorAddressCode()).filter(code -> !code.trim().isEmpty())
                            .flatMap(code -> addressList.stream().filter(x -> x.getAddressShortCode().equalsIgnoreCase(code)).findFirst())
                            .orElseGet(() -> addressList.stream().filter(x -> x.getOrgId().equals(capturedCreditorId) && Boolean.TRUE.equals(x.getDefaultAddress())).findFirst().orElse(null))
                    : null;

            AddressData debtorAddressDetails = Optional.ofNullable(billCharge.getDebitorAddressCode()).filter(code -> !code.trim().isEmpty())
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
                                    .debtorAddressId(Optional.ofNullable(debtorAddressDetails).map(AddressData::getId)
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

    private record MeasurementBasisRecord(String revenueMeasurementBasisV2, String measurementBasisUnit) {

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

    private List<OrganizationsMasterData> getOrganizationsListForBillCreationRequest(BookingEntity entity) {
        List<String> clientCodeList = getClientCodeListForBillCreationRequest(entity);
        List<OrganizationsMasterData> organizationList = new ArrayList<>();
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
            organizationList = jsonHelper.convertValueToList(orgResponse.entities, OrganizationsMasterData.class);
        }
        return organizationList;
    }

    private List<AddressData> getAddressListForBillCreationRequest(List<OrganizationsMasterData> organizations) {
        List<AddressData> addressList = new ArrayList<>();
        if (ObjectUtils.isNotEmpty(organizations)) {
            List<Object> finalCriteria = new ArrayList<>();
            CommonV1ListRequest addressRequest = new CommonV1ListRequest();

            List<Object> orgIdField = new ArrayList<>(List.of(Constants.OrgId));
            List<Long> orgIdList = organizations.stream().filter(ObjectUtils::isNotEmpty).map(OrganizationsMasterData::getId).toList();
            List<Object> orgIdCriteria = new ArrayList<>(List.of(orgIdField, Constants.IN, List.of(orgIdList)));
            finalCriteria.add(orgIdCriteria);

            finalCriteria.add("and");

            List<Object> activeClient = new ArrayList<>(List.of(Constants.Active));
            List<Object> activeClientCriteria = new ArrayList<>(List.of(activeClient, Constants.EQ, 1));
            finalCriteria.add(activeClientCriteria);

            addressRequest.setCriteriaRequests(finalCriteria);
            addressRequest.setTake(orgIdList.size());
            V1DataResponse addressResponse = v1Service.addressList(addressRequest);
            addressList = jsonHelper.convertValueToList(addressResponse.entities, AddressData.class);
        }
        return addressList;
    }

}
