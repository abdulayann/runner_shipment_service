package com.dpw.runner.shipment.services.utils;


import static com.dpw.runner.shipment.services.commons.constants.PartiesConstants.ACTIVE_CLIENT;
import static com.dpw.runner.shipment.services.commons.constants.PartiesConstants.FULLNAME;
import static com.dpw.runner.shipment.services.commons.constants.PartiesConstants.ORG_ID;
import static com.dpw.runner.shipment.services.utils.CommonUtils.IsStringNullOrEmpty;
import static com.dpw.runner.shipment.services.validator.constants.CustomerBookingConstants.CONSIGNEE_REQUEST;
import static com.dpw.runner.shipment.services.validator.constants.CustomerBookingConstants.CONSIGNOR_REQUEST;
import static com.dpw.runner.shipment.services.validator.constants.CustomerBookingConstants.CUSTOMER_REQUEST;
import static com.dpw.runner.shipment.services.validator.constants.CustomerBookingConstants.NOTIFY_PARTY_REQUEST;

import com.dpw.runner.shipment.services.adapters.config.BillingServiceUrlConfig;
import com.dpw.runner.shipment.services.adapters.impl.BillingServiceAdapter;
import com.dpw.runner.shipment.services.adapters.interfaces.IPlatformServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.RequestAuthContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.EventConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.enums.DBOperationType;
import com.dpw.runner.shipment.services.commons.requests.AuditLogMetaData;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.commons.responses.IRunnerResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IEventDao;
import com.dpw.runner.shipment.services.dao.interfaces.IIntegrationResponseDao;
import com.dpw.runner.shipment.services.dao.interfaces.IShipmentDao;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.EventsRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.platform.ChargesRequest;
import com.dpw.runner.shipment.services.dto.request.platform.DimensionDTO;
import com.dpw.runner.shipment.services.dto.request.platform.DocumentMetaDTO;
import com.dpw.runner.shipment.services.dto.request.platform.HazardousInfoRequest;
import com.dpw.runner.shipment.services.dto.request.platform.LoadRequest;
import com.dpw.runner.shipment.services.dto.request.platform.OrgRequest;
import com.dpw.runner.shipment.services.dto.request.platform.PlatformCreateRequest;
import com.dpw.runner.shipment.services.dto.request.platform.PlatformUpdateRequest;
import com.dpw.runner.shipment.services.dto.request.platform.ReeferInfoRequest;
import com.dpw.runner.shipment.services.dto.request.platform.ReferenceNumbersRequest;
import com.dpw.runner.shipment.services.dto.request.platform.RouteLegRequest;
import com.dpw.runner.shipment.services.dto.request.platform.RouteRequest;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitResponse;
import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.v1.response.UpdateOrgCreditLimitBookingResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1ShipmentCreationResponse;
import com.dpw.runner.shipment.services.entity.BookingCharges;
import com.dpw.runner.shipment.services.entity.Containers;
import com.dpw.runner.shipment.services.entity.CustomerBooking;
import com.dpw.runner.shipment.services.entity.Events;
import com.dpw.runner.shipment.services.entity.IntegrationResponse;
import com.dpw.runner.shipment.services.entity.Packing;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.Routings;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.commons.BaseEntity;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferCarrier;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferChargeType;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferVessels;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.kafka.dto.DocumentDto;
import com.dpw.runner.shipment.services.kafka.dto.DocumentDto.Document;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IAuditLogService;
import com.dpw.runner.shipment.services.service.interfaces.IEventService;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;


/**
 * this helper is used to implement all common methods in all projects like utils function
 */
@Component
@Slf4j
@EnableAsync(proxyTargetClass = true)
public class BookingIntegrationsUtility {

    @Autowired
    private IV1Service v1Service;
    @Autowired
    private JsonHelper jsonHelper;
    @Autowired
    private IPlatformServiceAdapter platformServiceAdapter;
    @Autowired
    private MasterDataUtils masterDataUtils;
    @Autowired
    private ICustomerBookingDao customerBookingDao;
    @Autowired
    private IIntegrationResponseDao integrationResponseDao;
    @Autowired
    private IShipmentService shipmentService;
    @Autowired
    private MasterDataFactory masterDataFactory;
    @Autowired
    private EmailServiceUtility emailServiceUtility;
    @Autowired
    private IShipmentDao shipmentDao;
    @Autowired
    private CommonUtils commonUtils;
    @Autowired
    private IEventDao eventDao;
    @Autowired
    private IEventService eventService;
    @Autowired
    private IAuditLogService auditLogService;

    @Value("${platform.failure.notification.enabled}")
    private Boolean isFailureNotificationEnabled;
    @Value("#{'${platform.failure.notification.to}'.split(',')}")
    private List<String> failureNotificationEmailTo;
    @Value("#{'${platform.failure.notification.cc}'.split(',')}")
    private List<String> failureNotificationEmailCC;
    @Value("${platform.business.code.FCL}")
    private String fclBusinessCode;
    @Value("${platform.business.code.LCL}")
    private String lclBusinessCode;
    @Value("${platform.business.code.LSE}")
    private String lseBusinessCode;
    @Autowired
    private BillingServiceAdapter billingServiceAdapter;
    @Autowired
    private BillingServiceUrlConfig billingServiceUrlConfig;

    static Integer maxAttempts = 5;
    private RetryTemplate retryTemplate = RetryTemplate.builder()
            .maxAttempts(maxAttempts)
            .fixedBackoff(1000)
            .retryOn(Exception.class)
            .build();

    private Map<BookingStatus, String> platformStatusMap = Map.ofEntries(
            Map.entry(BookingStatus.CANCELLED, "CANCELLED"),
            Map.entry(BookingStatus.READY_FOR_SHIPMENT, "CONFIRMED"),
            Map.entry(BookingStatus.PENDING_FOR_KYC, "BOOKED"),
            Map.entry(BookingStatus.PENDING_FOR_CREDIT_LIMIT, "BOOKED")
    );

    public void createBookingInPlatform(CustomerBooking customerBooking) {
        var request = createPlatformCreateRequest(customerBooking);
        try {
            if (!Objects.equals(customerBooking.getTransportType(), Constants.TRANSPORT_MODE_ROA) && !Objects.equals(customerBooking.getTransportType(), Constants.TRANSPORT_MODE_RAI)) {
                platformServiceAdapter.createAtPlatform(request);
                this.saveErrorResponse(customerBooking.getId(), Constants.BOOKING, IntegrationType.PLATFORM_CREATE_BOOKING, Status.SUCCESS, "SAVED SUCESSFULLY");
            }
        } catch (Exception ex) {
            this.saveErrorResponse(customerBooking.getId(), Constants.BOOKING, IntegrationType.PLATFORM_CREATE_BOOKING, Status.FAILED, ex.getLocalizedMessage());
            log.error("Booking Creation error from Platform for booking number: {} with error message: {}", customerBooking.getBookingNumber(), ex.getMessage());
            sendFailureAlerts(jsonHelper.convertToJson(request), jsonHelper.convertToJson(ex.getLocalizedMessage()), customerBooking.getBookingNumber(), null);
        }
    }

    //Not to be used now
    public void updateBookingInPlatform(CustomerBooking customerBooking) {
        var request = createPlatformUpdateRequest(customerBooking);
        try {
            if(!Objects.equals(customerBooking.getTransportType(), Constants.TRANSPORT_MODE_ROA) && !Objects.equals(customerBooking.getTransportType(), Constants.TRANSPORT_MODE_RAI))
                platformServiceAdapter.updateAtPlaform(request);
        } catch (Exception e) {
            this.saveErrorResponse(customerBooking.getId(), Constants.BOOKING, IntegrationType.PLATFORM_UPDATE_BOOKING, Status.FAILED, e.getLocalizedMessage());
            log.error("Booking Update error from Platform for booking number: {} with error message: {}", customerBooking.getBookingNumber(), e.getMessage());
            sendFailureAlerts(jsonHelper.convertToJson(request), jsonHelper.convertToJson(e.getLocalizedMessage()), customerBooking.getBookingNumber(), null);
        }
    }

    private CommonRequestModel createPlatformContainerRequest(String bookingReference) {
        List<LoadRequest> loadRequests = new ArrayList<>();
        PlatformUpdateRequest platformUpdateRequest = PlatformUpdateRequest.builder()
                .booking_reference_code(bookingReference)
                .load(loadRequests)
                .build();

        return CommonRequestModel.buildRequest(platformUpdateRequest);
    }

    public void updateBookingInPlatformEmptyContainer(ShipmentDetails shipmentDetails) {
        if (Objects.equals(shipmentDetails.getBookingType(), CustomerBookingConstants.ONLINE) && !Objects.isNull(shipmentDetails.getBookingReference())) {
            var request = createPlatformContainerRequest(shipmentDetails.getBookingReference());
            try {
                if(!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_ROA) && !Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_RAI))
                    platformServiceAdapter.updateAtPlaform(request);
            } catch (Exception e) {
                this.saveErrorResponse(shipmentDetails.getId(), Constants.SHIPMENT, IntegrationType.PLATFORM_UPDATE_BOOKING, Status.FAILED, e.getLocalizedMessage());
                log.error("Empty Container update error from Platform from Shipment for booking number: {} with error message: {}", shipmentDetails.getBookingReference(), e.getMessage());
                sendFailureAlerts(jsonHelper.convertToJson(request), jsonHelper.convertToJson(e.getLocalizedMessage()), shipmentDetails.getBookingReference(), shipmentDetails.getShipmentId());
            }
        }
    }

    public void updateBookingInPlatform(ShipmentDetails shipmentDetails) {
        if (Objects.equals(shipmentDetails.getBookingType(), CustomerBookingConstants.ONLINE) && !Objects.isNull(shipmentDetails.getBookingReference())) {
            var request = createPlatformUpdateRequestFromShipment(shipmentDetails);
            try {
                if(!Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_ROA) && !Objects.equals(shipmentDetails.getTransportMode(), Constants.TRANSPORT_MODE_RAI))
                    platformServiceAdapter.createAtPlatform(request);
            } catch (Exception e) {
                this.saveErrorResponse(shipmentDetails.getId(), Constants.SHIPMENT, IntegrationType.PLATFORM_CREATE_BOOKING, Status.FAILED, e.getLocalizedMessage());
                log.error("Booking Update error from Platform from Shipment for booking number: {} with error message: {}", shipmentDetails.getBookingReference(), e.getMessage());
                sendFailureAlerts(jsonHelper.convertToJson(request), jsonHelper.convertToJson(e.getLocalizedMessage()), shipmentDetails.getBookingReference(), shipmentDetails.getShipmentId());
            }
        }
    }

    public ResponseEntity<V1ShipmentCreationResponse> createShipmentInV1(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled, UUID shipmentGuid, HttpHeaders headers) {
        try {
            var response = v1Service.createBooking(customerBooking, isShipmentEnabled, isBillingEnabled, shipmentGuid, headers);
            this.saveErrorResponse(customerBooking.getId(), Constants.BOOKING, IntegrationType.V1_SHIPMENT_CREATION, Status.SUCCESS, "SAVED SUCESSFULLY");
            return response;
        } catch (Exception ex) {
            this.saveErrorResponse(customerBooking.getId(), Constants.BOOKING, IntegrationType.V1_SHIPMENT_CREATION, Status.FAILED, ex.getLocalizedMessage());
            log.error("Booking Creation error from V1 for booking number: {} with error message: {}", customerBooking.getBookingNumber(), ex.getMessage());
            throw ex;
        }
    }

    @Async
    public void createShipment(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled, ShipmentDetailsResponse shipmentResponse, HttpHeaders headers) {
        retryTemplate.execute(ctx -> {
            log.info("Current retry : {}", ctx.getRetryCount());
            if (ctx.getLastThrowable() != null) {
                log.error("V1 error for bill creation -> {}", ctx.getLastThrowable().getMessage());
            }
            try {
                try {
                    TimeUnit.SECONDS.sleep(5);
                } catch (Exception ex) {
                    log.error("Wait failed due to {}", ex.getMessage());
                }
                this.createBill(customerBooking, false, true, shipmentResponse, headers);
                customerBookingDao.updateBillStatus(customerBooking.getId(), true);
            } catch (Exception e) {
                log.error("Event: {} Bill creation  for shipment with booking reference {} failed due to following error: {}", IntegrationType.V1_SHIPMENT_CREATION,
                        shipmentResponse.getBookingReference(), e.getMessage());
                throw e;
            }
            return ResponseHelper.buildSuccessResponse();
        });
    }

    private void createBill(CustomerBooking customerBooking, boolean isShipmentEnabled,
            boolean isBillingEnabled, ShipmentDetailsResponse shipmentDetailsResponse, HttpHeaders headers) {

        if (Boolean.TRUE.equals(billingServiceUrlConfig.getEnableBillingIntegration())) {
            billingServiceAdapter.createBillV2(customerBooking, isShipmentEnabled,
                    isBillingEnabled, shipmentDetailsResponse, headers);
        } else {
            this.createShipmentInV1(customerBooking, false, true, shipmentDetailsResponse.getGuid(), headers);
        }
    }

    public ResponseEntity<UpdateOrgCreditLimitBookingResponse> updateOrgCreditLimitFromBooking(CheckCreditLimitResponse request) {
        try {
            return v1Service.updateOrgCreditLimitFromBooking(request);
        } catch (Exception ex) {
            log.error("Error updating Credit Limit for Org with CustomerIdentifier: {} with error message: {}", request.getAccountNumber(), ex.getMessage());
            throw ex;
        }
    }

    public ResponseEntity<IRunnerResponse> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws RunnerException {
        try {
            var response = shipmentService.createShipmentInV2(customerBookingRequest);
            return response;
        } catch (Exception ex) {
            log.error("Shipment Creation failed for booking number {} with error message: {}", customerBookingRequest.getBookingNumber(), ex.getMessage());
            throw ex;
        }
    }


    private void saveErrorResponse(Long entityId, String entityType, IntegrationType integrationType, Status status, String message) {
        IntegrationResponse response = IntegrationResponse.builder()
                .entityId(entityId).entityType(entityType).integrationType(integrationType).status(status)
                .response_message(jsonHelper.convertToJson(message))
                .build();
        integrationResponseDao.save(response);
    }

    private CommonRequestModel createPlatformCreateRequest(CustomerBooking customerBooking) {
        var carrierDetails = customerBooking.getCarrierDetails();
        PlatformCreateRequest platformCreateRequest = PlatformCreateRequest.builder()
                .booking_ref_code(customerBooking.getBookingNumber())
                .origin_code(carrierDetails.getOrigin())
                .destination_code(carrierDetails.getDestination())
                .pol(carrierDetails.getOriginPort())
                .pod(carrierDetails.getDestinationPort())
                .contract_id(StringUtility.isEmpty(customerBooking.getContractId()) ? null : customerBooking.getContractId())
                .created_at(customerBooking.getCreatedAt())
                .customer_org_id(customerBooking.getCustomer().getOrgCode())
                .customer_email(customerBooking.getCustomerEmail())
                .business_code(StringUtility.isNotEmpty(customerBooking.getBusinessCode()) ? customerBooking.getBusinessCode() : getBusinessCode(customerBooking.getCargoType()))
                .bill_to_party(Collections.singletonList(createOrgRequest(customerBooking.getCustomer())))
                .parent_contract_id(StringUtility.isEmpty(customerBooking.getParentContractId()) ? null : customerBooking.getParentContractId())
                .branch_info(ListContractResponse.BranchInfo.builder().
                        id(customerBooking.getSalesBranch()).
                        sales_agent_primary_email(customerBooking.getPrimarySalesAgentEmail()).
                        sales_agent_secondary_email(customerBooking.getSecondarySalesAgentEmail()).
                        build())
                .mainLegCarrierCode(getCarrierSCACCodeFromItemValue(carrierDetails.getShippingLine()))
                .minTransitHours(carrierDetails.getMinTransitHours())
                .maxTransitHours(carrierDetails.getMaxTransitHours())
                .charges(createCharges(customerBooking))
                .transportMode(customerBooking.getTransportType())
                .shipmentMovement(customerBooking.getDirection())
                .isDg(customerBooking.getIsDg())
                .carrierDisplayName(masterDataUtils.getCarrierName(carrierDetails.getShippingLine()))
                .vesselName(masterDataUtils.getVesselName(carrierDetails.getVessel()))
                .voyage(carrierDetails.getVoyage())
                .load(createLoad(customerBooking))
                .route(createRoute(customerBooking))
                .source(CustomerBookingConstants.RUNNER)
                .status(platformStatusMap.get(customerBooking.getBookingStatus()))
                .referenceNumbers(new ArrayList<>())
                .addresses(getPartyAddresses(customerBooking.getCustomer(), customerBooking.getConsignor(), customerBooking.getConsignee(), customerBooking.getNotifyParty()))
                .branchId(UserContext.getUser().getTenantId())
                .build();
        return CommonRequestModel.builder().data(platformCreateRequest).build();
    }

    private List<Map<String, Object>> getPartyAddresses(Parties client, Parties consignor, Parties consignee, Parties notifyParty) {
        List<Map<String, Object>> response = new ArrayList<>();
        if(client != null && client.getOrgData() != null && client.getAddressData() != null) {
            response.add(getPartyObjectForPlatform(client, "CLIENT"));
        }
        if(consignor != null && consignor.getOrgData() != null && consignor.getAddressData() != null) {
            response.add(getPartyObjectForPlatform(consignor, "CONSIGNOR"));
        }
        if(consignee != null && consignee.getOrgData() != null && consignee.getAddressData() != null) {
            response.add(getPartyObjectForPlatform(consignee, "CONSIGNEE"));
        }
        if(notifyParty != null && notifyParty.getOrgData() != null && notifyParty.getAddressData() != null) {
            response.add(getPartyObjectForPlatform(notifyParty, "NOTIFY_PARTY1"));
        }
        return response;
    }

    private Map<String, Object> getPartyObjectForPlatform(Parties parties, String partyType) {
        Map<String, Object> map = new HashMap<>();
        // Org Data
        if(parties.getOrgData().containsKey(FULLNAME))
            map.put("name", parties.getOrgData().get(FULLNAME));
        if(parties.getOrgData().containsKey(PartiesConstants.ORGANIZATION_CODE))
            map.put("org_code", parties.getOrgData().get(PartiesConstants.ORGANIZATION_CODE));
        map.put("address_type", partyType);
        // Address Data
        if(parties.getAddressData().containsKey(PartiesConstants.ADDRESS1))
            map.put("address1", parties.getAddressData().get(PartiesConstants.ADDRESS1));
        if(parties.getAddressData().containsKey(PartiesConstants.ADDRESS2))
            map.put("address2", parties.getAddressData().get(PartiesConstants.ADDRESS2));
        if(parties.getAddressData().containsKey(PartiesConstants.COUNTRY))
            map.put("country", parties.getAddressData().get(PartiesConstants.COUNTRY));
        if(parties.getAddressData().containsKey(PartiesConstants.CITY))
            map.put("city", parties.getAddressData().get(PartiesConstants.CITY));
        if(parties.getAddressData().containsKey(PartiesConstants.STATE))
            map.put("state", parties.getAddressData().get(PartiesConstants.STATE));
        if(parties.getAddressData().containsKey(PartiesConstants.ZIP_POST_CODE))
            map.put("zip_postcode", parties.getAddressData().get(PartiesConstants.ZIP_POST_CODE));
        if(parties.getAddressData().containsKey(PartiesConstants.CONTACT_PHONE))
            map.put("contact_phone", parties.getAddressData().get(PartiesConstants.CONTACT_PHONE));
        if(parties.getAddressData().containsKey(PartiesConstants.EMAIL))
            map.put("email", parties.getAddressData().get(PartiesConstants.EMAIL));
        if(parties.getAddressData().containsKey(PartiesConstants.ADDRESS_SHORT_CODE))
            map.put("office_code", parties.getAddressData().get(PartiesConstants.ADDRESS_SHORT_CODE));
        return map;
    }

    private String getCarrierSCACCodeFromItemValue(String itemValue) {
        if(IsStringNullOrEmpty(itemValue))
            return null;
        Set<String> carrierCodes = new HashSet<>();
        carrierCodes.add(itemValue);
        Map<String, EntityTransferCarrier> map = masterDataUtils.fetchInBulkCarriers(carrierCodes);
        if(map.containsKey(itemValue))
            return map.get(itemValue).Identifier1;
        return null;
    }

    public List<String> createEmailIds(String primaryEmail, String secondaryEmail)
    {
        if(primaryEmail == null)
            return null;
        if(secondaryEmail == null)
            return List.of(primaryEmail);
        return Arrays.asList(primaryEmail, secondaryEmail);
    }

    private List<LoadRequest> createLoad(final CustomerBooking customerBooking) {
        List<LoadRequest> loadRequests = new ArrayList<>();
        //Container -> FCL
        if (customerBooking.getCargoType() != null && customerBooking.getCargoType().equals("FCL")) {
            List<Containers> containers = customerBooking.getContainersList();
            if(Objects.isNull(containers) || containers.isEmpty())
                return loadRequests;
            containers.forEach(container -> {
                loadRequests.add(LoadRequest.builder()
                        .load_uuid(container.getGuid())
                        .load_type(customerBooking.getCargoType())
                        .container_type_code(container.getContainerCode())
                        .product_category_code(container.getCommodityGroup())
                        .pkg_type(null)
                        .is_package(false)
                        .weight(container.getGrossWeight())
                        .quantity(container.getContainerCount())
                        .weight_uom(container.getGrossWeightUnit())
                        .quantity_uom("unit")
                        .volume(container.getGrossVolume())
                        .volume_uom(container.getGrossVolumeUnit())
                        .dimensions(null) // Resolved
                        .build());
            });
        }

        if (customerBooking.getCargoType() != null && (customerBooking.getCargoType().equals("LCL") || customerBooking.getCargoType().equals("LSE"))) {
            List<Packing> packings = customerBooking.getPackingList();
            if(Objects.isNull(packings) || packings.isEmpty())
                return loadRequests;
            packings.forEach(packing -> {
                loadRequests.add(LoadRequest.builder()
                        .load_uuid(packing.getGuid())
                        .load_type(customerBooking.getCargoType())
                        .container_type_code(null)
                        .product_category_code(packing.getCommodityGroup())
                        .pkg_type(packing.getPacksType())
                        .is_package(true)
                        .weight(packing.getWeight())
                        .quantity(Long.valueOf(packing.getPacks()))
                        .weight_uom(packing.getWeightUnit())
                        .quantity_uom("unit")
                        .volume(packing.getVolume())
                        .volume_uom(packing.getVolumeUnit())
                        .dimensions(getDimension(customerBooking.getCargoType(), packing))
                        .build());
            });
        }

        return loadRequests;
    }


    private List<LoadRequest> createLoad(final ShipmentDetails shipmentDetails) {
        List<LoadRequest> loadRequests = new ArrayList<>();
        if (Objects.equals(shipmentDetails.getShipmentType(), Constants.CARGO_TYPE_FCL)) {
            Set<Containers> containers = shipmentDetails.getContainersList();
            if(Objects.isNull(containers) || containers.isEmpty())
                return loadRequests;
            containers.forEach(container -> {
                loadRequests.add(LoadRequest.builder()
                        .load_uuid(container.getGuid())
                        .load_type(shipmentDetails.getShipmentType())
                        .container_type_code(container.getContainerCode())
                        .pkg_type(container.getPacksType())
                        .is_package(false)
                        .product_category_code(container.getCommodityGroup())
                        .product_name(getItemDescription(MasterDataType.COMMODITY_GROUP, container.getCommodityGroup()))
                        .is_reefer(container.getIsReefer())
                        .reefer_info(ReeferInfoRequest.builder().temperature(!Objects.isNull(container.getMinTemp()) ? container.getMinTemp().intValue() : null).build())
                        .is_hazardous(container.getHazardous() != null && container.getHazardous())
                        .hazardous_info(HazardousInfoRequest.builder().product_un_id(container.getUnNumber()).product_class(container.getDgClass()).build()) // TODO
                        .weight(container.getGrossWeight())
                        .weight_uom(container.getGrossWeightUnit())
                        .quantity(container.getContainerCount())
                        .quantity_uom(CustomerBookingConstants.UNIT)
                        .volume(container.getGrossVolume())
                        .volume_uom(container.getGrossVolumeUnit())
                        .build());
            });
        }

        if (Objects.equals(shipmentDetails.getShipmentType(), Constants.SHIPMENT_TYPE_LCL) || Objects.equals(shipmentDetails.getShipmentType(), Constants.SHIPMENT_TYPE_LSE)) {
            List<Packing> packings = shipmentDetails.getPackingList();
            if(Objects.isNull(packings) || packings.isEmpty())
                return loadRequests;
            packings.forEach(packing -> {
                loadRequests.add(LoadRequest.builder()
                        .load_uuid(packing.getGuid())
                        .load_type(shipmentDetails.getShipmentType())
                        .pkg_type(packing.getPacksType())
                        .is_package(true)
                        .product_category_code(packing.getCommodityGroup())
                        .product_name(getItemDescription(MasterDataType.COMMODITY_GROUP, packing.getCommodityGroup()))
                        .weight(packing.getWeight())
                        .weight_uom(packing.getWeightUnit())
                        .quantity(Long.valueOf(packing.getPacks()))
                        .quantity_uom(CustomerBookingConstants.UNIT)
                        .volume(packing.getVolume())
                        .volume_uom(packing.getVolumeUnit())
                        .dimensions(getDimension(shipmentDetails.getShipmentType(), packing))
                        .is_hazardous(packing.getHazardous() != null && packing.getHazardous())
                        .build());
            });
        }

        return loadRequests;
    }

    private String getItemDescription(MasterDataType type, String itemValue) {
        var masterData = masterDataUtils.getMasterListData(type, itemValue);
        return Objects.isNull(masterData) ? null : masterData.getItemDescription();
    }

    private RouteRequest createRoute(CustomerBooking customerBooking) {
        List<RouteLegRequest> legRequestList = new ArrayList<>();
        List<Routings> routingsList = customerBooking.getRoutingList();

        for (int counter = 0; counter < routingsList.size(); counter++) {
            legRequestList.add(RouteLegRequest.builder()
                    .destination_code(routingsList.get(counter).getPod())
                    .origin_code(routingsList.get(counter).getPol())
                    .order(String.valueOf(routingsList.get(counter).getLeg()))
                    .transport_mode(routingsList.get(counter).getMode() != null ? routingsList.get(counter).getMode() : customerBooking.getTransportType())
                    .build());
        }

        return RouteRequest.builder()
                .legs(legRequestList)
                .build();
    }

    private RouteRequest createRoute(ShipmentDetails shipmentDetails) {
        List<RouteLegRequest> legRequestList = new ArrayList<>();
        List<Routings> routingsList = shipmentDetails.getRoutingsList();
        if(routingsList == null || routingsList.isEmpty())
            return RouteRequest.builder().legs(legRequestList).build();
        Map<String, EntityTransferVessels> entityTransferVesselsMap = masterDataUtils.fetchInBulkVessels(routingsList.stream().map(Routings::getVesselName).filter(Objects::nonNull).collect(Collectors.toSet()));
        Map<String, EntityTransferCarrier> entityTransferCarrierMap = masterDataUtils.fetchInBulkCarriers(routingsList.stream().map(Routings::getCarrier).filter(Objects::nonNull).collect(Collectors.toSet()));
        processRoutingList(shipmentDetails, routingsList, entityTransferVesselsMap, entityTransferCarrierMap, legRequestList);

        return RouteRequest.builder().legs(legRequestList).build();
    }

    private void processRoutingList(ShipmentDetails shipmentDetails, List<Routings> routingsList, Map<String, EntityTransferVessels> entityTransferVesselsMap, Map<String, EntityTransferCarrier> entityTransferCarrierMap, List<RouteLegRequest> legRequestList) {
        for (int counter = 0; counter < routingsList.size(); counter++) {
            var vessel = entityTransferVesselsMap.get(routingsList.get(counter).getVesselName());
            var carrier = entityTransferCarrierMap.get(routingsList.get(counter).getCarrier());
            legRequestList.add(RouteLegRequest.builder()
                    .destination_code(routingsList.get(counter).getPod())
                    .origin_code(routingsList.get(counter).getPol())
                    .order(String.valueOf(routingsList.get(counter).getLeg()))
                    .transport_mode(routingsList.get(counter).getMode() != null ? routingsList.get(counter).getMode() : shipmentDetails.getTransportMode())
                    .voyageNumber(routingsList.get(counter).getVoyage())
                    .flightNumber(routingsList.get(counter).getFlightNumber())
                    .eta(routingsList.get(counter).getEta())
                    .ets(routingsList.get(counter).getEtd())
                    .ata(routingsList.get(counter).getAta())
                    .ats(routingsList.get(counter).getAtd())
                    .carrierScacCode(!Objects.isNull(carrier) ? carrier.getIdentifier1() : null)
                    .airlinePrefix(!Objects.isNull(carrier) ? carrier.getAirlineCode() : null)
                    .airlineIataCode(!Objects.isNull(carrier) ? carrier.getIATACode() : null)
                    .vesselName(!Objects.isNull(vessel) ? vessel.getName() : null)
                    .vesselImo(!Objects.isNull(vessel) ? vessel.getImo() : null)
                    .vesselMmsi(!Objects.isNull(vessel) ? vessel.getMmsi() : null)
                    .build());
        }
    }

    private DimensionDTO getDimension(String cargoType, Packing packing) {
        if (Objects.equals(cargoType, "LSE") || Objects.equals(cargoType, "LCL"))
            return DimensionDTO.builder()
                    .length(packing.getLength())
                    .width(packing.getWidth())
                    .height(packing.getHeight())
                    .uom(packing.getLengthUnit())
                    .build();
        return null;
    }

    private List<ChargesRequest> createCharges(CustomerBooking customerBooking) {
        var bookingCharges = customerBooking.getBookingCharges();
        List<ChargesRequest> charges = new ArrayList<>();
        if(Objects.isNull(bookingCharges) || bookingCharges.isEmpty())
            return charges;
        List<String> chargeTypes = bookingCharges.stream().map(BookingCharges::getChargeType).toList();
        Map<String, EntityTransferChargeType> chargeTypeMap = masterDataUtils.getChargeTypes(chargeTypes);
        log.info("ChargeTypeMap from V1 Charge Codes: "+ jsonHelper.convertToJson(chargeTypeMap));

        bookingCharges.forEach(
                bookingCharge -> {
                    charges.add(
                            ChargesRequest.builder()
                                    .load_uuid(Objects.isNull(bookingCharge.getContainersList()) || bookingCharge.getContainersList().isEmpty() ? null : bookingCharge.getContainersList().stream().map(c -> c.getGuid()).collect(Collectors.toList()))
                                    .charge_group(chargeTypeMap.containsKey(bookingCharge.getChargeType()) ? chargeTypeMap.get(bookingCharge.getChargeType()).getServices() : null)
                                    .charge_code(bookingCharge.getChargeType())
                                    .charge_code_desc(chargeTypeMap.containsKey(bookingCharge.getChargeType()) ? chargeTypeMap.get(bookingCharge.getChargeType()).getDescription() : null)
                                    .base_charge_value(bookingCharge.getLocalSellAmount())
                                    .charge_value(bookingCharge.getOverseasSellAmount())
                                    .base_currency(bookingCharge.getLocalSellCurrency())
                                    .charge_currency(bookingCharge.getOverseasSellCurrency())
                                    .exchange_rate(bookingCharge.getSellExchange())
                                    .is_grouped(bookingCharge.getContainersList() != null && bookingCharge.getContainersList().size() > 1)
                                    .taxes(null) // optional
                                    .charge_id(bookingCharge.getGuid().toString())
                                    .build()
                    );
                }
        );

        return charges;
    }

    private CommonRequestModel createPlatformUpdateRequest(@NonNull final CustomerBooking customerBooking) {
        var carrierDetails = customerBooking.getCarrierDetails();
        PlatformUpdateRequest platformUpdateRequest = PlatformUpdateRequest.builder()
                .booking_reference_code(customerBooking.getBookingNumber())
                .contractId(customerBooking.getContractId())
                .parentContractId(customerBooking.getParentContractId())
                .origin_code(carrierDetails.getOrigin())
                .destination_code(carrierDetails.getDestination())
                .customer_email(customerBooking.getCustomerEmail())
                .pol(carrierDetails.getOriginPort())
                .pod(carrierDetails.getDestinationPort())
                .air_carrier_details(null)
                .status(platformStatusMap.get(customerBooking.getBookingStatus()))
                .pickup_date(null)
                .eta(carrierDetails.getEta())
                .ets(carrierDetails.getEtd())
                .charges(createCharges(customerBooking))
                .carrier_code(getCarrierSCACCodeFromItemValue(StringUtility.getNullIfEmpty(carrierDetails.getShippingLine())))
                .carrier_display_name(masterDataUtils.getCarrierName(carrierDetails.getShippingLine()))
                .vessel_name(masterDataUtils.getVesselName(carrierDetails.getVessel()))
                .voyage(carrierDetails.getVoyage())
                .transportMode(customerBooking.getTransportType())
                .shipmentMovement(customerBooking.getDirection())
                .isDg(customerBooking.getIsDg())
                .load(createLoad(customerBooking))
                .route(createRoute(customerBooking))
                .source(CustomerBookingConstants.RUNNER)
                .branchId(UserContext.getUser().getTenantId())
                .build();
        return CommonRequestModel.builder().data(platformUpdateRequest).build();
    }

    private CommonRequestModel createPlatformUpdateRequestFromShipment(@NonNull final ShipmentDetails shipmentDetails) {
        var carrierDetails = shipmentDetails.getCarrierDetails();
        PlatformCreateRequest platformUpdateRequest = PlatformCreateRequest.builder()
                .booking_ref_code(StringUtility.getNullIfEmpty(shipmentDetails.getBookingReference()))
                .origin_code(StringUtility.getNullIfEmpty(carrierDetails.getOrigin()))
                .destination_code(StringUtility.getNullIfEmpty(carrierDetails.getDestination()))
                .load(createLoad(shipmentDetails))
                .pol(StringUtility.getNullIfEmpty(carrierDetails.getOriginPort()))
                .pod(StringUtility.getNullIfEmpty(carrierDetails.getDestinationPort()))
                .mainLegCarrierCode(getCarrierSCACCodeFromItemValue(StringUtility.getNullIfEmpty(carrierDetails.getShippingLine())))
                .carrierDisplayName(masterDataUtils.getCarrierName(carrierDetails.getShippingLine()))
                .vesselName(masterDataUtils.getVesselName(carrierDetails.getVessel()))
                .status(mapBookingStatus(ShipmentStatus.fromValue(shipmentDetails.getStatus())))
                .eta(carrierDetails.getEta())
                .ets(carrierDetails.getEtd())
                .ata(carrierDetails.getAta())
                .ats(carrierDetails.getAtd())
                .contract_id(shipmentDetails.getContractId())
                .parent_contract_id(shipmentDetails.getParentContractId())
                .voyage(StringUtility.getNullIfEmpty(carrierDetails.getVoyage()))
                .transportMode(shipmentDetails.getTransportMode())
                .isDg(shipmentDetails.getContainsHazardous())
                .shipmentMovement(shipmentDetails.getDirection())
                .route(createRoute(shipmentDetails))
                .referenceNumbers(createReferenceNumbers(shipmentDetails))
                .source(CustomerBookingConstants.RUNNER)
                .business_code(getBusinessCode(shipmentDetails.getShipmentType()))
                .customer_org_id(shipmentDetails.getClient().getOrgCode())
                .bill_to_party(Collections.singletonList(createOrgRequest(shipmentDetails.getClient())))
                .branch_info(ListContractResponse.BranchInfo.builder().
                        id(StringUtility.isEmpty(shipmentDetails.getSalesBranch()) ? null : shipmentDetails.getSalesBranch()).
                        sales_agent_primary_email(StringUtility.isEmpty(shipmentDetails.getPrimarySalesAgentEmail()) ? null : shipmentDetails.getPrimarySalesAgentEmail()).
                        sales_agent_secondary_email(StringUtility.isEmpty(shipmentDetails.getSecondarySalesAgentEmail()) ? null : shipmentDetails.getSecondarySalesAgentEmail()).
                        build())
                .created_at(shipmentDetails.getBookingCreatedDate())
                .addresses(getPartyAddresses(shipmentDetails.getClient(), shipmentDetails.getConsigner(), shipmentDetails.getConsignee(), shipmentDetails.getAdditionalDetails().getNotifyParty()))
                .build();
        return CommonRequestModel.builder().data(platformUpdateRequest).build();
    }

    private List<ReferenceNumbersRequest> createReferenceNumbers(ShipmentDetails shipmentDetails) {
        List<ReferenceNumbersRequest> requestList = new ArrayList<>();
        if(!StringUtility.isEmpty(shipmentDetails.getHouseBill()))
        {
            Map<String, String> metaMap = new HashMap<>();
            if(Boolean.TRUE.equals(shipmentDetails.getAdditionalDetails().getPrintedOriginal())){
                metaMap.put("Status", "ORIGINAL");
            }
            else{
                metaMap.put("Status", "DRAFT");
            }
            requestList.add(ReferenceNumbersRequest.builder()
                    .key("HBL")
                    .value(shipmentDetails.getHouseBill())
                    .meta(metaMap)
                    .build());
        }
        if(!StringUtility.isEmpty(shipmentDetails.getMasterBill()))
        {
            requestList.add(ReferenceNumbersRequest.builder()
                    .key("MBL")
                    .value(shipmentDetails.getMasterBill())
                    .meta(new HashMap<>())
                    .build());
        }
        var referenceNumbers = shipmentDetails.getReferenceNumbersList();
        if(!Objects.isNull(referenceNumbers) && !referenceNumbers.isEmpty())
        {
            referenceNumbers.forEach(r -> {
                Map<String, String> metaMap = new HashMap<>();
                metaMap.put("country_of_issue", r.getCountryOfIssue());
                requestList.add(ReferenceNumbersRequest.builder()
                                .key(r.getType())
                                .value(r.getReferenceNumber())
                                .meta(metaMap)
                        .build());
            });
        }
        return requestList;
    }

    private String mapBookingStatus(ShipmentStatus status) {
        if (status == ShipmentStatus.Cancelled)
            return ShipmentConstants.CANCELLED;
        else
            return ShipmentConstants.CONFIRMED;
    }
    private OrgRequest createOrgRequest(Parties parties) {
        return OrgRequest.builder()
                .org_id(parties.getOrgCode())
                .office_id(parties.getAddressCode())
                .org_name(String.valueOf(parties.getOrgData().get(FULLNAME)))
                .build();
    }

    private void setOrgDataInPartiesRequest(String partyType, Map<String, PartiesRequest> requestMap, Map organizationRow, PartiesRequest party,
                                            List<Long> orgIds) {
        if(requestMap.containsKey(partyType) && requestMap.get(partyType).getOrgCode().equals(organizationRow.get(PartiesConstants.ORGANIZATION_CODE))) {
            party.setOrgCode(organizationRow.get(PartiesConstants.ORGANIZATION_CODE).toString());
            party.setOrgData(organizationRow);
            if(organizationRow.containsKey(PartiesConstants.ID)) {
                party.setOrgId(String.valueOf(organizationRow.get(PartiesConstants.ID)));
                orgIds.add(Long.valueOf(organizationRow.get(PartiesConstants.ID).toString()));
            }
        }
    }

    private void setAddressDataInPartiesRequest(String partyType, Map<String, PartiesRequest> requestMap, Map addressRow, PartiesRequest party) {
        if(addressRow.containsKey(ORG_ID) && party.getOrgId() != null && requestMap.containsKey(partyType) &&
                Objects.equals(Long.valueOf(addressRow.get(ORG_ID).toString()), Long.valueOf(party.getOrgId())) &&
                Objects.equals(requestMap.get(partyType).getAddressCode(), addressRow.get(PartiesConstants.ADDRESS_SHORT_CODE))) {
            party.setAddressCode(addressRow.get(PartiesConstants.ADDRESS_SHORT_CODE).toString());
            party.setAddressData(addressRow);
            if(addressRow.containsKey(PartiesConstants.ID)) {
                party.setAddressId(String.valueOf(addressRow.get(PartiesConstants.ID)));
            }
        }
    }

    public void transformOrgAndAddressPayloadToGivenParties(Map<String, PartiesRequest> requestMap) {
        if(requestMap == null || requestMap.isEmpty())
            return;
        try {
            List<String> orgCodes = new ArrayList<>();
            List<String> addressCodes = new ArrayList<>();
            for(Map.Entry<String, PartiesRequest> entry: requestMap.entrySet()) {
                orgCodes.add(entry.getValue().getOrgCode());
                addressCodes.add(entry.getValue().getAddressCode());
            }
            CommonV1ListRequest orgRequest = createCriteriaForTwoFields(PartiesConstants.ORGANIZATION_CODE, "in", List.of(orgCodes), ACTIVE_CLIENT, "=", Boolean.TRUE);
            DependentServiceResponse v1OrgResponse = masterDataFactory.getMasterDataService().fetchOrganizationData(orgRequest);
            if (v1OrgResponse.getData() == null || ((ArrayList)v1OrgResponse.getData()).isEmpty()) {
                log.error("Request: {} || No organization exist in Runner V1 with OrgCodes: {}", LoggerHelper.getRequestIdFromMDC() ,orgCodes);
                throw new DataRetrievalFailureException("No organization exist in Runner V1 with OrgCodes: " + orgCodes);
            }
            PartiesRequest client = new PartiesRequest();
            PartiesRequest consignor = new PartiesRequest();
            PartiesRequest consignee = new PartiesRequest();
            PartiesRequest notifyParty = new PartiesRequest();
            List<Object> orgRows = jsonHelper.convertValueToList(v1OrgResponse.getData(), Object.class);
            List<Long> orgIds = new ArrayList<>();
            for(Object orgRow: orgRows) {
                Map organizationRow = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(orgRow));
                setOrgDataInPartiesRequest(CUSTOMER_REQUEST, requestMap, organizationRow, client, orgIds);
                setOrgDataInPartiesRequest(CONSIGNOR_REQUEST, requestMap, organizationRow, consignor, orgIds);
                setOrgDataInPartiesRequest(CONSIGNEE_REQUEST, requestMap, organizationRow, consignee, orgIds);
                setOrgDataInPartiesRequest(NOTIFY_PARTY_REQUEST, requestMap, organizationRow, notifyParty, orgIds);
            }

            CommonV1ListRequest addressRequest = createCriteriaForThreeFields(
                    ORG_ID, "in", List.of(orgIds),
                    PartiesConstants.ADDRESS_SHORT_CODE, "in", List.of(addressCodes),
                    "Active", "=", Boolean.TRUE);
            DependentServiceResponse v1AddressResponse = masterDataFactory.getMasterDataService().addressList(addressRequest);
            validateV1OrgResponseData(v1OrgResponse, v1AddressResponse, orgCodes, addressCodes);
            processAddressRows(requestMap, v1AddressResponse, client, consignor, consignee, notifyParty);
            if(client.getOrgId() != null && client.getAddressId() != null) {
                requestMap.put("Customer", client);
            }
            if(consignor.getOrgId() != null && consignor.getAddressId() != null) {
                requestMap.put("Consignor", consignor);
            }
            if(consignee.getOrgId() != null && consignee.getAddressId() != null) {
                requestMap.put("Consignee", consignee);
            }
            if(notifyParty.getOrgId() != null && notifyParty.getAddressId() != null) {
                requestMap.put("Notify Party", notifyParty);
            }
        }
        catch (Exception ex) {
            log.error("Error occurred during Organization fetch from V1 with exception: {}", ex.getLocalizedMessage());
            throw new DataRetrievalFailureException(ex.getMessage());
        }

    }

    private void processAddressRows(Map<String, PartiesRequest> requestMap, DependentServiceResponse v1AddressResponse, PartiesRequest client, PartiesRequest consignor, PartiesRequest consignee, PartiesRequest notifyParty) {
        List<Object> addressRows = jsonHelper.convertValueToList(v1AddressResponse.getData(), Object.class);
        for(Object addressRow: addressRows) {
            Map addressRowMap = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(addressRow));
            setAddressDataInPartiesRequest(CUSTOMER_REQUEST, requestMap, addressRowMap, client);
            setAddressDataInPartiesRequest(CONSIGNOR_REQUEST, requestMap, addressRowMap, consignor);
            setAddressDataInPartiesRequest(CONSIGNEE_REQUEST, requestMap, addressRowMap, consignee);
            setAddressDataInPartiesRequest(NOTIFY_PARTY_REQUEST, requestMap, addressRowMap, notifyParty);
        }
    }

    private void validateV1OrgResponseData(DependentServiceResponse v1OrgResponse, DependentServiceResponse v1AddressResponse, List<String> orgCodes, List<String> addressCodes) {
        if (v1OrgResponse.getData() == null || ((ArrayList) v1AddressResponse.getData()).isEmpty()) {
            log.error("Request: {} || No Address exist in Runner V1 with OrgCodes: {} with AddressCodes: {}", LoggerHelper.getRequestIdFromMDC(), orgCodes, addressCodes);
            throw new DataRetrievalFailureException("No Address exist in Runner V1 with OrgCodes: " + orgCodes + " with AddressCodes: " + addressCodes);
        }
    }

    public void transformOrgAndAddressPayload(PartiesRequest request, String addressCode, String orgCode) {
        try {
            CommonV1ListRequest orgRequest = createCriteriaForTwoFields(PartiesConstants.ORGANIZATION_CODE, "=", orgCode, ACTIVE_CLIENT, "=", Boolean.TRUE);
            DependentServiceResponse v1OrgResponse = masterDataFactory.getMasterDataService().fetchOrganizationData(orgRequest);
            if (v1OrgResponse.getData() == null || ((ArrayList)v1OrgResponse.getData()).isEmpty()) {
                log.error("Request: {} || No organization exist in Runner V1 with OrgCode: {}", LoggerHelper.getRequestIdFromMDC() ,orgCode);
                throw new DataRetrievalFailureException("No organization exist in Runner V1 with OrgCode: " + orgCode);
            }
            Map organizationRow = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(((ArrayList)v1OrgResponse.getData()).get(0)));
            if(organizationRow.containsKey(PartiesConstants.ID))
                request.setOrgId(String.valueOf(organizationRow.get(PartiesConstants.ID)));
            request.setOrgData(organizationRow);

            CommonV1ListRequest addressRequest = createCriteriaForThreeFields(
                    ORG_ID, "=", organizationRow.get(PartiesConstants.ID),
                    PartiesConstants.ADDRESS_SHORT_CODE, "=", addressCode,
                    "Active", "=", Boolean.TRUE);
            DependentServiceResponse v1AddressResponse = masterDataFactory.getMasterDataService().addressList(addressRequest);
            if (v1OrgResponse.getData() == null || ((ArrayList)v1AddressResponse.getData()).isEmpty()) {
                log.error("Request: {} || No Address exist in Runner V1 with OrgCode: {} with AddressCode: {}", LoggerHelper.getRequestIdFromMDC(), orgCode , addressCode);
                throw new DataRetrievalFailureException("No Address exist in Runner V1 with OrgCode: " + orgCode + " with AddressCode: " + addressCode);
            }
            Map addressRow = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(((ArrayList)v1AddressResponse.getData()).get(0)));
            if(addressRow.containsKey(PartiesConstants.ID))
                request.setAddressId(String.valueOf(addressRow.get(PartiesConstants.ID)));
            request.setAddressData(addressRow);
        }
        catch (Exception ex) {
            log.error("Error occurred during Organization fetch from V1 with exception: {}", ex.getLocalizedMessage());
            throw new DataRetrievalFailureException(ex.getMessage());
        }

    }

    private CommonV1ListRequest createCriteriaForTwoFields(String field1, String operator1, Object value1, String field2, String operator2, Object value2) {
        List<Object> field1_ = new ArrayList<>(List.of(field1));
        List<Object> criteria1 = new ArrayList<>(List.of(field1_, operator1, value1));

        List<Object> field2_ = new ArrayList<>(List.of(field2));
        List<Object> criteria2 = new ArrayList<>(List.of(field2_, operator2, value2));

        return CommonV1ListRequest.builder().criteriaRequests(List.of(criteria1, "and", criteria2)).build();
    }

    private CommonV1ListRequest createCriteriaForThreeFields(String field1, String operator1, Object value1,
                                                             String field2, String operator2, Object value2,
                                                             String field3, String operator3, Object value3) {
        List<Object> field1_ = new ArrayList<>(List.of(field1));
        List<Object> criteria1 = new ArrayList<>(List.of(field1_, operator1, value1));

        List<Object> field2_ = new ArrayList<>(List.of(field2));
        List<Object> criteria2 = new ArrayList<>(List.of(field2_, operator2, value2));

        List<Object> field3_ = new ArrayList<>(List.of(field3));
        List<Object> criteria3 = new ArrayList<>(List.of(field3_, operator3, value3));

        return CommonV1ListRequest.builder().criteriaRequests(List.of(List.of(criteria1, "and", criteria2), "and", criteria3)).build();
    }

    private void sendFailureAlerts(String request, String response, String bookingNumber, String shipmentId) {
        try {
            var body = CustomerBookingConstants.PLATFORM_FAILURE_EMAIL_BODY;
            body = body.replace(CustomerBookingConstants.BOOKING_NUMBER, bookingNumber);
            body = body.replace(CustomerBookingConstants.SHIPMENT_ID, Objects.isNull(shipmentId) ? CustomerBookingConstants.SHIPMENT_NOT_CREATED : shipmentId);
            body = body.replace(CustomerBookingConstants.RESPONSE, response);
            body = body.replace(CustomerBookingConstants.REQUEST, request);
            if (Boolean.TRUE.equals(isFailureNotificationEnabled))
                emailServiceUtility.sendEmail(body, String.format(CustomerBookingConstants.PLATFORM_FAILURE_EMAIL_SUBJECT, bookingNumber), failureNotificationEmailTo, failureNotificationEmailCC, null, null);
        } catch (Exception e) {
            log.error(e.getMessage());
        }
    }

    private String getBusinessCode(String cargoType)
    {
        return switch (cargoType) {
            case "FCL" -> fclBusinessCode;
            case "LCL" -> lclBusinessCode;
            case "LSE" -> lseBusinessCode;
            case "BBK" -> lclBusinessCode;
            case "ROR" -> lclBusinessCode;
            default -> null;
        };
    }

    /**
     * This method will be used to send shipment document to platform
     * @param payload
     */
    @Transactional
    public void documentUploadEvent(DocumentDto payload) {

        Document payloadData = payload.getData();
        String payloadAction = payload.getAction();
        var shipments = shipmentDao.findShipmentsByGuids(Set.of(UUID.fromString(payload.getData().getEntityId())));
        var shipment = shipments.stream().findFirst().orElse(new ShipmentDetails());
        if (Constants.KAFKA_EVENT_CREATE.equalsIgnoreCase(payloadAction)
                && Objects.equals(payloadData.getEntityType(), Constants.SHIPMENTS_CAPS)
                && Boolean.TRUE.equals(payloadData.getCustomerPortalVisibility())) {

            // Sending document to Logistics Platform in case of online booking
            if (Objects.equals(shipment.getBookingType(), CustomerBookingConstants.ONLINE) && !Objects.isNull(shipment.getBookingReference())) {
                this.sendDocumentsToPlatform(shipment, payload);
            }
        }

        handleEventCreation(payloadAction, payloadData);

    }

    /**
     * Processes the creation or updating of events related to shipment entities based on the provided payload action and data.
     * <p>
     * This method identifies the intended action (either creation or update) from the provided payload action.
     * If the event corresponding to the shipment already exists in the database, it updates the event details.
     * Otherwise, if no event exists and the payload contains specific event codes, it initiates the auto-generation
     * of a new event for the shipment entity.
     * </p>
     *
     * @param payloadAction The action to be performed, specifying either "create" or "update" for the event.
     * @param payloadData   The data payload containing information about the document and associated entity,
     *                      including attributes such as the entity type, event code, and ID.
     */
    private void handleEventCreation(String payloadAction, Document payloadData) {
        log.info("Starting event handling process for action: {} and entity ID: {}", payloadAction, payloadData.getEntityId());

        try {
            // Check if the action and entity type match the criteria for event handling
            if ((Constants.KAFKA_EVENT_CREATE.equalsIgnoreCase(payloadAction) || Constants.KAFKA_EVENT_UPDATE.equalsIgnoreCase(payloadAction))
                    && Objects.equals(payloadData.getEntityType(), Constants.SHIPMENTS_CAPS)) {

                log.debug("Valid action and entity type. Fetching shipment details for entity ID: {}", payloadData.getEntityId());

                validateEntityId(payloadData);

                var shipmentDetails = shipmentDao.findShipmentsByGuids(Set.of(UUID.fromString(payloadData.getEntityId())))
                        .stream().findFirst().orElse(new ShipmentDetails());

                List<Events> eventListFromDb = shipmentDetails.getEventsList();
                RequestAuthContext.setAuthToken("Bearer " + StringUtils.defaultString(v1Service.generateToken()));
                TenantContext.setCurrentTenant(shipmentDetails.getTenantId());
                UserContext.setUser(UsersDto.builder().TenantId(shipmentDetails.getTenantId()).Permissions(new HashMap<>()).build());
                commonUtils.impersonateUser(shipmentDetails.getTenantId());
                boolean updatedExistingEvent = false;

                // If existing events are found, iterate through them for potential updates
                updatedExistingEvent = getAndUpdateExistingEvent(payloadData, eventListFromDb, shipmentDetails, updatedExistingEvent);
                if (!updatedExistingEvent && (EventConstants.DNMU.equals(payloadData.getEventCode()) || EventConstants.FNMU.equals(payloadData.getEventCode())
                    || (EventConstants.BOCO.equals(payloadData.getEventCode()) && Objects.equals(shipmentDetails.getDirection(), Constants.DIRECTION_EXP)))) {
                    log.debug("No existing events found for shipment with entity ID: {}. Checking conditions for auto-generation.", payloadData.getEntityId());

                    if (shipmentDetails.getId() == null) {
                        throw new IllegalStateException("Shipment ID is null for the provided entity ID.");
                    }

                    EventsRequest eventsRequest = new EventsRequest();
                    eventsRequest.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    eventsRequest.setEntityId(shipmentDetails.getId());
                    eventsRequest.setEntityType(Constants.SHIPMENT);
                    eventsRequest.setEventCode(payloadData.getEventCode());
                    eventsRequest.setSource(Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER);
                    eventsRequest.setUserName(payloadData.getUserDisplayName());
                    eventsRequest.setUserEmail(payloadData.getUserEmail());
                    eventsRequest.setBranchName(payloadData.getBranchDisplayName());
                    eventsRequest.setBranch(payloadData.getBranchCode());
                    if (EventConstants.FNMU.equals(payloadData.getEventCode())) {
                        eventsRequest.setContainerNumber(shipmentDetails.getMasterBill());
                    }
                    log.info("Generating event with code: {} for shipment entity ID: {}", payloadData.getEventCode(), shipmentDetails.getId());
                    eventService.saveEvent(eventsRequest);
                    log.info("Event generated successfully for entity ID: {}", shipmentDetails.getId());
                }
            }
        } catch (Exception ex) {
            log.error("Unexpected error in handleEventCreation: {}", ex.getMessage(), ex);
        } finally {
            log.info("Completed event handling process for action: {} and entity ID: {}", payloadAction, payloadData.getEntityId());
            TenantContext.removeTenant();
            UserContext.removeUser();
            RequestAuthContext.removeToken();
        }
    }

    private void validateEntityId(Document payloadData) {
        if (payloadData.getEntityId() == null) {
            throw new IllegalArgumentException("Entity ID in payload data is null.");
        }
    }

    private boolean getAndUpdateExistingEvent(Document payloadData, List<Events> eventListFromDb, ShipmentDetails shipmentDetails, boolean updatedExistingEvent) {
        if (ObjectUtils.isNotEmpty(eventListFromDb)) {
            log.debug("Existing events found for shipment with entity ID: {}", payloadData.getEntityId());

            for (Events event : eventListFromDb) {
                if (Objects.equals(event.getEventCode(), payloadData.getEventCode()) &&
                        Constants.MASTER_DATA_SOURCE_CARGOES_RUNNER.equals(event.getSource())) {

                    log.info("Updating event: {} with new actual time and entity type.", event.getEventCode());
                    event.setActual(commonUtils.getUserZoneTime(LocalDateTime.now()));
                    event.setEntityType(Constants.SHIPMENT);
                    event.setUserName(payloadData.getUserDisplayName());
                    event.setUserEmail(payloadData.getUserEmail());
                    event.setBranchName(payloadData.getBranchDisplayName());
                    event.setBranch(payloadData.getBranchCode());
                    if (EventConstants.FNMU.equals(payloadData.getEventCode())) {
                        event.setContainerNumber(shipmentDetails.getMasterBill());
                    }

                    // Update the event details and save in the database
                    eventDao.updateEventDetails(event);
                    eventDao.save(event);
                    updatedExistingEvent = true;
                    log.info("Event updated successfully for event code: {}", event.getEventCode());
                }
            }
        }
        return updatedExistingEvent;
    }

    private void sendDocumentsToPlatform(ShipmentDetails shipmentDetails, DocumentDto payload) {
        var request = createPlatformDocumentRequest(shipmentDetails.getBookingReference(), payload.getData());
        try {
            platformServiceAdapter.updateAtPlaform(request);
        } catch (Exception ex) {
            log.error("Document Update error from Platform from Shipment for booking number: {} with error message: {}", shipmentDetails.getBookingReference(), ex.getMessage());
            sendFailureAlerts(jsonHelper.convertToJson(request), jsonHelper.convertToJson(ex.getLocalizedMessage()), shipmentDetails.getBookingReference(), shipmentDetails.getShipmentId());
        }
    }

    private CommonRequestModel createPlatformDocumentRequest(String bookingReference, DocumentDto.Document document) {
        PlatformUpdateRequest platformUpdateRequest = PlatformUpdateRequest.builder()
                .booking_reference_code(bookingReference)
                .document_meta(List.of(
                        DocumentMetaDTO.builder()
                                .name(document.getFileName())
                                .document_type(document.getDocType())
                                .document_link(document.getSecureDownloadLink())
                                .uploaded_by_user_id(document.getUploadedBy())
                                .build()))
                .build();

        return CommonRequestModel.buildRequest(platformUpdateRequest);
    }
}
