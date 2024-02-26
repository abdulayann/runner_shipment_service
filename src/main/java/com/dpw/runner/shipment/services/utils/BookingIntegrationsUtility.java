package com.dpw.runner.shipment.services.utils;


import com.dpw.runner.shipment.services.adapters.interfaces.IPlatformServiceAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.CustomerBookingConstants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.constants.ShipmentConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.commons.responses.DependentServiceResponse;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IIntegrationResponseDao;
import com.dpw.runner.shipment.services.dto.request.CustomerBookingRequest;
import com.dpw.runner.shipment.services.dto.request.PartiesRequest;
import com.dpw.runner.shipment.services.dto.request.platform.*;
import com.dpw.runner.shipment.services.dto.response.CheckCreditLimitResponse;
import com.dpw.runner.shipment.services.dto.response.ShipmentDetailsResponse;
import com.dpw.runner.shipment.services.dto.response.ListContractResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferChargeType;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.helpers.LoggerHelper;
import com.dpw.runner.shipment.services.helpers.ResponseHelper;
import com.dpw.runner.shipment.services.masterdata.enums.MasterDataType;
import com.dpw.runner.shipment.services.masterdata.factory.MasterDataFactory;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.interfaces.IShipmentService;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.support.RetryTemplate;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;


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

    @Async
    public void createBookingInPlatform(CustomerBooking customerBooking) {
        try {
            platformServiceAdapter.createAtPlatform(createPlatformCreateRequest(customerBooking));
            customerBooking.setIsPlatformBookingCreated(true);
            customerBookingDao.save(customerBooking);
            this.saveErrorResponse(customerBooking.getId(), Constants.BOOKING, IntegrationType.PLATFORM_CREATE_BOOKING, Status.SUCCESS, "SAVED SUCESSFULLY");
        } catch (Exception ex) {
            this.saveErrorResponse(customerBooking.getId(), Constants.BOOKING, IntegrationType.PLATFORM_CREATE_BOOKING, Status.FAILED, ex.getLocalizedMessage());
            log.error("Booking Creation error from Platform for booking number: {} with error message: {}", customerBooking.getBookingNumber(), ex.getMessage());
        }
        throw new RuntimeException();
    }

    public void updateBookingInPlatform(CustomerBooking customerBooking) {
        try {
            platformServiceAdapter.updateAtPlaform(createPlatformUpdateRequest(customerBooking));
        } catch (Exception e) {
            this.saveErrorResponse(customerBooking.getId(), Constants.BOOKING, IntegrationType.PLATFORM_UPDATE_BOOKING, Status.FAILED, e.getLocalizedMessage());
            log.error("Booking Update error from Platform for booking number: {} with error message: {}", customerBooking.getBookingNumber(), e.getMessage());
        }
    }

    @Async
    public void updateBookingInPlatform(ShipmentDetails shipmentDetails) {
        try {
            if (Objects.equals(shipmentDetails.getBookingType(), CustomerBookingConstants.ONLINE))
                platformServiceAdapter.updateAtPlaform(createPlatformUpdateRequestFromShipment(shipmentDetails));
        } catch (Exception e) {
            this.saveErrorResponse(shipmentDetails.getId(), Constants.SHIPMENT, IntegrationType.PLATFORM_UPDATE_BOOKING, Status.FAILED, e.getLocalizedMessage());
            log.error("Booking Update error from Platform for booking number: {} with error message: {}", shipmentDetails.getBookingReference(), e.getMessage());
        }
    }

    public ResponseEntity<?> createShipmentInV1(CustomerBooking customerBooking, boolean isShipmentEnabled, boolean isBillingEnabled, UUID shipmentGuid, HttpHeaders headers) {
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
                    TimeUnit.SECONDS.sleep(2);
                } catch (Exception ex) {
                    log.error("Wait failed due to {}", ex.getMessage());
                }
                this.createShipmentInV1(customerBooking, false, true, shipmentResponse.getGuid(), headers);
            } catch (Exception e) {
                log.error("Event: {} Bill creation  for shipment with booking reference {} failed due to following error: {}", IntegrationType.V1_SHIPMENT_CREATION, shipmentResponse.getBookingReference(), e.getMessage());
                throw e;
            }
            return ResponseHelper.buildSuccessResponse();
        });
    }

    public ResponseEntity<?> updateOrgCreditLimitFromBooking(CheckCreditLimitResponse request) {
        try {
            var response = v1Service.updateOrgCreditLimitFromBooking(request);
            return response;
        } catch (Exception ex) {
            log.error("Error updating Credit Limit for Org with CustomerIdentifier: {} with error message: {}", request.getAccountNumber(), ex.getMessage());
            throw ex;
        }
    }

    public ResponseEntity<?> createShipmentInV2(CustomerBookingRequest customerBookingRequest) throws Exception {
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
        var carrierDetails = Optional.ofNullable(customerBooking.getCarrierDetails());
        PlatformCreateRequest platformCreateRequest = PlatformCreateRequest.builder()
                .booking_ref_code(customerBooking.getBookingNumber())
                .origin_code(carrierDetails.map(c -> c.getOrigin()).orElse(null))
                .destination_code(carrierDetails.map(c -> c.getDestination()).orElse(null))
                .pol(carrierDetails.map(c -> c.getOriginPort()).orElse(null))
                .pod(carrierDetails.map(c -> c.getDestinationPort()).orElse(null))
                .contract_id(customerBooking.getContractId())
                .created_at(customerBooking.getCreatedAt())
                .customer_org_id(customerBooking.getCustomer().getOrgCode())
                .customer_email(customerBooking.getCustomerEmail())
                .load(createLoad(customerBooking))
                .route(createRoute(customerBooking))
                .charges(createCharges(customerBooking))
                .business_code(customerBooking.getBusinessCode())
                .bill_to_party(Arrays.asList(createOrgRequest(customerBooking.getCustomer())))
                .parent_contract_id(customerBooking.getParentContractId())
//                .branch_info(ListContractResponse.BranchInfo.builder().
//                        id(customerBooking.getSalesBranch()).
//                        opportunity_owner_email_ids(createEmailIds(customerBooking.getPrimarySalesAgentEmail(), customerBooking.getSecondarySalesAgentEmail())).
//                        build())
                .build();
        return CommonRequestModel.builder().data(platformCreateRequest).build();
    }

    private List<String> createEmailIds(String primaryEmail, String secondaryEmail)
    {
        if(primaryEmail == null)
            return null;
        if(secondaryEmail == null)
            return Arrays.asList(primaryEmail);
        return Arrays.asList(primaryEmail, secondaryEmail);
    }

    private List<LoadRequest> createLoad(final CustomerBooking customerBooking) {
        List<LoadRequest> loadRequests = new ArrayList<>();
        //Container -> FCL
        if (customerBooking.getCargoType() != null && customerBooking.getCargoType().equals("FCL")) {
            List<Containers> containers = customerBooking.getContainersList();
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
            List<Containers> containers = shipmentDetails.getContainersList();
            containers.forEach(container -> {
                loadRequests.add(LoadRequest.builder()
                        .load_uuid(container.getGuid())
                        .load_type(shipmentDetails.getShipmentType())
                        .container_type_code(container.getContainerCode())
                        .pkg_type(container.getPacksType())
                        .is_package(StringUtility.isNotEmpty(container.getPacksType()))
                        .product_category_code(container.getCommodityGroup())
                        .product_name(getItemDescription(MasterDataType.COMMODITY_GROUP, container.getCommodityGroup()))
                        .is_reefer(container.getIsReefer())
                        .reefer_info(ReeferInfoRequest.builder().temperature(!Objects.isNull(container.getMinTemp()) ? container.getMinTemp().intValue() : null).build())
                        .is_hazardous(container.getHazardous() != null && container.getHazardous())
                        .hazardous_info(HazardousInfoRequest.builder().product_un_id(container.getHazardousUn()).product_class(container.getDgClass()).build()) // TODO
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
                    .transport_mode(routingsList.get(counter).getMode())
                    .build());
        }

        return RouteRequest.builder()
                .legs(legRequestList)
                .build();
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
        List<String> chargeTypes = bookingCharges.stream().map(BookingCharges::getChargeType).collect(Collectors.toList());
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
        var carrierDetails = Optional.ofNullable(customerBooking.getCarrierDetails());
        PlatformUpdateRequest platformUpdateRequest = PlatformUpdateRequest.builder()
                .booking_reference_code(customerBooking.getBookingNumber())
                .origin_code(carrierDetails.map(c -> c.getOrigin()).orElse(null))
                .destination_code(carrierDetails.map(c -> c.getDestination()).orElse(null))
                .load(createLoad(customerBooking))
                .charges(createCharges(customerBooking))
                .customer_email(customerBooking.getCustomerEmail())
                .pol(carrierDetails.map(c -> c.getOriginPort()).orElse(null))
                .pod(carrierDetails.map(c -> c.getDestinationPort()).orElse(null))
                .carrier_code(carrierDetails.map(c -> c.getJourneyNumber()).orElse(null))
                .air_carrier_details(null)
                .status(platformStatusMap.get(customerBooking.getBookingStatus()))
                .pickup_date(null)
                .eta(carrierDetails.map(c -> c.getEta()).orElse(null))
                .ets(carrierDetails.map(c -> c.getEtd()).orElse(null))
                .build();
        return CommonRequestModel.builder().data(platformUpdateRequest).build();
    }

    private CommonRequestModel createPlatformUpdateRequestFromShipment(@NonNull final ShipmentDetails shipmentDetails) {
        var carrierDetails = shipmentDetails.getCarrierDetails();
        PlatformUpdateRequest platformUpdateRequest = PlatformUpdateRequest.builder()
                .booking_reference_code(StringUtility.getNullIfEmpty(shipmentDetails.getBookingReference()))
                .origin_code(StringUtility.getNullIfEmpty(carrierDetails.getOrigin()))
                .destination_code(StringUtility.getNullIfEmpty(carrierDetails.getDestination()))
                .load(createLoad(shipmentDetails))
                .pol(StringUtility.getNullIfEmpty(carrierDetails.getOriginPort()))
                .pod(StringUtility.getNullIfEmpty(carrierDetails.getDestinationPort()))
                .carrier_code(StringUtility.getNullIfEmpty(carrierDetails.getShippingLine()))
                .carrier_display_name(masterDataUtils.getCarrierName(carrierDetails.getShippingLine()))
                .vessel_name(masterDataUtils.getVesselName(carrierDetails.getVessel()))
                .air_carrier_details(null)
                .status(mapBookingStatus(ShipmentStatus.fromValue(shipmentDetails.getStatus())))
                .pickup_date(null)
                .eta(carrierDetails.getEta())
                .ets(carrierDetails.getEtd())
                .voyage(StringUtility.getNullIfEmpty(carrierDetails.getVoyage()))
                .build();
        return CommonRequestModel.builder().data(platformUpdateRequest).build();
    }

    private String mapBookingStatus(ShipmentStatus status) {
        if (status == ShipmentStatus.Created)
            return ShipmentConstants.PENDING;
        else if (status == ShipmentStatus.Booked)
            return ShipmentConstants.BOOKED;
        else if (status == ShipmentStatus.Cancelled)
            return ShipmentConstants.CANCELLED;
        else
            return ShipmentConstants.CONFIRMED;
    }
    private OrgRequest createOrgRequest(Parties parties) {
        return OrgRequest.builder()
                .org_id(parties.getOrgCode())
                .office_id(parties.getAddressCode())
                .org_name(String.valueOf(parties.getOrgData().get(PartiesConstants.FULLNAME)))
                .build();
    }

    public void transformOrgAndAddressPayload(PartiesRequest request, String addressCode, String orgCode) {
        try {
            CommonV1ListRequest orgRequest = createCriteriaForTwoFields("OrganizationCode", orgCode, "ActiveClient", Boolean.TRUE);
            DependentServiceResponse v1OrgResponse = masterDataFactory.getMasterDataService().fetchOrganizationData(orgRequest);
            if (v1OrgResponse.getData() == null || ((ArrayList)v1OrgResponse.getData()).isEmpty()) {
                log.error("Request: {} || No organization exist in Runner V1 with OrgCode: {}", LoggerHelper.getRequestIdFromMDC() ,orgCode);
                throw new DataRetrievalFailureException("No organization exist in Runner V1 with OrgCode: " + orgCode);
            }
            Map organizationRow = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(((ArrayList)v1OrgResponse.getData()).get(0)));
            request.setOrgData(organizationRow);

            CommonV1ListRequest addressRequest = createCriteriaForThreeFields("OrgId", organizationRow.get("Id"), "AddressShortCode", addressCode, "Active", Boolean.TRUE);
            DependentServiceResponse v1AddressResponse = masterDataFactory.getMasterDataService().addressList(addressRequest);
            if (v1OrgResponse.getData() == null || ((ArrayList)v1AddressResponse.getData()).isEmpty()) {
                log.error("Request: {} || No Address exist in Runner V1 with OrgCode: {} with AddressCode: {}", LoggerHelper.getRequestIdFromMDC(), orgCode , addressCode);
                throw new DataRetrievalFailureException("No Address exist in Runner V1 with OrgCode: " + orgCode + " with AddressCode: " + addressCode);
            }
            Map addressRow = jsonHelper.convertJsonToMap(jsonHelper.convertToJson(((ArrayList)v1AddressResponse.getData()).get(0)));
            request.setAddressData(addressRow);
        }
        catch (Exception ex) {
            log.error("Error occurred during Organization fetch from V1 with exception: {}", ex.getLocalizedMessage());
            throw new DataRetrievalFailureException(ex.getMessage());
        }

    }

    private CommonV1ListRequest createCriteriaForTwoFields(String field1, Object value1, String field2, Object value2) {
        List<Object> criteria1 = new ArrayList<>();
        List<Object> field1_ = new ArrayList<>(List.of(field1));
        criteria1.addAll(List.of(field1_, "=", value1));

        List<Object> criteria2 = new ArrayList<>();
        List<Object> field2_ = new ArrayList<>(List.of(field2));
        criteria2.addAll(List.of(field2_, "=", value2));

        return CommonV1ListRequest.builder().criteriaRequests(List.of(criteria1, "and", criteria2)).build();
    }

    private CommonV1ListRequest createCriteriaForThreeFields(String field1, Object value1, String field2, Object value2, String field3, Object value3) {
        List<Object> criteria1 = new ArrayList<>();
        List<Object> field1_ = new ArrayList<>(List.of(field1));
        criteria1.addAll(List.of(field1_, "=", value1));

        List<Object> criteria2 = new ArrayList<>();
        List<Object> field2_ = new ArrayList<>(List.of(field2));
        criteria2.addAll(List.of(field2_, "=", value2));

        List<Object> criteria3 = new ArrayList<>();
        List<Object> field3_ = new ArrayList<>(List.of(field3));
        criteria3.addAll(List.of(field3_, "=", value3));

        return CommonV1ListRequest.builder().criteriaRequests(List.of(List.of(criteria1, "and", criteria2), "and", criteria3)).build();
    }


}
