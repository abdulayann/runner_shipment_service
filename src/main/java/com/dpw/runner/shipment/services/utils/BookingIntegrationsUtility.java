package com.dpw.runner.shipment.services.utils;


import com.dpw.runner.shipment.services.adapters.interfaces.IPlatformServiceAdapter;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.commons.constants.PartiesConstants;
import com.dpw.runner.shipment.services.commons.requests.CommonRequestModel;
import com.dpw.runner.shipment.services.dao.interfaces.ICustomerBookingDao;
import com.dpw.runner.shipment.services.dao.interfaces.IIntegrationResponseDao;
import com.dpw.runner.shipment.services.dto.request.platform.*;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.entity.enums.IntegrationType;
import com.dpw.runner.shipment.services.entity.enums.Status;
import com.dpw.runner.shipment.services.entitytransfer.dto.EntityTransferChargeType;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import lombok.NonNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;

import java.util.*;
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

    @Async
    public void updateBookingInPlatform(CustomerBooking customerBooking) {
        try {
            platformServiceAdapter.updateAtPlaform(createPlatformUpdateRequest(customerBooking));
        } catch (Exception e) {
            this.saveErrorResponse(customerBooking.getId(), Constants.BOOKING, IntegrationType.PLATFORM_UPDATE_BOOKING, Status.FAILED, e.getLocalizedMessage());
            log.error("Booking Update error from Platform for booking number: {} with error message: {}", customerBooking.getBookingNumber(), e.getMessage());
        }
    }


    public ResponseEntity<?> createShipmentInV1(CustomerBooking customerBooking) {
        try {
            var response = v1Service.createBooking(customerBooking);
            this.saveErrorResponse(customerBooking.getId(), Constants.BOOKING, IntegrationType.V1_SHIPMENT_CREATION, Status.SUCCESS, "SAVED SUCESSFULLY");
            return response;
        } catch (Exception ex) {
//            this.saveErrorResponse(customerBooking.getId(), Constants.BOOKING, IntegrationType.V1_SHIPMENT_CREATION, Status.FAILED, ex.getLocalizedMessage());
            log.error("Booking Creation error from Platform for booking number: {} with error message: {}", customerBooking.getBookingNumber(), ex.getMessage());
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
                .load(createLoad(customerBooking))
                .route(createRoute(customerBooking))
                .charges(createCharges(customerBooking))
                .business_code(customerBooking.getBusinessCode())
                .bill_to_party(Arrays.asList(createOrgRequest(customerBooking.getCustomer())))
                .build();
        return CommonRequestModel.builder().data(platformCreateRequest).build();
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
                        .product_category_code(container.getCommodityCode())
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
                        .product_category_code(packing.getCommodity())
                        .pkg_type(packing.getPacksType())
                        .is_package(true)
                        .weight(packing.getWeight())
                        .quantity(Long.valueOf(packing.getPacks()))
                        .weight_uom(packing.getWeightUnit())
                        .quantity_uom("unit")
                        .volume(packing.getVolume())
                        .volume_uom(packing.getVolumeUnit())
                        .dimensions(getDimension(customerBooking, packing))
                        .build());
            });
        }

        return loadRequests;
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

    private DimensionDTO getDimension(CustomerBooking booking, Packing packing) {
        if (booking.getCargoType() != null && (booking.getCargoType().equals("LCL") || booking.getCargoType().equals("LSE")))
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
        List<String> chargeTypes = bookingCharges.stream().map(c -> c.getChargeType()).collect(Collectors.toList());
        Map<String, EntityTransferChargeType> chargeTypeMap = masterDataUtils.getChargeTypes(chargeTypes);

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
                .carrier_code(carrierDetails.map(c -> c.getJourneyNumber()).orElse(null))
                .air_carrier_details(null)
                .status(platformStatusMap.get(customerBooking.getBookingStatus()))
                .pickup_date(null)
                .eta(carrierDetails.map(c -> c.getEta()).orElse(null))
                .ets(carrierDetails.map(c -> c.getEtd()).orElse(null))
                .build();
        return CommonRequestModel.builder().data(platformUpdateRequest).build();
    }

    private OrgRequest createOrgRequest(Parties parties) {
        return OrgRequest.builder()
                .org_id(parties.getOrgCode())
                .office_id(parties.getAddressCode())
                .org_name(String.valueOf(parties.getOrgData().get(PartiesConstants.FULLNAME)))
                .build();
    }


}
