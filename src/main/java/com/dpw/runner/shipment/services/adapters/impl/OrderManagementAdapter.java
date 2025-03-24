package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.OrderManagementDTO;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.OrderManagementResponse;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.OrderPartiesResponse;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.ReferencesResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.*;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.entity.enums.OrderPartiesPartyType;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.masterdata.request.CommonV1ListRequest;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.validator.enums.Operators;
import com.fasterxml.jackson.core.type.TypeReference;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import org.springframework.web.client.RestTemplate;
import com.dpw.runner.shipment.services.utils.V2AuthHelper;

import java.time.LocalDateTime;
import java.util.*;

@Slf4j
@Service
public class OrderManagementAdapter implements IOrderManagementAdapter {

    @Autowired
    @Qualifier("restTemplateForOrderManagement")
    private RestTemplate restTemplate;

    @Value("${order.management.baseUrl}")
    private String baseUrl;
    @Value("${order.management.getOrder}")
    private String getOrderUrl;
    @Value("${order.management.getOrderbyGuid}")
    private String getOrderbyGuidUrl;

    @Autowired
    private V2AuthHelper v2AuthHelper;

    @Autowired
    private IV1Service v1Service;

    @Autowired
    private JsonHelper jsonHelper;

    public static final String X_SOURCE= "x-source";
    public static final String X_SOURCE_VALUE= "shipment-service";


    @Override
    public ShipmentDetails getOrder(String orderId) throws RunnerException {
        try {
            String url = baseUrl + getOrderUrl + orderId;
            var response = restTemplate.exchange(url, HttpMethod.GET, null, OrderManagementResponse.class);
            var shipment = generateShipmentFromOrder(Objects.requireNonNull(response.getBody()).getOrder());
            return shipment;
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public ShipmentDetails getOrderByGuid(String orderGuid) throws RunnerException {
        try {
            String url = baseUrl + getOrderbyGuidUrl + orderGuid;
            HttpEntity<Object> httpEntity = new HttpEntity<>(v2AuthHelper.getOrderManagementServiceSourceHeader());
            log.info("Request to Order Service: {}", url);
            var response = restTemplate.exchange(url, HttpMethod.GET, httpEntity, OrderManagementResponse.class);
            log.info("Response from Order Service: {}", response.getBody());
            return generateShipmentFromOrder(Objects.requireNonNull(response.getBody()).getOrder());
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RunnerException(e.getMessage());
        }
    }

    @Override
    public CustomerBookingResponse getOrderForBooking(String orderId) throws RunnerException {
        try {
            String url = baseUrl + getOrderUrl + orderId;
            var response = restTemplate.exchange(url, HttpMethod.GET, null, OrderManagementResponse.class);
            var bookingResponse = mapOrderToBooking(Objects.requireNonNull(response.getBody()).getOrder());
            return bookingResponse;
        } catch (Exception e) {
            log.error(e.getMessage());
            throw new RunnerException(e.getMessage());
        }
    }

    private CustomerBookingResponse mapOrderToBooking(OrderManagementDTO order)
    {
        Map<String, OrderPartiesResponse> partyCodeMap = getPartyOrgCodeDataMap(order);
        List<String> partyList = new ArrayList<>(partyCodeMap.keySet());
        var partyMap = getPartyDetails(partyList);
        CustomerBookingResponse customerBookingResponse = CustomerBookingResponse.builder()
                .bookingDate(LocalDateTime.now())
                .bookingStatus(BookingStatus.PENDING_FOR_KYC)
                .carrierDetails(CarrierDetailResponse.builder()
                        .origin(order.getOrigin())
                        .destination(order.getDestination())
                        .originPort(order.getOriginPort())
                        .destinationPort(order.getDestinationPort())
                        .build())
                .orderManagementId(order.getGuid().toString())
                .orderManagementNumber(order.getOrderNumber())
                .transportType(order.getTransportMode())
                .cargoType(order.getContainerMode())
                .serviceMode(order.getServiceMode())
                .incoTerms(order.getIncoTerm())
                .build();
        for (Map.Entry<String, OrderPartiesResponse> entry : partyCodeMap.entrySet()) {
            String partyCode = entry.getKey();
            OrderPartiesResponse partyData = entry.getValue();

            if(partyMap.get(partyCode) == null)
                continue;

            mapPartyDataInBooking(partyData, customerBookingResponse, partyMap, partyCode);
        }

        return customerBookingResponse;
    }

    private void mapPartyDataInBooking(OrderPartiesResponse partyData, CustomerBookingResponse customerBookingResponse, Map<String, Map<String, Object>> partyMap, String partyCode) {
        if (Objects.equals(partyData.getPartyType(), OrderPartiesPartyType.CONSIGNOR.getDescription())) {
            customerBookingResponse.setConsignor(new PartiesResponse());
            customerBookingResponse.getConsignor().setOrgCode(partyData.getPartyCode());
            customerBookingResponse.getConsignor().setOrgData(partyMap.get(partyCode));
            customerBookingResponse.getConsignor().setOrgId(String.valueOf(partyMap.get(partyCode).get("Id")));
            customerBookingResponse.getConsignor().setAddressCode(partyData.getAddressCode());
            if (partyData.getAddress() != null && partyData.getAddress().containsKey("Id"))
                customerBookingResponse.getConsignor().setAddressId(String.valueOf(partyData.getAddress().get("Id")));
            customerBookingResponse.getConsignor().setAddressData(partyData.getAddress());
        }
        if (Objects.equals(partyData.getPartyType(), OrderPartiesPartyType.CONSIGNEE.getDescription())) {
            customerBookingResponse.setConsignee(new PartiesResponse());
            customerBookingResponse.getConsignee().setOrgCode(partyCode);
            customerBookingResponse.getConsignee().setOrgData(partyMap.get(partyCode));
            customerBookingResponse.getConsignee().setOrgId(String.valueOf(partyMap.get(partyCode).get("Id")));
            customerBookingResponse.getConsignee().setAddressCode(partyData.getAddressCode());
            if (partyData.getAddress() != null && partyData.getAddress().containsKey("Id"))
                customerBookingResponse.getConsignee().setAddressId(String.valueOf(partyData.getAddress().get("Id")));
            customerBookingResponse.getConsignee().setAddressData(partyData.getAddress());
        }

        if (Objects.equals(partyData.getPartyType(), OrderPartiesPartyType.NOTIFY_PARTY.getDescription())) {
            customerBookingResponse.setNotifyParty(new PartiesResponse());
            customerBookingResponse.getNotifyParty().setOrgCode(partyCode);
            customerBookingResponse.getNotifyParty().setOrgData(partyMap.get(partyCode));
            customerBookingResponse.getNotifyParty().setOrgId(String.valueOf(partyMap.get(partyCode).get("Id")));
            customerBookingResponse.getNotifyParty().setAddressCode(partyData.getAddressCode());
            if (partyData.getAddress() != null && partyData.getAddress().containsKey("Id"))
                customerBookingResponse.getNotifyParty().setAddressId(String.valueOf(partyData.getAddress().get("Id")));
            customerBookingResponse.getNotifyParty().setAddressData(partyData.getAddress());
        }
    }

    private ShipmentDetails generateShipmentFromOrder(OrderManagementDTO order) {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setAdditionalDetails(new AdditionalDetails());
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        Map<String, OrderPartiesResponse> partyCodeMap = getPartyOrgCodeDataMap(order);
        List<String> partyList = new ArrayList<>(partyCodeMap.keySet());

        var partyMap = getPartyDetails(partyList);

        shipmentDetails.setTransportMode(order.getTransportMode());
        shipmentDetails.setIncoterms(order.getIncoTerm());

        // LATER : to revert this change once order team gets back on this
        // Skipping this fields for now as integration for LocationReferenceGuid is pending from order team
        shipmentDetails.getCarrierDetails().setOrigin(order.getOrigin());
        shipmentDetails.getCarrierDetails().setOriginPort(order.getOriginPort());
        shipmentDetails.getCarrierDetails().setDestination(order.getDestination());
        shipmentDetails.getCarrierDetails().setDestinationPort(order.getDestinationPort());



        for (Map.Entry<String, OrderPartiesResponse> entry : partyCodeMap.entrySet()) {
            String partyCode = entry.getKey();
            OrderPartiesResponse partyData = entry.getValue();

            if(partyMap.get(partyCode) == null)
                continue;

            getShipmentPartyDataFromOrder(partyData, shipmentDetails, partyMap, partyCode);
        }

        shipmentDetails.setShipmentType(order.getContainerMode());
        if(order.getPacksAmount() != null){
            shipmentDetails.setNoOfPacks(order.getPacksAmount().getAmount() != null ? order.getPacksAmount().getAmount().intValue() : null);
            shipmentDetails.setPacksUnit(order.getPacksAmount().getUnit());
        }
        if(order.getWeightAmount() != null){
            shipmentDetails.setWeight(order.getWeightAmount().getAmount());
            shipmentDetails.setWeightUnit(order.getWeightAmount().getUnit());
        }
        if(order.getVolumeAmount() != null){
            shipmentDetails.setVolume(order.getVolumeAmount().getAmount());
            shipmentDetails.setVolumeUnit(order.getVolumeAmount().getUnit());
        }
        if (order.getGoodsDescription() != null) {
            shipmentDetails.setGoodsDescription(order.getGoodsDescription());
        }

        if (order.getReferences() != null) {
            var referenceNumbersList = getReferenceNumbersList(order.getReferences());
            shipmentDetails.setReferenceNumbersList(referenceNumbersList);
        }

        shipmentDetails.setOrderManagementId(order.getGuid().toString());
        shipmentDetails.setOrderManagementNumber(order.getOrderNumber());
        shipmentDetails.setShipmentOrders(Collections.singletonList(ShipmentOrder.builder().orderGuid(order.getGuid()).orderNumber(order.getOrderNumber()).build()));

        shipmentDetails.setServiceType(order.getServiceMode());

        shipmentDetails.setStatus(ShipmentStatus.Created.getValue());
        shipmentDetails.setSource(Constants.SYSTEM);
        shipmentDetails.setCreatedBy(UserContext.getUser().getUsername());
        shipmentDetails.setShipmentCreatedOn(LocalDateTime.now());
        shipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));

        return shipmentDetails;
    }

    private void getShipmentPartyDataFromOrder(OrderPartiesResponse partyData, ShipmentDetails shipmentDetails, Map<String, Map<String, Object>> partyMap, String partyCode) {
        if (Objects.equals(partyData.getPartyType(), OrderPartiesPartyType.CONSIGNOR.getDescription())) {
            shipmentDetails.setConsigner(new Parties());
            shipmentDetails.getConsigner().setOrgCode(partyData.getPartyCode());
            shipmentDetails.getConsigner().setOrgData(partyMap.get(partyCode));
            shipmentDetails.getConsigner().setOrgId(String.valueOf(partyMap.get(partyCode).get("Id")));
            shipmentDetails.getConsigner().setAddressCode(partyData.getAddressCode());
            if (partyData.getAddress() != null && partyData.getAddress().containsKey("Id"))
                shipmentDetails.getConsigner().setAddressId(String.valueOf(partyData.getAddress().get("Id")));
            shipmentDetails.getConsigner().setAddressData(partyData.getAddress());
        }
        if (Objects.equals(partyData.getPartyType(), OrderPartiesPartyType.CONSIGNEE.getDescription())) {
            shipmentDetails.setConsignee(new Parties());
            shipmentDetails.getConsignee().setOrgCode(partyCode);
            shipmentDetails.getConsignee().setOrgData(partyMap.get(partyCode));
            shipmentDetails.getConsignee().setOrgId(String.valueOf(partyMap.get(partyCode).get("Id")));
            shipmentDetails.getConsignee().setAddressCode(partyData.getAddressCode());
            if (partyData.getAddress() != null && partyData.getAddress().containsKey("Id"))
                shipmentDetails.getConsignee().setAddressId(String.valueOf(partyData.getAddress().get("Id")));
            shipmentDetails.getConsignee().setAddressData(partyData.getAddress());
        }

        if (Objects.equals(partyData.getPartyType(), OrderPartiesPartyType.NOTIFY_PARTY.getDescription())) {
            shipmentDetails.getAdditionalDetails().setNotifyParty(new Parties());
            shipmentDetails.getAdditionalDetails().getNotifyParty().setOrgCode(partyCode);
            shipmentDetails.getAdditionalDetails().getNotifyParty().setOrgData(partyMap.get(partyCode));
            shipmentDetails.getAdditionalDetails().getNotifyParty().setOrgId(String.valueOf(partyMap.get(partyCode).get("Id")));
            shipmentDetails.getAdditionalDetails().getNotifyParty().setAddressCode(partyData.getAddressCode());
            if (partyData.getAddress() != null && partyData.getAddress().containsKey("Id"))
                shipmentDetails.getAdditionalDetails().getNotifyParty().setAddressId(String.valueOf(partyData.getAddress().get("Id")));
            shipmentDetails.getAdditionalDetails().getNotifyParty().setAddressData(partyData.getAddress());
        }
    }

    private Map<String, OrderPartiesResponse> getPartyOrgCodeDataMap(OrderManagementDTO order) {

        Map<String, OrderPartiesResponse> partyCodeMap = new HashMap<>();
        List<String> partyLists = List.of(OrderPartiesPartyType.CONSIGNOR.getDescription(),
                OrderPartiesPartyType.CONSIGNEE.getDescription(),
                OrderPartiesPartyType.NOTIFY_PARTY.getDescription());
        if (!ObjectUtils.isEmpty(order.getParties())) {
            for (var party : order.getParties()) {
                if (partyLists.contains(party.getPartyType())) {
                    partyCodeMap.put(party.getPartyCode(), party);
                }
            }
        }
        return partyCodeMap;
    }

    private Map<String, Map<String, Object>> getPartyDetails (List<String> orgCodes) {
        CommonV1ListRequest orgRequest = new CommonV1ListRequest();
        List<Object> orgField = new ArrayList<>(List.of("OrganizationCode"));
        String operator = Operators.IN.getValue();
        List<Object> orgCriteria = new ArrayList<>(List.of(orgField, operator, List.of(orgCodes)));
        orgRequest.setCriteriaRequests(orgCriteria);
        V1DataResponse orgResponse = v1Service.fetchOrganization(orgRequest);
        List<Map<String, Object>> responseMap = jsonHelper.convertValue(orgResponse.entities, new TypeReference<List<Map<String, Object>>>() {});
        Map<String, Map<String, Object>> res = new HashMap<>();
        if(responseMap != null) {
            for (Map<String, Object> i : responseMap) {
                res.putIfAbsent((String) i.get("OrganizationCode"), i);
            }
        }
        return res;
    }

    private List<ReferenceNumbers> getReferenceNumbersList(List<ReferencesResponse> references) {

        List<ReferenceNumbers> newReferenceNumbersList = new ArrayList<>();
        for (ReferencesResponse refMap : references) {
            ReferenceNumbers referenceNumberObj = new ReferenceNumbers();

            referenceNumberObj.setCountryOfIssue(refMap.getCountryOfIssue());
            referenceNumberObj.setType(refMap.getType());
            referenceNumberObj.setReferenceNumber(refMap.getReference());

            newReferenceNumbersList.add(referenceNumberObj);
        }
        return newReferenceNumbersList;


    }

}
