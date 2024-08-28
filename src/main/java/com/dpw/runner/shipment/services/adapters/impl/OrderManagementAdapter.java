package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.OrderManagementDTO;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.OrderManagementResponse;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.ReferencesResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.AdditionalDetails;
import com.dpw.runner.shipment.services.entity.CarrierDetails;
import com.dpw.runner.shipment.services.entity.Parties;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
import com.dpw.runner.shipment.services.entity.enums.ShipmentStatus;
import com.dpw.runner.shipment.services.entity.ReferenceNumbers;
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
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

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
            HttpHeaders headers = new HttpHeaders();
            headers.add(X_SOURCE, X_SOURCE_VALUE);
            HttpEntity<Object> httpEntity = new HttpEntity<>(headers);
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
        var partyMap = getPartyDetails(List.of(order.getSupplierCode(), order.getBuyerCode(), order.getNotifyPartyCode()));
        CustomerBookingResponse customerBookingResponse = CustomerBookingResponse.builder()
                .bookingDate(LocalDateTime.now())
                .bookingStatus(BookingStatus.PENDING_FOR_KYC)
                .carrierDetails(CarrierDetailResponse.builder()
                        .origin(order.getOrigin())
                        .destination(order.getDestination())
                        .originPort(order.getOriginPort())
                        .destinationPort(order.getDestinationPort())
                        .build())
                .consignor(partyMap.get(order.getSupplierCode()) != null ? PartiesResponse.builder()
                        .orgCode(order.getSupplierCode())
                        .orgData(partyMap.get(order.getSupplierCode()))
                        .addressCode(order.getSupplierAddressCode())
                        .addressData(order.getSupplierAddress())
                        .build() : null)
                .consignee(partyMap.get(order.getBuyerCode()) != null ? PartiesResponse.builder()
                        .orgCode(order.getBuyerCode())
                        .orgData(partyMap.get(order.getBuyerCode()))
                        .addressCode(order.getBuyerAddressCode())
                        .addressData(order.getBuyerAddress())
                        .build() : null)
                .notifyParty(partyMap.get(order.getNotifyPartyCode()) != null ? PartiesResponse.builder()
                        .orgCode(order.getNotifyPartyCode())
                        .orgData(partyMap.get(order.getNotifyPartyCode()))
                        .addressCode(order.getNotifyPartyAddressCode())
                        .addressData(order.getNotifyPartyAddress())
                        .build() : null)
                .orderManagementId(order.getGuid().toString())
                .orderManagementNumber(order.getOrderNumber())
                .transportType(order.getTransportMode())
                .cargoType(order.getContainerMode())
                .serviceMode(order.getServiceMode())
                .incoTerms(order.getIncoTerm())
                .build();
        return customerBookingResponse;
    }

    private ShipmentDetails generateShipmentFromOrder(OrderManagementDTO order) {
        ShipmentDetails shipmentDetails = new ShipmentDetails();
        shipmentDetails.setAdditionalDetails(new AdditionalDetails());
        shipmentDetails.setCarrierDetails(new CarrierDetails());
        var partyMap = getPartyDetails(List.of(order.getSupplierCode(), order.getBuyerCode(), order.getNotifyPartyCode(), order.getSendingAgentCode(), order.getReceivingAgentCode()));

        shipmentDetails.setTransportMode(order.getTransportMode());
        shipmentDetails.setIncoterms(order.getIncoTerm());

        // TODO : to revert this change once order team gets back on this
        // Skipping this fields for now as integration for LocationReferenceGuid is pending from order team
        shipmentDetails.getCarrierDetails().setOrigin(order.getOrigin());
        shipmentDetails.getCarrierDetails().setOriginPort(order.getOriginPort());
        shipmentDetails.getCarrierDetails().setDestination(order.getDestination());
        shipmentDetails.getCarrierDetails().setDestinationPort(order.getDestinationPort());



        if(partyMap.get(order.getSupplierCode()) != null) {
            shipmentDetails.setConsigner(new Parties());
            shipmentDetails.getConsigner().setOrgCode(order.getSupplierCode());
            shipmentDetails.getConsigner().setOrgData(partyMap.get(order.getSupplierCode()));
            shipmentDetails.getConsigner().setAddressCode(order.getSupplierAddressCode());
            shipmentDetails.getConsigner().setAddressData(order.getSupplierAddress());
        }
        if(partyMap.get(order.getBuyerCode()) != null) {
            shipmentDetails.setConsignee(new Parties());
            shipmentDetails.getConsignee().setOrgCode(order.getBuyerCode());
            shipmentDetails.getConsignee().setOrgData(partyMap.get(order.getBuyerCode()));
            shipmentDetails.getConsignee().setAddressCode(order.getBuyerAddressCode());
            shipmentDetails.getConsignee().setAddressData(order.getBuyerAddress());
        }
        if(partyMap.get(order.getNotifyPartyCode()) != null) {
            shipmentDetails.getAdditionalDetails().setNotifyParty(new Parties());
            shipmentDetails.getAdditionalDetails().getNotifyParty().setOrgCode(order.getNotifyPartyCode());
            shipmentDetails.getAdditionalDetails().getNotifyParty().setOrgData(partyMap.get(order.getNotifyPartyCode()));
            shipmentDetails.getAdditionalDetails().getNotifyParty().setAddressCode(order.getNotifyPartyAddressCode());
            shipmentDetails.getAdditionalDetails().getNotifyParty().setAddressData(order.getNotifyPartyAddress());
        }
        if(partyMap.get(order.getSendingAgentCode()) != null) {
            shipmentDetails.getAdditionalDetails().setSendingAgent(new Parties());
            shipmentDetails.getAdditionalDetails().getSendingAgent().setOrgCode(order.getSendingAgentCode());
            shipmentDetails.getAdditionalDetails().getSendingAgent().setOrgData(partyMap.get(order.getSendingAgentCode()));
            shipmentDetails.getAdditionalDetails().getSendingAgent().setAddressCode(order.getSendingAgentAddressCode());
            shipmentDetails.getAdditionalDetails().getSendingAgent().setAddressData(order.getSendingAgentAddress());

            shipmentDetails.getAdditionalDetails().setExportBroker(new Parties());
            shipmentDetails.getAdditionalDetails().getExportBroker().setOrgCode(order.getSendingAgentCode());
            shipmentDetails.getAdditionalDetails().getExportBroker().setOrgData(partyMap.get(order.getSendingAgentCode()));
            shipmentDetails.getAdditionalDetails().getExportBroker().setAddressCode(order.getSendingAgentAddressCode());
            shipmentDetails.getAdditionalDetails().getExportBroker().setAddressData(order.getSendingAgentAddress());
        }
        if(partyMap.get(order.getReceivingAgentCode()) != null) {
            shipmentDetails.getAdditionalDetails().setReceivingAgent(new Parties());
            shipmentDetails.getAdditionalDetails().getReceivingAgent().setOrgCode(order.getReceivingAgentCode());
            shipmentDetails.getAdditionalDetails().getReceivingAgent().setOrgData(partyMap.get(order.getReceivingAgentCode()));
            shipmentDetails.getAdditionalDetails().getReceivingAgent().setAddressCode(order.getReceivingAgentAddressCode());
            shipmentDetails.getAdditionalDetails().getReceivingAgent().setAddressData(order.getReceivingAgentAddress());


            shipmentDetails.getAdditionalDetails().setImportBroker(new Parties());
            shipmentDetails.getAdditionalDetails().getImportBroker().setOrgCode(order.getReceivingAgentCode());
            shipmentDetails.getAdditionalDetails().getImportBroker().setOrgData(partyMap.get(order.getReceivingAgentCode()));
            shipmentDetails.getAdditionalDetails().getImportBroker().setAddressCode(order.getReceivingAgentAddressCode());
            shipmentDetails.getAdditionalDetails().getImportBroker().setAddressData(order.getReceivingAgentAddress());


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

        shipmentDetails.setServiceType(order.getServiceMode());

        shipmentDetails.setStatus(ShipmentStatus.Created.getValue());
        shipmentDetails.setSource(Constants.SYSTEM);
        shipmentDetails.setCreatedBy(UserContext.getUser().getUsername());
        shipmentDetails.setShipmentCreatedOn(LocalDateTime.now());
        shipmentDetails.setSourceTenantId(Long.valueOf(UserContext.getUser().TenantId));

        return shipmentDetails;
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
