package com.dpw.runner.shipment.services.adapters.impl;

import com.dpw.runner.shipment.services.adapters.interfaces.IOrderManagementAdapter;
import com.dpw.runner.shipment.services.dto.response.CarrierDetailResponse;
import com.dpw.runner.shipment.services.dto.response.CustomerBookingResponse;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.OrderManagementDTO;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.OrderManagementResponse;
import com.dpw.runner.shipment.services.dto.response.PartiesResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.entity.enums.BookingStatus;
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
import org.springframework.http.HttpMethod;
import org.springframework.stereotype.Service;
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
        return CustomerBookingResponse.builder()
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

}
