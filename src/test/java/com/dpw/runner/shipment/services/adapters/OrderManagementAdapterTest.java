package com.dpw.runner.shipment.services.adapters;

import com.dpw.runner.shipment.services.adapters.impl.OrderManagementAdapter;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.ShipmentSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.TenantSettingsDetailsContext;
import com.dpw.runner.shipment.services.aspects.MultitenancyAspect.UserContext;
import com.dpw.runner.shipment.services.commons.constants.Constants;
import com.dpw.runner.shipment.services.dto.request.UsersDto;
import com.dpw.runner.shipment.services.dto.request.platform.OrderListResponse;
import com.dpw.runner.shipment.services.dto.request.platform.PurchaseOrdersResponse;
import com.dpw.runner.shipment.services.dto.response.OrderManagement.*;
import com.dpw.runner.shipment.services.dto.v1.response.V1DataResponse;
import com.dpw.runner.shipment.services.dto.v1.response.V1TenantSettingsResponse;
import com.dpw.runner.shipment.services.entity.ShipmentDetails;
import com.dpw.runner.shipment.services.entity.ShipmentSettingsDetails;
import com.dpw.runner.shipment.services.exception.exceptions.RunnerException;
import com.dpw.runner.shipment.services.helper.JsonTestUtility;
import com.dpw.runner.shipment.services.helpers.JsonHelper;
import com.dpw.runner.shipment.services.service.v1.IV1Service;
import com.dpw.runner.shipment.services.utils.V2AuthHelper;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpEntity;
import org.springframework.web.client.RestTemplate;

import java.io.IOException;
import java.math.BigDecimal;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@Execution(ExecutionMode.CONCURRENT)
class OrderManagementAdapterTest {

    @Mock
    private RestTemplate restTemplate;

    @Mock
    private IV1Service v1Service;

    @Mock
    private V2AuthHelper v2AuthHelper;

    @Mock
    private JsonHelper jsonHelper;

    @InjectMocks
    private OrderManagementAdapter orderManagementAdapter;

    private static JsonTestUtility jsonTestUtility;

    private static ObjectMapper objectMapperTest;

    @BeforeAll
    static void init(){
        try {
            jsonTestUtility = new JsonTestUtility();
            objectMapperTest = JsonTestUtility.getMapper();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @BeforeEach
    void setUp() {
        TenantSettingsDetailsContext.setCurrentTenantSettings(
                V1TenantSettingsResponse.builder().P100Branch(false).build());
        UsersDto mockUser = new UsersDto();
        mockUser.setTenantId(1);
        mockUser.setUsername("user");
        UserContext.setUser(mockUser);
        ShipmentSettingsDetailsContext.setCurrentTenantSettings(ShipmentSettingsDetails.builder().multipleShipmentEnabled(true).mergeContainers(false).volumeChargeableUnit("M3").weightChargeableUnit("KG").build());
        MockitoAnnotations.initMocks(this);
    }

    @Test
    void getOrder() throws Exception {
        OrderManagementResponse response = new OrderManagementResponse();
        QuantityPair quantityPair = new QuantityPair();
        quantityPair.setAmount(new BigDecimal(23));
        quantityPair.setUnit(Constants.WEIGHT_UNIT_KG);
        UUID guid = UUID.randomUUID();

        Map<String, Object> partiesAddress = new HashMap<>();
        partiesAddress.put("Id", 36118);
        partiesAddress.put("OrgId", 24008);
        OrderPartiesResponse partyConsignor = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("supCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyConsignee = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("buyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyNotifyParty = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("notifyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderManagementDTO orderManagementDTO = OrderManagementDTO.builder()
                .parties(List.of(partyConsignor, partyConsignee,partyNotifyParty ))
                .packsAmount(quantityPair)
                .weightAmount(quantityPair)
                .volumeAmount(quantityPair)
                .guid(guid)
                .build();
        response.setOrder(orderManagementDTO);
        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull123", HttpMethod.GET, null, OrderManagementResponse.class);
        when(v1Service.fetchOrganization(any())).thenReturn(V1DataResponse.builder().build());
        List<Map<String, Object>> responseMap = new ArrayList<>();
        Map<String, Object> map = new HashMap<>();
        map.put("OrganizationCode", "supCode");
        responseMap.add(map);
        map = new HashMap<>();
        map.put("OrganizationCode", "buyCode");
        responseMap.add(map);
        map = new HashMap<>();
        map.put("OrganizationCode", "notifyCode");
        responseMap.add(map);
        map = new HashMap<>();
        map.put("OrganizationCode", "sendingCode");
        responseMap.add(map);
        map = new HashMap<>();
        map.put("OrganizationCode", "receivingCode");
        responseMap.add(map);
        doReturn(responseMap).when(jsonHelper).convertValue(any(), any(TypeReference.class));
        ShipmentDetails shipmentDetails = orderManagementAdapter.getOrder("123");
        assertNotNull(shipmentDetails);
        assertEquals(guid.toString(), shipmentDetails.getOrderManagementId());
    }

    @Test
    void getOrder_Branches() throws Exception {
        OrderManagementResponse response = new OrderManagementResponse();
        QuantityPair quantityPair = new QuantityPair();
        quantityPair.setAmount(new BigDecimal(23));
        quantityPair.setUnit(Constants.WEIGHT_UNIT_KG);
        UUID guid = UUID.randomUUID();
        Map<String, Object> partiesAddress = new HashMap<>();
        partiesAddress.put("Id", 36118);
        partiesAddress.put("OrgId", 24008);
        OrderPartiesResponse partyConsignor = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("supCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyConsignee = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("buyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyNotifyParty = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("notifyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderManagementDTO orderManagementDTO = OrderManagementDTO.builder()
                .parties(List.of(partyConsignor, partyConsignee,partyNotifyParty ))
                .guid(guid)
                .build();
        response.setOrder(orderManagementDTO);
        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull123", HttpMethod.GET, null, OrderManagementResponse.class);
        when(v1Service.fetchOrganization(any())).thenReturn(V1DataResponse.builder().build());
        List<Map<String, Object>> responseMap = new ArrayList<>();
        Map<String, Object> map = new HashMap<>();
        doReturn(responseMap).when(jsonHelper).convertValue(any(), any(TypeReference.class));
        ShipmentDetails shipmentDetails = orderManagementAdapter.getOrder("123");
        assertNotNull(shipmentDetails);
        assertEquals(guid.toString(), shipmentDetails.getOrderManagementId());
    }

    @Test
    void getOrder_Failure() throws Exception {
        OrderManagementResponse response = new OrderManagementResponse();
        QuantityPair quantityPair = new QuantityPair();
        quantityPair.setAmount(new BigDecimal(23));
        quantityPair.setUnit(Constants.WEIGHT_UNIT_KG);
        UUID guid = UUID.randomUUID();
        Map<String, Object> partiesAddress = new HashMap<>();
        partiesAddress.put("Id", 36118);
        partiesAddress.put("OrgId", 24008);
        OrderPartiesResponse partyConsignor = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("supCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyConsignee = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("buyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyNotifyParty = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("notifyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderManagementDTO orderManagementDTO = OrderManagementDTO.builder()
                .parties(List.of(partyConsignor, partyConsignee,partyNotifyParty ))
                .packsAmount(quantityPair)
                .weightAmount(quantityPair)
                .volumeAmount(quantityPair)
                .guid(guid)
                .build();
        response.setOrder(orderManagementDTO);
        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull123", HttpMethod.GET, null, OrderManagementResponse.class);
        when(v1Service.fetchOrganization(any())).thenThrow(new RuntimeException());
        assertThrows(RunnerException.class, () -> orderManagementAdapter.getOrder("123"));
    }

    @Test
    void getOrderForBookingException() throws RunnerException {
        OrderManagementResponse response = new OrderManagementResponse();
        QuantityPair quantityPair = new QuantityPair();
        quantityPair.setAmount(new BigDecimal(23));
        quantityPair.setUnit(Constants.WEIGHT_UNIT_KG);
        UUID guid = UUID.randomUUID();
        Map<String, Object> partiesAddress = new HashMap<>();
        partiesAddress.put("Id", 36118);
        partiesAddress.put("OrgId", 24008);
        OrderPartiesResponse partyConsignor = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("supCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyConsignee = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("buyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyNotifyParty = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("notifyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderManagementDTO orderManagementDTO = OrderManagementDTO.builder()
                .parties(List.of(partyConsignor, partyConsignee,partyNotifyParty ))
                .packsAmount(quantityPair)
                .weightAmount(quantityPair)
                .volumeAmount(quantityPair)
                .guid(guid)
                .build();
        response.setOrder(orderManagementDTO);
        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull123", HttpMethod.GET, null, OrderManagementResponse.class);
        assertThrows(RunnerException.class, () -> {
            orderManagementAdapter.getOrderForBooking("1234");
        });
    }

    @Test
    void getOrderForBooking() throws RunnerException {
        OrderManagementResponse response = new OrderManagementResponse();
        QuantityPair quantityPair = new QuantityPair();
        quantityPair.setAmount(new BigDecimal(23));
        quantityPair.setUnit(Constants.WEIGHT_UNIT_KG);
        UUID guid = UUID.randomUUID();
        Map<String, Object> partiesAddress = new HashMap<>();
        partiesAddress.put("Id", 36118);
        partiesAddress.put("OrgId", 24008);
        OrderPartiesResponse partyConsignor = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("supCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyConsignee = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("buyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyNotifyParty = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("notifyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderManagementDTO orderManagementDTO = OrderManagementDTO.builder()
                .parties(List.of(partyConsignor, partyConsignee,partyNotifyParty ))
                .packsAmount(quantityPair)
                .weightAmount(quantityPair)
                .volumeAmount(quantityPair)
                .guid(guid)
                .build();
        response.setOrder(orderManagementDTO);
        when(v1Service.fetchOrganization(any())).thenReturn(V1DataResponse.builder().build());
        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull123", HttpMethod.GET, null, OrderManagementResponse.class);
        List<Map<String, Object>> responseMap = new ArrayList<>();

        Map<String, Object> hm = new HashMap<>();

        Map<String, Object> hm1 = new HashMap<>();
        Map<String, Object> hm2 = new HashMap<>();
        Map<String, Object> hm3 = new HashMap<>();

        hm1.put("OrganizationCode", "supCode");
        hm2.put("OrganizationCode", "buyCode");
        hm3.put("OrganizationCode", "notifyCode");

        hm.put("supCode", hm1);
        hm.put("buyCode", hm2);
        hm.put("notifyCode", hm3);

        responseMap.add(hm1);
        responseMap.add(hm2);
        responseMap.add(hm3);

        doReturn(responseMap).when(jsonHelper).convertValue(any(), any(TypeReference.class));
        assertNotNull(orderManagementAdapter.getOrderForBooking("123"));
    }


    @Test
    void getOrderShipmentForBooking() throws RunnerException {
        OrderManagementResponse response = new OrderManagementResponse();
        QuantityPair quantityPair = new QuantityPair();
        quantityPair.setAmount(new BigDecimal(23));
        quantityPair.setUnit(Constants.WEIGHT_UNIT_KG);
        UUID guid = UUID.randomUUID();
        var referenceResponse = new ReferencesResponse();
        var goodsDescription = "GoodsDescription";
        Map<String, Object> partiesAddress = new HashMap<>();
        partiesAddress.put("Id", 36118);
        partiesAddress.put("OrgId", 24008);
        OrderPartiesResponse partyConsignor = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("supCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyConsignee = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("buyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyNotifyParty = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("notifyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderManagementDTO orderManagementDTO = OrderManagementDTO.builder()
                .parties(List.of(partyConsignor, partyConsignee,partyNotifyParty ))
                .packsAmount(quantityPair)
                .weightAmount(quantityPair)
                .volumeAmount(quantityPair)
                .guid(guid)
                .references(Arrays.asList(referenceResponse))
                .goodsDescription(goodsDescription)
                .build();

        response.setOrder(orderManagementDTO);
        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull123", HttpMethod.GET, null, OrderManagementResponse.class);
        when(v1Service.fetchOrganization(any())).thenReturn(V1DataResponse.builder().build());
        List<Map<String, Object>> responseMap = new ArrayList<>();
        Map<String, Object> map = new HashMap<>();
        map.put("OrganizationCode", "supCode");
        responseMap.add(map);
        map = new HashMap<>();
        map.put("OrganizationCode", "buyCode");
        responseMap.add(map);
        map = new HashMap<>();
        map.put("OrganizationCode", "notifyCode");
        responseMap.add(map);
        map = new HashMap<>();
        map.put("OrganizationCode", "sendingCode");
        responseMap.add(map);
        map = new HashMap<>();
        map.put("OrganizationCode", "receivingCode");
        responseMap.add(map);
        doReturn(responseMap).when(jsonHelper).convertValue(any(), any(TypeReference.class));
        ShipmentDetails shipmentDetails = orderManagementAdapter.getOrder("123");
        assertNotNull(shipmentDetails);
        assertNotNull(shipmentDetails.getReferenceNumbersList());
        assertEquals(goodsDescription, shipmentDetails.getGoodsDescription());
    }

    @Test
    void getOrderUsingGuid() throws Exception {
        OrderManagementResponse response = new OrderManagementResponse();
        QuantityPair quantityPair = new QuantityPair();
        quantityPair.setAmount(new BigDecimal(23));
        quantityPair.setUnit(Constants.WEIGHT_UNIT_KG);
        UUID guid = UUID.randomUUID();
        Map<String, Object> partiesAddress = new HashMap<>();
        partiesAddress.put("Id", 36118);
        partiesAddress.put("OrgId", 24008);
        OrderPartiesResponse partyConsignor = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("supCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyConsignee = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("buyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderPartiesResponse partyNotifyParty = OrderPartiesResponse.builder()
                .id("Parties00691")
                .partyType("Consignor")
                .partyCode("notifyCode")
                .partyName("DP World Egypt Logistic Service (1034563)")
                .addressCode("VNSGN - DP World Vietnam Ho chi")
                .reference("contact@godship.com")
                .address(partiesAddress)
                .build();

        OrderManagementDTO orderManagementDTO = OrderManagementDTO.builder()
                .parties(List.of(partyConsignor, partyConsignee,partyNotifyParty ))
                .packsAmount(quantityPair)
                .weightAmount(quantityPair)
                .volumeAmount(quantityPair)
                .guid(guid)
                .build();
        response.setOrder(orderManagementDTO);
        HttpHeaders headers = new HttpHeaders();
        when(v2AuthHelper.getOrderManagementServiceSourceHeader()).thenReturn(headers);
        HttpEntity<Object> httpEntity = new HttpEntity<>(headers);
        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull1234-5678-9123-4567", HttpMethod.GET, httpEntity, OrderManagementResponse.class);
        when(v1Service.fetchOrganization(any())).thenReturn(V1DataResponse.builder().build());
        List<Map<String, Object>> responseMap = new ArrayList<>();
        doReturn(responseMap).when(jsonHelper).convertValue(any(), any(TypeReference.class));
        ShipmentDetails shipmentDetails = orderManagementAdapter.getOrderByGuid("1234-5678-9123-4567");
        assertNotNull(shipmentDetails);
        assertEquals(guid.toString(), shipmentDetails.getOrderManagementId());
    }

    @Test
    void getOrdersUsingShipmentId() throws RunnerException {
        OrderListResponse response = new OrderListResponse();
        OrderManagementDTO orderManagementDTO1 = OrderManagementDTO.builder().orderId("abc").orderNumber("ord1").build();
        response.setData(List.of(orderManagementDTO1));
        HttpHeaders headers = new HttpHeaders();
        Map<String, Object> criteria = new HashMap<>();
        criteria.put("shipmentId", "SHP000125865");
        Map<String, Object> requestBody = new HashMap<>();
        requestBody.put("criteria", criteria);
        requestBody.put("pageNumber", 1);
        requestBody.put("pageSize", 10000);
        requestBody.put("orderQtyNeeded", true);
        when(v2AuthHelper.getOrderManagementServiceSourceHeader()).thenReturn(headers);
        HttpEntity<Object> httpEntity = new HttpEntity<>(requestBody, headers);

        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull", HttpMethod.POST, httpEntity, OrderListResponse.class);

        List<PurchaseOrdersResponse> responses = orderManagementAdapter.getOrdersByShipmentId("SHP000125865");
        assertNotNull(responses);
        assertEquals(orderManagementDTO1.getOrderId(), responses.get(0).getOrder_id());
    }

    @Test
    void getOrdersUsingShipmentIdException() {
        OrderListResponse response = new OrderListResponse();
        OrderManagementDTO orderManagementDTO1 = OrderManagementDTO.builder().orderId("abc").orderNumber("ord1").build();
        response.setData(List.of(orderManagementDTO1));
        HttpHeaders headers = new HttpHeaders();
        Map<String, Object> criteria = new HashMap<>();
        criteria.put("shipmentId", "SHP000125865");
        Map<String, Object> requestBody = new HashMap<>();
        requestBody.put("criteria", criteria);
        requestBody.put("pageNumber", 1);
        requestBody.put("pageSize", 10000);
        requestBody.put("orderQtyNeeded", true);
        when(v2AuthHelper.getOrderManagementServiceSourceHeader()).thenReturn(headers);

        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull", HttpMethod.POST, null, OrderListResponse.class);
        assertThrows(RunnerException.class, () -> {
            orderManagementAdapter.getOrdersByShipmentId("SHP000125865");
        });
    }


    @Test
    void getOrderUsingGuidException() throws RunnerException {
        OrderManagementResponse response = new OrderManagementResponse();
        QuantityPair quantityPair = new QuantityPair();
        quantityPair.setAmount(new BigDecimal(23));
        quantityPair.setUnit(Constants.WEIGHT_UNIT_KG);
        UUID guid = UUID.randomUUID();
        OrderManagementDTO orderManagementDTO = OrderManagementDTO.builder()
                .packsAmount(quantityPair)
                .weightAmount(quantityPair)
                .volumeAmount(quantityPair)
                .guid(guid)
                .build();
        response.setOrder(orderManagementDTO);
        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull123", HttpMethod.GET, null, OrderManagementResponse.class);
        assertThrows(RunnerException.class, () -> {
            orderManagementAdapter.getOrderByGuid("1234");
        });
    }

    @Test
    void getOrderUsingGuidPartyList() throws Exception {
        OrderManagementResponse response = new OrderManagementResponse();
        QuantityPair quantityPair = new QuantityPair();
        quantityPair.setAmount(new BigDecimal(23));
        quantityPair.setUnit(Constants.WEIGHT_UNIT_KG);
        UUID guid = UUID.randomUUID();
        OrderManagementDTO orderManagementDTO = OrderManagementDTO.builder()
                .packsAmount(quantityPair)
                .weightAmount(quantityPair)
                .volumeAmount(quantityPair)
                .guid(guid)
                .build();
        response.setOrder(orderManagementDTO);
        HttpHeaders headers = new HttpHeaders();
        when(v2AuthHelper.getOrderManagementServiceSourceHeader()).thenReturn(headers);
        HttpEntity<Object> httpEntity = new HttpEntity<>(headers);
        doReturn(new ResponseEntity<>(response, HttpStatus.OK)).when(restTemplate).exchange("nullnull1234-5678-9123-4567", HttpMethod.GET, httpEntity, OrderManagementResponse.class);
        when(v1Service.fetchOrganization(any())).thenReturn(V1DataResponse.builder().build());
        List<Map<String, Object>> responseMap = new ArrayList<>();
        doReturn(responseMap).when(jsonHelper).convertValue(any(), any(TypeReference.class));
        ShipmentDetails shipmentDetails = orderManagementAdapter.getOrderByGuid("1234-5678-9123-4567");
        assertNotNull(shipmentDetails);
        assertEquals(guid.toString(), shipmentDetails.getOrderManagementId());
    }
}